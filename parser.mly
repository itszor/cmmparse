%{

open Cmm

exception Parse_error

let colour_of_num = function
    0 -> White
  | 1 -> Grey
  | 2 -> Blue
  | 3 -> Black
  | _ -> failwith "Not a colour"

let tag_of_num = function
    0 -> Tag_tsa
  | 246 -> Tag_lazy
  | 247 -> Tag_closure
  | 248 -> Tag_object
  | 249 -> Tag_infix
  | 250 -> Tag_forward
  | 251 -> Tag_abstract
  | 252 -> Tag_string
  | 253 -> Tag_double
  | 254 -> Tag_double_array
  | 255 -> Tag_custom
  | x -> Tag_variant x

let detag = function
    Int_const i ->
      let sz = Int64.shift_right_logical i 10
      and tag = tag_of_num (Int64.to_int (Int64.logand i 0xffL)) in
      Block_header (tag, sz)
  | x -> x

let count_data dlist =
  let rec scan amt = function
    [] -> amt
  | (Define_symbol _ | Define_label _ | Global_symbol _) :: rest ->
      scan amt rest
  | Int8_lit _ :: rest -> scan (1 + amt) rest
  | Int16_lit _ :: rest -> scan (2 + amt) rest
  | Int32_lit _ :: rest -> scan (4 + amt) rest
  | Int_lit _ :: rest -> scan (8 + amt) rest
  | Float_lit _ :: rest -> scan (8 + amt) rest
  | Double_lit _ :: rest -> scan (8 + amt) rest
  | Symbol_address _ :: rest -> scan (8 + amt) rest
  | Label_address _ :: rest -> scan (8 + amt) rest
  | String_lit s :: rest -> scan (String.length s + amt) rest
  | Data_header _ :: rest -> scan (8 + amt) rest
  | Skip i :: rest -> scan (i + amt) rest
  | Align i :: rest ->
      let mask = lnot (i - 1) in
      scan ((amt + i - 1) land mask) rest in
  scan 0 dlist

let detag_data dlist =
  let check header_size data =
    let size_bytes = count_data data in
    let size_words = (size_bytes + 7) / 8 in
    size_words = header_size in
  let rec scan = function
    [] -> []
  | Int_lit il :: rest ->
      let sz = Int64.shift_right_logical il 10
      and tag = tag_of_num (Int64.to_int (Int64.logand il 0xffL))
      and colour = colour_of_num (Int64.to_int
		     (Int64.logand (Int64.shift_right_logical il 8) 3L)) in
      if check (Int64.to_int sz) rest then
	Data_header (tag, colour, sz) :: rest
      else
        Int_lit il :: rest
  | (Global_symbol _ as gs) :: rest -> gs :: scan rest
  | x -> x in
  scan dlist

%}

%token <Cmm.machtype> MACHTYPE
%token <Cmm.scalar> SCALAR
%token <Cmm.oper> OPER
%token <Cmm.raise_kind> RAISE
%token <string> STRING VARNAME
%token <int64> INT PTR_CONST
%token <float> FLOAT
%token <int> LABEL
%token LOAD STORE OPENPAREN CLOSEPAREN LET ASSIGN SEQ IF SWITCH
%token LOOP CATCH WITH EXIT TRY GLOBAL BYTE INT16 INT32 INTLIT SINGLE DOUBLE
%token ADDR VAL STRINGLIT SKIP ALIGN COLON
%token FUNCTION DATA
%token EOF

%start <Cmm.stanza list> toplevel

%%

toplevel: EOF				{ [] }
	| d = datablock ts = toplevel	{ Data (detag_data d) :: ts }
	| f = fnblock ts = toplevel	{ Function f :: ts }
	| error				{ raise Parse_error }
;

datablock: OPENPAREN DATA ds = list(data_item) CLOSEPAREN
					{ ds }
;

data_item: s = STRING COLON		{ Define_symbol s }
	 | l = LABEL COLON		{ Define_label l }
	 | GLOBAL s = STRING		{ Global_symbol s }
	 | BYTE n = INT			{ Int8_lit (Int64.to_int n) }
	 | INT16 n = INT		{ Int16_lit (Int64.to_int n) }
	 | INT32 n = INT		{ Int32_lit (Int64.to_int32 n) }
	 | INTLIT n = INT		{ Int_lit n }
	 | SINGLE n = FLOAT		{ Float_lit n }
	 | DOUBLE n = FLOAT		{ Double_lit n }
	 | STRINGLIT s = STRING		{ String_lit s }
	 | ADDR s = STRING		{ Symbol_address s }
	 | ADDR l = LABEL		{ Label_address l }
	 | SKIP n = INT			{ Skip (Int64.to_int n) }
	 | ALIGN n = INT		{ Align (Int64.to_int n) }
	 | error			{ raise Parse_error }
;

fnblock: OPENPAREN FUNCTION n = name OPENPAREN args = list(arg) CLOSEPAREN
	 body = expr_seq CLOSEPAREN	{ { fnname = n;
					    fnargs = args;
					    fnbody = body } }
;

name: l = LABEL				{ "L" ^ (string_of_int l) }
    | ASSIGN				{ "assign" }
    | SEQ				{ "seq" }
    | SWITCH				{ "switch" }
    | LOOP				{ "loop" }
    | CATCH				{ "catch" }
    | EXIT				{ "exit" }
    | DATA				{ "data" }
    | GLOBAL				{ "global" }
    | BYTE				{ "byte" }
    | INT16				{ "int16" }
    | INT32				{ "int32" }
    | INTLIT				{ "int" }
    | SINGLE				{ "single" }
    | DOUBLE				{ "double" }
    | ADDR				{ "addr" }
    | SKIP				{ "skip" }
    | ALIGN				{ "align" }
    | v = VARNAME			{ v }
;

machtype: m = MACHTYPE			{ m }
	| ADDR				{ Addr }
	| VAL				{ Val }
	| INTLIT			{ Int }
;

arg: n = name COLON m = machtype	{ n, m }
;

expr: OPENPAREN o = OPER el = list(expr) CLOSEPAREN
					{ match o, el with
					  Alloc, hdr::lst ->
					    Op (o, (detag hdr)::lst)
					| _ ->
					    Op (o, el) }
    | i = INT				{ Int_const i }
    | f = FLOAT				{ Float_const f }
    | p = PTR_CONST			{ Pointer_const p }
    | s = STRING			{ Sym_const s }
    | n = name				{ Var n }
    | OPENPAREN ASSIGN n = name e = expr
					{ Assign (n, e) }
    | OPENPAREN SEQ e1 = expr e2 = expr CLOSEPAREN
					{ Sequence (e1, e2) }
    | OPENPAREN IF e1 = expr e2 = expr e3 = expr CLOSEPAREN
					{ IfThenElse (e1, e2, e3) }
    | OPENPAREN LOOP e = expr CLOSEPAREN
					{ Loop e }
    | OPENPAREN LOAD s = accsz el = list(expr) CLOSEPAREN
    					{ Op (Load s, el) }
    | OPENPAREN STORE s = accsz el = list(expr) CLOSEPAREN
    					{ Op (Store s, el) }
    | OPENPAREN LET n = name e1 = expr e2 = expr_seq CLOSEPAREN
					{ Let (n, e1, e2) }
    | OPENPAREN LET OPENPAREN bs = list(bindings) CLOSEPAREN body = expr_seq
      CLOSEPAREN			{ let rec lets = function
					    [] -> failwith "Empty"
					  | [n, b] -> Let (n, b, body)
					  | (n, b) :: rest ->
					      Let (n, b, lets rest) in
					  lets bs }
    | OPENPAREN TRY e1 = expr_seq WITH n = name e2 = expr_seq CLOSEPAREN
					{ TryWith (e1, n, e2) }
    | OPENPAREN r = RAISE el = list(expr) CLOSEPAREN
					{ Op (Raise r, el) }
    | error				{ raise Parse_error }
;

bindings: n = name def = expr		{ n, def }
;

expr_seq: e1 = expr e2 = expr_seq	{ Sequence (e1, e2) }
        | e = expr			{ e }
;

accsz: /* empty */			{ Word_int }
     | VAL				{ Word_val }
     | s = SCALAR			{ s }
;
