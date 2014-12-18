%{

open Cmm

exception Parse_error

%}

%token <Cmm.machtype> MACHTYPE
%token <Cmm.scalar> SCALAR
%token <Cmm.oper> OPER
%token <Cmm.datum> DATUM
%token <Cmm.raise_kind> RAISE
%token <string> STRING VARNAME
%token <int64> INT PTR_CONST
%token <float> FLOAT
%token <int> LABEL
%token LOAD STORE OPENPAREN CLOSEPAREN LET ASSIGN SEQ IF SWITCH
%token LOOP CATCH WITH EXIT TRY GLOBAL BYTE INT16 INT32 INTLIT SINGLE DOUBLE
%token ADDR STRINGLIT SKIP ALIGN COLON
%token FUNCTION DATA
%token EOF

%start <Cmm.stanza list> toplevel

%%

toplevel: EOF				{ [] }
	| d = datablock ts = toplevel	{ Data d :: ts }
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
    | LET				{ "let" }
    | ASSIGN				{ "assign" }
    | SEQ				{ "seq" }
    | IF				{ "if" }
    | SWITCH				{ "switch" }
    | LOOP				{ "loop" }
    | CATCH				{ "catch" }
    | TRY				{ "try" }
    | WITH				{ "with" }
    | EXIT				{ "exit" }
    | FUNCTION				{ "function" }
    | DATA				{ "data" }
    | GLOBAL				{ "global" }
    | BYTE				{ "byte" }
    | INT16				{ "int16" }
    | INT32				{ "int32" }
    | INTLIT				{ "int" }
    | SINGLE				{ "single" }
    | DOUBLE				{ "double" }
    | ADDR				{ "addr" }
    | STRINGLIT				{ "string" }
    | SKIP				{ "skip" }
    | ALIGN				{ "align" }
    | v = VARNAME			{ v }
;

machtype: m = MACHTYPE			{ m }
	| ADDR				{ Addr }
	| INTLIT			{ Int }
;

arg: n = name COLON m = machtype	{ n, m }
;

expr: OPENPAREN o = OPER el = list(expr) CLOSEPAREN
					{ Op (o, el) }
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
    | error				{ raise Parse_error }
;

bindings: n = name def = expr		{ n, def }
;

expr_seq: e1 = expr e2 = expr_seq	{ Sequence (e1, e2) }
        | e = expr			{ e }
;

accsz: /* empty */			{ Word }
     | s = SCALAR			{ s }
;
