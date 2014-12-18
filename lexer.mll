(* Lexer for OCaml CMM dumps.  *)

{
  open Cmm
  open Parser
  
  let cmpcode = function
    "==" -> Eq
  | "!=" -> Ne
  | "<" -> Lt
  | "<=" -> Le
  | ">" -> Gt
  | ">=" -> Ge
  | _ -> failwith "Not a comparison"
}

let num = ['0'-'9']+
let snum = num | '-' num
let flnum = snum '.' | snum '.' num | snum 'e' snum | snum '.' num 'e' snum
let hexdigit = ['0'-'9'] | ['a'-'f'] | ['A'-'F']
let comparison = "==" | "!=" | "<" | "<=" | ">" | ">="
let asc = ['a'-'z'] | ['A'-'Z'] | '_'
let label = asc ( asc | num | '/' )*
let noquote = [^'"']

rule token = parse
    "float"			{ MACHTYPE Float }
  | "unsigned int8"		{ SCALAR Uint8 }
  | "signed int8"		{ SCALAR Sint8 }
  | "unsigned int16"		{ SCALAR Uint16 }
  | "signed int16"		{ SCALAR Sint16 }
  | "unsigned int32"		{ SCALAR Uint32 }
  | "signed int32"		{ SCALAR Sint32 }
  | "float32"			{ SCALAR Float }
  | "float64"			{ SCALAR Double }
  | "float64u"			{ SCALAR Double_u }
  | "app"			{ OPER Apply }
  | "extcall"			{ OPER Extcall }
  | "load"			{ LOAD }
  | "alloc"			{ OPER Alloc }
  | "store"			{ STORE }
  | "+"				{ OPER Iadd }
  | "-"				{ OPER Isub }
  | "*"				{ OPER Imul }
  | "*h"			{ OPER Imulh }
  | "/"				{ OPER Idiv }
  | "mod"			{ OPER Imod }
  | "and"			{ OPER Iand }
  | "or"			{ OPER Ior }
  | "xor"			{ OPER Ixor }
  | "<<"			{ OPER Ilsl }
  | ">>u"			{ OPER Ilsr }
  | ">>s"			{ OPER Iasr }
  | comparison as cmp		{ OPER (Icmp (cmpcode cmp)) }
  | "+a"			{ OPER Aadd }
  | "-a"			{ OPER Asub }
  | (comparison as cmp) "a"	{ OPER (Acmp (cmpcode cmp)) }
  | "~f"			{ OPER Fneg }
  | "absf"			{ OPER Fabs }
  | "+f"			{ OPER Fadd }
  | "-f"			{ OPER Fsub }
  | "*f"			{ OPER Fmul }
  | "/f"			{ OPER Fdiv }
  | "floatofint"		{ OPER Itof }
  | "intoffloat"		{ OPER Ftoi }
  | (comparison as cmp) "f"	{ OPER (Fcmp (cmpcode cmp)) }
  | "raise"			{ RAISE Raise }
  | "reraise"			{ RAISE Reraise }
  | "raise_notrace"		{ RAISE Raise_notrace }
  | "checkbound"		{ OPER Checkbound }
  | snum as snum		{ INT (Int64.of_string snum) }
  | flnum as flnum		{ FLOAT (float_of_string flnum) }
  | '"' (noquote* as str) '"'	{ STRING str }
  | (snum as snum) 'a'		{ PTR_CONST (Int64.of_string snum) }
  | '('				{ OPENPAREN }
  | ')'				{ CLOSEPAREN }
  | ':'				{ COLON }
  | "let"			{ LET }
  | "assign"			{ ASSIGN }
  | "seq"			{ SEQ }
  | "if"			{ IF }
  | "switch"			{ SWITCH }
  | "loop"			{ LOOP }
  | "catch"			{ CATCH }
  | "try"			{ TRY }
  | "with"			{ WITH }
  | "exit"			{ EXIT }
  | "function"			{ FUNCTION }
  | "data"			{ DATA }
  | "global"			{ GLOBAL }
  | "byte"			{ BYTE }
  | "int16"			{ INT16 }
  | "int32"			{ INT32 }
  | "int"			{ INTLIT }
  | "single"			{ SINGLE }
  | "double"			{ DOUBLE }
  | "addr"			{ ADDR }
  | "string"			{ STRINGLIT }
  | "skip"			{ SKIP }
  | "align"			{ ALIGN }
  | 'L' (num as num)		{ LABEL (int_of_string num) }
  | label as varname		{ VARNAME varname }
  | (" "|"\t")+			{ token lexbuf }
  | ("\n")			{ Lexing.new_line lexbuf; token lexbuf }
  | eof				{ EOF }
