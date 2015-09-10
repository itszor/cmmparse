type machtype = Addr
              | Val
              | Int
	      | Float

type cmp = Eq
         | Ne
	 | Lt
	 | Le
	 | Gt
	 | Ge

type scalar = Uint8
            | Sint8
	    | Uint16
	    | Sint16
	    | Uint32
	    | Sint32
	    | Word_int
	    | Word_val
	    | Float
	    | Double
	    | Double_u

type raise_kind = Raise | Reraise | Raise_notrace

type blk_tag = Tag_tsa
             | Tag_lazy
	     | Tag_closure
	     | Tag_object
	     | Tag_infix
	     | Tag_forward
	     | Tag_abstract
	     | Tag_string
	     | Tag_double
	     | Tag_double_array
	     | Tag_custom
	     | Tag_int
	     | Tag_out_of_heap
	     | Tag_unaligned
	     | Tag_variant of int

type colour = White | Grey | Blue | Black

type oper = Apply of scalar
          | Extcall
	  | Load of scalar
	  | Alloc
	  | Store of scalar
	  | Iadd
	  | Isub
	  | Imul
	  | Imulh
	  | Idiv
	  | Imod
	  | Iand
	  | Ior
	  | Ixor
	  | Ilsl
	  | Ilsr
	  | Iasr
	  | Icmp of cmp
	  | Aadd
	  | Asub
	  | Acmp of cmp
	  | Fneg
	  | Fabs
	  | Fadd
	  | Fsub
	  | Fmul
	  | Fdiv
	  | Ftoi
	  | Itof
	  | Fcmp of cmp
	  | Raise of raise_kind
	  | Checkbound
	  
type expr = Int_const of int64
	  | Float_const of float
	  | Sym_const of string
	  | Pointer_const of int64
	  | Block_header of blk_tag * int64
	  | Var of string
	  | Let of string * expr * expr
	  | Assign of string * expr
	  | Tuple of expr list
	  | Op of oper * expr list
	  | Sequence of expr * expr
	  | IfThenElse of expr * expr * expr
	  | Switch of expr * int64 array * expr array
	  | Loop of expr
	  | Catch of int64 * string list * expr * expr
	  | Exit of int64 * expr list
	  | TryWith of expr * string * expr

type datum = Define_symbol of string
           | Define_label of int
	   | Global_symbol of string
	   | Data_header of blk_tag * colour * int64
	   | Int8_lit of int
	   | Int16_lit of int
	   | Int32_lit of int32
	   | Int_lit of int64
	   | Float_lit of float
	   | Double_lit of float
	   | Symbol_address of string
	   | Label_address of int
	   | String_lit of string
	   | Skip of int
	   | Align of int

type func_info =
  {
    fnname: string;
    fnargs: (string * machtype) list;
    fnbody: expr
  }

type stanza = Function of func_info
            | Data of datum list
