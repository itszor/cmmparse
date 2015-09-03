open Cmm

module StringSet = Set.Make (String)

module TupleTypeSet = Set.Make
  (struct
    type t = machtype list
    let compare = compare
   end)

let find_globals dlist sset =
  List.fold_right
    (fun dat acc ->
      match dat with
        Global_symbol s -> StringSet.add s acc
      | _ -> acc)
    dlist
    sset

let find_all_globals toplevel =
  List.fold_right
    (fun stanza acc ->
      match stanza with
        Data dl -> find_globals dl acc
      | _ -> acc)
    toplevel
    StringSet.empty

let mach_type_for_data_item = function
    Int_lit _ -> Int
  | Float_lit _ | Double_lit _ -> Float
  | Symbol_address _ | Label_address _ -> Addr
  | _ -> raise Not_found

let tuple_type dlist =
  let rec build = function
    [] -> []
  | Int_lit _ :: rest -> Int :: build rest
  | (Float_lit _ | Double_lit _) :: rest -> Float :: build rest
  | (Symbol_address _ | Label_address _) :: rest -> Addr :: build rest
  | (Define_label _ | Define_symbol _ | Global_symbol _) :: rest -> build rest
  | _ -> failwith "unexpected item in tuple" in
  build dlist

let rec array_type_for_data dlist =
  let rec same_as first = function
    [] -> Some first
  | q::rest ->
      if first <> mach_type_for_data_item q then
        None
      else
        same_as first rest in
  match dlist with
    [] -> None
  | (Int_lit _ | Float_lit _ | Double_lit _ | Symbol_address _
     | Label_address _ as first) :: rest ->
    same_as (mach_type_for_data_item first) rest
  | (Define_symbol _ | Define_label _ | Global_symbol _) :: rest ->
      array_type_for_data rest
  | Data_header (Tag_tsa, _, _) :: rest -> array_type_for_data rest
  | _ -> None

let tuple_type_for_data dlist =
  let rec find_tuple = function
    [] -> None
  | Global_symbol _ :: rest -> find_tuple rest
  | Data_header (Tag_tsa, _, _) :: rest -> Some (tuple_type rest)
  | (Define_label _ | Define_symbol _) :: rest -> find_tuple rest
  | _ -> None in
  find_tuple dlist

type tsa_type =
    Tsa_array of machtype
  | Tsa_tuple_struct of machtype list

let classify_tsa dlist =
  match array_type_for_data dlist with
    None ->
      begin match tuple_type_for_data dlist with
        None -> raise Not_found
      | Some tup -> Tsa_tuple_struct tup
      end
  | Some elemtype -> Tsa_array elemtype

let rec tag_for_data = function
    [] -> None
  | Data_header (blk_tag, _, _) :: _ -> Some blk_tag
  | (Define_symbol _ | Define_label _ | Global_symbol _) :: rest ->
      tag_for_data rest
  | _ -> None

let find_all_tuple_types toplevel =
  let rec scan ttset = function
    [] -> ttset
  | Data dlist :: rest ->
      let ttset' =
        match tag_for_data dlist with
	  Some Tag_tsa ->
	    begin match classify_tsa dlist with
              Tsa_tuple_struct ttype -> TupleTypeSet.add ttype ttset
	    | _ -> ttset
	    end
	| _ -> ttset in
      scan ttset' rest
  | Function _ :: rest -> scan ttset rest in
  scan TupleTypeSet.empty toplevel

let ctype_for_mt = function
    Addr -> "void *"
  | Int -> "intptr_t"
  | Float -> "double"

let print_ctype_for_mt fh mt =
  Printf.fprintf fh "%s" (ctype_for_mt mt)

let tuple_type_basename ttype =
  let arr = Array.of_list ttype in
  Bytes.init (Array.length arr)
	     (fun i -> match arr.(i) with
	       Int -> 'i' | Float -> 'f' | Addr -> 'a')

let tuple_type_name ttype =
  "tup_" ^ tuple_type_basename ttype

let emit_tuple_types fh ttset =
  TupleTypeSet.iter
    (fun mtlist ->
      let basename = tuple_type_basename mtlist in
      Printf.fprintf fh "struct tup_%s_body {\n" basename;
      List.iteri
        (fun i mt ->
	  Printf.fprintf fh "  %a f%d;\n" print_ctype_for_mt mt (i + 1))
	mtlist;
      Printf.fprintf fh "};\n\n";
      Printf.fprintf fh "struct tup_%s {\n" basename;
      Printf.fprintf fh "  intptr_t blkhdr;\n";
      Printf.fprintf fh "  struct tup_%s_body b;\n" basename;
      Printf.fprintf fh "};\n\n")
    ttset

let emit_data fh toplevel =
  let ttset = find_all_tuple_types toplevel in
  emit_tuple_types fh ttset;
  List.iter
    (function
      Data dlist ->
        begin match tag_for_data dlist with
	  Some Tag_tsa ->
            let ctype = classify_tsa dlist in
	    begin match ctype with
	      Tsa_array mt -> ()
	    | Tsa_tuple_struct tt ->
		let type_name = tuple_type_name tt in
		Printf.fprintf fh "type %s, bla bla\n" type_name
	    end
	| _ -> ()
	end
    | Function _ -> ())
    toplevel
