let parse fname =
  let ic = open_in fname in
  let lex = Lexing.from_channel ic in
  begin try
    let ret = Parser.toplevel Lexer.token lex in
    close_in ic;
    ret
  with ex ->
    let open Lexing in
    Printf.fprintf stderr "Parse error at line %d\n"
      (lexeme_start_p lex).pos_lnum;
    flush stderr;
    close_in ic;
    []
  end
