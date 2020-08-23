open Base
open Stdio


let pp exp =
  print_s ([%sexp_of: Ast.program] exp)

(* let () = print_endline ((Parse.eval "1+2*3") |> Int.to_string) *)
let () =
  let i = Stdio.In_channel.input_all Stdio.stdin in
  let s = Parse.eval_prog i in
  let s2 = Symbols.coversion s in
  let s3 = Codegen.codegen s2 in
  List.iter s3 ~f:print_endline
