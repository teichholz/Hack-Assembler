open Base
open Ast




let init () =
  let rs = List.map (List.range 0 15) ~f:(fun i -> "R" ^ Int.to_string i, i) in
  let pairs = rs @ ["SCREEN", 16384; "KBD", 24576; "SP", 0; "LCL", 1; "ARG", 2;
                    "THIS", 3; "THAT", 4; "LOOP", 4] in
  Hashtbl.of_alist_exn (module String) pairs


let symboltable :(string, int) Hashtbl.t = init ()
let lookup symbol =
  Hashtbl.find symboltable symbol

let start = ref 16
let pos = ref 0

let resolve_symbol instr =
  match instr with
  | Ainstr (Symbol str) ->
    (match lookup str with
     | Some int -> Ainstr(Value(int))
     | None ->
       Hashtbl.add_exn symboltable ~key:str ~data:!start;
       start := !start + 1;
       Ainstr(Value(!start-1)))
  | _ ->
    instr


let resolve_label instr =
  match instr with
  | Ainstr (Label label) ->
       Hashtbl.add_exn symboltable ~key:label ~data:(!pos+1);
       Ainstr(Removed)
  | _ ->
    pos := !pos + 1;
    instr

let resolve_symbols program =
  match program with
  | Prog instrs -> Prog(List.map instrs ~f:resolve_symbol)

let resolve_labels program =
  match program with
  | Prog instrs -> Prog(List.map instrs ~f:resolve_label)

let clear_ainstrs program =
  let Prog(instrs) = program in
  let rec aux instrs =
    match instrs with
    | Ainstr(Removed)::tl -> aux tl
    | hd::tl -> hd::aux tl
    | [] -> [] in Prog(aux instrs)

let coversion program =
  program |> resolve_labels |> resolve_symbols |> clear_ainstrs
