open Base
open Ast



let dec_to_bin x =
  let rec aux x lst =
    match x with
    | 0 -> lst
    | _ -> aux (x/2) (x mod 2::lst)
  in aux x []

let rec fill_with_zero lst =
  match List.length lst with
  | n when n<16 -> fill_with_zero (0::lst)
  | _ -> lst

let dest_to_bin (dest : dest) =
  match dest with
  | Null -> [0;0;0]
  | M -> [0;0;1]
  | D -> [0;1;0]
  | MD -> [0;1;1]
  | A -> [1;0;0]
  | AM -> [1;0;1]
  | AD -> [1;1;0]
  | AMD -> [1;1;1]

let jump_to_bin (jump : jump) =
  match jump with
  | Null -> [0;0;0]
  | JGT -> [0;0;1]
  | JEQ -> [0;1;0]
  | JGE -> [0;1;1]
  | JLT -> [1;0;0]
  | JNE -> [1;0;1]
  | JLE -> [1;1;0]
  | JMP -> [1;1;1]

let comp_to_bin (comp : comp) =
  match comp with
  | Zero -> [0;1;0;1;0;1;0]
  | One -> [0;1;1;1;1;1;1]
  | Neg1 -> [0;1;1;1;0;1;0]
  | D -> [0;0;0;1;1;0;0]
  | A  -> [0;1;1;0;0;0;0]
  | M -> [1;1;1;0;0;0;0]
  | NotD -> [0;0;0;1;1;0;1]
  | NotA -> [0; 1;1;0;0;0;1]
  | NotM -> [1; 1;1;0;0;0;1]
  | NegD -> [0;0;0;1;1;1;1]
  | NegA -> [0;1;1;0;0;1;1]
  | NegM -> [1;1;1;0;0;1;1]
  | Dplus1 -> [0;0;1;1;1;1;1]
  | Aplus1 -> [0;1;1;0;1;1;1]
  | Mplus1 -> [1;1;1;0;1;1;1]
  | Dminus1 -> [0;0;0;1;1;1;0]
  | Aminus1 -> [0;1;1;0;0;1;0]
  | Mminus1 -> [1;1;1;0;0;1;0]
  | DplusA -> [0;0;0;0;0;1;0]
  | DplusM -> [1;0;0;0;0;1;0]
  | DminusA-> [0;0;1;0;0;1;1]
  | DminusM -> [1;0;1;0;0;1;1]
  | AminusD -> [0;0;0;0;1;1;1]
  | MminusD -> [1;0;0;0;1;1;1]
  | DandA  -> [0;0;0;0;0;0;0]
  | DandM -> [1;0;0;0;0;0;0]
  | DorA-> [0;0;1;0;1;0;1]
  | DorM -> [1;0;1;0;1;0;1]

let list_to_string lst =
  List.fold lst ~init:"" ~f:(fun acc ele -> acc ^ Int.to_string ele)

let ainstr_to_binary int =
  list_to_string (fill_with_zero (dec_to_bin int))

let cinstr_to_binary dest comp jump =
  list_to_string ([1;1;1] @ dest_to_bin dest @ comp_to_bin comp @ jump_to_bin jump)


let codegen program =
  let aux acc instr =
    match instr with
  | Ainstr (Value int) -> acc @ [ainstr_to_binary int]
  | Cinstr (Instr (dest, comp, jump)) -> acc @ [cinstr_to_binary dest comp jump]
  | _ -> raise (Failure "codegen error")
  in let Prog(program) = program in List.fold program ~init:[] ~f:aux;
