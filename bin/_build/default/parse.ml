open Base
open Angstrom
open Ast


let numberp =
  function | '0' .. '9' -> true | _ -> false

let symbolp =
  function | '\n' | '\t' | '(' | ')' | ' ' -> false | _ -> true

let spacep =
  function |'\t' | ' ' -> true | _ -> false

let dests = ["null"; "M"; "D"; "MD"; "A"; "AM"; "AD"; "AMD"]
let to_dest : string -> dest =
  function | "null" -> Null |  "M" -> M | "D" -> D | "MD" -> MD | "A" -> A | "AM" -> AM | "AD" -> AD | "AMD" -> AMD
let jumps = ["null"; "JGT"; "JEQ"; "JGE"; "JLT"; "JNE"; "JLE"; "JMP"]
let to_jump : string -> jump =
  function | "null" -> Null |  "JGT" -> JGT | "JEQ" -> JEQ | "JGE" -> JGE | "JLT" -> JLT | "JNE" -> JNE | "JLE" -> JLE | "JMP" -> JMP
let comps = ["0"; "1"; "-1"; "D"; "A"; "!D"; "!A"; "-D"; "-A"; "D+1"; "A+1"; "D-1"; "A-1"; "D+A"; "D-A"; "A-D"; "D&A"; "D|A";
             "M"; "!M"; "-M"; "M+1"; "M-1"; "D+M"; "D-M"; "M-D"; "D&M"; "D|M"]
let to_comp =
  function | "0" -> Zero | "1" -> One | "-1" -> Neg1 | "D" -> D | "A" -> A | "!D" ->NotD| "!A" -> NotA | "-D" -> NegD | "-A" -> NotA | "D+1" -> Dplus1 | "A+1" -> Aplus1 | "D-1" -> Dminus1 | "A-1" -> Aminus1 | "D+A" -> DplusA | "D-A" -> DminusA | "A-D" -> AminusD | "D&A" -> DandA | "D|A" -> DorA
           | "M" -> M | "!M" -> NotM | "-M" -> NegM | "M+1" -> Mplus1 | "M-1" -> Mminus1 | "D+M" -> DplusM | "D-M" -> DminusM | "M-D" -> MminusD | "D&M" -> DandM | "D|M" -> DorM

let white = skip_while spacep <|>
            string_ci "//" *> skip_while (function | '\n' -> false | _ -> true)
let number = lift Int.of_string (take_while1 numberp)
let symbol = lift Fun.id (take_while1 symbolp)
let paren p = char '(' *> p <* char ')'

let ainstr = char '@' *> (lift (fun num -> Value num) number <|> lift (fun symbol -> Symbol symbol) symbol)
             <|> lift (fun label -> Label label) (paren symbol)

let liftl l f =
  let p = List.fold l ~init:(fail "") ~f:(fun acc str -> acc <|> string str) in
  lift f p
let dest = liftl dests to_dest <* char '='
let jump = char ';' *> liftl jumps to_jump
let comp = liftl comps to_comp
let cinstr =
  lift3 (fun dest comp jump -> Instr (dest, comp, jump)) dest comp jump
  <|> lift2 (fun dest comp -> Instr (dest, comp, Null)) dest comp
  <|> lift2 (fun comp jump -> Instr (Null ,comp, jump)) comp jump

let instr =
  let ins = lift (fun cinstr -> Cinstr cinstr) cinstr <|> lift (fun ainstr -> Ainstr ainstr) ainstr in
  (white <|> skip_many end_of_line) *> ins <* white <* end_of_line
  (* ins <* white <* end_of_line *)

let program =
  let prog = many1 instr <* end_of_input in
  lift (fun prog -> Prog(prog)) prog

let eval_prog str =
  match parse_string program str with
  | Ok v      -> v
  | Error msg -> failwith msg
