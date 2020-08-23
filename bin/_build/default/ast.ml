open Base

type ainstr =
  | Value of int
  | Symbol of string
  | Label of string
  | Removed  (* for removed labels *)
[@@deriving sexp]

type dest =
  | Null
  | M
  | D
  | MD
  | A
  | AM
  | AD
  | AMD
[@@deriving sexp]

type comp =
  | Zero
  | One
  | Neg1
  | M | NotM | NegM | Mplus1 | Mminus1
  | D | NotD | NegD | Dplus1 | Dminus1
  | A | NotA | NegA | Aplus1 | Aminus1
  | DplusA | DminusA | AminusD | DandA | DorA
  | DplusM | DminusM | MminusD | DandM | DorM
[@@deriving sexp]

type jump =
  | Null
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
[@@deriving sexp]

type cinstr =
  | Instr of dest * comp * jump
[@@deriving sexp]

type instr =
  | Cinstr of cinstr
  | Ainstr of ainstr
[@@deriving sexp]

type program =
  | Prog of instr list
[@@deriving sexp]
