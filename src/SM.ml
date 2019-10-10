open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
*)                         
let evalIns (stack, (st, i, o)) ins =
        match ins with
        | BINOP opName ->
                (match stack with
                 | y :: x :: restStack -> (Language.Expr.applyOpTo opName x y :: restStack, (st, i, o))
                 | _                   -> failwith "Lacks of arguments for binary operation")
        | CONST cVal -> (cVal :: stack, (st, i, o))
        | READ       ->
                (match i with
                 | hi :: ti -> (hi :: stack, (st, ti, o))
                 | _        -> failwith "Try read without input")
        | WRITE      ->
                (match stack with
                 | hstack :: tstack -> (tstack, (st, i, o @ [hstack]))
                 | _                -> failwith "Try write from empty stack")
        | LD name    -> (st name :: stack, (st, i, o))
        | ST name ->
                (match stack with
                 | hstack :: tstack -> (tstack, (Language.Expr.update name hstack st, i, o))
                 | _                -> failwith "Try store from empty stack")
        
let rec eval conf prog =
        match prog with
        | hprog :: tprog -> eval (evalIns conf hprog) tprog
        | _              -> conf

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]
