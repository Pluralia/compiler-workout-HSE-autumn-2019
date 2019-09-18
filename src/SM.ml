open GT       
       
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
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter
     val eval : config -> prg -> config
   Takes a configuration and a program, and returns a configuration as a result
 *)
let evalIns (stack, (st, i, o)) ins =
        match ins with
        | BINOP opName ->
                (match stack with
                 | y :: x :: restStack -> (Syntax.Expr.applyOpTo opName x y :: restStack, (st, i, o))
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
                 | hstack :: tstack -> (tstack, (Syntax.Expr.update name hstack st, i, o))
                 | _                -> failwith "Try store from empty stack")
        
let rec eval conf prog =
        match prog with
        | hprog :: tprog -> eval (evalIns conf hprog) tprog
        | _              -> conf

(* Top-level evaluation
     val run : int list -> prg -> int list
   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler
     val compile : Syntax.Stmt.t -> prg
   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compileExpr expr =
         match expr with
         | Syntax.Expr.Const cVal                   -> [CONST cVal]
         | Syntax.Expr.Var name                     -> [LD name]
         | Syntax.Expr.Binop (opName, expr1, expr2) ->
                 compileExpr expr1 @ compileExpr expr2 @ [BINOP opName]
 
let rec compile stm =
         match stm with
         | Syntax.Stmt.Read name           -> [READ; ST name]
         | Syntax.Stmt.Write expr          -> compileExpr expr @ [WRITE] 
         | Syntax.Stmt.Assign (name, expr) -> compileExpr expr @ [ST name]
         | Syntax.Stmt.Seq (stm1, stm2)    -> compile stm1 @ compile stm2

