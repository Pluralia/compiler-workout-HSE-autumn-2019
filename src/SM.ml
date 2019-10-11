open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)
let rec eval env cfg = function
        | [] -> cfg
        | hp :: tp ->
            match (hp, cfg) with
            | (BINOP opName, (y :: x :: restStack, stmCfg)) ->
                            eval env (Language.Expr.applyOpTo opName x y :: restStack, stmCfg) tp
            | (CONST x, (stack, stmCfg)) ->
                            eval env (x :: stack, stmCfg) tp
            | (READ, (stack, (st, hi :: ti, o))) ->
                            eval env (hi :: stack, (st, ti, o)) tp
            | (WRITE, (hstack :: tstack, (st, i, o))) ->
                            eval env (tstack, (st, i, o @ [hstack])) tp
            | (LD name, (stack, (st, i, o))) ->
                            eval env (st name :: stack, (st, i, o)) tp
            | (ST name, (hstack :: tstack, (st, i, o))) ->
                            eval env (tstack, (Language.Expr.update name hstack st, i, o)) tp
            | (LABEL name, cfg) ->
                            eval env cfg tp
            | (JMP name, cfg) ->
                            eval env cfg (env#labeled name)
            | (CJMP (cond, name), (hstack :: tstack, stmCfg)) ->
                            eval env cfg (if (hstack = 0 && cond = "z")
                                            then env#labeled name
                                            else tp)

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let labGen =
        object
                val mutable num = 0

                method next =
                        num <- num + 1;
                        "label" ^ string_of_int num
        end

let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)   -> compile s1 @ compile s2
  | Stmt.Read x         -> [READ; ST x]
  | Stmt.Write e        -> expr e @ [WRITE]
  | Stmt.Assign (x, e)  -> expr e @ [ST x]
  | Stmt.Skip           -> []
  | Stmt.If (e, sT, sF) ->
                  let labElse = labGen#next
                   in let labFi = labGen#next
                       in expr e @ [CJMP ("z", labElse)] @
                          compile sT @ [JMP labFi; LABEL labElse] @
                          compile sF @ [LABEL labFi]
  | Stmt.While (e, s)   ->
                  let labDo = labGen#next
                   in let labOd = labGen#next
                       in [LABEL labDo] @ expr e @ [CJMP ("z", labOd)] @
                          compile s @ [JMP labDo; LABEL labOd]
  | Stmt.Until (s, e)   ->
                  let labRepeat = labGen#next
                   in [LABEL labRepeat] @ compile s @ expr e @ [CJMP ("z", labRepeat)] 
