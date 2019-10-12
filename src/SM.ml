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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Stmt.config

(* Stack machine interpreter
     val eval : env -> config -> prg -> config
   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)
let rec eval env cfg = function
        | [] -> cfg
        | hp :: tp ->
            match hp, cfg with
            | BINOP opName, (ctrl, y :: x :: stack, stmCfg) ->
                            eval env (ctrl, Expr.to_func opName x y :: stack, stmCfg) tp
            | CONST x, (ctrl, stack, stmCfg) ->
                            eval env (ctrl, x :: stack, stmCfg) tp
            | READ, (ctrl, stack, (st, hi :: ti, o)) ->
                            eval env (ctrl, hi :: stack, (st, ti, o)) tp
            | WRITE, (ctrl, hstack :: tstack, (st, i, o)) ->
                            eval env (ctrl, tstack, (st, i, o @ [hstack])) tp
            | LD name, (ctrl, stack, (st, i, o)) ->
                            eval env (ctrl, State.eval st name :: stack, (st, i, o)) tp
            | ST name, (ctrl, hstack :: tstack, (st, i, o)) ->
                            eval env (ctrl, tstack, (State.update name hstack st, i, o)) tp
            | LABEL name, cfg ->
                            eval env cfg tp
            | JMP name, cfg ->
                            eval env cfg (env#labeled name)
            | CJMP (cond, name), (_, hstack :: _, _) ->
                            eval env cfg (if (hstack = 0 && cond = "z" || hstack <> 0 && cond = "nz")
                                            then env#labeled name
                                            else tp)
            | BEGIN (args, locals), (ctrl, stack, (st, i, o)) ->
                            let updStack, updSt = State.updateMany args stack
                                                      (State.push_scope st (args @ locals))
                             in eval env (ctrl, updStack, (updSt, i, o)) tp 
            | END, (ctrl, stack, (st, i, o)) ->
                           (match ctrl with
                            | [] -> cfg
                            | (p, pSt) :: ctrl ->
                                           eval env (ctrl, stack, (State.drop_scope st pSt, i, o)) p)
            | CALL name, (ctrl, stack, (st, i, o)) ->
                            eval env ((tp, st) :: ctrl, stack, (st, i, o)) (env#labeled name)

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
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler
     val compile : Language.t -> prg
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

let rec expr = function
| Expr.Var   x          -> [LD x]
| Expr.Const n          -> [CONST n]
| Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]

let rec stmt = function
| Stmt.Seq (s1, s2)   -> stmt s1 @ stmt s2
| Stmt.Read x         -> [READ; ST x]
| Stmt.Write e        -> expr e @ [WRITE]
| Stmt.Assign (x, e)  -> expr e @ [ST x]
| Stmt.Skip           -> []
| Stmt.If (e, sT, sF) ->
                let labElse = labGen#next
                 in let labFi = labGen#next
                     in expr e @ [CJMP ("z", labElse)] @
                        stmt sT @ [JMP labFi; LABEL labElse] @
                        stmt sF @ [LABEL labFi]
| Stmt.While (e, s) ->
                let labDo = labGen#next
                 in let labOd = labGen#next
                     in [LABEL labDo] @ expr e @ [CJMP ("z", labOd)] @
                        stmt s @ [JMP labDo; LABEL labOd]
| Stmt.Repeat (s, e) ->
                let labRepeat = labGen#next
                 in [LABEL labRepeat] @ stmt s @ expr e @ [CJMP ("z", labRepeat)]
| Stmt.Call (name, eL) ->
                List.fold_right (fun e acc -> expr e @ acc) eL [CALL name]

let rec defs = function
        | (name, (args, locals, body)) :: ds ->
                [LABEL name; BEGIN (args, locals)] @ stmt body @ [END] @ defs ds
        | [] -> []

let rec compile (d, main) =
        let labMain = labGen#next
         in [JMP labMain] @ defs d @ [LABEL labMain] @ stmt main

