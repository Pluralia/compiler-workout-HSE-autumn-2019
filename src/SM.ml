open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP   of string
(* put a constant on the stack     *) | CONST   of int
(* put a string on the stack       *) | STRING  of string                      
(* load a variable to the stack    *) | LD      of string
(* store a variable from the stack *) | ST      of string
(* store in an array               *) | STA     of string * int
(* a label                         *) | LABEL   of string
(* unconditional jump              *) | JMP     of string
(* conditional jump                *) | CJMP    of string * string
(* begins procedure definition     *) | BEGIN   of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL    of string * int * bool
(* returns from a function         *) | RET     of bool with show
                                                   
(* The type for the stack machine program *)
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
*)
type config = (prg * State.t) list * Value.t list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)
let split n l =
  let rec unzip (taken, rest) = function
  | 0 -> (List.rev taken, rest)
  | n -> let h::tl = rest in unzip (h::taken, tl) (n-1)
  in
  unzip ([], l) n

let rec eval env ((ctrl, stack, ((st, i, o) as conf)) as config) = function
        | [] -> config
        | hp :: tp ->
            match hp with
            | BINOP op ->
                            let (y :: x :: stack) = stack in
                            let rx = Value.to_int x in
                            let ry = Value.to_int y in
                            eval env (ctrl, Value.of_int (Expr.to_func op rx ry) :: stack, conf) tp
            | CONST n ->
                            eval env (ctrl, Value.of_int n :: stack, conf) tp
            | STRING str ->
                            eval env (ctrl, Value.of_string str :: stack, conf) tp
            | LD name ->
                            eval env (ctrl, State.eval st name :: stack, conf) tp
            | ST name ->
                            let (hstack :: tstack) = stack in
                            eval env (ctrl, tstack, (State.update name hstack st, i, o)) tp
            | STA (arr, n) ->
                            let (a :: args, restStack) = split (n + 1) stack in
                            eval env (ctrl, restStack, (Stmt.update st arr a (List.rev args), i,o)) tp
            | LABEL _ ->
                            eval env config tp
            | JMP name ->
                            eval env config (env#labeled name)
            | CJMP (cond, name) ->
                            let (hstack :: tstack) = stack in
                            let rhstack = Value.to_int hstack in
                            let prg = if (rhstack = 0 && cond = "z" || rhstack <> 0 && cond = "nz")
                                        then env#labeled name
                                        else tp in
                            eval env (ctrl, tstack, conf) prg
            | BEGIN (_, args, locals) ->
                            let updStack, updSt = State.updateMany args stack
                                                      (State.enter st (args @ locals)) in
                            eval env (ctrl, updStack, (updSt, i, o)) tp
            | END | RET _ ->
                            (match ctrl with
                            | [] ->
                                            config
                            | (p, pSt) :: ctrl ->
                                            eval env (ctrl, stack, (State.leave st pSt, i, o)) p)
            | CALL (name, args, flag) ->
                            if (env#is_label name)
                              then eval env ((tp, st) :: ctrl, stack, conf) (env#labeled name)
                              else eval env (env#builtin config name args flag) tp

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
  let (_, _, (_, _, o)) =
    eval
      (object
         method is_label l = M.mem l m
         method labeled l = M.find l m
         method builtin (cstack, stack, (st, i, o)) f n p =
           let f = match f.[0] with 'L' -> String.sub f 1 (String.length f - 1) | _ -> f in
           let args, stack' = split n stack in
           let (st, i, o, r) = Language.Builtin.eval (st, i, o, None) (List.rev args) f in
           let stack'' = if (not p) then stack' else let Some r = r in r::stack' in
           Printf.printf "Builtin: %s\n";
           (cstack, stack'', (st, i, o))
       end
      )
      ([], [], (State.empty, i, []))
      p
  in
  o

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
        | Expr.Const n ->
                        [CONST n]
        | Expr.Array arr ->
                        List.flatten (List.map expr arr) @ [CALL ("$array", List.length arr, true)]
        | Expr.String str ->
                        [STRING str]
        | Expr.Var x ->
                        [LD x]
        | Expr.Binop (op, x, y) ->
                        expr x @ expr y @ [BINOP op]
        | Expr.Elem (arr, idx) ->
                        expr arr @ expr idx @ [CALL ("$elem", 2, true)]
        | Expr.Length t ->
                        expr t @ [CALL ("$length", 1, true)]
        | Expr.Call (name, args) ->
                        List.fold_left (fun acc e -> expr e @ acc) [CALL (name, List.length args, true)] args

let rec stmt = function
        | Stmt.Seq (s1, s2) ->
                        stmt s1 @ stmt s2
        | Stmt.Assign (x, [], e) ->
                        expr e @ [ST x]
        | Stmt.Assign (x, es, e) ->
                        List.flatten (List.map expr (es @ [e])) @ [STA (x, List.length es)]
        | Stmt.Skip ->
                        []
        | Stmt.If (e, sT, sF) ->
                        let labElse = labGen#next in
                        let labFi = labGen#next in
                        expr e @ [CJMP ("z", labElse)] @
                            stmt sT @ [JMP labFi; LABEL labElse] @
                            stmt sF @ [LABEL labFi]
        | Stmt.While (e, s) ->
                        let labDo = labGen#next in
                        let labOd = labGen#next in
                        [LABEL labDo] @ expr e @ [CJMP ("z", labOd)] @ 
                            stmt s @ [JMP labDo; LABEL labOd]
        | Stmt.Repeat (s, e) ->
                        let labRepeat = labGen#next in
                        [LABEL labRepeat] @ stmt s @ expr e @ [CJMP ("z", labRepeat)]
        | Stmt.Call (name, args) ->
                        List.fold_left (fun acc e -> expr e @ acc) [CALL (name, List.length args, false)] args
        | Stmt.Return optE ->
                        (match optE with
                        | Some e -> expr e @ [RET true]
                        | None -> [RET false])

let rec defs = function
        | (name, (args, locals, body)) :: ds ->
                [LABEL name; BEGIN (name, args, locals)] @ stmt body @ [END] @ defs ds        
        | [] -> []     
                                                         
let rec compile (d, main) =
        stmt main @ [END] @ defs d
