(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators

(* Values *)
module Value =
  struct

    @type t = Int of int | String of string | Array of t list with show

    let to_int = function 
    | Int n -> n 
    | _ -> failwith "int value expected"

    let to_string = function 
    | String s -> s 
    | _ -> failwith "string value expected"

    let to_array = function
    | Array a -> a
    | _       -> failwith "array value expected"

    let of_int    n = Int    n
    let of_string s = String s
    let of_array  a = Array  a

    let update_string s i x = String.init (String.length s) (fun j -> if j = i then x else s.[j])
    let update_array  a i x = List.init   (List.length a)   (fun j -> if j = i then x else List.nth a j)

  end
       
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> Value.t; l : string -> Value.t; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Update many: simple update for many variables in xs to values vs; values can be from stack so
       it returns pair: rest values and new state
    *)
    let rec updateMany xs vs s =
            match xs, vs with
            | x :: xs, v :: vs -> updateMany xs vs (update x v s)
            | [], vs -> vs, s
            | _ :: _, [] -> failwith (Printf.sprintf "Need more values for variables")

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end

(* Builtins *)
module Builtin =
  struct

    let eval (st, i, o, _) args = function
    | "read"     -> (match i with z::i' -> (st, i', o, Some (Value.of_int z)) | _ -> failwith "Unexpected end of input")
    | "write"    -> (st, i, o @ [Value.to_int @@ List.hd args], None)
    | "$elem"    -> let [b; j] = args in
                    (st, i, o, let i = Value.to_int j in
                               Some (match b with
                                     | Value.String s -> Value.of_int @@ Char.code s.[i]
                                     | Value.Array  a -> List.nth a i
                               )
                    )         
    | "$length"  -> (st, i, o, Some (Value.of_int (match List.hd args with Value.Array a -> List.length a | Value.String s -> String.length s)))
    | "$array"   -> (st, i, o, Some (Value.of_array args))
    | "isArray"  -> let [a] = args in (st, i, o, Some (Value.of_int @@ match a with Value.Array  _ -> 1 | _ -> 0))
    | "isString" -> let [a] = args in (st, i, o, Some (Value.of_int @@ match a with Value.String _ -> 1 | _ -> 0))

    let isBuiltin = function
            | "read" | "write" | "$elem" | "$length" | "$array" | "isArray" | "isString" -> true
            | _ -> false
       
  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant   *) | Const  of int
    (* array              *) | Array  of t list
    (* string             *) | String of string
    (* S-expressions      *) | Sexp   of string * t list
    (* variable           *) | Var    of string
    (* binary operator    *) | Binop  of string * t * t
    (* element extraction *) | Elem   of t * t
    (* length             *) | Length of t 
    (* function call      *) | Call   of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * Value.t option
                                      
    let bti   = function true -> 1 | _ -> 0
    let itb b = b <> 0

    let to_func op =
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)
                      
    (* Expression evaluator

          val eval : env -> config -> t -> int * config

       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns a pair: the return value for the call and the resulting configuration
    *)
    let rec eval env ((st, i, o, r) as conf) = function
(*
    (* integer constant   *) | Const  of int
    (* array              *) | Array  of t list
    (* string             *) | String of string
    (* S-expressions      *) | Sexp   of string * t list
    (* variable           *) | Var    of string
    (* binary operator    *) | Binop  of string * t * t
    (* element extraction *) | Elem   of t * t
    (* length             *) | Length of t 
    (* function call      *) | Call   of string * t list with show
*)
            | Const i ->
                            (st, i, o, Some (Value.of_int i))
            | Array tlist ->
                            let ((st, i, o, _), evaledTlist) = List.fold_left
                                (fun (conf, acc) x ->
                                        let (_, _, _, Some vx) as conf' = eval env conf x in
                                        (conf', acc @ [vx]))
                                (conf, []) tlist
                            in
                            (st, i, o, Some (Value.of_array evaledTlist))
            | String str ->
                            (st, i, o, Some (Value.of_string str))
            | Sexp (str, tlist) ->
                            failwith (Printf.sprintf "expr.eval: s-expr was not implemented")
            | Var name ->
                            (st, i, o, Some (State.eval st name))
            | Binop (op, x, y) ->
                            let (_, _, _, Some rx) as conf' = eval env conf x in
                            let vrx = Value.to_int rx in
                            let (st'', i'', o'', Some ry) = eval env conf' y in
                            let vry = Value.to_int ry in
                            (st'', i'', o'', Some (Value.of_int (to_func op vrx vry)))
            | Elem (t1, t2) ->
                            failwith (Printf.sprintf "expr.eval: elem was not implemented")
            | Length t ->
                            failwith (Printf.sprintf "epxr.eval: length was not implemented")
            | Call (name, args) ->
                            let ((st, i, o, _), optArgs) = List.fold_left
                                (fun (conf, acc) x ->
                                        let (_, _, _, Some rx) as conf' = eval env conf x in
                                        (conf', acc @ [rx]))
                                (conf, []) args
                            in
                            env#definition env name optArgs conf
    and eval_list env conf xs =
      let vs, (st, i, o, _) =
        List.fold_left
          (fun (acc, conf) x ->
             let (_, _, _, Some v) as conf = eval env conf x in
             v::acc, conf
          )
          ([], conf)
          xs
      in
      (st, i, o, List.rev vs)
         
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string                                                                                                                  
    *)
    ostap (                                      
      parse:
  	  !(Ostap.Util.expr 
             (fun x -> x)
      	     (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |] 
             )
             primary);

      parseArgs:   <x::xs> :!(Util.listBy)[ostap(",")][parse] {x::xs}
            | ""                                              {[]};
      
      primary:
        name:IDENT "(" args:parseArgs ")" {Call (name, args)}
      | n:DECIMAL                         {Const n}
      | x:IDENT                           {Var x}
      | -"(" parse -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    type t =
    (* assignment                       *) | Assign of string * Expr.t list * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* return statement                 *) | Return of Expr.t option
    (* call a procedure                 *) | Call   of string * Expr.t list
                                                                    
    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment is the same as for expressions
    *)

    let update st x v is =
      let rec update a v = function
      | []    -> v           
      | i::tl ->
          let i = Value.to_int i in
          (match a with
           | Value.String s when tl = [] -> Value.String (Value.update_string s i (Char.chr @@ Value.to_int v))
           | Value.Array a               -> Value.Array  (Value.update_array  a i (update (List.nth a i) v tl))
          ) 
      in
      State.update x (match is with [] -> v | _ -> update (State.eval st x) v is) st
          
    let meta_op s1 = function
            | Skip -> s1
            | s2 -> Seq (s1, s2)

    let rec eval env ((st, i, o, r) as conf) k stm =
            match k, stm with
            | Skip, Skip ->
                            conf
            | k, Skip ->
                            eval env conf Skip k
            | k, Assign (name, _, x) ->
                            let (st, i, o, Some rx) = Expr.eval env conf x in
                            eval env (State.update name rx st, i, o, None) Skip k
            | k, Seq (stm1, stm2) ->
                            eval env conf (meta_op stm2 k) stm1
            | k, If (cond, stmT, stmF) ->
                            let (st, i, o, Some rx) = Expr.eval env conf cond in
                            let cond = Expr.itb (Value.to_int rx) in
                            eval env (st, i, o, None) k (if cond then stmT else stmF)
            | k, (While (cond, stm) as wStm) ->
                            let (st, i, o, Some rx) = Expr.eval env conf cond in
                            let cond = Expr.itb (Value.to_int rx) in
                            if cond
                              then eval env (st, i, o, None) (meta_op wStm k) stm
                              else eval env (st, i, o, None) Skip k
            | k, (Repeat (stm, cond) as rStm) ->
                            let ifStm = If (Binop ("==", cond, Const 0), rStm, Skip) in
                            eval env conf (meta_op ifStm k) stm
            | k, Call (name, args) ->
                            let ((st, i, o, _), optArgs) = List.fold_left
                                (fun (conf, acc) x ->
                                        let (_, _, _, Some rx) as conf' = Expr.eval env conf x in
                                        (conf', acc @ [rx]))
                                (conf, [])
                                args in
                            eval env (env#definition env name optArgs (st, i, o, None)) Skip k
            | _, Return r ->
                            match r with
                            | None -> conf
                            | Some r -> Expr.eval env conf r
         
    (* Statement parser *)
    ostap (
      parse: seq | atom;

        atom: assign | skip | ifStm | whileStm | untilStm | forStm | call | return;

        assign:    x:IDENT ":=" expr:!(Expr.parse)    {Assign (x, [], expr)};

        seq:       stmt1:atom ";" stmt2:parse         {Seq (stmt1, stmt2)};

        skip:      "skip"                             {Skip};

        ifStm:     "if" expr:!(Expr.parse)
                   "then" stmt:!(parse)
                   stmf:!(ifElse)                     {If (expr, stmt, stmf)};

        ifElse:      "elif" expr:!(Expr.parse)
                       "then" stmt:!(parse)
                       stmf:!(ifElse)                 {If (expr, stmt, stmf)}
                   | "else" stmf:!(parse) "fi"        {stmf}
                   | "fi"                             {Skip};

        whileStm:  "while" expr:!(Expr.parse)
                   "do" stm:!(parse) "od"             {While (expr, stm)};

        untilStm:  "repeat" stm:!(parse)
                   "until" expr:!(Expr.parse)         {Repeat (stm, expr)};

        forStm:    "for" assign:!(parse) ","
                         expr:!(Expr.parse) ","
                         updAssign:!(parse)
                   "do" stm:!(parse) "od"             {Seq (assign,
                                                       While (expr, Seq (stm, updAssign)))};

        call:      name:IDENT
                   "(" args:!(parseArgs) ")"          {Call (name, args)};

        return:    "return" ret:!(Expr.parse)?        {Return ret};

        parseArgs:   <x::xs> :!(Util.listBy)
                             [ostap(",")][Expr.parse] {x::xs}
                   | ""                               {[]}
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      arg  : IDENT;
      parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")"
         locs:(%"local" !(Util.list arg))?
        "{" body:!(Stmt.parse) "}" {
        (name, (args, (match locs with None -> [] | Some l -> l), body))
      }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args ((st, i, o, r) as conf) =
           try
             let xs, locs, s      =  snd @@ M.find f m in
             let st'              = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args) in
             let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
             (State.leave st'' st, i', o', r')
           with Not_found -> Builtin.eval conf args f
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
