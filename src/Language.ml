(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

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
    let push_scope st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let drop_scope st st' = {st' with g = st.g}

  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    let bti   = function true -> 1 | _ -> 0
    let itb b = b <> 0

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        *, /, %              --- multiplication, division, reminder
    *)                                                  
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

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

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
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters and a body for given definition
    *)
    let rec eval env cfg stm =
         let (st, i, o) = cfg
          in match stm with
             | Read name ->
                             (match i with
                              | hi :: ti -> (State.update name hi st, ti, o)
                              | _        -> failwith "Try read without input")
             | Write expr ->
                             (st, i, o @ [Expr.eval st expr])
             | Assign (name, expr) ->
                             (State.update name (Expr.eval st expr) st, i, o)
             | Seq (stm1, stm2) ->
                             eval env (eval env cfg stm1) stm2
             | Skip ->
                             cfg
             | If (expr, stmT, stmF) ->
                             eval env cfg (
                                     if (Expr.itb (Expr.eval st expr))
                                       then stmT
                                       else stmF)
             | While (expr, stm) ->
                             if (Expr.itb (Expr.eval st expr))
                               then eval env (eval env cfg stm) (While (expr, stm))
                               else cfg
             | Repeat (stm, expr) ->
                             let cfg = eval env cfg stm
                              in let (st, i, o) = cfg
                                  in if (Expr.itb (Expr.eval st expr))
                                       then cfg
                                       else eval env cfg (Repeat (stm, expr))
             | Call (name, argExpr) ->
                             let (argName, locName, stm) = env#definition name
                              in let argVal = List.map (Expr.eval st) argExpr
                                  in let _, updSt = State.updateMany argName argVal
                                                   (State.push_scope st (argName @ locName))
                                      in let (newSt, newI, newO) = eval env (updSt, i, o) stm
                                          in (State.drop_scope newSt st, newI, newO)
                                
    (* Statement parser *)
    ostap (
        parse: seq | atom;

        atom: read | write | assign | skip | ifAtom | whileAtom | untilAtom | forAtom | call;

        read:      "read" "(" x:IDENT ")"             {Read x};
        
        write:     "write" "(" expr:!(Expr.parse) ")" {Write expr};
        
        assign:    x:IDENT ":=" expr:!(Expr.parse)    {Assign (x, expr)};
        
        seq:       stmt1:atom ";" stmt2:parse         {Seq (stmt1, stmt2)};
        
        skip:      "skip"                             {Skip};

        ifAtom:    "if" expr:!(Expr.parse)
                   "then" stmt:!(parse)
                   stmf:!(ifElse)                     {If (expr, stmt, stmf)};

        ifElse:      "elif" expr:!(Expr.parse)
                       "then" stmt:!(parse)
                       stmf:!(ifElse)                 {If (expr, stmt, stmf)}
                   | "else" stmf:!(parse) "fi"        {stmf}
                   | "fi"                             {Skip};

        whileAtom: "while" expr:!(Expr.parse)
                   "do" stm:!(parse) "od"             {While (expr, stm)};

        untilAtom: "repeat" stm:!(parse)
                   "until" expr:!(Expr.parse)         {Repeat (stm, expr)};

        forAtom:   "for" assign:!(parse) ","
                         expr:!(Expr.parse) ","
                         updAssign:!(parse)
                   "do" stm:!(parse) "od"             {Seq (assign,
                                                       While (expr, Seq (stm, updAssign)))};

        call:      name:IDENT
                   "(" args:!(parseArgs) ")"          {Call (name, args)};

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

    let maybe n = function
            | Some a -> a
            | _ -> n

    ostap (
        parse:    "fun" name:IDENT
                  "(" args:parseArgs ")"
                  loc:(-"local" parseArgs)?
                  "{" stm:!(Stmt.parse) "}"          {(name, (args, maybe [] loc, stm))};

        parseArgs:   <x::xs> :!(Util.listBy)
                             [ostap(",")][parseName] {x::xs}
                   | ""                              {[]};
       
        parseName: name:IDENT                        {name}

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
  let m        = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o  = Stmt.eval (object method definition f = snd @@ M.find f m end) (State.empty, i, []) body in o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
