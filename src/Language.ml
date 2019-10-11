(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
       
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

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let i2b i =
            match i with
            | 0 -> false
            | _ -> true
    
    let b2i b =
            match b with
            | false -> 0
            | true  -> 1
    
    let applyOpTo op e1 e2 =
            match op with
            | "+"  -> e1 + e2
            | "-"  -> e1 - e2
            | "*"  -> e1 * e2 
            | "/"  -> e1 / e2
            | "%"  -> e1 mod e2
            | "<"  -> b2i (e1 < e2)
            | "<=" -> b2i (e1 <= e2)
            | ">"  -> b2i (e1 > e2)
            | ">=" -> b2i (e1 >= e2)
            | "==" -> b2i (e1 == e2)
            | "!=" -> b2i (e1 != e2)
            | "&&" -> b2i (i2b e1 && i2b e2)
            | "!!" -> b2i (i2b e1 || i2b e2)
            | und  -> failwith (Printf.sprintf "Undefined operator %s" und)
    
    let rec eval state expr =
            match expr with
            | Const cVal                   -> cVal
            | Var name                     -> state name
            | Binop (opName, expr1, expr2) -> applyOpTo opName (eval state expr1) (eval state expr2)

    (* Expression parser. You can use the following terminals:
         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    ostap (
        parse:
            !(Util.expr
                (fun x -> x)
                [|
                    `Righta , [ ostap ("!!"), (fun e1 e2 -> Binop ("!!", e1, e2))
                              ];
                    `Righta , [ ostap ("&&"), (fun e1 e2 -> Binop ("&&", e1, e2))
                              ];
                    `Nona   , [ ostap ("!="), (fun e1 e2 -> Binop ("!=", e1, e2))
                              ; ostap ("=="), (fun e1 e2 -> Binop ("==", e1, e2))
                              ; ostap (">="), (fun e1 e2 -> Binop (">=", e1, e2))
                              ; ostap (">"),  (fun e1 e2 -> Binop (">" , e1, e2))
                              ; ostap ("<="), (fun e1 e2 -> Binop ("<=", e1, e2))
                              ; ostap ("<"),  (fun e1 e2 -> Binop ("<" , e1, e2))
                              ];
                    `Lefta  , [ ostap ("+"),  (fun e1 e2 -> Binop ("+" , e1, e2))
                              ; ostap ("-"),  (fun e1 e2 -> Binop ("-" , e1, e2))
                              ];
                    `Lefta  , [ ostap ("*"),  (fun e1 e2 -> Binop ("*" , e1, e2))
                              ; ostap ("/"),  (fun e1 e2 -> Binop ("/" , e1, e2))
                              ; ostap ("%"),  (fun e1 e2 -> Binop ("%" , e1, e2))
                              ]
                |]
                primary
            );

        primary:
          x:IDENT        {Var x}
        | n:DECIMAL      {Const n}
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
    (* loop with a post-condition       *) | Until  of t * Expr.t with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval cfg stm =
         let (st, i, o) = cfg
          in match stm with
             | Read name ->
                             (match i with
                              | hi :: ti -> (Expr.update name hi st, ti, o)
                              | _        -> failwith "Try read without input")
             | Write expr ->
                             (st, i, o @ [Expr.eval st expr])
             | Assign (name, expr) ->
                             (Expr.update name (Expr.eval st expr) st, i, o)
             | Seq (stm1, stm2) ->
                             eval (eval cfg stm1) stm2
             | Skip ->
                             cfg
             | If (expr, stmT, stmF) ->
                             eval cfg (
                                     if (Expr.i2b (Expr.eval st expr))
                                       then stmT
                                       else stmF)
             | While (expr, stm) ->
                             if (Expr.i2b (Expr.eval st expr))
                               then eval (eval cfg stm) (While (expr, stm))
                               else cfg
             | Until (stm, expr) ->
                             let cfg = eval cfg stm
                              in let (st, i, o) = cfg
                                  in if (Expr.i2b (Expr.eval st expr))
                                       then cfg
                                       else eval cfg (Until (stm, expr))

    (* Statement parser *)
    ostap (
        parse: seq | atom;

        atom: read | write | assign | skip | ifAtom | whileAtom | untilAtom | forAtom;

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
                   "until" expr:!(Expr.parse)         {Until (stm, expr)};

        forAtom:   "for" assign:!(parse) ","
                         expr:!(Expr.parse) ","
                         updAssign:!(parse)
                   "do" stm:!(parse) "od"             {Seq (assign, While (expr, Seq (stm, updAssign)))}
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
