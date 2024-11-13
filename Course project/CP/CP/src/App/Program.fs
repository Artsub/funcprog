module main

open FParsec
open System

type Expression =
    | Int of int                                          
    | Float of float
    | Boolean of bool
    | String of string
    | Application of Expression * Expression
    | Lambda of ID * Expression
    | Variable of ID
    | PFunc of ID
    | Condition of Expression * Expression * Expression
    | Let of ID * Expression * Expression
    | RecursiveLet of ID * Expression * Expression
    | BuiltInOperation of ID * int * list<Expression>
    | Closure of Expression * Environment
    | RecursiveClosure of Expression * Environment * ID
and
    Environment = Map<ID, Expression>
and
    ID = string

let funof = function

    | "+" -> function 
        | [Int(a); Int(b)]     -> Int(a + b)
        | [Float(a); Float(b)] -> Float(a + b)
        | [Float(a); Int(b)]   -> Float(a + float b)
        | [Int(a); Float(b)]   -> Float(float a + b)
        | [String(a); String(b)]   -> String(b + a)
        | _ -> failwith   "Invalid arguments for + operation"

    | "-" -> function
        | [Int(a); Int(b)]     -> Int(b - a)
        | [Float(a); Float(b)] -> Float(b - a)
        | [Float(a); Int(b)]   -> Float(float b - a)
        | [Int(a); Float(b)]   -> Float(b - float a)
        | _ -> failwith   "Invalid arguments for - operation"

    | "*" -> function
        | [Int(a); Int(b)]     -> Int(a * b)
        | [Float(a); Float(b)] -> Float(a * b)
        | [Float(a); Int(b)]   -> Float(a * float b)
        | [Int(a); Float(b)]   -> Float(float a * b)
        | _ -> failwith   "Invalid arguments for - operation"

    | "/" -> function

        | [Int(a); Int(b)]     -> Float(float b / float a)
        | [Float(a); Float(b)] -> Float(b / a)
        | [Float(a); Int(b)]   -> Float(float b / a)
        | [Int(a); Float(b)]   -> Float(b / float a)
        | _ -> failwith   "Invalid arguments for / operation"

    | "==" -> function
        | [Int(a); Int(b)]       -> if a = b then Boolean(true) else Boolean(false)
        | [Float(a); Float(b)]   -> if a = b then Boolean(true) else Boolean(false)
        | [Float(a); Int(b)]     -> if a = (float b) then Boolean(true) else Boolean(false)
        | [Int(a); Float(b)]     -> if (float a) = b then Boolean(true) else Boolean(false)
        | [String(a); String(b)] -> if a = b then Boolean(true) else Boolean(false)
        | _ -> failwith   "Invalid arguments for == operation"

    | ">" -> function
        | [Int(a); Int(b)]       -> if a < b then Boolean(true) else Boolean(false)
        | [Float(a); Float(b)]   -> if a < b then Boolean(true) else Boolean(false)
        | [Float(a); Int(b)]     -> if a < (float b) then Boolean(true) else Boolean(false)
        | [Int(a); Float(b)]     -> if (float a) < b then Boolean(true) else Boolean(false)
        | [String(a); String(b)] -> if a < b then Boolean(true) else Boolean(false)
        | _ -> failwith   "Invalid arguments for > operation"

    | "<" -> function
        | [Int(a); Int(b)]       -> if a > b then Boolean(true) else Boolean(false)
        | [Float(a); Float(b)]   -> if a > b then Boolean(true) else Boolean(false)
        | [Float(a); Int(b)]     -> if a > (float b) then Boolean(true) else Boolean(false)
        | [Int(a); Float(b)]     -> if (float a) > b then Boolean(true) else Boolean(false)
        | [String(a); String(b)] -> if a > b then Boolean(true) else Boolean(false)
        | _ -> failwith   "Invalid arguments for < operation"

    | "<=" -> function
        | [Int(a); Int(b)]       -> if a >= b then Boolean(true) else Boolean(false)
        | [Float(a); Float(b)]   -> if a >= b then Boolean(true) else Boolean(false)
        | [Float(a); Int(b)]     -> if a >= (float b) then Boolean(true) else Boolean(false)
        | [Int(a); Float(b)]     -> if (float a) >= b then Boolean(true) else Boolean(false)
        | [String(a); String(b)] -> if a >= b then Boolean(true) else Boolean(false)
        | _ -> failwith   "Invalid arguments for <= operation"

    | ">=" -> function
        | [Int(a); Int(b)]       -> if a <= b then Boolean(true) else Boolean(false)
        | [Float(a); Float(b)]   -> if a <= b then Boolean(true) else Boolean(false)
        | [Float(a); Int(b)]     -> if a <= (float b) then Boolean(true) else Boolean(false)
        | [Int(a); Float(b)]     -> if (float a) <= b then Boolean(true) else Boolean(false)
        | [String(a); String(b)] -> if a <= b then Boolean(true) else Boolean(false)
        | _ -> failwith   "Invalid arguments for >= operation"

    | _ as something -> failwithf "Invalid funof for %A" something

let rec eval exp env =
    match exp with
        | Int(value) -> Int(value)
        | String(value) -> String(value)
        | Float(value) -> Float(value)
        | Boolean(value) -> Boolean(value)

        | Variable(identifier) -> Map.find identifier env

        | Lambda(_, _) -> Closure(exp, env) 

        | Application(exp1, exp2) -> apply (eval exp1 env) (eval exp2 env)
        | PFunc(id) -> BuiltInOperation(id, 2, []) // Имя операции, её арность, и аргументы операции

        | Condition(condition_expression, if_branch_expression, else_branch_expression) ->
            if (eval condition_expression env) = Boolean(true) then
                eval if_branch_expression env
            else
                eval else_branch_expression env
        
        | Let(id, exp1, exp2) -> 
            eval exp2 (Map.add id (eval exp1 env) env)

        | RecursiveLet(id, exp1, exp2) ->
            eval exp2 (Map.add id (RecursiveClosure(exp1, env, id)) env)
        
        | _ -> failwithf "Invalid eval for EXP = %A\n\nENV = %A" exp env


and apply exp1 exp2 =

    match exp1 with
        | Closure(Lambda(v, e), env) ->
                eval e (Map.add v exp2 env)

        | RecursiveClosure(Lambda(v, e), env, id) ->
                eval e (Map.add v exp2 (Map.add id exp1 env))

        | BuiltInOperation(id, n, args) ->
            if n = 1 then
                (funof id)(exp2 :: args)
            else
                BuiltInOperation(id, n - 1, exp2 :: args)
    
        | _ -> failwithf "Invalid apply for EXP1 = %A\n\nEXP2 = %A" exp1 exp2

let test p str =
    match run p str with
    | Success(result, _, _)   ->
        printfn "Success: %A\n\n\n" result
        printfn "Evaluating ..."
        Some(eval result Map.empty)
    | Failure(errorMsg, _, _) ->
        printfn "Failure: %s" errorMsg
        None


let ws = spaces

let str s = pstring s

let str_ws s = str s .>> ws
let AST_bool  = str_ws "bool" >>. (stringReturn "true"  (Boolean true) <|> stringReturn "false" (Boolean false))
// let AST_false = str_ws "bool" >>. stringReturn "false" (Boolean false)

let AST_float = str_ws "float" >>. ws >>. pfloat .>> ws |>> Float

let AST_int = str_ws "int" >>. ws >>. pint32 .>> ws |>> Int

let AST_identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "Invalid identifier error"
    .>> ws

let AST_Variable = AST_identifier |>> Variable

let AST_ID = AST_identifier |>> ID
let AST_string_literal =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))

let AST_string = str_ws "str" >>. AST_string_literal |>> String


let AST_value, AST_value_ref = createParserForwardedToRef<Expression, unit>()

let AST_operation = pipe3 (str_ws "(" >>. AST_value) 
                          (ws >>. choice [str "+"; str "-"; str "*"; str "/";
                          str "=="; str ">="; str "<="; str ">"; str "<"])
                          (ws >>. AST_value .>> str_ws ")")
                          (fun lhs op rhs -> Application(Application(PFunc(op), lhs), rhs))


let rec make_lambda_by_ID_list (ids: list<ID>) (expr: Expression) = 

    match ids with
        | head :: []   -> Lambda(head, expr)
        | head :: tail -> Lambda(head, make_lambda_by_ID_list tail expr)
        | _ -> failwithf "make lambda error. IDs = %A, Expr = %A" ids expr



let AST_lambda = pipe2
                        (str_ws "lambda" >>. str_ws "(" >>. sepBy AST_ID (str_ws ",") .>> str_ws ")")
                        (str_ws "{" >>. AST_value .>> str_ws "}")
                        (make_lambda_by_ID_list)

let AST_condition = pipe3
                         (str_ws "if" >>. AST_value .>> ws)
                         (str_ws "{" >>. AST_value .>> str_ws "}")
                         (str_ws "else" >>. str_ws "{" >>. AST_value .>> str_ws "}")
                         (fun cond if_expr else_expr -> Condition(cond, if_expr, else_expr))

let AST_let = pipe3 
                    (str_ws "let" >>. AST_ID)
                    (str_ws "=" >>. AST_value .>> ws)
                    (ws >>. AST_value .>> ws)
                    (fun id expr1 expr2 -> Let(id, expr1, expr2))

let AST_rec_let = pipe3 
                    (str_ws "reclet" >>. AST_ID)
                    (str_ws "=" >>. AST_value .>> ws)
                    (ws >>. AST_value .>> ws)
                    (fun id expr1 expr2 -> RecursiveLet(id, expr1, expr2))

let rec execute_function func args =
    match args with
        | head :: []   -> Application(Variable(func), head)
        | head :: tail -> Application(execute_function func tail, head)
        | _ -> failwithf "execute function error. IDs = %A, Expr = %A" func args


let AST_execute = pipe2
                        (str_ws "exec" >>. AST_ID .>> ws)
                        (str_ws "(" >>. sepBy AST_value (str_ws ",") .>> str_ws ")" .>> ws)
                        (fun func args -> execute_function func (List.rev args))

do AST_value_ref := choice [
                    AST_int
                    AST_float
                    AST_string
                    AST_bool

                    AST_execute

                    AST_let
                    AST_rec_let


                    AST_condition

                    AST_lambda

                    AST_Variable



                    AST_operation
                    ]

let main_parser = ws >>. AST_value .>> ws .>> eof


[<EntryPoint>]
let main (args : string[]) =
    if args.Length <> 1 then
        failwith "Error: Expected filename"

    let program = System.IO.File.ReadAllText args.[0]
    let AST = run main_parser program

    let program_result = 
        match AST with
            | Success(parsed_AST, _, _) -> 
                Some(eval parsed_AST Map.empty)
            | Failure(errorMsg, _, _) ->
                printfn "Failure: %s" errorMsg
                None
    
    match program_result with
        | Some (result) -> printfn "%A" result
        | _ -> printfn "Failure: %A" program_result

    0