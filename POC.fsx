// expression (name | function | application)

// name - some chars, e.g. abc
// function - \name.body, e.g. \x.x
// body - expression

// application - (func-expression argument-expression), e.g. (x y)

// * What does it do? *

// name -> name

// function -> function

// application -> apply function to arg
// 1. if func-expression is not a func, evauate it
// 2. if func-expression is a func, apply it to argument-expression without evaluating arg-expression
// 3. if func-expression is not a func, return the whole thing

type Expression =
    | Name of string
    | Function of string * Expression
    | Application of Expression * Expression

// name -> name

let rec parseFunction (source:string) =
    let functionParts = source.Substring(1).Split('.')
    let functionName = functionParts.[0]
    let functionBody = functionParts.[1]
    Function(functionName, parse functionBody)

and parseFunctionApplication (source:string) =
    let functionApplicationParts = source.Substring(1).Split(' ')
    let functionExpression = parse functionApplicationParts.[0]
    let argumentExpression = parse (functionApplicationParts.[1].Substring(0, functionApplicationParts.[1].Length-1))
    Application(functionExpression, argumentExpression)

and parse (source:string) =  
    match source.[0] with
    | '\\' -> parseFunction source
    | '(' -> parseFunctionApplication source
    | _ -> Name(source)

let rec evaluate tree =
    match tree with
    | Name(x) -> Name(x)
    | Function(name,body) -> Function(name, evaluate body)
    | Application(Function(name,body),argExpr) -> 
        body |> replaceNameWith argExpr name
    | Application(funcExpr,argExpr) ->  Application(funcExpr, argExpr)
and replaceNameWith expr name body =
    match body with
    | Name(x) when x=name -> expr
    | _ -> body

let print expression =
    let rec print' expression =
        match expression with
        | Name(x) -> sprintf "%s" x
        | Function(name,body) -> sprintf "\\%s.%s" name (print' body)
        | Application(funcExpr,argExpr) -> 
            "(" + print' funcExpr + " " + print' argExpr + ")"

    printfn "%s" (print' expression)


"(x arg)"// -> x
|> parse
|> evaluate
|> print 

//let evaluate parsedExpression =
    



