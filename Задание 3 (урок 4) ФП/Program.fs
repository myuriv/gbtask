// Вычисление выражения в обратной польской записи

[<Literal>] 
let Minus = "-"
[<Literal>] 
let Plus = "+"
[<Literal>] 
let Multiplication = "*"
[<Literal>] 
let Division = "/"


// Проверка, что лексема является обозначением операцией
let isOperation (lexeme : string) = Array.contains lexeme [|Minus; Plus; Multiplication; Division|]


// Выполняем операцию и возвращаем список операндов с добавленным результатом
let calcOperation (operandList : double[]) operation : double[] =
    let firstOperand = Array.item (operandList.Length - 2) operandList
    let secondOperand = Array.item (operandList.Length - 1) operandList
    let operationResult = 
        match operation with
        | Plus -> firstOperand + secondOperand
        | Minus -> firstOperand - secondOperand
        | Multiplication -> firstOperand * secondOperand
        | Division -> firstOperand / secondOperand
    let resultOperandList = Array.sub operandList 0 (operandList.Length - 1)
    Array.set resultOperandList (resultOperandList.Length - 1) operationResult
    resultOperandList
    

// Обработка лексемы выражения (при обработке выражения слева направо)
let processLexeme (operandList : double[]) (lexeme : string) =
    if isOperation lexeme then calcOperation operandList lexeme
    else Array.append operandList [|double lexeme|]


// Вычисление выражения, в качестве разделителя должен быть пробел
let calculateExpression (expression : string)  = 
    let rec processExpression (operandList : double[]) (lexemes : string[]) : double[] =
        if lexemes.Length = 0 then operandList
        else 
            let lexeme = Array.item 0 lexemes
            let resultOperandList = processLexeme operandList lexeme
            let cutedLexemeList = Array.sub lexemes 1 (lexemes.Length - 1)
            processExpression resultOperandList cutedLexemeList
     
    let lexemeList = expression.Split [|' '|]
    let singleElementList = processExpression [||] lexemeList
    double (Array.get singleElementList 0)         


[<EntryPoint>]
let main argv =
   //let expression = "25 4 - 3 2 + 5 * +"
   printfn "Введите выражение в польской записи через пробел:"
   let expression = System.Console.ReadLine()
   printfn "Результат: %s" (string (calculateExpression expression))   
   0 


