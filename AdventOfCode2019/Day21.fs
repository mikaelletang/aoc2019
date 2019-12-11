module Day21

    open System.IO

    type Operation = One|Two
    type Intcode = {Operation: int; LeftOperand:int; RightOperand:int; TargetPosition:int}

    
    let test expected actual = 
        match actual with 
            | actual when actual = expected -> printfn "PASS"
            | _ -> printfn "FAIL, expected %d, got %d" expected actual

    let rec replace value position list =
        match position, list with
        | 0, head::rest -> value::rest
        | i, head::rest -> head::replace value (position - 1) rest
        | i, [] -> failwith "index out of range"

    let rec run(program:list<int>) (yet_to_execute:list<int>) = 
        printfn "program : %A, yet to execute : %A" program yet_to_execute
        match yet_to_execute with
        | 1::firstPosition::secondPosition::targetPosition::tail -> 
            let total = program.[firstPosition] + program.[secondPosition]
            let updated_program = replace total targetPosition program
            run updated_program tail
        | 2::firstPosition::secondPosition::targetPosition::tail -> 
            let total = program.[firstPosition] * program.[secondPosition]
            let updated_program = replace total targetPosition program
            run updated_program tail
        | 99::rest ->
            List.head(program)
        | _ -> 
            printfn "ERROR"
            -1


    let makeListFromUrl url = 
        let rawInput = (File.OpenText url).ReadToEnd() 
        rawInput.Split(",") |> List.ofArray |> List.map(int)

    let executeTest1 = 
        let url = sprintf "%s/%s" __SOURCE_DIRECTORY__ "../test211.txt"
        let input = makeListFromUrl url
        let result = run input input
        test 7 result

    let executeTest2 = 
        let url = sprintf "%s/%s" __SOURCE_DIRECTORY__ "../test212.txt"
        let input = makeListFromUrl url
        let result = run input input
        test 10 result

    let executeTest3 = 
        let url = sprintf "%s/%s" __SOURCE_DIRECTORY__ "../test213.txt"
        let input = makeListFromUrl url
        let result = run input input
        test 3500 result

    let execute = 
        executeTest1
        executeTest2
        executeTest3

        let url = sprintf "%s/%s" __SOURCE_DIRECTORY__ "../Day2.txt"
        let input = makeListFromUrl url
        let noun = 12
        let verb = 2
        let corrected_input = input |> replace noun 1 |> replace verb 2
        let result = run corrected_input corrected_input
        result
    
