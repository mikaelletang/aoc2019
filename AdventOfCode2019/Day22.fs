module Day22

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

    let correct_input noun verb input = 
        input |> replace noun 1 |> replace verb 2

    let execute = 
        executeTest1
        executeTest2
        executeTest3

        let url = sprintf "%s/%s" __SOURCE_DIRECTORY__ "../Day2.txt"
        let input = makeListFromUrl url

        let nouns = [0..99]
        let verbs = [0..99]

        let mutable result = 0
        let mutable noun = 0
        let mutable verb = 0
        let mutable target_noun = -1
        let mutable target_verb = -1
        while noun <= 99 do
            verb <- 0
            while verb <= 99 do
                printfn "%d %d" noun verb
                let corrected_input = correct_input noun verb input
                if (run corrected_input corrected_input = 19690720) then 
                    printfn "found "
                    target_noun <- noun
                    target_verb <-verb

                verb <- verb + 1
            noun <- noun + 1

        100 * target_noun + target_verb
    
