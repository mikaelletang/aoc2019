module Tools

    let test expected actual = 
        match actual with 
            | actual when actual = expected -> printfn "PASS"
            | _ -> printfn "FAIL, expected %d, got %d" expected actual