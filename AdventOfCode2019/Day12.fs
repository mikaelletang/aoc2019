module Day12

    open System.IO

    
    let test expected actual = 
        match actual with 
            | actual when actual = expected -> printfn "PASS"
            | _ -> printfn "FAIL, expected %d, got %d" expected actual

    let compute_fuel mass =
        mass / 3 - 2

    let compute_all_fuel_for_vessel (mass:int) = 
        let mutable fuel = compute_fuel mass
        let mutable fuel_bits = [fuel]
        while (fuel > 0) do
            let fuel_bit = compute_fuel fuel
            if fuel_bit > 0 then fuel_bits <- fuel_bit::fuel_bits
            fuel <- fuel_bit

        printfn "Fuel elements : %A" fuel_bits
        fuel_bits |> List.reduce(fun acc fuel -> acc + fuel)

    let compute_all_fuel(masses:StreamReader) =
        masses.ReadToEnd().Split("\n") 
        |> List.ofArray 
        |> List.map(fun mass -> compute_all_fuel_for_vessel(int(mass))) 
        |> List.reduce(fun acc fuel -> acc + fuel)


    let execute = 
        let url = sprintf "%s/%s" __SOURCE_DIRECTORY__ "../Day1.txt"
        let masses = File.OpenText url
        let result = compute_all_fuel masses
        result

    
