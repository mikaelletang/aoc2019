module Day11

    open System.IO


    let compute_fuel mass =
        mass / 3 - 2

    let compute_all_fuel(masses:StreamReader) =
        masses.ReadToEnd().Split("\n") 
        |> List.ofArray 
        |> List.map(fun mass -> compute_fuel(int(mass))) 
        |> List.reduce(fun acc fuel -> acc + fuel)

    let execute = 
        let url = sprintf "%s/%s" __SOURCE_DIRECTORY__ "../Day1.txt"
        let masses = File.OpenText url
        compute_all_fuel masses

    
