module Day31

    open System.IO

    type Direction = Right|Down|Up|Left

    type Path = {direction:Direction; length:int}

    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

    let pathFromString pathString =
        match pathString with 
        | Prefix "R" rest -> Some {direction=Right; length=int(rest)}
        | Prefix "D" rest -> Some {direction=Down; length=int(rest)}
        | Prefix "U" rest -> Some {direction=Up; length=int(rest)}
        | Prefix "L" rest -> Some {direction=Left; length=int(rest)}
        | _ -> None

    let extractFromUrl url = 
        let rawInput = (File.OpenText url).ReadToEnd() 
        let rawWires = rawInput.Split("\n") |> List.ofArray
        let wires = rawWires |> List.map(fun(rawWire) -> rawWire.Split(",") |> List.ofArray)
        let wire1String = wires.[0]
        let wire2String = wires.[1]

        let wire1 = wire1String |> List.map(fun(path) -> pathFromString(path))
        let wire2 = wire2String |> List.map(fun(path) -> pathFromString(path))

        (wire1, wire2)

    let execute = 
        let url = sprintf "%s/%s" __SOURCE_DIRECTORY__ "../test31.txt"
        let (wire1, wire2) = extractFromUrl url
        printfn "%A %A" wire1 wire2
        0

    
