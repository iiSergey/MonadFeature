// Learn more about F# at http://fsharp.org

open System

type IoStep2<'a> =  IO of (unit -> 'a)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    
    let step1play=
        let readkeyIO = fun () -> Console.ReadKey().Key
        let printfnIO state = fun () -> printfn "%s" state
    
        printfn "before readkey"
        let key=readkeyIO ()
        printfn "after readkey%A" key

        let rec play moveIO state:string =
            (printfnIO state)()
            match moveIO () with
            |ConsoleKey.UpArrow ->play moveIO "+";
            |ConsoleKey.DownArrow ->play moveIO "-"; 
            |ConsoleKey.Escape -> state; 
            |_ ->play moveIO  "?"; 
        play readkeyIO

    step1play "--------------" |> ignore
    
    let step2play=
        let readkeyIO = IO (fun () -> Console.ReadKey().Key)
        let readkeyToStringIO key = IO (fun () -> sprintf "%A" key)
        let printfnIO state = IO(fun () -> printfn "%s" state)
        
        //io monad run
        let run (IO thunk) = thunk()
        //io monad bind
        let (>>=) (m:IoStep2<'a>) (fn: 'a -> IoStep2<'b>): IoStep2<'b> = IO(fun () -> run <| fn (run m ))

        let getAndPrint = readkeyIO >>= readkeyToStringIO >>= printfnIO
        printfn "run 1"
        run getAndPrint
        printfn "run 2"
        run getAndPrint

        printfn "before readkey"
        let key= run readkeyIO
        printfn "after readkey%A" key

        let rec play moveIO riderIO state:string =
            run (riderIO state)
            match run moveIO with
            |ConsoleKey.UpArrow ->play moveIO riderIO "+";
            |ConsoleKey.DownArrow ->play moveIO riderIO "-"; 
            |ConsoleKey.Escape -> state; 
            |_ ->play moveIO riderIO  "?"; 
        play readkeyIO printfnIO

    step2play "--------------" |> ignore
    0 // return an integer exit code
