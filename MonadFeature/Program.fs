// Learn more about F# at http://fsharp.org

open System


type Io<'a> = IO of (unit -> 'a)

//io monad run
let run (IO thunk) = thunk()
let comb io1 io2 = IO (fun () -> run io1; run io2)

//io monad bind
let (>>=) (m:Io<'a>) (fn: 'a -> Io<'b>): Io<'b> = 
    IO(fun () -> run <| fn (run m ))

type IoMonadBuilder() =
    //member this.Run (m) = run m
    member this.Return(t) = IO(fun () -> t)
    member this.ReturnFrom(t):Io<_> = t
    member this.Bind(m, fn) = (>>=) m fn
    member this.Delay(g) : Io<_> = g ()
    member this.Combine(io1, io2) = comb io1 io2
    
let io =IoMonadBuilder()

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    
    let step1play _ =
        printfn "Hello from step 1!"
        let readkeyIO = fun () -> Console.ReadKey().Key
        let printfnIO state = fun () -> printfn "%s" state
    
        printfn "before readkey press key..."
        let key=readkeyIO ()
        printfn "after readkey key %A" key

        let rec play moveIO riderIO state:string =
            (riderIO "press key up and down. For exit press ESC")()
            (riderIO state)()
            match moveIO () with
            |ConsoleKey.UpArrow ->play moveIO riderIO "+";
            |ConsoleKey.DownArrow ->play moveIO riderIO "-"; 
            |ConsoleKey.Escape -> state; 
            |_ ->play moveIO riderIO "?"; 
        play readkeyIO printfnIO

    (step1play 1) "--------------" |> ignore
    
    let step2play _=
        printfn "Hello from step 2!"
        let readkeyIO = IO (fun () -> Console.ReadKey().Key)
        let readkeyToStringIO key = IO (fun () -> sprintf "%A" key)
        let printfnIO state = IO(fun () -> printfn "%s" state)
        
        
        let getAndPrint = readkeyIO >>= readkeyToStringIO >>= printfnIO
        printfn "run getAndPrint  1 press key..."
        run getAndPrint
        printfn "run getAndPrint  2 press key..."
        run getAndPrint
        
        printfn "before readkey press key..."
        let key= run readkeyIO
        printfn "after readkey key %A" key

        let rec play moveIO riderIO state:string =
            run (riderIO "press key up and down. For exit press ESC")
            run (riderIO state)
            match run moveIO with
            |ConsoleKey.UpArrow ->play moveIO riderIO "+";
            |ConsoleKey.DownArrow ->play moveIO riderIO "-"; 
            |ConsoleKey.Escape -> state; 
            |_ ->play moveIO riderIO  "?"; 
        play readkeyIO printfnIO

    (step2play 1) "--------------" |> ignore
    
    
    let step3play _=
        printfn "Hello from step 3!"
        
        let readkeyIO = IO (fun () -> Console.ReadKey().Key)
        let readkeyToStringIO key = IO (fun () -> sprintf "%A" key)
        let printfnIO state = IO(fun () -> printfn "%s" state)

        let getAndPrint = io { 
            let! key = readkeyIO
            let! keyString = readkeyToStringIO key
            let keyString2 = keyString + keyString
            let! _ = printfnIO keyString2
            return! printfnIO keyString2
            }
        let getAndPrint2 = io {
            let! key1 = readkeyIO
            let! key2 = readkeyIO
            let! keyString1 = readkeyToStringIO key1
            let! keyString2 = readkeyToStringIO key2
            return! printfnIO keyString1
            return! printfnIO keyString2
        }  
        printfn "run getAndPrint  1 press key..."
        printfn "%A" (run <| getAndPrint)
        
        printfn "run getAndPrint  2 press key..."
        printfn "%A" (run <| getAndPrint2)

        printfn "run getAndPrint  3 press key..."
        printfn "%A" (run <| getAndPrint2)
        
        printfn "before fake readkey press key..."
        let key= run (io { return "FAKE" })
        printfn "after fake readkey key %A" key

        let rec play moveIO riderIO state:string =
            run (riderIO "press key up and down. For exit press ESC")
            run (riderIO state)
            match run moveIO with
            |ConsoleKey.UpArrow ->play moveIO riderIO "+";
            |ConsoleKey.DownArrow ->play moveIO riderIO "-"; 
            |ConsoleKey.Escape -> state; 
            |_ ->play moveIO riderIO  "?"; 
        play readkeyIO printfnIO

    (step3play 1) "--------------" |> ignore
    0 // return an integer exit code
