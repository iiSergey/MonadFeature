// Learn more about F# at http://fsharp.org

open System
open System.Threading.Tasks

module IOMonad =
    type IO<'a> = IO of (unit -> 'a)
    
    module IO =
        let run (IO thunk) = thunk()
        let combine io1 io2 = IO (fun () -> run io1; run io2)
        let bind m fn = IO(fun () -> run <| fn (run m ))
        let map io f = IO(fun () -> f (run io))
    
        let (>>=) = bind

    type IoMonadBuilder() =
        //member this.Run (m) = run m
        member this.Return(t) = IO(fun () -> t)
        member inline this.ReturnFrom(t):IO<_> = t
        member this.Bind(m, fn) = IO.bind m fn
        member this.Delay(g) : IO<_> = g ()
        member this.Combine(io1, io2) = IO.combine io1 io2
        member this.Zero () = IO(fun () -> ())
    
    let io =IoMonadBuilder()

module StateMonad =
    type State<'s, 'a> = State of ('s -> ('a * 's))

    module State =
        let Run (State f) state  = f state
        let Return x = State(fun s -> x, s)
        let Zero = State(fun s -> (), s)
        let Bind f xState = 
            let run state = 
                let x, newState = Run xState state
                Run (f x) newState
            State run
        let Combine xStateLeft xStateRight =
            //todo: review
            let run state = 
                let _result, newState = Run xStateLeft state 
                Run xStateRight newState
            State run
        let Get = 
            let run state = 
                state, state
            State run
        let Put newState = 
            let run _ = 
                (), newState
            State run

    type StateBuilder() =
        member this.Zero() = State.Zero
        member this.Return(x) = State.Return x
        member inline this.ReturnFrom (x: State<'s, 'a>) = x
        member this.Bind(xState,f) = State.Bind f xState
        member this.Combine(xStateLeft, xStateRight) = State.Combine xStateLeft xStateRight
        
    let state = new StateBuilder()
    
open IOMonad
open StateMonad
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let readkeyIO = io { 
        let! keyConsole = IO (fun () -> Console.ReadKey(false))
        return keyConsole.Key
        }

    let printfnIO state= io {
        do! IO(fun () -> printfn "%s" state)
    }  
    
    let move drawIO =
        state {
            let! board = State.Get
            let key = IO.run drawIO 
            let st= match key with
            |ConsoleKey.UpArrow ->"+";
            |ConsoleKey.DownArrow ->"-"; 
            |ConsoleKey.Escape -> "exit!"; 
            |_ ->"?"; 
            return! State.Put (board + st)
        }
    let moveUp () =
        state {
            let! board = State.Get
            return! State.Put (board + "+")
        }
 
    let moveDown () =
        state {
            let! board = State.Get
            return! State.Put (board + "-")
        }
        
    let Read info =
        state {
            let! board = State.Get
            return info + board
        }

    let draw drawIO =
        state {
            let! res = Read "play: "
            IO.run (drawIO res)
        }
    
    
    //let aa:State<string,string>= play ()
    //let aaa=State.Run aa "!"
    
         
    let rec game moveIO drawIO state:State<_,string> =    
        let _,drawState =State.Run (draw drawIO) state
        let _,newState=State.Run (move moveIO) drawState
        game moveIO drawIO newState

    game readkeyIO printfnIO "!" |> ignore
        
    0 // return an integer exit code
