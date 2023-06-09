namespace ElmishTicTacToe

open Elmish
open GameState
open GameBoard

module Program =

    [<EntryPoint>]
    let runProgram _ =
        Elmish.Program.mkSimple init update view |> Program.run |> (fun _ -> 0)
