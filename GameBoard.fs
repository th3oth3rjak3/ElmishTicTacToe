namespace ElmishTicTacToe

open Types
open GameState

module GameBoard =
    open System

    let gameStatusMessage (state: GameState) =
        match state.GameStatus with
        | InProgress -> $"   Player {state.Player}'s Turn"
        | Tie -> " Tie Game, Try Again!"
        | Winner player -> $"    Player {player} Won!"

    let update (msg: Msg) (state: GameState) : GameState =
        match msg with
        | CellChosen cell -> setValue state { cell with Status = Full state.Player }
        | Instructions message ->
            printfn "\n%s" message
            state
        | Reset ->
            printfn "\n%s" "Resetting the game."
            init ()
        | Exit -> exit 0

    let pipesWithSpaces = "       |     |     \n"

    let pipesWithUnderscores = "  _____|_____|_____\n"

    let getTopRow state =
        state.Cells
        |> List.where (fun ({ Position = (CellPosition position) }) -> position < 3)

    let getMiddleRow state =
        state.Cells
        |> List.where (fun ({ Position = (CellPosition position) }) -> position > 2 && position < 6)

    let getBottomRow state =
        state.Cells
        |> List.where (fun ({ Position = (CellPosition position) }) -> position > 5)

    let leftCellString (updateValue: string) (str: string) = str + sprintf "  %s  |" updateValue

    let rightCellString (updateValue: string) (str: string) = str + sprintf "  %s  \n" updateValue


    let toRowString (cellValues: string list) =
        "  "
        |> leftCellString cellValues[0]
        |> leftCellString cellValues[1]
        |> rightCellString cellValues[2]

    let getRowString (state: GameState) (rowFunc: RowFunction) =
        state |> rowFunc |> getListOfValues |> toRowString


    let printTopRow state =
        pipesWithSpaces + getRowString state getTopRow + pipesWithUnderscores
        |> printf "\n%s"

    let printMiddleRow state =
        pipesWithSpaces + getRowString state getMiddleRow + pipesWithUnderscores
        |> printf "%s"

    let printBottomRow state =
        pipesWithSpaces + getRowString state getBottomRow + pipesWithSpaces
        |> printf "%s"


    let printGameBoard state =
        printTopRow state
        printMiddleRow state
        printBottomRow state

    let getCellByPosition ({ Cells = cells } as state) position =
        cells
        |> List.where (fun cell -> cell.Position = CellPosition position)
        |> fun cellList ->
            match cellList.Length with
            | len when len > 0 -> cellList |> Seq.head |> Some
            | _ -> None

    let tryParseInt s =
        try
            s |> int |> Some
        with :? FormatException ->
            None

    let view state dispatch =
        printfn "\n%s" (gameStatusMessage state)
        printGameBoard state
        printf "\n%s\n%s" "Type 'help' for help." "Choose a cell: "
        let userInput = Console.ReadLine()

        match userInput with
        | "reset" -> dispatch Reset
        | "quit" -> dispatch Exit
        | "exit" -> dispatch Exit
        | "help" ->
            dispatch (Instructions "Type a number from 1-9, 'reset' to reset the game, or 'exit' to quit playing.")
        | _ ->
            userInput
            |> tryParseInt
            |> function
                | Some number ->
                    number - 1
                    |> getCellByPosition state
                    |> function
                        | Some cell ->
                            match state.GameStatus with
                            | InProgress -> dispatch (CellChosen cell)
                            | _ -> dispatch (Instructions "The game is over, please type 'reset' to start again.")
                        | None ->
                            dispatch (Instructions "The position you entered isn't quite right. Please try again.")
                | None -> dispatch (Instructions "Hmm, that doesn't seem to be a valid input. Please try again.")
