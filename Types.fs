namespace ElmishTicTacToe

module Types =

    type PlayerToken =
        | X
        | O

    type CellStatus =
        | Empty
        | Full of PlayerToken

    type CellPosition = CellPosition of int

    type Cell =
        { Status: CellStatus
          Position: CellPosition }

    type CellUpdateResult = { Cell: Cell; Updated: bool }

    type GameStatus =
        | InProgress
        | Tie
        | Winner of PlayerToken

    type GameState =
        { Cells: Cell list
          Player: PlayerToken
          GameStatus: GameStatus }

    type Msg =
        | Reset
        | Exit
        | Instructions of string
        | CellChosen of Cell

    type RowFunction = GameState -> Cell list
