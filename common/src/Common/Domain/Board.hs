module Common.Domain.Board (
    createBoard,
    getCellAtIndex,
    whichPlayer,
    whichCell,
    checkBoardForWinner
) where

data Selection = Circle | Cross deriving (Eq, Show)

newtype Cell = Cell (Maybe Selection) deriving (Eq, Show)

type CellIndex = (Int, Int)
instance Monoid CellIndex where
    (a,b) <> (x,y) = (a + x, b + y)

data Board = Board
    { _board_cells :: [[Cell]]
    , _board_width :: Int
    , _board_length :: Int
    }

createBoard :: Int -> Int -> Board
createBoard width length = Board 
    { _board_cells = replicate length $ replicate width (Cell Nothing)
    , _board_width = width
    , _board_length = length
    }

getCellAtIndex :: Board -> CellIndex -> Maybe Cell
getCellAtIndex board (x, y)
    | x >= 0 && x < _board_width board && y >= 0 && y < _board_length board = Just ((_board_cells board !! y) !! x)
    | otherwise = Nothing

whichPlayer :: Maybe Cell -> Maybe Player
whichPlayer (Just (Cell (Just Circle))) = Just Player1
whichPlayer (Just (Cell (Just Cross))) = Just Player2
whichPlayer _ = Nothing

whichCell :: Maybe Player -> Maybe Cell
whichCell (Just Player1) = (Just (Cell (Just Circle)))
whichCell (Just Player2) = (Just (Cell (Just Cross))) 
whichCell _ = Nothing

checkBoardForWinner :: Board -> Maybe Player
checkBoardForWinner board = 
    whichPlayer $ listToMaybe . catMaybes [ checkHorizontals, checkVerticals, checkDiagonalLR, checkDiagonalRL ]
        where
            checkHorizontals = listToMaybe . catMaybes [checkContinuous (0, 0) (1, 0), checkContinuous (0, 1) (1, 0), checkContinuous (0, 2) (1, 0)]
            checkVerticals = listToMaybe . catMaybes [checkContinuous (0, 0) (0, 1), checkContinuous (1, 0) (0, 1), checkContinuous (2, 0) (0, 1)]
            checkDiagonalLR = checkContinuous (0, 0) (1, 1)
            checkDiagonalRL = checkContinuous (2, 2) (-1, -1)
                where 
                    checkContinuous coords offset = aux coords 0
                        where
                            aux coords n
                                | n == 3 = getCellAtIndex board coords
                                | getCellAtIndex board coords == Just (Cell Nothing) = Nothing
                                | getCellAtIndex board coords == getCellAtIndex board (coords <> offset) = aux (coords <> offset) (n + 1)
                                | otherwise = Nothing
