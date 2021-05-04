module Minesweeper where
import Lib

import Data.Maybe (isNothing,fromJust)
import Control.Exception (SomeException,throwIO)
import Control.Concurrent
import Control.Monad (guard,when)
import System.Random (mkStdGen)

spreadTheVoid :: Board -> Board -> Cell -> Board
spreadTheVoid board1 board2 cell =
  spreadTheVoid' board1 board2 cell 3 where
  spreadTheVoid' :: Board -> Board -> Cell -> Int -> Board
  spreadTheVoid' board1 board2 cell@(x,y) counter
    | counter == 0 = board2
    | otherwise =
        let listTuple = neighbours board1 cell
            itsVoidNeighbours = filter (\(_,cType) -> cType == Void) listTuple
            newBoard2         = foldl editBoard board2 itsVoidNeighbours
        in foldl (\l r -> spreadTheVoid' board1 l r (counter-1)) newBoard2 $ extractCells $ Board itsVoidNeighbours

minesweeper :: IO ()
minesweeper = do
  putStrLn "Enter the following values as Int:"
  threadDelay 500000

  putStr "depth: "
  depth <- readLn :: IO Int
  putStr "width: "
  width <- readLn :: IO Int
  putStr "number of mines: "
  nMines <- readLn :: IO Int
  putStr "random generator Id: "
  ranGenId <- readLn :: IO Int

  let temp = createInitialBoard (depth,width) nMines (mkStdGen ranGenId)
  when (isNothing temp) $ throwIO (error "Invalid number of mines" :: SomeException) :: IO ()
  let solvedBoard = enumerateBoard $ fromJust temp
      undiscoveredBoard = createTypeBoard (depth,width) Undiscovered
  printBoard undiscoveredBoard

  play solvedBoard undiscoveredBoard (depth,width) where
  play :: Board -> Board -> (Int,Int) -> IO ()
  play board1 board2@(Board listTuple) (depth,width) =
    case countCellType board2 Undiscovered of
      0 -> do printBoard board2
              putStrLn "You won the game!"

      _ ->if (isSafe board1 board2) then do
                printBoard board2
                putStrLn "You won the game!"
          else
           do putStrLn "Make a move"
              putStrLn "Type c/C to unveil cell"
              putStrLn "Type f/F to plant/unplant Flag"
              choice <- getChar
              guard (choice == 'C' || choice == 'c' || choice == 'F' || choice == 'f')
              putStr "\nChoose line: "
              x <- readLn :: IO Int
              putStr "Choose column: "
              y <- readLn :: IO Int
              let cType = extractCellType board1 (x,y)
              case choice of
                   val | (val == 'C' || val == 'c') ->
                         case cType of
                              Mine -> do
                                printBoard board1
                                putStrLn "Unveiling cell, Mine found"
                                putStrLn "You stepped on a Mine!"
                                putStrLn "Game Over!"
                              Enumerated val -> do
                                let newBoard = editBoard board2 ((x,y),Enumerated val)
                                printBoard newBoard
                                putStrLn "Unveiling cell, Enumerated found"
                                play board1 newBoard (depth,width)
                              Void -> do
                                let newBoard = editBoard board2 ((x,y),Void)
                                    spreadVoid = spreadTheVoid board1 newBoard (x,y)
                                printBoard spreadVoid
                                putStrLn "C chosen, VOid found"
                                play board1 spreadVoid (depth,width)
                   val | (val == 'F' || val == 'f') ->
                         let originalCellType = extractCellType board2 (x,y)
                         in case originalCellType of
                                 Flag -> do
                                   let newBoard = editBoard board2 ((x,y),Undiscovered)
                                   printBoard newBoard
                                   putStrLn "Flag unplanted"
                                   play board1 newBoard (depth,width)

                                 Undiscovered -> do
                                   let newBoard = editBoard board2 ((x,y),Flag)
                                   printBoard newBoard
                                   putStrLn "Flag planted"
                                   play board1 newBoard (depth,width)

                                 Void -> do
                                   printBoard board2
                                   putStrLn "You can't plant a flag in an already discovered Void cell!"
                                   play board1 board2 (depth,width)

                                 Enumerated val -> do
                                   putStrLn "You can't plant a flag on an already enumerated cell!"
                                   threadDelay 500000
                                   play board1 board2 (depth,width)
