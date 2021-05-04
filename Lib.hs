module Lib where
import System.Random (random,randomR,Random(..),randomRs,mkStdGen,RandomGen)
import Data.Maybe (catMaybes,isNothing,fromJust)
import Data.List (sortOn,nubBy,nub)
import Control.Applicative (ZipList(..))
data CellType  = Mine | Flag | Void | Enumerated Int | Undiscovered deriving Eq

newtype Board = Board [(Cell,CellType)] deriving(Eq)
type Cell = (Int,Int)

instance Show CellType where
  show Flag               = "\x1b[33m" ++ "\ESC[1m" ++ "F" ++ "\ESC[0m" ++ "\x1b[0m"
  show Void               = "_"
  show Undiscovered       = " "
  show Mine               = "\x1b[31m" ++ "\ESC[1m" ++ "X" ++ "\ESC[0m" ++ "\x1b[0m"
  show (Enumerated value) = "\x1b[36m" ++ "\ESC[1m" ++ show value ++ "\ESC[0m" ++ "\x1b[0m"

cellsFromTo :: (Int,Int) -> Cell -> Cell -> [Cell]
cellsFromTo size@(depth,width) cell1@(x1,y1) cell2@(x2,y2)
  | cell1 > size || cell2 > size ||
    cell1 <= (0,0) || cell1 > cell2 = []

  | otherwise = nub $ cellsFromTo' (depth,width) (x1,y1) (x2,y2) where
      cellsFromTo' size@(depth,width) coor1@(x1,y1) coor2@(x2,y2)
        | coor1 == coor2 = [coor1]
        | y1 < width = (x1,y1) : cellsFromTo' (depth,width) (x1,y1+1) (x2,y2)
        | y1 == width = (x1,y1) : cellsFromTo' (depth,width) (x1+1,1) (x2,y2)

randomCells :: RandomGen g => (Int,Int) -> (Cell,Cell) -> g -> [Cell]
randomCells size@(depth,width) (cell1, cell2) g =
  let x = randomR (1,depth*width) g
      cells = cellsFromTo size cell1 cell2
      elemToAppend = cells !! (fst x -1)
  in elemToAppend : randomCells size (cell1,cell2) (snd x)

createCells :: (Int,Int) -> [Cell]
createCells size = cellsFromTo size (1,1) size

createTypeBoard :: (Int,Int) -> CellType -> Board
createTypeBoard size cType = Board $ createCells size `zip` repeat cType

extractCells :: Board -> [Cell]
extractCells (Board list) = map fst list

extractCellType :: Board -> Cell -> CellType
extractCellType (Board list) cell = snd $ head $ filter (\(c,cType)->c==cell) list

hasCell :: Board -> Cell -> Bool
hasCell board@(Board list) cell = elem cell (extractCells board)

sortBoard :: Board -> Board
sortBoard board@(Board list) = Board $ sortOn fst list

countCellType :: Board -> CellType -> Int
countCellType (Board list) cType = foldl (f cType) 0 list where
  f :: CellType -> Int -> (Cell,CellType) -> Int
  f (Enumerated _) n (_,Enumerated _)      = n+1
  f cType1 n (_,cType2) | cType1 == cType2 = n+1
  f _ n _                                  = n

createMines :: RandomGen g => (Int,Int) -> Int -> g -> Maybe [Cell]
createMines (depth,width) nMines g
  | nMines < 0 || nMines > depth*width = Nothing
  | otherwise = createMines' nMines [] 0 where
      createMines' nMines minesCells counter
        | nMines == 0 = Just minesCells
        | otherwise =
            let randoming = randomCells (depth,width) ((1,1),(depth,width)) g
                cellAtPos = randoming !! counter
            in if elem cellAtPos minesCells then createMines' nMines minesCells (counter+1)
               else createMines' (nMines-1) (minesCells ++ [cellAtPos]) (counter+1)

editBoard :: Board -> (Cell,CellType) -> Board
editBoard board@(Board tupleList) (cell,cType) =
  Board $ map (\meow@(cell2,cType2)->if cell == cell2 then (cell,cType) else meow) tupleList
{-editBoard :: Board -> (Cell,CellType) -> Board
editBoard board@(Board tupleList) (cell,cType) =
  sortBoard $ Board $ nubBy (\(cell1,_) (cell2,_) -> cell1 == cell2) ((cell,cType) : tupleList)-}

createInitialBoard :: RandomGen g => (Int,Int) -> Int -> g -> Maybe Board
createInitialBoard size@(depth,width) nMines g =
  let voidBoard  = createTypeBoard size Void
      minesCells = createMines size nMines g
  in if isNothing minesCells then Nothing
     else Just $ foldl editBoard voidBoard (zip (fromJust minesCells) (replicate (depth*width) Mine))

neighbours :: Board -> Cell -> [(Cell,CellType)]
neighbours board@(Board listTuple) cell@(x,y)
  | cell <= (0,0) || cell > (fst.last $ listTuple) = []
  | otherwise = 
      let cells = filter (hasCell board) [(x-1,y-1),
                                          (x-1,y),
                                          (x-1,y+1),
                                          (x,y+1),
                                          (x+1,y+1),
                                          (x+1,y),
                                          (x+1,y-1),
                                          (x,y-1)]
      in map (\cell -> (,) cell (extractCellType board cell)) cells

isSafe :: Board -> Board -> Bool
isSafe board1@(Board list1) board2@(Board list2) =
  let minesList = filter (\(_,cType)->cType == Mine) list1
      flagsList = filter (\(_,cType)->cType == Flag) list2
  in if (null flagsList && not (null minesList)) ||
         length flagsList /= length minesList
        then False
     else
        null $ catMaybes $ getZipList $ f <$> ZipList list1 <*> ZipList list2 where
        f :: (Cell,CellType) -> (Cell,CellType) -> Maybe (Cell,CellType)
        f (cell1,_) (cell2,_)
          | cell1 == cell2 = Nothing
          | otherwise = Just (((-1),(-1)),Mine)

enumerateBoard :: Board -> Board
enumerateBoard board@(Board listTuple) = 
  let noMine = filter (\(cell,cType) -> cType /= Mine) listTuple
      mine = filter (\(cell,cType) -> cType == Mine) listTuple
      enumerate = map f noMine where
        f :: (Cell,CellType) -> (Cell,CellType)
        f (cell,_) =
          if countCellType (Board $ neighbours board cell) Mine == 0 then (cell,Void)
          else (cell,Enumerated (countCellType (Board $ neighbours board cell) Mine))
  in sortBoard $ Board $ enumerate ++ mine

instance Show Board where
  show board@(Board tupleList) =
    let ((depth,width),_) = last tupleList
    in "\ESC[2J" ++ "   " ++ (
         concat [str|w<-[1 .. width] ,
                     let str
                           | w<10      = " " ++ show w ++ "  "
                           | otherwise = " " ++ show w ++ " "]++"\n")
                 ++ concatMap (f (depth,width)) tupleList
      where
        f (depth,width) ((x,y),cType) | x<10 && y==1         = "   " ++ concat (replicate width "|---") ++ "|" ++ "\n" ++
                                                               " " ++ show x  ++ " " ++ "| " ++ show cType  ++ " "

        f (depth,width) ((x,y),cType) | x>=10 && y==1        = "   " ++ concat (replicate width "|---") ++ "|" ++ "\n" ++
                                                               " " ++ show x  ++ "| " ++ show cType  ++ " "

        f (depth,width) ((x,y),cType) | y/=width             = "|" ++ " " ++ show cType  ++ " "

        f (depth,width) ((x,y),cType) | x/=depth && y==width = "|" ++ " " ++ show cType  ++ " " ++ "|" ++ "\n"

        f (depth,width) ((x,y),cType) | x==depth && y==width = "|" ++ " " ++ show cType  ++ " " ++ "|" ++ "\n" ++
                                                               "   " ++ concat (replicate width "|---") ++ "|"

printBoard :: Board -> IO ()
printBoard = print
