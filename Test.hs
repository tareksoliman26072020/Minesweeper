module Test where

import Lib

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (fromJust,fromMaybe)
import System.Random (mkStdGen)
import Data.List (sortOn)

run = defaultMain tests

tests = testGroup "Tests" $ [cellsFromToTests,createCellsTests,createTypeBoardTests,
                             extractCellsTests,extractCellTypeTests,hasCellTests,
                             sortBoardTests,countCellTypeTests]
                             ++
                            [createMinesTests,editBoardTests,createInitialBoardTests]
                             ++
                            [neighboursTests,enumerateBoardTests]
                             ++
                            [isSafeTests]

cells3_3 :: [Cell]
cells3_3 = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]

-- undiscovered 3X3 Board
initialBoard3_3_0 :: Board
initialBoard3_3_0 = Board [((1,1),Undiscovered),((1,2),Undiscovered),((1,3),Undiscovered),((2,1),Undiscovered),((2,2),Undiscovered),((2,3),Undiscovered),((3,1),Undiscovered),((3,2),Undiscovered),((3,3),Undiscovered)]

-- fromJust $ createInitialBoard (3,3) 4 (mkStdGen 1)
initialBoard3_3_1 :: Board
initialBoard3_3_1 = Board [((1,1),Void),((1,2),Void),((1,3),Mine),((2,1),Void),((2,2),Mine),((2,3),Mine),((3,1),Void),((3,2),Mine),((3,3),Void)]

-- fromJust $ createInitialBoard (3,3) 0 (mkStdGen 1)
initialBoard3_3_2 :: Board
initialBoard3_3_2 = Board [((1,1),Void),((1,2),Void),((1,3),Void),((2,1),Void),((2,2),Void),((2,3),Void),((3,1),Void),((3,2),Void),((3,3),Void)]

-- enumerateBoard $ fromJust $ createInitialBoard (3,3) 4 (mkStdGen 1)
board3_3 :: Board
board3_3 = Board [((1,1),Enumerated 1),((1,2),Enumerated 3),((1,3),Mine),((2,1),Enumerated 2),((2,2),Mine),((2,3),Mine),((3,1),Enumerated 2),((3,2),Mine),((3,3),Enumerated 3)]

board3_3_entschaerft :: Board
board3_3_entschaerft = Board [((1,1),Enumerated 1),((1,2),Enumerated 3),((1,3),Flag),((2,1),Enumerated 2),((2,2),Flag),((2,3),Flag),((3,1),Enumerated 2),((3,2),Flag),((3,3),Enumerated 3)]

board3_3_nichtEntschaerft :: Board
board3_3_nichtEntschaerft = Board [((1,1),Enumerated 1),((1,2),Enumerated 3),((1,3),Mine),((2,1),Enumerated 2),((2,2),Flag),((2,3),Flag),((3,1),Enumerated 2),((3,2),Flag),((3,3),Enumerated 3)]

-- neighbours board3_3 (2,2)
neighbours3_3_1 :: [(Cell,CellType)]
neighbours3_3_1 = [((1,1),Enumerated 1),((1,2),Enumerated 3),((1,3),Mine),((2,3),Mine),((3,3),Enumerated 3),((3,2),Mine),((3,1),Enumerated 2),((2,1),Enumerated 2)]

-- neighbours board3_3 (1,1)
neighbours3_3_2 :: [(Cell,CellType)]
neighbours3_3_2 = [((1,2),Enumerated 3),((2,2),Mine),((2,1),Enumerated 2)]

-- neighbours board3_3 (1,3)
neighbours3_3_3 :: [(Cell,CellType)]
neighbours3_3_3 = [((2,3),Mine),((2,2),Mine),((1,2),Enumerated 3)]

-- neighbours board3_3 (3,1)
neighbours3_3_4 :: [(Cell,CellType)]
neighbours3_3_4 = [((2,1),Enumerated 2),((2,2),Mine),((3,2),Mine)]

-- neighbours board3_3 (3,3)
neighbours3_3_5 :: [(Cell,CellType)]
neighbours3_3_5 = [((2,2),Mine),((2,3),Mine),((3,2),Mine)]

-- neighbours board3_3 (1,2)
neighbours3_3_6 :: [(Cell,CellType)]
neighbours3_3_6 = [((1,3),Mine),((2,3),Mine),((2,2),Mine),((2,1),Enumerated 2),((1,1),Enumerated 1)]

-- neighbours board3_3 (2,1)
neighbours3_3_7 :: [(Cell,CellType)]
neighbours3_3_7 = [((1,1),Enumerated 1),((1,2),Enumerated 3),((2,2),Mine),((3,2),Mine),((3,1),Enumerated 2)]

-- neighbours board3_3 (3,2)
neighbours3_3_8 :: [(Cell,CellType)]
neighbours3_3_8 = [((2,1),Enumerated 2),((2,2),Mine),((2,3),Mine),((3,3),Enumerated 3),((3,1),Enumerated 2)]

--neighbours board3_3 (2,3)
neighbours3_3_9 :: [(Cell,CellType)]
neighbours3_3_9 = [((1,2),Enumerated 3),((1,3),Mine),((3,3),Enumerated 3),((3,2),Mine),((2,2),Mine)]

unsortedBoard3_3 :: Board
unsortedBoard3_3 = Board [((1,2),Enumerated 3),((1,1),Enumerated 1),((3,3),Enumerated 3),((2,1),Enumerated 2),((2,2),Mine),((2,3),Mine),((3,1),Enumerated 2),((3,2),Mine),((1,3),Mine)]

-- enumerateBoard $ fromJust $ createInitialBoard (4,4) 4 (mkStdGen 1)
board4_4 :: Board
board4_4 = Board [((1,1),Enumerated 1),((1,2),Mine),((1,3),Enumerated 3),((1,4),Enumerated 2),((2,1),Enumerated 1),((2,2),Enumerated 2),((2,3),Mine),((2,4),Mine),((3,1),Void),((3,2),Enumerated 1),((3,3),Enumerated 3),((3,4),Enumerated 3),((4,1),Void),((4,2),Void),((4,3),Enumerated 1),((4,4),Mine)]

-- board4_4 with Flag at coordinates (2,4)
flaggedBoard :: Board
flaggedBoard = Board [((1,1),Enumerated 1),((1,2),Mine),((1,3),Enumerated 3),((1,4),Enumerated 2),((2,1),Enumerated 1),((2,2),Enumerated 2),((2,3),Mine),((2,4),Flag),((3,1),Void),((3,2),Enumerated 1),((3,3),Enumerated 3),((3,4),Enumerated 3),((4,1),Void),((4,2),Void),((4,3),Enumerated 1),((4,4),Mine)]

----------------------------------------------------------------------------
----------------------------------------------------------------------------

cellsFromToTests :: TestTree
cellsFromToTests = testGroup (replicate 22 ' ' ++ "<<<<<<cellsFromTo>>>>>>")
 [ testCase "Anfangs-Zelle < Ende-Zelle" $
   assertEqual "wrong result" cells3_3 (cellsFromTo (3,3) (1,1) (3,3)),

   testCase "Anfang-Zelle = Ende-Zelle" $
   assertEqual "wrong result" [(2,2)] (cellsFromTo (3,3) (2,2) (2,2)),

   testCase "Anfang-Zelle > Ende-Zelle" $
   assertEqual "wrong result" [] (cellsFromTo (3,3) (2,2) (2,1)),

   testCase "Anfang- oder Ende-Zelle ist nicht im Brett" $
   assertEqual "wrong result" [] (cellsFromTo (3,3) (2,2) (3,4))
 ]

createCellsTests :: TestTree
createCellsTests = testGroup ("\n" ++ replicate 22 ' ' ++ "<<<<<<createCells>>>>>>")
 [ testCase "createCells (2,3)" $
   assertEqual "wrong result" [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)] (createCells (2,3)),

   testCase "createCells (3,2)" $
   assertEqual "wrong result" [(1,1),(1,2),(2,1),(2,2),(3,1),(3,2)] (createCells (3,2))
 ]

createTypeBoardTests :: TestTree
createTypeBoardTests = testGroup ("\n" ++ replicate 22 ' ' ++ "<<<<<<createTypeBoard>>>>>>")
 [ testCase "createTypeBoard (2,2) Mine" $
   assertEqual "wrong result" (Board [((1,1),Mine),((1,2),Mine),((2,1),Mine),((2,2),Mine)]) (createTypeBoard (2,2) Mine)
 ]

extractCellsTests :: TestTree
extractCellsTests = testGroup ("\n" ++ replicate 22 ' ' ++ "<<<<<<extractCells>>>>>>")
 [ testCase "extractCells" $
   assertEqual "wrong result" [(1,1),(1,2)] (extractCells (Board [ ((1,1),Mine) , ((1,2),Enumerated 1) ]))
 ]

extractCellTypeTests :: TestTree
extractCellTypeTests = testGroup ("\n" ++ replicate 22 ' ' ++ "<<<<<<extractCellType>>>>>>")
 [ testCase "extractCellType" $
   assertEqual "wrong result" Mine (extractCellType (Board [ ((1,1),Mine) , ((1,2),Enumerated 1) ]) (1,1))
 ]

hasCellTests :: TestTree
hasCellTests = testGroup ("\n" ++ replicate 22 ' ' ++ "<<<<<<hasCell>>>>>>")
 [ testCase "hasCell ~~> True" $
   assertEqual "wrong result" True (hasCell board3_3 (2,2)),

   testCase "hasCell ~~> False" $
   assertEqual "wrong result" False (hasCell board3_3 (3,4))
 ]

sortBoardTests :: TestTree
sortBoardTests = testGroup ("\n" ++ replicate 22 ' ' ++ "<<<<<<sortBoard>>>>>>")
 [ testCase "sort unsorted Board" $
   assertEqual "wrong result" board3_3 (sortBoard unsortedBoard3_3),

   testCase "sort sorted Board" $
   assertEqual "wrong result" board3_3 (sortBoard board3_3)
 ]

countCellTypeTests :: TestTree
countCellTypeTests = testGroup ("\n" ++ replicate 22 ' ' ++ "<<<<<<countCellType>>>>>>")
 [ testCase "count Mine" $
   assertEqual "wrong result" 4 (countCellType board4_4 Mine),

   testCase "count Void" $
   assertEqual "wrong result" 3 (countCellType board4_4 Void),

   testCase "count Enumerated" $
   assertEqual "wrong result" 9 (countCellType board4_4 (Enumerated 666)), -- Enumerated val, wobei Wert von val nicht relevant ist.

      testCase "count Flag" $
   assertEqual "wrong result" 1 (countCellType flaggedBoard Flag)
 ]

----------------------------------------------------------------------------------------

createMinesTests :: TestTree
createMinesTests = testGroup ("\n" ++ replicate 50 '-' ++ "\n\n" ++ replicate 22 ' ' ++ "<<<<<<createMines>>>>>>")
 [ testCase "createMines (3,3) 3 (mkStdGen 1)" $
   assertEqual "wrong result" [(2,3),(3,2),(2,2)] (fromJust $ createMines (3,3) 3 (mkStdGen 1)),

   testCase "number of mines = 0" $
   assertEqual "wrong result" [] (fromJust $ createMines (3,3) 0 (mkStdGen 1)),

   testCase "number of mines < 0" $
   assertEqual "wrong result" "Nothing" (maybe "Nothing" show $ createMines (3,3) (-1) (mkStdGen 1)),

   testCase "number of mines way too big" $
   assertEqual "wrong result" "Nothing" (maybe "Nothing" show $ createMines (3,3) 10 (mkStdGen 1))
 ]

editBoardTests :: TestTree
editBoardTests = testGroup ("\n" ++ replicate 22 ' ' ++ "<<<<<<editBoard>>>>>>")
 [ testCase "insert flag" $
   assertEqual "wrong result" flaggedBoard (editBoard board4_4 ((2,4),Flag))
 ]

createInitialBoardTests :: TestTree
createInitialBoardTests = testGroup ("\n" ++ replicate 22 ' ' ++ "<<<<<<createInitialBoard>>>>>>")
 [ testCase "createInitialBoard (3,3) 4 (mkStdGen 1)" $
   assertEqual "wrong result" initialBoard3_3_1 (fromJust $ createInitialBoard (3,3) 4 (mkStdGen 1)),
   
   testCase "createInitialBoard (3,3) 0 (mkStdGen 1)" $
   assertEqual "wrong result" initialBoard3_3_2 (fromJust $ createInitialBoard (3,3) 0 (mkStdGen 1)), -- keine Minen

   testCase "createInitialBoard (3,3) 10 (mkStdGen 1)" $
   assertEqual "wrong result" "Nothing" (maybe "Nothing" (\_->"meow") $ createInitialBoard (3,3) 10 (mkStdGen 1)), -- zu viele Minen

   testCase "createInitialBoard (3,3) (-1) (mkStdGen 1)" $
   assertEqual "wrong result" "Nothing" (maybe "Nothing" (\_->"meow") $ createInitialBoard (3,3) (-1) (mkStdGen 1)) -- zu wenig Minen
 ]

----------------------------------------------------------------------------------------

neighboursTests :: TestTree
neighboursTests = testGroup ("\n" ++ replicate 50 '-' ++ "\n\n" ++ replicate 22 ' ' ++ "<<<<<<neighbours>>>>>>")
  [ testCase "has 8 neighbours" $
    assertEqual "wrong result" (sortOn fst neighbours3_3_1) (sortOn fst $ neighbours board3_3 (2,2)),
    
    testCase "has 3 neighbours: upper-left corner" $
    assertEqual "wrong result" (sortOn fst neighbours3_3_2) (sortOn fst $ neighbours board3_3 (1,1)),
    
    testCase "has 3 neighbours: upper-right corner" $
    assertEqual "wrong result" (sortOn fst neighbours3_3_3) (sortOn fst $ neighbours board3_3 (1,3)),

    testCase "has 3 neighbours: lower-left corner" $
    assertEqual "wrong result" (sortOn fst neighbours3_3_4) (sortOn fst $ neighbours board3_3 (3,1)),

    testCase "has 3 neighbours: lower-right corner" $
    assertEqual "wrong result" (sortOn fst neighbours3_3_5) (sortOn fst $ neighbours board3_3 (3,3)),

    testCase "has 5 neighbours: upper wall" $
    assertEqual "wrong result" (sortOn fst neighbours3_3_6) (sortOn fst $ neighbours board3_3 (1,2)),

    testCase "has 5 neighbours: left wall" $
    assertEqual "wrong result" (sortOn fst neighbours3_3_7) (sortOn fst $ neighbours board3_3 (2,1)),

    testCase "has 5 neighbours: lower wall" $
    assertEqual "wrong result" (sortOn fst neighbours3_3_8) (sortOn fst $ neighbours board3_3 (3,2)),

    testCase "has 5 neighbours: right wall" $
    assertEqual "wrong result" (sortOn fst neighbours3_3_9) (sortOn fst $ neighbours board3_3 (2,3))
  ]

enumerateBoardTests :: TestTree
enumerateBoardTests = testGroup ("\n" ++ replicate 22 ' ' ++ "<<<<<<enumerateBoard>>>>>>")
 [ testCase "3X3 Board, 4 mines, g = mkStdGen 1" $
   assertEqual "wrong result" board3_3 (enumerateBoard $ fromJust $ createInitialBoard (3,3) 4 (mkStdGen 1))
 ]

----------------------------------------------------------------------------------------

isSafeTests :: TestTree
isSafeTests = testGroup ("\n" ++ replicate 50 '-' ++ "\n\n" ++ replicate 22 ' ' ++ "<<<<<<isSafe>>>>>>")
 [ testCase "3X3 Board, 4 mines, g = mkStdGen 1, entschärft" $
   assertEqual "wrong result" True (isSafe board3_3 board3_3_entschaerft),

   testCase "3X3 Board, 4 mines, g = mkStdGen 1, undiscovered" $
   assertEqual "wrong result" False (isSafe board3_3 initialBoard3_3_0),

   testCase "3X3 Board, 4 mines, g = mkStdGen 1, nicht Entschärft" $
   assertEqual "wrong result" False (isSafe board3_3 board3_3_nichtEntschaerft)
 ]
