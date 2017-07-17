module Main where

import Lib
import SudokuPJ
import TestPJ
import Control.Monad
import Data.List
import Data.Maybe

main :: IO ()
main = do
  let
    test1 =
      [" 76     9"
      ,"    12 36"
      ,"42   68 1"
      ,"6   53   "
      ,"3  2 8  5"
      ,"   96   7"
      ,"9 18   24"
      ,"73 62    "
      ,"8     79 "]
    test2 =
      ["5  17 82 "
      ,"1    9 5 "
      ,"  3 5    "
      ,"  7     1"
      ,"  97156  "
      ,"4     9  "
      ,"    4 7  "
      ," 3 6    8"
      ," 46 28  3"]

  putStrLn "========= Deterministic solver ============"
  sudokus <- lines <$> readFile "sudoku17.txt"
  -- let sudokus = [
  --         "000000051020600000000000000070000200300050000000040800501000030400008000000200600"
  --       , "000000072080500000010000000200097000000000100300000000703000060000180500000400000"
  --       ]
  forM_ sudokus $ \sudoku -> do
    let g = strToGrid sudoku
        o = outputGrid g
        so' = run1 Final o
        so = "Success" `isPrefixOf` show so'
        sd = solveDet g
        sg = isSolved sd
    when (so /= sg) $ do
      print (sudoku, so', sg)
      -- printGrid' sd
      -- print $ run1 All $ outputGrid sd
  -- let g = strToGrid "000000071000040000600000000000705000200000600000100300087000050010300000000060400"
  -- putStrLn $ unlines $ map (intersperse '|') $ outputGrid g
  -- let sol = solveDet g
  -- printGrid' sol
  -- print $ run1 All $ outputGrid sol
  -- print $ run allTests
  -- mapM_ (\(n, g) -> print (n, isSolved $ solveDet $ readGrid g)) allTests --printGrid' . solveDet . readGrid . snd) allTests
  -- mapM_ (\(n, g) -> print (n, isSolved $ solveDet $ readGrid g)) allTests --printGrid' . solveDet . readGrid . snd) allTests

  -- putStrLn "========= Non-Deterministic solver ============"
  -- mapM_ printGrid' $ solveND $ readGrid test1
  -- mapM_ printGrid' $ solveND $ readGrid test2

  -- print $ run1 All tst1
