{-# LANGUAGE TupleSections, MultiWayIf #-}
{-# OPTIONS_GHC -Wall -Wextra #-}
module Lib (solveDet, solveND, isSolved, readGrid, printGrid,
            printGrid', outputGrid, strToGrid) where

import Data.Array
import Data.List
import Control.Monad
import Control.Monad.State
import qualified Data.Set as S
import Data.Char
import Data.Tuple
import Control.Applicative
import qualified Data.Map as M
import Debug.Trace

------------ Datatypes ------------

type Value = Int
type D = S.Set Value
type Grid = Array (Int, Int) D

------------ Utilities ------------

boxed :: Integral a => a -> a
boxed x = ((x - 1) `quot` 3)*3

fixElems :: (Int, Int) -> Grid -> [(Int, Int)]
fixElems (i, j) g =
  filter ((1==) . S.size . (g !)) $ concat [r,c,b]
  where
    bi = boxed i
    bj = boxed j
    b  = [(i', j') | i' <- [bi+1..bi+3], j' <- [bj+1..bj+3]]
    r  = [(i, j') | j' <- [1..bj]++[bj+4..9]]
    c  = [(i', j) | i' <- [1..bi]++[bi+4..9]]

rowFor :: (Num t1, Enum t1) => t -> [(t, t1)]
rowFor i = [(i, j') | j' <- [1..9]]

colFor :: (Num t1, Enum t1) => t -> [(t1, t)]
colFor j = [(i', j) | i' <- [1..9]]

boxFor :: (Integral t1, Integral t) => (t, t1) -> [(t, t1)]
boxFor (i,j) = [(boxed i + i', boxed j + j') | i' <- [1..3], j' <- [1..3]]

isSolved :: Grid -> Bool
isSolved g | allFixed = True -- && constraints = True
          --  | allFixed = error "constraints unsatisfied"
           | otherwise = False
  where
  allFixed = all ((1==) . S.size) . elems $ g
  -- constraints = all (\i -> S.elemAt 0 (g ! i) `S.notMember` fixElems i g) (indices g)

------------ Deterministic solver ------------

obviousConstraints :: Grid -> Maybe Grid
obviousConstraints g
  | null corrections = Nothing
  | otherwise = Just $ g // corrections
  where
    corrections = do
      i <- [1..9]
      j <- [1..9]
      let corr = base S.\\ S.unions (map (g !) $ delete (i,j) $ fixElems (i,j) g)
          base = g ! (i, j)
      when (S.null corr) $ error "constraint failure"
      guard $ corr /= base
      return ((i, j), corr)

rowele, colele, boxele, elef :: [[(Int, Int)]]
rowele = [rowFor i | i <- [1..9]]
colele = [colFor j | j <- [1..9]]
boxele = [boxFor (i,j) | i <- [1,4..9], j <- [1,4..9]]
elef = concat [rowele, colele, boxele]

hiddenGroups :: Grid -> Maybe Grid
hiddenGroups g
  | null corrections = Nothing
  | otherwise = Just $ g // M.toList (M.fromListWith S.intersection corrections)
  where
    corrections = do
      ele <- elef
      let subs = subsequences $ S.toList $ S.unions [ g ! ix | ix <- ele]
      sub <- subs
      guard $ not $ null sub
      let contains = [ix | ix <- ele, any (`S.member` (g ! ix)) sub]
      guard $ length contains == length sub
      ix <- ele
      guard $ S.size (g ! ix) > 1
      let base = g ! ix
          corr | ix `elem` contains = base `S.intersection` S.fromDistinctAscList sub
               | otherwise = base S.\\ S.fromDistinctAscList sub
      -- guard $ not $ S.null corr
      guard $ corr /= base
      return (ix, corr)

boxedGroups :: Grid -> Maybe Grid
boxedGroups g
  | null corrections = Nothing
  | otherwise = Just $ g // M.toList (M.fromListWith S.intersection corrections)
  where
    corrections = do
      ele <- elef
      let subs = S.toList $ S.unions [ g ! ix | ix <- ele]
      sub <- subs
      let contains = [ix | ix <- ele, sub `S.member` (g ! ix)]
      let cSameRow = all ((ci ==) . fst) contains
          cSameCol = all ((cj ==) . snd) contains
          cSameBox = all (\(i,j) -> boxed i == bi && boxed j == bj) contains
          (ci, cj) = head contains
          (bi, bj) = (boxed ci, boxed cj)
          sr | cSameRow = rowFor ci
             | otherwise = []
          sc | cSameCol = colFor cj
             | otherwise = []
          sb | cSameBox = boxFor (ci, cj)
             | otherwise = []
      ix <- map head (group $ sort (sr ++ sc ++ sb)) \\ contains
      guard $ S.size (g ! ix) > 1
      let corr = base S.\\ S.singleton sub
          base = g ! ix
      -- guard $ not $ S.null corr
      guard $ corr /= base
      return (ix, corr)

jellyfish :: Int -> Grid -> Maybe Grid
jellyfish n g
  | null corrections = Nothing
  | otherwise = Just $ g // M.toList (M.fromListWith S.intersection corrections)
  where
  corrections = do
    c <- [1 .. 9]
    is <- idxs
    m <- [id, swap]
    let js = map head $ group [ j | j <- [1..9], i <- is, c `S.member` (g ! m (i, j))]
    guard $ length js == n
    i <- [1..9] \\ is
    j <- js
    let base = g ! m (i, j)
        corr = S.delete c base
    guard $ c `S.member` base
    return (m (i,j), corr)
  idxs = filter ((n==) . length) $ subsequences [1..9]

excludePossibilities :: Grid -> Maybe Grid
excludePossibilities g = foldr1 (<|>) $ map ($ g) $
      [ obviousConstraints
      , hiddenGroups
      , boxedGroups
      ] ++ map jellyfish [2..9]

solveDet :: Grid -> Grid
solveDet g = maybe g solveDet $ excludePossibilities g

------------- Nondet solver --------------

type Sudoku a = StateT Grid [] a

place :: (Int, Int) -> Sudoku ()
place ix = do
  g <- get
  let vs = g ! ix
  when (S.size vs > 1) $ do
    v <- lift $ S.toList vs
    guard $ v `S.notMember` S.unions (map (g!) $ fixElems ix g)
    put $ g // [(ix, S.singleton v)]

solveND :: Grid -> [Grid]
solveND g = flip execStateT g $ forM [(i,j) | i <- [1..9], j <- [1..9]] place

------------ IO ------------

readGrid :: [String] -> Grid
readGrid x = listArray ((1,1),(9,9)) els
  where
    els = concatMap mkr x
    mkr = map mke . (\x' -> if length x' < 9 then x' ++ replicate (9-length x') ' ' else x')
    mke x' | isDigit x' = S.singleton $ read [x']
           | otherwise = S.fromList [1 .. 9]

printGrid :: Grid -> [String]
printGrid g = concatMap (printRow . map (g!)) [rowFor i | i<- [1..9]]

printGrid' :: Grid -> IO ()
printGrid' = putStrLn . unlines . printGrid

printRow :: [D] -> [String]
printRow x = replicate (27+9) '-' : map line [0..2]
  where
    line n = concatMap (printElem n) x
    printElem n el = '|' : concatMap (printI el . (3*n+)) [1..3]
    printI el i | S.member i el = show i
                | otherwise = " "

outputGrid :: Grid -> [String]
outputGrid g = map (printRow' . map (g!)) [rowFor i | i<- [1..9]]
  where
    printRow' = concatMap printEl
    printEl x | S.size x == 1 = show $ S.elemAt 0 x
              | otherwise = "_"

strToGrid :: String -> Grid
strToGrid s = listArray ((1,1),(9,9)) $ map s2e s
  where s2e '0' = S.fromList [1 .. 9]
        s2e x = S.singleton $ read [x]
