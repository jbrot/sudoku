module Sudoku where

import Data.Foldable
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

type Sudoku = V.Vector (U.Vector Int)
type Row = Int
type Col = Int

rowContainsValue :: Row -> Int -> Sudoku -> Bool
rowContainsValue row val = U.any (== val) . (V.! row)

colContainsValue :: Col -> Int -> Sudoku -> Bool
colContainsValue col val = V.any (== val) . fmap (U.! col)

boxContainsValue :: Row -> Col -> Int -> Sudoku -> Bool
boxContainsValue row col val = V.any id . fmap (U.any (== val) . U.slice (col - (col `mod` 3)) 3) .  V.slice (row - (row `mod` 3)) 3

valueFitsSquare :: Row -> Col -> Int -> Sudoku -> Bool
valueFitsSquare r c v p = not $ any (\f -> f v p) [ rowContainsValue r, colContainsValue c, boxContainsValue r c]

solveSquare :: Row -> Col -> Sudoku -> Maybe Sudoku
solveSquare row col p
  | col >= 9                   = solveSquare (row + 1) 0 p
  | row >= 9                   = Just p
  | (p V.! row) U.! col /= 0   = solveSquare row (col + 1) p
  | otherwise                  = asum . fmap update $ [1..9]
     where update :: Int -> Maybe Sudoku
           update n = if valueFitsSquare row col n p 
                         then solveSquare row (col + 1) (p V.// [(row, (p V.! row) U.// [(col, n)])])
                         else Nothing

solve :: Sudoku -> Maybe Sudoku
solve = solveSquare 0 0

fromList :: [[Int]] -> Sudoku
fromList = V.fromList . fmap U.fromList

printS :: Sudoku -> IO ()
printS = putStrLn . format "\n" . V.toList . fmap (format " " . fmap show . U.toList)
    where format tok = foldr1 (combine (tok <> tok)) . fmap (foldr1 (combine tok)) . group 3 
        
          combine tok a b = a <> tok <> b

          group _ [] = []
          group n l  = let (f, s) = splitAt n l in f : group n s
