{-# LANGUAGE CPP #-}
{- hmatrix-labeled: matrices with row and column headers.
   Copyright 2014 Nikita Karetnikov <nikita@karetnikov.org>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, version 3 of the License.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Data.Packed.LMatrix
       ( LMatrix(..)
       , lab, pos
       , row, col, row',col'
       , rowLabels, colLabels
       , lRows, lCols
       , elem, elem'
       , label, unlabel
       , rows, cols
       , (><)
       , trans
       , reshape
       , flatten
       , fromLists
       , toLists
       , buildMatrix
       , (@@>)
       , asRow, asCol
       , fromRows, toRows
       , fromCols, toCols
       -- , fromBlocks, diagBlock, toBlocks, toBlocksEvery, repmat
       , reverseRows, reverseCols
       , subMatrix
       , takeRows, dropRows
       , takeCols, dropCols
       -- , extractRows, diagRect
       , takeDiag
       , map
       -- , mapMatrixWithIndex, mapMatrixWithIndexM, mapMatrixWithIndexM_
       -- , liftMatrix, liftMatrix2, liftMatrix2Auto
       -- , fromArray2D
       ) where

import Prelude hiding (reverse, map, elem)
#if MIN_VERSION_hmatrix(0,17,0)
import qualified Numeric.LinearAlgebra.Data as P
#else
import qualified Data.Packed as P
#endif
import qualified Data.List  as L hiding (elem)

type Row = String
type Col = String

-- | A pair containing the label and the corresponding position.  Both
-- must be unique for this module to work properly.
data Pair = Pair { _lab :: String
                 , _pos :: Int
                 } deriving Show

type LRows = [Pair]
type LCols = [Pair]

-- | Labeled matrix.
data LMatrix a = LMatrix LRows LCols (P.Matrix a)

-- | Find the label matching 'n'.
lab :: Int -> [Pair] -> Maybe String
lab n xs = if null xs'
           then Nothing
           else Just . _lab $ head xs'
  where xs' = flip filter xs $ \p -> _pos p == n

-- | Find the position matching 's'.
pos :: String -> [Pair] -> Maybe Int
pos s xs = if null xs'
             then Nothing
             else Just . _pos $ head xs'
  where xs' = flip filter xs $ \p -> _lab p == s

instance (Show a, P.Element a) => Show (LMatrix a) where
  show lm@(LMatrix _ _ m) =
    "(" ++ show (P.rows m) ++ "><" ++ show (P.cols m) ++ ")\n" ++
    concatMap ((++ "\n") . unwords) m'''
      where maximum' xs = L.maximumBy cmp xs
            cmp x y
              | length x > length y = GT
              | length x < length y = LT
              | otherwise           = compare x y
            rowH    = rowLabels lm
            colH    = colLabels lm
            m'      = L.map (L.map show) (L.transpose $ P.toLists m)
            m''     = ("" : rowH) : (zipWith (:) colH m')
            maxes   = L.map (length . maximum') m''
            lens    = L.map (L.map length) m''
            offsets = zipWith (\m l -> (L.map (flip replicate ' ' . (m-)) l))
                        maxes lens
            m'''    = L.transpose $ zipWith (zipWith (++)) offsets m''

row :: P.Element a => Row -> LMatrix a -> [a]
row s lm@(LMatrix r _ m) =
  case pos s r of
    Nothing -> []
    Just n  -> row' n lm

col :: P.Element a => Col -> LMatrix a -> [a]
col s lm@(LMatrix _ c m) =
  case pos s c of
    Nothing -> []
    Just n  -> col' n lm

row' :: P.Element a => Int -> LMatrix a -> [a]
row' n (LMatrix _ _ m) =
  concat . P.toLists . P.dropRows n $ P.takeRows (n+1) m

col' :: P.Element a => Int -> LMatrix a -> [a]
col' n (LMatrix _ _ m) =
  concat . P.toLists . P.dropColumns n $ P.takeColumns (n+1) m

rowLabels :: LMatrix a -> [String]
rowLabels (LMatrix r _ _) = L.map _lab r

colLabels :: LMatrix a -> [String]
colLabels (LMatrix _ c _) = L.map _lab c

lRows :: LMatrix a -> LRows
lRows (LMatrix r _ _) = r

lCols :: LMatrix a -> LCols
lCols (LMatrix _ c _) = c

elem :: P.Element a => (Row, Col) -> LMatrix a -> Maybe a
elem (r,c) m = case pos c $ lCols m of
  Nothing -> Nothing
  Just n  -> let xs = row r m in
    if n > length xs - 1
    then Nothing
    else Just $ xs !! n

elem' :: P.Element a => (Int, Int) -> LMatrix a -> Maybe a
elem' (r,c) m = let xs = col' c m in
  if r > length xs - 1
  then Nothing
  else Just $ xs !! r

-- | Label the matrix.
label :: [Row] -> [Col] -> P.Matrix a -> LMatrix a
label rs cs m
  | length rs /= P.rows m = error "label: wrong number of rows"
  | length cs /= P.cols m = error "label: wrong number of columns"
  | otherwise = LMatrix (index rs) (index cs) m
    where index xs = zipWith Pair xs $ [0..length xs]

-- | Drop the labels.
unlabel :: LMatrix a -> P.Matrix a
unlabel (LMatrix _ _ m) = m

rows :: LMatrix a -> Int
rows = P.rows . unlabel

cols :: LMatrix a -> Int
cols = P.cols . unlabel

(><) :: P.Element a => Int -> Int -> [Row] -> [Col] -> [a] -> LMatrix a
(><) r c rs cs = label rs cs . (r P.>< c)

trans :: LMatrix a -> LMatrix a
trans (LMatrix r c m) = LMatrix c r $ P.trans m

reshape :: P.Element a => Int -> [Row] -> [Col] -> P.Vector a -> LMatrix a
reshape n rs cs = label rs cs . P.reshape n

flatten :: P.Element a => LMatrix a -> P.Vector a
flatten = P.flatten . unlabel

fromLists :: P.Element a => [Row] -> [Col] -> [[a]] -> LMatrix a
fromLists rs cs = label rs cs . P.fromLists

toLists :: P.Element a => LMatrix a -> [[a]]
toLists = P.toLists . unlabel

buildMatrix :: P.Element a => Int -> Int -> ((Int, Int) -> a)
            -> [Row] -> [Col] -> LMatrix a
buildMatrix r c f rs cs = label rs cs $ P.buildMatrix r c f

(@@>) :: P.Element a => LMatrix a -> (Int,Int) -> a
(@@>) m p = unlabel m P.@@> p

asRow :: P.Element a => P.Vector a -> [Row] -> [Col] -> LMatrix a
asRow v rs cs = label rs cs $ P.asRow v

asCol :: P.Element a => P.Vector a -> [Row] -> [Col] -> LMatrix a
asCol v rs cs = label rs cs $ P.asColumn v

fromRows :: P.Element a => [P.Vector a] -> [Row] -> [Col] -> LMatrix a
fromRows v rs cs = label rs cs $ P.fromRows v

toRows :: P.Element a => LMatrix a -> [P.Vector a]
toRows = P.toRows . unlabel

fromCols :: P.Element a => [P.Vector a] -> [Row] -> [Col] -> LMatrix a
fromCols v rs cs = label rs cs $ P.fromColumns v

toCols :: P.Element a => LMatrix a -> [P.Vector a]
toCols = P.toColumns . unlabel

reverse :: [Pair] -> [Pair]
reverse xs = zipWith Pair (L.map _lab $ L.reverse xs) [0..length xs]

reverseRows :: P.Element a => LMatrix a -> LMatrix a
reverseRows (LMatrix r c m) = LMatrix (reverse r) c $ P.flipud m

reverseCols :: P.Element a => LMatrix a -> LMatrix a
reverseCols (LMatrix r c m) = LMatrix r (reverse c) $ P.fliprl m

subMatrix :: P.Element a => (Int, Int) -> (Int, Int) -> LMatrix a -> LMatrix a
subMatrix (rs,cs) (rd,cd) (LMatrix r c m) =
  LMatrix r' c' $ P.subMatrix (rs,cs) (rd,cd) m
    where renumber ps = zipWith Pair (L.map _lab ps) [0..length ps]
          r' = renumber . drop rs $ take (rs+rd) r
          c' = renumber . drop cs $ take (cs+cd) c

takeRows :: P.Element a => Int -> LMatrix a -> LMatrix a
takeRows n m = subMatrix (0,0) (n,cols m) m

dropRows :: P.Element a => Int -> LMatrix a -> LMatrix a
dropRows n m = subMatrix (n,0) (rows m - n, cols m) m

takeCols :: P.Element a => Int -> LMatrix a -> LMatrix a
takeCols n m = subMatrix (0,0) (rows m,n) m

dropCols :: P.Element a => Int -> LMatrix a -> LMatrix a
dropCols n m = subMatrix (0,n) (rows m, cols m - n) m

takeDiag :: P.Element a => LMatrix a -> P.Vector a
takeDiag (LMatrix _ _ m) = P.takeDiag m

map :: (P.Element a, P.Element b) => (a -> b) -> LMatrix a -> LMatrix b
map f (LMatrix r c m) = LMatrix r c $ P.mapMatrix f m
