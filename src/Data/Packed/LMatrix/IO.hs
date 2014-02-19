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

module Data.Packed.LMatrix.IO
       ( readFile
       , writeFile
       ) where

import Prelude hiding (readFile, writeFile)
import qualified Data.Packed as P
import Data.Packed.LMatrix hiding (map, row, rows)
import qualified System.IO as S
import Text.Parsec hiding (label, space)

space :: Parsec String () Char
space = char ' ' <|> char '\t'

colHeader :: Parsec String () [String]
colHeader = do
  optional (many space)
  cs <- sepEndBy1 (many1 letter) (many1 space)
  newline
  return cs

digits :: Parsec String () String
digits = many1 digit

integer :: Parsec String () String
integer = minus <|> digits
  where minus = do m  <- char '-'
                   ds <- digits
                   return (m : ds)

float :: Parsec String () String
float = do
  i <- integer
  decimal <- option "" $ do
    d  <- char '.'
    ds <- digits
    return (d : ds)
  exponent <- option "" $ do
    e    <- char 'e' <|> char 'E'
    sign <- option "" $ fmap (:[]) (char '-' <|> char '+')
    ds   <- digits
    return (e : sign ++ ds)
  return (i ++ decimal ++ exponent)

row :: Parsec String () (String, [Double])
row = do
  optional (many space)
  word <- many1 letter
  many space
  numbers <- sepEndBy1 float (many1 space)
  newline
  return (word, map read numbers)

lMatrix = do
  ch   <- colHeader
  rows <- many1 row
  let rh = map fst rows
      m  = map snd rows
  eof
  return (rh, ch, m)

readFile :: FilePath -> IO (LMatrix Double)
readFile name = do
  s <- S.readFile name
  case parse lMatrix "invalid syntax" s
    of Left e            -> error $ "readFile: " ++ show e
       Right (rh, ch, m) -> return . label rh ch $ P.fromLists m

writeFile :: (P.Element a, Show a) => FilePath -> (LMatrix a) -> IO ()
writeFile name m = S.writeFile name m'
  -- hmatrix does not allow to create empty matrices, so it is safe to
  -- use 'tail' here.
  where m' = tail . dropWhile (/= '\n') $ show m
