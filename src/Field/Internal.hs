{-# LANGUAGE TupleSections #-}

module Field.Internal where

import qualified Data.Aeson as A
type Cell = (Int, Int)

newtype Row a = Row [a]

instance Foldable Row where
  foldr f z (Row s) = foldr f z s

instance A.ToJSON a => A.ToJSON (Row a) where
  toJSON (Row xs) = A.toJSON xs

instance A.FromJSON a => A.FromJSON (Row a) where
  parseJSON v = do
    xs <- A.parseJSON v
    return (Row xs)

type FieldRow = Row Char
type Field = [FieldRow]

createField :: Int -> String -> Field
createField size initWord = replaceRow (createEmptyField size) (size `div` 2) (Row initWord)

createEmptyField :: Int -> Field
createEmptyField size = replicate size $ Row (replicate size '.')

letterAt :: Field -> Cell -> Char
letterAt field (x, y) = row !! y
  where
    Row row = field !! x

replaceRow :: Field -> Int -> FieldRow -> Field
replaceRow field index newRow = take index field ++ [newRow] ++ drop (index + 1) field

neighboursOf :: Field -> Cell -> [Cell]
neighboursOf field (x, y) = filter (\(a, b) -> 0 <= a && a < length field && 0 <= b && b < length (field !! a)) neighbours
  where
    neighbours = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

replaceLetter :: Field -> Cell -> Char -> Field
replaceLetter field (x, y) letter = replaceRow field x $ replaceLetterInRow (field !! x) y letter

replaceLetterInRow :: FieldRow -> Int -> Char -> FieldRow
replaceLetterInRow row index letter = Row (take index characters ++ [letter] ++ drop (index + 1) characters)
  where
    Row characters = row

hasNeighboursWithLetter :: Field -> Cell -> Bool
hasNeighboursWithLetter field cell = any (hasLetter field) $ field `neighboursOf` cell

isEmpty :: Field -> Cell -> Bool
isEmpty field cell = field `letterAt` cell == '.'

hasLetter :: Field -> Cell -> Bool
hasLetter field cell = not (isEmpty field cell)

getAvailableCells :: Field -> [Cell]
getAvailableCells field = filter (isCellAvailable field) $ allCells field

isCellAvailable :: Field -> Cell -> Bool
isCellAvailable field cell = isEmpty field cell && hasNeighboursWithLetter field cell

allCells :: Field -> [Cell]
allCells field = concatMap (\i -> map (i,) [0 .. length (field !! i) - 1]) [0 .. length field - 1]

cellsWithLetters :: Field -> [Cell]
cellsWithLetters field = filter (hasLetter field) $ allCells field
