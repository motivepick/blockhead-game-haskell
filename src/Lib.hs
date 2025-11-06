{-# LANGUAGE TupleSections #-}

module Lib (Field, createField, createNewField, makeMove, Difficulty (Easy, Medium, Hard)) where

import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.List (sortBy)
import Dictionary (Dictionary (Dictionary), PrefixDictionary (PrefixDictionary), wordsOfLength)
import Field
import System.Random (randomRIO)

type Path = [Cell]

type WordPath = (String, Path)

type Move = (Cell, Char)

data Difficulty = Easy | Medium | Hard

-- | The bigger the value the easier to play.
wordPickRange :: Difficulty -> Int
wordPickRange Easy = 30
wordPickRange Medium = 15
wordPickRange Hard = 0

createNewField :: Dictionary -> Int -> IO Field
createNewField dictionary size = do
  let initWords = wordsOfLength size dictionary
  initWordIndex <- randomRIO (0, length initWords - 1) :: IO Int
  let initWord = initWords !! initWordIndex
  return (createField size initWord)

reachableCells :: Field -> Cell -> S.HashSet Cell -> [Cell]
reachableCells field cell visited =
  filter isNotVisitedLetter $ field `neighboursOf` cell
  where
    isNotVisitedLetter :: Cell -> Bool
    isNotVisitedLetter c = not (c `S.member` visited) && hasLetter field c

appendCell :: Field -> WordPath -> Cell -> WordPath
appendCell field (word, path) cell = (word ++ [field @ cell], path ++ [cell])

alphabet :: String
alphabet = ['А' .. 'Е'] ++ ['Ё'] ++ ['Ж' .. 'Я']

getAvailableMoves :: Field -> [Move]
getAvailableMoves field = [(cell, letter) | cell <- getAvailableCells field, letter <- alphabet]

getWords :: PrefixDictionary -> Field -> [(Path, String, Move)]
getWords prefixDictionary field = getWords' prefixDictionary field (getAvailableMoves field)

getWords' :: PrefixDictionary -> Field -> [Move] -> [(Path, String, Move)]
getWords' prefixDictionary field moves = mkUniq $ concatMap (getWords'' prefixDictionary field) moves

getWords'' :: PrefixDictionary -> Field -> Move -> [(Path, String, Move)]
getWords'' prefixDictionary field (cell, letter) = map (\(word, path) -> (path, word, (cell, letter))) (getWords''' prefixDictionary fieldAfterMove cell)
  where
    fieldAfterMove = replaceLetter field cell letter

getWords''' :: PrefixDictionary -> Field -> Cell -> [WordPath]
getWords''' prefixDictionary field updatedCell = concatMap (filter (\(_, path) -> updatedCell `elem` path) . paths prefixDictionary field) (cellsWithLetters field)

paths :: PrefixDictionary -> Field -> Cell -> [WordPath]
paths prefixDictionary field start = paths' prefixDictionary field start (S.singleton start) ([field @ start], [start])

paths' :: PrefixDictionary -> Field -> Cell -> S.HashSet Cell -> WordPath -> [WordPath]
paths' prefixDictionary@(PrefixDictionary prefixes) field current visited wordPathSoFar@(word, _)
  | word `S.member` prefixes = wordPathSoFar : concatMap (\cell -> paths' prefixDictionary field cell (cell `S.insert` visited) (appendCell field wordPathSoFar cell)) (reachableCells field current visited)
  | otherwise = []

mkUniq :: (Hashable a) => [a] -> [a]
mkUniq = S.toList . S.fromList

makeMove :: PrefixDictionary -> Dictionary -> Difficulty -> [String] -> Field -> IO (Bool, Field, Path, String, Move)
makeMove prefixDictionary (Dictionary ws) difficulty usedWords field = do
  let foundWords = filter (\(_, w, _) -> w `S.member` ws && (w `notElem` usedWords)) $ getWords prefixDictionary field
  if null foundWords
    then
      return (False, field, [], "", ((0, 0), ' '))
    else do
      let longestWordsFirst = sortBy (\(_, a, _) (_, b, _) -> compare (length b) (length a)) foundWords
      wordIndex <- randomRIO (0, min (wordPickRange difficulty) $ length longestWordsFirst - 1) :: IO Int
      let oneOfLongestWord = longestWordsFirst !! wordIndex
      let (path, word, (cell, letter)) = oneOfLongestWord
      let updatedField = replaceLetter field cell letter
      return (True, updatedField, path, word, (cell, letter))
