module Lib (Field, createField, createNewField, makeMove, Difficulty (Easy, Medium, Hard)) where

import Control.Monad (guard)
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
getWords dict field = mkUniq $ do
  move <- getAvailableMoves field
  wordsForMove dict field move

wordsForMove :: PrefixDictionary -> Field -> Move -> [(Path, String, Move)]
wordsForMove dict field move@(cell, letter) = do
  let updatedField = replaceLetter field cell letter
  (word, path) <- wordsTouchingCell dict updatedField cell
  return (path, word, move)

wordsTouchingCell :: PrefixDictionary -> Field -> Cell -> [WordPath]
wordsTouchingCell dict field cell = do
  start <- cellsWithLetters field
  wordPath@(_, path) <- findWordPaths dict field start
  guard (cell `elem` path)
  return wordPath

-- | DFS to find all words starting from a given cell.
-- Returns list of (word, path) pairs.
-- The words are valid prefixes in the prefix dictionary, however, not all prefixes are full words.
findWordPaths :: PrefixDictionary -> Field -> Cell -> [WordPath]
findWordPaths (PrefixDictionary prefixes) field start = dfs start (S.singleton start) ([field @ start], [start])
  where
    dfs :: Cell -> S.HashSet Cell -> WordPath -> [WordPath]
    dfs current visited wordPathSoFar@(word, _)
      | word `S.member` prefixes =
          wordPathSoFar : do
            cell <- reachableCells field current visited
            let newVisited = S.insert cell visited
                newWordPath = appendCell field wordPathSoFar cell
            dfs cell newVisited newWordPath
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
