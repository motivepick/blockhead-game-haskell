module Dictionary.Internal where

import qualified Data.HashSet as S
import Paths_blockhead_game (getDataFileName)

newtype Dictionary = Dictionary (S.HashSet String)

newtype PrefixDictionary = PrefixDictionary (S.HashSet String)

readDictionary :: IO Dictionary
readDictionary = do
  dictionaryFileName <- getDataFileName "dictionary.txt"
  contents <- readFile dictionaryFileName
  return $ Dictionary (S.fromList $ lines contents)

toPrefixDictionary :: Dictionary -> PrefixDictionary
toPrefixDictionary (Dictionary ws) = PrefixDictionary (S.fromList $ concatMap prefixes ws)
  where
    prefixes :: String -> [String]
    prefixes word = map (`take` word) [1 .. length word]

wordsOfLength :: Int -> Dictionary -> [String]
wordsOfLength n (Dictionary ws) = filter (\w -> length w == n) (S.toList ws)