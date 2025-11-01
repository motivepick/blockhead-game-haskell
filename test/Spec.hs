import Field.Internal (createEmptyField, createField, Field, Row(..), FieldRow)
import Test.QuickCheck

rowAt :: Field -> Int -> FieldRow
rowAt [] _ = Row ""
rowAt (x : _) 0 = x
rowAt (_ : xs) n = rowAt xs (n - 1)

countEmptyCells :: Field -> Int
countEmptyCells field = length [c | Row r <- field, c <- r, c == '.']

prop_createEmptyField :: Int -> Property
prop_createEmptyField size = size > 0 ==> length field == size && length (rowAt field 0) == size && countEmptyCells field == size * size
  where
    field = createEmptyField size

prop_createField :: Int -> String -> Property
prop_createField size initWord = size > 0 ==> unRow row == initWord
  where
    row = createField size initWord !! (size `div` 2)
    unRow (Row s) = s

main :: IO ()
main = do
  quickCheck prop_createEmptyField
  quickCheck prop_createField
