import Field.Internal (createEmptyField, createField, Field)
import Test.QuickCheck

row :: Field -> Int -> String
row [] _ = []
row (x : _) 0 = x
row (_ : xs) n = row xs (n - 1)

prop_createEmptyField :: Int -> Property
prop_createEmptyField size = size > 0 ==> length field == size && length (row field 0) == size && length (concatMap (filter (== '.')) field) == size * size
  where
    field = createEmptyField size

prop_createField :: Int -> String -> Property
prop_createField size initWord = size > 0 ==> createField size initWord !! (size `div` 2) == initWord

main :: IO ()
main = do
  quickCheck prop_createEmptyField
  quickCheck prop_createField
