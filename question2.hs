
-- foldr to pos inf so try first
mySort :: [Int] -> [Int]
mySort [] = []
mySort list = foldr insertionSort [] list
-- empty list so doesn't acc?

insertionSort :: Ord a=>a -> [a] -> [a]
insertionSort toInsert [] = [toInsert]
insertionSort toInsert (x:xs)
 | toInsert >= x = x:toInsert : xs
 | otherwise = toInsert:x : xs
 -- i think ord means orderable check that tho