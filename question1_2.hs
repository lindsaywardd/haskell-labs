{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
-- Lindsay Ward, 2/19/24
-- will fix compiling issues asap

-- double int fn for map
double :: Int -> Int
double x = 2 * x

partA :: [Int] -> [Int]
partA [] = []
partA(x:xs)
 | x > 0 = map double (x:xs)
 | x < 0 = map double xs
 -- can write map (\x-> 2*x)(x:xs) instead

partB :: [Int] -> [Int]
partB [] = []
partB(x:xs) = filter (\x->x>0 && even x) (x:xs)

partC :: Fractional t => t -> t-> [t]
partC n t = iterate (\x->(x+n/x)/2) t
-- x is initial guess

-- works but not sure fully understand ord, num here
partD :: (Ord a, Num a) => [a] -> a -> a
partD [] _ = 0
partD (x:xt:xs) n = head(filter (\a -> abs(x - xt) <= n) (x:xt:xs))
 {- ignore:
 difference:: a-> a-> Bool
difference a b
 | abs(xt - x) <= n = True
 | otherwise = False -}

 -- wow mess -- cleanup 
partE :: (Fractional t, Ord t) => t -> t -> t
partE 0 _ = 0 -- not nec.
partE x t = head (filter (\(a b)-> abs (a - b) <= t) (zip (partC x 1) (tail (partC x 1))))
-- cannot figure out (a,b) error for life of me
-- paranth problems almost def aru

partF :: (a-> a) -> [a] -> [a]
partF fs xs = map fs xs

partG :: a -> [(a->b)] -> [b]
partG _ [] = []
partG x (f:fs) = map (\f ->f x) fs

partH :: Eq a=> a-> [a] -> [a]
partH _ [] = []
partH n x = filter (/=n) x
-- type dec...update to match orig prompt

partI :: Eq a => [a] -> [a] -> [a]
partI [][] = []
partI [] _ = []
partI _ [] = []
partI (a:as) bs = filter (/=a) bs ++ partI as bs
-- does not use H
 -- | a == b = (a, dropWhile (==a) b) :partI as bs

partJ :: [a] -> [[a]]
partJ (x:xs) = foldr foldFn1 [[]] xs-- [[]] start with empty list

partK :: [a] -> [[a]]
partK [] = [[]]
partK (x:xs) = foldr foldFn2 [[]] xs

foldFn1 :: a -> [[a]] -> [[a]]
foldFn1 x a = [] : map(x:) a

foldFn2 :: a -> [[a]] -> [[a]]
foldFn2 x z = z ++ map(x:) z