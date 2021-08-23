-- exercise 1
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (b:bs) = b && myAnd bs
-- myAnd [True] = True
-- myAnd (b:bs) | b == True = myAnd bs
--              | otherwise False
-- myAnd (False:xs) = False
-- myAnd (True:xs) = myAnd xs

myConcat :: [[a]]  -> [a]
myConcat [] = []
myConcat (xs:xss) = xs ++ myConcat xss

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n value = value : myReplicate (n - 1) value

mySelect :: [a] -> Int -> a
mySelect (x:_) 0 = x
mySelect (_:xs) i = mySelect xs (i - 1)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = x == y || myElem x ys
-- myElem value (x:xs) | value == x = True
--                     | otherwise = myElem value xs

-- exercise 2
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x : xs) ys
-- merge (x:xs) ys = smaller ++ [x] ++ merge xs larger
--                   where
--                       smaller = [a | a <- ys, a <= x]
--                       larger  = [b | b <- ys, b > x]

-- exercise 3
halveList :: [a] -> ([a], [a])
halveList xs =
    splitAt halfListLength xs
    where halfListLength = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []     = []
msort [x]    = [x]
msort xs     =
    merge (msort firstHalf) (msort secondHalf)
    where
        halvedList = halveList xs
        firstHalf = fst halvedList
        secondHalf = snd halvedList
{-
msort [3,1,4,2]
merge (msort [3,1]) (msort [4,2])
merge (merge (msort [3]) (msort [1])) (msort [4,2])
merge (merge [3] [1]) (msort [4,2])
merge [1,3] [2,4]
[1,2,3,4]
-}
