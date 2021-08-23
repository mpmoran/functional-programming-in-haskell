--  exercise 1
safetail1 :: Eq a => [a] -> [a]
safetail1 xs = if not (null xs) then tail xs else []

safetail2 :: Eq a => [a] -> [a]
safetail2 xs | not (null xs) = tail xs
             | otherwise      = []

safetail3 :: Eq a => [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

-- exercise 2
or1 :: Bool -> Bool -> Bool
or1 True True = True
or1 True False = True
or1 False True = True
or1 False False = False

or2 :: Bool -> Bool -> Bool
or2 True _ = True
or2 _ True = True
or2 _ _ = False

or3 :: Bool -> Bool -> Bool
or3 False False = False
or3 _ _ = True

-- exercise 3
myAnd :: Bool -> Bool -> Bool
myAnd x y = if x then
                if y then True else False
                else False

-- exercise 4
myAnd2 :: Bool -> Bool -> Bool
myAnd2 x y = if x then y else False
