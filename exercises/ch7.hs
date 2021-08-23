-- exercise 2
-- map f (filter p xs)

-- exercise 3
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x xs -> f x : xs) [] xs

myFilter:: (a -> Bool) -> [a] -> [a]
myFilter p xs = foldr (\x xs -> if p x then x : xs else xs) [] xs
