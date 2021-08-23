import Distribution.SPDX.LicenseId (LicenseId(XSkat))
double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

-- exercise 2
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- exercise 3
myLast1 xs = head (reverse xs)

-- exercise 4
myLast2 xs = xs !! (length xs - 1)

-- exercise 5
myInit1 xs = take (length xs - 1) xs

myInit2 xs = reverse (tail (reverse xs))
