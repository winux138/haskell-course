foo :: Integer -> Integer
foo 0 = 16
foo 1 
        | "Haskell" > "C++" = 3
        | otherwise         = 4
foo n
        | n < 0            = 0
        | n `mod` 17 == 2  = -43
        | otherwise        = n + 3

hailstone ::Integer -> Integer
hailstone 0 = 0
hailstone n
        | mod n 2 == 0 = div n 2
        | otherwise    = n*3 + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

listLen :: [Integer] -> Integer
listLen [] = 0
listLen (_head:tail) = 1 + listLen tail
