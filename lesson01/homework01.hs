reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:[]) = [x]
reverseList (head:tail) = (reverseList tail) ++ [head]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
        | n > 0 = mod n 10 : toDigitsRev (div n 10)
        | otherwise = []

toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)

doubleEveryOtherFromStart :: [Integer] -> [Integer]
doubleEveryOtherFromStart [] = []
doubleEveryOtherFromStart (n: []) = [n]
doubleEveryOtherFromStart (a: (b: tail)) = a : 2*b : (doubleEveryOtherFromStart tail)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverseList (doubleEveryOtherFromStart (reverseList n))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (head:tail)
        | head > 10 = sumDigits (toDigits head) + sumDigits tail
        | head > 0  = head + sumDigits tail
        | otherwise = sumDigits tail

validate :: Integer -> Bool
validate 0 = False
validate n
        | n > 0     = mod ( sumDigits (doubleEveryOther (toDigits n))) 10 == 0
        | otherwise = False
