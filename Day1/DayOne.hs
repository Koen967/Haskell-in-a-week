module DayOne where

    allEven :: [Integer] -> [Integer]
    allEven [] = []
    allEven (h:t) = if even h then h:allEven t else allEven t

    allEvenMod :: [Integer] -> [Integer]
    allEvenMod [] = []
    allEvenMod (h:t) = if h `mod` 2 == 0 then h:allEvenMod t else allEven t

    allEvenComp :: [Integer] -> [Integer]
    allEvenComp xs = [x | x <- xs, even x]

    allEvenFilter :: [Integer] -> [Integer]
    allEvenFilter xs = filter even xs

    reverseList :: [Integer] -> [Integer]
    reverseList [] = []
    reverseList (h:t) = reverseList t ++ [h]

    combiFinder :: [String] -> [(String, String)]
    combiFinder xs = [ (x, y) | x <- xs, y <- xs, x < y]

    multiplicationTable :: Integer -> [(Integer, Integer, Integer)]
    multiplicationTable l = [(x, y, x*y) | x <- [1..l], y <- [1..l]]