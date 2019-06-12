module DayTwo where
    import Data.List
    import Data.Char

    -- Write a sort that takes a list and returns a sorted list.

    sortListData [] = []
    sortListData list = sort list

    sortList [] = []
    sortList (x:xs) = insert x $ sortList xs
        where   insert x [] = [x]
                insert x (y:ys) | x > y = y:insert x ys
                                | x <= y = x:y:ys
    
    -- Write a sort that takes a list and a function that compares it's two arguments and then returns a sorted list.
    orderSmallToLarge x y = x > y
    orderLargeToSmall x y = x < y

    sortListFunc :: (a -> a -> Bool) -> [a] -> [a]
    sortListFunc _ [] = []
    sortListFunc f (x:xs) = insert f x $ sortListFunc f xs
        where   insert _ x [] = [x]
                insert f x (y:ys)   | f x y == True = y:insert f x ys
                                    | otherwise = x:y:ys


    -- Write a function to convert a string to a number. The string in the form of $2,345,678.99 and could have leading zeros.
    stringToNumber :: [Char] -> Double
    stringToNumber s = read $ filter isDigitOrDecimal s :: Double
        where isDigitOrDecimal x = isDigit x || x == '.'

    -- Write a function that takes argument x and returns a lazy sequence that has every thirth number starting with x.
    -- Then write a function that takes y and returns every fifth number.
    -- Then combine these functions to return every eighth number beginning with x + y
    lazySequence x y = [x, x + y ..]
    lazySequence3 x = lazySequence x 3
    lazySequence5 y = lazySequence y 5
    lazySequence8 x y = zipWith (+) (lazySequence3 x) (lazySequence5 y)

    -- Write a partilly applied function that returns half of a number
    half = (/ 2)

    -- Write a partially applied function that appends \n to a string
    appendNewLine = (++ "\n")