module HighOrder where

    squareAll :: [Integer] -> [Integer]
    squareAll list = map square list
        where square x = x * x
    
    addList :: [Integer] -> Integer
    addList list = foldl addition 0 list
        where addition carr x = carr + x