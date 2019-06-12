module DayThree where

    import Data.Map (Map)

    -- Write a function that looks up a hash table value using the maybe monad.
    ages = Map.fromList [("Koen", 23), ("Fred", 21), ("Tony", 22)]

    getValue key [] = Nothing
    getValue key map = case Map.lookup key map of
        Nothing -> "No value found for key " ++ key
        Just value -> "value of key: " ++ key ++ "is " ++ value 