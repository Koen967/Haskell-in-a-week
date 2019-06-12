module Fibonacci where

    fib :: Integer -> Integer
    fib 0 = 0
    fib 1 = 1
    fib x = fib (x-1) + fib (x - 2)

    fibTuple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
    fibTuple (x, y, 0) = (x, y, 0)
    fibTuple (x, y, index) = fibTuple (y, x + y, index - 1)

    fibResult :: (Integer, Integer, Integer) -> Integer
    fibResult (x, y, z) = x

    fibImproved :: Integer -> Integer
    fibImproved x = fibResult (fibTuple (0, 1, x))

    fibNextPair :: (Integer, Integer) -> (Integer, Integer)
    fibNextPair (x, y) = (y, x + y)

    fibNthPair :: Integer -> (Integer, Integer)
    fibNthPair 1 = (1, 1)
    fibNthPair n = fibNextPair (fibNthPair (n - 1))

    fibCompos :: Integer -> Integer
    fibCompos = fst . fibNthPair

    fibLazy x y = x:(fibLazy y (x + y))

    fibExec = fibLazy 1 1

    fibNth x = head (drop (x - 1) (take (x) fibExec))
    