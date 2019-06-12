module LazyEval where

    myRange start step = start:(myRange (start + step) step)