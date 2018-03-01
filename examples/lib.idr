module Lib

import Data.List

addLists : List Int -> List Int -> List Int
addLists xs ys = xs ++ ys

nil : List Int
nil = []

cons : Int -> List Int -> List Int
cons x xs = x :: xs

--show' : List Int -> Ex_IO String
--show' xs = do putStrLn' "Ready to show..."
--              pure (show xs)

