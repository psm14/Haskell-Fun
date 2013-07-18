import Control.Monad
import Control.Monad.State

-- Folding

splitOn :: Char -> String -> [String]
splitOn c s = foldr woot [""] s
    where woot x acc
             | x == c    = "" : acc
             | otherwise = (x : head acc) : tail acc

calc :: (Fractional n, Read n) => [n] -> String -> [n]
calc (a : b : as) "+" = a + b : as
calc (a : b : as) "-" = a - b : as
calc (a : b : as) "*" = a * b : as
calc (a : b : as) "/" = a / b : as
calc as num = read num : as 

rpn :: (Fractional n, Read n) => String -> n
rpn = head . foldl calc [] . splitOn ' '

-- Imperative

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f (a : as) = if f a then [] else a : takeUntil f as

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f l@(a : as) = if f a then l else dropUntil f as

type Stack a t = State [a] t

push :: a -> Stack a ()
push v = do vs  <- get
            put $ v : vs
            return ()

pop :: Stack a a
pop = do v : vs <- get
         put vs
         return v