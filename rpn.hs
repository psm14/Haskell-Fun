import Prelude hiding (splitAt)
import Control.Monad
import Control.Monad.State
import Control.Monad.Loops
import Debug.Trace

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

-- TODO: Write these next 3 imperatively
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f      []  = []
takeUntil f (a : as) = if f a then [] else a : takeUntil f as

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f        []  = []
dropUntil f l@(a : as) = if f a then l else dropUntil f as

splitAt :: (a -> Bool) -> [a] -> ([a],[a])
splitAt f as = let bs = takeUntil f as
                   cs = dropUntil f as
                   end = if length cs == 0 then [] else tail cs
               in (bs, end)

type Env = State (String, [Double])

push :: Double -> Env ()
push v = do (str,vs)  <- get
            put $ (str,v : vs)
            return ()

pop :: Env Double
pop = do (str, v : vs) <- get
         put (str,vs)
         return v

nextToken :: Env String
nextToken = do (str,vs) <- get
               let (token, rest) = splitAt (== ' ') str
               put (rest,vs)
               return token

moreInput :: Env Bool
moreInput = do (str,_) <- get
               return $ str /= "" 

rpn' :: String -> Double
rpn' str = (flip evalState) (str,[]) $ do
               whileM_ moreInput $ do
                   token <- nextToken
                   case token of
                       "+" -> do a <- pop
                                 b <- pop
                                 push (a + b)
                       "-" -> do a <- pop
                                 b <- pop
                                 push (a - b)
                       "*" -> do a <- pop
                                 b <- pop
                                 push (a * b)
                       "/" -> do a <- pop
                                 b <- pop
                                 push (a / b)
                       num -> push $ read num
               pop