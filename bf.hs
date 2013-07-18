import Control.Monad.State

data Mem = Mem [Int] Int [Int]
         deriving (Show)

blankMem :: Mem
blankMem = Mem [] 0 []

memLeft :: Mem -> Mem
memLeft (Mem (a : as) b cs) = Mem as a (b : cs)
memLeft (Mem [] b cs) = Mem [] 0 (b : cs)

memRight :: Mem -> Mem
memRight (Mem as b (c : cs)) = Mem (b : as) c cs
memRight (Mem as b []) = Mem (b : as) 0 []

memDeref :: Mem -> Int
memDeref (Mem _ b _) = b

memMod :: (Int -> Int) -> Mem -> Mem
memMod f (Mem as b cs) = Mem as (f b) cs

memInc :: Mem -> Mem
memInc = memMod (+1)

memDec :: Mem -> Mem
memDec = memMod $ flip (-) 1 -- Parser reads (-1) as negative 1

memSet :: Int -> Mem -> Mem
memSet b = memMod $ const b

bfStep :: Char -> StateT ([String],Mem) IO ()
bfStep = undefined

brainfuck :: String -> IO ()
brainfuck = undefined