import Control.Monad.State
import Data.Char

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

memOp :: (Mem -> Mem) -> String -> StateT ([String],Mem) IO ()
memOp f cs = do (stack,mem) <- get
                put (stack, f mem)
                bfStep cs

findJump' :: Int -> String -> String
findJump' 0 (']' : cs) = cs
findJump' n (']' : cs) = findJump' (n - 1) cs
findJump' n ('[' : cs) = findJump' (n + 1) cs
findJump' n (c : cs)   = findJump' n cs
findJump' n "" = error "Jumped off the end of the program"

findJump :: String -> String
findJump cs = findJump' 0 cs
    
bfStep :: String -> StateT ([String],Mem) IO ()
bfStep "" = lift $ return ()
bfStep ('<' : cs) = memOp memLeft  cs
bfStep ('>' : cs) = memOp memRight cs
bfStep ('+' : cs) = memOp memInc   cs
bfStep ('-' : cs) = memOp memDec   cs
bfStep ('.' : cs) = do (stack,mem) <- get
                       lift $ putChar (chr $ memDeref mem)
                       bfStep cs
bfStep (',' : cs) = do (stack,mem) <- get
                       c <- lift $ getChar
                       put (stack, memSet (ord c) mem)
                       bfStep cs
bfStep ('[' : cs) = do (stack, mem) <- get
                       if memDeref mem == 0 then
                         bfStep $ findJump cs
                       else
                         put (cs : stack, mem) >> bfStep cs
bfStep (']' : cs) = do (stack, mem) <- get
                       if memDeref mem == 0 then
                         put (tail stack,mem) >> bfStep cs
                       else
                         bfStep $ head stack
bfStep (c : cs) = bfStep cs

brainfuck :: String -> IO ()
brainfuck bf = evalStateT (bfStep bf) ([],blankMem)