import System.Environment
import Control.Monad.State
import Data.Char

class Monad m => MonadRW m where
    readChar  :: m Char
    writeChar :: Char -> m ()

instance MonadRW IO where
    readChar  = getChar
    writeChar = putChar

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

memGet :: Mem -> Int
memGet (Mem _ b _) = b

memMod :: (Int -> Int) -> Mem -> Mem
memMod f (Mem as b cs) = Mem as (f b) cs

memInc :: Mem -> Mem
memInc = memMod (+1)

memDec :: Mem -> Mem
memDec = memMod $ flip (-) 1 -- Parser reads (-1) as negative 1

memSet :: Int -> Mem -> Mem
memSet b = memMod $ const b

findJump :: String -> String
findJump cs = findJump' 0 cs
  where findJump' 0 (']' : cs) = cs
        findJump' n (']' : cs) = findJump' (n - 1) cs
        findJump' n ('[' : cs) = findJump' (n + 1) cs
        findJump' n (c   : cs) = findJump' n cs
        findJump' n ""         = error "Jumped off the end of the program"
    


brainfuck :: MonadRW m => String -> m ()
brainfuck bf = evalStateT (bfStep bf) ([],blankMem)
  where bfStep "" = return ()
        bfStep ('<' : cs) = memOp memLeft  cs
        bfStep ('>' : cs) = memOp memRight cs
        bfStep ('+' : cs) = memOp memInc   cs
        bfStep ('-' : cs) = memOp memDec   cs
        bfStep ('.' : cs) = do (stack,mem) <- get
                               lift $ writeChar (chr $ memGet mem)
                               bfStep cs
        bfStep (',' : cs) = do (stack,mem) <- get
                               c <- lift $ readChar
                               put (stack, memSet (ord c) mem)
                               bfStep cs
        bfStep ('[' : cs) = do (stack, mem) <- get
                               if memGet mem == 0 then
                                 bfStep $ findJump cs
                               else do
                                 put (cs : stack, mem)
                                 bfStep cs
        bfStep (']' : cs) = do (stack, mem) <- get
                               if memGet mem == 0 then do
                                 put (tail stack,mem)
                                 bfStep cs
                               else
                                 bfStep $ head stack
        bfStep (c : cs) = bfStep cs
        memOp f cs      = do (stack,mem) <- get
                             put (stack, f mem)
                             bfStep cs

main = do args <- getArgs
          let file = head args
          prog <- readFile file
          brainfuck prog
