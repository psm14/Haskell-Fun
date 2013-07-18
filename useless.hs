import Control.Monad

data Useless a = Unit

instance Monad Useless where
  return x  = Unit
  (>>=) a f = Unit
