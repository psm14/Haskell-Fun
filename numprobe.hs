module NumProbe (probeExpr, simplify, simplify', prettyPrint) where

data Probe a = Constant a String
             | Variable String
             | Function a String (Probe a)
             | BinOp a String (Probe a) (Probe a)

val :: Probe a -> a
val (Constant a _)   = a
val (Variable _)     = undefined
val (Function a _ _) = a
val (BinOp a _ _ _)  = a

binOp :: String -> (a -> a -> a) -> Probe a -> Probe a -> Probe a
binOp name f a b = let v = f (val a) (val b) in BinOp v name a b

func :: String -> (a -> a) -> Probe a -> Probe a
func name f a = let v = f (val a) in Function v name a

instance Show (Probe a) where
  show (Constant _ a) = a
  show (Variable a)   = a
  show (Function _ name f) = name ++ "(" ++ show f ++ ")" 
  show (BinOp _ name a b)  = "(" ++ show a ++ " " ++ name ++ " " ++ show b ++ ")"

instance (Eq a, Num a) => Num (Probe a) where
  (+) = binOp "+" (+)
  (-) = binOp "-" (-)
  (*) = binOp "*" (*)
  abs = func "abs" abs
  fromInteger a = Constant (fromInteger a) (show a)
  signum a = case signum (val a) of
      (-1) -> Constant (-1) "-1"
      1    -> Constant 1  "1"

instance Eq a => Eq (Probe a) where
  (==) a b = val a == val b

instance Ord a => Ord (Probe a) where
  (<=) a b = val a <= val b

instance (Eq a, Fractional a) => Fractional (Probe a) where
  (/) = binOp "/" (/)
  recip a = 1 / a
  fromRational a = Constant (fromRational a) (show a)

instance (Eq a, Floating a) => Floating (Probe a) where
  pi   = Constant pi "π"
  exp  = func "exp" exp
  log  = func "log" log
  sqrt = func "sqrt" sqrt
  sin  = func "sin" sin
  cos  = func "cos" cos
  tan  = func "tan" tan 
  sinh = func "sinh" sinh
  cosh = func "cosh" cosh
  asin = func "asin" asin
  acos = func "acos" acos
  atan = func "atan" atan
  asinh = func "asinh" asinh
  acosh = func "acosh" acosh
  atanh = func "atanh" atanh
  (**)  = binOp "^" (**)

probeExpr :: (Probe a -> Probe a) -> Probe a
probeExpr f = f $ Variable "x"

simplify :: Show a => Probe a -> Probe a
simplify (BinOp v _ (Constant _ _) (Constant _ _)) = Constant v (show v)
simplify (BinOp v name a b)  = BinOp v name (simplify a) (simplify b)
simplify (Function v name a) = Function v name (simplify a)
simplify a = a

simplify' :: Show a => Integer -> Probe a -> Probe a
simplify' 0 a = simplify a
simplify' n a = simplify' (n-1) (simplify a)

prettyPrint :: (Eq a, Num a) => Probe a -> String
prettyPrint (BinOp _ "+" a (Constant 0 _)) = prettyPrint a
prettyPrint (BinOp _ "+" (Constant 0 _) b) = prettyPrint b
prettyPrint (BinOp _ "-" a (Constant 0 _)) = prettyPrint a
prettyPrint (BinOp _ "-" (Constant 0 _) b) = "-" ++ prettyPrint b
prettyPrint (BinOp _ "*" (Constant 1 _) b) = prettyPrint b
prettyPrint (BinOp _ "*" a (Constant 1 _)) = prettyPrint a
prettyPrint (BinOp _ n a b) = "(" ++ prettyPrint a ++ " " ++ n ++ " " ++ prettyPrint b ++ ")"
prettyPrint a = show a