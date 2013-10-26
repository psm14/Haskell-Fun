module Derivative (derivative) where

-- Dual numbers are similar to complex numbers, except with a constant ϵ
-- defined such that ϵ != 0, but ϵ^2 == 0.
data Dual a = Dual { real :: a, ϵ :: a }
	deriving Eq

instance (Eq a, Num a, Show a) => Show (Dual a) where
	show (Dual a 0) = show a
	show (Dual 0 b) = (show b) ++ "ϵ"
	show (Dual a b) = (show a) ++ " + " ++ (show b) ++ "ϵ"

instance Num a => Num (Dual a) where
	(+) (Dual a b) (Dual c d) = Dual (a+c) (b+d)
	(-) (Dual a b) (Dual c d) = Dual (a-c) (b-d)
	(*) (Dual a b) (Dual c d) = Dual (a*c) (b*c + a*d)
	abs (Dual a b)            = Dual (abs a) b
	fromInteger a             = Dual (fromInteger a) 0
	signum (Dual a _)         = Dual (signum a) 0

-- Not entirely sure this ordering is sound.
-- I'm going off the intuition that since ϵ != 0, 2ϵ should be greater than ϵ and so on.
instance Ord a => Ord (Dual a) where
	(<=) (Dual a b) (Dual c d) | a < c     = True
	                           | a > c     = False
	                           | otherwise = b <= d

instance Fractional a => Fractional (Dual a) where
	recip (Dual a b) = Dual (1 / a) ((0 - b) / (a * a))
	fromRational a   = Dual (fromRational a) 0

instance Floating a => Floating (Dual a) where
	pi = Dual pi 0
	exp   (Dual a b) = Dual (exp a)   (b  * exp a)
	log   (Dual a b) = Dual (log a)   (-b / (a**2))
	sqrt  (Dual a b) = Dual (sqrt a)  (b  / (2 * sqrt a))
	sin   (Dual a b) = Dual (sin a)   (b  * cos a)
	cos   (Dual a b) = Dual (cos a)   (-b * sin a)
	tan   (Dual a b) = Dual (tan a)   (b  * (1 + (tan a)**2))
	sinh  (Dual a b) = Dual (sinh a)  (b  * cosh a)
	cosh  (Dual a b) = Dual (cosh a)  (b  * sinh a)
	asin  (Dual a b) = Dual (asin a)  (b  / sqrt (1 - a**2))
	acos  (Dual a b) = Dual (acos a)  (-b / sqrt (1 - a**2))
	atan  (Dual a b) = Dual (atan a)  (b  / (a**2 + 1))
	asinh (Dual a b) = Dual (asinh a) (b  / sqrt (a**2 + 1))
	acosh (Dual a b) = Dual (acosh a) (b  / sqrt (a - 1) / sqrt (a + 1))
	atanh (Dual a b) = Dual (atanh a) (b  / (1 - a**2))

-- Compute the exact numeric derivative of an expression
-- Example:  derivative (\x -> 1 / x) 2 => -0.25
--           derivative (\x -> x ^ 3) 2 => 12
--           derivative (\x -> x ^ 2) (toRational 0.75) => 3 % 2
--           derivative (derivative (derivative (derivative sin))) $ 3*pi/2 => -1.0
--
-- This works because f(x + ϵ) == f(x) + f'(x)ϵ
derivative :: Num a => (Dual a -> Dual a) -> a -> a
derivative f a = (ϵ . f) (Dual a 1)
