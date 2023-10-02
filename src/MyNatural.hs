{-# LANGUAGE InstanceSigs #-}

module MyNatural (
    MyNatural(Zero,Succ),
    zero,
    one,
    two,
    three,
    four,
    five,
    (|+),
    (|*),
    (|^),
    fromInt,
    toInt
) where

-- Succ here represents the successor function, which works as follows:
-- Succ Zero = 1
-- Succ (Succ Zero) = 2
-- Succ (Succ (Succ Zero)) = 3
-- ...
data MyNatural = Zero | Succ MyNatural

-- The following are defined for convenience
zero :: MyNatural
zero = Zero

one :: MyNatural
one = Succ Zero

two :: MyNatural
two = Succ one

three :: MyNatural
three = Succ two

four :: MyNatural
four = Succ three

five :: MyNatural
five = Succ four

-- According to the Peano axioms, we know that the natural number zero cannot
-- be a successor of any natural number, and that the successor function
-- is an injective function. Together with the usual axioms of equality
-- (reflexivity, symmetry, and transitivity), we can use this to define
-- the Eq MyNatural instance.
instance Eq MyNatural where
  (==) :: MyNatural -> MyNatural -> Bool
  Zero == Zero     = True
  Zero == _        = False
  _    == Zero     = False
  Succ n == Succ m = n == m

-- The Num type class requires that we define the negation of a natural number,
-- which does not make sense at this point, for obvious reasons.
-- Therefore, we shall define our own arithmetic operations for the MyNatural type.

-- Addition
(|+) :: MyNatural -> MyNatural -> MyNatural
Zero |+ m   = m
Succ n |+ m = Succ (n|+m)

-- Multiplication
(|*) :: MyNatural -> MyNatural -> MyNatural
Zero |* _   = Zero
Succ n |* m = n |* m |+ m

-- Exponentiation
(|^) :: MyNatural -> MyNatural -> MyNatural
_ |^ Zero   = one
m |^ Succ n = (m |^ n) |* m

-- Ordering of natural numbers
instance Ord MyNatural where
    (<=) :: MyNatural -> MyNatural -> Bool
    Zero <= _        = True
    _ <= Zero        = False
    Succ n <= Succ m = n <= m

-- The name is pretty self-explanatory
toInt :: MyNatural -> Int
toInt Zero     = 0
toInt (Succ n) = 1 + toInt n

-- So is the name of this function.
-- It takes an integer, and if it's nonnegative, it converts it into a MyNatural.
fromInt :: Int -> Maybe MyNatural
fromInt n
    | n == 0    = return Zero
    | n >  0    = do
        x <- fromInt (n-1)
        return $ one |+ x
    | otherwise = Nothing

-- Clearly, viewing a natural number as a chain of Succ's is not very human-readable.
-- For this reason, we define the Show MyNatural instance as follows to display
-- the decimal expansion of the natural number.
instance Show MyNatural where
    show :: MyNatural -> String
    show = show . toInt
