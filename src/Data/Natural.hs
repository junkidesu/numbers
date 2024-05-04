{-# LANGUAGE InstanceSigs #-}

module Data.Natural (
    Natural (Zero, Succ),
    zero,
    one,
    two,
    three,
    four,
    five,
    six,
    seven,
    eight,
    nine,
) where

-- Succ here represents the successor function, which works as follows:
-- Succ Zero = 1
-- Succ (Succ Zero) = 2
-- Succ (Succ (Succ Zero)) = 3
-- ...
data Natural = Zero | Succ Natural

zero :: Natural
zero = Zero

one :: Natural
one = Succ Zero

two :: Natural
two = Succ one

three :: Natural
three = Succ two

four :: Natural
four = Succ three

five :: Natural
five = Succ four

six :: Natural
six = Succ five

seven :: Natural
seven = Succ six

eight :: Natural
eight = Succ seven

nine :: Natural
nine = Succ eight

instance Num Natural where
    (+) :: Natural -> Natural -> Natural
    Zero + m = m
    Succ n + m = Succ (n + m)

    (-) :: Natural -> Natural -> Natural
    Zero - Succ _ = error "Not a natural number"
    n - Zero = n
    (Succ a@n) - (Succ b@m)
        | n == m = Zero
        | n > m = a - b
        | otherwise = error "Not a natural number"

    (*) :: Natural -> Natural -> Natural
    Zero * _ = Zero
    Succ n * m = n * m + m

    abs :: Natural -> Natural
    abs = id

    signum :: Natural -> Natural
    signum Zero = Zero
    signum _ = one

    fromInteger :: Integer -> Natural
    fromInteger 0 = Zero
    fromInteger n
        | n > 0 = Succ (fromInteger (n - 1))
        | otherwise = error "Cannot convert negative integer to natural"

instance Eq Natural where
    (==) :: Natural -> Natural -> Bool
    Zero == Zero = True
    Zero == _ = False
    _ == Zero = False
    Succ n == Succ m = n == m

instance Ord Natural where
    (<=) :: Natural -> Natural -> Bool
    Zero <= _ = True
    _ <= Zero = False
    Succ n <= Succ m = n <= m

instance Enum Natural where
    toEnum 0 = Zero
    toEnum n
        | n > 0 = Succ (toEnum (n - 1))
        | otherwise = error "Not a natural number"

    fromEnum Zero = 0
    fromEnum (Succ n) = 1 + fromEnum n

instance Show Natural where
    show :: Natural -> String
    show = show . fromEnum
