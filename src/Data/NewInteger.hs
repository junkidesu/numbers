{-# LANGUAGE InstanceSigs #-}

module Data.NewInteger (NewInteger (..), zero) where

import Data.Natural (Natural (Succ, Zero))

data NewInteger = NewInteger Natural Natural

fromNatural :: Natural -> NewInteger
fromNatural n = NewInteger n Zero

zero :: NewInteger
zero = fromNatural Zero

one :: NewInteger
one = fromNatural (Succ Zero)

instance Eq NewInteger where
    (==) :: NewInteger -> NewInteger -> Bool
    NewInteger a b == NewInteger c d = a + d == b + c

instance Ord NewInteger where
    (<=) :: NewInteger -> NewInteger -> Bool
    NewInteger a b <= NewInteger c d = a + d <= b + c

instance Num NewInteger where
    (+) :: NewInteger -> NewInteger -> NewInteger
    NewInteger a b + NewInteger c d = NewInteger (a + c) (b + d)

    (*) :: NewInteger -> NewInteger -> NewInteger
    NewInteger a b * NewInteger c d = NewInteger (a * c + b * d) (a * d + b * c)

    negate :: NewInteger -> NewInteger
    negate (NewInteger a b) = NewInteger b a

    signum :: NewInteger -> NewInteger
    signum n
        | n == zero = zero
        | n > 0 = one
        | otherwise = negate one

    abs :: NewInteger -> NewInteger
    abs n
        | n >= 0 = n
        | otherwise = negate n

    fromInteger :: Integer -> NewInteger
    fromInteger n
        | n >= 0 = fromNatural (fromInteger n)
        | otherwise = negate . fromNatural . fromInteger . negate $ n

instance Enum NewInteger where
    toEnum :: Int -> NewInteger
    toEnum n
        | n >= 0 = fromNatural . toEnum $ n
        | otherwise = fromNatural . toEnum . negate $ n

    fromEnum :: NewInteger -> Int
    fromEnum (NewInteger a b) = fromEnum a - fromEnum b
