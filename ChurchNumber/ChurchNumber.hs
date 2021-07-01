{-# LANGUAGE RankNTypes #-}

module ChurchNumber where

import Prelude hiding (succ)

newtype Number = Nr (forall a. (a -> a) -> a -> a)

zero :: Number
zero = Nr (\ _ x -> x)

succ :: Number -> Number
succ (Nr n) = Nr (\ f x -> f (n f x))

one :: Number
one = succ zero

add :: Number -> Number -> Number
add (Nr m) (Nr n) = Nr $ \ f x -> m f (n f x)

mult :: Number -> Number -> Number
mult (Nr m) (Nr n) = Nr $ \ f x -> m (n f) x

pow :: Number -> Number -> Number
pow (Nr m) (Nr n) = Nr $ n m