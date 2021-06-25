{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances, TemplateHaskell #-}
module InvertAdd where

-- | The natural numbers, encoded in types.
data Z
data S n -- Phantom types

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- Lemmas

refl :: Natural n -> Equal n n
refl NumZ = EqlZ
refl (NumS n) = EqlS (refl n) 

-- do some computation

invert :: Natural a -> Natural b -> Equal (a :+: a) (b :+: b) -> Equal a b
invert NumZ NumZ (EqlS EqlZ) = EqlZ