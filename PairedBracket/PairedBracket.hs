{-# LANGUAGE KindSignatures, DataKinds, GADTs #-}

module PairedBracket where 

data Nat = Z | S Nat deriving Show

instance Show (Paren n) where
    show PEmpty = ""
    show (PLeft p) = '(' : show p
    show (PRight p) = ')' : show p

instance Eq (Paren n) where
    PEmpty == PEmpty = True
    (PLeft n1) == (PLeft n2)  = n1 == n2
    (PRight n1) == (PRight n2)  = n1 == n2
    _ == _ = False

-- data definition, implement Paren encoding to contain balancing information.

data Paren :: Nat -> * where
    PEmpty :: Paren Z
    PLeft :: Paren (S n) -> Paren n
    PRight :: Paren n -> Paren (S n)

-- use the Paren data definition


-- makeNestedParenOfSize :: Int -> Paren 
-- makeNestedParenOfSize n | n <= 0 = PEmpty
-- makeNestedParenOfSize n = foldr ($) PEmpty (replicate n PLeft ++ replicate n PRight)

-- implement the above function with your `Paren :: Nat -> *` definition.

makeNestedParenOfSize :: Int -> Paren Z
makeNestedParenOfSize n  | n <= 0 = PEmpty
makeNestedParenOfSize n = makeHelper n $ PEmpty where
    makeHelper :: Int -> (Paren n -> Paren n)
    makeHelper n  | n <= 1 = PLeft . PRight
    makeHelper n = PLeft . (makeHelper (n - 1)) . PRight 