{-# LANGUAGE GADTs #-}
module Trivial where

data Paren where
   PEmpty :: Paren 
   PLeft :: Paren -> Paren 
   PRight :: Paren -> Paren

instance Show Paren where
    show PEmpty = ""
    show (PLeft p) = '(' : show p
    show (PRight p) = ')' : show p


makeNestedParenOfSize :: Int -> Paren 
makeNestedParenOfSize n | n <= 0 = PEmpty
makeNestedParenOfSize n = foldr ($) PEmpty (replicate n PLeft ++ replicate n PRight)