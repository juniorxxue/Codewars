module Fixit where
import Prelude hiding (reverse, foldr)

fix :: (a -> a) -> a
fix f = let x = f x in x

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' f [] = []
reverse' f (x:xs) = (f xs) ++ [x]

-- foldr :: (a -> b -> b) -> b -> [a] -> b
foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' f g z [] = z
foldr' f g z (x : xs) = g x (f g z xs)