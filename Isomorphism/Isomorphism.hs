module Isomorphism where

import Data.Void
-- A type of `Void` have no value.
-- So it is impossible to construct `Void`,
-- unless using undefined, error, unsafeCoerce, infinite recursion, etc
-- And there is a function
-- absurd :: Void -> a
-- That get any value out of `Void`
-- We can do this becuase we can never have void in the zeroth place.

-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (x, y) = (y, x)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
-- (a -> b, b -> a)
-- (b -> c, c -> b)
-- (a -> c, c -> a)
trans (x1, y1) (x2, y2) = (x2 . x1, y1 . y2)

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
-- (a -> b, b -> a)
-- (c -> d, d -> c)
-- ((a, c) -> (b, d), (b, d) -> (a, c))
isoTuple (ab, ba) (cd, dc) =
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
-- (a -> b, b -> a)
-- ([a] -> [b], [b] -> [a])
isoList (ab, ba) =
  (\al -> fmap ab al, \bl -> fmap ba bl)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
-- (a -> b, b -> a)
-- (Maybe a -> Maybe b, Maybe b -> Maybe a)
isoMaybe (ab, ba) =
  (\ma -> case ma of
    Nothing -> Nothing
    Just a -> Just (ab a),
   \mb -> case mb of
    Nothing -> Nothing
    Just b -> Just (ba b))

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
-- (a -> b, b -> a)
-- (c -> d, d -> c)
-- (Either a c -> Either b d, Either b d -> Either a c)
isoEither (ab, ba) (cd, dc) =
  (\l -> case l of
    Left a -> Left (ab a)
    Right c -> Right (cd c),
   \r -> case r of
    Left b -> Left (ba b)
    Right d -> Right (dc d))

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
-- ((a -> c) -> (b -> d), (b -> d) -> (a -> c))
isoFunc (ab, ba) (cd, dc) =
  (\ac -> cd . ac . ba,
   \bd -> dc . bd . ab)

fromJust :: Maybe a -> a
fromJust Nothing = error "error: fromJustNothing!"
fromJust (Just a) = a

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
-- (Maybe a -> Maybe b, Maybe b -> Maybe a)
-- (a -> b, b -> a)

-- Hint: Maybe a -> Maybe b is not a -> Maybe b
-- Constructing b from a
isoUnMaybe (mamb, mbma) = (ab , ba)
  where
    ab a = case mamb (Just a) of
      Nothing -> fromJust $ mamb Nothing
      otherwise -> fromJust $ mamb (Just a)
    ba b = case mbma (Just b) of
      Nothing -> fromJust $ mbma Nothing
      otherwise -> fromJust $ mbma (Just b)

isoListofTuple :: ISO [()] [()]
isoListofTuple = refl

isoTupleVoid :: ISO () Void
-- (() -> Void, Void -> ())
isoTupleVoid = undefined

isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = undefined

isoSymm :: ISO (ISO a b) (ISO b a)
-- ((ISO a b) -> (ISO b a), (ISO b a) -> (ISO a b))
-- (a -> b, b -> a) -> (b -> a, a -> b), (b -> a, a -> b) -> (a -> b, b -> a)
isoSymm = (x, y) where
  x = \(x1, x2) -> (x2, x1)
  y = \(y1, y2) -> (y2, y1)