{-# Language RankNTypes #-}

module Church (Boolean,false,true,not,and,or,xor) where

import Prelude hiding (Bool,False,True,not,and,or,(&&),(||),(==),(/=))

type Boolean = forall a. a -> a -> a

false,true :: Boolean
false = \ t f -> f
true  = \ t f -> t

not :: Boolean -> Boolean
and,or,xor :: Boolean -> Boolean -> Boolean

not = \ a   -> \ t f -> a f t
and = \ a b -> a b a

-- and true true
-- true true true
-- true

-- and false true
-- false true false
-- false

-- and true false
-- true false true
-- false

-- and false false
-- false false false
-- false

or  = \ a b -> a a b

-- or true true
-- true true true
-- true

-- or true false
-- true true false
-- true

-- or false true
-- false false true
-- true

-- or false false
-- false false false
-- false

xor = \ a b -> a (not b) b

-- xor true true
-- true false false
-- false

-- xor true false
-- true true false
-- true

-- xor false true
-- true

-- xor false false
-- false