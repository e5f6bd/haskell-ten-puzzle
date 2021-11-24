module Lib
    (
     stacks, reduced, allReduced, expressions, candidates
    ) where

import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Data.List (permutations)

data Operator = Plus | Minus | Times | Divides deriving Show
data Expr a = Term a | Binop (Expr a) Operator (Expr a) deriving Show
type Stack a = [Expr a]

ops :: [Operator]
ops = [Plus, Minus, Times, Divides]

-- | Enumerates stacks that can be obtained by
-- constructing binary operation of the top two elements.
reduced :: Stack a -> [Stack a]
reduced (x:y:rem) = ops >>= allReduced . (:rem) . flip (Binop x) y
reduced _ = []

-- | Enumerates all possible stacks that can be obtained by repeating
-- the operation of constructing binary operation of the top two elements.
allReduced :: Stack a -> [Stack a]
allReduced xs = xs:reduced xs

-- | Enumerates all possible stacks that can be obtained from the terms
-- used in the given order.
stacks :: [a] -> [Stack a]
stacks [] = [[]]
stacks (x:xs) = stacks xs >>= allReduced . (Term x:)

singleToJust :: Stack a -> Maybe (Expr a)
singleToJust [x] = Just x
singleToJust _ = Nothing

-- | Enumerates all possible expressions that can be obtained from the terms
-- appearing in the given order.
expressions :: [a] -> [Expr a]
expressions xs = mapMaybe singleToJust $ stacks xs

candidates :: [a] -> [Expr a]
candidates xs = permutations xs >>= expressions
