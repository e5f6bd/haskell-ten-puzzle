{-|
This module solves so-called "10 puzzle"
( https://ja.wikipedia.org/wiki/%E3%83%86%E3%83%B3%E3%83%91%E3%82%BA%E3%83%AB ).
The entrypoint function is `solve`.
-}
module Lib
  ( stacks,
    reduced,
    allReduced,
    expressions,
    candidates,
    evaluate,
    solve,
    exprToStr,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (permutations)
import Data.Maybe (mapMaybe)

data Operator = Plus | Minus | Times | Divides deriving (Show)

data Expr a = Term a | Binop (Expr a) Operator (Expr a) deriving (Show)

type Stack a = [Expr a]

ops :: [Operator]
ops = [Plus, Minus, Times, Divides]

-- | Enumerates stacks that can be obtained by
-- constructing binary operation of the top two elements.
reduced :: Stack a -> [Stack a]
reduced (x : y : rem) = ops >>= allReduced . (: rem) . flip (Binop x) y
reduced _ = []

-- | Enumerates all possible stacks that can be obtained by repeating
-- the operation of constructing binary operation of the top two elements.
allReduced :: Stack a -> [Stack a]
allReduced xs = xs : reduced xs

-- | Enumerates all possible stacks that can be obtained from the terms
-- used in the given order.
stacks :: [a] -> [Stack a]
stacks [] = [[]]
stacks (x : xs) = stacks xs >>= allReduced . (Term x :)

singleToJust :: Stack a -> Maybe (Expr a)
singleToJust [x] = Just x
singleToJust _ = Nothing

-- | Enumerates all possible expressions that can be obtained from the terms
-- appearing in the given order.
expressions :: [a] -> [Expr a]
expressions xs = mapMaybe singleToJust $ stacks xs

-- | Enumerates all possible expressions that is constructed from
-- given elements, +, -, *, /, and parenthesis.
candidates :: [a] -> [Expr a]
candidates xs = permutations xs >>= expressions

-- | Evaluates given expresison.  Returns Nothing if there is a div-by-0.
evaluate :: (Fractional a, Eq a) => (e -> a) -> Expr e -> Maybe a
evaluate f (Term a) = Just $ f a
evaluate f (Binop a' op b') = do
  a <- evaluate f a'
  b <- evaluate f b'
  case op of
    Plus -> Just (a + b)
    Minus -> Just (a - b)
    Times -> Just (a * b)
    Divides -> if b == fromRational 0 then Nothing else Just (a / b)

-- | Find an expression that evaluates to given number.
-- Type e is the type of terms in the expression,
-- and type a is the type of intermediate evaluations.
-- Typically, e is Int and a is Ratio (rational number).
solve :: (Fractional a, Eq a) => (e -> a) -> e -> [e] -> [Expr e]
solve f n xs = candidates xs & filter ((== Just (f n)) . evaluate f)

exprToStr :: Show a => Expr a -> String
exprToStr (Term a) = show a
exprToStr (Binop a op b) = "(" ++ exprToStr a ++ opStr ++ exprToStr b ++ ")"
  where
    opStr = case op of
      Plus -> "+"
      Minus -> "-"
      Times -> "*"
      Divides -> "/"
