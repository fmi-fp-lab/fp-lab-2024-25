{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

{-# HLINT ignore "Use foldr" #-}

module Folds where

import Prelude hiding (all, and, concat, drop, filter, foldr, length, map, null, or, product, reverse, subtract, sum, take, zip, zipWith, (++))

-- WARNING: talk about
-- first homework dealine soon!
-- expect second homework really soon (due end of November)

-- abstracting
-- "automating recursion"

-- show how we reached map
-- squareList
-- megaPair

-- "replacing constructors"

-- fold
-- catamorphism

data Nat
  = Zero
  | Succ Nat
  deriving (Show)

-- TODO: implement foldNat, required
foldNat :: a -> (a -> a) -> Nat -> a
foldNat = undefined

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ $ integerToNat $ n - 1

-- show how we abstract
-- addNat multNat, generalise required for @natToInteger@
-- TODO: implement multNat: necessary for expNat

-- addNat :: Nat -> Nat -> Nat
-- addNat Zero m = m
-- addNat (Succ n) m = Succ $ addNat n m
--
-- multNat :: Nat -> Nat -> Nat
-- multNat Zero _ = Zero
-- multNat (Succ n) m = addNat m $ multNat n m

-- do some reductions on foldNat

-- reach foldr
-- TODO: implement foldr, required
-- sum, product, append
-- TODO: implement (++), required

-- EXERCISE
-- Implement natToInteger using foldNat.
-- EXAMPLES
-- >>> natToInteger $ Succ $ Succ $ Succ Zero
-- 3
natToInteger :: Nat -> Integer
natToInteger = undefined

-- EXERCISE
-- Implement exponentiation(n ^ m) using foldNat.
-- EXAMPLES
-- >>> natToInteger $ expNat (integerToNat 2) (integerToNat 10)
-- 1024
expNat :: Nat -> Nat -> Nat
expNat = undefined

---------------
-- EXERCISES --
---------------

-- EXERCISE
-- Implement and using foldr
-- EXAMPLES
-- >>> and [False]
-- False
-- >>> and [True, True]
-- True
and :: [Bool] -> Bool
and = undefined

-- EXERCISE
-- Implement or using foldr
-- EXAMPLES
-- >>> or [False]
-- False
-- >>> or [True, True]
-- True
or :: [Bool] -> Bool
or = undefined

-- EXERCISE
-- Implement length using foldr
-- EXAMPLES
-- >>> length [1,2,8]
-- 3
-- >>> length []
-- 0
length :: [a] -> Integer
length = undefined

-- EXERCISE
-- Implement (++) using foldr
-- >>> [1,2,3]
(++) :: [a] -> [a] -> [a]
(++) = undefined

-- EXERCISE
-- Implement concat using foldr
-- >>> concat [[1,2,3], [42,69], [5,7,8,9]]
-- [1,2,3,42,69,5,7,8,9]
-- >>> concat [[1,2,3], [], [5,7,8,9]]
-- [1,2,3,5,7,8,9]
-- >>> concat []
-- []
concat :: [[a]] -> [a]
concat = undefined

-- EXERCISE
-- Implement reverse using foldr (it's fine to do this in O(n^2)
-- EXAMPLES
-- >>> reverse [1,2,3]
-- [3,2,1]
-- >>> reverse []
-- []
reverse :: [a] -> [a]
reverse = undefined

-- EXERCISE
-- Implement map using foldr
-- EXAMPLES
-- >>> map succ [1,2,3]
-- [2,3,4]
-- >>> map (\x -> x * x) [1,2,3] -- same as squareList
-- [1,4,9]
-- >>> map (\x -> (3,x)) [1,2,3] -- same as megaPair 3
-- [(3,1),(3,2),(3,3)]
map :: (a -> b) -> [a] -> [b]
map = undefined

-- EXERCISE
-- Implement filter using foldr
-- EXAMPLES
-- >>> even 2
-- True
-- >>> even 3
-- False
-- >>> filter even [1..10]
-- [2,4,6,8,10]
-- >>> filter isPrime [1..20]
-- [2,3,5,7,11,13,17,19]
filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

-- EXERCISE
-- Implement null using foldr
-- EXAMPLES
-- >>> null []
-- True
-- >>> null [1]
-- False
null :: [a] -> Bool
null = undefined

-- EXERCISE
-- Implement headMaybe using foldr
-- EXAMPLES
-- >>> headMaybe []
-- Nothing
-- >>> headMaybe [1,2,3]
-- Just 1
headMaybe :: [a] -> Maybe a
headMaybe = undefined

-- EXERCISE
-- Implement a function that splits a list into two based on a predicate p
-- those that satisfy p and those that don't.
-- EXAMPLES
-- >>> partition (<5) [1..10]
-- ([1,2,3,4],[5,6,7,8,9,10])
-- >>> partition even [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = undefined

-- EXERCISE
-- Implement partition using foldr
-- EXAMPLES
-- >>> partitionfoldr (<5) [1..10]
-- ([1,2,3,4],[5,6,7,8,9,10])
-- >>> partitionfoldr even [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])
partitionfoldr :: (a -> Bool) -> [a] -> ([a], [a])
partitionfoldr = undefined

-- EXERCISE
-- Implement validateList using foldr.
-- EXAMPLES
-- >>> validateList []
-- Just []
-- >>> validateList [Just 42, Just 6, Just 9]
-- Just [42,6,9]
-- >>> validateList [Nothing, Just 6, Just 9]
-- Nothing
-- >>> validateList [Just 42, Nothing, Just 9]
-- Nothin
-- >>> validateList [Just 42, Just 6, Nothing]
-- Nothing
validateList :: [Maybe a] -> Maybe [a]
validateList = undefined

-- EXERCISE
-- Look at the recursor for nats - foldNat. In there we replaced @Nat@'s constructors with "things".
-- Think about how a fold for tuples should look like, and implement it.
-- Does this function remind you of another function we've previously implemented?
-- foldTuple :: ?
-- foldTuple = undefined

-- EXERCISE
-- Same as above, but this time for Maybe
-- foldMaybe :: ?
-- foldMaybe = undefined

-- EXERCISE
-- Same as above, but this time for Either
-- Reminder: Either is defined like so:
-- data Either a b = Left a | Right b
--
-- foldEither :: ?
-- foldEither = undefined

-- EXERCISE
-- If Nats can be converted to "n times applications" via foldNat,
-- is it perhaps true that "n times applications" can also be converted to Nats somehow?
--
-- You can ignore this "forall explanation" bit below if you want to - just assume the forall means "the passed function must be polymorphic over a"
-- START "forall explanation"
-- Usually when we have a polymorphic function, like id :: a -> a
-- the *caller* chooses what a will be - when the caller writes @id 'a'@, they instantiate @a@ with @Char@, so @id@ becomes @id :: Char -> Char@
-- However, here we will need our function to work for any @a@, and so we must *require* something of the caller -
-- that they provide a function working *for any* @a@ - meaning *we*(the callee) can decide what @a@ to apply it for, reversing who can pick what the type is.
--
-- As a concrete example, consider
-- @
-- f :: (a -> a) -> Bool
-- f g = g True
-- @
-- this does *not* compile - let's assume it did:
-- If we have
-- @
-- h :: Int -> Int
-- h x = x + 1
-- @
-- then the caller would be able to write @f h@, (as they pick what @a@ is) which is not valid,
-- since @h@ requires its argument and return types to be @Int@, and @True :: Bool@, which is not @Int@
-- If we instead consider
-- @
-- f :: (forall a. a -> a) -> Bool
-- f g = g True
-- @
-- we, as implementors of @f@, are now allowed to pick what @a@ to use, hence we are allowed to pick @a ~ Bool@,
-- by calling @g@ with @True :: Bool@
-- Hence, this function now compiles, and furthermore, if our hypothetical caller now attempts to do
-- @f h@
-- They will get a compilation error, since the argument to @f@ needs to work *for any* type, while @h@ has the concrete type @Int -> Int@
-- END "forall explanation"
--
-- EXAMPLES
-- >>> iterateToNat (\f x -> f (f (f x)))
-- Succ (Succ (Succ Zero))
iterateToNat :: (forall a. (a -> a) -> a -> a) -> Nat
iterateToNat _f = undefined

-- EXERCISE
-- This is the same as foldNat, except with arguments reaarranged to mirror @iterateToNat@
natToIterate :: Nat -> (a -> a) -> a -> a
natToIterate = undefined

type Natural = forall a. (a -> a) -> a -> a

-- EXERCISE
-- Hey, if we can convert between Natural (the type argument to @iterateToNat@, now with a synonym) and @Nat@ without losing information
-- wouldn't that mean that they are equivalent, and we can do the same things with both?
-- Let's reimplement some of the operations over @Nat@ with @Natural@ instead
-- These are called "church encoded" natural numbers - they're used to represent natural numbers when the only thing you have is functions.
--
-- Here's some exposition:
-- As you saw in the @iterateToNat@ example, these Naturals are essentially applying some function to some value a number of times.
-- The idea is that we represent the number @n@ as applying a function @f@ @n@ times to a value @v@.
-- For example:
-- 0 is represented by \f v -> v
zero :: Natural
zero _f v = v

-- 1 is represented by \f v -> f v
-- 2 is represented by \f v -> f (f v)
-- 3 is represented by \f v -> f (f (f v))
-- and so on
-- With this function, we need to somehow "add another f".
-- EXAMPLES
-- >>> iterateToNat zero
-- Zero
-- >>> iterateToNat $ suc $ suc zero
-- Succ (Succ Zero)
-- >>> natToInteger $ iterateToNat $ suc $ natToIterate $ integerToNat 5
-- 6
suc :: Natural -> Natural
suc _n = undefined

-- EXERCISE
-- We can also add these. Here we need to think about how to add f n times to another Natural.
-- EXAMPLES
-- >>> iterateToNat $ add (suc (suc zero)) zero
-- Succ (Succ Zero)
-- >>> iterateToNat $ add (suc (suc zero)) (suc (suc (suc zero)))
-- Succ (Succ (Succ (Succ (Succ Zero))))
-- >>> natToInteger $ iterateToNat $ add (suc (suc zero)) (suc (suc (suc (suc zero))))
-- 6
add :: Natural -> Natural -> Natural
add _n _m = undefined

-- EXERCISE
-- Now multiply them
-- >>> iterateToNat $ mult (suc (suc zero)) zero
-- Zero
-- >>> iterateToNat $ mult zero (suc (suc zero))
-- Zero
-- >>> iterateToNat $ mult (suc (suc zero)) (suc (suc zero))
-- Succ (Succ (Succ (Succ Zero)))
-- >>> natToInteger $ iterateToNat $ mult (suc (suc zero)) (suc (suc (suc zero)))
-- 6
mult :: Natural -> Natural -> Natural
mult _n _m = undefined

-- Is the same true for lists? Is there some function type that is "isomorphic" to lists - you can convert
-- back and forth between lists and the function, without losing data? Like how Natural is to Nat
-- (or if you prefer - can you express lists by only using lambdas?)
