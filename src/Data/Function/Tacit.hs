-- | Write functions in tacit (pointless) style using Applicative and De
--   Bruijn index notation.
--
--   Examples:
--
--   - @
--     \f x y -> f x == f y
--     = lurryA \@N3 ((==) \<$\> (_1 \<*\> _2) (_1 \<*\> _3))
--     @
--   - @
--     \f g x -> f x (g x)
--     = lurryA \@N3 ((_1 \<*\> _3) \<*\> (_2 \<*\> _3))
--     @
--   - @
--     \a b -> b
--     = lurryA \@N2 _2
--     @
--
--   This module is intended to be used with 'Control.Applicative' but
--   does not export it.
--
--   Opposite to De Bruijn indices, this module orders the arguments
--   from the outside-in, rather than the inside-out (or left-to-right
--   instead of right-to-left). For example, the conventional
--   @λλλ3 1 (2 1)@ is instead @λλλ1 3 (2 3)@.
--  
--   The first argument is @z@, the second argument @z.s@, the third
--   argument @z.s.s@, and so on. For the first few arguments convenient
--   names have been defined, such as '_1', '_2', '_3', and so on.
--
--   To export a function use 'lurryA'. You must specify the arity of
--   the function, which is intended to be done with TypeApplications
--   (new in GHC 8.0).  @lurryA \@(S Z) f@ says the arity of @f@ is one,
--   @lurryA \@(S (S Z)) f@ says the arity is two, and so on. For
--   convenience the first few Peano numbers have been given aliases,
--   such as @N1@, @N2@, @N3@, and so on.
--
--   You can write all functions with '<*>' and '<$>' from
--   'Applicative' — should be able to, yet unproven.
--
--   There is a type inference problem with functions where the highest
--   index does not match the function arity, such as 'const'. To
--   resolve this ambiguity you must give an explicit signature. For
--   example: @lurryA \@N2 (_1 :: (a, (b, c)) -> a)@.
--
--   TODO:
--
--   - Construction rules for rewriting functions into this tacit form.
--     More precise than just examples. Would demonstrate that any
--     function can be written in this tacit form.
--   - An inverse for @lurry@, @unlurry@. Type inference seems
--     difficult.
--   - Inference problem when the highest index does not match the
--     function arity.
--
--   NOTES:
--
--   - The implementation would be simpler and less prone to inference
--     problems if GHC had closed classes. Given a type family @F@, a
--     corresponding value-level implementation may exist for
--     @x -> F x@. This implementation can be given by a class and an
--     instance corresponding to each case in the type family. However,
--     if the type family is closed and we only have open classes, we
--     cannot always define corresponding instances which are
--     unambiguous. An example of this correspondence is
--     'Lurried'/'Lurry'.
--
{-# LANGUAGE
  TypeFamilies
, FlexibleInstances
, FlexibleContexts
, DataKinds
, GADTs
, AllowAmbiguousTypes #-}
module Data.Function.Tacit
( Lurried
, Lurry(lurry)
, s, z
, _1, _2, _3, _4, _5, _6, _7, _8, _9
, Nat(Z, S)
, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9
, Take
, lurryA
, shift
) where



import Prelude ((.), fst, snd)



-- | \"Curry\" a function type with a tuple-list argument.
--
--   Example:
--
--   @Lurried ((a, (b, (c, ()))) -> d) ~ a -> b -> c -> d@
--
type family Lurried (a :: *) where
  Lurried ((a, ()     ) -> r) = a -> r
  Lurried ((a, (b, cs)) -> r) = a -> Lurried ((b, cs) -> r)
--



-- | \"Curry\" a function with a tuple-list argument.
--
--   This type class should be treated as closed. The instances provided
--   map exactly to the type-level recursion defined by 'Lurried'.
--
--   Use 'lurryA' instead of 'lurry', which helps resolve ambiguity.
--
class Lurry f where
  lurry :: f -> Lurried f
--



-- | Base case for 'Lurry'.
instance Lurry ((a, ()) -> r) where
  lurry f = \a -> f (a, ())
--



-- | Recursive case for 'Lurry'.
instance (Lurry ((b, cs) -> r)) => Lurry ((a, (b, cs)) -> r) where
  lurry f = \x -> lurry (\xs -> f (x, xs))
--



-- | First argument.
z :: (a, b) -> a
z = fst



-- | Next argument.
s :: (a, b) -> b
s = snd



-- | First argument.
_1 :: (a, b) -> a
_1 = z



-- | Second argument.
_2 :: (a, (b, c)) -> b
_2 = z.s



-- | Third argument.
_3 :: (a, (b, (c, d))) -> c
_3 = z.s.s



-- | Fourth argument.
_4 :: (a, (b, (c, (e, f)))) -> e
_4 = z.s.s.s



-- | Fifth argument.
_5 :: (a, (b, (c, (e, (f, g))))) -> f
_5 = z.s.s.s.s



-- | Sixth argument.
_6 :: (a, (b, (c, (e, (f, (g, h)))))) -> g
_6 = z.s.s.s.s.s



-- | Seventh argument.
_7 :: (a, (b, (c, (e, (f, (g, (h, i))))))) -> h
_7 = z.s.s.s.s.s.s



-- | Eighth argument.
_8 :: (a, (b, (c, (e, (f, (g, (h, (i, j)))))))) -> i
_8 = z.s.s.s.s.s.s.s



-- | Ninth argument.
_9 :: (a, (b, (c, (e, (f, (g, (h, (i, (j, k))))))))) -> j
_9 = z.s.s.s.s.s.s.s.s



-- | Cap a tuple-list to the given length.
--
--   Example:
--
--   @Take N2 (a, (b, (c, d))) ~ (a, (b, ()))@
--
type family Take (n :: Nat) (p :: *) where
  Take (S Z) (a, _      ) = (a, ())
  Take (S n) (a, (b, cs)) = (a, (Take n (b, cs)))
--



-- | Lurry a function of given arity. This arity must match exactly to
--   the highest index used to avoid ambiguity (see the module docs).
--   Otherwise, an explicit signature for the function must be given.
--
--   Example:
--
--   @lurryA \@N2 (_1 <*> _2) = ($)@
--
lurryA :: ( Take n p ~ p'
          , p ~ p'
          , Lurry (p -> r)
          ) =>
          (p -> r) -> Lurried (p' -> r)
lurryA = lurry



-- | Peano numbers.
--
data Nat where
  Z :: Nat 
  S :: Nat -> Nat
--



-- | The Peano number 0.
type N0 = Z



-- | The Peano number 1.
type N1 = S N0



-- | The Peano number 2.
type N2 = S N1



-- | The Peano number 3.
type N3 = S N2



-- | The Peano number 4.
type N4 = S N3



-- | The Peano number 5.
type N5 = S N4



-- | The Peano number 6.
type N6 = S N5



-- | The Peano number 7.
type N7 = S N6



-- | The Peano number 8.
type N8 = S N7



-- | The Peano number 9.
type N9 = S N8



-- | Increments the argument indices of a function.
--
--   Example:
--
--   @shift (_1 <*> _2) = _2 <*> _3@
--
shift :: ((b, c) -> d) -> (a, (b, c)) -> d
shift f (_, (b, c)) = f (b, c)
