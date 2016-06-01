# Data.Function.Tacit

Write functions in tacit (pointless) style using Applicative and De
Bruijn index notation.

Examples:

- ```
  \f x y -> f x == f y
  = lurryA @N3 ((==) <$> (_1 <*> _2) (_1 <*> _3))
  ```
- ```
  \f g x -> f x (g x)
  = lurryA @N3 ((_1 <*> _3) <*> (_2 <*> _3))
  ```
- ```
  \a b -> b
  = lurryA @N2 _2
  ```

This module is intended to be used with `Control.Applicative` but
does not export it.

Opposite to De Bruijn indices, this module orders the arguments
from the outside-in, rather than the inside-out (or left-to-right
instead of right-to-left). For example, the conventional
`λλλ3 1 (2 1)` is instead `λλλ1 3 (2 3)`.
  
The first argument is `z`, the second argument `z.s`, the third
argument `z.s.s`, and so on. For the first few arguments convenient
names have been defined, such as `_1`, `_2`, `_3`, and so on.

To export a function use `lurryA`. You must specify the arity of
the function, which is intended to be done with TypeApplications
(new in GHC 8.0).  `lurryA @(S Z) f` says the arity of @f@ is one,
`lurryA @(S (S Z)) f` says the arity is two, and so on. For
convenience the first few Peano numbers have been given aliases,
such as `N1`, `N2`, `N3`, and so on.

You can write all functions with `<*>` and `<$>` from
`Applicative` — should be able to, yet unproven.

There is a type inference problem with functions where the highest
index does not match the function arity, such as `const`. To
resolve this ambiguity you must give an explicit signature. For
example: `lurryA @N2 (_1 :: (a, (b, c)) -> a)`.

TODO:

- Construction rules for rewriting functions into this tacit form.
  More precise than just examples. Would demonstrate that any
  function can be written in this tacit form.
- An inverse for `lurry`, `unlurry`. Type inference seems
  difficult.
- Inference problem when the highest index does not match the
  function arity.

NOTES:

- The implementation would be simpler and less prone to inference
  problems if GHC had closed classes. Given a type family `F`, a
  corresponding value-level implementation may exist for
  `x -> F x`. This implementation can be given by a class and an
  instance corresponding to each case in the type family. However,
  if the type family is closed and we only have open classes, we
  cannot always define corresponding instances which are
  unambiguous. An example of this correspondence is
  `Lurried`/`Lurry`.
