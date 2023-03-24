% CS 340: Programming Paradigms and Patterns
% Lect 09 - Functors, Applicatives, and Monads
% Michael Lee

\begin{code}
module Lect09 where
import Prelude hiding (Functor, fmap, (<$>),
                       Applicative, pure, (<*>),
                       Monad, (>>=), (>>), return)
import Data.List hiding (find)
\end{code}


Functors, Applicatives, and Monads
==================================

Agenda:
  - Functors
  - Applicatives
  - Monads


Functors
--------

Functors are a class of types that support a "mapping" operation. An instance of
functor must be a polymorphic type, as a type variable will be used to repesent
the target of the mapped function. Here's the `Functor` class:

\begin{code}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
\end{code}

Note that the kind of type `f` implied by the class definition above is 
`* -> *`, as `f a` and `f b` in the type definition of `fmap` use `f` as a type
constructor.

---

We can make a list a Functor, where `fmap` is identical to `map`

<<<<<<< HEAD
> instance Functor [] where
>   fmap = map
=======
\begin{code}
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = undefined
\end{code}
>>>>>>> 47e06c2bd6abaad4c5446e15d12485e64899436d

Note that the implied type of `fmap` in the instance (replacing `f` from the
class definition with the instance `[]`) is:

    fmap :: (a -> b) -> [a] -> [b]

Which is exactly what we want.

We can now do

    fmap (2*) [1..10]

to map the function (2*) over the values in the list.

---

Let's define `fmap` for the Maybe type:

<<<<<<< HEAD
> instance Functor Maybe where
>   fmap f Nothing = Nothing
>   fmap f (Just x) = Just (f x)
=======
\begin{code}
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = undefined
\end{code}
>>>>>>> 47e06c2bd6abaad4c5446e15d12485e64899436d

we can now do:

\begin{verbatim}
  fmap (2*) Just 10
  
  fmap (100^) Nothing
\end{verbatim}


If we consider `Nothing` to be the result of a failed computation, it makes
sense that `fmap`-ing over it also results in `Nothing`.     

---

We can define an infix form of `fmap`:

\begin{code}
infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
\end{code}

Think of this operator as representing function application to values in an 
arbitrary context, where the context is represented by a functor instance. 

E.g.,

\begin{verbatim}
  reverse <$> ["Hello", "how", "are", "you"]
  
  reverse <$> Just "Aloha"
  
  reverse <$> Nothing
\end{verbatim}

---

Let's define `fmap` for a tree:

<<<<<<< HEAD
> data Tree a = Node a [Tree a] | Leaf a
>
> instance Functor Tree where
>   fmap f (Leaf x) = Leaf $ f x
>   fmap f (Node x ts) = Node (f x) $ fmap (fmap f) ts
=======
\begin{code}
data Tree a = Node a [Tree a] | Leaf a

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap = undefined
\end{code}

>>>>>>> 47e06c2bd6abaad4c5446e15d12485e64899436d

Here's a `Show` instance that makes it easier to visualize trees and a few trees
to play with:

\begin{code}
-- Show instance below is to make visualizing tree changes easier!
instance (Show a) => Show (Tree a) where
  show :: Show a => Tree a -> String
  show t = s 0 t
    where s n (Leaf x) = replicate n '.' ++ show x ++ "\n"
          s n (Node x t) = replicate n '.'
                               ++ show x ++ "\n"
                               ++ concat (map (s (n+2)) t)

t1 :: Tree String
t1 = Node "Animals" [
       Leaf "Arthropods", 
       Node "Chordates" [ Leaf "Birds", Leaf "Mammals", Leaf "Reptiles" ],
       Leaf "Nematodes"
     ]

t2 :: Tree Integer
t2 = Node 8 [
       Node 5 [ 
           Node 2 [Leaf 1, Leaf 1], 
           Node 3 [Leaf 1, 
                   Node 2 [Leaf 1, Leaf 1] ]
       ],
       Node 3 [Leaf 1, 
                Node 2 [Leaf 1, Leaf 1] ]
     ]
\end{code}

---

Even functions can be thought of as Functors! A function with type `a -> b` is a
computation that takes a type `a` and eventually produces a type `b`; i.e., it
is a computational context for a value of type `b`.

The `->` operator itself is a type constructor with kind `* -> * -> *`, so we
can write a Functor instance for the type `((->) a)` (note that `a` is the first
/ left-hand argument to the type constructor `->`):

<<<<<<< HEAD
> instance Functor ((->) a) where
>   fmap = (.)
=======
\begin{code}
instance Functor ((->) a) where
  fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = undefined
\end{code}
>>>>>>> 47e06c2bd6abaad4c5446e15d12485e64899436d

How should we interpret the following?

\begin{verbatim}
  ((2*) <$> (5+) <$> (100-)) 90    
\end{verbatim}

---

While Functors represent computation contexts, they are quite limited. We cannot
arbitrarily place new values in Functor contexts, nor can we "combine" separate
contexts (e.g., one containing a function and another a value). Applicatives
let us do that.


Applicative Functors
--------------------

The Applicative class extends Functors with additional methods. "pure" takes a
value and wraps it in a Functor instance, while "<*>" applies a function found
in one Functor to a value in another Functor.

\begin{code}
class (Functor f) => Applicative f where
  pure :: a -> f a
  
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b
\end{code}

---

Let's make `Maybe` an Applicative instance:

<<<<<<< HEAD
> instance Applicative Maybe where
>   pure = Just
>
>   Just f <*> Just x = Just $ f x
>   _ <*> _ = Nothing
=======
\begin{code}
instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = undefined
  
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) = undefined
\end{code}

>>>>>>> 47e06c2bd6abaad4c5446e15d12485e64899436d

Now we can do:

\begin{verbatim}
  pure (2*) <*> pure 5

  pure ("hello" ++ ) <*> Just "world"

  (*) <$> Just 2 <*> Just 5

  (\x y z -> x++y++z)  <$> Just "hello" <*> Just "hola" <*> Just "hi"
\end{verbatim}


More interestingly, if any of the arguments are `None`, the Applicative instance
handles it correctly:

\begin{verbatim}
  (*) <$> Nothing <*> Just 5

  (\x y z -> x++y++z)  <$> Just "hello" <*> Nothing <*> Just "hi"
\end{verbatim}

---

To make a list an Applicative instance, we have to decide what sort of context a
list represents for its values, and how to combine them (via `<*>`). One way is
just to think of them as ordered sequences of values, and for `<*>` to apply a
function in the first list to the corresponding element in the second:

<<<<<<< HEAD
> instance Applicative [] where
>   pure x = repeat x
>
>   [] <*> _ = []
>   _ <*> [] = []
>   (f:fs) <*> (x:xs) = f x : (fs <*> xs)
=======
\begin{code}
instance Applicative [] where
  pure :: a -> [a]
  pure = undefined
  
  (<*>) :: [a -> b] -> [a] -> [b]
  (<*>) = undefined
\end{code}
>>>>>>> 47e06c2bd6abaad4c5446e15d12485e64899436d

Try:

\begin{verbatim}
  [(2^), (5+), (3*)] <*> [5..7]
\end{verbatim}

A more interesting approach would be to think of a list as representing a
non-deterministic context --- i.e., one in which all choices are equally likely.
To combine two lists with `<*>`, then, would mean that we have to generate all
possible combinations of results!

We can't define multiple instances of a class for a given type, so if we want to
define a separate Applicative instance for the list type, we have to create a
new type based on the list. We can do this with the `newtype` keyword, which
lets us create a type based on another with a single value constructor:

\begin{code}
newtype NDList a = NDList [a] deriving Show
\end{code}

Now we can make `NDList` an instance of Functor and Applicative:

<<<<<<< HEAD
> instance Functor NDList where
>   fmap f (NDList l) = NDList $ map f l
>
> instance Applicative NDList where
>   pure x = NDList [x]
>
>   NDList fs <*> NDList xs = NDList [f x | f <- fs, x <- xs]
=======
\begin{code}
instance Functor NDList where
  fmap :: (a -> b) -> NDList a -> NDList b
  fmap = undefined

instance Applicative NDList where
  pure :: a -> NDList a
  pure = undefined
  
  (<*>) :: NDList (a -> b) -> NDList a -> NDList b
  (<*>) = undefined
\end{code}

>>>>>>> 47e06c2bd6abaad4c5446e15d12485e64899436d

Try:

\begin{verbatim}
  NDList [("Hello "++)] <*> NDList ["Michael", "Mary", "Beth"]

  NDList [(2^), (5+), (3*)] <*> NDList [5..7]

  NDList [(,)] <*> NDList [1..10] <*> NDList "hello"
\end{verbatim}

The built-in Applicative list instance is based on this non-deterministic
interpretation.

---

We can also make a function an Applicative:

<<<<<<< HEAD
> instance Applicative ((->) a) where
>   pure x = \_ -> x
>
>   f <*> g = \x -> (f x) (g x)
=======
\begin{code}
instance Applicative ((->) a) where
  pure :: b -> (a -> b)
  pure = undefined

  (<*>) :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
  (<*>) = undefined
\end{code}
>>>>>>> 47e06c2bd6abaad4c5446e15d12485e64899436d

What does this do?

\begin{verbatim}
  (zip <*> drop 5) [1..10]
\end{verbatim}

---

Applicatives are handy for when we need to apply "pure" functions to values in
contexts represented by Functors. A pure function is one that does not wrap its
return value in a context. 

E.g., 

\begin{verbatim}
  even <$> [1..10]

  (+) <$> Just 5 <*> Just 10

  (++) <$> ["hello, ", "hola, "] <*> ["mister", "senor"]

  (++) <$> NDList ["hello, ", "hola, "] <*> NDList ["mister", "senor"]
\end{verbatim}

But what happens when we want to apply an "impure" function --- i.e., one that
returns its own context --- to a value in a Functor? 

E.g., recall the `find` function:

\begin{code}
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) | p x = Just x
              | otherwise = find p xs
\end{code}

What happens when we do the following?

\begin{verbatim}
  find even <$> Just [1..10]

  find (>100) <$> Just [1..100]
\end{verbatim}

We end up with contexts within contexts ... where what we likely want is to
somehow merge the contexts together in a meaningful way (e.g., in a `Maybe`
Functor, `Just` represents a successful computation, so `Just (Just x)` should
probably just be interpreted as `Just x`, and `Just Nothing` should probably
just be `Nothing`).



Monads
------

The Monad class further extends Applicatives so that they support the additional
methods `>>=` ("bind"), `>>` ("sequence"), and `return`:

\begin{code}
class (Applicative m) => Monad m where
  infixl 1 >>=
  (>>=) :: m a -> (a -> m b) -> m b -- called "bind"

  infixl 1 >>
  (>>) :: m a -> m b -> m b -- called "sequence"
  x >> y = x >>= \_ -> y

  return :: a -> m a
  return = pure
\end{code}

As with the preceding classes, a Monad represents a context for computations or
values. The `>>=` (bind) operator takes a monad and a function, applies the
function to the value in the monad, which generates another monad, then combines
the contexts of the two monads to produce a result.

`>>` (sequence) has a default implementation built from `>>=`, and `return` is a
synonym for `pure`.

---

Let's make `Maybe` a Monad instance:

<<<<<<< HEAD
> instance Monad Maybe where
>   Nothing >>= _ = Nothing
>   Just x >>= f = f x
=======
\begin{code}
instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) = undefined
\end{code}
>>>>>>> 47e06c2bd6abaad4c5446e15d12485e64899436d

Now we get sensible results by doing:

\begin{verbatim}
  Just [1..10] >>= find even

  Just [1..10] >>= find (>100)
\end{verbatim}

---

For a better example of how this is useful, consider `div`, defined in Prelude:

    div :: Integral a => a -> a -> a

`div` throws an exception if the denominator is 0. We can improve on this by
writing `safeDiv` as follows:

\begin{code}
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x `div` y
\end{code}

Imagine that we need to implement a function that computes the following with
integral operands:

                          (a / b) + (c / d)
    fDivs a b c d e f  =  ----------------- * f
                                  e

Without bind, we might write:

<<<<<<< HEAD
> fDivs :: Integral a => a -> a -> a -> a -> a -> a -> Maybe a
> fDivs a b c d e f = case a `safeDiv` b 
>                     of Nothing -> Nothing
>                        Just r ->
>                          case c `safeDiv` d 
>                          of Nothing -> Nothing
>                             Just r' ->
>                               case (r + r') `safeDiv` e 
>                               of Nothing -> Nothing
>                                  Just r'' -> Just $ r'' * f

Or we can use our bind operator:

> fDivs' :: Integral a => a -> a -> a -> a -> a -> a -> Maybe a
> fDivs' a b c d e f = a `safeDiv` b >>= \r ->
>                      c `safeDiv` d >>= \r' ->
>                      (r + r') `safeDiv` e >>= \r'' ->
>                      Just $ r'' * f
=======
\begin{code}
fDivs :: Integral a => a -> a -> a -> a -> a -> a -> Maybe a
fDivs a b c d e f = undefined
\end{code}

Or we can use our bind operator:

\begin{code}
fDivs' :: Integral a => a -> a -> a -> a -> a -> a -> Maybe a
fDivs' a b c d e f = undefined
\end{code}
>>>>>>> 47e06c2bd6abaad4c5446e15d12485e64899436d

Notice how each bind operation is followed by a lambda that is passed the value
"inside" the preceding monad, and how each lambda itself evaluates to a monad.
Also, note that lambda bodies extend as far towards the end of the expression as
possible, so a fully parenthesized body of `fDivs'`` would look like this:

    a `safeDiv` b >>= \r -> (
      c `safeDiv` d >>= \r' -> (
        (r + r') `safeDiv` e >>= \r'' -> (
          Just $ r'' * f
        )
      )
    )

Finally, and perhaps most importantly, the bind operation takes care of the
logic of chaining together multiple monadic values. In this case, it means we
don't need to keep checking to see if any of the `saveDiv` calls failed!

---

The lambda chaining is such a common pattern that Haskell provides special
syntax for doing this -- "do notation":

\begin{code}
fDivs'' :: Integral a => a -> a -> a -> a -> a -> a -> Maybe a
fDivs'' a b c d e f = do r   <- a `safeDiv` b
                         r'  <- c `safeDiv` d
                         r'' <- (r + r') `safeDiv` e
                         return $ r'' * f
\end{code}

Each line in a do block represents a monadic value, and "<-" appears to allow us
to "extract" the contents of a monad. Behind the scenes, what's really going on
is that the bind operator (>>=) is automatically being invoked between lines!

If a line in a `do` block does not use the `<-` operator, then it and the
following line are combined using the `>>` (sequence) operator. For example,
translate the following `do` block to lambda notation using `>>=` and `>>`:

    do r1 <- func1 x
       r2 <- func2 y
       func3 r1 r2
       r3 <- func4 z
       func5 r3
       return (r1, r2, r3)

    func1 x >>= \r1 ->
    func2 y >>= \r2 ->
    func3 r1 r2 >>
    func4 z >>= \r3 ->
    func5 r3 >>
    return (r1, r2, r3)

`do` blocks also support `let` definitions, but `in` is omitted, e.g.:

    do r1 <- func1 w
       r2 <- func2 x
       let y = pureFunc r1 r2
           z = pureVal
       r3 <- func3 y z
       return (r1, r2, r3)


Laws
----

Properly implemented instances of Functors, Applicatives, and Monads should
conform to a handful of laws. These laws exist to ensure that all class
instances behave in a predictable way. The compiler does not, however, enforce
these laws for us!

-- Functor laws

  1. Identity:
    
       fmap id = id

  2. Composition:

       fmap (f . g) = (fmap f . fmap g)

-- Applicative laws

  1. Identity:

       pure id <*> v = v

  2. Homomorphism:

       pure f <*> pure x = pure (f x)

  3. Interchange:

       u <*> pure x = pure ($ x) <*> u

  4. Composition:

       pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    
-- Monad laws

  1. Left Identity:

       return x >>= f = f x

     or equivalently: 
     
       do { y <- return x; f y } = do { f x }

  2. Right Identity:

       m >>= return = m

     or equivalently:

       do { x <- m; return x } = do { m }

  3. Associativity:

       (m >>= \x -> f x) >>= g = m >>= (\x -> f x >>= g)
    
     or equivalently:

       do                     do
         y <- do x <- m   =     x <- m
                 f x            y <- f x
         g y                    g y
