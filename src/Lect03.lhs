% CS 340: Programming Paradigms and Patterns
% Lect 03 - Functions
% Michael Lee

> module Lect03 where
> import Data.Char

Functions
=========

Agenda:
  - Defining functions
    - Pattern matching
    - Guards
    - `where` clause
  - Some useful language constructs
    - `if-else` expressions
    - `case` expressions
    - `let-in` expressions


Defining Functions
------------------

Functions are defined with one or more equations. You should always include a type signature declaration alongside a function definition.

E.g., define the following functions:
  - nand (Boolean not-and)
  - c2f (convert Celsius to Fahrenheit)
  - distance (Euclidean distance between two points)

> nand :: Bool -> Bool -> Bool
> nand x y = not (x && y)
>
> c2f :: (Floating a) => a -> a
> c2f c = c * 9 / 5 + 32
>
> distance :: (Floating a) => (a, a) -> (a, a) -> a
> distance p1 p2 = sqrt ((fst p1 - fst p2)^2 + (snd p1 - snd p2)^2)


-- Pattern matching

Instead of using a variable in a function definition, we can use a *pattern* to match against the parameter value.

E.g., define `not` using pattern matching:

> not' :: Bool -> Bool
> not' True = False
> not' False = True


Patterns are matched top down. A variable can be used as a "catch-all" pattern.

E.g., define `fib` (to return the nth Fibonacci number ) using pattern matching:

> fib :: Integer -> Integer
> fib 0 = 0
> fib 1 = 1
> fib n = fib (n-1) + fib (n-2)


E.g., define `greet`, which returns an opinionated greeting:

> greet :: String -> String
> greet "Michael" = "Hey, old friend!"
> greet "Jane" = "Yo, Jane-o"
> greet name = "Hello, " ++ name


Sometimes we don't care about the value of a parameter. We use `_` as the matching variable name to indicate this.

E.g., define `nand` again using pattern matching:

> nand' :: Bool -> Bool -> Bool
> nand' False False = True
> nand' _ _ = False


Patterns can also be used to "deconstruct" values. 

E.g., define `fst` and `snd` using pattern matching:

> fst' :: (a,b) -> a
> fst' (x,_) = x
>
> snd' :: (a,b) -> b
> snd' (_,y) = y


E.g., redefine `distance` using pattern matching:

> distance' :: (Floating a) => (a, a) -> (a, a) -> a
> distance' (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)


E.g., define the `mapTup` function using pattern matching:

> mapTup :: (a -> b) -> (a, a) -> (b, b)
> mapTup f (x,y) = (f x, f y)


As-patterns can be used to bind a variable to a sub-pattern.

E.g., implement the (very contrived) function `foo`:

> foo :: (a, (b, c)) -> ((a, (b, c)), (b, c), (a, b, c))
> foo p@(x, q@(y, z)) = (p, q, (x, y, z))


-- Guards

Boolean "guards" can be used to select between multiple right-hand-sides in a single function equation (`otherwise` designates the default).

E.g., redefine `fib` using guards. Is it any clearer?

> fib' :: Integer -> Integer
> fib' n | n == 0 = 0
>        | n == 1 = 1
>        | otherwise = fib' (n-1) + fib' (n-2)

E.g., define `c2h`, which converts Celsius to a "human readable" string:

> c2h :: (Floating a, Ord a) => a -> String
> c2h c | c2f c >= 100 = "hot"
>       | c2f c >= 70  = "comfortable"
>       | c2f c >= 50  = "cool"
>       | otherwise    = "cold"

E.g., define `quadrant` which returns the quadrant of a point:

> quadrant :: (Num a, Ord a) => (a, a) -> Int
> quadrant (x, y) | x > 0 && y > 0 = 1
>                 | x < 0 && y > 0 = 2
>                 | x < 0 && y < 0 = 3
>                 | x > 0 && y < 0 = 4
>                 | otherwise = 0

E.g., define `c2h`, which converts Celsius to a "human readable" string:

> c2h' :: (Floating a, Ord a) => a -> String
> c2h' c | f >= 100 = "hot"
>        | f >= 70  = "comfortable"
>        | f >= 50  = "cool"
>        | otherwise    = "cold"
>       where f = c2f c


E.g., define `quadrant` which returns the quadrant of a point:


Some useful language constructs
-------------------------------

Note: all the constructs in this section define *expressions* --- i.e., each evaluates to a value (which must have a consistent, static type). They are not statements!


-- `if-then-else` expressions

Syntax:

    if e1 then e2 else e3


What's wrong with:

    if n < 0 then True else "False"


E.g., define `closer` which returns the point closest to a source point:

> closer :: (Floating a, Ord a) => (a, a) -> (a, a) -> (a, a) -> (a, a)
> closer src dst1 dst2 = if d1 < d2 then dst1 else dst2
>   where d1 = distance src dst1
>         d2 = distance src dst2


-- `case` expressions

`case` expressions are general pattern-matching forms.

Syntax:

    case exp of pat_1 -> e_1
                pat_2 -> e_2
                ...
                pat_n -> e_n

An `if-then-else` expression is just a special form of `case`:

    if e1 then e2 else e3 === case e1 of True  -> e2
                                         False -> e3

All result expressions must have the same type!

E.g., define `quadrantNames` which returns the name of a quadrant:

> quadrantNames :: (Int, Int) -> String
> quadrantNames (x, y) = case quadrant (x, y) of 1 -> "All"
>                                                2 -> "Science"
>                                                3 -> "Teachers"
>                                                4 -> "Crazy"
>                                                _ -> "Origin"


-- `let-in` expressions

`let` creates local bindings (for vars/fns) for the expression following `in`. These bindings can also perform pattern matching!

Syntax:

    let pat_1 = e_1
        pat_2 = e_2
        ...
        pat_n = e_n
    in e

E.g., define `quadRoots` which returns the roots of a quadratic equation:

> quadRoots :: Double -> Double -> Double -> (Double, Double)
> quadRoots a b c = let disc = a^2 - 4*a*c
>                       x1 = (-b + sqrt disc) / (2*a)
>                       x2 = (-b - sqrt disc) / (2*a)
>                   in (x1, x2)
