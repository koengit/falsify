# falsify

This module provides the function:

> falsifyBox :: StdGen              -- random seed
>           -> ([Double] -> Double) -- function to falsify f(xs)=y
>           -> [(Double,Double)]    -- specify for each x what interval to search
>           -> [([Double],Double)]  -- a lazy list of attempts (xs,y)

Example use:

> putStrLn
>   $ unlines
>   $ map show
>   $ takeUntil (\(xs,y) -> y<=0)
>   $ falsifyBox rnd f [(-3,3),(-3,3)]
> 
> f [x,y] = (1-x)^2 + 100*(y - x^2)^2 -- "Rosenbrock's banana function"
> rnd     = mkStdGen 117

The aim for "falsifyBox f" is to find xs::[Double] such that f(xs)=y<=0.

The algorithm uses a Bayesian optimization inspired method for finding
g(x)=y<=0 for a 1-dimensional function g. This particular method
uses line segments and slope differences to model functions.

For n dimensions, a point is repeatedly improved by shooting a random line
through the point, and using the 1-dimenstional algorithm to improve it.

The method works well when small changes to the inputs yield small changes
to the output most of the time. Try to make the output as sensitive to
small changes as you can. Local minima are not much of a problem!
