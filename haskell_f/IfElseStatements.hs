-- |Daniel Gallab
-- |CPSC 326-02
-- |Assignment 8
-- |use recursion to implement 10 functions


myMin xs = if length xs == 1 then head xs else if head xs >= head (tail xs)
                             then myMin (tail xs) else myMin ((head xs):tail(tail xs))
-- |Ord a => [a] -> a

myRev xs = if null xs then xs else (last xs):myRev (init xs)
-- |myRev :: [a] -> [a]

myLen xs = if null xs then 0 else myLen(tail xs)+1
-- |Num t => [a] -> t
myElem n xs = if null xs then False else if head xs == n then True 
              else myElem n (tail xs)
-- |Eq a => a -> [a] -> Bool
myElems xs ys = if null xs then True else if myElem (head xs) (ys) then myElems (tail xs) ys else False
-- |Eq a => [a] -> [a] -> Bool
myRemElem n xs = if null xs then xs else if n == head xs then myRemElem n (tail xs) else (head xs):myRemElem n (tail xs)
-- |Eq a => a -> [a] -> [a]
myRepl p xs = if null xs then xs else if fst p == head xs then snd p:myRepl p (tail xs)
              else head xs:myRepl p (tail xs) 
-- |Eq a => (a, a) -> [a] -> [a]
mySub pxs xs = if null pxs then xs 
               else mySub (tail pxs) (myRepl(head pxs)xs)
-- |Eq a => [(a, a)] -> [a] -> [a]
myElemSum n xs = if null xs then 0 else if head xs == n then myElemSum n(tail xs)+n 
                 else myElemSum n(tail xs)
-- |(Num a, Eq a) => a -> [a] -> a
myAtIndex i xs = if length xs == (i+1) then last xs else myAtIndex i (init xs)
-- | Int -> [a] -> a