-- |Daniel Gallab
-- |CPSC 326-02
-- |Assignment 9
-- |use patterns and guards to implement the 10 functions in assignment 8

-- | myMin :: Ord a => [a] -> a
myMin [x] = x
myMin (x:y:zs) 
    |x > y = myMin(y:zs)
    |otherwise = myMin(x:zs)

-- | myRev :: [t] -> [t]
myRev [] = []
myRev (x:xs) = (myRev xs)++[x]

-- | myLen :: Num t => [t1] -> t
myLen [] = 0 
myLen (x:xs) = 1+myLen xs

-- |myElem :: Eq t => t -> [t] -> Bool
myElem _ [] = False 
myElem n (x:xs)
       |x == n = True
       |otherwise = myElem n (xs)

-- |myElems :: Eq t => [t] -> [t] -> Bool
myElems [] _ = True
myElems (x:xs) ys = myElem(x)ys && myElems(xs)ys

-- |myRemElem :: Eq a => a -> [a] -> [a]
myRemElem n [] = []
myRemElem n (x:xs) 
         |n == x = myRemElem n (xs) 
         |otherwise = x:myRemElem n (xs)


count_digits x 
    |x <= 1 = 0
    |x mod 10 == 0 = 1 + count_digits (x div 10)
    |otherwise = count_digits (x div 10)

oddOccurrencesOfZeros [] = 0
oddOccurrencesOfZeros (x:a) = count_digits x + oddOccurrencesOfZeros a

-- |myRepl :: Eq a => (a, a) -> [a] -> [a]
myRepl p [] = [] 
myRepl p (x:xs) 
       |fst p == x = snd p:myRepl p (xs)
       |otherwise =  x:myRepl p (xs)

-- |mySub :: Eq a => [(a, a)] -> [a] -> [a]
mySub [] xs = xs
mySub (p:pxs) xs = mySub (pxs) (myRepl(p)xs)

-- |myElemSum :: (Num t, Eq t) => t -> [t] -> t
myElemSum _ [] = 0 
myElemSum n (x:xs) 
        |x == n = myElemSum n(xs)+x 
        |otherwise = myElemSum n(xs)

-- |myAtIndex :: (Eq t1, Num t1) => t1 -> [t] -> t
myAtIndex i (x:xs)
        |i == 0 = x
        |otherwise = myAtIndex (i-1) (xs)