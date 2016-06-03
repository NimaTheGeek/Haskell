module SimpleFunctions where

-- Nima Mazraeh Farahani
-- homework 2 Problem 1

-- filter first element
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = error "wrong input!"
filterFirst func (x:xs)
    | func x =  x : filterFirst func xs
    | otherwise = drop 0 xs


-- filter last element
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast _ [] = error "wrong input!"
filterLast func xs = reverse(filterFirst func (reverse xs) )


-- split
split :: [a] -> ([a],[a])
split [] = ([],[])
split xs = (a,b)
    where a = map ((!!) xs ) [0,2..(length xs - 1)]
          b = map ((!!) xs ) [1,3..(length xs - 1)]

-- interleave
interleave :: ([a],[a]) -> [a]
interleave (x,[]) = x
interleave ([], y) = y
interleave (x:xs , y:ys) = x : y : interleave (xs, ys) 


--merge
merge :: (Ord a) => ([a],[a]) -> [a]
merge (x,[]) = x
merge ([], y) = y
merge (x:xs, ys) = sort'(x : merge(xs, ys))

-- sort 
sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' (x:xs) =
    let firstList = sort' (filter (<=x) xs)
        secList = sort' (filter (>x) xs)
    in  firstList ++ [x] ++ secList
 

-- mergSort
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort xs = merge (split xs) 
