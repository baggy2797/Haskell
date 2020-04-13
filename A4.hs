-- Question 1: maximum element of a list maxlist lt
maxlist :: (Ord a) => [a] -> a  
maxlist [] = error "maximum of empty list"  
maxlist [x] = x  
maxlist (x:xs) = max x (maxlist xs)


-- Question 2: delete every kth element delete k lt
delete :: Int -> [a] -> [a]
delete = recur 1
    where recur _ _ []     = []
          recur i n (x:xs) = if i == n
            then recur 1 n xs
            else x:recur (i+1) n xs


-- Question 3: isort lt sort using insertion sort
--helper for insertion sort
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) = if x < y 
                 then x:y:ys 
         else y : insert x ys
-- insertion sort
isort :: [Int] -> [Int]
isort [x] = [x]
isort (x:xs) = insert x (isort xs)


-- Question 4: rotate n lt
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop (n+1) (cycle xs)) xs


-- Question 5: single lt that is list of list
single :: [a]->[[a]]
single [] = []
single(x:xs) =[x]:single xs


-- Question 6: double lt doubles every element at the odd position of the list
double :: Num a => [a] -> [a]
double [] = []
double [x] = [x]
double (x:y:xs) = (x *2) : (y) : double xs


