-- NUMERI
-- 1
fact 0 = 1
fact n = n * fact (n-1)

--2
combinations n k = fact(n) / ((fact (n - k)) * fact k)

-- LISTE
-- 1

removeEavenPositions [] = []
removeEavenPositions list = removeEavenPositions' list  1 [] 
removeEavenPositions' (x:xs) n acc = if( (mod n 2 ) > 0) then removeEavenPositions' xs (n+1) (x:acc) else removeEavenPositions' xs (n+1) acc
removeEavenPositions' [] n acc = reverse acc

-- 2

sumNotEavenElements list = foldl (+) 0  (removeEavenPositions' list 1 [])
 
-- MATRICI
-- 1

--I suppose that the structure of the matrix is [ [ 1,2,...n] , [ 1,2,...,n], [1,2, ... ,n] ] dim_matrix = 3 n
-- This function suppose the structure is right ancd check if is a valid matrix 
dim_list [] = 0
dim_list list = dim_list' list 0

dim_list' [] n = n 
dim_list' (x:xs) n = dim_list' xs (n+1)

dim_matrix_all_row_check (x:xs) l = if ((dim_list x) == l ) then dim_matrix_all_row_check xs l else False
dim_matrix_all_row_check [] n = True

dim_matrix [] = [0,0]
dim_matrix (x:xs) = if dim_matrix_all_row_check (x:xs) (dim_list x) then [ dim_list (x:xs) , dim_list x] else [-1,-1]

--2

rowSum [] = []
rowSum (x:xs) =  [ foldl (+) 0 x ] : rowSum xs 

get_n_of_list list n = get_n_of_list' list n 1
get_n_of_list' (x:xs) n acc = if (n == acc) then x else get_n_of_list' xs n (acc+1)

get_n_of_list_of_list (x:xs) n = (get_n_of_list x n) : (get_n_of_list_of_list xs n)
get_n_of_list_of_list [] n = []

transpose (x:xs) = transpose' (x:xs) 1 (dim_list(x))
transpose' (x:xs) n  acc = if(n <= acc) then (get_n_of_list_of_list (x:xs) n)  : transpose' (x:xs) (n+1) acc  else []

colSum list = rowSum (transpose list)

-- ALBERI BINARI DI RICERCA 
--1
data BST a = Nil | Node {
val :: a ,
left , right :: BST a
} deriving ( Eq , Ord , Read , Show )

bstSum Nil = 0
bstSum (Node x l r) = x + bstSum l + bstSum r
--2
bstSumOdd Nil = 0
bstSumOdd (Node a l r) = if ((mod a 2) == 0 ) then bstSumOdd l + bstSumOdd r else a + bstSumOdd l + bstSumOdd r

--3

samesums list = sameContent (map bstSum list)
sameContent [] = True
sameContent (x:xs) = sameContent' xs x
sameContent' [] x = True
sameContent' (x:xs) l = if(l == x) then sameContent' xs l else False

-- if my bst is not ordered (so only a bynary tree), how could i verify every for every node?
--4
bstElem x Nil = False
bstElem y (Node x l r) 
    | x == y    = True
    | y < x     = bstElem y l
    | otherwise = bstElem y r


-- ALBERI GENERICI

data Tree a = Nil' | Node' a [Tree a] deriving (Eq, Show)