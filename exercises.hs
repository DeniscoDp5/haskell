-- NUMERI
-- 1
fact 0 = 1
fact n = n * fact (n-1)

--2
combinations n k = fact(n) / ((fact (n - k)) * fact k)

-- LISTE
-- 2

removeEavenPositions [] = []
removeEavenPositions list = removeEavenPositions' list  1 [] 
removeEavenPositions' (x:xs) n acc = if( (mod n 2 ) > 0) then removeEavenPositions' xs (n+1) (x:acc) else removeEavenPositions' xs (n+1) acc
removeEavenPositions' [] n acc = reverse acc

-- 3

sumNotEavenElements list = foldl (+) 0  (removeEavenPositions' list 1 [])
 

