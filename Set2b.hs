-- Ex 1 
binomial :: Integer -> Integer -> Integer
binomial n 0 = 1 
binomial 0 k = if k > 0 then 0 else 1 
binomial n k = (binomial (n - 1) k) + (binomial (n - 1) (k - 1)) 

-- Ex 2 
oddFactorial :: Integer -> Integer
oddFactorial n
    | n == 1 = 1 
    | even n = 1 * oddFactorial (n - 1)
    | odd n  = n * oddFactorial (n - 1) 

-- Ex 3
myGcd :: Integer -> Integer -> Integer  
myGcd a b
    | a == 0    = b 
    | b == 0    = a  
    | a > b     = myGcd (a - b) b 
    | a <= b    = myGcd a (b - a)   

-- Ex 4 
leftpad :: String -> Int -> String 
leftpad str p 
    | length str >= p = str 
    | otherwise       = " " ++ leftpad str (p - 1)

-- Ex 5
countdown :: Integer -> String
countdown' :: Integer -> Integer -> String
countdown n = countdown' n n
countdown' n m
    | m <= 0 = "Liftoff!"
    | n == m = "Ready! " ++ show m ++ "... " ++ countdown' n (m - 1)
    | otherwise = show m ++ "... " ++ countdown' n (m - 1)

-- Ex 6 
smallestDivisor :: Integer -> Integer
smallestDivisor' :: Integer -> Integer -> Integer 

smallestDivisor n = smallestDivisor' n 2 
smallestDivisor' n m
    | n `mod` m == 0 = m
    | otherwise      = smallestDivisor' n (m + 1)

-- Ex 7 
isPrime :: Integer -> Bool 
isPrime n = smallestDivisor n == n 

-- Ex 8 
biggestPrimeAtMost :: Integer -> Integer 
biggestPrimeAtMost' :: Integer -> Integer -> Integer

biggestPrimeAtMost n = biggestPrimeAtMost' n n 
biggestPrimeAtMost' n m
    | isPrime m = m
    | otherwise = biggestPrimeAtMost' n (m - 1) 

