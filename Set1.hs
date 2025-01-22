-- Ex.1 
one :: Int
two :: Int 

one = 1  
two = 2

-- Ex.2 
double :: Integer -> Integer
double x = x * 2

-- Ex.3
quadruple :: Integer -> Integer 
quadruple x = double (double x)


-- Ex.4
--distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt(square (x1 - x2) + square (y1 - y2))

square x = x * x


-- Ex.5
eeny :: Integer -> String
eeny n = if even n then "eeny" else "meeny" 

-- Ex.6
checkPassword :: String -> String
checkPassword password = if password == "swordfish" || password == "mellon"
                         then "You're in."
                         else "ACCESS DENIED!" 

-- Ex.7
postagePrice :: Int -> Int 
postagePrice weight = if weight > 5000
                 then 6000
                 else 
                    if weight > 500
                    then 300 + (weight - 500)
                    else 250

-- Ex.8 
isZero :: Integer -> Bool 
isZero 0 = True
isZero n = False 

-- Ex.9
sumTo :: Int -> Int
sumTo 1 = 1
sumTo n = n + sumTo(n - 1)

-- Ex.10 
power :: Integer -> Integer -> Integer 
power n 0 = 1
power n 1 = n
power n k = n * power n (k - 1) 

-- Ex.11
ilog3 :: Integer -> Integer 
ilog3 0 = 0
ilog3 n = 1 + ilog3 (n `div` 3)

