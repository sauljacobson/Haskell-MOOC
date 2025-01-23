import Data.List 

-- Ex 1 
years = [1982, 2004, 2020] 

-- Ex 2
takeFinal :: Int -> [a] -> [a] 
takeFinal n xs 
    | length xs < n = xs 
    | otherwise     = drop (length xs - n) xs 

-- Ex 3 
updateAt :: Int -> a -> [a] -> [a] 
updateAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs 

-- Ex 4 
substring :: Int -> Int -> String -> String 
substring i j s = drop i (take j s)

-- Ex 5 
isPalindrome :: String -> Bool 
isPalindrome str = str == reverse str 

-- Ex 6 
palindromify :: String -> String 
palindromify str 
    | null str = "" 
    | str == reverse str = str
    | str /= reverse str = palindromify (drop 1 (init str))

-- Ex 7
safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y) 

-- Ex 8 
greet :: String -> Maybe String -> String 
greet first Nothing = "Hello, " ++ first ++ "!"
greet first (Just last) = "Hello, " ++ first ++ " " ++ last ++ "!"

-- Ex 9 
safeIndex :: [a] -> Int -> Maybe a 
safeIndex list index
    | index > (length list - 1) = Nothing 
    | index < 0                 = Nothing 
    | otherwise                 = Just (list !! index) 

-- Ex 10 
eitherDiv :: Integer -> Integer -> Either String Integer
eitherDiv x 0 = Left (show x ++ "/" ++ show 0)  
eitherDiv x y = Right (x `div` y)

-- Ex 11 
addEithers :: Either String Int -> Either String Int -> Either String Int
addEithers (Right x) (Right y) = Right (x + y) 
addEithers (Right x) (Left y) = Left y 
addEithers (Left x) (Right y) = Left x
addEithers (Left x) (Left y) = Left x

