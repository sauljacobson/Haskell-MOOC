updateAt :: Int -> a -> [a] -> [a]
updateAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs


