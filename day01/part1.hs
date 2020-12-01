main = do
    list <- map read . words <$> getContents :: IO [Int]
    print(product $ filter (\x -> 2020-x `elem` list) list)
