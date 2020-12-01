productOfPairWhoseSumIs :: [Int] -> Int -> Int
productOfPairWhoseSumIs l s = product $ filter (\x -> s-x `elem` l) l

solve :: [Int] -> Int
solve l = a * pair 
    where
        productForSum s = productOfPairWhoseSumIs l (2020-s)
        -- Assumes <1,1,x> is not a triplet from answer (numbers are actually unique).
        a = head $ filter (\x -> (productForSum x) > 1) l
        pair = productForSum a

main = do
    list <- map read . words <$> getContents :: IO [Int]
    print(solve list)
