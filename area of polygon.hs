-- Enter your code here. Read input from STDIN. Print output to STDOUT

distance (x0, y0) (x1, y1) = sqrt $ (x1 - x0) ** 2 + (y1 - y0) ** 2

readPoints acc 0 = return acc
readPoints acc n = do
    point <- fmap ((\[x, y] -> (x, y)) . (fmap read) . words) getLine
    readPoints (point:acc) (n - 1)

area points pointCount = 0.5 * abs (sumProduct series - sumProduct series')
    where
        indices = [0..(pointCount - 1)]
        indices' = [1..(pointCount - 1)] ++ [0]
        mkSeries = zipWith (\x y -> (fst $ points !! x, snd $ points !! y))
        series = mkSeries indices indices'
        series' = mkSeries indices' indices
        sumProduct = foldl (+) 0 . map (\(x, y) -> x * y)


main = do
    pointCount <- fmap read getLine
    points <- readPoints [] pointCount
    putStrLn $ show $ area points pointCount
