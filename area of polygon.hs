-- Enter your code here. Read input from STDIN. Print output to STDOUT

distance (x0, y0) (x1, y1) = sqrt $ (x1 - x0) ** 2 + (y1 - y0) ** 2

readPoints acc 0 = return acc
readPoints acc n = do
    point <- fmap ((\[x, y] -> (x, y)) . (fmap read) . words) getLine
    readPoints (point:acc) (n - 1)

-- https://www.wikihow.com/Sample/Area-of-an-Irregular-Polygon
area points pointCount = 0.5 * abs (sumProduct pointsPairedForward - sumProduct pointsPairedBackwards)
    where
        pointsPairedForward = undefined
        pointsPairedBackwards = undefined
        sumProduct = foldl (+) 0 . map (\(x, y) -> x * y)


main = do
    pointCount <- fmap read getLine
    points <- readPoints [] pointCount
    putStrLn $ show $ area points pointCount
