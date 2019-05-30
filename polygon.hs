distance (x0, y0) (x1, y1) = sqrt $ (x1 - x0) ** 2 + (y1 - y0) ** 2

readPoints acc 0 = return acc
readPoints acc n = do
    point <- fmap ((\[x, y] -> (x, y)) . (fmap read) . words) getLine
    readPoints (point:acc) (n - 1)

perimeter [] = 0
perimeter [_] = 0
perimeter (point:rest) = fst $ foldl (\(acc, p0) p1 -> (acc + distance p0 p1, p1)) (0, point) (rest ++ [ point ])

main = do
    pointCount <- fmap read getLine
    points <- readPoints [] pointCount
    putStrLn $ show $ perimeter points
