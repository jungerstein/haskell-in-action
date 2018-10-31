module Vector where
  dot :: [Float] -> [Float] -> Float
  dot a b = sum $ zipWith (*) a b

  cross :: [Float] -> [Float] -> [Float]
  cross [a, b, c] [x, y, z] = [
        b * z - c * y, 
        c * x - a * z, 
        a * y - b * x
      ]

  norm :: [Float] -> Float
  norm a = sqrt $ dot a a

  unit :: [Float] -> [Float]
  unit a = map (/ r) a where r = norm a

  mix_product :: [Float] -> [Float] -> [Float] -> Float
  mix_product a b c = dot a $ cross b c

  abs_cross :: [Float] -> [Float] -> Float
  abs_cross a b = norm $ cross a b

  abs_mix_product :: [Float] -> [Float] -> [Float] -> Float
  abs_mix_product a b c = abs $ mix_product a b c

  test :: IO()
  test = do
    print $ dot [1, 2, 3] [4, 5, 6]
    print $ cross [1, 2, 3] [4, 5, 6]
    print $ norm [3, 4, 12]
    print $ unit [2.0/3, 1.0/3, -2.0/3]
    print $ mix_product [1, 0, 0] [0, 1, 0] [0, 1, -1]
    print $ abs_cross [1, 2, 3] [4, 5, 6]
    print $ abs_mix_product [1, 0, 0] [0, 1, 0] [0, 1, -1]
