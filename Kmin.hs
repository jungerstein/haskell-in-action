module Kmin where
  import Vector
  
  cal_h_ :: [Float] -> [Float] -> [Float] -> Float
  cal_h_ a b c = (Vector.abs_mix_product a b c) / (2 * (Vector.abs_cross a b))

  cal_h :: [[Float]] -> Float
  cal_h l = cal_h_ (l!!0) (l!!1) (l!!2)

  cal_k_min_ :: [Float] -> [Float] -> [Float] -> Float
  cal_k_min_ a b c = minimum (map cal_h combinations) where
     combinations = [[a, b, c], [b, c, a], [c, a, b]]

  cal_k_min :: [[Float]] -> Float
  cal_k_min l = cal_k_min_ (l!!0) (l!!1) (l!!2)

  test :: IO()
  test = do
    print $ cal_h_ [1, 0, 0] [0, 1, 0] [0, 0, 1]
    print $ cal_h [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    print $ cal_k_min_ [1, 0, 0] [0, 1, 0] [0, 0, 1]
    print $ cal_k_min [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
  
