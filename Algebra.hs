module Algebra where
  import Numeric.LinearAlgebra.Data
  import Numeric.LinearAlgebra
  import Data.Complex
  import Debug.Trace

  the_map = cmap
  (>.<) = (Numeric.LinearAlgebra.<>)

  type MatrixC = Matrix C
  type MatrixR = Matrix R

  einheit :: MatrixC -> MatrixC
  einheit x = ident (rows x) 
  friction_ :: MatrixC -> Complex Double -> MatrixC 
  friction_ x eps = x + (the_map (* eps) (einheit x)) 
  friction :: MatrixC -> MatrixC 
  friction = (flip friction_) 1e-8
  chain_product :: [MatrixC] -> MatrixC
  chain_product (car:[]) = car
  chain_product (car:cdr) = car >.< (chain_product cdr)
  chain_product_v_M_v :: [Complex Double] -> [MatrixC] -> [Complex Double] -> Double
  chain_product_v_M_v v_hor ms v_vert = the_real prod where
    prod = chain_product ([v_hor_mat] ++ ms ++[v_vert_mat]) 
    v_hor_mat = fromLists [v_hor]
    v_vert_mat = fromLists (map (\x -> [x]) v_vert)
  dagger :: MatrixC -> MatrixC
  dagger = tr -- Merely the name we like. 
  the_val :: MatrixC -> Complex Double
  the_val = (`atIndex` (0, 0))
  the_real :: MatrixC -> Double
  the_real = realPart . the_val
  inverse_with_fric :: MatrixC -> MatrixC
  inverse_with_fric = inv . friction
  outer_self_dagger_self :: [Complex Double] -> MatrixC
  outer_self_dagger_self l = (dagger v) >.< v where
    v :: MatrixC
    v = fromLists [l]
  the_dot :: [Double] -> [Double] -> Double
  the_dot x y = Prelude.sum $ zipWith (*) x y
  ii = 0.0:+1.0


  test :: IO()
  test = do 
    print "Friction"
    print $ friction_ sample_matrix 1e-8
    print $ friction sample_matrix 
    print "Dot, a.k.a. chain_product"
    print $ chain_product [sample_matrix, sample_matrix]
    print $ chain_product [sample_matrix, sample_matrix, sample_matrix]
    print $ chain_product [sample_complex, sample_complex]
    print $ chain_product_v_M_v [1, 2] [sample_matrix, sample_matrix] [5, 6]
    print "Dagger"
    print $ dagger sample_complex
    print "Take value"
    print $ the_val sample_matrix
    print $ the_real sample_matrix
    print "Inverse"
    print $ sample_inverse
    print $ sample_complex >.< sample_inverse
    print "Self_dagger_self"
    print $ outer_self_dagger_self [1:+2, 3:+4]
    print "Real number dot"
    print $ the_dot [1, 2] [10, 100]
    where
      sample_matrix = fromLists [[1, 2], [3, 4]] :: MatrixC
      sample_complex = fromLists [[1, 2], [3:+1, 4]] :: MatrixC
      sample_inverse = inverse_with_fric sample_complex
    
     
