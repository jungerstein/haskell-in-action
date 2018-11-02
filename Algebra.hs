module Algebra where
  import Data.Eigen.Matrix
  import Data.Complex
  import Debug.Trace

  the_map = Data.Eigen.Matrix.map

  einheit :: MatrixXcd -> MatrixXcd
  einheit x = identity (rows x) (cols x)
  friction_ :: MatrixXcd -> Complex Double -> MatrixXcd 
  friction_ x eps = x + (the_map (* eps) (einheit x)) 
  friction :: MatrixXcd -> MatrixXcd 
  friction = (flip friction_) 1e-8
  chain_product :: [MatrixXcd] -> MatrixXcd
  -- chain_product x = foldl (*) ((einheit.head) x) x
  chain_product [] = empty
  chain_product (car:[]) = car
  chain_product (car:cdr) = car * chain_product cdr
  chain_product_v_M_v :: [Complex Double] -> [MatrixXcd] -> [Complex Double] -> Double
  chain_product_v_M_v v_hor ms v_vert = the_real prod where
    prod = chain_product ([v_hor_mat]++ms++[v_vert_mat]) 
    v_hor_mat = fromList [v_hor]
    v_vert_mat = transpose(fromList [v_vert])
    n1 = rows v_hor_mat
    n2 = cols v_hor_mat
    n3 = rows v_vert_mat
    n4 = cols v_vert_mat
  dagger :: MatrixXcd -> MatrixXcd
  dagger = adjoint -- Merely the name we like. 
  the_val :: MatrixXcd -> Complex Double
  the_val = coeff 0 0
  the_real :: MatrixXcd -> Double
  the_real = realPart . the_val
  inverse_with_fric :: MatrixXcd -> MatrixXcd
  inverse_with_fric = inverse . friction
  outer_self_dagger_self :: [Complex Double] -> MatrixXcd
  outer_self_dagger_self l = (dagger v) * v where
    v = fromList [l]
  the_dot :: [Double] -> [Double] -> Double
  the_dot x y = Prelude.sum $ zipWith (*) x y
  ii = 0.0:+1.0


  test :: IO()
  test = do 
    print $ friction_ sample_matrix 1e-8
    print $ friction sample_matrix 
    print $ chain_product [sample_matrix, sample_matrix]
    print $ chain_product [sample_matrix, sample_matrix, sample_matrix]
    print $ dagger sample_complex
    print $ the_val sample_matrix
    print $ the_real sample_matrix
    print $ sample_inverse
    print $ sample_matrix * sample_inverse
    print $ outer_self_dagger_self [1:+2, 3:+4]
    print $ the_dot [1, 2] [10, 100]
    print $ chain_product_v_M_v [1, 2] [sample_matrix, sample_matrix] [5, 6]
    where
      sample_matrix = fromList [[1, 2], [3, 4]] :: MatrixXcd
      sample_complex = fromList [[1, 2], [3:+1, 4]] :: MatrixXcd
      sample_inverse = inverse_with_fric sample_matrix
    
     
