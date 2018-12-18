module CaponFilter where
  import Algebra
  import Data.Eigen.Matrix
  import Data.Complex

  type LocStars = [[Double]]

  h_vec :: [Double] -> LocStars -> [Complex Double]
  h_vec kappa loc = [exp((-ii) * ((the_dot kappa a_loc):+0)) | a_loc <- loc]
  s_vec :: [Double] -> LocStars -> [Complex Double]
  s_vec k loc = [exp(ii * ((the_dot k a_loc):+0)) | a_loc <- loc]
  filter_theory :: [Double] -> [Double] -> LocStars -> Double
  filter_theory_step_big_R_inv :: [Double] -> LocStars -> MatrixXcd
  filter_theory_step_big_R_inv k loc = inverse_with_fric (outer_self_dagger_self s) where
    s = s_vec k loc
  filter_theory_step_final :: MatrixXcd -> [Double] -> LocStars -> Double
  filter_theory_step_final big_R_inv kappa loc = 1.0 / (chain_product_v_M_v h_dagger [big_R_inv] h) where
    h = h_vec kappa loc
    h_dagger = Prelude.map Data.Complex.conjugate h
  filter_theory kappa k loc = filter_theory_step_final (filter_theory_step_big_R_inv k loc) kappa loc

  test :: IO()
  test = do 
    print "Basic Tests for Capon Filter"
    print $ h_vec [10.0, 20.0, 30.0] loc_stars
    print $ s_vec [10.0, 20.0, 30.0] loc_stars
    print $ filter_theory [10, 20, 30] [10, 20, 30] loc_stars
    where
      loc_stars = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [-2, -3, -4]]
