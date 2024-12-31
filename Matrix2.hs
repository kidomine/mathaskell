module Matrix2
  ( Point  (..)
  , Matrix (..)
  , mat2_ident
  , mat2_det
  , mat2_inv
  , mat2_smul
  , mat2_sdiv
  , mat2_mmul
  , mat2_mdiv
  , mat2_madd
  , mat2_msub
  ) where
  
  import Matrix

  mat2_pivot :: Matrix -> Matrix
  mat2_pivot (Matrix2x2 (Point2D a b) (Point2D c d))
    = Matrix2x2
      (Point2D d ((-1) * b))
      (Point2D ((-1) * c) a)
      
  mat2_ident :: Matrix
  mat2_ident = Matrix2x2 (Point2D 1 0) (Point2D 0 1)
  
  mat2_det :: Matrix -> Float
  mat2_det (Matrix2x2 (Point2D a b) (Point2D c d))
    = (a * d) - (b * c)
    
  mat2_inv :: Matrix -> Matrix
  mat2_inv (matrix)
    = mat2_sdiv (mat2_pivot (matrix), mat2_det (matrix))
 
  mat2_smul :: (Matrix, Float) -> Matrix
  mat2_smul (
    Matrix2x2 (Point2D a b) (Point2D c d), s)
    = Matrix2x2
      (Point2D (a * s) (b * s))
      (Point2D (c * s) (d * s))
    
  mat2_sdiv :: (Matrix, Float) -> Matrix
  mat2_sdiv (
    Matrix2x2 (Point2D a b) (Point2D c d), s)
    = Matrix2x2
      (Point2D (a / s) (b / s))
      (Point2D (c / s) (d / s))
    
  mat2_madd :: (Matrix, Matrix) -> Matrix
  mat2_madd (
    (Matrix2x2 (Point2D a b) (Point2D c d)),
    (Matrix2x2 (Point2D e f) (Point2D g h)))
    = Matrix2x2
      (Point2D (a + e) (b + f))
      (Point2D (c + g) (d + h))
  
  mat2_msub :: (Matrix, Matrix) -> Matrix
  mat2_msub (
    (Matrix2x2 (Point2D a b) (Point2D c d)),
    (Matrix2x2 (Point2D e f) (Point2D g h)))
    = Matrix2x2
      (Point2D (a - e) (b - f))
      (Point2D (c - g) (d - h))
    
  mat2_mmul :: (Matrix, Matrix) -> Matrix
  mat2_mmul (
    (Matrix2x2 (Point2D a b) (Point2D c d)),
    (Matrix2x2 (Point2D e f) (Point2D g h)))
    = Matrix2x2
      (Point2D
        (mat2_dot ((Point2D a c), (Point2D e f)))
        (mat2_dot ((Point2D a c), (Point2D g h)))
      )
      (Point2D
        (mat2_dot ((Point2D b d), (Point2D e f)))
        (mat2_dot ((Point2D b d), (Point2D g h)))
      )

  mat2_mdiv :: (Matrix, Matrix) -> Matrix
  mat2_mdiv (matrix1, matrix2)
    = mat2_mmul(matrix1, (mat2_inv matrix2))

  
