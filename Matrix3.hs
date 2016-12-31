module Matrix3
  ( mat3_ident
  , mat3_det
  , mat3_inv
  , mat3_smul
  , mat3_sdiv
  , mat3_mmul
  , mat3_mdiv
  , mat3_madd
  , mat3_msub
  ) where
  
  import Matrix  
  import Matrix2
  
  mat3_ident :: Matrix
  mat3_ident = (Matrix3x3 (Point3D 1 0 0) (Point3D 0 1 0) (Point3D 0 0 1))
  
  mat3_det :: Matrix -> Float
  mat3_det (Matrix3x3 (Point3D a b c) (Point3D d e f) (Point3D g h i))
    = (a * mat2_det (Matrix2x2 (Point2D e f) (Point2D h i))) -
    (d * mat2_det (Matrix2x2 (Point2D b c) (Point2D h i))) +
    (g * mat2_det (Matrix2x2 (Point2D b c) (Point2D e f)))
    
  -- todo: need to fix this
  -- a d g
  -- b e h
  -- c f i
  mat3_minors :: Matrix -> Matrix
  mat3_minors (Matrix3x3 (Point3D a b c) (Point3D d e f) (Point3D g h i))
    = Matrix3x3
      (Point3D
        (mat2_det (Matrix2x2 (Point2D e f) (Point2D h i)))
        (mat2_det (Matrix2x2 (Point2D d f) (Point2D g i)))
        (mat2_det (Matrix2x2 (Point2D d e) (Point2D g h)))
      )
      (Point3D
        (mat2_det (Matrix2x2 (Point2D b c) (Point2D h i)))
        (mat2_det (Matrix2x2 (Point2D a c) (Point2D g i)))
        (mat2_det (Matrix2x2 (Point2D a b) (Point2D g h)))
      )
      (Point3D
        (mat2_det (Matrix2x2 (Point2D b c) (Point2D e f)))
        (mat2_det (Matrix2x2 (Point2D a c) (Point2D d f)))
        (mat2_det (Matrix2x2 (Point2D a b) (Point2D d e)))
      )
      
  mat3_cofactor :: Matrix -> Matrix
  mat3_cofactor (Matrix3x3 (Point3D a b c) (Point3D d e f) (Point3D g h i))
    = Matrix3x3
      (Point3D a ((-1) * b) c)
      (Point3D ((-1) * d) e ((-1) * f))
      (Point3D g ((-1) * h) i)

  mat3_pivot :: Matrix -> Matrix
  mat3_pivot (Matrix3x3 (Point3D a b c) (Point3D d e f) (Point3D g h i))
    = Matrix3x3 (Point3D a d g) (Point3D b e h) (Point3D c f i)
  
  mat3_inv :: Matrix -> Matrix
  mat3_inv (matrix)
    = mat3_smul
      (mat3_pivot
        (mat3_cofactor
          (mat3_minors (matrix))), (1 / mat3_det(matrix)))
    
  mat3_smul :: (Matrix, Float) -> Matrix
  mat3_smul (
    (Matrix3x3 (Point3D a b c) (Point3D d e f) (Point3D g h i)), s)
    = Matrix3x3
      (Point3D (a * s) (b * s) (c * s))
      (Point3D (d * s) (e * s) (f * s))
      (Point3D (g * s) (h * s) (i * s))
    
  mat3_sdiv :: (Matrix, Float) -> Matrix
  mat3_sdiv (
    (Matrix3x3 (Point3D a b c) (Point3D d e f) (Point3D g h i)), s)
    = Matrix3x3
      (Point3D (a / s) (b / s) (c / s))
      (Point3D (d / s) (e / s) (f / s))
      (Point3D (g / s) (h / s) (i / s))
    
  mat3_madd :: (Matrix, Matrix) -> Matrix
  mat3_madd (
    (Matrix3x3 (Point3D a b c) (Point3D d e f) (Point3D g h i)),
    (Matrix3x3 (Point3D j k l) (Point3D m n o) (Point3D p q r)))
    = Matrix3x3
      (Point3D (a + j) (b + k) (c + l))
      (Point3D (d + m) (e + n) (f + f))
      (Point3D (g + p) (h + q) (i + r))
  
  mat3_msub :: (Matrix, Matrix) -> Matrix
  mat3_msub (
    (Matrix3x3 (Point3D a b c) (Point3D d e f) (Point3D g h i)),
    (Matrix3x3 (Point3D j k l) (Point3D m n o) (Point3D p q r)))
    = Matrix3x3
      (Point3D (a - j) (b - k) (c - l))
      (Point3D (d - m) (e - n) (f - f))
      (Point3D (g - p) (h - q) (i - r))
    
  mat3_mmul :: (Matrix, Matrix) -> Matrix
  mat3_mmul (
    (Matrix3x3 (Point3D a b c) (Point3D d e f) (Point3D g h i)),
    (Matrix3x3 (Point3D j k l) (Point3D m n o) (Point3D p q r)))
    = Matrix3x3
      (Point3D
        (mat3_dot ((Point3D a b c), (Point3D j k l)))
        (mat3_dot ((Point3D d e f), (Point3D j k l)))
        (mat3_dot ((Point3D g h i), (Point3D j k l))))
      (Point3D
        (mat3_dot ((Point3D a b c), (Point3D m n o)))
        (mat3_dot ((Point3D d e f), (Point3D m n o)))
        (mat3_dot ((Point3D g h i), (Point3D m n o))))
      (Point3D
        (mat3_dot ((Point3D a b c), (Point3D p q r)))
        (mat3_dot ((Point3D d e f), (Point3D p q r)))
        (mat3_dot ((Point3D g h i), (Point3D p q r))))
    
  mat3_mdiv :: (Matrix, Matrix) -> Matrix
  mat3_mdiv (matrix1, matrix2)
    = mat3_mmul(matrix1, (mat3_inv matrix2))
