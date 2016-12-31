module Matrix
  ( Point  (..)
  , Matrix (..)
  , mat2_dot
  , mat3_dot
  ) where
    
  {-|
    2x2 matrix m =  a c
                    b d

                    a d g
    3x3 matrix m =  b e h
                    c f i
  -}
  
  data Point
    = Point2D { x :: Float, y :: Float }
    | Point3D { x :: Float, y :: Float, z :: Float }
    deriving (Show)
    
  data Matrix
    = Matrix2x2 { pt1 :: Point, pt2 :: Point }
    | Matrix3x3 { pt1 :: Point, pt2 :: Point, pt3 :: Point }
    deriving (Show)
  
  mat2_dot :: (Point, Point) -> Float
  mat2_dot (
    (Point2D a b), (Point2D c d))
    = (a * c) + (b * d)
    
  mat3_dot :: (Point, Point) -> Float
  mat3_dot (
    (Point3D a b c), (Point3D d e f))
    = (a * d) + (b * e) + (c * f)