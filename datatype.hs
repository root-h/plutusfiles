data Point = Point Float Float | Point Int Int deriving Show
data Fo =Volta Point deriving Show
kanko :: Fo -> Float | Int 
 kanko Volta t x = x + t
kanko (Volta z y) = z * y


nudge :: Fo -> Float -> Float -> Fo
nudge (Aire (Point x y) r) a b = Aire (Point (x+a) (y+b)) r
nudge (Volta (Point x1 y1) (Point x2 y2)) a b = Volta (Point (x1+a) (y1+b)) (Point (x2+a)
 (y2+b))

data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)
scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n