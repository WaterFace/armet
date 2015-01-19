module Vector where

import Control.Arrow

class Vector a where
    scale :: Double -> a -> a
    add :: a -> a -> a
    zero :: a

instance Vector Double where
    scale = (*)
    add = (+)
    zero = 0

instance (Vector a, Vector b) => Vector (a, b) where
    scale d = (scale d) *** (scale d)
    add (a, b) (a', b') = (add a a', add b b')
    zero = (zero, zero)

--instance Integral a => Vector a where
--    scale d = toIntegral