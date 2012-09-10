{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Util.Integer
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra.Util.Integer(
) where

import Data.Packed
import Numeric.ContainerBoot

-- reference implementation

instance Element Int

instance SContainer Vector Int where
    scale x = mapVector (x*)
    addConstant x = mapVector (x+)
    add = zipVectorWith (+)
    sub = zipVectorWith subtract
    mul = zipVectorWith (*)
--    equal u v = dim u == dim v && maxElement (vectorMapF Abs (sub u v)) == 0.0
    scalar x = fromList [x]
--    konst = constantD
--    build = buildV
    cmap = mapVector
    atIndex = (@>)
--    minIndex     = round . toScalarF MinIdx
--    maxIndex     = round . toScalarF MaxIdx
--    minElement  = toScalarF Min
--    maxElement  = toScalarF Max
--    sumElements  = sumF
--    prodElements = prodF
--    step = stepF
--    find = findV
--    assoc = assocV
--    accum = accumV
--    cond = condV condF

instance Product Int


adaptScalar f1 f2 f3 x y
    | dim x == 1 = f1   (x@>0) y
    | dim y == 1 = f3 x (y@>0)
    | otherwise = f2 x y

instance Num (Vector Int) where
    (+) = adaptScalar addConstant add (flip addConstant)
    negate = scale (-1)
    (*) = adaptScalar scale mul (flip scale)
    -- signum = vectorMapF Sign
    -- abs = vectorMapF Abs
    fromInteger = fromList . return . fromInteger

