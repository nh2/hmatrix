{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
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
import Data.List(elemIndex)

-- reference implementation
-- TODO: move default defs to the class
--       create optimized instances for Int32, Int64
--       check defs, create tests

instance Element Int

instance SContainer Vector Int where
    scale x = mapVector (x*)
    addConstant x = mapVector (x+)
    add = zipVectorWith (+)
    sub = zipVectorWith subtract
    mul = zipVectorWith (*)
    equal u v = dim u == dim v && maxElement (mapVector abs (sub u v)) == 0
    scalar x = fromList [x]
    konst v n = fromList (replicate n v)
    build = buildV
    cmap = mapVector
    atIndex = (@>)
    minIndex     = pos minimum
    maxIndex     = pos maximum
    minElement  = minimum . toList
    maxElement  = maximum . toList
    sumElements  = sum . toList
    prodElements = product . toList
    step = undefined -- cannot match (FIXME)
    find = findV
    assoc = assocV
    accum = accumV
    cond = undefined -- cannot match (FIXME)

pos f v = p where
    Just p = elemIndex (f l) l
    l = toList v


type instance RealOf Int = Double

instance Product Int where
    multiply a b = fromLists [[ doth ai bj | bj <- toColumns b] | ai <- toRows a ]
      where doth u v = sum $ zipWith (*) (toList u) (toList v)
    dot u v = (asRow u `multiply` asColumn v) @@> (0,0)
    absSum = fromIntegral . sumElements . mapVector abs
    norm1 = absSum
    norm2 v = sqrt $ fromIntegral (v `dot` v)
    normInf = fromIntegral . maxElement . mapVector abs


instance Num (Vector Int) where
    (+) = adaptScalar addConstant add (flip addConstant)
    negate = scale (-1)
    (*) = adaptScalar scale mul (flip scale)
    signum = mapVector signum
    abs = mapVector abs
    fromInteger = fromList . return . fromInteger

