{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Data.Sphere where

import Linear.Metric
import Linear.Vector
import Linear.V3

data Sphere = Sphere
  { center :: V3 Float
  , radius :: Float
  } deriving (Eq, Ord, Show)

rayIntercept :: Sphere -> V3 Float -> V3 Float -> Maybe Float
rayIntercept Sphere { .. } orig dir
  | d2 > radius * radius = Nothing
  | t0 < 0 && t1 < 0 = Nothing
  | t0 < 0 = Just t1
  | otherwise = Just t0
  where
    l = center ^-^ orig
    tca = l `dot` dir
    d2 = l `dot` l - tca * tca

    thc = sqrt $ radius * radius - d2
    t0 = tca - thc
    t1 = tca + thc
