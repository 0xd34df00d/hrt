{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Framebuffer where

import qualified Data.Array.IArray as A

newtype Pixel = Pixel (Double, Double, Double) deriving (Eq, Ord, Show)

newtype Width = Width { getWidth :: Int } deriving (Eq, Ord, Show, Enum, Num, Real, Integral, A.Ix)
newtype Height = Height { getHeight :: Int } deriving (Eq, Ord, Show, Enum, Num, Real, Integral, A.Ix)
newtype Idx = Idx (Width, Height) deriving (Eq, Ord, Show, A.Ix)

data Framebuffer = Framebuffer
  { width :: Width
  , height :: Height
  , pixels :: A.Array Idx Pixel
  } deriving (Eq, Ord, Show)

(./.) :: Integral a => a -> a -> Double
a ./. b = fromIntegral a / fromIntegral b

uniformFB :: Width -> Height -> Framebuffer
uniformFB width height = Framebuffer { .. }
  where pixels = A.array (Idx (0, 0), Idx (wBound, hBound)) pxList
        pxList =
          [ (Idx (i, j), px)
          | i <- [0 .. wBound]
          , j <- [0 .. hBound]
          , let px = Pixel (j ./. height, i ./. width, 0)
          ]
        (wBound, hBound) = (width - 1, height - 1)
