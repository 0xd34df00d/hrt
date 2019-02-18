{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Framebuffer where

import qualified Data.Array.IArray as A
import qualified Data.ByteString as BS
import qualified Data.String.Interpolate.IsString as I

newtype Pixel = Pixel (Float, Float, Float) deriving (Eq, Ord, Show)

newtype Width = Width { getWidth :: Int } deriving (Eq, Ord, Show, Enum, Num, Real, Integral, A.Ix)
newtype Height = Height { getHeight :: Int } deriving (Eq, Ord, Show, Enum, Num, Real, Integral, A.Ix)
newtype Idx = Idx (Width, Height) deriving (Eq, Ord, Show, A.Ix)

data Framebuffer = Framebuffer
  { width :: Width
  , height :: Height
  , pixels :: A.Array Idx Pixel
  } deriving (Eq, Ord, Show)

(./.) :: Integral a => a -> a -> Float
a ./. b = fromIntegral a / fromIntegral b

uniformFB :: Width -> Height -> Framebuffer
uniformFB width height = Framebuffer { .. }
  where pixels = A.listArray (Idx (0, 0), Idx (wBound, hBound)) pxList
        pxList =
          [ (Idx (i, j), px)
          | i <- [0 .. wBound]
          , j <- [0 .. hBound]
          , let px = Pixel (j ./. height, i ./. width, 0)
          ]
        (wBound, hBound) = (width - 1, height - 1)

fb2ppm :: Framebuffer -> BS.ByteString
fb2ppm Framebuffer { .. } = [I.i|P6\n#{getWidth width} #{getHeight height}\n255\n|] <> pixelsData
  where pixelsData = BS.pack
          [ showPart p
          | j <- [0 .. height - 1]
          , i <- [0 .. width - 1]
          , let Pixel (r, g, b) = pixels A.! Idx (i, j)
          , p <- [r, g, b]
          ]
        showPart c = round $ (255 *) $ clamp 0 1 c
        clamp lo hi = max lo . min hi
