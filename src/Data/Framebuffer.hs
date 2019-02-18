{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Strict #-}

module Data.Framebuffer where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.String.Interpolate.IsString as I
import qualified Data.Vector.Unboxed as V

newtype Width = Width { getWidth :: Int } deriving (Eq, Ord, Show, Enum, Num, Real, Integral)
newtype Height = Height { getHeight :: Int } deriving (Eq, Ord, Show, Enum, Num, Real, Integral)
newtype Idx = Idx (Width, Height) deriving (Eq, Ord, Show)

idx :: Framebuffer -> Idx -> Int
idx Framebuffer { .. } (Idx (w, h)) = getHeight h * getWidth width + getWidth w

data Framebuffer = Framebuffer
  { width :: Width
  , height :: Height
  , pixels :: V.Vector (Float, Float, Float)
  } deriving (Eq, Ord, Show)

(./.) :: Integral a => a -> a -> Float
a ./. b = fromIntegral a / fromIntegral b

uniformFB :: Width -> Height -> Framebuffer
uniformFB width height = Framebuffer { .. }
  where pixels = V.fromList pxList
        pxList =
          [ (j ./. height, i ./. width, 0)
          | j <- [0 .. hBound]
          , i <- [0 .. wBound]
          ]
        (wBound, hBound) = (width - 1, height - 1)

fb2ppm :: Framebuffer -> BSL.ByteString
fb2ppm Framebuffer { .. } = [I.i|P6\n#{getWidth width} #{getHeight height}\n255\n|] <> pixelsData
  where pixelsData = BSL.pack
          [ showPart p
          | (r, g, b) <- V.toList pixels
          , p <- [r, g, b]
          ]
        showPart c = round $ (255 *) $ clamp 0 1 c
        clamp lo hi = max lo . min hi
