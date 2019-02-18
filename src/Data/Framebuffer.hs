module Data.Framebuffer where

newtype Pixel = Pixel (Double, Double, Double) deriving (Eq, Ord, Show)

data Framebuffer = Framebuffer
  { width :: Int
  , height :: Int
  , pixels :: [Pixel]
  } deriving (Eq, Ord, Show)
