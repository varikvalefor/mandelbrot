import Data.Bool;
import Data.List;
import System.IO;
import Data.Complex;
import Control.Monad;
import Data.List.Split;
import System.Environment;
import System.OpenBSD.Plegg;

main :: IO ()
main = security >> nobuf >> getArgs >>= runWithArgs
  where
  security = plegg [Stdio] >> univac []
  nobuf = hSetBuffering stdout NoBuffering
  runWithArgs (w:h:it:xmin:xmax:ymin:ymax:_) = printDebug >> printReal
    where
    printDebug = putStrLn $ "P1\n" ++ w ++ " " ++ h
    printReal = mapM_ putChar howie
      where
      xr = (read xmin, read xmax)
      yr = (read ymin, read ymax)
      howie = drawMandelbrot (read w) (read h) (read it) 2 xr yr;

-- | @drawMandelbrot@ outputs a 1-bit 'String'-based bitmap image which
-- represents complex numbers' membership of the MANDELBROT set.
drawMandelbrot :: Int
               -- ^ This argument is the width of the output image.
               -> Int
               -- ^ This argument is the height of the output image.
               -> Int
               -- ^ This argument is the number of iterations.
               -> Double
               -- ^ This argument is the threshold of "hey, this thing
               -- isn't in the set."
               -> (Double, Double)
               -- ^ This tuple contains the beginning of the real range
               -- and the end of the real range, respectively.
               -> (Double, Double)
               -- ^ This tuple contains the beginning of the imaginary
               -- range and the end of the imaginary range,
               -- respectively.
               -> String;
drawMandelbrot w' h' i t r m = toString $ map inSet numList
  where
  numList = map toComplex $ liftM2 (,) yCoords xCoords
  inSet a = inMandelbrotSet a t i
  xCoords = [1..w]
  yCoords = [1..h]
  toString = unlines . chunksOf w' . map (bool ' ' '█')
  toComplex (b,a) = m1 :+ m2
    where
    m1 = fst r + (a / w) * (snd r - fst r)
    m2 = fst m + (b / h) * (snd m - fst m)
  [w, h] = map fromIntegral [w', h'];

-- @inMandelbrotSet t d k@ iff MANDELBROT function of @t@ exceeds @d@
-- within @k@ iterations.
inMandelbrotSet :: Complex Double
                -- ^ @inMandelbrotSet@ determines whether or not this
                -- number is in the MANDELBROT set.
                -> Double
                -- ^ This number is the threshold which is used to
                -- determine whether or not the MANDELBROT function of
                -- the first argument tends to infinity.
                -> Int
                -- ^ This number is the maximum number of iterations
                -- which are used to determine whether or not the
                -- MANDELBROT function of the first argument tends to
                -- infinity.
                -> Bool;
inMandelbrotSet c t i = not $ any ((>= t) . magnitude) $ take i $ iters
  where iters = iterate ((+c) . (**2)) 0;
