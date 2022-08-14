import Data.Bool;
import Data.List;
import System.IO;
import Data.Complex;
import Data.List.Split;
import System.Environment;

main = hSetBuffering stdout NoBuffering >> getArgs >>= \(w:h:it:xmin:xmax:ymin:ymax:_) -> putStrLn ("P1\n" ++ w ++ " " ++ h) >> (mapM_ putChar {- (\a -> mapM_ putChar a >> putStrLn "") $ chunksOf (read w) -} $ (drawMandelbrot (read w) (read h) (read it) 2 (read xmin,read xmax) (read ymin,read ymax)));
--main = hSetBuffering stdout NoBuffering >> getArgs >>= \(w:h:it:xmin:xmax:ymin:ymax:_) -> putStrLn ("P1\n" ++ w ++ " " ++ h) >> (mapM_ putChar (drawMandelbrot (read w) (read h) (read it) 2 (read xmin,read xmax) (read ymin,read ymax)));

-- | @drawMandelbrot@ outputs a 1-bit 'String'-based bitmap image which
-- represents complex numbers' membership of the MANDELBROT set.
drawMandelbrot :: Int
               -- ^ This argument is the width of the output image.
               -> Int
               -- ^ This argument is the height of the output image.
               -> Int
               -- ^ This argvment is the number of iterations.
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
  numList = map toComplex [(b,a) | a <- reverse [1,2..h], b <- [1,2..w]]
  inSet c = not $ any ((>= t) . magnitude) $ take i $ iterate ((+c) . (**2)) 0
  toString = init . unlines . chunksOf w' . map (bool ' ' '█')
  toComplex (a,b) = m1 :+ m2
    where
    m1 = fst r + (a / w) * (snd r - fst r)
    m2 = fst m + (b / h) * (snd m - fst m)
  [w, h] = map fromIntegral [w', h'];
