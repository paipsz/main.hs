-- Felipe Giraldo Neira CC 1020302073

import Data.Char (digitToInt)

aliquotSum :: Int -> Int
aliquotSum n = sum [i | i <- [1..n-1], n `mod` i == 0]

classifyNumber :: Int -> String
classifyNumber n
  | aliquotSum n == n = "Engineering"
  | aliquotSum n > n  = "Administrative"
  | otherwise         = "Humanities"

formatPeriod :: Int -> String
formatPeriod n =
  let year = 2000 + (n `div` 10)
      semester = n `mod` 10
  in show year ++ "-" ++ show semester

evenOdd :: Int -> String
evenOdd n
  | n `mod` 2 == 0 = "even"
  | otherwise      = "odd"

analyzeCode :: Int -> String
analyzeCode code =
  let period = code `div` 100000
      categoryNum = (code `div` 1000) `mod` 100
      admissionNum = code `mod` 1000
      periodStr = formatPeriod period
      categoryStr = classifyNumber categoryNum
      admissionStr = "num" ++ show admissionNum
      evenOddStr = evenOdd code
  in periodStr ++ " " ++ categoryStr ++ " " ++ admissionStr ++ " " ++ evenOddStr

main :: IO ()
main = do
  input <- getLine
  let code = read input :: Int
  putStrLn $ analyzeCode code

