import Data.List
convertToArray :: [a] -> [a]
convertToArray x = concat [ x ]

groupingDNA :: Ord a => [a] -> [Int]
groupingDNA (x:xs) = map length (group(sort(x:xs)))

