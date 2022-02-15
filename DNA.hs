import Data.List


groupingDNA :: Ord a => [a] -> String
groupingDNA [] = error "There is no length for empty list " 
groupingDNA (x:xs) = concatMap(++" "  show (map length (group(sort(x:xs)))))



