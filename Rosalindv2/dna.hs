import Data.Map

data DnaCountingResults =
  DnaCountingResults
  { aCount :: Int
  , cCount :: Int
  , gCount :: Int
  , tCount :: Int
  , unrecognized :: Map Char Int 
  } deriving (Show)


nucleotidesCount :: [Char] -> DnaCountingResults
nucleotidesCount [] =
  DnaCountingResults
  { aCount = 0
  , cCount = 0
  , gCount = 0
  , tCount = 0
  , unrecognized = empty 
  }
nucleotidesCount (x:xs) =
  let y = nucleotidesCount xs
      f :: Maybe Int -> Maybe Int
      f = \x -> case x of
                    Nothing -> Just 1 
                    Just i -> Just (i + 1) 
  in case x of
       'A' -> y { aCount = aCount y + 1 }
       'C' -> y { cCount = cCount y + 1 }
       'G' -> y { gCount = gCount y + 1 }
       'T' -> y { tCount = tCount y + 1 }
       _  ->  y { unrecognized  =
                  alter f x ( unrecognized y )
                }




test1 =  "AAA"

test2 = "AAAXAAYASSSAAAA"
