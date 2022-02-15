

import Data.List

transcript [] = "Empty list"
transcript xs =  [if x == 'T' then 'U' else x | x <- xs]






