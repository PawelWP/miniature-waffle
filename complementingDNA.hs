


complementingDna xs = reverse [if x == 'A' then 'T' else if x == 'T' then 'A' else if  x == 'C' then 'G'else if x == 'G' then 'C' else x  | x <- xs]
