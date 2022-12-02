module Helpers where

-- foldr f a [b, c, d] takes last item in the list (d) and applies f d a, then sets that as a and continues with the list: foldr f [b, c] result
splitBy delimiter = foldr f [[]]
  where
    -- The list passed as 2nd arg is passed as list@(x:xs) e.g. [a, b, c] -> [a,b,c]@(a: [b, c])
    -- f (last line of input: numberstr) [[]] yields [] ++ numberstr, which is passed back to foldr, so we have
    -- f nextlastline [numberstr] = [nextlastline, numberstr] until we have a blank line when we will return [] : [existing list] and we start a new list
    f c list@(x : xs)
      | c == delimiter = [] : list
      | otherwise = (c : x) : xs