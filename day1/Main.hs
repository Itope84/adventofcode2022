import Data.List

main = do
  content <- readFile "input.txt"
  -- let linesOfFile = lines content
  let x = getElvesCalories content
  print (maxCal x)
  print (top3Cals x)

maxCal :: [Int] -> Int
maxCal [a] = a
maxCal (x:xs) | x > y = x
          | otherwise = y
      where y = maxCal xs

totalCal :: [Int] -> Int
totalCal [] = 0
totalCal (x:xs) = x + totalCal xs

top3Cals arr = a1 + a2 + a3 
    where a1 = sorted!!0
          a2 = sorted!!1
          a3 = sorted!!2
          sorted = reverse (sort arr)

-- foldr f a [b, c, d] takes last item in the list (d) and applies f d a, then sets that as a and continues with the list: foldr f [b, c] result
splitBy delimiter = foldr f [[]] 
      -- The list passed as 2nd arg is passed as list@(x:xs) e.g. [a, b, c] -> [a,b,c]@(a: [b, c])
      -- f (last line of input: numberstr) [[]] yields [] ++ numberstr, which is passed back to foldr, so we have 
      -- f nextlastline [numberstr] = [nextlastline, numberstr] until we have a blank line when we will return [] : [existing list] and we start a new list
      where f c list@(x:xs) | c == delimiter = []:list
                       | otherwise = (c:x):xs

getElvesCalories :: String -> [Int]
getElvesCalories "" = []
getElvesCalories input = map f1 (splitBy "" (lines input))
  where f1 arr = foldr (+) 0 (map f2 arr)
        f2 str = read str :: Int