-- a)
strings :: Int -> [ String ]
strings 0 = [""]
strings n = concat ( map (\ x -> map (\ tail -> x : tail ) tails ) ['a'.. 'z'])
    where tails = strings (n -1)

removeFirst :: [x] -> [x]
removeFirst [] = []
removeFirst (x:xs) = xs

stringToPalindrome :: [Char] -> Int -> [Char]
stringToPalindrome x n  | (mod n 2)==0 = concat ([x, (reverse x)])
                            | otherwise = concat ([x, (removeFirst (reverse x))])

stringsToPalindrome :: [String] -> Int -> [String]
stringsToPalindrome [] n = []
stringsToPalindrome (x:xs) n = (stringToPalindrome x n) : stringsToPalindrome xs n


palindromesN :: Int -> [ String ]
palindromesN 0 = []
palindromesN n = stringsToPalindrome (strings n) n

palindromesAboveN :: Int -> [ String ]
palindromesAboveN n = concat (palindromesN n : [palindromesAboveN (n+1)])

palindromes :: [String]
palindromes = palindromesAboveN 0

main :: IO()
main = print (palindromesN 2) --palindromes