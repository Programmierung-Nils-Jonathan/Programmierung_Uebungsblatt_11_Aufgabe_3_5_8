import Data.List

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

-- b)
divisors :: Int -> [Int]
divisors x = filter (\y -> mod x y == 0) [1.. div x 2]

perfect :: [Int]
perfect = filter (\x -> x == sum (divisors x )) [2..]

addsUpTo :: Int -> [Int] -> Bool
addsUpTo x y = (sum y) == x

isSemiperfect :: Int -> Bool
isSemiperfect x = any (addsUpTo x) (subsequences (divisors x))

semiperfectNumbesAboveN :: Int -> [Int]
--semiperfectNumbesAboveN 50 = [] --limit n for testing
semiperfectNumbesAboveN n   | isSemiperfect n = n : (semiperfectNumbesAboveN (n+1))
                            | otherwise = semiperfectNumbesAboveN (n+1)

semiperfectNumbers :: [Int]
semiperfectNumbers = semiperfectNumbesAboveN 0

main :: IO()
main = do
    print ("a)")
    print (palindromesN 2) --palindromes
    print ("b)")
    print ("isSemiperfect 4 (Expected: False)")
    print (isSemiperfect 4)
    print ("isSemiperfect 12 (Expected: True)")
    print (isSemiperfect 12)
    print ("semiperfectNumbers")
    --print (semiperfectNumbers) --will not terminate
