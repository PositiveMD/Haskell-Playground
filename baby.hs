doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int 
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber' :: Int -> Int
doubleSmallNumber' x = (if x > 100
						then x
						else x*2) + 1


conanO'Brien = "It's a-me, Conan O'Brien!"

boomBang :: [Int] -> [String]
boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' :: [Int] -> Int
length' xs = sum [1 | _ <-xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]