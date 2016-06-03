module RegularExpressions where
import Data.List
import Prelude hiding ((<*>))

type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (== "")

char :: Char ->  RegExp
char ch = (== [ch])

(|||) :: RegExp -> RegExp -> RegExp
(|||) e1 e2 =
  \s -> e1 s || e2 s

splits :: [a] -> [([a],[a])]
-- splits "fun" ~~>
-- [("","fun"),("f","un"),
--  ("fu","n"),("fun","")]
splits xs =
  map (flip splitAt xs) [0..length xs]
-- alternatively, we could also use a list comprehension like so
-- [splitAt i xs | i <- [0..length xs]]

(<*>) :: RegExp -> RegExp -> RegExp
(<*>) e1 e2 =
  \s -> or [ e1 prefix && e2 suffix | (prefix,suffix) <- splits s]

(<**>) :: RegExp -> RegExp -> RegExp
(<**>) e1 e2 =
  \s -> or [ e1 prefix && e2 suffix | (prefix,suffix) <- drop 1 (splits s)]

star :: RegExp -> RegExp
star e = epsilon ||| (e <**> star e)

-- put your solutions here

-- option
option :: RegExp -> RegExp
option func a 
    |a == "" = True
    |func a == True = True
    |otherwise = False

-- plus
plus :: RegExp -> RegExp
plus func a 
    |a == "" = False
    |func a == True = True
    |plus func (tail a) == True = True
    |otherwise = False

-- number
number :: RegExp
number func
    |digits func == False = False
    |(head func == '0') && (length func /= 1) == True = False
    |otherwise = True

-- fractional number
fractional :: RegExp
fractional func
    |func == "" = True
    |removeDot(splitAt (fIndex func) func) == False = False
    |(head func == '0') && (length func /= 3)= False
    |otherwise = True


-- helper functions    
digits :: RegExp
digits func
    |func == "" = True
    |notElem (head func) ['0', '1'..'9'] == True = False
    |otherwise = digits (tail func)


digitsAndDot :: RegExp
digitsAndDot func
    |func == "0.0" = True 
    |notElem (head func) ['0','1','2','3','4','5','6','7','8','9','.'] == True =False
    |otherwise = digitsAndDot (tail func)
    
    
removeDot :: (String,String) -> Bool
removeDot ([],y) = False
removeDot (x,[]) = False
removeDot (x,y)
    | (number x) && (tail' (tail y)) == True = True
    |otherwise = False


tail' :: String ->Bool
tail' x 
    | (last x == '0') && (length x > 1) == True = False
    | otherwise = True


fIndex :: String -> Int
fIndex x 
    | length x == 0 = 0
    | head x /= '.' = fIndex (tail x) + 1
    | otherwise = 0