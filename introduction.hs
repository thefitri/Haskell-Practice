import Data.List
import System.IO

{-
Int: -2^63 2^63
Integer: Unbounded
Float: Single Precision
Double: 11 Points Precision
Bool: True or False
Char: 'x'
Tupple: ()
-}

maxInt = maxBound :: Int
minInt = minBound :: Int

sumOfNums = sum [1..1000]

addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4

-- Prefix Operators
modEx = mod 5 4
-- Infix Operators
modEx2 = 5 `mod`4
-- Negative numbers need to be inside brackets
negNumEx = 5 + (-4)

num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)

-- Built in Math functions
piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.999
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999

-- Also sin, cos, tan,
-- asin, acos, atan,
-- sinh cosh, tanh,
-- asinh, acosh, atanh

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

{-
  use :t on GHCi to find out about functions eg ":t sqrt". ":t (+)"
-}

primeNumbers = [3,5,7,11]
morePrimes = primeNumbers ++ [13,17,19,23,20]

-- ++ is concatenate

favNums = 2 : 7 : 21 : 66 : []
multList = [[3,5,7],[11,13,17]]
morePrimes2 = 2 : morePrimes
lenPrime = length morePrimes2


revprime = reverse morePrimes2
isListEmpty = null morePrimes2
-- Sort of like take value at index ?
secondPrime = morePrimes2 !! 1
firstPrime = head morePrimes2
lastPrime = last morePrimes2
primeInit = init morePrimes2
first3Primes = take 3 morePrimes2
removedPrimes = drop 3 morePrimes2
is7InList = 7 `elem` morePrimes2
maxPrime =  maximum morePrimes2
minPrime = minimum morePrimes2

numbers = [1,2,3]
numbersSum = sum numbers
numbersProduct = product numbers

zeroToTen = [0..10]
evenList = [2,4..20]
letterList = ['A','C'..'Z']
-- Infinite list
infinPow10 = [10,20..]
many2s = take 10 (repeat 2)
cycleList = take 10 (cycle [1..5])

many3s = replicate 10 3

listTimes2 = [x * 2 | x <- [1..10]]
listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50]
divisBy9N13 = [x | x <- [1..500], mod x 9 == 0, mod x 13 == 0]

sortedList = sort [9,1,8,3,4,7,6]
sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

listBiggerThan5 = filter (>5) morePrimes
evensUpTo20 = takeWhile (<= 20) [2,4..]

-- Like Reduce!
-- fold(l): left to right. fold(r): right to left
multOfList = foldl (*) 1 [1,2,3,4,5]

myList = [1,2,3,4,5]
foldList = foldl (*) (head myList) myList

pow3List = [3^n | n <- [1..10]]

multTable =[[x * y | y <- [1..10]] | x <- [2,4..10]]

-- TUPLES

randTuple = (1,"Random Tuple")
bobSmith = ("Bob Smith", 52)

bobsName = fst bobSmith
bobsAge = snd bobSmith


names = ["Bob", "Mary", "Tom"]
ages = [21,22,23]

namesNAge = zip names ages
