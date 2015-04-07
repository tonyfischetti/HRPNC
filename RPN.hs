
import Control.Monad
import Data.Char
import System.IO


-- passes the appropriate arithmetic function to operate
perform :: String -> [Double] -> [Double]
perform "+" = operate (+)
perform "-" = operate (-)
perform "*" = operate (*)
perform "/" = operate (/)

-- takes a fx and stack and returns stack after fx application
operate :: (Double -> Double -> Double) -> [Double] -> [Double]
operate f stack
    | length stack < 2  = error "wrong number of operators"
    | otherwise         = f sd fs : rest
                          where (fs:sd:rest) = stack

-- tests whether current token is a recognized operator
isOperator :: String -> Bool
isOperator string = elem (head string) "+-*/"

-- tests whether current token is a recognized number
isANumber :: String -> Bool
isANumber string
    | (snd . head) parsed == ""  = True
    | otherwise                  = False
    where parsed = reads string :: [(Double, String)]

-- reducer task for the fold operation
reducer :: [Double] -> String -> [Double]
reducer stack currentToken
    | isOperator currentToken  = perform currentToken stack
    | isANumber currentToken   = read currentToken : stack
    | otherwise                = error "Unrecognized input"

-- makes sure everything is kosher
receiveFinalStack :: [Double] -> Double
receiveFinalStack (x:xs:[]) = error "wrong number of operators"
receiveFinalStack (x:xs)    = x

dispatcher :: String -> Double
dispatcher input = receiveFinalStack $ foldl reducer [] $ words input

main :: IO ()
main = forever $ do
        putStr "RPN: "
        hFlush stdout
        input <- getLine
        print $ dispatcher input
