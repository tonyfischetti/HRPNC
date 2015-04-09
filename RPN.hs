
import Control.Monad
import Control.Applicative
import Data.Char
import System.IO



---- Testing functions --------------------------------
-- tests whether current token is a recognized operator
isOperator :: String -> Bool
isOperator string = elem (head string) "+-*/"

-- tests whether current token is a recognized number
isANumber :: String -> Bool
isANumber string
    | null parsed                = False
    | (snd . head) parsed == ""  = True
    | otherwise                  = False
    where parsed = reads string :: [(Double, String)]
-------------------------------------------------------


-- passes the appropriate arithmetic function to operate
perform :: String -> [Double] -> [Double]
perform "+" = operate (+)
perform "-" = operate (-)
perform "*" = operate (*)
perform "/" = operate (/)

-- takes a fx and stack and returns stack after fx application
operate :: (Double -> Double -> Double) -> [Double] -> [Double]
operate f stack = f sd fs : rest where (fs:sd:rest) = stack


-- reducer task for the fold operation
reducer :: [Double] -> String -> Either String [Double]
reducer stack currentToken
    | isANumber currentToken   = Right $ read currentToken : stack
    | isOperator currentToken  = if (length stack < 2)
                                     then Left "Too many operators\n"
                                     else Right $ perform currentToken stack
    | otherwise                = Left "Unrecognized input\n"


-- if we got an error string, return the error string
-- if we got back a stack with more than one value, there
-- were too few operators
receiveFinalStack :: Either String [Double] -> Either String Double
receiveFinalStack (Left s)          = Left s
receiveFinalStack (Right (x:xs:[])) = Left "Too few operators\n"
receiveFinalStack (Right (x:xs))    = Right x
receiveFinalStack _                 = Left ""

-- breaks input string into list of tokens, folds
-- them with the reducer function and processes result
dispatcher :: String -> Either String Double
dispatcher = receiveFinalStack . (foldM reducer []) . words

displayResult :: Either String Double -> IO ()
displayResult v = case v of
                      Left s  -> putStr s
                      Right d -> print d

main :: IO ()
main = forever $ do
        putStr "RPN: "
        hFlush stdout
        getLine >>= (displayResult . dispatcher)
