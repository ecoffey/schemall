module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal =
  Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseStringEscapes :: Parser Char
parseStringEscapes = do
  first <- (noneOf "\\") <|> (char '\\')
  case first of
    '\\' -> anyChar
    _    -> return first

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (parseStringEscapes)
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber = (many1 digit) >>= \p -> return (Number (read p))
--parseNumber = do
--  digits <- many1 digit
-- return $ Number (read digits)

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found val"

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

