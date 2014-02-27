module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readOct, readDec, readHex, readInt)
import Data.Char (digitToInt)

data LispVal =
  Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Character Char
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

parseCharacter :: Parser LispVal
parseCharacter = do
  string "#\\"
  char <- string "space" <|> string "newline" <|> many1 anyChar
  trailing <- optionMaybe (oneOf "( ")
  case char of
    "space" -> return $ Character ' '
    "newline" -> return $ Character '\n'
    _ -> case trailing of
      Just _ -> return $ Character (char !! 0)


parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

isBinDigit :: Char -> Bool
isBinDigit c = (c == '0' || c == '1')

readBin :: (Integral a) => ReadS a
readBin = readInt 2 isBinDigit digitToInt

binDigit :: Parser Char
binDigit = oneOf "01"

parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
--parseNumber = (many1 digit) >>= \p -> return (Number (read p))
parseNumber = do
  prefix <- (string "#b") <|> (string "#o") <|> (string "#d") <|> (string "#x") <|> (many1 digit)
  case prefix of
    "#b" -> liftM (Number . fst . head . readBin) $ many1 binDigit
    "#o" -> liftM (Number . fst . head . readOct) $ many1 octDigit
    "#x" -> liftM (Number . fst . head . readHex) $ many1 hexDigit
    "#d" -> liftM (Number . read) $ many1 digit
    _    -> return (Number (read prefix))

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found val"

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

