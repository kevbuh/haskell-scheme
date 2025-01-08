module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)

-- Define valid Scheme characters
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Main function to read and evaluate expressions
main :: IO ()
main = do
    args <- getArgs
    case args of
        (expr:_) -> putStrLn $ readExpr expr
        _        -> putStrLn "Usage: cabal build && cabal run haskell-scheme <expression>"

-- Function to read and parse the input expression
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "Error: " ++ show err
    Right val -> "Parsed: " ++ show val

-- Parser to skip one or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- Data type representing Lisp values
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

-- Parser for Lisp strings
parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ String x

-- Parser for Lisp atoms
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

-- Parser for Lisp numbers
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

-- Parser for Lisp expressions
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do
            _ <- char '('
            x <- try parseList <|> parseDottedList
            _ <- char ')'  -- Explicitly discard the result of char ')'
            return x

-- Parser for Lisp lists
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

-- Parser for Lisp dotted lists
parseDottedList :: Parser LispVal
parseDottedList = do
    head' <- endBy parseExpr spaces
    tail' <- char '.' >> spaces >> parseExpr
    return $ DottedList head' tail'

-- Parser for Lisp quoted expressions
parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]