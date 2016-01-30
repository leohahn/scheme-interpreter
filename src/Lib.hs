module Lib
    (entry)
    where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Char (digitToInt)
import Numeric (readInt, readOct, readHex)
import Data.Maybe (listToMaybe, fromJust)
--import Control.Monad

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Char Char
    | Bool Bool
    deriving(Show)


-- | Entry point of the application, gets arguments and evaluates it with
-- the readExpr function
entry :: IO ()
entry = do args <- getArgs
           putStrLn $ readExpr (head args)


-- | Given a string, reads the expression and evaluates it
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val


-- | Parses the whole expression, returning the evaluated value
parseExpr :: Parser LispVal
parseExpr = parseNumber
        <|> parseString
        <|> parseAtom


-- | Parses a string with escaped characters
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many innerChar
                 char '"'
                 return $ String x
              where innerChar = noneOf ['\"', '\\'] <|> escapeChar
                    escapeChar =
                        do char '\\'
                           c <- oneOf ['n','"','r','t']
                           return $ case c of
                                      '"' -> '\"'
                                      'n' -> '\n'
                                      'r' -> '\r'
                                      't' -> '\t'
                                      _ -> undefined


-- | Parses an atom, returning the atom or a boolean
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _ -> Atom atom


-- | Parses numbers in different bases (decimal, binary, octal and hexa)
parseNumber :: Parser LispVal
parseNumber = parseNumberBase 'd'
          <|> do char '#'
                 base <- oneOf "bdoh"
                 parseNumberBase base


-- | Parses a number at a specific base
parseNumberBase :: Char -> Parser LispVal
parseNumberBase 'b' =
    do digits <- many1 (oneOf "01")
       return $ (Number . fromJust . readBinary) digits
parseNumberBase 'o' =
    do digits <- many1 octDigit
       return $ Number (fst (readOct digits !! 0))
parseNumberBase 'd' =
    do digits <- many1 digit
       return $ (Number . read) digits
parseNumberBase 'h' =
    do digits <- many1 hexDigit
       return $ Number (fst (readHex digits !! 0))
parseNumberBase _ =
    error "Wrong number base"


readBinary :: String -> Maybe Integer
readBinary =
    fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt


-- | Parses a symbol according to RSR5
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"


-- | Removes all spaces until other character is found
-- spaces :: Parser ()
-- spaces = skipMany1 space



