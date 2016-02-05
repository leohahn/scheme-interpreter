module Lib
    ( entry
    ) where

import           Control.Monad
import           Data.Char                     (digitToInt)
import           Data.Maybe                    (fromJust, listToMaybe)
import           Numeric                       (readFloat, readHex, readInt,
                                                readOct)
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)


data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | Float Float
    | String String
    | Char Char
    | Bool Bool
    deriving (Show)


-- | Entry point of the application, gets arguments and evaluates it with
-- the readExpr function
entry :: IO ()
entry =
    do args <- getArgs
       putStrLn $ readExpr (head args)


-- | Given a string, reads the expression and evaluates it
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val


-- | Parses the whole expression, returning the evaluated value
parseExpr :: Parser LispVal
parseExpr = parseHash
        <|> parseNumberBase 'd'
        <|> parseString
        <|> parseAtom
        <|> parseQuoted
        <|> parseAnyList


parseAnyList :: Parser LispVal
parseAnyList =
    do char '('
       firstPart <- sepEndBy parseExpr spaces
       spacesOpt
       sep <- choice [char '.',char ')']
       case sep of
         '.' ->
             do spaces
                secondPart <- parseExpr
                char ')'
                return (DottedList firstPart secondPart)
         ')' ->
             return (List firstPart)
         _   ->
             undefined

spacesOpt :: Parser ()
spacesOpt = skipMany space

-- | Parses a list with expressions separated by spaces
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces


-- | Parses a list in its dotted form
parseDottedList :: Parser LispVal
parseDottedList =
    do x <- endBy parseExpr spaces
       xs <- char '.' >> spaces >> parseExpr
       return $ DottedList x xs


-- | Parses a LispVal that is quoted
parseQuoted :: Parser LispVal
parseQuoted =
    do char '\''
       x <- parseExpr
       return $ List [Atom "quote",x]


-- Parses all LispVal's that begin with a hash character
parseHash :: Parser LispVal
parseHash =
    do char '#'
       choice [ oneOf ['b','d','h','o'] >>= parseNumberBase
              , parseBoolean
              , parseChar
              ]


-- | Parses a string with escaped characters
parseString :: Parser LispVal
parseString =
    do char '"'
       x <- many innerChar
       char '"'
       return $ String x
    where
      innerChar = noneOf ['\"', '\\'] <|> escapeChar
      escapeChar =
          do char '\\'
             c <- oneOf ['n','"','r','t']
             return $ case c of
                        '"' -> '\"'
                        'n' -> '\n'
                        'r' -> '\r'
                        't' -> '\t'
                        _   -> undefined


-- | Parses a boolean, given that '#' was already consumed
parseBoolean :: Parser LispVal
parseBoolean =
    do val <- oneOf ['t','f']
       case val of
         't' -> return $ Bool True
         'f' -> return $ Bool False
         _   -> undefined


parseAtom :: Parser LispVal
parseAtom =
    do first <- choice [initial,peculiarIdentifier]
       second <- many subsequent
       case (length second,first) of
         (0, '+') -> return $ Atom (first : "")
         (0, '-') -> return $ Atom (first : "")
         (_, '+') -> error "Identifier begins with +"
         (_, '-') -> error "Identifier begins with -"
         _        -> return $ Atom (first : second)


-- | Parses a character
parseChar :: Parser LispVal
parseChar =
    do char '\\'
       c <- anyChar
       return $ Char c


-- | Parses a number at a specific base
parseNumberBase :: Char -> Parser LispVal
parseNumberBase 'b' =
    do digits <- many1 (oneOf "01")
       return $ (Number . fromJust . readBinary) digits
parseNumberBase 'o' =
    do digits <- many1 octDigit
       return $ Number (fst (readOct digits !! 0))
parseNumberBase 'd' =
    do first <- digit
       restDigits <-  many (digit <|> char '.')
       let digits = first : restDigits
       let numPoints = (length . filter (== '.')) digits
       case numPoints of
         0 -> return $ (Number . read) digits

         1 -> do let float = fst $ (readFloat digits :: [(Float,String)]) !! 0
                 return $ Float float

         _ -> fail "More than one point in a float number"
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
symbol = oneOf "!$%&*+-./:<=?>@^_~"


initial :: Parser Char
initial = letter <|> specialInitial


specialInitial :: Parser Char
specialInitial =
    oneOf ['!','$','%','&','*','/',':','<','=','>','?','^','_','~']


subsequent :: Parser Char
subsequent = initial <|> digit <|> specialSubsequent


specialSubsequent :: Parser Char
specialSubsequent = oneOf ['+','-','.','@']


peculiarIdentifier :: Parser Char
peculiarIdentifier = oneOf ['+','-']


-- | Removes all spaces until other character is found
spaces :: Parser ()
spaces = skipMany1 space
