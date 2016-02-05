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

instance Show LispVal where
    show = showVal


showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Char contents) = "#\\" ++ [contents]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList first second) =
    "(" ++ unwordsList first ++ " . " ++ showVal second ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


-- | Entry point of the application, gets arguments and evaluates it with
-- the readExpr function
entry :: IO ()
entry =
    getArgs >>= putStrLn . show . eval . readExpr . head


-- | Given a string, reads the expression and evaluates it
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No match: " ++ show err
                   Right val -> val


-- | Parses the whole expression, returning the value
parseExpr :: Parser LispVal
parseExpr = parseHash
        <|> parseNumberBase 'd'
        <|> parseString
        <|> parseAtom
        <|> parseQuoted
        <|> parseAnyList


-- | Parses both normal lists or dotted lists
parseAnyList :: Parser LispVal
parseAnyList =
    do char '('
       firstPart <- sepEndBy parseExpr spaces1
       spaces
       sep <- choice [char '.',char ')']
       case sep of
         '.' ->
             do spaces1
                secondPart <- parseExpr
                char ')'
                return (DottedList firstPart secondPart)
         ')' ->
             return (List firstPart)
         _   ->
             undefined


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


-- | Parses a number at a specific base (Need refactoring, it is ugly)
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


-- | Defines how a symbol should begin
initial :: Parser Char
initial = letter <|> specialInitial


specialInitial :: Parser Char
specialInitial =
    oneOf ['!','$','%','&','*','/',':','<','=','>','?','^','_','~']


-- | Defines which characters are allowed in the middle of a identifier
subsequent :: Parser Char
subsequent = initial <|> digit <|> specialSubsequent


specialSubsequent :: Parser Char
specialSubsequent = oneOf ['+','-','.','@']


peculiarIdentifier :: Parser Char
peculiarIdentifier = oneOf ['+','-']


-- | Removes all spaces until other character is found
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- | Removes all spaces if any exists, otherwise do nothing
spaces :: Parser ()
spaces = skipMany space

----------------------------------------------------------
-- Evaluation
----------------------------------------------------------

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives


primitives :: [(String,[LispVal] -> LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop (*))
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("string?", isString)
             , ("float?", isFloat)
             ]


isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _ = Bool False


isFloat :: [LispVal] -> LispVal
isFloat [Float _] = Bool True
isFloat _ = Bool False


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op args = Number $ foldl1 op (map unpackNum args)


unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) =
    if null parsed
        then 0
        else fst $ (head parsed)
    where
      parsed = reads s
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
