import Control.Applicative
import Data.Char
import qualified Data.Map.Strict as M

-- data JsonValue = JsonNull | JsonBool Bool | JsonNum Integer | JsonString String | JsonArray [JsonValue] | JsonObject (M.Map String JsonValue) deriving (Show, Eq)
data JsonValue = JsonNull | JsonBool Bool | JsonNum Integer | JsonString String | JsonArray [JsonValue] | JsonObject [(String, JsonValue)] deriving (Show, Eq)

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

charParser :: Char -> Parser Char
charParser c = Parser f
  where
    f (x : xs)
      | c == x = Just (c, xs)
      | otherwise = Nothing
    f [] = Nothing

instance Functor Parser where
  fmap f (Parser p) = Parser $ \inp -> do
    (x, input') <- p inp
    Just (f x, input')

instance Applicative Parser where
  pure x = Parser $ \inp -> Just (x, inp)

  -- take the input, put it through the first parser, then the second
  (Parser p1) <*> (Parser p2) = Parser $ \inp -> do
    -- currying
    (f, inp') <- p1 inp
    (a, inp'') <- p2 inp'
    Just (f a, inp'')

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \inp ->
    p1 inp <|> p2 inp

stringParser :: String -> Parser String
-- stringParser = sequenceA . (map charParser)
stringParser = traverse charParser

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringParser "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringParser "true" <|> stringParser "false")
  where
    f inp
      | inp == "true" = JsonBool True
      | inp == "false" = JsonBool False

-- where
--   f "true" = JsonBool True
--   f "false" = JsonBool False
--   f _       = undefined

-- JSON NUM

notNull :: Foldable t => Parser (t a) -> Parser (t a)
notNull (Parser p) = Parser $ \inp -> do
  (json, inp') <- p inp
  if null json
    then Nothing
    else Just (json, inp')

-- * Main Data.Char> span isDigit "12345hello"

-- ("12345","hello")
-- prefix === span, iterate string until predicate is false
prefixParser :: (Char -> Bool) -> Parser String
prefixParser predicate =
  Parser $ \inp -> Just (span predicate inp)

-- Parser $ \inp ->
--   let (prefix, inp') = span predicate inp
--    in Just (prefix, inp')

-- When you see let A in B you can think of B as the value of the statement. In an imperative language, you might write it as A; return B.
-- https://stackoverflow.com/questions/24234003/using-let-in-with-function
-- https://stackoverflow.com/questions/4362328/haskell-where-vs-let
digitParser :: Parser String
digitParser = prefixParser isDigit

jsonNum :: Parser JsonValue
jsonNum = f <$> notNull digitParser
  where
    f x = JsonNum (read x)

-- JSON STRING

literalParser :: Parser String -- parser of type string
literalParser = charParser '"' *> prefixParser (/= '"') <* charParser '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> literalParser

-- JSON ARRAY

whitespaceParser :: Parser String
whitespaceParser = prefixParser isSpace

-- split :: Parser Char -> Parser [JsonValue] -- this thing repeats until it fails
split :: Parser a1 -> Parser a2 -> Parser [a2]
split sepParser json = (:) <$> json <*> many (sepParser *> json) <|> pure []

pluralParser :: Parser a -> Parser [a] -- plural of type, seperated by commas
pluralParser json = split (whitespaceParser *> charParser ',' <* whitespaceParser) json

jsonArray :: Parser JsonValue
jsonArray =
  JsonArray
    <$> ( charParser '[' *> whitespaceParser
            *> pluralParser jsonValue -- plural of jsonValues
            <* whitespaceParser
            <* charParser ']'
        )

-- JSON OBJECT

pairParser :: Parser (String, JsonValue)
pairParser =
  (\key _ value -> (key, value)) <$> literalParser <*> (whitespaceParser *> charParser ':' <* whitespaceParser) <*> jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charParser '{' *> whitespaceParser *> pluralParser pairParser <* whitespaceParser <* charParser '}')

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNum <|> jsonString <|> jsonArray <|> jsonObject

-- parseFile "parsec/ditto.json"
parseFile filename = do
  inp <- readFile filename
  return (fst <$> parse jsonValue inp)

-- parse (the thing you want to parse) (input)
main :: IO ()
main = undefined