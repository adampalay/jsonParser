{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE DeriveGeneric #-}

-- our home-grown JSON parser!

module JsonParser where

import Control.Applicative ((<$), Alternative(..))
import Data.Maybe (fromMaybe)
import GHC.Generics


data JSON = N Double | S String | B Bool | Null | L [JSON] | O [(String, JSON)]
    deriving (Show, Eq)

data Result a = Success a String | Error
    deriving (Show, Functor, Eq, Generic)

instance Applicative Result where
    pure = flip Success ""
    Error       <*> _           = Error
    Success f s <*> Error       = Error
    -- This feels odd (why do we append the remaining strings together?)
    -- but is the only version I can figure out that actually obeys the laws.
    Success f s <*> Success x s' = Success (f x) (s `mappend` s')

instance Alternative Result where
    empty = Error
    Error <|> r = r
    l     <|> _ = l

newtype ParserM a = ParserM (String -> Result a) deriving (Functor, Generic)

instance Applicative ParserM where
    pure = ParserM <$> Success

    -- (<*>) :: ParserM (a -> b) -> ParserM a -> ParserM b
    ParserM p <*> ParserM q = ParserM $ \s ->
        case p s of
            Success f s' -> case q s' of
                Success x s'' -> Success (f x) s''
                Error -> Error
            Error -> Error

instance Alternative ParserM where
    empty = ParserM $ const Error
    ParserM p <|> ParserM q = ParserM $ \s -> p s <|> q s

instance Monad ParserM where
    -- (>>=) :: ParserM a -> (a -> ParserM b) -> ParserM b
    (ParserM p) >>= f = ParserM $ \s ->
        case p s of
            Success x s' -> q s' where ParserM q = f x
            Error -> Error
    -- return :: a -> ParserM a
    -- return should default to pure

    -- fail :: String -> ParserM a
    fail _ = ParserM (\_ -> Error)

char :: Char -> ParserM Char
char c = do
    c' <- readChar
    if c == c' then return c' else fail $ "expected " ++ [c]

literal :: String -> ParserM String
literal s = mapM char s

runParser :: ParserM a -> String -> Result a
runParser (ParserM f) s = f s

null' :: ParserM JSON
null' = Null <$ literal "null"

parseTrue :: ParserM JSON
parseTrue = B True <$ literal "true"

parseFalse :: ParserM JSON
parseFalse = B False <$ literal "false"

bool :: ParserM JSON
bool = parseTrue <|> parseFalse

string :: ParserM JSON
string = S <$> quotedString

quotedString :: ParserM String
quotedString = char '"' *> (until' (char '"') readChar) <* char '"'

readChar :: ParserM Char
readChar = ParserM $ \s -> case s of
    [] -> Error
    (x:xs) -> Success x xs

until' :: ParserM a -> ParserM b -> ParserM [b]
until' end next = do
    x <- peek end
    case x of
        Nothing -> (:) <$> next <*> until' end next
        Just _ -> return []

peek :: ParserM a -> ParserM (Maybe a)
peek p = ParserM $ \s -> case runParser p s of
    Success x _ -> Success (Just x) s
    Error -> Success Nothing s

optional :: ParserM a -> ParserM (Maybe a)
optional p = (Just <$> p) <|> (pure Nothing)

zeroOrMore:: ParserM a -> ParserM [a]
zeroOrMore p = do
    x <- optional p
    case x of
        Nothing -> return []
        Just x' -> (x':) <$> zeroOrMore p

oneOrMore:: ParserM a -> ParserM [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

list :: ParserM JSON
list = char '[' *> (L <$> commaSeparated parse) <* char ']'

commaSeparated :: ParserM a -> ParserM [a]
commaSeparated p = fromMaybe [] <$> (sp *> (optional $ (:) <$> p <*> zeroOrMore item) <* sp)
    where
        sp = zeroOrMore whiteSpace
        item = sp *> char ',' *> sp *> p

object :: ParserM JSON
object = char '{' *> (O <$> commaSeparated item) <* char '}'
    where
        sp = zeroOrMore whiteSpace
        item = (,) <$> quotedString <*> (sp *> char ':' *> sp *> parse)

oneOf :: [ParserM a] -> ParserM a
oneOf [] = fail "No alternative matched"
oneOf (x:xs) = x <|> (oneOf xs)

whiteSpace :: ParserM Char
whiteSpace = oneOf [char ' ', char '\n', char '\t']

parse :: ParserM JSON
parse = sp *> oneOf [object, list, string, bool, null'] <* sp
    where sp = zeroOrMore whiteSpace
