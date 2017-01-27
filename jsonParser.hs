{-#LANGUAGE DeriveFunctor #-}
-- our home-grown JSON parser!

data JSON = N Double | S String | B Bool | Null | L [JSON] | O [(String, JSON)]
    deriving (Show, Eq)

data Result a = Success a String | Error
    deriving (Show, Functor)

newtype ParserM a = ParserM (String -> Result a) deriving Functor

instance Applicative ParserM where
    pure x = ParserM (\s -> Success x s)

    -- (<*>) :: ParserM (a -> b) -> ParserM a -> ParserM b
    ParserM p <*> ParserM q = ParserM $ \s ->
        case p s of
            Success f s' -> case q s' of
                Success x s'' -> Success (f x) s''
                Error -> Error
            Error -> Error

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
null' = do
    literal "null"
    return Null

parseTrue :: ParserM JSON
parseTrue = literal "true" >> (return (B True))

parseFalse :: ParserM JSON
parseFalse = do
    literal "false"
    return $ B False

(<|>) :: ParserM a -> ParserM a -> ParserM a
(<|>) (ParserM f) (ParserM g) = ParserM $ \s -> case f s of
    Error -> g s
    result -> result

bool :: ParserM JSON
bool = parseTrue <|> parseFalse

string :: ParserM JSON
string = do
    char '"'
    body <- until' (char '"') readChar
    char '"'
    return $ S body

readChar :: ParserM Char
readChar = ParserM $ \s -> case s of
    [] -> Error
    (x:xs) -> Success x xs

until' :: ParserM a -> ParserM b -> ParserM [b]
until' end next = do
    x <- peek end
    case x of
        Nothing -> do
            c <- next
            cs <- until' end next
            return (c:cs)
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
        Just x' -> do
            xs <- zeroOrMore p
            return (x':xs)

oneOrMore:: ParserM a -> ParserM [a]
oneOrMore p = do
    x <- p
    xs <- zeroOrMore p
    return (x:xs)

list :: ParserM JSON
list = do
    char '['
    items <- commaSeparated parse
    char ']'
    return $ L items

commaSeparated :: ParserM a -> ParserM [a]
commaSeparated p = do
    zeroOrMore whiteSpace
    items <- optional $ do
        item <- p
        items <- zeroOrMore $ do
            zeroOrMore whiteSpace
            char ','
            zeroOrMore whiteSpace
            p
        zeroOrMore whiteSpace
        return (item:items)
    return $ case items of
        Nothing -> []
        Just xs -> xs

object :: ParserM JSON
object = do
    char '{'
    items <- commaSeparated $ do
        key <- string
        case key of
            S key' -> do
                zeroOrMore whiteSpace
                char ':'
                zeroOrMore whiteSpace
                value <-  parse
                return (key', value)
            _ -> fail "Object keys must be strings"
    char '}'
    return $ O items

oneOf :: [ParserM a] -> ParserM a
oneOf [] = fail "No alternative matched"
oneOf (x:xs) = x <|> (oneOf xs)

whiteSpace :: ParserM Char
whiteSpace = oneOf [char ' ', char '\n', char '\t']

parse :: ParserM JSON
parse = do
    zeroOrMore whiteSpace
    object <- oneOf [object, list, string, bool, null']
    zeroOrMore whiteSpace
    return object
