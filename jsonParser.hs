{-#LANGUAGE DeriveFunctor #-}
-- our home-grown JSON parser!

data JSON = N Double | S String | B Bool | Null | L [JSON] | O [(String, JSON)]
    deriving (Show, Eq)

type Parser a = Result a

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
char c = ParserM $ \s -> case s of
    [] -> Error
    (x:xs) -> if x == c then Success x xs else Error

string :: String -> ParserM String
string s = mapM char s


runParser :: ParserM a -> String -> Result a
runParser (ParserM f) s = f s




-- O [("key", I 1)]

-- {"key": "value", "key2": 1}

-- starting from first character, it can start with {, [, ", 0-9, -


topParse :: String -> Parser JSON
topParse s = if eatWhitespace rest == "" then Success json "" else Error
    where Success json rest = parse s

parse :: String -> Parser JSON
parse s@('{':_) = parseObject s
parse s@('[':_) = parseList s
parse s@('-':_) = parseNumber s
parse s@('"':_) = parseString s
parse s@('t':_) = parseBool s
parse s@('f':_) = parseBool s
parse s@('n':_) = runParser parseNull s
parse (' ':xs) = parse xs
parse ('\n':xs) = parse xs
parse ('\t':xs) = parse xs
-- parse s = parseNumber s
parse _ = Error

parseNull :: ParserM JSON
parseNull = do
    string "null"
    return Null

parseBool :: String -> Parser JSON
parseBool ('t':'r':'u':'e':xs) = Success (B True) xs
parseBool ('f':'a':'l':'s':'e':xs) = Success (B False) xs
parseBool _ = Error

parseString :: String -> Parser JSON
parseString ('"':xs) = Success (S body) xs'
    where Success body ('"':xs') = parseBody xs
parseString _ = Error


parseBody :: String -> Parser String
parseBody s@('"':xs) = Success "" s
parseBody ('\\':'n':xs) = Success ('\n':body) xs'
    where Success body xs' = parseBody xs
parseBody ('\\':'t':xs) = Success ('\t':body) xs'
    where Success body xs' = parseBody xs
parseBody ('\\':'\\':xs) = Success ('\\':body) xs
    where Success body xs' = parseBody xs
parseBody ('\\':'"':xs) = Success ('"':body) xs
    where Success body xs' = parseBody xs
parseBody ('\\':c:xs) = Success (c:body) xs
    where Success body xs' = parseBody xs
parseBody (x:xs) = Success (x:body) xs'
    where Success body xs' = parseBody xs

parseList :: String -> Parser JSON
parseList ('[':xs) = Success (L items) xs'
    where Success items (']':xs') = parseListItems xs

parseListItems :: String -> Parser [JSON]
parseListItems (' ':xs) = parseListItems xs -- still looking for an item
parseListItems ('\n':xs) = parseListItems xs -- still looking for an item
parseListItems s = Success (item:items) xs'
    where
       Success item xs = parse s
       Success items xs' = parseListNext xs

parseListNext :: String -> Parser [JSON]
parseListNext s@(']':xs) = Success [] s  -- no next item, the list is done
parseListNext (',':xs) = parseListItems xs -- parse the next item
parseListNext (' ':xs) = parseListNext xs -- still looking for a separator
parseListNext ('\n':xs) = parseListNext xs -- still looking for a separator

parseObject :: String -> Parser JSON
parseObject ('{':xs) = Success (O pairs) xs'
    where Success pairs ('}':xs') = parseObjectPairs xs

parseObjectPairs :: String -> Parser [(String, JSON)]
parseObjectPairs (' ':xs) = parseObjectPairs xs
parseObjectPairs ('\n':xs) = parseObjectPairs xs
parseObjectPairs ('\t':xs) = parseObjectPairs xs
parseObjectPairs s = Success (pair:pairs) xs'
    where
        Success pair xs = parsePair s
        Success pairs xs' = parsePairNext xs

parsePair :: String -> Parser (String, JSON)
parsePair s = Success (key, value) xsFinal
    where
        Success (S key) xs = parseString s
        xs' = (eatWhitespace . (eat ':') . eatWhitespace) xs
        Success value xsFinal = parse xs'

parsePairNext :: String -> Parser [(String, JSON)]
parsePairNext s@('}':xs) = Success [] s
parsePairNext (' ':xs) = parsePairNext xs
parsePairNext ('\n':xs) = parsePairNext xs
parsePairNext ('\t':xs) = parsePairNext xs
parsePairNext s = Success pairs xs
    where
        s' = (eatWhitespace . (eat ',') . eatWhitespace) s
        Success pairs xs = parseObjectPairs s'

eat :: Char -> String -> String
eat c (x:xs) | x == c = xs

eatWhitespace :: String -> String
eatWhitespace (' ':xs) = eatWhitespace xs
eatWhitespace ('\t':xs) = eatWhitespace xs
eatWhitespace ('\n':xs) = eatWhitespace xs
eatWhitespace s = s


parseNumber = undefined
