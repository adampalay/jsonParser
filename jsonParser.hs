-- our home-grown JSON parser!

data JSON = N Double | S String | B Bool | Null | L [JSON] | O [(String, JSON)]
    deriving (Show, Eq)

data Parser a = (a, String)

-- O [("key", I 1)]

-- {"key": "value", "key2": 1}

-- starting from first character, it can start with {, [, ", 0-9, -

parse :: String -> JSON
parse s@('{':_) = parseObject s
parse s@('[':_) = parseList s
parse s@('-':_) = parseNumber s
parse s@('"':_) = parseString s
parse s@('t':_) = parseBool s
parse s@('f':_) = parseBool s
parse s@('n':_) = parseNull s
parse s = parseNumber s

parseNull :: String -> Parser JSON
parseNull 'n':'u':'l':'l':xs = (Null, xs)

parseBool :: String -> Parser JSON
parseBool 't':'r':'u':'e':xs = (B True, xs)
parseBool 'f':'a':'l':'s':'e':xs = (B False, xs)

parseString :: String -> Parser JSON
parseString ('"':xs) =
    (S body, xs')
    where
        (body, ('"':xs')) = parseBody xs

parseBody :: String -> Parser String
parseBody ('"':xs) = ("", ('"':xs))
parseBody ('\\':'n':xs) = '\n': (parseBody xs)
parseBody ('\\':'t':xs) = '\t': (parseBody xs)
parseBody ('\\':'\\':xs) = '\\': (parseBody xs)
parseBody ('\\':'"':xs) = '"': (parseBody xs)
parseBody ('"':xs) = error "unescaped quote"
parseBody (x:xs) = x: parseBody xs

parseList :: String -> JSON
parseList ('\[':xs) =

parseObject = undefined
parseList = undefined
parseNumber = undefined
