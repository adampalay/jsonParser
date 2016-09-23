-- our home-grown JSON parser!

data JSON = N Double | S String | B Bool | Null | L [JSON] | O [(String, JSON)]
    deriving (Show, Eq)

type Parser a = (a, String)

-- O [("key", I 1)]

-- {"key": "value", "key2": 1}

-- starting from first character, it can start with {, [, ", 0-9, -

topParse :: String -> JSON
topParse s = if eatWhitespace rest == "" then json else error "didn't work"
    where (json, rest) = parse s

parse :: String -> Parser JSON
parse s@('{':_) = parseObject s
parse s@('[':_) = parseList s
parse s@('-':_) = parseNumber s
parse s@('"':_) = parseString s
parse s@('t':_) = parseBool s
parse s@('f':_) = parseBool s
parse s@('n':_) = parseNull s
parse (' ':xs) = parse xs
parse ('\n':xs) = parse xs
parse ('\t':xs) = parse xs
parse s = parseNumber s

parseNull :: String -> Parser JSON
parseNull ('n':'u':'l':'l':xs) = (Null, xs)

parseBool :: String -> Parser JSON
parseBool ('t':'r':'u':'e':xs) = (B True, xs)
parseBool ('f':'a':'l':'s':'e':xs) = (B False, xs)

parseString :: String -> Parser JSON
parseString ('"':xs) = (S body, xs')
    where (body, ('"':xs')) = parseBody xs

parseBody :: String -> Parser String
parseBody s@('"':xs) = ("", s)
parseBody ('\\':'n':xs) = ('\n':body, xs')
    where (body, xs') = parseBody xs
parseBody ('\\':'t':xs) = ('\t':body, xs')
    where (body, xs') = parseBody xs
parseBody ('\\':'\\':xs) = ('\\':body, xs)
    where (body, xs') = parseBody xs
parseBody ('\\':'"':xs) = ('"':body, xs)
    where (body, xs') = parseBody xs
parseBody ('\\':c:xs) = (c:body, xs)
    where (body, xs') = parseBody xs
parseBody (x:xs) = (x:body, xs')
    where (body, xs') = parseBody xs

parseList :: String -> Parser JSON
parseList ('[':xs) = (L items, xs')
    where (items, ']':xs') = parseListItems xs

parseListItems :: String -> Parser [JSON]
parseListItems (' ':xs) = parseListItems xs -- still looking for an item
parseListItems ('\n':xs) = parseListItems xs -- still looking for an item
parseListItems s = (item:items, xs')
    where
       (item, xs) = parse s
       (items, xs') = parseListNext xs

parseListNext :: String -> Parser [JSON]
parseListNext s@(']':xs) = ([], s)  -- no next item, the list is done
parseListNext (',':xs) = parseListItems xs -- parse the next item
parseListNext (' ':xs) = parseListNext xs -- still looking for a separator
parseListNext ('\n':xs) = parseListNext xs -- still looking for a separator

parseObject :: String -> Parser JSON
parseObject ('{':xs) = (O pairs, xs')
    where (pairs, '}':xs') = parseObjectPairs xs

parseObjectPairs :: String -> Parser [(String, JSON)]
parseObjectPairs (' ':xs) = parseObjectPairs xs
parseObjectPairs ('\n':xs) = parseObjectPairs xs
parseObjectPairs ('\t':xs) = parseObjectPairs xs
parseObjectPairs s = (pair:pairs, xs')
    where
        (pair, xs) = parsePair s
        (pairs, xs') = parsePairNext xs

parsePair :: String -> Parser (String, JSON)
parsePair s = ((key, value), xsFinal)
    where
        (S key, xs) = parseString s
        xs' = (eatWhitespace . (eat ':') . eatWhitespace) xs
        (value, xsFinal) = parse xs'

parsePairNext :: String -> Parser [(String, JSON)]
parsePairNext s@('}':xs) = ([], s)
parsePairNext (' ':xs) = parsePairNext xs
parsePairNext ('\n':xs) = parsePairNext xs
parsePairNext ('\t':xs) = parsePairNext xs
parsePairNext s = (pairs, xs)
    where
        s' = (eatWhitespace . (eat ',') . eatWhitespace) s
        (pairs, xs) = parseObjectPairs s'

eat :: Char -> String -> String
eat c (x:xs) | x == c = xs

eatWhitespace :: String -> String
eatWhitespace (' ':xs) = eatWhitespace xs
eatWhitespace ('\t':xs) = eatWhitespace xs
eatWhitespace ('\n':xs) = eatWhitespace xs
eatWhitespace s = s


parseNumber = undefined
