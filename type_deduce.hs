import qualified Data.Char

main :: IO ()
main = do
    decl <- getLine
    print (tokenize [decl])

tokenize :: [[Char]] -- [tokens..., untokenized]
  -> [[Char]] -- [tokens..., untokenized]
tokenize tokens_rest
    | not (null (last tokens_rest)) = tokenize (getToken tokens_rest)
    | otherwise = reverse (init tokens_rest)

reservedTokens :: [Char]
reservedTokens = ['*', '(', ')', '[', ']', ',']

getToken :: [[Char]] -- [tokens..., untokenized]
  -> [[Char]] -- [tokens..., untokenized]
getToken tokens_rest
    | null (last tokens_rest)
    = tokens_rest
    | Data.Char.isSpace (head (last tokens_rest))
    = getToken (init tokens_rest ++ [tail (last tokens_rest)])
    | head (last tokens_rest) `elem` reservedTokens
    = [head (last tokens_rest)] : (init tokens_rest ++ [tail (last tokens_rest)])
    | Data.Char.isDigit (head (last tokens_rest))
    = getNumOrId Data.Char.isDigit ([head (last tokens_rest)] : (init tokens_rest ++ [tail (last tokens_rest)]))
    | Data.Char.isAlpha (head (last tokens_rest))
    = getNumOrId Data.Char.isAlphaNum ([head (last tokens_rest)] : (init tokens_rest ++ [tail (last tokens_rest)]))
    | otherwise
    = tokens_rest

getNumOrId f numRest
    | null (last numRest)
    = reverse (head numRest) : tail numRest
    | f (head (last numRest))
    = getNumOrId f (((head (last numRest) : head numRest) : tail (init numRest)) ++ [tail (last numRest)])
    | otherwise
    = reverse (head numRest) : tail numRest