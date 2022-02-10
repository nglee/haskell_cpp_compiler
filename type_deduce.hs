import qualified Data.Char

main :: IO ()
main = do
    decl <- getLine
    print (tokenize [decl])

reservedTokens :: [Char]
reservedTokens = ['*', '(', ')', '[', ']', ',']

tokenize :: [[Char]] -> [[Char]]
tokenize [""] = [""]
tokenize ("" : tokens) = tokens
tokenize (decl : tokens)
    | Data.Char.isSpace (head decl)
    = tokenize (tail decl : tokens)
    | head decl `elem` reservedTokens
    = tokenize (tail decl : (tokens ++ [[head decl]]))
    | Data.Char.isDigit (head decl)
    = tokenize (getNumOrId Data.Char.isDigit (tail decl : (tokens ++ [[head decl]])))
    | Data.Char.isAlpha (head decl)
    = tokenize (getNumOrId Data.Char.isAlphaNum (tail decl : (tokens ++ [[head decl]])))
    | otherwise
    = decl : tokens
tokenize _ = [""]

getNumOrId :: (Char -> Bool) -> [[Char]] -> [[Char]]
getNumOrId f [""] = [""]
getNumOrId f ("" : tokens) = "" : tokens
getNumOrId f (decl : tokens)
    | f (head decl)
    = getNumOrId f (tail decl : (init tokens ++ [last tokens ++ [head decl]]))
    | otherwise
    = decl : tokens
getNumOrId f _ = [""]