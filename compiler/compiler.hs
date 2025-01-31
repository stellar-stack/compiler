{-# LANGUAGE DerivingStrategies #-}
module Main where

main :: IO()
main = putStrLn "Hello world"

-- mov ax, 5
-- hlt

data Lexer a = Lexer { run :: String -> Maybe (a, String)}

char :: Char -> Lexer Char
char x = Lexer $ \s ->
    if x == head s then
        Just (x, tail s)
    else
        Nothing

string :: String -> Lexer String
string x = Lexer $ \s -> let
    len = length x
    str = take len s
    str' = drop len s

 in 
    case str == x of
        True -> Just(str, str')
        _    -> Nothing

data Token = 
    Instruction
    | Comma
    | LiteralInt
 deriving stock Show

mov :: Lexer Token
mov = Lexer $ \s -> let
    look = "mov"
    len = length look
    str = take len s
    str' = drop len s
    ret = if str == look then
        Just (Instruction, str')
    else
        Nothing
 in 
    ret


hlt :: Lexer Token
hlt = Lexer $ \s -> let
    look = "mov"
    len = length look
    str = take len s
    str' = drop len s
    ret = if str == look then
        Just (Instruction, str')
    else
        Nothing
 in 
    ret