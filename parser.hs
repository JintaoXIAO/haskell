module Parsers where

import Prelude hiding ((>>=), (>>), return)
import Data.Char

type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
                    [] -> []
                    [(v, out)] -> parse (f v) out

(>>) :: Parser a -> Parser b -> Parser b
f >> p =
  f >>= \_ ->
  p




(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                    [] -> parse q inp
                    [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
                   if p x then
                     return x
                   else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) =
  char x >>
  string xs >>
  return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 p =
  p >>= \v ->
  many p >>= \vs ->
  return (v:vs)

ident :: Parser String
ident =
  lower >>= \v ->
  many alphanum >>= \vs ->
  return (v:vs)

nat :: Parser Int
nat =
  many1 digit >>= \vs ->
  return (read vs)

space :: Parser ()
space =
  many (sat isSpace) >>
  return ()

token :: Parser a -> Parser a
token p =
  space >>
  p >>= \v ->
  space >>
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

int :: Parser Int
int =
  (sat (== '-') +++ return ' ') >>= \sign ->
  many (sat isDigit) >>= \xs ->
  return (read (sign:xs))

comment :: Parser ()
comment =
  many (sat (== '-')) >>
  many alphanum >>
  sat (== '\n') >>
  return ()

expr :: Parser Int
expr =
  term >>= \t ->
              (symbol "+" >>
               expr >>= \e ->
               return (t + e))
  +++ return t

term :: Parser Int
term =
  factor >>= \f ->
        (symbol "*" >>
        term >>= \t ->
        return (f * t)) +++
  return f

factor :: Parser Int
factor =
  (symbol "(" >>
   expr >>= \e ->
   symbol ")" >>
   return e) +++
  natural

eval :: String -> Int
eval xs = case parse expr xs of
  [(n, [])] -> n
  [(_, out)] -> error("unused input" ++ out)
  [] -> error "invalid input"
