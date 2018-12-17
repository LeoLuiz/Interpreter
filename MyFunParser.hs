module MyFunParser where

import Control.Applicative (some, (<|>))
import Text.ParserCombinators.ReadP(many)
import Parser
import Language

-- A parser for double
double :: Parser Double
double =
  do x <- integer
     y <- decimalPart
     return (fromIntegral x + y)            
  where
    decimalPart =
      do char '.'
         rest <- some digit
         return (read rest / 10 ^ (length rest))
      <|>
      return 0

-- A parser for constant expressions
cte :: Parser Expr
cte = Cte <$> token double

-- A parser for variable expressions
var :: Parser Expr
var = Var <$> token identifier

-- A parser for atomic expressions, that is, the simplest expressions,
-- with the highest precedence
atomic :: Parser Expr
atomic = cte <|>
         var <|>
         token (char '(') *> expr <* token (char ')')


-- A parser for a binary operator
binop :: String -> (a -> a -> a) -> Parser (a -> a -> a)
binop name function = token (string name) *> pure function


-- A parser for pow
potencia :: Parser Expr
potencia = chainl1 atomic (binop "^" (Bin Potencia))


-- A parser for multiplication and division
mul :: Parser Expr
mul = chainl1 potencia (binop "*" (Bin Mul) <|> binop "/" (Bin Div))


-- A parser for addition and subtraction
add :: Parser Expr
add = chainl1 mul (binop "+" (Bin Add) <|> binop "-" (Bin Sub))

negativo :: Parser Expr
negativo = do token (char '-')
              ex <- expr
              return (BinUna Negativo ex)

negacao :: Parser Expr
negacao = do token (char '!')
             ex <- expr
             return (BinUna Negacao ex)


-- A parser for all logic-relational operators
opLog :: Parser Expr
opLog = chainl1 add (binop ">=" (Bin MaiorIgual) <|> binop "<=" (Bin MenorIgual) <|> binop ">" (Bin Maior) 
        <|> binop "<" (Bin Menor) <|> binop "=" (Bin Igual) <|> binop "!=" (Bin Diferente) <|> binop "&&" (Bin And) <|> binop "||" (Bin Or) <|> binop "!" (Bin Negacao))

-- A parser for expressions
expr :: Parser Expr
expr = negativo <|> negacao <|> opLog <|> add <|> potencia <|> mul

-- A parser for assignment command
assign2 :: Parser Cmd
assign2 = Assign <$> token identifier <*
                     token (string ":=") <*>
                     expr

-- ... or alternatively
assign :: Parser Cmd
assign = do id <- token identifier
            token (string ":=")
            e <- expr
            return (Assign id e)

-- A parser for print command
printcmd :: Parser Cmd
printcmd = Print <$> (token (string "print") *> expr)
 
-- A parser for if(<expr>) then <c1> else <c2> command
ifcmd :: Parser Cmd
ifcmd = do token (string "if")
           token (char '(')
           e <- expr
           token (string ") then")
           c1 <- cmd
           token (string "else") 
           c2 <- cmd
           return (If e c1 c2)

-- A parser for while(<expr>) <c1> command
whilecmd :: Parser Cmd
whilecmd = do token (string "while")
              token (char '(')
              e <- expr
              token (char ')')
              command <- cmd
              return (While e command)

-- A parser for read command: read <var> -> gets value of <var> with getLine
readcmd :: Parser Cmd
readcmd = do token (string "read")
             id <- token identifier 
             aux <- token spaces
             e <- token double 
             return (Read id e)

-- A parser for a sequence of commands separated by ';': {<c1>; <c2>; <cn>;}
seqcmd :: Parser Cmd
seqcmd = token (char '{') *> montagem <* token (char ';') <* token (char '}')
    where
      montagem = Sequencia <$> chainl1 ((:[]) <$> cmd) (token (char ';') *> pure (++))


-- A parser for command
cmd :: Parser Cmd
cmd = assign <|> readcmd <|> printcmd  <|> ifcmd <|> seqcmd <|> whilecmd
