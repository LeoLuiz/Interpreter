module Language where

-- Definição de tipos Expr
data Expr = Cte Double
          | Var String
          | Bin Op Expr Expr
          | Bin Op Expr
          deriving (Show)

data Op = Add | Sub      | Mul       | Div   | Potencia
              | Igual    | Diferente | Maior | MaiorIgual | Menor | MenorIgual
              | And      | Or        
              | Negativo | Negacao
              deriving (Show)

-- Definição de tipos Command
data Cmd = Assign String Expr                       
         | Print Expr 
         | Read String Double                                
         | Sequencia [Cmd]
         | If Expr Cmd Cmd                       
         | While Expr Cmd 
         deriving (Show)
