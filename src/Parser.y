{
module Parser where
import Data.Char
import Arithmetic
}

%name arithmetic
%tokentype { Token }
%error { parseError }

%token
    true   { TokenTrue }
    false  { TokenFalse }
    '0'    { TokenZero }
    succ   { TokenSucc }
    pred   { TokenPred }
    null   { TokenNull }
    if     { TokenIf }
    then   { TokenThen }
    else   { TokenElse }

%%

Term 
    : true                        { TermTrue }
    | false                       { TermFalse }
    | '0'                         { TermZero }
    | succ Term                   { TermSucc $2 }
    | pred Term                   { TermPred $2 }
    | null Term                   { TermNull $2 }
    | if Term then Term else Term { TermCond $2 $4 $6 }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
    = TokenTrue
    | TokenFalse
    | TokenZero
    | TokenSucc
    | TokenPred
    | TokenNull
    | TokenIf
    | TokenThen
    | TokenElse
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | '0' == c  = TokenZero : lexer cs

lexVar cs =
    case span isAlpha cs of
        ("true", rest) -> TokenTrue : lexer rest
        ("false", rest) -> TokenFalse : lexer rest
        ("succ", rest) -> TokenSucc : lexer rest
        ("pred", rest) -> TokenPred : lexer rest
        ("null", rest) -> TokenNull : lexer rest
        ("if", rest) -> TokenIf : lexer rest
        ("then", rest) -> TokenThen : lexer rest
        ("else", rest) -> TokenElse : lexer rest
}