module Arithmetic where

data Term
    = TermTrue
    | TermFalse
    | TermZero
    | TermSucc Term
    | TermPred Term
    | TermNull Term
    | TermCond Term Term Term
    deriving (Eq)

instance Show Term where
    show :: Term -> String
    show TermTrue         = "true"
    show TermFalse        = "false"
    show TermZero         = "0"
    show (TermSucc t)     = "(succ" ++ " " ++ show t ++ ")"
    show (TermPred t)     = "(pred" ++ " " ++ show t ++ ")"
    show (TermNull t)     = "(null" ++ " " ++ show t ++ ")"
    show (TermCond b s t) 
        = "(if " ++ show b ++ " then " ++ show s ++ " else " ++ show t ++ ")"

evalStep :: Term -> Term
evalStep (TermSucc t) = TermSucc t' where t' = eval t
evalStep (TermPred TermZero) = TermZero
evalStep (TermPred (TermSucc t)) = t
evalStep (TermPred t) = TermPred t' where t' = eval t
evalStep (TermNull TermZero) = TermTrue
evalStep (TermNull (TermSucc _)) = TermFalse 
evalStep (TermNull t) = TermNull t' where t' = eval t 
evalStep (TermCond TermTrue t _)  = eval t
evalStep (TermCond TermFalse _ t) = eval t
evalStep (TermCond b s t)         = eval (TermCond b' s t) where b' = eval b
evalStep t = t 

eval :: Term -> Term
eval t = if t == t' then t else eval t' where t' = evalStep t