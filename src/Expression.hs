module Expression where

data Expression a = 
    Var String
    |Const a
    |BinOp String (Expression a) (Expression a)
instance Show a => Show (Expression a) where
    show (Var x) = x
    show (Const n) = show n
    show (BinOp o e1 e2) = "(" ++ (show e1) ++ o ++ (show e2) ++ ")"
instance Num a => Num (Expression a) where
    fromInteger n = Const (fromInteger n)
    a + b = BinOp "+" a b 
    a - b = BinOp "-" a b 
    a * b = BinOp "*" a b 

eval :: Num a => Expression a -> [(String, a)] -> a
eval (Const n) _ = n
eval (Var s) vals = find s vals where
    find s ((name, value):t) 
        | s == name = value
        | otherwise = find s t
eval (BinOp o e1 e2) vals = case o of
    "+" -> (eval e1 vals) + (eval e2 vals)
    "-" -> (eval e1 vals) - (eval e2 vals)
    "*" -> (eval e1 vals) * (eval e2 vals)

diff (Const n) _ = Const 0
diff (Var v) (Var x) 
    | x /= v = Const 0
    | x == v = Const 1
diff (BinOp o e1 e2) x = case o of
    "+" -> (diff e1 x) + (diff e2 x)
    "-" -> (diff e1 x) - (diff e2 x)
    "*" -> (diff e1 x) * e2 + e1 * (diff e2 x)
