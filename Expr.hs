module Expr where

import Parsing
import Data.Maybe
import Test.QuickCheck


-- \ A
data Expr = N Double | X | Op Ops Expr Expr | F Func Expr deriving Eq
data Ops = Add | Mul deriving (Eq,Show)
data Func = Sin | Cos deriving (Eq,Show)

x = X
example = (Op Mul (N 5) (F Sin x))
example2 = (F Sin x)
example3 = (Op Add (Op Mul (N 5) (N 6)) (F Cos x))


-- | B
instance Show Expr where
  show = showExpr

instance Arbitrary Expr where
  arbitrary = sized arbExpr

showExpr :: Expr -> String
showExpr (Op o e1 e2) = showExpr e1 ++ showOp o ++ showExpr e2
showExpr (F f e)      = (showFunc f) ++ " (" ++ (showExpr e) ++ ")"
showExpr (X)          = "x"
showExpr (N d)        = show d

showFunc Sin = "sin"
showFunc Cos = "cos"
showOp Add = "+"
showOp Mul = "*"

-- | C

eval :: Expr -> Double -> Double
eval (N d) _            = d
eval (X) val            = val
eval (Op Add e1 e2) val = eval e1 val + eval e2 val
eval (Op Mul e1 e2) val = eval e1 val * eval e2 val
eval (Op _ _ _)  _      = error "Wrong operand"
eval (F Sin e) val      = sin (eval e val)
eval (F Cos e) val      = cos (eval e val)
eval (F _ _) _          = error "Wrong function"

-- | D

readExpr :: String -> Maybe Expr
readExpr s = getElem (parse expr s)
  where
    getElem (Just (s,_)) = Just s
    getElem Nothing      = Nothing

-- | Parsers for expr, terms and factors
expr, term, factor :: Parser Expr
expr = leftAssoc (Op Add) term (char '+')
term = leftAssoc (Op Mul) factor (char '*')
factor = (N <$> readsP) <|> (char '(' *> expr <* char ')')
                          <|> do f <- func'
                                 e <- factor
                                 return (F f e)
                          <|> (do x <- char 'x'
                                  return X)

-- | parser for a Func data type
func' :: Parser Func
func' = do string "sin "
           return Sin
      <|>
        do string "cos "
           return Cos

-- | parser for a string
string :: String -> Parser String
string ""    = return ""
string (c:s) = do c' <- char c
                  s' <- string s
                  return (c':s')

-- | input: constructor, parser for result, parser for seperators
leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do i:is <- chain item sep
                           return (foldl op i is)
-- | E

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = fromJust (readExpr (showExpr e)) == e


-- TO DO
arbExpr :: Int -> Gen Expr
arbExpr n = frequency [(1, do d <- rDouble; return (N d)),
                       (n, do e1 <- arbExpr s; e2 <- arbExpr s; return (Op Add e1 e2)),
                       (n, do e1 <- arbExpr s; e2 <- arbExpr s; return (Op Mul e1 e2)),
                       (n, do e <- arbExpr s; return (F Sin e)),
                       (n, do e <- arbExpr s; return (F Cos e)),
                       (1, do return X)]
                        where
                          s = div n 2

rDouble :: Gen Double
rDouble = arbitrary

-- | F
-- | Simplifies the Expr where possible, lots of patterns lol
simplify :: Expr -> Expr
simplify (N d)                  = (N d)
simplify X                      = X
simplify (Op Add (N 0) e)       = simplify e
simplify (Op Mul (N 0) _)       = (N 0)
simplify (Op Add e (N 0))       = simplify e
simplify (Op Mul _ (N 0))       = (N 0)
simplify (Op Mul (N 1) e)       = simplify e
simplify (Op Mul e (N 1))       = simplify e
simplify (Op Add (N d1) (N d2)) = (N (d1+d2))
simplify (Op Mul (N d1) (N d2)) = (N (d1*d2))
simplify (Op Mul e1 e2)         = (Op Mul (simplify e1) (simplify e2))
simplify (Op Add e1 e2)         = (Op Add (simplify e1) (simplify e2))
simplify (F f e)                = (F f (simplify e))

-- | Symbolic Differentiation function
differentiate :: Expr -> Expr
differentiate e = simplify (differentiate' e)

differentiate' :: Expr -> Expr
differentiate' (N d)            = N 0
differentiate' (X)              = N 1
differentiate' (Op Add e1 e2)   = (Op Add (differentiate' e1) (differentiate' e2))
differentiate' (Op Mul e1 e2)   = (Op Add (Op Mul (differentiate' e1) e2) (Op Mul e1 (differentiate' e2)))
differentiate' (Op Mul (N d) e) = (Op Mul (N d) (differentiate' e))
differentiate' (Op Mul e (N d)) = (Op Mul (differentiate' e) (N d))
differentiate' (F Sin e)        = (Op Mul (F Cos (e)) (differentiate' e))
differentiate' (F Cos e)        = (Op Mul (Op Mul (N (-1)) (F Sin (e))) (differentiate' e))
differentiate' (F Sin X)        = (F Cos X)
differentiate' (F Cos X)        = (Op Mul (N (-1)) (F Sin X))
