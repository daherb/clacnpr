module Parser (Parser.parse, evalExprs) where

import Text.Parsec
import qualified Text.Parsec as P
data Expr = Lit Float
          | Op Oper
          deriving Show

data Oper = Plus
          | Minus
          | Times
          | Div
          | Pop
          deriving (Eq,Show)

parseLit :: Parsec String () Expr
parseLit = do
  sig <- option '0' (char '-')
  pre <- many1 digit
  post <- option ".0" (do
                            dot <- char '.'
                            post <- many digit
                            return (dot : post)
                        )
  return $ Lit $ read ((sig : pre) ++ post)

ops :: [(String,Oper,(Float -> Float -> Float))]
ops = [("+",Plus,(+)),
       ("-",Minus,(-)),
       ("*",Times,(*)),
       ("/",Div,(/)),
       (".",Pop,\_ -> id)]
parseOp :: Parsec String () Expr
parseOp = do
  choice [do { string s ; return $ Op o } | (s,o,_) <- ops]

parse :: String -> Either ParseError [Expr]
parse s =
  P.parse (do
              es <- many (do
                             e <- parseOp <|>  parseLit
                             spaces
                             return e
                         )
              eof
              return es
          ) "" s

evalExprs :: [Expr] -> [Float] -> (String,[Float])
evalExprs [] s = ("",s)
evalExprs (Lit l:es) s = evalExprs es (l:s)
evalExprs (Op Pop:es) (s:ss) =
  let (a,b) = evalExprs es ss
  in
    (a ++ "\n" ++ show s, b)
evalExprs (Op o:es) (a:b:s) = let f = lookupFun o ops in evalExprs es ((f b a):s)
evalExprs _ s = ("Stack empty",s)

lookupFun :: Oper -> [(String,Oper,Float -> Float -> Float)] -> (Float -> Float -> Float)
lookupFun _ [] = undefined 
lookupFun o' ((_,o,f):os)
  | o == o' = f
  | otherwise = lookupFun o' os
