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
          | Exp
          | Sqrt
          | Pop
          | Swap
          | Dup
          | Help
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
  exp <- option "0" (do
                        e <- char 'e'
                        esig <- option '0' (char '-')
                        edig <- many digit
                        return (e:esig:edig)
                    )
  return $ Lit $ read ((sig : pre) ++ post ++ exp)

nop :: Float -> Float -> Float
nop = \_ -> id

ops :: [(String,Oper,(Float -> Float -> Float),String)]
ops = [("+",Plus,(+), "Adds the two values on top of the stack and puts the result on top"),
       ("-",Minus,(-), "Substracts the two values on top of the stack and puts the result on top"),
       ("*",Times,(*), "Multiplies the two values on top of the stack and puts the result on top"),
       ("/",Div,(/), "Divides the two values on top of the stack and puts the result on top"),
       ("^",Exp,(**), "Computes the power of the two values on the top of the stack and puts the result on top"),
       ("sqrt",Sqrt,nop,"Comutes the square root of the value on the top of the stack and puts the result on top"),
       (".",Pop,nop, "Pops and prints the top value of the stack"),
       ("swp",Swap,nop, "Swaps the two topmost elements of the stack"),
       ("dup",Dup,nop, "Duplicates the topmost elements of the stack"),
       ("help",Help,nop, "Prints this help message")]
      
parseOp :: Parsec String () Expr
parseOp = do
  choice [do { try (string s) ; return $ Op o } | (s,o,_,_) <- ops]

parse :: String -> Either ParseError [Expr]
parse s =
  P.parse (do
              es <- many (do
                             e <- try parseLit <|> parseOp
                             spaces
                             return e
                         )
              eof
              return es
          ) "" s

evalExprs :: [Expr] -> [Float] -> (String,[Float])
evalExprs [] s = ("",s)
evalExprs (Lit l:es) s = evalExprs es (l:s)
evalExprs (Op Sqrt:es) (s:ss) =
  evalExprs es (sqrt s:ss)
evalExprs (Op Pop:es) (s:ss) =
  let (a,b) = evalExprs es ss
  in
    (show s ++ "\n" ++ a, b)
evalExprs (Op Swap:es) (a:b:ss) = evalExprs es (b:a:ss)
evalExprs (Op Dup:es) (a:ss) = evalExprs es (a:a:ss)
evalExprs (Op Help:es) ss =
  let (a,b) = evalExprs es ss
  in
    ("\n" ++ printHelp ++ "\n" ++ a, b)
  where
    printHelp =
      unlines ["\t" ++ s ++ "\t" ++ d | (s,_,_,d) <- ops ]
evalExprs (Op o:es) (a:b:s) = let f = lookupFun o ops in evalExprs es ((f b a):s)
evalExprs _ [] = ("Stack empty",[])
evalExprs _ s@[_] = ("Too few operands",s)

lookupFun :: Oper -> [(String,Oper,Float -> Float -> Float,String)] -> (Float -> Float -> Float)
lookupFun _ [] = undefined 
lookupFun o' ((_,o,f,_):os)
  | o == o' = f
  | otherwise = lookupFun o' os
