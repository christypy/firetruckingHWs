-- Parser for Mini-Haskell

module Parser (parseFile) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Exp (Oper(..), Exp(..), opStrs, bopApp, manyApp, tuple)

----------------------------------------------------------------
-- Exported Functions

parseFile :: SourceName -> IO (Either ParseError Exp)
parseFile = parseFromFile program

----------------------------------------------------------------
-- Top-level Parser

program :: Parser Exp
program
    = do{ whiteSpace
        ; e <- exprParser
        ; eof
        ; return e
        }

----------------------------------------------------------------
-- Expression Parser

exprParser :: Parser Exp
exprParser = buildExpressionParser exprOps appParser
    <?> "expression"

exprOps :: OperatorTable Char () Exp
exprOps  = [ [ binary "*" (bopApp Times) AssocLeft ]
           , [ binary "+" (bopApp Plus) AssocLeft ]
           , [ binary "==" (bopApp Equal) AssocLeft ]
           , [ binary "||" (bopApp Or) AssocLeft
             , binary "&&" (bopApp And) AssocLeft ]
           , [ binary ":" (bopApp Cons) AssocRight ] ]

appParser :: Parser Exp
appParser = do{es <- many1 nonAppParser; return (manyApp es)}
        <?> "application"

nonAppParser :: Parser Exp
nonAppParser = varParser <|> constParser
              <|> ifParser <|> letParser
              <|> parens tupleParser

tupleParser :: Parser Exp
tupleParser
    = do{es <- sepBy exprParser commaSep; return $ tuple es}
    <?> "tuple"

varParser :: Parser Exp
varParser = do{i <- identifier; return $ Var i}<?>"variable"

constParser :: Parser Exp
constParser
    =   (do{n <- natural; return $ N $ fromInteger n}<?>"integer")
    <|> (do{symbol "True"; return $ B True}<?>"True")
    <|> (do{symbol "False"; return $ B False}<?>"False")
    <|> (do{reserved "[]"; return Nil}<?>"[]")
    <|> do{op <- choice (map each opStrs); return $ Op op}
  where each (o,s) = (do{reserved s; return o}<?>("operator: "++s))

ifParser :: Parser Exp
ifParser
    = do{ reserved "if" ;   e1 <- exprParser
        ; reserved "then" ; e2 <- exprParser
        ; reserved "else" ; e3 <- exprParser
        ; return $ If e1 e2 e3
        }
    <?> "conditional expression (if)"

letParser :: Parser Exp
letParser
    = do{ reserved "let"
        ; xs <- parens (sepBy identifier commaSep)
            <|> (do{x <- identifier; return [x]}<?>"variable")
        ; reserved "="
        ; e <- exprParser
        ; reserved "in"
        ; be <- exprParser
        ; return $ Let xs e be
        }
    <?> "let binding"

----------------------------------------------------------------
-- Parsec Definitions

langDef
    = haskellStyle
    { identStart        = letter
    , identLetter       = alphaNum <|> oneOf "_'"
    , opStart           = opLetter langDef
    , opLetter          = oneOf "+*=&|:\\"
    , reservedOpNames   = ["(+)","(*)","(==)",
                           "(&&)","(||)","(:)",
                           "\\","[]","="]
    , reservedNames     = [ "True", "False", "not",
                            "if", "then", "else",
                            "head", "tail",
                            "let", "in"]
    }

lang            = P.makeTokenParser langDef
whiteSpace      = P.whiteSpace lang
symbol          = P.symbol lang
identifier      = P.identifier lang
reserved        = P.reserved lang
reservedOp      = P.reservedOp lang
natural         = P.natural lang
parens p        = between (symbol "(") (symbol ")") p
commaSep        = skipMany1 (space <|> char ',')
binary name f assoc = Infix (do{reservedOp name; return f}) assoc
prefix name f       = Prefix (do{reservedOp name; return f})