module Parser (entry, go) where

import ParseTree
import Data.Functor (($>))
import System.IO
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Token qualified as P

lexer =
  P.makeTokenParser
    emptyDef
      { P.commentLine = "#",
        P.reservedNames = ["let", "T", "F"],
        P.reservedOpNames = ["=", "."]
      }

literal :: Parser Lit
literal =
  (<?> "Literal") $
    IntegerLit <$> P.natural lexer
      <|> reserved "T" $> BoolLit True
      <|> reserved "F" $> BoolLit False

identifier =
  P.identifier lexer <> many (char '\'')

reserved = P.reserved lexer

reservedOp = P.reservedOp lexer

lexeme = P.lexeme lexer

parens = P.parens lexer

go :: FilePath -> IO ()
go fileName = print =<< parseFromFile entry fileName

entry :: Parser [Assign]
entry = spaces *> many assign <* eof

assign :: Parser Assign
assign =
  Assign
    <$> (reserved "let" *> identifier)
    <*> (reservedOp "=" *> expr)

expr :: Parser Expr
expr = app <|> atom

app :: Parser Expr
app = chainl1 atom (return Application)

atom :: Parser Expr
atom =
    parens expr
    <|> try lambda
    <|> Identifier <$> identifier
    <|> Literal <$> literal

lambda :: Parser Expr
lambda =
  Lambda
    <$> identifier
    <*> (reservedOp "." *> expr)
