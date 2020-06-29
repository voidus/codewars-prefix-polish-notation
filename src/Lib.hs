{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Lib
  ( calculate,
  )
where

import Control.Monad (liftM2)
import Data.Functor (($>), (<&>))
import Data.Functor.Classes (Show1 (liftShowsPrec))
import Data.Functor.Foldable (Fix (Fix), cata)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, errorBundlePretty, parse, try, (<|>))
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, float)

type Parser = Parsec Void String

data Op = Add | Sub | Mul | Div
  deriving (Show)

data Token
  = Op Op
  | Number Double
  deriving (Show)

numberParser :: Parser Double
numberParser =
  try float
    <|> (fromIntegral @Integer <$> decimal)

tokenNumberParser :: Parser Token
tokenNumberParser =
  Number <$> numberParser

tokenOpParser :: Parser Token
tokenOpParser =
  Op
    <$> choice
      [ char '*' $> Mul,
        char '+' $> Add,
        char '-' $> Sub,
        char '/' $> Div
      ]

tokenParser :: Parser Token
tokenParser =
  (tokenOpParser <|> tokenNumberParser) <* space

data ExprF a
  = EOp Op a a
  | ENumber Double
  deriving (Functor)

-- this is only needed so we can print Expr's in the repl
instance Show1 ExprF where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> ExprF a -> ShowS
  liftShowsPrec showPrecedence _ precedence exprF =
    \moreString -> case exprF of
      ENumber num -> show num
      EOp op a b -> "(" <> showPrecedence precedence a (show op <> showPrecedence precedence b (")" <> moreString))

type Expr = Fix ExprF

exprParser :: Parser Expr
exprParser =
  do
    tokenParser >>= \case
      Number num -> pure $ ENumber num
      Op op -> (liftM2 $ EOp op) exprParser exprParser
    <&> Fix

fold :: ExprF Double -> Double
fold (ENumber n) = n
fold (EOp op a b) =
  doOp op a b
  where
    doOp :: Op -> Double -> Double -> Double
    doOp Add = (+)
    doOp Sub = (-)
    doOp Mul = (*)
    doOp Div = (/)

calculate :: String -> Double
calculate input =
  case parse exprParser "" input of
    Left bundle -> error $ errorBundlePretty bundle
    Right expression -> cata fold expression
