{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.Json.MegaparsecLex where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.HashMap.Strict as H
import qualified Data.List.NonEmpty as NE
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Void (Void)
import Data.Word (Word8)
import ParsersBench.Json.Common
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L
import Text.Megaparsec.LexicalStream

parseJson :: ByteString -> Value
parseJson bs =
  case parse lexJson "" bs of
    Left err -> error (errorBundlePretty err)
    Right x -> case parse json "" (LexicalStream bs x) of
      Left err -> error (errorBundlePretty err)
      Right y -> y

instance VisualStream [WithOffset JsonTok] where
  showTokens _ = unwords . NE.toList . fmap (prettyTok . unOffset)
  tokensLength _ ts = o1 - o0 + tokenLength t1
    where
      WithOffset o0 _ = NE.head ts
      WithOffset o1 t1 = NE.last ts

type Lexer = Parsec Void ByteString

data JsonTok
  = TNum Scientific
  | TStr Text
  | TOpenCurly
  | TCloseCurly
  | TOpenSquare
  | TCloseSquare
  | TComma
  | TColon
  | TNull
  | TTrue
  | TFalse
  deriving (Eq, Ord)

prettyTok :: JsonTok -> String
prettyTok t = case t of
  TNum n -> show n
  TStr s -> show s
  TOpenCurly -> "{"
  TCloseCurly -> "}"
  TOpenSquare -> "["
  TCloseSquare -> "]"
  TComma -> ","
  TColon -> ":"
  TNull -> "null"
  TTrue -> "true"
  TFalse -> "false"

tokenLength :: JsonTok -> Int
tokenLength = length . prettyTok

lexJson :: Lexer [WithOffset JsonTok]
lexJson = space *> many (withOffset l) <* eof
  where
    l =
      TOpenCurly <$ lexChar (BS.c2w '{')
        <|> TCloseCurly <$ lexChar (BS.c2w '}')
        <|> TOpenSquare <$ lexChar (BS.c2w '[')
        <|> TCloseSquare <$ lexChar (BS.c2w ']')
        <|> TComma <$ lexChar (BS.c2w ',')
        <|> TColon <$ lexChar (BS.c2w ':')
        <|> TNull <$ lexString "null"
        <|> TTrue <$ lexString "true"
        <|> TFalse <$ lexString "false"
        <|> TNum <$> lexeme (L.signed space L.scientific)
        <|> TStr . TE.decodeUtf8
          <$> let w = BS.c2w '"'
               in between (char w) (lexChar w) (takeWhileP (Just "string char") (/= w))

type Parser = Parsec Void (LexicalStream ByteString [WithOffset JsonTok])

json :: Parser Value
json = (Object <$> jobject <|> Array <$> jarray) <* eof

value :: Parser Value
value =
  Object <$> jobject
    <|> Array <$> jarray
    <|> String <$> jstring
    <|> Bool <$> jbool
    <|> Null <$ offsetted TNull
    <|> Number <$> jnumber

jobject :: Parser (H.HashMap Text Value)
jobject =
  between
    (offsetted TOpenCurly)
    (offsetted TCloseCurly)
    (H.fromList <$> jField `sepBy` offsetted TComma)
  where
    jField = (,) <$> jstring <*> (offsetted TColon *> value)
{-# INLINE jobject #-}

jarray :: Parser (V.Vector Value)
jarray =
  between
    (offsetted TOpenSquare)
    (offsetted TCloseSquare)
    (V.fromList <$> value `sepBy` offsetted TComma)
{-# INLINE jarray #-}

jbool :: Parser Bool
jbool = False <$ offsetted TFalse <|> True <$ offsetted TTrue
{-# INLINE jbool #-}

jnumber :: Parser Scientific
jnumber = flip token mempty $ \case
  WithOffset _ (TNum n) -> Just n
  _ -> Nothing
{-# INLINE jnumber #-}

jstring :: Parser Text
jstring = flip token mempty $ \case
  WithOffset _ (TStr s) -> Just s
  _ -> Nothing
{-# INLINE jstring #-}

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme space
{-# INLINE lexeme #-}

lexChar :: Word8 -> Lexer Word8
lexChar = lexeme . char
{-# INLINE lexChar #-}

lexString :: ByteString -> Lexer ByteString
lexString = lexeme . string
{-# INLINE lexString #-}
