{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.Json.MegaparsecSimpleOpt
  ( parseJson,
  )
where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.HashMap.Strict as H
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void
import Data.Word (Word8)
import ParsersBench.Json.Common
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void ByteString

cCloseCurly, cCloseSquare, cComma, cDoubleQuote, cOpenCurly, cOpenSquare, cColon, cMinus, cf, cn, ct, c0, c9 :: Word8
cCloseCurly = BS.c2w '}'
cCloseSquare = BS.c2w ']'
cComma = BS.c2w ','
cDoubleQuote = BS.c2w '"'
cOpenCurly = BS.c2w '{'
cOpenSquare = BS.c2w '['
cColon = BS.c2w ':'
cMinus = BS.c2w '-'
cf = BS.c2w 'f'
cn = BS.c2w 'n'
ct = BS.c2w 't'
c0 = BS.c2w '0'
c9 = BS.c2w '9'

parseJson :: ByteString -> Value
parseJson bs =
  case parse json "" bs of
    Left err -> error (errorBundlePretty err)
    Right x -> x

json :: Parser Value
json = space *> (Object <$> jobject <|> Array <$> jarray)

value :: Parser Value
value = do
  w <- lookAhead anySingle
  if
    | w == cDoubleQuote -> String <$> jstring
    | w == cOpenCurly -> Object <$> jobject
    | w == cOpenSquare -> Array <$> jarray
    | w == cf || w == ct -> Bool <$> jbool
    | w == cn -> Null <$ lexString "null"
    | w >= c0 && w <= c9 || w == cMinus -> Number <$> jnumber
    | otherwise -> fail "not a valid json value"

jobject :: Parser (H.HashMap Text Value)
jobject =
  between
    (lexChar cOpenCurly)
    (lexChar cCloseCurly)
    (H.fromList <$> jField `sepBy` lexChar cComma)
  where
    jField = (,) <$> jstring <*> (lexChar cColon *> value)
{-# INLINE jobject #-}

jarray :: Parser (Vector Value)
jarray =
  between
    (lexChar cOpenSquare)
    (lexChar cCloseSquare)
    (V.fromList <$> value `sepBy` lexChar cComma)
{-# INLINE jarray #-}

jbool :: Parser Bool
jbool = False <$ lexString "false" <|> True <$ lexString "true"
{-# INLINE jbool #-}

jnumber :: Parser Scientific
jnumber = lexeme $ L.signed space L.scientific
{-# INLINE jnumber #-}

jstring :: Parser Text
jstring =
  between
    (char cDoubleQuote)
    (lexChar cDoubleQuote)
    (TE.decodeUtf8 <$> takeWhileP (Just "string char") (/= cDoubleQuote))
{-# INLINE jstring #-}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space
{-# INLINE lexeme #-}

lexChar :: Word8 -> Parser Word8
lexChar = lexeme . char
{-# INLINE lexChar #-}

lexString :: ByteString -> Parser ByteString
lexString = lexeme . string
{-# INLINE lexString #-}
