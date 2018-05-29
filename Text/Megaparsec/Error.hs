-- |
-- Module      :  Text.Megaparsec.Error
-- Copyright   :  © 2015–2018 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse errors. Current version of Megaparsec supports well-typed errors
-- instead of 'String'-based ones. This gives a lot of flexibility in
-- describing what exactly went wrong as well as a way to return arbitrary
-- data in case of failure.
--
-- You probably do not want to import this module directly because
-- "Text.Megaparsec" re-exports it anyway.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Megaparsec.Error
  ( -- * Parse error type
    ErrorItem (..)
  , ErrorFancy (..)
  , ParseError (..)
  , errorPos
    -- * Pretty-printing
  , ShowToken (..)
  , LineToken (..)
  , ShowErrorComponent (..)
  , parseErrorPretty
  , parseErrorPretty'
  , parseErrorPretty_
  , parseErrorTextPretty )
where

import Control.DeepSeq
import Control.Exception
import Data.Char (chr)
import Data.Data (Data)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Void
import Data.Word (Word8)
import GHC.Generics
import Text.Megaparsec.Pos
import Text.Megaparsec.Stream
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as E

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

----------------------------------------------------------------------------
-- Parse error type

-- | Data type that is used to represent “unexpected\/expected” items in
-- 'ParseError'. The data type is parametrized over the token type @t@.
--
-- @since 5.0.0

data ErrorItem t
  = Tokens (NonEmpty t)      -- ^ Non-empty stream of tokens
  | Label (NonEmpty Char)    -- ^ Label (cannot be empty)
  | EndOfInput               -- ^ End of input
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, Functor)

instance NFData t => NFData (ErrorItem t)

-- | Additional error data, extendable by user. When no custom data is
-- necessary, the type is typically indexed by 'Void' to “cancel” the
-- 'ErrorCustom' constructor.
--
-- @since 6.0.0

data ErrorFancy e
  = ErrorFail String
    -- ^ 'fail' has been used in parser monad
  | ErrorIndentation Ordering Pos Pos
    -- ^ Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
  | ErrorCustom e
    -- ^ Custom error data, can be conveniently disabled by indexing
    -- 'ErrorFancy' by 'Void'
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, Functor)

instance NFData a => NFData (ErrorFancy a) where
  rnf (ErrorFail str) = rnf str
  rnf (ErrorIndentation ord ref act) = ord `seq` rnf ref `seq` rnf act
  rnf (ErrorCustom a) = rnf a

-- | @'ParseError' t e@ represents a parse error parametrized over the token
-- type @t@ and the custom data @e@.
--
-- Note that the stack of source positions contains current position as its
-- head, and the rest of positions allows to track full sequence of include
-- files with topmost source file at the end of the list.
--
-- 'Semigroup' and 'Monoid' instances of the data type allow to merge parse
-- errors from different branches of parsing. When merging two
-- 'ParseError's, the longest match is preferred; if positions are the same,
-- custom data sets and collections of message items are combined. Note that
-- fancy errors take precedence over trivial errors in merging.
--
-- @since 6.0.0

data ParseError t e
  = TrivialError SourcePos (Maybe (ErrorItem t)) (Set (ErrorItem t))
    -- ^ Trivial errors, generated by Megaparsec's machinery. The data
    -- constructor includes the source position of error, unexpected token
    -- (if any), and expected tokens.
    --
    -- Type of the first argument was changed in the version /7.0.0/.
  | FancyError SourcePos (Set (ErrorFancy e))
    -- ^ Fancy, custom errors.
    --
    -- Type of the first argument was changed in the version /7.0.0/.
  deriving (Show, Read, Eq, Data, Typeable, Generic)

instance (NFData t, NFData e) => NFData (ParseError t e)

instance (Ord t, Ord e) => Semigroup (ParseError t e) where
  (<>) = mergeError
  {-# INLINE (<>) #-}

instance (Ord t, Ord e) => Monoid (ParseError t e) where
  mempty  = TrivialError (initialPos "") Nothing E.empty
  mappend = (<>)
  {-# INLINE mappend #-}

instance ( Show t
         , Ord t
         , ShowToken t
         , Typeable t
         , Show e
         , ShowErrorComponent e
         , Typeable e )
  => Exception (ParseError t e) where
  displayException = parseErrorPretty

-- | Get position of given 'ParseError'.
--
-- @since 6.0.0

errorPos :: ParseError t e -> SourcePos
errorPos (TrivialError p _ _) = p
errorPos (FancyError   p _)   = p

-- | Merge two error data structures into one joining their collections of
-- message items and preferring the longest match. In other words, earlier
-- error message is discarded. This may seem counter-intuitive, but
-- 'mergeError' is only used to merge error messages of alternative branches
-- of parsing and in this case longest match should be preferred.

mergeError :: (Ord t, Ord e)
  => ParseError t e
  -> ParseError t e
  -> ParseError t e
mergeError e1 e2 =
  case errorPos e1 `compare` errorPos e2 of
    LT -> e2
    EQ ->
      case (e1, e2) of
        (TrivialError s1 u1 p1, TrivialError _ u2 p2) ->
          TrivialError s1 (n u1 u2) (E.union p1 p2)
        (FancyError {}, TrivialError {}) -> e1
        (TrivialError {}, FancyError {}) -> e2
        (FancyError s1 x1, FancyError _ x2) ->
          FancyError s1 (E.union x1 x2)
    GT -> e1
  where
    -- NOTE The logic behind this merging is that since we only combine
    -- parse errors that happen at exactly the same position, all the
    -- unexpected items will be prefixes of input stream at that position or
    -- labels referring to the same thing. Our aim here is to choose the
    -- longest prefix (merging with labels and end of input is somewhat
    -- arbitrary, but is necessary because otherwise we can't make
    -- ParseError lawful Monoid and have nice parse errors at the same
    -- time).
    n Nothing  Nothing = Nothing
    n (Just x) Nothing = Just x
    n Nothing (Just y) = Just y
    n (Just x) (Just y) = Just (max x y)
{-# INLINE mergeError #-}

----------------------------------------------------------------------------
-- Pretty-printing

-- | Type class 'ShowToken' includes methods that allow to pretty-print
-- single token as well as stream of tokens. This is used for rendering of
-- error messages.
--
-- @since 5.0.0

class ShowToken a where

  -- | Pretty-print non-empty stream of tokens. This function is also used
  -- to print single tokens (represented as singleton lists).

  showTokens :: NonEmpty a -> String

instance ShowToken Char where
  showTokens = stringPretty

instance ShowToken Word8 where
  showTokens = stringPretty . fmap (chr . fromIntegral)

-- | Type class for tokens that support operations necessary for selecting
-- and displaying relevant line of input.
--
-- @since 6.0.0

class LineToken a where

  -- | Convert a token to a 'Char'. This is used to print relevant line from
  -- input stream by turning a list of tokens into a 'String'.

  tokenAsChar :: a -> Char

  -- | Check if given token is a newline or contains newline.

  tokenIsNewline :: a -> Bool

instance LineToken Char where
  tokenAsChar = id
  tokenIsNewline x = x == '\n'

instance LineToken Word8 where
  tokenAsChar = chr . fromIntegral
  tokenIsNewline x = x == 10

-- | The type class defines how to print custom data component of
-- 'ParseError'.
--
-- @since 5.0.0

class Ord a => ShowErrorComponent a where

  -- | Pretty-print custom data component of 'ParseError'.

  showErrorComponent :: a -> String

  -- | Length of the error component in characters, used for highlighting of
  -- parse errors in input string.
  --
  -- @since 7.0.0

  errorComponentLen :: a -> Int
  errorComponentLen _ = 1

instance (Ord t, ShowToken t) => ShowErrorComponent (ErrorItem t) where
  showErrorComponent = \case
    Tokens   ts -> showTokens ts
    Label label -> NE.toList label
    EndOfInput  -> "end of input"
  errorComponentLen = \case
    Tokens   ts -> NE.length ts
    _           -> 1

instance ShowErrorComponent e => ShowErrorComponent (ErrorFancy e) where
  showErrorComponent (ErrorFail msg) = msg
  showErrorComponent (ErrorIndentation ord ref actual) =
    "incorrect indentation (got " <> show (unPos actual) <>
    ", should be " <> p <> show (unPos ref) <> ")"
    where
      p = case ord of
            LT -> "less than "
            EQ -> "equal to "
            GT -> "greater than "
  showErrorComponent (ErrorCustom a) = showErrorComponent a

instance ShowErrorComponent Void where
  showErrorComponent = absurd

-- | Pretty-print a 'ParseError'. The rendered 'String' always ends with a
-- newline.
--
-- @since 5.0.0

parseErrorPretty
  :: ( Ord t
     , ShowToken t
     , ShowErrorComponent e )
  => ParseError t e    -- ^ Parse error to render
  -> String            -- ^ Result of rendering
parseErrorPretty e =
  sourcePosPretty (errorPos e) <> ":\n" <> parseErrorTextPretty e

-- | Pretty-print a 'ParseError' and display the line on which the parse
-- error occurred. The rendered 'String' always ends with a newline.
--
-- Note that if you work with include files and have a stack of
-- 'SourcePos'es in 'ParseError', it's up to you to provide correct input
-- stream corresponding to the file in which parse error actually happened.
--
-- 'parseErrorPretty'' is defined in terms of the more general
-- 'parseErrorPretty_' function which allows to specify tab width as well:
--
-- > parseErrorPretty' = parseErrorPretty_ defaultTabWidth
--
-- @since 6.0.0

parseErrorPretty'
  :: ( ShowToken (Token s)
     , LineToken (Token s)
     , ShowErrorComponent e
     , Stream s )
  => s                 -- ^ Original input stream
  -> ParseError (Token s) e -- ^ Parse error to render
  -> String            -- ^ Result of rendering
parseErrorPretty' = parseErrorPretty_ defaultTabWidth

-- | Just like 'parseErrorPretty'', but allows to specify tab width.
--
-- @since 6.1.0

parseErrorPretty_
  :: forall s e.
     ( ShowToken (Token s)
     , LineToken (Token s)
     , ShowErrorComponent e
     , Stream s )
  => Pos               -- ^ Tab width
  -> s                 -- ^ Original input stream
  -> ParseError (Token s) e -- ^ Parse error to render
  -> String             -- ^ Result of rendering
parseErrorPretty_ w s e =
  sourcePosPretty (errorPos e) <> ":\n" <>
    padding <> "|\n" <>
    lineNumber <> " | " <> rline <> "\n" <>
    padding <> "| " <> rpadding <> pointer <> "\n" <>
    parseErrorTextPretty e
  where
    epos       = errorPos e
    elen       =
      case e of
        TrivialError _ Nothing _ -> 1
        TrivialError _ (Just x) _ -> errorComponentLen x
        FancyError _ xs ->
          E.foldl' (\a b -> max a (errorComponentLen b)) 1 xs
    lineNumber = (show . unPos . sourceLine) epos
    padding    = replicate (length lineNumber + 1) ' '
    rpadding   = replicate (unPos (sourceColumn epos) - 1) ' '
    pointer    = replicate elen '^'
    rline      =
      case rline' of
        [] -> "<empty line>"
        xs -> expandTab w xs
    rline'     = fmap tokenAsChar . chunkToTokens (Proxy :: Proxy s) $
      selectLine (sourceLine epos) s

-- | Pretty-print a textual part of a 'ParseError', that is, everything
-- except stack of source positions. The rendered 'String' always ends with a
-- newline.
--
-- @since 5.1.0

parseErrorTextPretty
  :: ( Ord t
     , ShowToken t
     , ShowErrorComponent e )
  => ParseError t e    -- ^ Parse error to render
  -> String            -- ^ Result of rendering
parseErrorTextPretty (TrivialError _ us ps) =
  if isNothing us && E.null ps
    then "unknown parse error\n"
    else messageItemsPretty "unexpected " (maybe E.empty E.singleton us) <>
         messageItemsPretty "expecting "  ps
parseErrorTextPretty (FancyError _ xs) =
  if E.null xs
    then "unknown fancy parse error\n"
    else unlines (showErrorComponent <$> E.toAscList xs)

----------------------------------------------------------------------------
-- Helpers

-- | @stringPretty s@ returns pretty representation of string @s@. This is
-- used when printing string tokens in error messages.

stringPretty :: NonEmpty Char -> String
stringPretty (x:|[])      = charPretty x
stringPretty ('\r':|"\n") = "crlf newline"
stringPretty xs           = "\"" <> concatMap f (NE.toList xs) <> "\""
  where
    f ch =
      case charPretty' ch of
        Nothing     -> [ch]
        Just pretty -> "<" <> pretty <> ">"

-- | @charPretty ch@ returns user-friendly string representation of given
-- character @ch@, suitable for using in error messages.

charPretty :: Char -> String
charPretty ' ' = "space"
charPretty ch = fromMaybe ("'" <> [ch] <> "'") (charPretty' ch)

-- | If the given character has a pretty representation, return that,
-- otherwise 'Nothing'. This is an internal helper.

charPretty' :: Char -> Maybe String
charPretty' '\NUL' = pure "null"
charPretty' '\SOH' = pure "start of heading"
charPretty' '\STX' = pure "start of text"
charPretty' '\ETX' = pure "end of text"
charPretty' '\EOT' = pure "end of transmission"
charPretty' '\ENQ' = pure "enquiry"
charPretty' '\ACK' = pure "acknowledge"
charPretty' '\BEL' = pure "bell"
charPretty' '\BS'  = pure "backspace"
charPretty' '\t'   = pure "tab"
charPretty' '\n'   = pure "newline"
charPretty' '\v'   = pure "vertical tab"
charPretty' '\f'   = pure "form feed"
charPretty' '\r'   = pure "carriage return"
charPretty' '\SO'  = pure "shift out"
charPretty' '\SI'  = pure "shift in"
charPretty' '\DLE' = pure "data link escape"
charPretty' '\DC1' = pure "device control one"
charPretty' '\DC2' = pure "device control two"
charPretty' '\DC3' = pure "device control three"
charPretty' '\DC4' = pure "device control four"
charPretty' '\NAK' = pure "negative acknowledge"
charPretty' '\SYN' = pure "synchronous idle"
charPretty' '\ETB' = pure "end of transmission block"
charPretty' '\CAN' = pure "cancel"
charPretty' '\EM'  = pure "end of medium"
charPretty' '\SUB' = pure "substitute"
charPretty' '\ESC' = pure "escape"
charPretty' '\FS'  = pure "file separator"
charPretty' '\GS'  = pure "group separator"
charPretty' '\RS'  = pure "record separator"
charPretty' '\US'  = pure "unit separator"
charPretty' '\DEL' = pure "delete"
charPretty' '\160' = pure "non-breaking space"
charPretty' _      = Nothing

-- | Transforms a list of error messages into their textual representation.

messageItemsPretty :: ShowErrorComponent a
  => String            -- ^ Prefix to prepend
  -> Set a             -- ^ Collection of messages
  -> String            -- ^ Result of rendering
messageItemsPretty prefix ts
  | E.null ts = ""
  | otherwise =
    let f = orList . NE.fromList . E.toAscList . E.map showErrorComponent
    in prefix <> f ts <> "\n"

-- | Print a pretty list where items are separated with commas and the word
-- “or” according to the rules of English punctuation.

orList :: NonEmpty String -> String
orList (x:|[])  = x
orList (x:|[y]) = x <> " or " <> y
orList xs       = intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

-- | Select a line from input stream given its number.

selectLine
  :: forall s. (LineToken (Token s), Stream s)
  => Pos               -- ^ Number of line to select
  -> s                 -- ^ Input stream
  -> Tokens s          -- ^ Selected line
selectLine l = go pos1
  where
    go !n !s =
      if n == l
        then fst (takeWhile_ notNewline s)
        else go (n <> pos1) (stripNewline $ snd (takeWhile_ notNewline s))
    notNewline = not . tokenIsNewline
    stripNewline s =
      case take1_ s of
        Nothing -> s
        Just (_, s') -> s'

-- | Replace tab characters with given number of spaces.

expandTab
  :: Pos
  -> String
  -> String
expandTab w' = go 0
  where
    go 0 []        = []
    go 0 ('\t':xs) = go w xs
    go 0 (x:xs)    = x : go 0 xs
    go !n xs       = ' ' : go (n - 1) xs
    w              = unPos w'
