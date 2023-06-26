{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Control.Monad.State as S
import Control.Monad.Identity
import qualified Text.Megaparsec.InjectState as Inj

newtype FakeState a = FakeState {runFakeState :: a }
  deriving (Functor, Applicative, Monad) via Identity

instance S.MonadState Int FakeState where
  get = pure 0
  put _ = pure ()

type Parser = ParsecT Void Text FakeState

type ParserTState = ParsecT Void Text (S.State Int)

type ParserInjectSt = Inj.ParsecT Int Void Text Identity

type StateTParser = S.StateT Int (Parsec Void Text)

type ParserM = MonadParsec Void Text

main :: IO ()
main =
  defaultMain
    [ bparser "no modify" manyAbs (const $ sepBy (char 'a') (char 'b'))
    , bparser "1/2 modify" manyAbs (const $ sepBy (char 'a') (S.modify succ *> char 'b'))
    , bparser "all modify" manyAbs (const $ sepBy (S.modify succ *> char 'a') (S.modify succ *> char 'b'))
    ]

-- | Perform a series to measurements with the same parser.
bparser :: forall a.
  (NFData a) =>
  -- | Name of the benchmark group
  String ->
  -- | How to construct input
  (Int -> Text) ->
  -- | The parser receiving its future input
  ((Text, Int) -> forall m. (ParserM m, S.MonadState Int m) => m a) ->
  -- | The benchmark
  Benchmark
bparser name f p = bgroup name
    [ bgroup "pure" (bs <$> stdSeries)
    , bgroup "ParserT State" (sbs <$> stdSeries)
    , bgroup "StateT Parser" (s'bs <$> stdSeries)
    , bgroup "State inject parser" (ibs <$> stdSeries)
    ]
  where
    bs n = env (return (f n, n)) (bench (show n) . nf p')
    p' (s, n) = runFakeState $ runParserT (p (s, n) :: Parser a) "" s
    sbs n = env (return (f n, n)) (bench (show n) . nf sp')
    sp' (s, n) = (`S.runState` 0) $ runParserT (p (s, n) :: ParserTState a) "" s
    s'bs n = env (return (f n, n)) (bench (show n) . nf s'p')
    s'p' (s, n) = runParser ((`S.runStateT` 0) (p (s, n) :: StateTParser a)) "" s
    ibs n = env (return (f n, n)) (bench (show n) . nf ip')
    ip' (s, n) = runIdentity $ Inj.runParserT (p (s, n) :: ParserInjectSt a) "" s 0


-- | The series of sizes to try as part of 'bparser'.
stdSeries :: [Int]
stdSeries = [1000] -- [500, 1000, 2000, 4000]

----------------------------------------------------------------------------
-- Helpers

-- | Like 'manyAs', but interspersed with \'b\'s.
manyAbs :: Int -> Text
manyAbs n = T.take (if even n then n + 1 else n) (T.replicate n "ab")
