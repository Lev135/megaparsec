{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Megaparsec.LexicalStream where

import Data.Data
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Text.Megaparsec

data LexicalStream rs ts = LexicalStream
  { rawStream :: rs,
    tokStream :: ts
  }

instance forall rs ts. (Stream ts) => Stream (LexicalStream rs ts) where
  type Token (LexicalStream rs ts) = Token ts
  type Tokens (LexicalStream rs ts) = Tokens ts

  tokenToChunk Proxy = tokenToChunk @ts Proxy
  tokensToChunk Proxy = tokensToChunk @ts Proxy
  chunkToTokens Proxy = chunkToTokens @ts Proxy
  chunkLength Proxy = chunkLength @ts Proxy
  chunkEmpty Proxy = chunkEmpty @ts Proxy
  take1_ (LexicalStream rs ts) = fmap (LexicalStream rs) <$> take1_ ts
  takeN_ n (LexicalStream rs ts) = fmap (LexicalStream rs) <$> takeN_ n ts
  takeWhile_ f (LexicalStream rs ts) = LexicalStream rs <$> takeWhile_ f ts

instance forall rs ts. (VisualStream ts) => VisualStream (LexicalStream rs ts) where
  showTokens Proxy = showTokens @ts Proxy
  tokensLength Proxy = tokensLength @ts Proxy

class HasRawOffset t where
  getRawOffset :: t -> Int

instance
  forall rs ts.
  (HasRawOffset (Token ts), Stream ts, TraversableStream rs) =>
  TraversableStream (LexicalStream rs ts)
  where
  reachOffsetNoLine o' (PosState (LexicalStream rs ts) o sp tw pref) =
    PosState (LexicalStream rs' ts') o' sp' tw' pref'
    where
      ts' = dropN_ (o' - o) ts
      PosState rs' _ sp' tw' pref' = case take1_ ts of
        Nothing -> PosState rs 0 sp tw pref
        Just (t, _) ->
          let ro = getRawOffset t
              ro' = maybe (ro + length_ rs - 1) (getRawOffset . fst) $ take1_ ts'
           in reachOffsetNoLine ro' $ PosState rs ro sp tw pref

  reachOffset o' (PosState (LexicalStream rs ts) o sp tw pref) =
    (mstr, PosState (LexicalStream rs' ts') o' sp' tw' pref')
    where
      ts' = dropN_ (o' - o) ts
      (mstr, PosState rs' _ sp' tw' pref') = case take1_ ts of
        Nothing -> (Nothing, PosState rs 0 sp tw pref)
        Just (t, _) ->
          let ro = getRawOffset t
              ro' = maybe (ro + length_ rs - 1) (getRawOffset . fst) $ take1_ ts'
           in reachOffset ro' $ PosState rs ro sp tw pref

dropN_ :: (Stream s) => Int -> s -> s
dropN_ n s = case takeN_ n s of
  Just (_, s') -> s'
  Nothing -> snd $ takeWhile_ (const True) s

length_ :: forall s. (Stream s) => s -> Int
length_ s = chunkLength @s Proxy $ fst $ takeWhile_ (const True) s

data WithOffset t = WithOffset Int t
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

unOffset :: WithOffset t -> t
unOffset (WithOffset _ t) = t

instance HasRawOffset (WithOffset t) where
  getRawOffset (WithOffset o _) = o

liftToken :: t -> WithOffset t
liftToken = WithOffset 0

withOffset :: (MonadParsec e s m) => m a -> m (WithOffset a)
withOffset la = getOffset >>= \o -> WithOffset o <$> la

offsetted :: (Token s ~ WithOffset t, MonadParsec e s m, Ord t) => t -> m (WithOffset t)
offsetted c = token test (Set.singleton . Tokens . NE.singleton . liftToken $ c)
  where
    test ot =
      if unOffset ot == c
        then Just ot
        else Nothing
