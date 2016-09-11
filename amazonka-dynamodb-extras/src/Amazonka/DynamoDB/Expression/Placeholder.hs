{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Amazonka.DynamoDB.Expression.Condition
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Generate placeholders for expression names and values.
--
-- /See:/
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
-- in the AWS documentation.
module Amazonka.DynamoDB.Expression.Placeholder
    ( Placeholders
    , placeholders

    , finalizeNames
    , finalizeValues

    , substituteName
    , substituteValue
    ) where

import Control.Lens
import Control.Monad.Trans.State.Strict (StateT)

import Data.Bifunctor         (first)
import Data.Hashable          (Hashable (..))
import Data.HashMap.Strict    (HashMap)
import Data.Text              (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Tuple             (swap)

import qualified Data.HashMap.Strict        as Map
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as Build
import qualified Data.Text.Lazy.Builder.Int as Build

newtype Supply = Supply [Builder]

data Placeholders a b = Placeholders
    { _supply :: Supply
    , _names  :: HashMap a Builder
    , _values :: HashMap b Builder
    }

placeholders :: (Eq a, Eq b, Hashable a, Hashable b) => Placeholders a b
placeholders = Placeholders (Supply (map Build.decimal [seed..])) mempty mempty
  where
    seed = 1 :: Int

supply :: Functor f
       => (Supply -> f Supply)
       -> Placeholders a b
       -> f (Placeholders a b)
supply = lens _supply (\s a -> s { _supply = a })

names :: Functor f
      => (HashMap a Builder -> f (HashMap a Builder))
      -> Placeholders a b
      -> f (Placeholders a b)
names = lens _names (\s a -> s { _names = a })

values :: Functor f
       => (HashMap b Builder -> f (HashMap b Builder))
       -> Placeholders a b
       -> f (Placeholders a b)
values = lens _values (\s a -> s { _values = a })

finalizeNames :: Placeholders a b -> HashMap Text a
finalizeNames = finalize . _names

finalizeValues :: Placeholders a b -> HashMap Text b
finalizeValues = finalize . _values

finalize :: HashMap a Builder -> HashMap Text a
finalize =
      Map.fromList
    . map (first (LText.toStrict . Build.toLazyText) . swap)
    . Map.toList

substituteName :: (Monad m, Eq a, Hashable a)
               => a
               -> StateT (Placeholders a b) m Builder
substituteName = replace "#n" names

substituteValue :: (Monad m, Eq b, Hashable b)
                => b
                -> StateT (Placeholders a b) m Builder
substituteValue = replace ":v" values

replace :: (Monad m, Eq c, Hashable c)
        => Builder -- ^ A prefix to prepend to the placeholder.
        -> Lens' (Placeholders a b) (HashMap c Builder)
        -> c
        -> StateT (Placeholders a b) m Builder
replace prefix l x = do
    mn <- uses l (Map.lookup x)
    case mn of
        Just n  -> pure n
        Nothing -> do
            n <- mappend prefix <$> next
            l %= Map.insert x n
            pure n

next :: Monad m => StateT (Placeholders a b) m Builder
next = do
    Supply (x:xs) <- use supply
    supply .= Supply xs
    pure x
