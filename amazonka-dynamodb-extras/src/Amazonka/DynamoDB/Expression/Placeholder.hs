{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Amazonka.DynamoDB.Expression.Placeholder
    ( Placeholders (..)
    , NamesAndValues
    , Values

    , Substitute
    , substituteAll
    ) where

import Amazonka.DynamoDB.Item (DynamoValue (..), Value)

import Control.Lens                     (Lens', lens, use, uses, (%=), (.=), _1,
                                         _2)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Maybe        (MaybeT)
import Control.Monad.Trans.State.Strict (State, runState)

import Data.Bifunctor         (second)
import Data.Hashable          (Hashable (..))
import Data.HashMap.Strict    (HashMap)
import Data.Text              (Text)
import Data.Text.Lazy.Builder (Builder)

import qualified Data.HashMap.Strict        as Map
import qualified Data.Text.Lazy.Builder     as Build
import qualified Data.Text.Lazy.Builder.Int as Build

type Substitute a = State (Memo a)

type NamesAndValues = (HashMap Text Builder, HashMap Value Builder)
type Values         = HashMap Value Builder

newtype Supply = Supply [Builder]

newSupply :: Builder -> Supply
newSupply p = Supply (map (mappend p . Build.decimal) ([1..] :: [Int]))

data Memo a = Memo
    { _names  :: Supply
    , _values :: Supply
    , _cached :: a
    }

names, values :: Lens' (Memo a) Supply
names  = lens _names  (\s a -> s { _names  = a })
values = lens _values (\s a -> s { _values = a })

cached :: Lens' (Memo a) a
cached = lens _cached (\s a -> s { _cached = a })

newMemo :: a -> Memo a
newMemo = Memo (newSupply "#n") (newSupply ":v")

substituteAll :: s -> State (Memo s) a -> (a, s)
substituteAll s m = second _cached (runState m (newMemo s))

class Monad m => Placeholders m where
    substituteName  :: Text -> m Builder
    substituteValue :: DynamoValue a => a -> m Builder

instance Placeholders m => Placeholders (MaybeT m) where
    substituteName  = lift . substituteName
    substituteValue = lift . substituteValue

instance Placeholders (State (Memo NamesAndValues)) where
    substituteName  = substitute names  (cached._1)
    substituteValue = substitute values (cached._2) . toValue

instance Placeholders (State (Memo Values)) where
    substituteName  = pure . Build.fromText
    substituteValue = substitute values cached . toValue

substitute :: (Hashable a, Eq a)
           => Lens' s Supply
           -> Lens' s (HashMap a Builder)
           -> a
           -> State s Builder
substitute supply memo k = do
    mn <- uses memo (Map.lookup k)
    case mn of
        Just  n -> return n
        Nothing -> do
            n <- next supply
            memo %= Map.insert k n
            return n

next :: Lens' s Supply -> State s Builder
next l = do
    Supply (x:xs) <- use l
    l .= Supply xs
    pure x
