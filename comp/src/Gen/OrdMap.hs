{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

module Gen.OrdMap
    ( OrdMap
    , toList
    , fromList
    , keys
    , values
    ) where

import Control.Applicative
import Control.Lens
import Data.Foldable       (Foldable (..))
import Data.Jason.Types
import Data.Monoid
import Data.Text           (Text)
import Data.Traversable
import Prelude             hiding (foldr, map)

data OrdMap k v = OrdMap { toList :: [(k, v)] }
    deriving (Eq, Functor, Show)

instance Foldable (OrdMap k) where
    foldr f x (OrdMap xs) = foldr (f . snd) x xs

instance Traversable (OrdMap k) where
    traverse f (OrdMap xs) = OrdMap <$> traverse (\(k, v) -> (k,) <$> f v) xs

instance Monoid (OrdMap k v) where
    mempty      = OrdMap mempty
    mappend a b = OrdMap (toList a <> toList b)

instance FromJSON v => FromJSON (OrdMap Text v) where
    parseJSON = withObject "ordered_map" $ \(unObject -> o) ->
        OrdMap <$> traverse (\(k, v) -> (k,) <$> parseJSON v) o

instance Bifunctor OrdMap where
    bimap f g = OrdMap . fmap (bimap f g) . toList

fromList :: [(k, v)] -> OrdMap k v
fromList = OrdMap

keys :: OrdMap k v -> [k]
keys = fmap fst . toList

values :: OrdMap k v -> [v]
values = fmap snd . toList
