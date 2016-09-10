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
-- Placeholders for Expression Names and Values.
--
-- /See:/
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
-- in the AWS documentation.
module Amazonka.DynamoDB.Expression.Placeholder
    ( substitute
    , bisubstitute
    , replace
    , identifier
    ) where

import Control.Lens                     (_1, _2)
import Control.Lens.Zoom                (zoom)
import Control.Monad.Trans.State.Strict (StateT, gets, modify')

import Data.Bitraversable     (Bitraversable (..))
import Data.Hashable          (Hashable (..))
import Data.HashMap.Strict    (HashMap)
import Data.Monoid            ((<>))
import Data.Text.Lazy.Builder (Builder)
import Data.Vector.Unboxed    (Vector)

import qualified Data.HashMap.Strict    as Map
import qualified Data.Text.Lazy.Builder as Build
import qualified Data.Vector.Unboxed    as Vector

-- | Substitute the argument of a 'Traversable' with a placeholder.
substitute :: (Monad m, Traversable t, Eq a, Hashable a)
           => t a
           -> StateT (HashMap a Builder) m (t Builder)
substitute = traverse (replace ":")

-- | Substitute both arguments of a 'Bitraversable' with placeholders.
bisubstitute :: (Monad m, Bitraversable p, Eq a, Eq b, Hashable a, Hashable b)
             => p a b
             -> StateT (HashMap a Builder, HashMap b Builder) m (p Builder Builder)
bisubstitute = bitraverse (zoom _1 . replace "#") (zoom _2 . replace ":")

-- | Replace a value with a memoized placeholder.
replace :: (Monad m, Eq a, Hashable a)
        => Builder -- ^ A prefix to prepend to the placeholder.
        -> a
        -> StateT (HashMap a Builder) m Builder
replace prefix x = do
    mn <- gets (Map.lookup x)
    case mn of
        Just n  -> pure n
        Nothing -> do
            let n = prefix <> identifier x
            modify' (Map.insert x n)
            pure n

identifier :: Hashable a => a -> Builder
identifier = Build.fromString . map ((Vector.!) alphabet) . toDigits . hash

toDigits :: Integral a => a -> [a]
toDigits 0 = []
toDigits x
    | x < 0     = 0 : digits
    | otherwise = digits
  where
    (rest, i) = quotRem (abs x) (fromIntegral base)

    digits = i : toDigits rest

base :: Int
base = Vector.length alphabet

alphabet :: Vector Char
alphabet = Vector.fromList $ ['a' .. 'z'] ++ ['0' .. '9']
