{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Compiler.JSON
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.JSON where

import           Compiler.Types
import           Control.Error
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Data.Aeson                as A
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Function             (on)
import           Data.Jason
import           Data.Jason.Types
import           Data.List
import           Data.List                 (intersperse)
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Builder    as Build
import qualified Data.Text.Lazy.IO         as LText
import           Data.Time
import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as Path
import           Formatting
import           Formatting.Time

decodeObject :: Monad m => ByteString -> EitherT LazyText m Object
decodeObject = either (Control.Error.left . LText.pack) return
    . eitherDecode'
    . LBS.fromStrict

parseObject :: (Monad m, FromJSON a) => Object -> EitherT LazyText m a
parseObject = hoistEither . fmapL LText.pack . parseEither parseJSON . Object

mergeObjects :: [Object] -> Object
mergeObjects = foldl' go mempty
  where
    go :: Object -> Object -> Object
    go (unObject -> a) (unObject -> b) = mkObject (assoc value a b)

    value :: Value -> Value -> Value
    value l r =
        case (l, r) of
            (Object x, Object y) -> Object (x `go` y)
            (_,        _)        -> l

    assoc :: Eq k => (v -> v -> v) -> [(k, v)] -> [(k, v)] -> [(k, v)]
    assoc f xs ys = unionBy ((==) `on` fst) (map g xs) ys
      where
        g (k, x) | Just y <- lookup k ys = (k, f x y)
                 | otherwise             = (k, x)

-- toEnv :: (Show a, A.ToJSON a) => a -> Script A.Object
-- toEnv (A.toJSON -> A.Object o) = right o
-- toEnv e                        = left $
--     "Failed to extract JSON Object from: " ++ show e
