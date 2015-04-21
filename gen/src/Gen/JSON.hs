{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Gen.JSON
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.JSON where

import           Control.Error
import qualified Data.Aeson       as A
import           Data.Function    (on)
import           Data.Jason.Types
import           Data.List
import           Data.Monoid

parse :: FromJSON a => Object -> Script a
parse = hoistEither . parseEither parseJSON . Object

toEnv :: (Show a, A.ToJSON a) => a -> Script A.Object
toEnv (A.toJSON -> A.Object o) = right o
toEnv e                        = left $
    "Failed to extract JSON Object from: " ++ show e

merge :: [Object] -> Object
merge = foldl' go mempty
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
