{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Gen.V2.JSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.JSON where

import qualified Data.Aeson       as A
import           Data.Function    (on)
import           Data.Jason.Types
import           Data.List
import           Data.Monoid
import           Data.SemVer      (Version, fromText, toText)
import qualified Data.Vector      as Vector

instance FromJSON Version where
    parseJSON = withText "semantic_version" $
        either fail return . fromText

instance A.ToJSON Version where
    toJSON = A.String . toText

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
