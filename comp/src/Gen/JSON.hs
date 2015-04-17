{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

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
import           Control.Monad
import           Control.Monad.Trans.Except
import Control.Monad.Error
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import           Data.ByteString      (ByteString)
import           Data.Function        (on)
import qualified Data.HashMap.Strict  as Map
import           Data.Jason.Types
import           Data.List
import           Data.Monoid
import           Data.Text            (Text)

def :: MonadError e m => Text -> m Object -> m Object
def k = flip catchError (const . return $ mkObject [(k, Object mempty)])

parse :: FromJSON a => Object -> Either String a
parse = parseEither parseJSON . Object

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
