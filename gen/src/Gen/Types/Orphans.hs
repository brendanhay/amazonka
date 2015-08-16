{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Gen.Types.Orphans
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Orphans where

import           Gen.Types.Id
import           Gen.Types.Map
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.List.NonEmpty    as NE
import           Data.Scientific       (floatingOrInteger)
import           Data.String
import qualified Language.Haskell.Exts as Exts
import           Numeric.Natural

instance FromJSON a => FromJSON (NonEmpty a) where
    parseJSON = parseJSON >=> maybe (fail msg) pure . NE.nonEmpty
      where
        msg = "Empty list when expecting at least one element."

instance FromJSON a => FromJSON (Map Id a) where
    parseJSON = parseJSON >=> return . (kvTraversal %~ first mkId)

instance FromJSON Natural where
    parseJSON = withScientific "natural" (f . floatingOrInteger)
      where
        f :: Either Double Integer -> Parser Natural
        f (Left  e)     = fail ("Double when expecting Natural: " ++ show e)
        f (Right x)
            | x < 0     = fail ("Negative when expecting Natural: " ++ show x)
            | otherwise = pure (fromInteger x)

instance ToJSON Natural where
    toJSON = toJSON . toInteger

instance ToJSON a => ToJSON (Identity a) where
    toJSON = toJSON . runIdentity

instance IsString Exts.QOp where
    fromString = Exts.op . Exts.sym
