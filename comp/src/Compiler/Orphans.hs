{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Compiler.Orphans
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Orphans where

import qualified Data.Aeson            as A
import           Data.Bifunctor
import           Data.CaseInsensitive  (CI)
import qualified Data.CaseInsensitive  as CI
import           Data.Functor.Identity
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as Map
import qualified Data.Jason            as J
import qualified Data.Jason.Types      as J
import           Data.Scientific       (floatingOrInteger)
import qualified Data.SemVer           as SemVer
import           Data.Text             (Text)
import           Numeric.Natural

instance (J.FromJSON a, CI.FoldCase a) => J.FromJSON (CI a) where
    parseJSON = fmap CI.mk . J.parseJSON

instance J.FromJSON a => J.FromJSON (HashMap (CI Text) a) where
    parseJSON = fmap (Map.fromList . map (first CI.mk) . Map.toList) . J.parseJSON

instance J.FromJSON Natural where
    parseJSON = J.withScientific "natural" (f . floatingOrInteger)
      where
        f :: Either Double Integer -> J.Parser Natural
        f (Left  e)     = fail ("Double when expecting Natural: " ++ show e)
        f (Right x)
            | x < 0     = fail ("Negative when expecting Natural: " ++ show x)
            | otherwise = pure (fromInteger x)

instance A.ToJSON Natural where
    toJSON = A.toJSON . toInteger

instance A.ToJSON SemVer.Version where
    toJSON = A.toJSON . SemVer.toText

instance A.ToJSON a => A.ToJSON (Identity a) where
    toJSON = A.toJSON . runIdentity
