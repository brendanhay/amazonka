{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Compiler.Types.Orphans
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Orphans where

import           Compiler.Types.Id
import           Compiler.Types.Map
import           Control.Lens
import           Control.Monad
import qualified Data.Aeson         as A
import           Data.Bifunctor
import qualified Data.Jason         as J
import qualified Data.Jason.Types   as J
import           Data.Scientific    (floatingOrInteger)
import           Numeric.Natural

instance J.FromJSON a => J.FromJSON (Map Id a) where
    parseJSON = J.parseJSON >=> return . (kvTraversal %~ first mkId)

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

instance A.ToJSON a => A.ToJSON (Identity a) where
    toJSON = A.toJSON . runIdentity
