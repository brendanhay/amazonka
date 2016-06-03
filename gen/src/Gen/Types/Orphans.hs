{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Gen.Types.Orphans
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Orphans where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Bifunctor
import           Data.String
import           Gen.Types.Id
import           Gen.Types.Map
import qualified Language.Haskell.Exts as Exts

instance FromJSON a => FromJSON (Map Id a) where
    parseJSON = parseJSON >=> return . (kvTraversal %~ first mkId)

instance IsString Exts.QOp where
    fromString = Exts.op . Exts.sym
