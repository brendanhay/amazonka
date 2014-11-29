{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Gen.Orphans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Orphans where

import qualified Data.Aeson           as A
import           Data.Bifunctor
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Jason.Types
import           Data.SemVer          (Version, fromText, toText)
import           Data.Text            (Text)

instance FromJSON (CI Text) where
    parseJSON = withText "case-insensitive" (return . CI.mk)

instance FromJSON a => FromJSON (HashMap (CI Text) a) where
    parseJSON = fmap (Map.fromList . map (first CI.mk) . Map.toList) . parseJSON

instance FromJSON Version where
    parseJSON = withText "semantic_version" $
        either fail return . fromText

instance A.ToJSON Version where
    toJSON = A.String . toText
