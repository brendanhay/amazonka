{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.V2.Naming
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Naming where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson.TH
import           Data.CaseInsensitive      (CI)
import           Data.Char
import           Data.Default
import           Data.Function
import qualified Data.HashMap.Strict       as Map
import           Data.Maybe
import           Data.Monoid               hiding (Sum)
import           Data.Ord
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Manipulate
import           Language.Haskell.TH
import           Network.HTTP.Types.Method

lensName :: Text -> Text
lensName t = fromMaybe t ("_" `Text.stripPrefix` t)

keyName :: Text -> Text
keyName t
    | "_" `Text.isPrefixOf` t = lowerHead (dropLower t)
    | otherwise               = t

dropLower :: Text -> Text
dropLower = Text.dropWhile (not . isUpper)
