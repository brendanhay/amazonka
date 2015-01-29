{-# LANGUAGE OverloadedStrings #-}

-- Module      : Test.AWS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Types where

import Data.Aeson
import Network.AWS.Data
import Network.AWS.Prelude
import Test.Tasty.HUnit

newtype Entries a = Entries a
    deriving (Eq, Show)

instance FromXML a => FromXML (Entries a) where
    parseXML x = Entries <$> x .@ "Entries"

assertXML :: (FromXML a, Show a, Eq a) => LazyByteString -> a -> Assertion
assertXML s x = (decodeXML s >>= parseXML) @?= Right x

assertJSON :: (FromJSON a, Show a, Eq a) => LazyByteString -> a -> Assertion
assertJSON s x = eitherDecode' s @?= Right x
