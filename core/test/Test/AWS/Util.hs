{-# LANGUAGE OverloadedStrings #-}

-- Module      : Test.AWS.Util
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Util where

import           Data.Aeson
import qualified Data.Text                 as Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Network.AWS.Prelude
import           Test.Tasty.HUnit

-- | Dummy root XML element for testing nested structures.
newtype Root a = Root a
    deriving (Eq, Show)

instance FromXML a => FromXML (Root a) where
    parseXML x = Root <$> parseXML x

assertXML :: (FromXML a, Show a, Eq a) => LazyByteString -> a -> Assertion
assertXML s x = (decodeXML s >>= parseXML) @?= Right x

assertJSON :: (FromJSON a, Show a, Eq a) => LazyByteString -> a -> Assertion
assertJSON s x = eitherDecode' s @?= Right x

doc :: QuasiQuoter
doc = QuasiQuoter { quoteExp = stringE }
