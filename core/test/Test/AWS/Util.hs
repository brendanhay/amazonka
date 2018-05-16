{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.Util
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Util where

import           Data.Aeson
import qualified Data.ByteString.Lazy      as LBS
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Network.AWS.Prelude
import           Test.Tasty
import           Test.Tasty.HUnit

doc :: QuasiQuoter
doc = QuasiQuoter
    { quoteExp  = stringE
    , quotePat  = const $ error "No quotePat  defined."
    , quoteType = const $ error "No quoteType defined."
    , quoteDec  = const $ error "No quoteDec  defined."
    }

-- | Dummy root element for testing nested structures.
newtype X a = X a
    deriving (Eq, Show)

instance ToQuery a => ToQuery (X a) where
    toQuery (X x) = "x" =: x

instance FromXML a => FromXML (X a) where
    parseXML = fmap X . parseXML

instance ToXML a => ToElement (X a) where
    toElement (X x) = mkElement "x" x

instance FromJSON a => FromJSON (X a) where
    parseJSON = withObject "X" (fmap X . (.: "x"))

instance ToJSON a => ToJSON (X a) where
    toJSON (X x) = object ["x" .= x]

testFromText :: (FromText a, Show a, Eq a)
             => TestName
             -> Text
             -> a
             -> TestTree
testFromText n t x = testCase n (Right x @?= fromText t)

testToText :: (ToText a, Show a, Eq a)
           => TestName
           -> Text
           -> a
           -> TestTree
testToText n t x = testCase n (t @?= toText x)

testToQuery :: (ToQuery a, Show a, Eq a)
            => TestName
            -> ByteString
            -> a
            -> TestTree
testToQuery n bs x = testCase n (bs @=? toBS (toQuery (X x)))

testFromXML :: (FromXML a, Show a, Eq a)
            => TestName
            -> LazyByteString
            -> a
            -> TestTree
testFromXML n bs x = testCase n $
     Right (X x) @?= (decodeXML (wrapXML bs) >>= parseXML)

testToXML :: (ToXML a, Show a, Eq a)
          => TestName
          -> LazyByteString
          -> a
          -> TestTree
testToXML n bs x = testCase n $ wrapXML bs @?= encodeXML (X x)

testFromJSON :: (FromJSON a, Show a, Eq a)
             => TestName
             -> LazyByteString
             -> a
             -> TestTree
testFromJSON n bs x = testCase n $
    Right (X x) @?= eitherDecode' ("{\"x\":" <> bs <> "}")

testToJSON :: (ToJSON a, Show a, Eq a)
           => TestName
           -> LazyByteString
           -> a
           -> TestTree
testToJSON n bs x = testCase n (bs @?= encode x)

str :: LazyByteString -> LazyByteString
str bs = "\"" <> bs <> "\""

wrapXML :: LazyByteString -> LazyByteString
wrapXML bs =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" <> "<x>" <> bs <> "</x>"

maxInt :: Int
maxInt = maxBound

minInt :: Int
minInt = minBound

toLazyBS :: ToByteString a => a -> LazyByteString
toLazyBS = LBS.fromStrict . toBS
