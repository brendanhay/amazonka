{-# LANGUAGE OverloadedStrings #-}

-- Module      : Test.AWS.Util
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

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
testFromText n t x = testCase n (fromText t @?= Right x)

testToText :: (ToText a, Show a, Eq a)
           => TestName
           -> Text
           -> a
           -> TestTree
testToText n t x = testCase n (toText x @?= t)

testToQuery :: (ToQuery a, Show a, Eq a)
            => TestName
            -> ByteString
            -> a
            -> TestTree
testToQuery n bs x = testCase n (toBS (toQuery (X x)) @?= "x=" <> bs)

testFromXML :: (FromXML a, Show a, Eq a)
            => TestName
            -> LazyByteString
            -> a
            -> TestTree
testFromXML n bs x = testCase n $
    (decodeXML ("<x>" <> bs <> "</x>") >>= parseXML) @?= Right (X x)

testToXML :: (ToXML a, Show a, Eq a)
          => TestName
          -> LazyByteString
          -> a
          -> TestTree
testToXML n bs x = testCase n $
    (encodeXML (X x) @?= declXML <> "<x>" <> bs <> "</x>")

testFromJSON :: (FromJSON a, Show a, Eq a)
             => TestName
             -> LazyByteString
             -> a
             -> TestTree
testFromJSON n bs x = testCase n $
    eitherDecode' ("{\"x\":" <> bs <> "}") @?= Right (X x)

testToJSON :: (ToJSON a, Show a, Eq a)
           => TestName
           -> LazyByteString
           -> a
           -> TestTree
testToJSON n bs x = testCase n (encode x @?= bs)

str :: LazyByteString -> LazyByteString
str bs = "\"" <> bs <> "\""

declXML :: LazyByteString
declXML = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

maxInt :: Int
maxInt = maxBound

minInt :: Int
minInt = minBound

toLazyBS :: ToByteString a => a -> LazyByteString
toLazyBS = LBS.fromStrict . toBS
