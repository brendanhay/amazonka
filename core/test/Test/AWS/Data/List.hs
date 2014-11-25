{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- Module      : Test.AWS.Data.List
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Data.List (tests) where

import Network.AWS.Prelude
import Network.AWS.Data
import Test.AWS.TH
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "list"
    [ testGroup "deserialise xml"
        [ testCase "entries" $
            parse unflattened (Entries items)
        , testCase "flattened" $
            parse flattened items
        ]
    ]

parse :: (FromXML a, Show a, Eq a) => LazyByteString -> a -> Assertion
parse s x = (decodeXML s >>= parseXML) @?= Right x

newtype Entries = Entries (List "Item" Item)
    deriving (Eq, Show)

instance FromXML Entries where
    parseXML x = Entries
        <$> x .@ "Entries"

data Item = Item
    { itemText :: Text
    , itemDate :: Text
    , itemInt  :: Int
    } deriving (Eq, Show)

instance FromXML Item where
    parseXML x = Item
        <$> x .@ "Text"
        <*> x .@ "Date"
        <*> x .@ "Int"

items :: List "Item" Item
items = List
    [ Item
        { itemText = "828ef3fdfa96f00ad9f27c383fc9ac7"
        , itemDate = "2006-01-01T12:00:00.000Z"
        , itemInt  = 5
        }
    , Item
        { itemText = "fe3f123jfa96f00ad9f27c383fc9sd1"
        , itemDate = "2014-11-02T01:20:12"
        , itemInt  = 123
        }
    ]

unflattened :: LazyByteString
unflattened = [doc|
    <?xml version="1.0" encoding="UTF-8"?>
    <Wrapper>
      <Foo>foo</Foo>
      <Bar>N</Bar>
      <Baz>Ned</Baz>
      <Entries>
        <Item>
          <Date>2006-01-01T12:00:00.000Z</Date>
          <Text>828ef3fdfa96f00ad9f27c383fc9ac7</Text>
          <Int>5</Int>
        </Item>
        <Item>
          <Date>2014-11-02T01:20:12</Date>
          <Text>fe3f123jfa96f00ad9f27c383fc9sd1</Text>
          <Int>123</Int>
        </Item>
      </Entries>
    </Wrapper>
    |]

flattened :: LazyByteString
flattened = [doc|
    <?xml version="1.0" encoding="UTF-8"?>
    <Wrapper>
      <Foo>foo</Foo>
      <Bar>N</Bar>
      <Baz>Ned</Baz>
      <Item>
        <Date>2006-01-01T12:00:00.000Z</Date>
        <Text>828ef3fdfa96f00ad9f27c383fc9ac7</Text>
        <Int>5</Int>
      </Item>
      <Item>
        <Date>2014-11-02T01:20:12</Date>
        <Text>fe3f123jfa96f00ad9f27c383fc9sd1</Text>
        <Int>123</Int>
      </Item>
    </Wrapper>
    |]
