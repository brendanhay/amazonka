{-# LANGUAGE OverloadedStrings #-}

-- Module      : Test.AWS.Data.List
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Data.List (tests) where

import           Network.AWS.Prelude
import           Test.AWS.Util
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "list"
    [ testGroup "xml"
        [ testGroup "deserialise non-flattened"
            [ testFromXML "absent"
                "<name>absent</name>"
                (NonFlat "absent" absent)

            , testFromXML "empty"
                "<name>empty</name><itemSet/>"
                (NonFlat "empty" absent)

            , testFromXML "primitive"
                "<name>primitive</name><itemSet><item>1</item><item>2</item><item>3</item></itemSet>"
                (NonFlat "primitive" (Just ([1, 2, 3] :: [Int])))

            , testFromXML "complex"
                "<name>complex</name><itemSet><item><value>1</value></item><item><value>2</value></item></itemSet>"
                (NonFlat "complex" (Just [Item 1, Item 2]))
            ]

        , testGroup "deserialise flattened xml"
            [ testFromXML "absent/empty"
                "<name>empty</name>"
                (Flat "empty" empty)

            , testFromXML "primitive"
                "<name>primitive</name><item>4</item><item>5</item><item>6</item>"
                (Flat "primitive" (Just ([4, 5, 6] :: [Int])))

            , testFromXML "complex"
                "<name>complex</name><item><value>9</value></item><item><value>10</value></item>"
                (Flat "complex" (Just [Item 9, Item 10]))
            ]
        ]

    -- , testGroup "serialise non-flattened xml"
    --     [
    --     ]

    -- , testGroup "serialise flattened xml"
    --     [
    --     ]
    ]

empty :: Maybe [Int]
empty = Just []

absent :: Maybe [Int]
absent = Nothing

data NonFlat a = NonFlat Text (Maybe [a])
    deriving (Eq, Show)

instance FromXML a => FromXML (NonFlat a) where
    parseXML x = NonFlat
         <$>  x .@  "name"
         <*> (x .@? "itemSet"
                .!@ mempty
                >>= may (parseXMLList "item"))

data Flat a = Flat Text (Maybe [a])
    deriving (Eq, Show)

instance FromXML a => FromXML (Flat a) where
    parseXML x = Flat
         <$> x .@  "name"
         <*> may (parseXMLList "item") x

newtype Item = Item Int
    deriving (Eq, Show)

instance FromXML Item where
    parseXML x = Item <$> x .@ "value"
