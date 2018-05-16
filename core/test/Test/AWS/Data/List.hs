{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.Data.List
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Data.List (tests) where

import           Network.AWS.Prelude
import           Test.AWS.Util
import           Test.Tasty

tests :: TestTree
tests = testGroup "list"
    [ testGroup "query"
        [ testGroup "serialise"
            [ testGroup "non-flattened"
                [ testToQuery "absent"
                    "x.name=absent"
                    (NonFlat "absent" absent)

                , testToQuery "primitive"
                    "x.itemSet.item.1=1&x.itemSet.item.2=2&x.itemSet.item.3=3&x.name=primitive"
                    (NonFlat "primitive" (Just ([1, 2, 3] :: [Int])))

                , testToQuery "complex"
                    "x.itemSet.item.1.value=1&x.itemSet.item.2.value=2&x.name=complex"
                    (NonFlat "complex" (Just [Item 1, Item 2]))
                ]

            , testGroup "flattened"
                [ testToQuery "absent/empty"
                    "x.name=empty"
                    (Flat "empty" empty)

                , testToQuery "primitive"
                    "x.item.1=4&x.item.2=5&x.item.3=6&x.name=primitive"
                    (Flat "primitive" (Just ([4, 5, 6] :: [Int])))

                , testToQuery "complex"
                    "x.item.1.value=9&x.item.2.value=10&x.name=complex"
                    (Flat "complex" (Just [Item 9, Item 10]))
                ]
            ]
        ]

    , testGroup "xml"
        [ testGroup "deserialise"
            [ testGroup "non-flattened"
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

            , testGroup "flattened"
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

        , testGroup "serialise"
            [ testGroup "non-flattened"
                [ testToXML "absent"
                    "<name>absent</name>"
                    (NonFlat "absent" absent)

                , testToXML "primitive"
                    "<name>primitive</name><itemSet><item>1</item><item>2</item><item>3</item></itemSet>"
                    (NonFlat "primitive" (Just ([1, 2, 3] :: [Int])))

                , testToXML "complex"
                    "<name>complex</name><itemSet><item><value>1</value></item><item><value>2</value></item></itemSet>"
                    (NonFlat "complex" (Just [Item 1, Item 2]))
                ]

            , testGroup "flattened"
                [ testToXML "absent/empty"
                    "<name>empty</name>"
                    (Flat "empty" empty)

                , testToXML "primitive"
                    "<name>primitive</name><item>4</item><item>5</item><item>6</item>"
                    (Flat "primitive" (Just ([4, 5, 6] :: [Int])))

                , testToXML "complex"
                    "<name>complex</name><item><value>9</value></item><item><value>10</value></item>"
                    (Flat "complex" (Just [Item 9, Item 10]))
                ]
            ]
        ]
    ]

empty :: Maybe [Int]
empty = Just []

absent :: Maybe [Int]
absent = Nothing

data NonFlat a = NonFlat Text (Maybe [a])
    deriving (Eq, Show)

instance ToQuery a => ToQuery (NonFlat a) where
    toQuery (NonFlat n x) = mconcat
        [ "name"    =: n
        , "itemSet" =: toQuery (toQueryList "item" <$> x)
        ]

instance FromXML a => FromXML (NonFlat a) where
    parseXML x = NonFlat
         <$>  x .@  "name"
         <*> (x .@? "itemSet"
                .!@ mempty
                >>= may (parseXMLList "item"))

instance ToXML a => ToXML (NonFlat a) where
    toXML (NonFlat n x) = mconcat
        [ "name"    @= n
        , "itemSet" @= toXML (toXMLList "item" <$> x)
        ]

data Flat a = Flat Text (Maybe [a])
    deriving (Eq, Show)

instance ToQuery a => ToQuery (Flat a) where
    toQuery (Flat n x) = mconcat
        [ "name" =: n
        , toQuery (toQueryList "item" <$> x)
        ]

instance FromXML a => FromXML (Flat a) where
    parseXML x = Flat
         <$> x .@  "name"
         <*> may (parseXMLList "item") x

instance ToXML a => ToXML (Flat a) where
    toXML (Flat n x) = mconcat
        [ "name" @= n
        , toXML (toXMLList "item" <$> x)
        ]

newtype Item = Item Int
    deriving (Eq, Show)

instance ToQuery Item where
    toQuery (Item n) = "value" =: n

instance FromXML Item where
    parseXML x = Item <$> x .@ "value"

instance ToXML Item where
    toXML (Item n) = "value" @= n
