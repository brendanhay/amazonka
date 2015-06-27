{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- Module      : Test.AWS.Data.Numeric
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Data.Numeric (tests) where

import Data.Aeson
import Network.AWS.Prelude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "numeric"
    [ testGroup "JSON serialization"
        [ testEncode encodeItems
        , testDecode decodeItems
        ]
    ]

-- With a basic record, checks several serialization edge cases
-- between String, Int, and Nat values for numbers.
data Item = Item
     { itemNat :: Maybe Nat
     , itemInt :: Maybe Int
     , itemStr :: Maybe String
     } deriving (Eq, Show)

instance ToJSON Item where
    toJSON Item{..} = object
        [ "item_nat" .= itemNat
        , "item_int" .= itemInt
        , "item_str" .= itemStr
        ]

instance FromJSON Item where
    parseJSON = withObject "Item" $ \o -> Item
        <$> o .:? "item_nat"
        <*> o .:? "item_int"
        <*> o .:? "item_str"

encodeItems :: [(Item, LazyByteString)]
encodeItems =
    [ ( Item (Just $ Nat 1) (Just 1) Nothing
      , "{\"item_str\":null,\"item_nat\":1,\"item_int\":1}"
      )

    , ( Item (Just $ Nat 1) (Just 1) (Just "1")
      , "{\"item_str\":\"1\",\"item_nat\":1,\"item_int\":1}"
      )
    ]

testEncode :: [(Item, LazyByteString)] -> TestTree
testEncode = testCase "Numeric JSON Encoding"
    . mapM_ (\(input, e) -> e @=? encode input)

decodeItems :: [(Maybe Item, LazyByteString)]
decodeItems =
    [ ( Just (Item (Just $ Nat 1) (Just 1) Nothing)
      , "{\"item_nat\":1,\"item_int\":1}"
      )

    , ( Just (Item (Just $ Nat 1) (Just (-1)) (Just "1"))
      , "{\"item_nat\":1,\"item_int\":-1, \"item_str\":\"1\"}"
      )

    , ( Nothing
      , "{\"item_nat\":1,\"item_int\":-1, \"item_str\":1}"
      )

    , ( Just (Item (Just $ Nat 1) (Just (-1)) Nothing)
      , "{\"item_nat\":1,\"item_int\":-1}"
      )

    , ( Just (Item (Just $ Nat 1) (Just (-1)) Nothing)
      , "{\"item_nat\":1.0,\"item_int\":-1}"
      )

    , ( Nothing
      , "{\"item_nat\":-1,\"item_int\":1}"
      )

    , ( Nothing
      , "{\"item_nat\":1.2,\"item_int\":1}"
      )

    , ( Nothing
      , "{\"item_nat\":\"1\",\"item_int\":1}"
      )
    ]

testDecode :: [(Maybe Item, LazyByteString)] -> TestTree
testDecode = testCase "Numeric JSON Decoding" . mapM_ check
  where
    check (e, input) = e @=? (decode input :: Maybe Item)
