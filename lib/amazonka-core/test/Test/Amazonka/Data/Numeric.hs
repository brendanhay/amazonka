-- |
-- Module      : Test.Amazonka.Data.Numeric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Data.Numeric (tests) where

import Amazonka.Prelude
import Test.Amazonka.Util
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "numeric"
    [ testGroup
        "json"
        [ testGroup
            "deserialisation"
            [ testFromJSON
                "natural"
                "123"
                (123 :: Natural),
              testFromJSON
                "int"
                (toLazyBS maxInt)
                maxInt,
              testFromJSON
                "negative int"
                (toLazyBS minInt)
                minInt,
              testFromJSON
                "integer"
                "891324"
                (891324 :: Integer),
              testFromJSON
                "negative integer"
                "-998"
                (-998 :: Integer),
              testFromJSON
                "double"
                "46.39212"
                (46.39212 :: Double),
              testFromJSON
                "negative double"
                "-1.09231"
                (-1.09231 :: Double),
              testFromJSON
                "numeric text"
                "\"22\""
                ("22" :: Text)
            ],
          testGroup
            "serialisation"
            [ testToJSON
                "natural"
                "123"
                (123 :: Natural),
              testToJSON
                "int"
                (toLazyBS maxInt)
                maxInt,
              testToJSON
                "negative int"
                (toLazyBS minInt)
                minInt,
              testToJSON
                "integer"
                "891324"
                (891324 :: Integer),
              testToJSON
                "negative integer"
                "-998"
                (-998 :: Integer),
              testToJSON
                "double"
                "46.39212"
                (46.39212 :: Double),
              testToJSON
                "negative double"
                "-1.09231"
                (-1.09231 :: Double),
              testToJSON
                "numeric string"
                "\"22\""
                ("22" :: String)
            ]
        ],
      testGroup
        "xml"
        [ testGroup
            "deserialisation"
            [ testFromXML
                "natural"
                "123"
                (123 :: Natural),
              testFromXML
                "int"
                (toLazyBS maxInt)
                maxInt,
              testFromXML
                "negative int"
                (toLazyBS minInt)
                minInt,
              testFromXML
                "integer"
                "891324"
                (891324 :: Integer),
              testFromXML
                "negative integer"
                "-998"
                (-998 :: Integer),
              testFromXML
                "double"
                "46.39212"
                (46.39212 :: Double),
              testFromXML
                "negative double"
                "-1.09231"
                (-1.09231 :: Double),
              testFromXML
                "numeric string"
                "22"
                ("22" :: Text)
            ],
          testGroup
            "serialisation"
            [ testToXML
                "natural"
                "123"
                (123 :: Natural),
              testToXML
                "int"
                (toLazyBS maxInt)
                maxInt,
              testToXML
                "negative int"
                (toLazyBS minInt)
                minInt,
              testToXML
                "integer"
                "891324"
                (891324 :: Integer),
              testToXML
                "negative integer"
                "-998"
                (-998 :: Integer),
              testToXML
                "double"
                "46.39212"
                (46.39212 :: Double),
              testToXML
                "negative double"
                "-1.09231"
                (-1.09231 :: Double),
              testToXML
                "numeric text"
                "22"
                ("22" :: Text)
            ]
        ]
    ]
