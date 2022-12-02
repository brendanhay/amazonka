-- |
-- Module      : Test.Amazonka.Data.Maybe
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Data.Maybe (tests) where

import Amazonka.Data
import Amazonka.Prelude hiding (Item)
import Test.Amazonka.Util
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "maybe"
    [ testGroup
        "xml"
        [ testGroup
            "serialise"
            [ testCase "nothing" $
                wrapXML "<Key>foo</Key>"
                  @?= encodeXML (X (Item "foo" Nothing)),
              testCase "just" $
                wrapXML "<Key>bar</Key><Num>23</Num>"
                  @?= encodeXML (X (Item "bar" (Just 23)))
            ]
        ]
    ]

data Item = Item Text (Maybe Int)
  deriving stock (Eq, Show)

instance ToXML Item where
  toXML (Item x y) =
    mconcat
      [ "Key" @= x,
        "Num" @= y
      ]
