{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.Data.Maybe
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Data.Maybe (tests) where

import           Network.AWS.Prelude
import           Test.AWS.Util
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "maybe"
    [ testGroup "xml"
        [ testGroup "serialise"
            [ testCase "nothing" $
                wrapXML "<Key>foo</Key>"
                    @?= encodeXML (X (Item "foo" Nothing))

            , testCase "just" $
                wrapXML "<Key>bar</Key><Num>23</Num>"
                    @?= encodeXML (X (Item "bar" (Just 23)))
            ]
        ]
    ]

data Item = Item Text (Maybe Int)
    deriving (Eq, Show)

instance ToXML Item where
    toXML (Item x y) = mconcat
        [ "Key" @= x
        , "Num" @= y
        ]
