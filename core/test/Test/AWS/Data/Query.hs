{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.Query.List
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Data.Query (tests) where

import           Network.AWS.Prelude
import           Test.Tasty
import           Test.Tasty.HUnit    (testCase, (@?=))

tests :: TestTree
tests = testGroup "query"
    [ testGroup "fromString"
        [ testCase "key" $
            parseQueryString "foo" @?=
                QList [ QPair "foo" (QValue Nothing)
                      ]

        , testCase "key=" $
            parseQueryString "foo=" @?=
                QList [ QPair "foo" (QValue Nothing)
                      ]

        , testCase "key=value" $
            parseQueryString "foo=bar" @?=
                QList [ QPair "foo" (QValue (Just "bar"))
                      ]

        , testCase "key&.." $
            parseQueryString "foo&bar&baz" @?=
                QList [ QPair "foo" (QValue Nothing)
                      , QPair "bar" (QValue Nothing)
                      , QPair "baz" (QValue Nothing)
                      ]

        , testCase "key=value&.." $
            parseQueryString "foo=1&bar=2&baz&qux=3" @?=
                QList [ QPair "foo" (QValue (Just "1"))
                      , QPair "bar" (QValue (Just "2"))
                      , QPair "baz" (QValue Nothing)
                      , QPair "qux" (QValue (Just "3"))
                      ]

        ]
    ]
