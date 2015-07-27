{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.S3.ObjectKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.S3.ObjectKey (tests) where

import           Network.AWS.Prelude
import           Network.AWS.S3
import           Test.Tasty
import           Test.Tasty.HUnit

-- FIXME: expand, convert to quick/smallcheck properties.
tests :: TestTree
tests = testGroup "object key"
    [ testGroup "encoding"
        [ testCase "without delimiters" $
            "key" @=? toBS (toPath (ObjectKey "key"))

        , testCase "without leading prefix" $
            "some/obj%23ect" @=? toBS (toPath (ObjectKey "some/obj#ect"))

        , testCase "custom delimiter" $
            "%5Esome%5Eobj%25ect%5Efoo" @=?
                 toBS (toPath (ObjectKey "^some^obj%ect^foo"))

        , testCase "leading prefix" $
             "/some=1/path%20to/foo=bar/object%20here" @=?
                 toBS (toPath (ObjectKey "/some=1/path to/foo=bar/object here"))
        ]
    ]
