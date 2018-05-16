{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Test.AWS.S3.Internal
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.S3.Internal where

import           Data.Time
import           Network.AWS.Prelude
import           Network.AWS.S3
import           Test.AWS.Fixture
import           Test.AWS.Prelude
import           Test.Tasty
import           Test.Tasty.HUnit

-- FIXME: expand, convert to quick/smallcheck properties.
objectKeyTests :: TestTree
objectKeyTests = testGroup "object key"
    [ testGroup "encoding"
        [ testCase "without delimiters" $
            "/key" @=?
                enc (ObjectKey "key")

        , testCase "without leading prefix" $
            "/some/obj%23ect" @=?
                enc (ObjectKey "some/obj#ect")

        , testCase "custom delimiter" $
            "/%5Esome%5Eobj%25ect%5Efoo" @=?
                enc (ObjectKey "^some^obj%ect^foo")

        , testCase "leading prefix" $
            "/some%3D1/path%20to/foo%3Dbar/object%20here" @=?
                enc (ObjectKey "/some=1/path to/foo=bar/object here")
        ]
    ]
  where
    enc :: ObjectKey -> ByteString
    enc = toBS . escapePath . rawPath

requestPutObjectACLWithBody :: PutObjectACL -> TestTree
requestPutObjectACLWithBody = req
    "PutObjectACLWithBody"
    "fixture/PutObjectACLWithBody.yaml"

requestPutObjectACLWithHeaders :: PutObjectACL -> TestTree
requestPutObjectACLWithHeaders = req
    "PutObjectACLWithHeaders"
    "fixture/PutObjectACLWithHeaders.yaml"
