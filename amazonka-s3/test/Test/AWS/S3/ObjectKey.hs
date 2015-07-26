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

import           Data.String
import           Network.AWS.Prelude
import           Network.AWS.S3
import           Test.Tasty
import           Test.Tasty.HUnit

-- FIXME: convert to quick/smallcheck
tests :: TestTree
tests = testGroup "object key"
    [ testGroup "encoding"
        [ testCase "without delimiters" $
            "key" @=? encodedKey (objectKey '%' "key")

        , testCase "without leading prefix" $
            "some/obj%26ect" @=? encodedKey (objectKey '/' "some/obj&ect")

        , testCase "custom delimiter" $
            "^some^obj%25ect^foo" @=? encodedKey (objectKey '^' "^some^obj%ect^foo")

        , testCase "leading prefix" $
            decoded @=? either ($ '/') id (decodedKey encoded)
        ]

    , testGroup "text"
        [ testCase "deserialise" $
            Right decoded @=? (either ($ '/') id . decodedKey <$> fromText decoded)
        ]
    ]

encoded :: ObjectKey
encoded = objectKey '/' decoded

decoded :: IsString a => a
decoded = "/some=1/path to/foo=bar/object here"
