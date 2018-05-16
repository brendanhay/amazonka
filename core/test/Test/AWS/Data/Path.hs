{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.Data.Path
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Data.Path (tests) where

import           Network.AWS.Prelude
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "path"
    [ testGroup "encode"
        [ testCase "doesn't append trailing slash" $
            let x = "/path/without/trailing/slash" in encode x @?= x

        , testCase "preserves trailing slash" $
            let x = "/some/path/with/trailing/slash/" in encode x @?= x

        , testCase "preserves adjacent slashes" $
            let x = "/path//with/adjacent///slahes/" in encode x @?= x
        ]
    ]

encode :: ByteString -> ByteString
encode = toBS . escapePath . rawPath
