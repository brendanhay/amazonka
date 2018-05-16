{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Test.AWS.Sign.V$
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Sign.V4 (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Test.AWS.Sign.V4.Chunked as Chunked

-- Write some V4 signing properties:
--  test canonical query
--  test query
--  test canonical headers
--  test signed headers
--  test time format
--  test host, x-amz-date, content-sha256 headers exist
--  test empty path
--  test empty query

tests :: TestTree
tests =
    testGroup "v4"
        [ testGroup "chunked"
            [ Chunked.tests
            ]
        ]
