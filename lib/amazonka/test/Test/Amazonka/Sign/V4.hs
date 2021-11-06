-- |
-- Module      : Test.Amazonka.Sign.V$
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Sign.V4 (tests) where

import qualified Test.Amazonka.Sign.V4.Chunked as Chunked
import Test.Tasty (TestTree, testGroup)

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
  testGroup
    "v4"
    [ testGroup
        "chunked"
        [ Chunked.tests
        ]
    ]
