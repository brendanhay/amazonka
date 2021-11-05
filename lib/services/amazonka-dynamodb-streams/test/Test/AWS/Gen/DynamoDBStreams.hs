{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DynamoDBStreams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.DynamoDBStreams where

import Amazonka.DynamoDBStreams
import qualified Data.Proxy as Proxy
import Test.AWS.DynamoDBStreams.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetShardIterator $
--             newGetShardIterator
--
--         , requestGetRecords $
--             newGetRecords
--
--         , requestListStreams $
--             newListStreams
--
--         , requestDescribeStream $
--             newDescribeStream
--
--           ]

--     , testGroup "response"
--         [ responseGetShardIterator $
--             newGetShardIteratorResponse
--
--         , responseGetRecords $
--             newGetRecordsResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseDescribeStream $
--             newDescribeStreamResponse
--
--           ]
--     ]

-- Requests

requestGetShardIterator :: GetShardIterator -> TestTree
requestGetShardIterator =
  req
    "GetShardIterator"
    "fixture/GetShardIterator.yaml"

requestGetRecords :: GetRecords -> TestTree
requestGetRecords =
  req
    "GetRecords"
    "fixture/GetRecords.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream =
  req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

-- Responses

responseGetShardIterator :: GetShardIteratorResponse -> TestTree
responseGetShardIterator =
  res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetShardIterator)

responseGetRecords :: GetRecordsResponse -> TestTree
responseGetRecords =
  res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecords)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreams)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStream)
