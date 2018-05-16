{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DynamoDBStreams
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DynamoDBStreams where

import Data.Proxy
import Network.AWS.DynamoDBStreams
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
--             getShardIterator
--
--         , requestGetRecords $
--             getRecords
--
--         , requestListStreams $
--             listStreams
--
--         , requestDescribeStream $
--             describeStream
--
--           ]

--     , testGroup "response"
--         [ responseGetShardIterator $
--             getShardIteratorResponse
--
--         , responseGetRecords $
--             getRecordsResponse
--
--         , responseListStreams $
--             listStreamsResponse
--
--         , responseDescribeStream $
--             describeStreamResponse
--
--           ]
--     ]

-- Requests

requestGetShardIterator :: GetShardIterator -> TestTree
requestGetShardIterator = req
    "GetShardIterator"
    "fixture/GetShardIterator.yaml"

requestGetRecords :: GetRecords -> TestTree
requestGetRecords = req
    "GetRecords"
    "fixture/GetRecords.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams = req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream = req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

-- Responses

responseGetShardIterator :: GetShardIteratorResponse -> TestTree
responseGetShardIterator = res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse.proto"
    dynamoDBStreams
    (Proxy :: Proxy GetShardIterator)

responseGetRecords :: GetRecordsResponse -> TestTree
responseGetRecords = res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse.proto"
    dynamoDBStreams
    (Proxy :: Proxy GetRecords)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams = res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    dynamoDBStreams
    (Proxy :: Proxy ListStreams)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream = res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    dynamoDBStreams
    (Proxy :: Proxy DescribeStream)
