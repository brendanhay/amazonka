{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DynamoDBStreams
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DynamoDBStreams where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DynamoDBStreams
import Test.AWS.DynamoDBStreams.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetShardIterator $
--             getShardIterator
--
--         , testGetRecords $
--             getRecords
--
--         , testListStreams $
--             listStreams
--
--         , testDescribeStream $
--             describeStream
--
--           ]

--     , testGroup "response"
--         [ testGetShardIteratorResponse $
--             getShardIteratorResponse
--
--         , testGetRecordsResponse $
--             getRecordsResponse
--
--         , testListStreamsResponse $
--             listStreamsResponse
--
--         , testDescribeStreamResponse $
--             describeStreamResponse
--
--           ]
--     ]

-- Requests

testGetShardIterator :: GetShardIterator -> TestTree
testGetShardIterator = req
    "GetShardIterator"
    "fixture/GetShardIterator.yaml"

testGetRecords :: GetRecords -> TestTree
testGetRecords = req
    "GetRecords"
    "fixture/GetRecords.yaml"

testListStreams :: ListStreams -> TestTree
testListStreams = req
    "ListStreams"
    "fixture/ListStreams.yaml"

testDescribeStream :: DescribeStream -> TestTree
testDescribeStream = req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

-- Responses

testGetShardIteratorResponse :: GetShardIteratorResponse -> TestTree
testGetShardIteratorResponse = res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse.proto"
    dynamoDBStreams
    (Proxy :: Proxy GetShardIterator)

testGetRecordsResponse :: GetRecordsResponse -> TestTree
testGetRecordsResponse = res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse.proto"
    dynamoDBStreams
    (Proxy :: Proxy GetRecords)

testListStreamsResponse :: ListStreamsResponse -> TestTree
testListStreamsResponse = res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    dynamoDBStreams
    (Proxy :: Proxy ListStreams)

testDescribeStreamResponse :: DescribeStreamResponse -> TestTree
testDescribeStreamResponse = res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    dynamoDBStreams
    (Proxy :: Proxy DescribeStream)
