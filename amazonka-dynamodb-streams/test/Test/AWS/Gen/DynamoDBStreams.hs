{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DynamoDBStreams
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         [ testGetRecords $
--             getRecords
--
--         , testGetShardIterator $
--             getShardIterator
--
--         , testListStreams $
--             listStreams
--
--         , testDescribeStream $
--             describeStream
--
--           ]

--     , testGroup "response"
--         [ testGetRecordsResponse $
--             getRecordsResponse
--
--         , testGetShardIteratorResponse $
--             getShardIteratorResponse
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

testGetRecords :: GetRecords -> TestTree
testGetRecords = req
    "GetRecords"
    "fixture/GetRecords"

testGetShardIterator :: GetShardIterator -> TestTree
testGetShardIterator = req
    "GetShardIterator"
    "fixture/GetShardIterator"

testListStreams :: ListStreams -> TestTree
testListStreams = req
    "ListStreams"
    "fixture/ListStreams"

testDescribeStream :: DescribeStream -> TestTree
testDescribeStream = req
    "DescribeStream"
    "fixture/DescribeStream"

-- Responses

testGetRecordsResponse :: GetRecordsResponse -> TestTree
testGetRecordsResponse = res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse"
    dynamoDBStreams
    (Proxy :: Proxy GetRecords)

testGetShardIteratorResponse :: GetShardIteratorResponse -> TestTree
testGetShardIteratorResponse = res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse"
    dynamoDBStreams
    (Proxy :: Proxy GetShardIterator)

testListStreamsResponse :: ListStreamsResponse -> TestTree
testListStreamsResponse = res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse"
    dynamoDBStreams
    (Proxy :: Proxy ListStreams)

testDescribeStreamResponse :: DescribeStreamResponse -> TestTree
testDescribeStreamResponse = res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse"
    dynamoDBStreams
    (Proxy :: Proxy DescribeStream)
