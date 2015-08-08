{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Kinesis
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Kinesis where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Kinesis
import Test.AWS.Kinesis.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testPutRecord $
--             putRecord
--
--         , testMergeShards $
--             mergeShards
--
--         , testGetRecords $
--             getRecords
--
--         , testGetShardIterator $
--             getShardIterator
--
--         , testListTagsForStream $
--             listTagsForStream
--
--         , testAddTagsToStream $
--             addTagsToStream
--
--         , testPutRecords $
--             putRecords
--
--         , testDeleteStream $
--             deleteStream
--
--         , testRemoveTagsFromStream $
--             removeTagsFromStream
--
--         , testListStreams $
--             listStreams
--
--         , testCreateStream $
--             createStream
--
--         , testSplitShard $
--             splitShard
--
--         , testDescribeStream $
--             describeStream
--
--           ]

--     , testGroup "response"
--         [ testPutRecordResponse $
--             putRecordResponse
--
--         , testMergeShardsResponse $
--             mergeShardsResponse
--
--         , testGetRecordsResponse $
--             getRecordsResponse
--
--         , testGetShardIteratorResponse $
--             getShardIteratorResponse
--
--         , testListTagsForStreamResponse $
--             listTagsForStreamResponse
--
--         , testAddTagsToStreamResponse $
--             addTagsToStreamResponse
--
--         , testPutRecordsResponse $
--             putRecordsResponse
--
--         , testDeleteStreamResponse $
--             deleteStreamResponse
--
--         , testRemoveTagsFromStreamResponse $
--             removeTagsFromStreamResponse
--
--         , testListStreamsResponse $
--             listStreamsResponse
--
--         , testCreateStreamResponse $
--             createStreamResponse
--
--         , testSplitShardResponse $
--             splitShardResponse
--
--         , testDescribeStreamResponse $
--             describeStreamResponse
--
--           ]
--     ]

-- Requests

testPutRecord :: PutRecord -> TestTree
testPutRecord = req
    "PutRecord"
    "fixture/PutRecord"

testMergeShards :: MergeShards -> TestTree
testMergeShards = req
    "MergeShards"
    "fixture/MergeShards"

testGetRecords :: GetRecords -> TestTree
testGetRecords = req
    "GetRecords"
    "fixture/GetRecords"

testGetShardIterator :: GetShardIterator -> TestTree
testGetShardIterator = req
    "GetShardIterator"
    "fixture/GetShardIterator"

testListTagsForStream :: ListTagsForStream -> TestTree
testListTagsForStream = req
    "ListTagsForStream"
    "fixture/ListTagsForStream"

testAddTagsToStream :: AddTagsToStream -> TestTree
testAddTagsToStream = req
    "AddTagsToStream"
    "fixture/AddTagsToStream"

testPutRecords :: PutRecords -> TestTree
testPutRecords = req
    "PutRecords"
    "fixture/PutRecords"

testDeleteStream :: DeleteStream -> TestTree
testDeleteStream = req
    "DeleteStream"
    "fixture/DeleteStream"

testRemoveTagsFromStream :: RemoveTagsFromStream -> TestTree
testRemoveTagsFromStream = req
    "RemoveTagsFromStream"
    "fixture/RemoveTagsFromStream"

testListStreams :: ListStreams -> TestTree
testListStreams = req
    "ListStreams"
    "fixture/ListStreams"

testCreateStream :: CreateStream -> TestTree
testCreateStream = req
    "CreateStream"
    "fixture/CreateStream"

testSplitShard :: SplitShard -> TestTree
testSplitShard = req
    "SplitShard"
    "fixture/SplitShard"

testDescribeStream :: DescribeStream -> TestTree
testDescribeStream = req
    "DescribeStream"
    "fixture/DescribeStream"

-- Responses

testPutRecordResponse :: PutRecordResponse -> TestTree
testPutRecordResponse = res
    "PutRecordResponse"
    "fixture/PutRecordResponse"
    (Proxy :: Proxy PutRecord)

testMergeShardsResponse :: MergeShardsResponse -> TestTree
testMergeShardsResponse = res
    "MergeShardsResponse"
    "fixture/MergeShardsResponse"
    (Proxy :: Proxy MergeShards)

testGetRecordsResponse :: GetRecordsResponse -> TestTree
testGetRecordsResponse = res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse"
    (Proxy :: Proxy GetRecords)

testGetShardIteratorResponse :: GetShardIteratorResponse -> TestTree
testGetShardIteratorResponse = res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse"
    (Proxy :: Proxy GetShardIterator)

testListTagsForStreamResponse :: ListTagsForStreamResponse -> TestTree
testListTagsForStreamResponse = res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse"
    (Proxy :: Proxy ListTagsForStream)

testAddTagsToStreamResponse :: AddTagsToStreamResponse -> TestTree
testAddTagsToStreamResponse = res
    "AddTagsToStreamResponse"
    "fixture/AddTagsToStreamResponse"
    (Proxy :: Proxy AddTagsToStream)

testPutRecordsResponse :: PutRecordsResponse -> TestTree
testPutRecordsResponse = res
    "PutRecordsResponse"
    "fixture/PutRecordsResponse"
    (Proxy :: Proxy PutRecords)

testDeleteStreamResponse :: DeleteStreamResponse -> TestTree
testDeleteStreamResponse = res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse"
    (Proxy :: Proxy DeleteStream)

testRemoveTagsFromStreamResponse :: RemoveTagsFromStreamResponse -> TestTree
testRemoveTagsFromStreamResponse = res
    "RemoveTagsFromStreamResponse"
    "fixture/RemoveTagsFromStreamResponse"
    (Proxy :: Proxy RemoveTagsFromStream)

testListStreamsResponse :: ListStreamsResponse -> TestTree
testListStreamsResponse = res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse"
    (Proxy :: Proxy ListStreams)

testCreateStreamResponse :: CreateStreamResponse -> TestTree
testCreateStreamResponse = res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse"
    (Proxy :: Proxy CreateStream)

testSplitShardResponse :: SplitShardResponse -> TestTree
testSplitShardResponse = res
    "SplitShardResponse"
    "fixture/SplitShardResponse"
    (Proxy :: Proxy SplitShard)

testDescribeStreamResponse :: DescribeStreamResponse -> TestTree
testDescribeStreamResponse = res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse"
    (Proxy :: Proxy DescribeStream)
