{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Kinesis
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Kinesis where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Kinesis

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
testPutRecord = undefined

testMergeShards :: MergeShards -> TestTree
testMergeShards = undefined

testGetRecords :: GetRecords -> TestTree
testGetRecords = undefined

testGetShardIterator :: GetShardIterator -> TestTree
testGetShardIterator = undefined

testListTagsForStream :: ListTagsForStream -> TestTree
testListTagsForStream = undefined

testAddTagsToStream :: AddTagsToStream -> TestTree
testAddTagsToStream = undefined

testPutRecords :: PutRecords -> TestTree
testPutRecords = undefined

testDeleteStream :: DeleteStream -> TestTree
testDeleteStream = undefined

testRemoveTagsFromStream :: RemoveTagsFromStream -> TestTree
testRemoveTagsFromStream = undefined

testListStreams :: ListStreams -> TestTree
testListStreams = undefined

testCreateStream :: CreateStream -> TestTree
testCreateStream = undefined

testSplitShard :: SplitShard -> TestTree
testSplitShard = undefined

testDescribeStream :: DescribeStream -> TestTree
testDescribeStream = undefined

-- Responses

testPutRecordResponse :: PutRecordResponse -> TestTree
testPutRecordResponse = resp
    "PutRecordResponse"
    "fixture/PutRecordResponse"
    (Proxy :: Proxy PutRecord)

testMergeShardsResponse :: MergeShardsResponse -> TestTree
testMergeShardsResponse = resp
    "MergeShardsResponse"
    "fixture/MergeShardsResponse"
    (Proxy :: Proxy MergeShards)

testGetRecordsResponse :: GetRecordsResponse -> TestTree
testGetRecordsResponse = resp
    "GetRecordsResponse"
    "fixture/GetRecordsResponse"
    (Proxy :: Proxy GetRecords)

testGetShardIteratorResponse :: GetShardIteratorResponse -> TestTree
testGetShardIteratorResponse = resp
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse"
    (Proxy :: Proxy GetShardIterator)

testListTagsForStreamResponse :: ListTagsForStreamResponse -> TestTree
testListTagsForStreamResponse = resp
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse"
    (Proxy :: Proxy ListTagsForStream)

testAddTagsToStreamResponse :: AddTagsToStreamResponse -> TestTree
testAddTagsToStreamResponse = resp
    "AddTagsToStreamResponse"
    "fixture/AddTagsToStreamResponse"
    (Proxy :: Proxy AddTagsToStream)

testPutRecordsResponse :: PutRecordsResponse -> TestTree
testPutRecordsResponse = resp
    "PutRecordsResponse"
    "fixture/PutRecordsResponse"
    (Proxy :: Proxy PutRecords)

testDeleteStreamResponse :: DeleteStreamResponse -> TestTree
testDeleteStreamResponse = resp
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse"
    (Proxy :: Proxy DeleteStream)

testRemoveTagsFromStreamResponse :: RemoveTagsFromStreamResponse -> TestTree
testRemoveTagsFromStreamResponse = resp
    "RemoveTagsFromStreamResponse"
    "fixture/RemoveTagsFromStreamResponse"
    (Proxy :: Proxy RemoveTagsFromStream)

testListStreamsResponse :: ListStreamsResponse -> TestTree
testListStreamsResponse = resp
    "ListStreamsResponse"
    "fixture/ListStreamsResponse"
    (Proxy :: Proxy ListStreams)

testCreateStreamResponse :: CreateStreamResponse -> TestTree
testCreateStreamResponse = resp
    "CreateStreamResponse"
    "fixture/CreateStreamResponse"
    (Proxy :: Proxy CreateStream)

testSplitShardResponse :: SplitShardResponse -> TestTree
testSplitShardResponse = resp
    "SplitShardResponse"
    "fixture/SplitShardResponse"
    (Proxy :: Proxy SplitShard)

testDescribeStreamResponse :: DescribeStreamResponse -> TestTree
testDescribeStreamResponse = resp
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse"
    (Proxy :: Proxy DescribeStream)

instance Out AddTagsToStream
instance Out AddTagsToStreamResponse
instance Out CreateStream
instance Out CreateStreamResponse
instance Out DeleteStream
instance Out DeleteStreamResponse
instance Out DescribeStream
instance Out DescribeStreamResponse
instance Out GetRecords
instance Out GetRecordsResponse
instance Out GetShardIterator
instance Out GetShardIteratorResponse
instance Out HashKeyRange
instance Out ListStreams
instance Out ListStreamsResponse
instance Out ListTagsForStream
instance Out ListTagsForStreamResponse
instance Out MergeShards
instance Out MergeShardsResponse
instance Out PutRecord
instance Out PutRecordResponse
instance Out PutRecords
instance Out PutRecordsRequestEntry
instance Out PutRecordsResponse
instance Out PutRecordsResultEntry
instance Out Record
instance Out RemoveTagsFromStream
instance Out RemoveTagsFromStreamResponse
instance Out SequenceNumberRange
instance Out Shard
instance Out ShardIteratorType
instance Out SplitShard
instance Out SplitShardResponse
instance Out StreamDescription
instance Out StreamStatus
instance Out Tag
