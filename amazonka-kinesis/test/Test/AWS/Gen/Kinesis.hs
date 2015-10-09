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
--         , testDecreaseStreamRetentionPeriod $
--             decreaseStreamRetentionPeriod
--
--         , testMergeShards $
--             mergeShards
--
--         , testGetShardIterator $
--             getShardIterator
--
--         , testGetRecords $
--             getRecords
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
--         , testIncreaseStreamRetentionPeriod $
--             increaseStreamRetentionPeriod
--
--         , testDescribeStream $
--             describeStream
--
--           ]

--     , testGroup "response"
--         [ testPutRecordResponse $
--             putRecordResponse
--
--         , testDecreaseStreamRetentionPeriodResponse $
--             decreaseStreamRetentionPeriodResponse
--
--         , testMergeShardsResponse $
--             mergeShardsResponse
--
--         , testGetShardIteratorResponse $
--             getShardIteratorResponse
--
--         , testGetRecordsResponse $
--             getRecordsResponse
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
--         , testIncreaseStreamRetentionPeriodResponse $
--             increaseStreamRetentionPeriodResponse
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
    "fixture/PutRecord.yaml"

testDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriod -> TestTree
testDecreaseStreamRetentionPeriod = req
    "DecreaseStreamRetentionPeriod"
    "fixture/DecreaseStreamRetentionPeriod.yaml"

testMergeShards :: MergeShards -> TestTree
testMergeShards = req
    "MergeShards"
    "fixture/MergeShards.yaml"

testGetShardIterator :: GetShardIterator -> TestTree
testGetShardIterator = req
    "GetShardIterator"
    "fixture/GetShardIterator.yaml"

testGetRecords :: GetRecords -> TestTree
testGetRecords = req
    "GetRecords"
    "fixture/GetRecords.yaml"

testListTagsForStream :: ListTagsForStream -> TestTree
testListTagsForStream = req
    "ListTagsForStream"
    "fixture/ListTagsForStream.yaml"

testAddTagsToStream :: AddTagsToStream -> TestTree
testAddTagsToStream = req
    "AddTagsToStream"
    "fixture/AddTagsToStream.yaml"

testPutRecords :: PutRecords -> TestTree
testPutRecords = req
    "PutRecords"
    "fixture/PutRecords.yaml"

testDeleteStream :: DeleteStream -> TestTree
testDeleteStream = req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

testRemoveTagsFromStream :: RemoveTagsFromStream -> TestTree
testRemoveTagsFromStream = req
    "RemoveTagsFromStream"
    "fixture/RemoveTagsFromStream.yaml"

testListStreams :: ListStreams -> TestTree
testListStreams = req
    "ListStreams"
    "fixture/ListStreams.yaml"

testCreateStream :: CreateStream -> TestTree
testCreateStream = req
    "CreateStream"
    "fixture/CreateStream.yaml"

testSplitShard :: SplitShard -> TestTree
testSplitShard = req
    "SplitShard"
    "fixture/SplitShard.yaml"

testIncreaseStreamRetentionPeriod :: IncreaseStreamRetentionPeriod -> TestTree
testIncreaseStreamRetentionPeriod = req
    "IncreaseStreamRetentionPeriod"
    "fixture/IncreaseStreamRetentionPeriod.yaml"

testDescribeStream :: DescribeStream -> TestTree
testDescribeStream = req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

-- Responses

testPutRecordResponse :: PutRecordResponse -> TestTree
testPutRecordResponse = res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    kinesis
    (Proxy :: Proxy PutRecord)

testDecreaseStreamRetentionPeriodResponse :: DecreaseStreamRetentionPeriodResponse -> TestTree
testDecreaseStreamRetentionPeriodResponse = res
    "DecreaseStreamRetentionPeriodResponse"
    "fixture/DecreaseStreamRetentionPeriodResponse.proto"
    kinesis
    (Proxy :: Proxy DecreaseStreamRetentionPeriod)

testMergeShardsResponse :: MergeShardsResponse -> TestTree
testMergeShardsResponse = res
    "MergeShardsResponse"
    "fixture/MergeShardsResponse.proto"
    kinesis
    (Proxy :: Proxy MergeShards)

testGetShardIteratorResponse :: GetShardIteratorResponse -> TestTree
testGetShardIteratorResponse = res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse.proto"
    kinesis
    (Proxy :: Proxy GetShardIterator)

testGetRecordsResponse :: GetRecordsResponse -> TestTree
testGetRecordsResponse = res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse.proto"
    kinesis
    (Proxy :: Proxy GetRecords)

testListTagsForStreamResponse :: ListTagsForStreamResponse -> TestTree
testListTagsForStreamResponse = res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    kinesis
    (Proxy :: Proxy ListTagsForStream)

testAddTagsToStreamResponse :: AddTagsToStreamResponse -> TestTree
testAddTagsToStreamResponse = res
    "AddTagsToStreamResponse"
    "fixture/AddTagsToStreamResponse.proto"
    kinesis
    (Proxy :: Proxy AddTagsToStream)

testPutRecordsResponse :: PutRecordsResponse -> TestTree
testPutRecordsResponse = res
    "PutRecordsResponse"
    "fixture/PutRecordsResponse.proto"
    kinesis
    (Proxy :: Proxy PutRecords)

testDeleteStreamResponse :: DeleteStreamResponse -> TestTree
testDeleteStreamResponse = res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    kinesis
    (Proxy :: Proxy DeleteStream)

testRemoveTagsFromStreamResponse :: RemoveTagsFromStreamResponse -> TestTree
testRemoveTagsFromStreamResponse = res
    "RemoveTagsFromStreamResponse"
    "fixture/RemoveTagsFromStreamResponse.proto"
    kinesis
    (Proxy :: Proxy RemoveTagsFromStream)

testListStreamsResponse :: ListStreamsResponse -> TestTree
testListStreamsResponse = res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    kinesis
    (Proxy :: Proxy ListStreams)

testCreateStreamResponse :: CreateStreamResponse -> TestTree
testCreateStreamResponse = res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    kinesis
    (Proxy :: Proxy CreateStream)

testSplitShardResponse :: SplitShardResponse -> TestTree
testSplitShardResponse = res
    "SplitShardResponse"
    "fixture/SplitShardResponse.proto"
    kinesis
    (Proxy :: Proxy SplitShard)

testIncreaseStreamRetentionPeriodResponse :: IncreaseStreamRetentionPeriodResponse -> TestTree
testIncreaseStreamRetentionPeriodResponse = res
    "IncreaseStreamRetentionPeriodResponse"
    "fixture/IncreaseStreamRetentionPeriodResponse.proto"
    kinesis
    (Proxy :: Proxy IncreaseStreamRetentionPeriod)

testDescribeStreamResponse :: DescribeStreamResponse -> TestTree
testDescribeStreamResponse = res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    kinesis
    (Proxy :: Proxy DescribeStream)
