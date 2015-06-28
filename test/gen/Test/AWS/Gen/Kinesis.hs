-- Module      : Test.AWS.Gen.Kinesis
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.Kinesis where

import           Data.Proxy
import           Network.AWS.Kinesis
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ putRecordTest $
--             putRecord
--
--         , mergeShardsTest $
--             mergeShards
--
--         , getRecordsTest $
--             getRecords
--
--         , getShardIteratorTest $
--             getShardIterator
--
--         , listTagsForStreamTest $
--             listTagsForStream
--
--         , addTagsToStreamTest $
--             addTagsToStream
--
--         , putRecordsTest $
--             putRecords
--
--         , deleteStreamTest $
--             deleteStream
--
--         , removeTagsFromStreamTest $
--             removeTagsFromStream
--
--         , listStreamsTest $
--             listStreams
--
--         , createStreamTest $
--             createStream
--
--         , splitShardTest $
--             splitShard
--
--         , describeStreamTest $
--             describeStream
--
--           ]

--     , testGroup "response"
--         [ putRecordResponseTest $
--             putRecordResponse
--
--         , mergeShardsResponseTest $
--             mergeShardsResponse
--
--         , getRecordsResponseTest $
--             getRecordsResponse
--
--         , getShardIteratorResponseTest $
--             getShardIteratorResponse
--
--         , listTagsForStreamResponseTest $
--             listTagsForStreamResponse
--
--         , addTagsToStreamResponseTest $
--             addTagsToStreamResponse
--
--         , putRecordsResponseTest $
--             putRecordsResponse
--
--         , deleteStreamResponseTest $
--             deleteStreamResponse
--
--         , removeTagsFromStreamResponseTest $
--             removeTagsFromStreamResponse
--
--         , listStreamsResponseTest $
--             listStreamsResponse
--
--         , createStreamResponseTest $
--             createStreamResponse
--
--         , splitShardResponseTest $
--             splitShardResponse
--
--         , describeStreamResponseTest $
--             describeStreamResponse
--
--           ]
--     ]

-- Requests

putRecordTest :: PutRecord -> TestTree
putRecordTest = undefined

mergeShardsTest :: MergeShards -> TestTree
mergeShardsTest = undefined

getRecordsTest :: GetRecords -> TestTree
getRecordsTest = undefined

getShardIteratorTest :: GetShardIterator -> TestTree
getShardIteratorTest = undefined

listTagsForStreamTest :: ListTagsForStream -> TestTree
listTagsForStreamTest = undefined

addTagsToStreamTest :: AddTagsToStream -> TestTree
addTagsToStreamTest = undefined

putRecordsTest :: PutRecords -> TestTree
putRecordsTest = undefined

deleteStreamTest :: DeleteStream -> TestTree
deleteStreamTest = undefined

removeTagsFromStreamTest :: RemoveTagsFromStream -> TestTree
removeTagsFromStreamTest = undefined

listStreamsTest :: ListStreams -> TestTree
listStreamsTest = undefined

createStreamTest :: CreateStream -> TestTree
createStreamTest = undefined

splitShardTest :: SplitShard -> TestTree
splitShardTest = undefined

describeStreamTest :: DescribeStream -> TestTree
describeStreamTest = undefined

-- Responses

putRecordResponseTest :: PutRecordResponse -> TestTree
putRecordResponseTest = resp
    "PutRecordResponse"
    "fixture/Kinesis/PutRecordResponse"
    (Proxy :: Proxy PutRecord)

mergeShardsResponseTest :: MergeShardsResponse -> TestTree
mergeShardsResponseTest = resp
    "MergeShardsResponse"
    "fixture/Kinesis/MergeShardsResponse"
    (Proxy :: Proxy MergeShards)

getRecordsResponseTest :: GetRecordsResponse -> TestTree
getRecordsResponseTest = resp
    "GetRecordsResponse"
    "fixture/Kinesis/GetRecordsResponse"
    (Proxy :: Proxy GetRecords)

getShardIteratorResponseTest :: GetShardIteratorResponse -> TestTree
getShardIteratorResponseTest = resp
    "GetShardIteratorResponse"
    "fixture/Kinesis/GetShardIteratorResponse"
    (Proxy :: Proxy GetShardIterator)

listTagsForStreamResponseTest :: ListTagsForStreamResponse -> TestTree
listTagsForStreamResponseTest = resp
    "ListTagsForStreamResponse"
    "fixture/Kinesis/ListTagsForStreamResponse"
    (Proxy :: Proxy ListTagsForStream)

addTagsToStreamResponseTest :: AddTagsToStreamResponse -> TestTree
addTagsToStreamResponseTest = resp
    "AddTagsToStreamResponse"
    "fixture/Kinesis/AddTagsToStreamResponse"
    (Proxy :: Proxy AddTagsToStream)

putRecordsResponseTest :: PutRecordsResponse -> TestTree
putRecordsResponseTest = resp
    "PutRecordsResponse"
    "fixture/Kinesis/PutRecordsResponse"
    (Proxy :: Proxy PutRecords)

deleteStreamResponseTest :: DeleteStreamResponse -> TestTree
deleteStreamResponseTest = resp
    "DeleteStreamResponse"
    "fixture/Kinesis/DeleteStreamResponse"
    (Proxy :: Proxy DeleteStream)

removeTagsFromStreamResponseTest :: RemoveTagsFromStreamResponse -> TestTree
removeTagsFromStreamResponseTest = resp
    "RemoveTagsFromStreamResponse"
    "fixture/Kinesis/RemoveTagsFromStreamResponse"
    (Proxy :: Proxy RemoveTagsFromStream)

listStreamsResponseTest :: ListStreamsResponse -> TestTree
listStreamsResponseTest = resp
    "ListStreamsResponse"
    "fixture/Kinesis/ListStreamsResponse"
    (Proxy :: Proxy ListStreams)

createStreamResponseTest :: CreateStreamResponse -> TestTree
createStreamResponseTest = resp
    "CreateStreamResponse"
    "fixture/Kinesis/CreateStreamResponse"
    (Proxy :: Proxy CreateStream)

splitShardResponseTest :: SplitShardResponse -> TestTree
splitShardResponseTest = resp
    "SplitShardResponse"
    "fixture/Kinesis/SplitShardResponse"
    (Proxy :: Proxy SplitShard)

describeStreamResponseTest :: DescribeStreamResponse -> TestTree
describeStreamResponseTest = resp
    "DescribeStreamResponse"
    "fixture/Kinesis/DescribeStreamResponse"
    (Proxy :: Proxy DescribeStream)
