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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Kinesis

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ addTagsToStreamTest $
--             addTagsToStream
--
--         , createStreamTest $
--             createStream
--
--         , deleteStreamTest $
--             deleteStream
--
--         , describeStreamTest $
--             describeStream
--
--         , getRecordsTest $
--             getRecords
--
--         , getShardIteratorTest $
--             getShardIterator
--
--         , listStreamsTest $
--             listStreams
--
--         , listTagsForStreamTest $
--             listTagsForStream
--
--         , mergeShardsTest $
--             mergeShards
--
--         , putRecordTest $
--             putRecord
--
--         , putRecordsTest $
--             putRecords
--
--         , removeTagsFromStreamTest $
--             removeTagsFromStream
--
--         , splitShardTest $
--             splitShard
--
--           ]

--     , testGroup "response"
--         [ addTagsToStreamResponseTest $
--             addTagsToStreamResponse
--
--         , createStreamResponseTest $
--             createStreamResponse
--
--         , deleteStreamResponseTest $
--             deleteStreamResponse
--
--         , describeStreamResponseTest $
--             describeStreamResponse
--
--         , getRecordsResponseTest $
--             getRecordsResponse
--
--         , getShardIteratorResponseTest $
--             getShardIteratorResponse
--
--         , listStreamsResponseTest $
--             listStreamsResponse
--
--         , listTagsForStreamResponseTest $
--             listTagsForStreamResponse
--
--         , mergeShardsResponseTest $
--             mergeShardsResponse
--
--         , putRecordResponseTest $
--             putRecordResponse
--
--         , putRecordsResponseTest $
--             putRecordsResponse
--
--         , removeTagsFromStreamResponseTest $
--             removeTagsFromStreamResponse
--
--         , splitShardResponseTest $
--             splitShardResponse
--
--           ]
--     ]

-- Requests

addTagsToStreamTest :: AddTagsToStream -> TestTree
addTagsToStreamTest = undefined

createStreamTest :: CreateStream -> TestTree
createStreamTest = undefined

deleteStreamTest :: DeleteStream -> TestTree
deleteStreamTest = undefined

describeStreamTest :: DescribeStream -> TestTree
describeStreamTest = undefined

getRecordsTest :: GetRecords -> TestTree
getRecordsTest = undefined

getShardIteratorTest :: GetShardIterator -> TestTree
getShardIteratorTest = undefined

listStreamsTest :: ListStreams -> TestTree
listStreamsTest = undefined

listTagsForStreamTest :: ListTagsForStream -> TestTree
listTagsForStreamTest = undefined

mergeShardsTest :: MergeShards -> TestTree
mergeShardsTest = undefined

putRecordTest :: PutRecord -> TestTree
putRecordTest = undefined

putRecordsTest :: PutRecords -> TestTree
putRecordsTest = undefined

removeTagsFromStreamTest :: RemoveTagsFromStream -> TestTree
removeTagsFromStreamTest = undefined

splitShardTest :: SplitShard -> TestTree
splitShardTest = undefined

-- Responses

addTagsToStreamResponseTest :: AddTagsToStreamResponse -> TestTree
addTagsToStreamResponseTest = resp
    "addTagsToStreamResponse"
    "fixture/AddTagsToStreamResponse"
    (Proxy :: Proxy AddTagsToStream)

createStreamResponseTest :: CreateStreamResponse -> TestTree
createStreamResponseTest = resp
    "createStreamResponse"
    "fixture/CreateStreamResponse"
    (Proxy :: Proxy CreateStream)

deleteStreamResponseTest :: DeleteStreamResponse -> TestTree
deleteStreamResponseTest = resp
    "deleteStreamResponse"
    "fixture/DeleteStreamResponse"
    (Proxy :: Proxy DeleteStream)

describeStreamResponseTest :: DescribeStreamResponse -> TestTree
describeStreamResponseTest = resp
    "describeStreamResponse"
    "fixture/DescribeStreamResponse"
    (Proxy :: Proxy DescribeStream)

getRecordsResponseTest :: GetRecordsResponse -> TestTree
getRecordsResponseTest = resp
    "getRecordsResponse"
    "fixture/GetRecordsResponse"
    (Proxy :: Proxy GetRecords)

getShardIteratorResponseTest :: GetShardIteratorResponse -> TestTree
getShardIteratorResponseTest = resp
    "getShardIteratorResponse"
    "fixture/GetShardIteratorResponse"
    (Proxy :: Proxy GetShardIterator)

listStreamsResponseTest :: ListStreamsResponse -> TestTree
listStreamsResponseTest = resp
    "listStreamsResponse"
    "fixture/ListStreamsResponse"
    (Proxy :: Proxy ListStreams)

listTagsForStreamResponseTest :: ListTagsForStreamResponse -> TestTree
listTagsForStreamResponseTest = resp
    "listTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse"
    (Proxy :: Proxy ListTagsForStream)

mergeShardsResponseTest :: MergeShardsResponse -> TestTree
mergeShardsResponseTest = resp
    "mergeShardsResponse"
    "fixture/MergeShardsResponse"
    (Proxy :: Proxy MergeShards)

putRecordResponseTest :: PutRecordResponse -> TestTree
putRecordResponseTest = resp
    "putRecordResponse"
    "fixture/PutRecordResponse"
    (Proxy :: Proxy PutRecord)

putRecordsResponseTest :: PutRecordsResponse -> TestTree
putRecordsResponseTest = resp
    "putRecordsResponse"
    "fixture/PutRecordsResponse"
    (Proxy :: Proxy PutRecords)

removeTagsFromStreamResponseTest :: RemoveTagsFromStreamResponse -> TestTree
removeTagsFromStreamResponseTest = resp
    "removeTagsFromStreamResponse"
    "fixture/RemoveTagsFromStreamResponse"
    (Proxy :: Proxy RemoveTagsFromStream)

splitShardResponseTest :: SplitShardResponse -> TestTree
splitShardResponseTest = resp
    "splitShardResponse"
    "fixture/SplitShardResponse"
    (Proxy :: Proxy SplitShard)
