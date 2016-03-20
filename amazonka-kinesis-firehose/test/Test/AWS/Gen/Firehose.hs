{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Firehose
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Firehose where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Firehose
import Test.AWS.Firehose.Internal

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
--         , testUpdateDestination $
--             updateDestination
--
--         , testPutRecordBatch $
--             putRecordBatch
--
--         , testCreateDeliveryStream $
--             createDeliveryStream
--
--         , testDescribeDeliveryStream $
--             describeDeliveryStream
--
--         , testListDeliveryStreams $
--             listDeliveryStreams
--
--         , testDeleteDeliveryStream $
--             deleteDeliveryStream
--
--           ]

--     , testGroup "response"
--         [ testPutRecordResponse $
--             putRecordResponse
--
--         , testUpdateDestinationResponse $
--             updateDestinationResponse
--
--         , testPutRecordBatchResponse $
--             putRecordBatchResponse
--
--         , testCreateDeliveryStreamResponse $
--             createDeliveryStreamResponse
--
--         , testDescribeDeliveryStreamResponse $
--             describeDeliveryStreamResponse
--
--         , testListDeliveryStreamsResponse $
--             listDeliveryStreamsResponse
--
--         , testDeleteDeliveryStreamResponse $
--             deleteDeliveryStreamResponse
--
--           ]
--     ]

-- Requests

testPutRecord :: PutRecord -> TestTree
testPutRecord = req
    "PutRecord"
    "fixture/PutRecord.yaml"

testUpdateDestination :: UpdateDestination -> TestTree
testUpdateDestination = req
    "UpdateDestination"
    "fixture/UpdateDestination.yaml"

testPutRecordBatch :: PutRecordBatch -> TestTree
testPutRecordBatch = req
    "PutRecordBatch"
    "fixture/PutRecordBatch.yaml"

testCreateDeliveryStream :: CreateDeliveryStream -> TestTree
testCreateDeliveryStream = req
    "CreateDeliveryStream"
    "fixture/CreateDeliveryStream.yaml"

testDescribeDeliveryStream :: DescribeDeliveryStream -> TestTree
testDescribeDeliveryStream = req
    "DescribeDeliveryStream"
    "fixture/DescribeDeliveryStream.yaml"

testListDeliveryStreams :: ListDeliveryStreams -> TestTree
testListDeliveryStreams = req
    "ListDeliveryStreams"
    "fixture/ListDeliveryStreams.yaml"

testDeleteDeliveryStream :: DeleteDeliveryStream -> TestTree
testDeleteDeliveryStream = req
    "DeleteDeliveryStream"
    "fixture/DeleteDeliveryStream.yaml"

-- Responses

testPutRecordResponse :: PutRecordResponse -> TestTree
testPutRecordResponse = res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    firehose
    (Proxy :: Proxy PutRecord)

testUpdateDestinationResponse :: UpdateDestinationResponse -> TestTree
testUpdateDestinationResponse = res
    "UpdateDestinationResponse"
    "fixture/UpdateDestinationResponse.proto"
    firehose
    (Proxy :: Proxy UpdateDestination)

testPutRecordBatchResponse :: PutRecordBatchResponse -> TestTree
testPutRecordBatchResponse = res
    "PutRecordBatchResponse"
    "fixture/PutRecordBatchResponse.proto"
    firehose
    (Proxy :: Proxy PutRecordBatch)

testCreateDeliveryStreamResponse :: CreateDeliveryStreamResponse -> TestTree
testCreateDeliveryStreamResponse = res
    "CreateDeliveryStreamResponse"
    "fixture/CreateDeliveryStreamResponse.proto"
    firehose
    (Proxy :: Proxy CreateDeliveryStream)

testDescribeDeliveryStreamResponse :: DescribeDeliveryStreamResponse -> TestTree
testDescribeDeliveryStreamResponse = res
    "DescribeDeliveryStreamResponse"
    "fixture/DescribeDeliveryStreamResponse.proto"
    firehose
    (Proxy :: Proxy DescribeDeliveryStream)

testListDeliveryStreamsResponse :: ListDeliveryStreamsResponse -> TestTree
testListDeliveryStreamsResponse = res
    "ListDeliveryStreamsResponse"
    "fixture/ListDeliveryStreamsResponse.proto"
    firehose
    (Proxy :: Proxy ListDeliveryStreams)

testDeleteDeliveryStreamResponse :: DeleteDeliveryStreamResponse -> TestTree
testDeleteDeliveryStreamResponse = res
    "DeleteDeliveryStreamResponse"
    "fixture/DeleteDeliveryStreamResponse.proto"
    firehose
    (Proxy :: Proxy DeleteDeliveryStream)
