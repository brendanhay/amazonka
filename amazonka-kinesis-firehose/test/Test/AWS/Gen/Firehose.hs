{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Firehose
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Firehose where

import Data.Proxy
import Network.AWS.Firehose
import Test.AWS.Firehose.Internal
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
--         [ requestPutRecord $
--             putRecord
--
--         , requestGetKinesisStream $
--             getKinesisStream
--
--         , requestUpdateDestination $
--             updateDestination
--
--         , requestPutRecordBatch $
--             putRecordBatch
--
--         , requestCreateDeliveryStream $
--             createDeliveryStream
--
--         , requestDescribeDeliveryStream $
--             describeDeliveryStream
--
--         , requestListDeliveryStreams $
--             listDeliveryStreams
--
--         , requestDeleteDeliveryStream $
--             deleteDeliveryStream
--
--           ]

--     , testGroup "response"
--         [ responsePutRecord $
--             putRecordResponse
--
--         , responseGetKinesisStream $
--             getKinesisStreamResponse
--
--         , responseUpdateDestination $
--             updateDestinationResponse
--
--         , responsePutRecordBatch $
--             putRecordBatchResponse
--
--         , responseCreateDeliveryStream $
--             createDeliveryStreamResponse
--
--         , responseDescribeDeliveryStream $
--             describeDeliveryStreamResponse
--
--         , responseListDeliveryStreams $
--             listDeliveryStreamsResponse
--
--         , responseDeleteDeliveryStream $
--             deleteDeliveryStreamResponse
--
--           ]
--     ]

-- Requests

requestPutRecord :: PutRecord -> TestTree
requestPutRecord = req
    "PutRecord"
    "fixture/PutRecord.yaml"

requestGetKinesisStream :: GetKinesisStream -> TestTree
requestGetKinesisStream = req
    "GetKinesisStream"
    "fixture/GetKinesisStream.yaml"

requestUpdateDestination :: UpdateDestination -> TestTree
requestUpdateDestination = req
    "UpdateDestination"
    "fixture/UpdateDestination.yaml"

requestPutRecordBatch :: PutRecordBatch -> TestTree
requestPutRecordBatch = req
    "PutRecordBatch"
    "fixture/PutRecordBatch.yaml"

requestCreateDeliveryStream :: CreateDeliveryStream -> TestTree
requestCreateDeliveryStream = req
    "CreateDeliveryStream"
    "fixture/CreateDeliveryStream.yaml"

requestDescribeDeliveryStream :: DescribeDeliveryStream -> TestTree
requestDescribeDeliveryStream = req
    "DescribeDeliveryStream"
    "fixture/DescribeDeliveryStream.yaml"

requestListDeliveryStreams :: ListDeliveryStreams -> TestTree
requestListDeliveryStreams = req
    "ListDeliveryStreams"
    "fixture/ListDeliveryStreams.yaml"

requestDeleteDeliveryStream :: DeleteDeliveryStream -> TestTree
requestDeleteDeliveryStream = req
    "DeleteDeliveryStream"
    "fixture/DeleteDeliveryStream.yaml"

-- Responses

responsePutRecord :: PutRecordResponse -> TestTree
responsePutRecord = res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    firehose
    (Proxy :: Proxy PutRecord)

responseGetKinesisStream :: GetKinesisStreamResponse -> TestTree
responseGetKinesisStream = res
    "GetKinesisStreamResponse"
    "fixture/GetKinesisStreamResponse.proto"
    firehose
    (Proxy :: Proxy GetKinesisStream)

responseUpdateDestination :: UpdateDestinationResponse -> TestTree
responseUpdateDestination = res
    "UpdateDestinationResponse"
    "fixture/UpdateDestinationResponse.proto"
    firehose
    (Proxy :: Proxy UpdateDestination)

responsePutRecordBatch :: PutRecordBatchResponse -> TestTree
responsePutRecordBatch = res
    "PutRecordBatchResponse"
    "fixture/PutRecordBatchResponse.proto"
    firehose
    (Proxy :: Proxy PutRecordBatch)

responseCreateDeliveryStream :: CreateDeliveryStreamResponse -> TestTree
responseCreateDeliveryStream = res
    "CreateDeliveryStreamResponse"
    "fixture/CreateDeliveryStreamResponse.proto"
    firehose
    (Proxy :: Proxy CreateDeliveryStream)

responseDescribeDeliveryStream :: DescribeDeliveryStreamResponse -> TestTree
responseDescribeDeliveryStream = res
    "DescribeDeliveryStreamResponse"
    "fixture/DescribeDeliveryStreamResponse.proto"
    firehose
    (Proxy :: Proxy DescribeDeliveryStream)

responseListDeliveryStreams :: ListDeliveryStreamsResponse -> TestTree
responseListDeliveryStreams = res
    "ListDeliveryStreamsResponse"
    "fixture/ListDeliveryStreamsResponse.proto"
    firehose
    (Proxy :: Proxy ListDeliveryStreams)

responseDeleteDeliveryStream :: DeleteDeliveryStreamResponse -> TestTree
responseDeleteDeliveryStream = res
    "DeleteDeliveryStreamResponse"
    "fixture/DeleteDeliveryStreamResponse.proto"
    firehose
    (Proxy :: Proxy DeleteDeliveryStream)
