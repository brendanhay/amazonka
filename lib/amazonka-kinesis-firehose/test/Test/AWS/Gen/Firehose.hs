{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Firehose
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkPutRecord
--
--         , requestStopDeliveryStreamEncryption $
--             mkStopDeliveryStreamEncryption
--
--         , requestTagDeliveryStream $
--             mkTagDeliveryStream
--
--         , requestUpdateDestination $
--             mkUpdateDestination
--
--         , requestPutRecordBatch $
--             mkPutRecordBatch
--
--         , requestUntagDeliveryStream $
--             mkUntagDeliveryStream
--
--         , requestCreateDeliveryStream $
--             mkCreateDeliveryStream
--
--         , requestStartDeliveryStreamEncryption $
--             mkStartDeliveryStreamEncryption
--
--         , requestDescribeDeliveryStream $
--             mkDescribeDeliveryStream
--
--         , requestListTagsForDeliveryStream $
--             mkListTagsForDeliveryStream
--
--         , requestListDeliveryStreams $
--             mkListDeliveryStreams
--
--         , requestDeleteDeliveryStream $
--             mkDeleteDeliveryStream
--
--           ]

--     , testGroup "response"
--         [ responsePutRecord $
--             mkPutRecordResponse
--
--         , responseStopDeliveryStreamEncryption $
--             mkStopDeliveryStreamEncryptionResponse
--
--         , responseTagDeliveryStream $
--             mkTagDeliveryStreamResponse
--
--         , responseUpdateDestination $
--             mkUpdateDestinationResponse
--
--         , responsePutRecordBatch $
--             mkPutRecordBatchResponse
--
--         , responseUntagDeliveryStream $
--             mkUntagDeliveryStreamResponse
--
--         , responseCreateDeliveryStream $
--             mkCreateDeliveryStreamResponse
--
--         , responseStartDeliveryStreamEncryption $
--             mkStartDeliveryStreamEncryptionResponse
--
--         , responseDescribeDeliveryStream $
--             mkDescribeDeliveryStreamResponse
--
--         , responseListTagsForDeliveryStream $
--             mkListTagsForDeliveryStreamResponse
--
--         , responseListDeliveryStreams $
--             mkListDeliveryStreamsResponse
--
--         , responseDeleteDeliveryStream $
--             mkDeleteDeliveryStreamResponse
--
--           ]
--     ]

-- Requests

requestPutRecord :: PutRecord -> TestTree
requestPutRecord =
  req
    "PutRecord"
    "fixture/PutRecord.yaml"

requestStopDeliveryStreamEncryption :: StopDeliveryStreamEncryption -> TestTree
requestStopDeliveryStreamEncryption =
  req
    "StopDeliveryStreamEncryption"
    "fixture/StopDeliveryStreamEncryption.yaml"

requestTagDeliveryStream :: TagDeliveryStream -> TestTree
requestTagDeliveryStream =
  req
    "TagDeliveryStream"
    "fixture/TagDeliveryStream.yaml"

requestUpdateDestination :: UpdateDestination -> TestTree
requestUpdateDestination =
  req
    "UpdateDestination"
    "fixture/UpdateDestination.yaml"

requestPutRecordBatch :: PutRecordBatch -> TestTree
requestPutRecordBatch =
  req
    "PutRecordBatch"
    "fixture/PutRecordBatch.yaml"

requestUntagDeliveryStream :: UntagDeliveryStream -> TestTree
requestUntagDeliveryStream =
  req
    "UntagDeliveryStream"
    "fixture/UntagDeliveryStream.yaml"

requestCreateDeliveryStream :: CreateDeliveryStream -> TestTree
requestCreateDeliveryStream =
  req
    "CreateDeliveryStream"
    "fixture/CreateDeliveryStream.yaml"

requestStartDeliveryStreamEncryption :: StartDeliveryStreamEncryption -> TestTree
requestStartDeliveryStreamEncryption =
  req
    "StartDeliveryStreamEncryption"
    "fixture/StartDeliveryStreamEncryption.yaml"

requestDescribeDeliveryStream :: DescribeDeliveryStream -> TestTree
requestDescribeDeliveryStream =
  req
    "DescribeDeliveryStream"
    "fixture/DescribeDeliveryStream.yaml"

requestListTagsForDeliveryStream :: ListTagsForDeliveryStream -> TestTree
requestListTagsForDeliveryStream =
  req
    "ListTagsForDeliveryStream"
    "fixture/ListTagsForDeliveryStream.yaml"

requestListDeliveryStreams :: ListDeliveryStreams -> TestTree
requestListDeliveryStreams =
  req
    "ListDeliveryStreams"
    "fixture/ListDeliveryStreams.yaml"

requestDeleteDeliveryStream :: DeleteDeliveryStream -> TestTree
requestDeleteDeliveryStream =
  req
    "DeleteDeliveryStream"
    "fixture/DeleteDeliveryStream.yaml"

-- Responses

responsePutRecord :: PutRecordResponse -> TestTree
responsePutRecord =
  res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutRecord)

responseStopDeliveryStreamEncryption :: StopDeliveryStreamEncryptionResponse -> TestTree
responseStopDeliveryStreamEncryption =
  res
    "StopDeliveryStreamEncryptionResponse"
    "fixture/StopDeliveryStreamEncryptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopDeliveryStreamEncryption)

responseTagDeliveryStream :: TagDeliveryStreamResponse -> TestTree
responseTagDeliveryStream =
  res
    "TagDeliveryStreamResponse"
    "fixture/TagDeliveryStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagDeliveryStream)

responseUpdateDestination :: UpdateDestinationResponse -> TestTree
responseUpdateDestination =
  res
    "UpdateDestinationResponse"
    "fixture/UpdateDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDestination)

responsePutRecordBatch :: PutRecordBatchResponse -> TestTree
responsePutRecordBatch =
  res
    "PutRecordBatchResponse"
    "fixture/PutRecordBatchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutRecordBatch)

responseUntagDeliveryStream :: UntagDeliveryStreamResponse -> TestTree
responseUntagDeliveryStream =
  res
    "UntagDeliveryStreamResponse"
    "fixture/UntagDeliveryStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagDeliveryStream)

responseCreateDeliveryStream :: CreateDeliveryStreamResponse -> TestTree
responseCreateDeliveryStream =
  res
    "CreateDeliveryStreamResponse"
    "fixture/CreateDeliveryStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDeliveryStream)

responseStartDeliveryStreamEncryption :: StartDeliveryStreamEncryptionResponse -> TestTree
responseStartDeliveryStreamEncryption =
  res
    "StartDeliveryStreamEncryptionResponse"
    "fixture/StartDeliveryStreamEncryptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartDeliveryStreamEncryption)

responseDescribeDeliveryStream :: DescribeDeliveryStreamResponse -> TestTree
responseDescribeDeliveryStream =
  res
    "DescribeDeliveryStreamResponse"
    "fixture/DescribeDeliveryStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDeliveryStream)

responseListTagsForDeliveryStream :: ListTagsForDeliveryStreamResponse -> TestTree
responseListTagsForDeliveryStream =
  res
    "ListTagsForDeliveryStreamResponse"
    "fixture/ListTagsForDeliveryStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForDeliveryStream)

responseListDeliveryStreams :: ListDeliveryStreamsResponse -> TestTree
responseListDeliveryStreams =
  res
    "ListDeliveryStreamsResponse"
    "fixture/ListDeliveryStreamsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDeliveryStreams)

responseDeleteDeliveryStream :: DeleteDeliveryStreamResponse -> TestTree
responseDeleteDeliveryStream =
  res
    "DeleteDeliveryStreamResponse"
    "fixture/DeleteDeliveryStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDeliveryStream)
