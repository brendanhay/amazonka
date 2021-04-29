{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Firehose
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestStartDeliveryStreamEncryption $
--             newStartDeliveryStreamEncryption
--
--         , requestStopDeliveryStreamEncryption $
--             newStopDeliveryStreamEncryption
--
--         , requestListDeliveryStreams $
--             newListDeliveryStreams
--
--         , requestPutRecordBatch $
--             newPutRecordBatch
--
--         , requestListTagsForDeliveryStream $
--             newListTagsForDeliveryStream
--
--         , requestUpdateDestination $
--             newUpdateDestination
--
--         , requestDescribeDeliveryStream $
--             newDescribeDeliveryStream
--
--         , requestCreateDeliveryStream $
--             newCreateDeliveryStream
--
--         , requestPutRecord $
--             newPutRecord
--
--         , requestDeleteDeliveryStream $
--             newDeleteDeliveryStream
--
--         , requestUntagDeliveryStream $
--             newUntagDeliveryStream
--
--         , requestTagDeliveryStream $
--             newTagDeliveryStream
--
--           ]

--     , testGroup "response"
--         [ responseStartDeliveryStreamEncryption $
--             newStartDeliveryStreamEncryptionResponse
--
--         , responseStopDeliveryStreamEncryption $
--             newStopDeliveryStreamEncryptionResponse
--
--         , responseListDeliveryStreams $
--             newListDeliveryStreamsResponse
--
--         , responsePutRecordBatch $
--             newPutRecordBatchResponse
--
--         , responseListTagsForDeliveryStream $
--             newListTagsForDeliveryStreamResponse
--
--         , responseUpdateDestination $
--             newUpdateDestinationResponse
--
--         , responseDescribeDeliveryStream $
--             newDescribeDeliveryStreamResponse
--
--         , responseCreateDeliveryStream $
--             newCreateDeliveryStreamResponse
--
--         , responsePutRecord $
--             newPutRecordResponse
--
--         , responseDeleteDeliveryStream $
--             newDeleteDeliveryStreamResponse
--
--         , responseUntagDeliveryStream $
--             newUntagDeliveryStreamResponse
--
--         , responseTagDeliveryStream $
--             newTagDeliveryStreamResponse
--
--           ]
--     ]

-- Requests

requestStartDeliveryStreamEncryption :: StartDeliveryStreamEncryption -> TestTree
requestStartDeliveryStreamEncryption =
  req
    "StartDeliveryStreamEncryption"
    "fixture/StartDeliveryStreamEncryption.yaml"

requestStopDeliveryStreamEncryption :: StopDeliveryStreamEncryption -> TestTree
requestStopDeliveryStreamEncryption =
  req
    "StopDeliveryStreamEncryption"
    "fixture/StopDeliveryStreamEncryption.yaml"

requestListDeliveryStreams :: ListDeliveryStreams -> TestTree
requestListDeliveryStreams =
  req
    "ListDeliveryStreams"
    "fixture/ListDeliveryStreams.yaml"

requestPutRecordBatch :: PutRecordBatch -> TestTree
requestPutRecordBatch =
  req
    "PutRecordBatch"
    "fixture/PutRecordBatch.yaml"

requestListTagsForDeliveryStream :: ListTagsForDeliveryStream -> TestTree
requestListTagsForDeliveryStream =
  req
    "ListTagsForDeliveryStream"
    "fixture/ListTagsForDeliveryStream.yaml"

requestUpdateDestination :: UpdateDestination -> TestTree
requestUpdateDestination =
  req
    "UpdateDestination"
    "fixture/UpdateDestination.yaml"

requestDescribeDeliveryStream :: DescribeDeliveryStream -> TestTree
requestDescribeDeliveryStream =
  req
    "DescribeDeliveryStream"
    "fixture/DescribeDeliveryStream.yaml"

requestCreateDeliveryStream :: CreateDeliveryStream -> TestTree
requestCreateDeliveryStream =
  req
    "CreateDeliveryStream"
    "fixture/CreateDeliveryStream.yaml"

requestPutRecord :: PutRecord -> TestTree
requestPutRecord =
  req
    "PutRecord"
    "fixture/PutRecord.yaml"

requestDeleteDeliveryStream :: DeleteDeliveryStream -> TestTree
requestDeleteDeliveryStream =
  req
    "DeleteDeliveryStream"
    "fixture/DeleteDeliveryStream.yaml"

requestUntagDeliveryStream :: UntagDeliveryStream -> TestTree
requestUntagDeliveryStream =
  req
    "UntagDeliveryStream"
    "fixture/UntagDeliveryStream.yaml"

requestTagDeliveryStream :: TagDeliveryStream -> TestTree
requestTagDeliveryStream =
  req
    "TagDeliveryStream"
    "fixture/TagDeliveryStream.yaml"

-- Responses

responseStartDeliveryStreamEncryption :: StartDeliveryStreamEncryptionResponse -> TestTree
responseStartDeliveryStreamEncryption =
  res
    "StartDeliveryStreamEncryptionResponse"
    "fixture/StartDeliveryStreamEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy StartDeliveryStreamEncryption)

responseStopDeliveryStreamEncryption :: StopDeliveryStreamEncryptionResponse -> TestTree
responseStopDeliveryStreamEncryption =
  res
    "StopDeliveryStreamEncryptionResponse"
    "fixture/StopDeliveryStreamEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy StopDeliveryStreamEncryption)

responseListDeliveryStreams :: ListDeliveryStreamsResponse -> TestTree
responseListDeliveryStreams =
  res
    "ListDeliveryStreamsResponse"
    "fixture/ListDeliveryStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeliveryStreams)

responsePutRecordBatch :: PutRecordBatchResponse -> TestTree
responsePutRecordBatch =
  res
    "PutRecordBatchResponse"
    "fixture/PutRecordBatchResponse.proto"
    defaultService
    (Proxy :: Proxy PutRecordBatch)

responseListTagsForDeliveryStream :: ListTagsForDeliveryStreamResponse -> TestTree
responseListTagsForDeliveryStream =
  res
    "ListTagsForDeliveryStreamResponse"
    "fixture/ListTagsForDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForDeliveryStream)

responseUpdateDestination :: UpdateDestinationResponse -> TestTree
responseUpdateDestination =
  res
    "UpdateDestinationResponse"
    "fixture/UpdateDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDestination)

responseDescribeDeliveryStream :: DescribeDeliveryStreamResponse -> TestTree
responseDescribeDeliveryStream =
  res
    "DescribeDeliveryStreamResponse"
    "fixture/DescribeDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDeliveryStream)

responseCreateDeliveryStream :: CreateDeliveryStreamResponse -> TestTree
responseCreateDeliveryStream =
  res
    "CreateDeliveryStreamResponse"
    "fixture/CreateDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeliveryStream)

responsePutRecord :: PutRecordResponse -> TestTree
responsePutRecord =
  res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    defaultService
    (Proxy :: Proxy PutRecord)

responseDeleteDeliveryStream :: DeleteDeliveryStreamResponse -> TestTree
responseDeleteDeliveryStream =
  res
    "DeleteDeliveryStreamResponse"
    "fixture/DeleteDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeliveryStream)

responseUntagDeliveryStream :: UntagDeliveryStreamResponse -> TestTree
responseUntagDeliveryStream =
  res
    "UntagDeliveryStreamResponse"
    "fixture/UntagDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy UntagDeliveryStream)

responseTagDeliveryStream :: TagDeliveryStreamResponse -> TestTree
responseTagDeliveryStream =
  res
    "TagDeliveryStreamResponse"
    "fixture/TagDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy TagDeliveryStream)
