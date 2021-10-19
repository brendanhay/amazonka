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
--         [ requestPutRecord $
--             newPutRecord
--
--         , requestStopDeliveryStreamEncryption $
--             newStopDeliveryStreamEncryption
--
--         , requestTagDeliveryStream $
--             newTagDeliveryStream
--
--         , requestUpdateDestination $
--             newUpdateDestination
--
--         , requestPutRecordBatch $
--             newPutRecordBatch
--
--         , requestUntagDeliveryStream $
--             newUntagDeliveryStream
--
--         , requestCreateDeliveryStream $
--             newCreateDeliveryStream
--
--         , requestStartDeliveryStreamEncryption $
--             newStartDeliveryStreamEncryption
--
--         , requestDescribeDeliveryStream $
--             newDescribeDeliveryStream
--
--         , requestListTagsForDeliveryStream $
--             newListTagsForDeliveryStream
--
--         , requestListDeliveryStreams $
--             newListDeliveryStreams
--
--         , requestDeleteDeliveryStream $
--             newDeleteDeliveryStream
--
--           ]

--     , testGroup "response"
--         [ responsePutRecord $
--             newPutRecordResponse
--
--         , responseStopDeliveryStreamEncryption $
--             newStopDeliveryStreamEncryptionResponse
--
--         , responseTagDeliveryStream $
--             newTagDeliveryStreamResponse
--
--         , responseUpdateDestination $
--             newUpdateDestinationResponse
--
--         , responsePutRecordBatch $
--             newPutRecordBatchResponse
--
--         , responseUntagDeliveryStream $
--             newUntagDeliveryStreamResponse
--
--         , responseCreateDeliveryStream $
--             newCreateDeliveryStreamResponse
--
--         , responseStartDeliveryStreamEncryption $
--             newStartDeliveryStreamEncryptionResponse
--
--         , responseDescribeDeliveryStream $
--             newDescribeDeliveryStreamResponse
--
--         , responseListTagsForDeliveryStream $
--             newListTagsForDeliveryStreamResponse
--
--         , responseListDeliveryStreams $
--             newListDeliveryStreamsResponse
--
--         , responseDeleteDeliveryStream $
--             newDeleteDeliveryStreamResponse
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
    defaultService
    (Proxy :: Proxy PutRecord)

responseStopDeliveryStreamEncryption :: StopDeliveryStreamEncryptionResponse -> TestTree
responseStopDeliveryStreamEncryption =
  res
    "StopDeliveryStreamEncryptionResponse"
    "fixture/StopDeliveryStreamEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy StopDeliveryStreamEncryption)

responseTagDeliveryStream :: TagDeliveryStreamResponse -> TestTree
responseTagDeliveryStream =
  res
    "TagDeliveryStreamResponse"
    "fixture/TagDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy TagDeliveryStream)

responseUpdateDestination :: UpdateDestinationResponse -> TestTree
responseUpdateDestination =
  res
    "UpdateDestinationResponse"
    "fixture/UpdateDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDestination)

responsePutRecordBatch :: PutRecordBatchResponse -> TestTree
responsePutRecordBatch =
  res
    "PutRecordBatchResponse"
    "fixture/PutRecordBatchResponse.proto"
    defaultService
    (Proxy :: Proxy PutRecordBatch)

responseUntagDeliveryStream :: UntagDeliveryStreamResponse -> TestTree
responseUntagDeliveryStream =
  res
    "UntagDeliveryStreamResponse"
    "fixture/UntagDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy UntagDeliveryStream)

responseCreateDeliveryStream :: CreateDeliveryStreamResponse -> TestTree
responseCreateDeliveryStream =
  res
    "CreateDeliveryStreamResponse"
    "fixture/CreateDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeliveryStream)

responseStartDeliveryStreamEncryption :: StartDeliveryStreamEncryptionResponse -> TestTree
responseStartDeliveryStreamEncryption =
  res
    "StartDeliveryStreamEncryptionResponse"
    "fixture/StartDeliveryStreamEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy StartDeliveryStreamEncryption)

responseDescribeDeliveryStream :: DescribeDeliveryStreamResponse -> TestTree
responseDescribeDeliveryStream =
  res
    "DescribeDeliveryStreamResponse"
    "fixture/DescribeDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDeliveryStream)

responseListTagsForDeliveryStream :: ListTagsForDeliveryStreamResponse -> TestTree
responseListTagsForDeliveryStream =
  res
    "ListTagsForDeliveryStreamResponse"
    "fixture/ListTagsForDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForDeliveryStream)

responseListDeliveryStreams :: ListDeliveryStreamsResponse -> TestTree
responseListDeliveryStreams =
  res
    "ListDeliveryStreamsResponse"
    "fixture/ListDeliveryStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeliveryStreams)

responseDeleteDeliveryStream :: DeleteDeliveryStreamResponse -> TestTree
responseDeleteDeliveryStream =
  res
    "DeleteDeliveryStreamResponse"
    "fixture/DeleteDeliveryStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeliveryStream)
