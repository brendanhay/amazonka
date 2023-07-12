{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Firehose
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Firehose where

import Amazonka.Firehose
import qualified Data.Proxy as Proxy
import Test.Amazonka.Firehose.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateDeliveryStream $
--             newCreateDeliveryStream
--
--         , requestDeleteDeliveryStream $
--             newDeleteDeliveryStream
--
--         , requestDescribeDeliveryStream $
--             newDescribeDeliveryStream
--
--         , requestListDeliveryStreams $
--             newListDeliveryStreams
--
--         , requestListTagsForDeliveryStream $
--             newListTagsForDeliveryStream
--
--         , requestPutRecord $
--             newPutRecord
--
--         , requestPutRecordBatch $
--             newPutRecordBatch
--
--         , requestStartDeliveryStreamEncryption $
--             newStartDeliveryStreamEncryption
--
--         , requestStopDeliveryStreamEncryption $
--             newStopDeliveryStreamEncryption
--
--         , requestTagDeliveryStream $
--             newTagDeliveryStream
--
--         , requestUntagDeliveryStream $
--             newUntagDeliveryStream
--
--         , requestUpdateDestination $
--             newUpdateDestination
--
--           ]

--     , testGroup "response"
--         [ responseCreateDeliveryStream $
--             newCreateDeliveryStreamResponse
--
--         , responseDeleteDeliveryStream $
--             newDeleteDeliveryStreamResponse
--
--         , responseDescribeDeliveryStream $
--             newDescribeDeliveryStreamResponse
--
--         , responseListDeliveryStreams $
--             newListDeliveryStreamsResponse
--
--         , responseListTagsForDeliveryStream $
--             newListTagsForDeliveryStreamResponse
--
--         , responsePutRecord $
--             newPutRecordResponse
--
--         , responsePutRecordBatch $
--             newPutRecordBatchResponse
--
--         , responseStartDeliveryStreamEncryption $
--             newStartDeliveryStreamEncryptionResponse
--
--         , responseStopDeliveryStreamEncryption $
--             newStopDeliveryStreamEncryptionResponse
--
--         , responseTagDeliveryStream $
--             newTagDeliveryStreamResponse
--
--         , responseUntagDeliveryStream $
--             newUntagDeliveryStreamResponse
--
--         , responseUpdateDestination $
--             newUpdateDestinationResponse
--
--           ]
--     ]

-- Requests

requestCreateDeliveryStream :: CreateDeliveryStream -> TestTree
requestCreateDeliveryStream =
  req
    "CreateDeliveryStream"
    "fixture/CreateDeliveryStream.yaml"

requestDeleteDeliveryStream :: DeleteDeliveryStream -> TestTree
requestDeleteDeliveryStream =
  req
    "DeleteDeliveryStream"
    "fixture/DeleteDeliveryStream.yaml"

requestDescribeDeliveryStream :: DescribeDeliveryStream -> TestTree
requestDescribeDeliveryStream =
  req
    "DescribeDeliveryStream"
    "fixture/DescribeDeliveryStream.yaml"

requestListDeliveryStreams :: ListDeliveryStreams -> TestTree
requestListDeliveryStreams =
  req
    "ListDeliveryStreams"
    "fixture/ListDeliveryStreams.yaml"

requestListTagsForDeliveryStream :: ListTagsForDeliveryStream -> TestTree
requestListTagsForDeliveryStream =
  req
    "ListTagsForDeliveryStream"
    "fixture/ListTagsForDeliveryStream.yaml"

requestPutRecord :: PutRecord -> TestTree
requestPutRecord =
  req
    "PutRecord"
    "fixture/PutRecord.yaml"

requestPutRecordBatch :: PutRecordBatch -> TestTree
requestPutRecordBatch =
  req
    "PutRecordBatch"
    "fixture/PutRecordBatch.yaml"

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

requestTagDeliveryStream :: TagDeliveryStream -> TestTree
requestTagDeliveryStream =
  req
    "TagDeliveryStream"
    "fixture/TagDeliveryStream.yaml"

requestUntagDeliveryStream :: UntagDeliveryStream -> TestTree
requestUntagDeliveryStream =
  req
    "UntagDeliveryStream"
    "fixture/UntagDeliveryStream.yaml"

requestUpdateDestination :: UpdateDestination -> TestTree
requestUpdateDestination =
  req
    "UpdateDestination"
    "fixture/UpdateDestination.yaml"

-- Responses

responseCreateDeliveryStream :: CreateDeliveryStreamResponse -> TestTree
responseCreateDeliveryStream =
  res
    "CreateDeliveryStreamResponse"
    "fixture/CreateDeliveryStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeliveryStream)

responseDeleteDeliveryStream :: DeleteDeliveryStreamResponse -> TestTree
responseDeleteDeliveryStream =
  res
    "DeleteDeliveryStreamResponse"
    "fixture/DeleteDeliveryStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeliveryStream)

responseDescribeDeliveryStream :: DescribeDeliveryStreamResponse -> TestTree
responseDescribeDeliveryStream =
  res
    "DescribeDeliveryStreamResponse"
    "fixture/DescribeDeliveryStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeliveryStream)

responseListDeliveryStreams :: ListDeliveryStreamsResponse -> TestTree
responseListDeliveryStreams =
  res
    "ListDeliveryStreamsResponse"
    "fixture/ListDeliveryStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeliveryStreams)

responseListTagsForDeliveryStream :: ListTagsForDeliveryStreamResponse -> TestTree
responseListTagsForDeliveryStream =
  res
    "ListTagsForDeliveryStreamResponse"
    "fixture/ListTagsForDeliveryStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForDeliveryStream)

responsePutRecord :: PutRecordResponse -> TestTree
responsePutRecord =
  res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRecord)

responsePutRecordBatch :: PutRecordBatchResponse -> TestTree
responsePutRecordBatch =
  res
    "PutRecordBatchResponse"
    "fixture/PutRecordBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRecordBatch)

responseStartDeliveryStreamEncryption :: StartDeliveryStreamEncryptionResponse -> TestTree
responseStartDeliveryStreamEncryption =
  res
    "StartDeliveryStreamEncryptionResponse"
    "fixture/StartDeliveryStreamEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDeliveryStreamEncryption)

responseStopDeliveryStreamEncryption :: StopDeliveryStreamEncryptionResponse -> TestTree
responseStopDeliveryStreamEncryption =
  res
    "StopDeliveryStreamEncryptionResponse"
    "fixture/StopDeliveryStreamEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDeliveryStreamEncryption)

responseTagDeliveryStream :: TagDeliveryStreamResponse -> TestTree
responseTagDeliveryStream =
  res
    "TagDeliveryStreamResponse"
    "fixture/TagDeliveryStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagDeliveryStream)

responseUntagDeliveryStream :: UntagDeliveryStreamResponse -> TestTree
responseUntagDeliveryStream =
  res
    "UntagDeliveryStreamResponse"
    "fixture/UntagDeliveryStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagDeliveryStream)

responseUpdateDestination :: UpdateDestinationResponse -> TestTree
responseUpdateDestination =
  res
    "UpdateDestinationResponse"
    "fixture/UpdateDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDestination)
