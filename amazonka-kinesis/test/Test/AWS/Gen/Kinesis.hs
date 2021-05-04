{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Kinesis
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Kinesis where

import Data.Proxy
import Network.AWS.Kinesis
import Test.AWS.Fixture
import Test.AWS.Kinesis.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestSubscribeToShard $
--             newSubscribeToShard
--
--         , requestAddTagsToStream $
--             newAddTagsToStream
--
--         , requestListTagsForStream $
--             newListTagsForStream
--
--         , requestIncreaseStreamRetentionPeriod $
--             newIncreaseStreamRetentionPeriod
--
--         , requestDisableEnhancedMonitoring $
--             newDisableEnhancedMonitoring
--
--         , requestSplitShard $
--             newSplitShard
--
--         , requestListStreamConsumers $
--             newListStreamConsumers
--
--         , requestDescribeLimits $
--             newDescribeLimits
--
--         , requestStopStreamEncryption $
--             newStopStreamEncryption
--
--         , requestRegisterStreamConsumer $
--             newRegisterStreamConsumer
--
--         , requestStartStreamEncryption $
--             newStartStreamEncryption
--
--         , requestEnableEnhancedMonitoring $
--             newEnableEnhancedMonitoring
--
--         , requestGetRecords $
--             newGetRecords
--
--         , requestGetShardIterator $
--             newGetShardIterator
--
--         , requestListShards $
--             newListShards
--
--         , requestDeleteStream $
--             newDeleteStream
--
--         , requestRemoveTagsFromStream $
--             newRemoveTagsFromStream
--
--         , requestDescribeStreamSummary $
--             newDescribeStreamSummary
--
--         , requestDeregisterStreamConsumer $
--             newDeregisterStreamConsumer
--
--         , requestPutRecords $
--             newPutRecords
--
--         , requestMergeShards $
--             newMergeShards
--
--         , requestDescribeStreamConsumer $
--             newDescribeStreamConsumer
--
--         , requestDecreaseStreamRetentionPeriod $
--             newDecreaseStreamRetentionPeriod
--
--         , requestPutRecord $
--             newPutRecord
--
--         , requestDescribeStream $
--             newDescribeStream
--
--         , requestUpdateShardCount $
--             newUpdateShardCount
--
--         , requestCreateStream $
--             newCreateStream
--
--         , requestListStreams $
--             newListStreams
--
--           ]

--     , testGroup "response"
--         [ responseSubscribeToShard $
--             newSubscribeToShardResponse
--
--         , responseAddTagsToStream $
--             newAddTagsToStreamResponse
--
--         , responseListTagsForStream $
--             newListTagsForStreamResponse
--
--         , responseIncreaseStreamRetentionPeriod $
--             newIncreaseStreamRetentionPeriodResponse
--
--         , responseDisableEnhancedMonitoring $
--             newEnhancedMonitoringOutput
--
--         , responseSplitShard $
--             newSplitShardResponse
--
--         , responseListStreamConsumers $
--             newListStreamConsumersResponse
--
--         , responseDescribeLimits $
--             newDescribeLimitsResponse
--
--         , responseStopStreamEncryption $
--             newStopStreamEncryptionResponse
--
--         , responseRegisterStreamConsumer $
--             newRegisterStreamConsumerResponse
--
--         , responseStartStreamEncryption $
--             newStartStreamEncryptionResponse
--
--         , responseEnableEnhancedMonitoring $
--             newEnhancedMonitoringOutput
--
--         , responseGetRecords $
--             newGetRecordsResponse
--
--         , responseGetShardIterator $
--             newGetShardIteratorResponse
--
--         , responseListShards $
--             newListShardsResponse
--
--         , responseDeleteStream $
--             newDeleteStreamResponse
--
--         , responseRemoveTagsFromStream $
--             newRemoveTagsFromStreamResponse
--
--         , responseDescribeStreamSummary $
--             newDescribeStreamSummaryResponse
--
--         , responseDeregisterStreamConsumer $
--             newDeregisterStreamConsumerResponse
--
--         , responsePutRecords $
--             newPutRecordsResponse
--
--         , responseMergeShards $
--             newMergeShardsResponse
--
--         , responseDescribeStreamConsumer $
--             newDescribeStreamConsumerResponse
--
--         , responseDecreaseStreamRetentionPeriod $
--             newDecreaseStreamRetentionPeriodResponse
--
--         , responsePutRecord $
--             newPutRecordResponse
--
--         , responseDescribeStream $
--             newDescribeStreamResponse
--
--         , responseUpdateShardCount $
--             newUpdateShardCountResponse
--
--         , responseCreateStream $
--             newCreateStreamResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--           ]
--     ]

-- Requests

requestSubscribeToShard :: SubscribeToShard -> TestTree
requestSubscribeToShard =
  req
    "SubscribeToShard"
    "fixture/SubscribeToShard.yaml"

requestAddTagsToStream :: AddTagsToStream -> TestTree
requestAddTagsToStream =
  req
    "AddTagsToStream"
    "fixture/AddTagsToStream.yaml"

requestListTagsForStream :: ListTagsForStream -> TestTree
requestListTagsForStream =
  req
    "ListTagsForStream"
    "fixture/ListTagsForStream.yaml"

requestIncreaseStreamRetentionPeriod :: IncreaseStreamRetentionPeriod -> TestTree
requestIncreaseStreamRetentionPeriod =
  req
    "IncreaseStreamRetentionPeriod"
    "fixture/IncreaseStreamRetentionPeriod.yaml"

requestDisableEnhancedMonitoring :: DisableEnhancedMonitoring -> TestTree
requestDisableEnhancedMonitoring =
  req
    "DisableEnhancedMonitoring"
    "fixture/DisableEnhancedMonitoring.yaml"

requestSplitShard :: SplitShard -> TestTree
requestSplitShard =
  req
    "SplitShard"
    "fixture/SplitShard.yaml"

requestListStreamConsumers :: ListStreamConsumers -> TestTree
requestListStreamConsumers =
  req
    "ListStreamConsumers"
    "fixture/ListStreamConsumers.yaml"

requestDescribeLimits :: DescribeLimits -> TestTree
requestDescribeLimits =
  req
    "DescribeLimits"
    "fixture/DescribeLimits.yaml"

requestStopStreamEncryption :: StopStreamEncryption -> TestTree
requestStopStreamEncryption =
  req
    "StopStreamEncryption"
    "fixture/StopStreamEncryption.yaml"

requestRegisterStreamConsumer :: RegisterStreamConsumer -> TestTree
requestRegisterStreamConsumer =
  req
    "RegisterStreamConsumer"
    "fixture/RegisterStreamConsumer.yaml"

requestStartStreamEncryption :: StartStreamEncryption -> TestTree
requestStartStreamEncryption =
  req
    "StartStreamEncryption"
    "fixture/StartStreamEncryption.yaml"

requestEnableEnhancedMonitoring :: EnableEnhancedMonitoring -> TestTree
requestEnableEnhancedMonitoring =
  req
    "EnableEnhancedMonitoring"
    "fixture/EnableEnhancedMonitoring.yaml"

requestGetRecords :: GetRecords -> TestTree
requestGetRecords =
  req
    "GetRecords"
    "fixture/GetRecords.yaml"

requestGetShardIterator :: GetShardIterator -> TestTree
requestGetShardIterator =
  req
    "GetShardIterator"
    "fixture/GetShardIterator.yaml"

requestListShards :: ListShards -> TestTree
requestListShards =
  req
    "ListShards"
    "fixture/ListShards.yaml"

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream =
  req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestRemoveTagsFromStream :: RemoveTagsFromStream -> TestTree
requestRemoveTagsFromStream =
  req
    "RemoveTagsFromStream"
    "fixture/RemoveTagsFromStream.yaml"

requestDescribeStreamSummary :: DescribeStreamSummary -> TestTree
requestDescribeStreamSummary =
  req
    "DescribeStreamSummary"
    "fixture/DescribeStreamSummary.yaml"

requestDeregisterStreamConsumer :: DeregisterStreamConsumer -> TestTree
requestDeregisterStreamConsumer =
  req
    "DeregisterStreamConsumer"
    "fixture/DeregisterStreamConsumer.yaml"

requestPutRecords :: PutRecords -> TestTree
requestPutRecords =
  req
    "PutRecords"
    "fixture/PutRecords.yaml"

requestMergeShards :: MergeShards -> TestTree
requestMergeShards =
  req
    "MergeShards"
    "fixture/MergeShards.yaml"

requestDescribeStreamConsumer :: DescribeStreamConsumer -> TestTree
requestDescribeStreamConsumer =
  req
    "DescribeStreamConsumer"
    "fixture/DescribeStreamConsumer.yaml"

requestDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriod -> TestTree
requestDecreaseStreamRetentionPeriod =
  req
    "DecreaseStreamRetentionPeriod"
    "fixture/DecreaseStreamRetentionPeriod.yaml"

requestPutRecord :: PutRecord -> TestTree
requestPutRecord =
  req
    "PutRecord"
    "fixture/PutRecord.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream =
  req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

requestUpdateShardCount :: UpdateShardCount -> TestTree
requestUpdateShardCount =
  req
    "UpdateShardCount"
    "fixture/UpdateShardCount.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream =
  req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

-- Responses

responseAddTagsToStream :: AddTagsToStreamResponse -> TestTree
responseAddTagsToStream =
  res
    "AddTagsToStreamResponse"
    "fixture/AddTagsToStreamResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToStream)

responseListTagsForStream :: ListTagsForStreamResponse -> TestTree
responseListTagsForStream =
  res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForStream)

responseIncreaseStreamRetentionPeriod :: IncreaseStreamRetentionPeriodResponse -> TestTree
responseIncreaseStreamRetentionPeriod =
  res
    "IncreaseStreamRetentionPeriodResponse"
    "fixture/IncreaseStreamRetentionPeriodResponse.proto"
    defaultService
    (Proxy :: Proxy IncreaseStreamRetentionPeriod)

responseDisableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseDisableEnhancedMonitoring =
  res
    "DisableEnhancedMonitoringResponse"
    "fixture/DisableEnhancedMonitoringResponse.proto"
    defaultService
    (Proxy :: Proxy DisableEnhancedMonitoring)

responseSplitShard :: SplitShardResponse -> TestTree
responseSplitShard =
  res
    "SplitShardResponse"
    "fixture/SplitShardResponse.proto"
    defaultService
    (Proxy :: Proxy SplitShard)

responseListStreamConsumers :: ListStreamConsumersResponse -> TestTree
responseListStreamConsumers =
  res
    "ListStreamConsumersResponse"
    "fixture/ListStreamConsumersResponse.proto"
    defaultService
    (Proxy :: Proxy ListStreamConsumers)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits =
  res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLimits)

responseStopStreamEncryption :: StopStreamEncryptionResponse -> TestTree
responseStopStreamEncryption =
  res
    "StopStreamEncryptionResponse"
    "fixture/StopStreamEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy StopStreamEncryption)

responseRegisterStreamConsumer :: RegisterStreamConsumerResponse -> TestTree
responseRegisterStreamConsumer =
  res
    "RegisterStreamConsumerResponse"
    "fixture/RegisterStreamConsumerResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterStreamConsumer)

responseStartStreamEncryption :: StartStreamEncryptionResponse -> TestTree
responseStartStreamEncryption =
  res
    "StartStreamEncryptionResponse"
    "fixture/StartStreamEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy StartStreamEncryption)

responseEnableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseEnableEnhancedMonitoring =
  res
    "EnableEnhancedMonitoringResponse"
    "fixture/EnableEnhancedMonitoringResponse.proto"
    defaultService
    (Proxy :: Proxy EnableEnhancedMonitoring)

responseGetRecords :: GetRecordsResponse -> TestTree
responseGetRecords =
  res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRecords)

responseGetShardIterator :: GetShardIteratorResponse -> TestTree
responseGetShardIterator =
  res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse.proto"
    defaultService
    (Proxy :: Proxy GetShardIterator)

responseListShards :: ListShardsResponse -> TestTree
responseListShards =
  res
    "ListShardsResponse"
    "fixture/ListShardsResponse.proto"
    defaultService
    (Proxy :: Proxy ListShards)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStream)

responseRemoveTagsFromStream :: RemoveTagsFromStreamResponse -> TestTree
responseRemoveTagsFromStream =
  res
    "RemoveTagsFromStreamResponse"
    "fixture/RemoveTagsFromStreamResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromStream)

responseDescribeStreamSummary :: DescribeStreamSummaryResponse -> TestTree
responseDescribeStreamSummary =
  res
    "DescribeStreamSummaryResponse"
    "fixture/DescribeStreamSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStreamSummary)

responseDeregisterStreamConsumer :: DeregisterStreamConsumerResponse -> TestTree
responseDeregisterStreamConsumer =
  res
    "DeregisterStreamConsumerResponse"
    "fixture/DeregisterStreamConsumerResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterStreamConsumer)

responsePutRecords :: PutRecordsResponse -> TestTree
responsePutRecords =
  res
    "PutRecordsResponse"
    "fixture/PutRecordsResponse.proto"
    defaultService
    (Proxy :: Proxy PutRecords)

responseMergeShards :: MergeShardsResponse -> TestTree
responseMergeShards =
  res
    "MergeShardsResponse"
    "fixture/MergeShardsResponse.proto"
    defaultService
    (Proxy :: Proxy MergeShards)

responseDescribeStreamConsumer :: DescribeStreamConsumerResponse -> TestTree
responseDescribeStreamConsumer =
  res
    "DescribeStreamConsumerResponse"
    "fixture/DescribeStreamConsumerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStreamConsumer)

responseDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriodResponse -> TestTree
responseDecreaseStreamRetentionPeriod =
  res
    "DecreaseStreamRetentionPeriodResponse"
    "fixture/DecreaseStreamRetentionPeriodResponse.proto"
    defaultService
    (Proxy :: Proxy DecreaseStreamRetentionPeriod)

responsePutRecord :: PutRecordResponse -> TestTree
responsePutRecord =
  res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    defaultService
    (Proxy :: Proxy PutRecord)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStream)

responseUpdateShardCount :: UpdateShardCountResponse -> TestTree
responseUpdateShardCount =
  res
    "UpdateShardCountResponse"
    "fixture/UpdateShardCountResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateShardCount)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStreams)
