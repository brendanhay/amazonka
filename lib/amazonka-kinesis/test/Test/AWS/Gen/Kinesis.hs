{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Kinesis
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestPutRecord $
--             mkPutRecord
--
--         , requestSubscribeToShard $
--             mkSubscribeToShard
--
--         , requestDecreaseStreamRetentionPeriod $
--             mkDecreaseStreamRetentionPeriod
--
--         , requestMergeShards $
--             mkMergeShards
--
--         , requestDeregisterStreamConsumer $
--             mkDeregisterStreamConsumer
--
--         , requestDescribeStreamSummary $
--             mkDescribeStreamSummary
--
--         , requestGetShardIterator $
--             mkGetShardIterator
--
--         , requestGetRecords $
--             mkGetRecords
--
--         , requestStopStreamEncryption $
--             mkStopStreamEncryption
--
--         , requestEnableEnhancedMonitoring $
--             mkEnableEnhancedMonitoring
--
--         , requestDescribeLimits $
--             mkDescribeLimits
--
--         , requestRegisterStreamConsumer $
--             mkRegisterStreamConsumer
--
--         , requestDisableEnhancedMonitoring $
--             mkDisableEnhancedMonitoring
--
--         , requestUpdateShardCount $
--             mkUpdateShardCount
--
--         , requestListTagsForStream $
--             mkListTagsForStream
--
--         , requestDescribeStreamConsumer $
--             mkDescribeStreamConsumer
--
--         , requestAddTagsToStream $
--             mkAddTagsToStream
--
--         , requestPutRecords $
--             mkPutRecords
--
--         , requestListShards $
--             mkListShards
--
--         , requestDeleteStream $
--             mkDeleteStream
--
--         , requestRemoveTagsFromStream $
--             mkRemoveTagsFromStream
--
--         , requestListStreams $
--             mkListStreams
--
--         , requestCreateStream $
--             mkCreateStream
--
--         , requestStartStreamEncryption $
--             mkStartStreamEncryption
--
--         , requestListStreamConsumers $
--             mkListStreamConsumers
--
--         , requestSplitShard $
--             mkSplitShard
--
--         , requestIncreaseStreamRetentionPeriod $
--             mkIncreaseStreamRetentionPeriod
--
--         , requestDescribeStream $
--             mkDescribeStream
--
--           ]

--     , testGroup "response"
--         [ responsePutRecord $
--             mkPutRecordResponse
--
--         , responseSubscribeToShard $
--             mkSubscribeToShardResponse
--
--         , responseDecreaseStreamRetentionPeriod $
--             mkDecreaseStreamRetentionPeriodResponse
--
--         , responseMergeShards $
--             mkMergeShardsResponse
--
--         , responseDeregisterStreamConsumer $
--             mkDeregisterStreamConsumerResponse
--
--         , responseDescribeStreamSummary $
--             mkDescribeStreamSummaryResponse
--
--         , responseGetShardIterator $
--             mkGetShardIteratorResponse
--
--         , responseGetRecords $
--             mkGetRecordsResponse
--
--         , responseStopStreamEncryption $
--             mkStopStreamEncryptionResponse
--
--         , responseEnableEnhancedMonitoring $
--             mkEnhancedMonitoringOutput
--
--         , responseDescribeLimits $
--             mkDescribeLimitsResponse
--
--         , responseRegisterStreamConsumer $
--             mkRegisterStreamConsumerResponse
--
--         , responseDisableEnhancedMonitoring $
--             mkEnhancedMonitoringOutput
--
--         , responseUpdateShardCount $
--             mkUpdateShardCountResponse
--
--         , responseListTagsForStream $
--             mkListTagsForStreamResponse
--
--         , responseDescribeStreamConsumer $
--             mkDescribeStreamConsumerResponse
--
--         , responseAddTagsToStream $
--             mkAddTagsToStreamResponse
--
--         , responsePutRecords $
--             mkPutRecordsResponse
--
--         , responseListShards $
--             mkListShardsResponse
--
--         , responseDeleteStream $
--             mkDeleteStreamResponse
--
--         , responseRemoveTagsFromStream $
--             mkRemoveTagsFromStreamResponse
--
--         , responseListStreams $
--             mkListStreamsResponse
--
--         , responseCreateStream $
--             mkCreateStreamResponse
--
--         , responseStartStreamEncryption $
--             mkStartStreamEncryptionResponse
--
--         , responseListStreamConsumers $
--             mkListStreamConsumersResponse
--
--         , responseSplitShard $
--             mkSplitShardResponse
--
--         , responseIncreaseStreamRetentionPeriod $
--             mkIncreaseStreamRetentionPeriodResponse
--
--         , responseDescribeStream $
--             mkDescribeStreamResponse
--
--           ]
--     ]

-- Requests

requestPutRecord :: PutRecord -> TestTree
requestPutRecord =
  req
    "PutRecord"
    "fixture/PutRecord.yaml"

requestSubscribeToShard :: SubscribeToShard -> TestTree
requestSubscribeToShard =
  req
    "SubscribeToShard"
    "fixture/SubscribeToShard.yaml"

requestDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriod -> TestTree
requestDecreaseStreamRetentionPeriod =
  req
    "DecreaseStreamRetentionPeriod"
    "fixture/DecreaseStreamRetentionPeriod.yaml"

requestMergeShards :: MergeShards -> TestTree
requestMergeShards =
  req
    "MergeShards"
    "fixture/MergeShards.yaml"

requestDeregisterStreamConsumer :: DeregisterStreamConsumer -> TestTree
requestDeregisterStreamConsumer =
  req
    "DeregisterStreamConsumer"
    "fixture/DeregisterStreamConsumer.yaml"

requestDescribeStreamSummary :: DescribeStreamSummary -> TestTree
requestDescribeStreamSummary =
  req
    "DescribeStreamSummary"
    "fixture/DescribeStreamSummary.yaml"

requestGetShardIterator :: GetShardIterator -> TestTree
requestGetShardIterator =
  req
    "GetShardIterator"
    "fixture/GetShardIterator.yaml"

requestGetRecords :: GetRecords -> TestTree
requestGetRecords =
  req
    "GetRecords"
    "fixture/GetRecords.yaml"

requestStopStreamEncryption :: StopStreamEncryption -> TestTree
requestStopStreamEncryption =
  req
    "StopStreamEncryption"
    "fixture/StopStreamEncryption.yaml"

requestEnableEnhancedMonitoring :: EnableEnhancedMonitoring -> TestTree
requestEnableEnhancedMonitoring =
  req
    "EnableEnhancedMonitoring"
    "fixture/EnableEnhancedMonitoring.yaml"

requestDescribeLimits :: DescribeLimits -> TestTree
requestDescribeLimits =
  req
    "DescribeLimits"
    "fixture/DescribeLimits.yaml"

requestRegisterStreamConsumer :: RegisterStreamConsumer -> TestTree
requestRegisterStreamConsumer =
  req
    "RegisterStreamConsumer"
    "fixture/RegisterStreamConsumer.yaml"

requestDisableEnhancedMonitoring :: DisableEnhancedMonitoring -> TestTree
requestDisableEnhancedMonitoring =
  req
    "DisableEnhancedMonitoring"
    "fixture/DisableEnhancedMonitoring.yaml"

requestUpdateShardCount :: UpdateShardCount -> TestTree
requestUpdateShardCount =
  req
    "UpdateShardCount"
    "fixture/UpdateShardCount.yaml"

requestListTagsForStream :: ListTagsForStream -> TestTree
requestListTagsForStream =
  req
    "ListTagsForStream"
    "fixture/ListTagsForStream.yaml"

requestDescribeStreamConsumer :: DescribeStreamConsumer -> TestTree
requestDescribeStreamConsumer =
  req
    "DescribeStreamConsumer"
    "fixture/DescribeStreamConsumer.yaml"

requestAddTagsToStream :: AddTagsToStream -> TestTree
requestAddTagsToStream =
  req
    "AddTagsToStream"
    "fixture/AddTagsToStream.yaml"

requestPutRecords :: PutRecords -> TestTree
requestPutRecords =
  req
    "PutRecords"
    "fixture/PutRecords.yaml"

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

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream =
  req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestStartStreamEncryption :: StartStreamEncryption -> TestTree
requestStartStreamEncryption =
  req
    "StartStreamEncryption"
    "fixture/StartStreamEncryption.yaml"

requestListStreamConsumers :: ListStreamConsumers -> TestTree
requestListStreamConsumers =
  req
    "ListStreamConsumers"
    "fixture/ListStreamConsumers.yaml"

requestSplitShard :: SplitShard -> TestTree
requestSplitShard =
  req
    "SplitShard"
    "fixture/SplitShard.yaml"

requestIncreaseStreamRetentionPeriod :: IncreaseStreamRetentionPeriod -> TestTree
requestIncreaseStreamRetentionPeriod =
  req
    "IncreaseStreamRetentionPeriod"
    "fixture/IncreaseStreamRetentionPeriod.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream =
  req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

-- Responses

responsePutRecord :: PutRecordResponse -> TestTree
responsePutRecord =
  res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    kinesisService
    (Proxy :: Proxy PutRecord)

responseSubscribeToShard :: SubscribeToShardResponse -> TestTree
responseSubscribeToShard =
  res
    "SubscribeToShardResponse"
    "fixture/SubscribeToShardResponse.proto"
    kinesisService
    (Proxy :: Proxy SubscribeToShard)

responseDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriodResponse -> TestTree
responseDecreaseStreamRetentionPeriod =
  res
    "DecreaseStreamRetentionPeriodResponse"
    "fixture/DecreaseStreamRetentionPeriodResponse.proto"
    kinesisService
    (Proxy :: Proxy DecreaseStreamRetentionPeriod)

responseMergeShards :: MergeShardsResponse -> TestTree
responseMergeShards =
  res
    "MergeShardsResponse"
    "fixture/MergeShardsResponse.proto"
    kinesisService
    (Proxy :: Proxy MergeShards)

responseDeregisterStreamConsumer :: DeregisterStreamConsumerResponse -> TestTree
responseDeregisterStreamConsumer =
  res
    "DeregisterStreamConsumerResponse"
    "fixture/DeregisterStreamConsumerResponse.proto"
    kinesisService
    (Proxy :: Proxy DeregisterStreamConsumer)

responseDescribeStreamSummary :: DescribeStreamSummaryResponse -> TestTree
responseDescribeStreamSummary =
  res
    "DescribeStreamSummaryResponse"
    "fixture/DescribeStreamSummaryResponse.proto"
    kinesisService
    (Proxy :: Proxy DescribeStreamSummary)

responseGetShardIterator :: GetShardIteratorResponse -> TestTree
responseGetShardIterator =
  res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse.proto"
    kinesisService
    (Proxy :: Proxy GetShardIterator)

responseGetRecords :: GetRecordsResponse -> TestTree
responseGetRecords =
  res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse.proto"
    kinesisService
    (Proxy :: Proxy GetRecords)

responseStopStreamEncryption :: StopStreamEncryptionResponse -> TestTree
responseStopStreamEncryption =
  res
    "StopStreamEncryptionResponse"
    "fixture/StopStreamEncryptionResponse.proto"
    kinesisService
    (Proxy :: Proxy StopStreamEncryption)

responseEnableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseEnableEnhancedMonitoring =
  res
    "EnableEnhancedMonitoringResponse"
    "fixture/EnableEnhancedMonitoringResponse.proto"
    kinesisService
    (Proxy :: Proxy EnableEnhancedMonitoring)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits =
  res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    kinesisService
    (Proxy :: Proxy DescribeLimits)

responseRegisterStreamConsumer :: RegisterStreamConsumerResponse -> TestTree
responseRegisterStreamConsumer =
  res
    "RegisterStreamConsumerResponse"
    "fixture/RegisterStreamConsumerResponse.proto"
    kinesisService
    (Proxy :: Proxy RegisterStreamConsumer)

responseDisableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseDisableEnhancedMonitoring =
  res
    "DisableEnhancedMonitoringResponse"
    "fixture/DisableEnhancedMonitoringResponse.proto"
    kinesisService
    (Proxy :: Proxy DisableEnhancedMonitoring)

responseUpdateShardCount :: UpdateShardCountResponse -> TestTree
responseUpdateShardCount =
  res
    "UpdateShardCountResponse"
    "fixture/UpdateShardCountResponse.proto"
    kinesisService
    (Proxy :: Proxy UpdateShardCount)

responseListTagsForStream :: ListTagsForStreamResponse -> TestTree
responseListTagsForStream =
  res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    kinesisService
    (Proxy :: Proxy ListTagsForStream)

responseDescribeStreamConsumer :: DescribeStreamConsumerResponse -> TestTree
responseDescribeStreamConsumer =
  res
    "DescribeStreamConsumerResponse"
    "fixture/DescribeStreamConsumerResponse.proto"
    kinesisService
    (Proxy :: Proxy DescribeStreamConsumer)

responseAddTagsToStream :: AddTagsToStreamResponse -> TestTree
responseAddTagsToStream =
  res
    "AddTagsToStreamResponse"
    "fixture/AddTagsToStreamResponse.proto"
    kinesisService
    (Proxy :: Proxy AddTagsToStream)

responsePutRecords :: PutRecordsResponse -> TestTree
responsePutRecords =
  res
    "PutRecordsResponse"
    "fixture/PutRecordsResponse.proto"
    kinesisService
    (Proxy :: Proxy PutRecords)

responseListShards :: ListShardsResponse -> TestTree
responseListShards =
  res
    "ListShardsResponse"
    "fixture/ListShardsResponse.proto"
    kinesisService
    (Proxy :: Proxy ListShards)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    kinesisService
    (Proxy :: Proxy DeleteStream)

responseRemoveTagsFromStream :: RemoveTagsFromStreamResponse -> TestTree
responseRemoveTagsFromStream =
  res
    "RemoveTagsFromStreamResponse"
    "fixture/RemoveTagsFromStreamResponse.proto"
    kinesisService
    (Proxy :: Proxy RemoveTagsFromStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    kinesisService
    (Proxy :: Proxy ListStreams)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    kinesisService
    (Proxy :: Proxy CreateStream)

responseStartStreamEncryption :: StartStreamEncryptionResponse -> TestTree
responseStartStreamEncryption =
  res
    "StartStreamEncryptionResponse"
    "fixture/StartStreamEncryptionResponse.proto"
    kinesisService
    (Proxy :: Proxy StartStreamEncryption)

responseListStreamConsumers :: ListStreamConsumersResponse -> TestTree
responseListStreamConsumers =
  res
    "ListStreamConsumersResponse"
    "fixture/ListStreamConsumersResponse.proto"
    kinesisService
    (Proxy :: Proxy ListStreamConsumers)

responseSplitShard :: SplitShardResponse -> TestTree
responseSplitShard =
  res
    "SplitShardResponse"
    "fixture/SplitShardResponse.proto"
    kinesisService
    (Proxy :: Proxy SplitShard)

responseIncreaseStreamRetentionPeriod :: IncreaseStreamRetentionPeriodResponse -> TestTree
responseIncreaseStreamRetentionPeriod =
  res
    "IncreaseStreamRetentionPeriodResponse"
    "fixture/IncreaseStreamRetentionPeriodResponse.proto"
    kinesisService
    (Proxy :: Proxy IncreaseStreamRetentionPeriod)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    kinesisService
    (Proxy :: Proxy DescribeStream)
