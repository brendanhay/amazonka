{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Kinesis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Kinesis where

import Amazonka.Kinesis
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Kinesis.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddTagsToStream $
--             newAddTagsToStream
--
--         , requestCreateStream $
--             newCreateStream
--
--         , requestDecreaseStreamRetentionPeriod $
--             newDecreaseStreamRetentionPeriod
--
--         , requestDeleteStream $
--             newDeleteStream
--
--         , requestDeregisterStreamConsumer $
--             newDeregisterStreamConsumer
--
--         , requestDescribeLimits $
--             newDescribeLimits
--
--         , requestDescribeStream $
--             newDescribeStream
--
--         , requestDescribeStreamConsumer $
--             newDescribeStreamConsumer
--
--         , requestDescribeStreamSummary $
--             newDescribeStreamSummary
--
--         , requestDisableEnhancedMonitoring $
--             newDisableEnhancedMonitoring
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
--         , requestIncreaseStreamRetentionPeriod $
--             newIncreaseStreamRetentionPeriod
--
--         , requestListShards $
--             newListShards
--
--         , requestListStreamConsumers $
--             newListStreamConsumers
--
--         , requestListStreams $
--             newListStreams
--
--         , requestListTagsForStream $
--             newListTagsForStream
--
--         , requestMergeShards $
--             newMergeShards
--
--         , requestPutRecord $
--             newPutRecord
--
--         , requestPutRecords $
--             newPutRecords
--
--         , requestRegisterStreamConsumer $
--             newRegisterStreamConsumer
--
--         , requestRemoveTagsFromStream $
--             newRemoveTagsFromStream
--
--         , requestSplitShard $
--             newSplitShard
--
--         , requestStartStreamEncryption $
--             newStartStreamEncryption
--
--         , requestStopStreamEncryption $
--             newStopStreamEncryption
--
--         , requestSubscribeToShard $
--             newSubscribeToShard
--
--         , requestUpdateShardCount $
--             newUpdateShardCount
--
--         , requestUpdateStreamMode $
--             newUpdateStreamMode
--
--           ]

--     , testGroup "response"
--         [ responseAddTagsToStream $
--             newAddTagsToStreamResponse
--
--         , responseCreateStream $
--             newCreateStreamResponse
--
--         , responseDecreaseStreamRetentionPeriod $
--             newDecreaseStreamRetentionPeriodResponse
--
--         , responseDeleteStream $
--             newDeleteStreamResponse
--
--         , responseDeregisterStreamConsumer $
--             newDeregisterStreamConsumerResponse
--
--         , responseDescribeLimits $
--             newDescribeLimitsResponse
--
--         , responseDescribeStream $
--             newDescribeStreamResponse
--
--         , responseDescribeStreamConsumer $
--             newDescribeStreamConsumerResponse
--
--         , responseDescribeStreamSummary $
--             newDescribeStreamSummaryResponse
--
--         , responseDisableEnhancedMonitoring $
--             newEnhancedMonitoringOutput
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
--         , responseIncreaseStreamRetentionPeriod $
--             newIncreaseStreamRetentionPeriodResponse
--
--         , responseListShards $
--             newListShardsResponse
--
--         , responseListStreamConsumers $
--             newListStreamConsumersResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseListTagsForStream $
--             newListTagsForStreamResponse
--
--         , responseMergeShards $
--             newMergeShardsResponse
--
--         , responsePutRecord $
--             newPutRecordResponse
--
--         , responsePutRecords $
--             newPutRecordsResponse
--
--         , responseRegisterStreamConsumer $
--             newRegisterStreamConsumerResponse
--
--         , responseRemoveTagsFromStream $
--             newRemoveTagsFromStreamResponse
--
--         , responseSplitShard $
--             newSplitShardResponse
--
--         , responseStartStreamEncryption $
--             newStartStreamEncryptionResponse
--
--         , responseStopStreamEncryption $
--             newStopStreamEncryptionResponse
--
--         , responseSubscribeToShard $
--             newSubscribeToShardResponse
--
--         , responseUpdateShardCount $
--             newUpdateShardCountResponse
--
--         , responseUpdateStreamMode $
--             newUpdateStreamModeResponse
--
--           ]
--     ]

-- Requests

requestAddTagsToStream :: AddTagsToStream -> TestTree
requestAddTagsToStream =
  req
    "AddTagsToStream"
    "fixture/AddTagsToStream.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream =
  req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriod -> TestTree
requestDecreaseStreamRetentionPeriod =
  req
    "DecreaseStreamRetentionPeriod"
    "fixture/DecreaseStreamRetentionPeriod.yaml"

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream =
  req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestDeregisterStreamConsumer :: DeregisterStreamConsumer -> TestTree
requestDeregisterStreamConsumer =
  req
    "DeregisterStreamConsumer"
    "fixture/DeregisterStreamConsumer.yaml"

requestDescribeLimits :: DescribeLimits -> TestTree
requestDescribeLimits =
  req
    "DescribeLimits"
    "fixture/DescribeLimits.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream =
  req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

requestDescribeStreamConsumer :: DescribeStreamConsumer -> TestTree
requestDescribeStreamConsumer =
  req
    "DescribeStreamConsumer"
    "fixture/DescribeStreamConsumer.yaml"

requestDescribeStreamSummary :: DescribeStreamSummary -> TestTree
requestDescribeStreamSummary =
  req
    "DescribeStreamSummary"
    "fixture/DescribeStreamSummary.yaml"

requestDisableEnhancedMonitoring :: DisableEnhancedMonitoring -> TestTree
requestDisableEnhancedMonitoring =
  req
    "DisableEnhancedMonitoring"
    "fixture/DisableEnhancedMonitoring.yaml"

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

requestIncreaseStreamRetentionPeriod :: IncreaseStreamRetentionPeriod -> TestTree
requestIncreaseStreamRetentionPeriod =
  req
    "IncreaseStreamRetentionPeriod"
    "fixture/IncreaseStreamRetentionPeriod.yaml"

requestListShards :: ListShards -> TestTree
requestListShards =
  req
    "ListShards"
    "fixture/ListShards.yaml"

requestListStreamConsumers :: ListStreamConsumers -> TestTree
requestListStreamConsumers =
  req
    "ListStreamConsumers"
    "fixture/ListStreamConsumers.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestListTagsForStream :: ListTagsForStream -> TestTree
requestListTagsForStream =
  req
    "ListTagsForStream"
    "fixture/ListTagsForStream.yaml"

requestMergeShards :: MergeShards -> TestTree
requestMergeShards =
  req
    "MergeShards"
    "fixture/MergeShards.yaml"

requestPutRecord :: PutRecord -> TestTree
requestPutRecord =
  req
    "PutRecord"
    "fixture/PutRecord.yaml"

requestPutRecords :: PutRecords -> TestTree
requestPutRecords =
  req
    "PutRecords"
    "fixture/PutRecords.yaml"

requestRegisterStreamConsumer :: RegisterStreamConsumer -> TestTree
requestRegisterStreamConsumer =
  req
    "RegisterStreamConsumer"
    "fixture/RegisterStreamConsumer.yaml"

requestRemoveTagsFromStream :: RemoveTagsFromStream -> TestTree
requestRemoveTagsFromStream =
  req
    "RemoveTagsFromStream"
    "fixture/RemoveTagsFromStream.yaml"

requestSplitShard :: SplitShard -> TestTree
requestSplitShard =
  req
    "SplitShard"
    "fixture/SplitShard.yaml"

requestStartStreamEncryption :: StartStreamEncryption -> TestTree
requestStartStreamEncryption =
  req
    "StartStreamEncryption"
    "fixture/StartStreamEncryption.yaml"

requestStopStreamEncryption :: StopStreamEncryption -> TestTree
requestStopStreamEncryption =
  req
    "StopStreamEncryption"
    "fixture/StopStreamEncryption.yaml"

requestSubscribeToShard :: SubscribeToShard -> TestTree
requestSubscribeToShard =
  req
    "SubscribeToShard"
    "fixture/SubscribeToShard.yaml"

requestUpdateShardCount :: UpdateShardCount -> TestTree
requestUpdateShardCount =
  req
    "UpdateShardCount"
    "fixture/UpdateShardCount.yaml"

requestUpdateStreamMode :: UpdateStreamMode -> TestTree
requestUpdateStreamMode =
  req
    "UpdateStreamMode"
    "fixture/UpdateStreamMode.yaml"

-- Responses

responseAddTagsToStream :: AddTagsToStreamResponse -> TestTree
responseAddTagsToStream =
  res
    "AddTagsToStreamResponse"
    "fixture/AddTagsToStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToStream)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStream)

responseDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriodResponse -> TestTree
responseDecreaseStreamRetentionPeriod =
  res
    "DecreaseStreamRetentionPeriodResponse"
    "fixture/DecreaseStreamRetentionPeriodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DecreaseStreamRetentionPeriod)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStream)

responseDeregisterStreamConsumer :: DeregisterStreamConsumerResponse -> TestTree
responseDeregisterStreamConsumer =
  res
    "DeregisterStreamConsumerResponse"
    "fixture/DeregisterStreamConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterStreamConsumer)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits =
  res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLimits)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStream)

responseDescribeStreamConsumer :: DescribeStreamConsumerResponse -> TestTree
responseDescribeStreamConsumer =
  res
    "DescribeStreamConsumerResponse"
    "fixture/DescribeStreamConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStreamConsumer)

responseDescribeStreamSummary :: DescribeStreamSummaryResponse -> TestTree
responseDescribeStreamSummary =
  res
    "DescribeStreamSummaryResponse"
    "fixture/DescribeStreamSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStreamSummary)

responseDisableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseDisableEnhancedMonitoring =
  res
    "DisableEnhancedMonitoringResponse"
    "fixture/DisableEnhancedMonitoringResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableEnhancedMonitoring)

responseEnableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseEnableEnhancedMonitoring =
  res
    "EnableEnhancedMonitoringResponse"
    "fixture/EnableEnhancedMonitoringResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableEnhancedMonitoring)

responseGetRecords :: GetRecordsResponse -> TestTree
responseGetRecords =
  res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecords)

responseGetShardIterator :: GetShardIteratorResponse -> TestTree
responseGetShardIterator =
  res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetShardIterator)

responseIncreaseStreamRetentionPeriod :: IncreaseStreamRetentionPeriodResponse -> TestTree
responseIncreaseStreamRetentionPeriod =
  res
    "IncreaseStreamRetentionPeriodResponse"
    "fixture/IncreaseStreamRetentionPeriodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IncreaseStreamRetentionPeriod)

responseListShards :: ListShardsResponse -> TestTree
responseListShards =
  res
    "ListShardsResponse"
    "fixture/ListShardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListShards)

responseListStreamConsumers :: ListStreamConsumersResponse -> TestTree
responseListStreamConsumers =
  res
    "ListStreamConsumersResponse"
    "fixture/ListStreamConsumersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamConsumers)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreams)

responseListTagsForStream :: ListTagsForStreamResponse -> TestTree
responseListTagsForStream =
  res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForStream)

responseMergeShards :: MergeShardsResponse -> TestTree
responseMergeShards =
  res
    "MergeShardsResponse"
    "fixture/MergeShardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeShards)

responsePutRecord :: PutRecordResponse -> TestTree
responsePutRecord =
  res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRecord)

responsePutRecords :: PutRecordsResponse -> TestTree
responsePutRecords =
  res
    "PutRecordsResponse"
    "fixture/PutRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRecords)

responseRegisterStreamConsumer :: RegisterStreamConsumerResponse -> TestTree
responseRegisterStreamConsumer =
  res
    "RegisterStreamConsumerResponse"
    "fixture/RegisterStreamConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterStreamConsumer)

responseRemoveTagsFromStream :: RemoveTagsFromStreamResponse -> TestTree
responseRemoveTagsFromStream =
  res
    "RemoveTagsFromStreamResponse"
    "fixture/RemoveTagsFromStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromStream)

responseSplitShard :: SplitShardResponse -> TestTree
responseSplitShard =
  res
    "SplitShardResponse"
    "fixture/SplitShardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SplitShard)

responseStartStreamEncryption :: StartStreamEncryptionResponse -> TestTree
responseStartStreamEncryption =
  res
    "StartStreamEncryptionResponse"
    "fixture/StartStreamEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartStreamEncryption)

responseStopStreamEncryption :: StopStreamEncryptionResponse -> TestTree
responseStopStreamEncryption =
  res
    "StopStreamEncryptionResponse"
    "fixture/StopStreamEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStreamEncryption)

responseUpdateShardCount :: UpdateShardCountResponse -> TestTree
responseUpdateShardCount =
  res
    "UpdateShardCountResponse"
    "fixture/UpdateShardCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateShardCount)

responseUpdateStreamMode :: UpdateStreamModeResponse -> TestTree
responseUpdateStreamMode =
  res
    "UpdateStreamModeResponse"
    "fixture/UpdateStreamModeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStreamMode)
