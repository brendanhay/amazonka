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

import qualified Data.Proxy as Proxy
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
--             newPutRecord
--
--         , requestSubscribeToShard $
--             newSubscribeToShard
--
--         , requestDecreaseStreamRetentionPeriod $
--             newDecreaseStreamRetentionPeriod
--
--         , requestMergeShards $
--             newMergeShards
--
--         , requestDeregisterStreamConsumer $
--             newDeregisterStreamConsumer
--
--         , requestDescribeStreamSummary $
--             newDescribeStreamSummary
--
--         , requestGetShardIterator $
--             newGetShardIterator
--
--         , requestGetRecords $
--             newGetRecords
--
--         , requestStopStreamEncryption $
--             newStopStreamEncryption
--
--         , requestEnableEnhancedMonitoring $
--             newEnableEnhancedMonitoring
--
--         , requestDescribeLimits $
--             newDescribeLimits
--
--         , requestRegisterStreamConsumer $
--             newRegisterStreamConsumer
--
--         , requestDisableEnhancedMonitoring $
--             newDisableEnhancedMonitoring
--
--         , requestUpdateShardCount $
--             newUpdateShardCount
--
--         , requestListTagsForStream $
--             newListTagsForStream
--
--         , requestDescribeStreamConsumer $
--             newDescribeStreamConsumer
--
--         , requestAddTagsToStream $
--             newAddTagsToStream
--
--         , requestPutRecords $
--             newPutRecords
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
--         , requestListStreams $
--             newListStreams
--
--         , requestCreateStream $
--             newCreateStream
--
--         , requestStartStreamEncryption $
--             newStartStreamEncryption
--
--         , requestListStreamConsumers $
--             newListStreamConsumers
--
--         , requestSplitShard $
--             newSplitShard
--
--         , requestIncreaseStreamRetentionPeriod $
--             newIncreaseStreamRetentionPeriod
--
--         , requestDescribeStream $
--             newDescribeStream
--
--           ]

--     , testGroup "response"
--         [ responsePutRecord $
--             newPutRecordResponse
--
--         , responseSubscribeToShard $
--             newSubscribeToShardResponse
--
--         , responseDecreaseStreamRetentionPeriod $
--             newDecreaseStreamRetentionPeriodResponse
--
--         , responseMergeShards $
--             newMergeShardsResponse
--
--         , responseDeregisterStreamConsumer $
--             newDeregisterStreamConsumerResponse
--
--         , responseDescribeStreamSummary $
--             newDescribeStreamSummaryResponse
--
--         , responseGetShardIterator $
--             newGetShardIteratorResponse
--
--         , responseGetRecords $
--             newGetRecordsResponse
--
--         , responseStopStreamEncryption $
--             newStopStreamEncryptionResponse
--
--         , responseEnableEnhancedMonitoring $
--             newEnhancedMonitoringOutput
--
--         , responseDescribeLimits $
--             newDescribeLimitsResponse
--
--         , responseRegisterStreamConsumer $
--             newRegisterStreamConsumerResponse
--
--         , responseDisableEnhancedMonitoring $
--             newEnhancedMonitoringOutput
--
--         , responseUpdateShardCount $
--             newUpdateShardCountResponse
--
--         , responseListTagsForStream $
--             newListTagsForStreamResponse
--
--         , responseDescribeStreamConsumer $
--             newDescribeStreamConsumerResponse
--
--         , responseAddTagsToStream $
--             newAddTagsToStreamResponse
--
--         , responsePutRecords $
--             newPutRecordsResponse
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
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseCreateStream $
--             newCreateStreamResponse
--
--         , responseStartStreamEncryption $
--             newStartStreamEncryptionResponse
--
--         , responseListStreamConsumers $
--             newListStreamConsumersResponse
--
--         , responseSplitShard $
--             newSplitShardResponse
--
--         , responseIncreaseStreamRetentionPeriod $
--             newIncreaseStreamRetentionPeriodResponse
--
--         , responseDescribeStream $
--             newDescribeStreamResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRecord)

responseDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriodResponse -> TestTree
responseDecreaseStreamRetentionPeriod =
  res
    "DecreaseStreamRetentionPeriodResponse"
    "fixture/DecreaseStreamRetentionPeriodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DecreaseStreamRetentionPeriod)

responseMergeShards :: MergeShardsResponse -> TestTree
responseMergeShards =
  res
    "MergeShardsResponse"
    "fixture/MergeShardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeShards)

responseDeregisterStreamConsumer :: DeregisterStreamConsumerResponse -> TestTree
responseDeregisterStreamConsumer =
  res
    "DeregisterStreamConsumerResponse"
    "fixture/DeregisterStreamConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterStreamConsumer)

responseDescribeStreamSummary :: DescribeStreamSummaryResponse -> TestTree
responseDescribeStreamSummary =
  res
    "DescribeStreamSummaryResponse"
    "fixture/DescribeStreamSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStreamSummary)

responseGetShardIterator :: GetShardIteratorResponse -> TestTree
responseGetShardIterator =
  res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetShardIterator)

responseGetRecords :: GetRecordsResponse -> TestTree
responseGetRecords =
  res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecords)

responseStopStreamEncryption :: StopStreamEncryptionResponse -> TestTree
responseStopStreamEncryption =
  res
    "StopStreamEncryptionResponse"
    "fixture/StopStreamEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStreamEncryption)

responseEnableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseEnableEnhancedMonitoring =
  res
    "EnableEnhancedMonitoringResponse"
    "fixture/EnableEnhancedMonitoringResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableEnhancedMonitoring)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits =
  res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLimits)

responseRegisterStreamConsumer :: RegisterStreamConsumerResponse -> TestTree
responseRegisterStreamConsumer =
  res
    "RegisterStreamConsumerResponse"
    "fixture/RegisterStreamConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterStreamConsumer)

responseDisableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseDisableEnhancedMonitoring =
  res
    "DisableEnhancedMonitoringResponse"
    "fixture/DisableEnhancedMonitoringResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableEnhancedMonitoring)

responseUpdateShardCount :: UpdateShardCountResponse -> TestTree
responseUpdateShardCount =
  res
    "UpdateShardCountResponse"
    "fixture/UpdateShardCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateShardCount)

responseListTagsForStream :: ListTagsForStreamResponse -> TestTree
responseListTagsForStream =
  res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForStream)

responseDescribeStreamConsumer :: DescribeStreamConsumerResponse -> TestTree
responseDescribeStreamConsumer =
  res
    "DescribeStreamConsumerResponse"
    "fixture/DescribeStreamConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStreamConsumer)

responseAddTagsToStream :: AddTagsToStreamResponse -> TestTree
responseAddTagsToStream =
  res
    "AddTagsToStreamResponse"
    "fixture/AddTagsToStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToStream)

responsePutRecords :: PutRecordsResponse -> TestTree
responsePutRecords =
  res
    "PutRecordsResponse"
    "fixture/PutRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRecords)

responseListShards :: ListShardsResponse -> TestTree
responseListShards =
  res
    "ListShardsResponse"
    "fixture/ListShardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListShards)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStream)

responseRemoveTagsFromStream :: RemoveTagsFromStreamResponse -> TestTree
responseRemoveTagsFromStream =
  res
    "RemoveTagsFromStreamResponse"
    "fixture/RemoveTagsFromStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreams)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStream)

responseStartStreamEncryption :: StartStreamEncryptionResponse -> TestTree
responseStartStreamEncryption =
  res
    "StartStreamEncryptionResponse"
    "fixture/StartStreamEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartStreamEncryption)

responseListStreamConsumers :: ListStreamConsumersResponse -> TestTree
responseListStreamConsumers =
  res
    "ListStreamConsumersResponse"
    "fixture/ListStreamConsumersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamConsumers)

responseSplitShard :: SplitShardResponse -> TestTree
responseSplitShard =
  res
    "SplitShardResponse"
    "fixture/SplitShardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SplitShard)

responseIncreaseStreamRetentionPeriod :: IncreaseStreamRetentionPeriodResponse -> TestTree
responseIncreaseStreamRetentionPeriod =
  res
    "IncreaseStreamRetentionPeriodResponse"
    "fixture/IncreaseStreamRetentionPeriodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IncreaseStreamRetentionPeriod)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStream)
