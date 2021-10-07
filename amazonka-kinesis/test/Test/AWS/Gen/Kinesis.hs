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
--         [ requestAddTagsToStream $
--             newAddTagsToStream
--
--         , requestSubscribeToShard $
--             newSubscribeToShard
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
--         , requestStopStreamEncryption $
--             newStopStreamEncryption
--
--         , requestEnableEnhancedMonitoring $
--             newEnableEnhancedMonitoring
--
--         , requestRegisterStreamConsumer $
--             newRegisterStreamConsumer
--
--         , requestStartStreamEncryption $
--             newStartStreamEncryption
--
--         , requestDescribeLimits $
--             newDescribeLimits
--
--         , requestListStreamConsumers $
--             newListStreamConsumers
--
--         , requestGetShardIterator $
--             newGetShardIterator
--
--         , requestGetRecords $
--             newGetRecords
--
--         , requestDeleteStream $
--             newDeleteStream
--
--         , requestListShards $
--             newListShards
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
--         , requestDecreaseStreamRetentionPeriod $
--             newDecreaseStreamRetentionPeriod
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
--         [ responseAddTagsToStream $
--             newAddTagsToStreamResponse
--
--         , responseSubscribeToShard $
--             newSubscribeToShardResponse
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
--         , responseStopStreamEncryption $
--             newStopStreamEncryptionResponse
--
--         , responseEnableEnhancedMonitoring $
--             newEnhancedMonitoringOutput
--
--         , responseRegisterStreamConsumer $
--             newRegisterStreamConsumerResponse
--
--         , responseStartStreamEncryption $
--             newStartStreamEncryptionResponse
--
--         , responseDescribeLimits $
--             newDescribeLimitsResponse
--
--         , responseListStreamConsumers $
--             newListStreamConsumersResponse
--
--         , responseGetShardIterator $
--             newGetShardIteratorResponse
--
--         , responseGetRecords $
--             newGetRecordsResponse
--
--         , responseDeleteStream $
--             newDeleteStreamResponse
--
--         , responseListShards $
--             newListShardsResponse
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
--         , responseDecreaseStreamRetentionPeriod $
--             newDecreaseStreamRetentionPeriodResponse
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

requestAddTagsToStream :: AddTagsToStream -> TestTree
requestAddTagsToStream =
  req
    "AddTagsToStream"
    "fixture/AddTagsToStream.yaml"

requestSubscribeToShard :: SubscribeToShard -> TestTree
requestSubscribeToShard =
  req
    "SubscribeToShard"
    "fixture/SubscribeToShard.yaml"

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

requestDescribeLimits :: DescribeLimits -> TestTree
requestDescribeLimits =
  req
    "DescribeLimits"
    "fixture/DescribeLimits.yaml"

requestListStreamConsumers :: ListStreamConsumers -> TestTree
requestListStreamConsumers =
  req
    "ListStreamConsumers"
    "fixture/ListStreamConsumers.yaml"

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

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream =
  req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestListShards :: ListShards -> TestTree
requestListShards =
  req
    "ListShards"
    "fixture/ListShards.yaml"

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

requestDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriod -> TestTree
requestDecreaseStreamRetentionPeriod =
  req
    "DecreaseStreamRetentionPeriod"
    "fixture/DecreaseStreamRetentionPeriod.yaml"

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

responseStopStreamEncryption :: StopStreamEncryptionResponse -> TestTree
responseStopStreamEncryption =
  res
    "StopStreamEncryptionResponse"
    "fixture/StopStreamEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy StopStreamEncryption)

responseEnableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseEnableEnhancedMonitoring =
  res
    "EnableEnhancedMonitoringResponse"
    "fixture/EnableEnhancedMonitoringResponse.proto"
    defaultService
    (Proxy :: Proxy EnableEnhancedMonitoring)

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

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits =
  res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLimits)

responseListStreamConsumers :: ListStreamConsumersResponse -> TestTree
responseListStreamConsumers =
  res
    "ListStreamConsumersResponse"
    "fixture/ListStreamConsumersResponse.proto"
    defaultService
    (Proxy :: Proxy ListStreamConsumers)

responseGetShardIterator :: GetShardIteratorResponse -> TestTree
responseGetShardIterator =
  res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse.proto"
    defaultService
    (Proxy :: Proxy GetShardIterator)

responseGetRecords :: GetRecordsResponse -> TestTree
responseGetRecords =
  res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRecords)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStream)

responseListShards :: ListShardsResponse -> TestTree
responseListShards =
  res
    "ListShardsResponse"
    "fixture/ListShardsResponse.proto"
    defaultService
    (Proxy :: Proxy ListShards)

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

responseDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriodResponse -> TestTree
responseDecreaseStreamRetentionPeriod =
  res
    "DecreaseStreamRetentionPeriodResponse"
    "fixture/DecreaseStreamRetentionPeriodResponse.proto"
    defaultService
    (Proxy :: Proxy DecreaseStreamRetentionPeriod)

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
