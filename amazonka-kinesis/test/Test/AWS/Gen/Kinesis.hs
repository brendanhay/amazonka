{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Kinesis
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             putRecord
--
--         , requestDecreaseStreamRetentionPeriod $
--             decreaseStreamRetentionPeriod
--
--         , requestMergeShards $
--             mergeShards
--
--         , requestDescribeStreamSummary $
--             describeStreamSummary
--
--         , requestGetShardIterator $
--             getShardIterator
--
--         , requestGetRecords $
--             getRecords
--
--         , requestStopStreamEncryption $
--             stopStreamEncryption
--
--         , requestEnableEnhancedMonitoring $
--             enableEnhancedMonitoring
--
--         , requestDescribeLimits $
--             describeLimits
--
--         , requestDisableEnhancedMonitoring $
--             disableEnhancedMonitoring
--
--         , requestUpdateShardCount $
--             updateShardCount
--
--         , requestListTagsForStream $
--             listTagsForStream
--
--         , requestAddTagsToStream $
--             addTagsToStream
--
--         , requestPutRecords $
--             putRecords
--
--         , requestListShards $
--             listShards
--
--         , requestDeleteStream $
--             deleteStream
--
--         , requestRemoveTagsFromStream $
--             removeTagsFromStream
--
--         , requestListStreams $
--             listStreams
--
--         , requestCreateStream $
--             createStream
--
--         , requestStartStreamEncryption $
--             startStreamEncryption
--
--         , requestSplitShard $
--             splitShard
--
--         , requestIncreaseStreamRetentionPeriod $
--             increaseStreamRetentionPeriod
--
--         , requestDescribeStream $
--             describeStream
--
--           ]

--     , testGroup "response"
--         [ responsePutRecord $
--             putRecordResponse
--
--         , responseDecreaseStreamRetentionPeriod $
--             decreaseStreamRetentionPeriodResponse
--
--         , responseMergeShards $
--             mergeShardsResponse
--
--         , responseDescribeStreamSummary $
--             describeStreamSummaryResponse
--
--         , responseGetShardIterator $
--             getShardIteratorResponse
--
--         , responseGetRecords $
--             getRecordsResponse
--
--         , responseStopStreamEncryption $
--             stopStreamEncryptionResponse
--
--         , responseEnableEnhancedMonitoring $
--             enhancedMonitoringOutput
--
--         , responseDescribeLimits $
--             describeLimitsResponse
--
--         , responseDisableEnhancedMonitoring $
--             enhancedMonitoringOutput
--
--         , responseUpdateShardCount $
--             updateShardCountResponse
--
--         , responseListTagsForStream $
--             listTagsForStreamResponse
--
--         , responseAddTagsToStream $
--             addTagsToStreamResponse
--
--         , responsePutRecords $
--             putRecordsResponse
--
--         , responseListShards $
--             listShardsResponse
--
--         , responseDeleteStream $
--             deleteStreamResponse
--
--         , responseRemoveTagsFromStream $
--             removeTagsFromStreamResponse
--
--         , responseListStreams $
--             listStreamsResponse
--
--         , responseCreateStream $
--             createStreamResponse
--
--         , responseStartStreamEncryption $
--             startStreamEncryptionResponse
--
--         , responseSplitShard $
--             splitShardResponse
--
--         , responseIncreaseStreamRetentionPeriod $
--             increaseStreamRetentionPeriodResponse
--
--         , responseDescribeStream $
--             describeStreamResponse
--
--           ]
--     ]

-- Requests

requestPutRecord :: PutRecord -> TestTree
requestPutRecord = req
    "PutRecord"
    "fixture/PutRecord.yaml"

requestDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriod -> TestTree
requestDecreaseStreamRetentionPeriod = req
    "DecreaseStreamRetentionPeriod"
    "fixture/DecreaseStreamRetentionPeriod.yaml"

requestMergeShards :: MergeShards -> TestTree
requestMergeShards = req
    "MergeShards"
    "fixture/MergeShards.yaml"

requestDescribeStreamSummary :: DescribeStreamSummary -> TestTree
requestDescribeStreamSummary = req
    "DescribeStreamSummary"
    "fixture/DescribeStreamSummary.yaml"

requestGetShardIterator :: GetShardIterator -> TestTree
requestGetShardIterator = req
    "GetShardIterator"
    "fixture/GetShardIterator.yaml"

requestGetRecords :: GetRecords -> TestTree
requestGetRecords = req
    "GetRecords"
    "fixture/GetRecords.yaml"

requestStopStreamEncryption :: StopStreamEncryption -> TestTree
requestStopStreamEncryption = req
    "StopStreamEncryption"
    "fixture/StopStreamEncryption.yaml"

requestEnableEnhancedMonitoring :: EnableEnhancedMonitoring -> TestTree
requestEnableEnhancedMonitoring = req
    "EnableEnhancedMonitoring"
    "fixture/EnableEnhancedMonitoring.yaml"

requestDescribeLimits :: DescribeLimits -> TestTree
requestDescribeLimits = req
    "DescribeLimits"
    "fixture/DescribeLimits.yaml"

requestDisableEnhancedMonitoring :: DisableEnhancedMonitoring -> TestTree
requestDisableEnhancedMonitoring = req
    "DisableEnhancedMonitoring"
    "fixture/DisableEnhancedMonitoring.yaml"

requestUpdateShardCount :: UpdateShardCount -> TestTree
requestUpdateShardCount = req
    "UpdateShardCount"
    "fixture/UpdateShardCount.yaml"

requestListTagsForStream :: ListTagsForStream -> TestTree
requestListTagsForStream = req
    "ListTagsForStream"
    "fixture/ListTagsForStream.yaml"

requestAddTagsToStream :: AddTagsToStream -> TestTree
requestAddTagsToStream = req
    "AddTagsToStream"
    "fixture/AddTagsToStream.yaml"

requestPutRecords :: PutRecords -> TestTree
requestPutRecords = req
    "PutRecords"
    "fixture/PutRecords.yaml"

requestListShards :: ListShards -> TestTree
requestListShards = req
    "ListShards"
    "fixture/ListShards.yaml"

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream = req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestRemoveTagsFromStream :: RemoveTagsFromStream -> TestTree
requestRemoveTagsFromStream = req
    "RemoveTagsFromStream"
    "fixture/RemoveTagsFromStream.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams = req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream = req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestStartStreamEncryption :: StartStreamEncryption -> TestTree
requestStartStreamEncryption = req
    "StartStreamEncryption"
    "fixture/StartStreamEncryption.yaml"

requestSplitShard :: SplitShard -> TestTree
requestSplitShard = req
    "SplitShard"
    "fixture/SplitShard.yaml"

requestIncreaseStreamRetentionPeriod :: IncreaseStreamRetentionPeriod -> TestTree
requestIncreaseStreamRetentionPeriod = req
    "IncreaseStreamRetentionPeriod"
    "fixture/IncreaseStreamRetentionPeriod.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream = req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

-- Responses

responsePutRecord :: PutRecordResponse -> TestTree
responsePutRecord = res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    kinesis
    (Proxy :: Proxy PutRecord)

responseDecreaseStreamRetentionPeriod :: DecreaseStreamRetentionPeriodResponse -> TestTree
responseDecreaseStreamRetentionPeriod = res
    "DecreaseStreamRetentionPeriodResponse"
    "fixture/DecreaseStreamRetentionPeriodResponse.proto"
    kinesis
    (Proxy :: Proxy DecreaseStreamRetentionPeriod)

responseMergeShards :: MergeShardsResponse -> TestTree
responseMergeShards = res
    "MergeShardsResponse"
    "fixture/MergeShardsResponse.proto"
    kinesis
    (Proxy :: Proxy MergeShards)

responseDescribeStreamSummary :: DescribeStreamSummaryResponse -> TestTree
responseDescribeStreamSummary = res
    "DescribeStreamSummaryResponse"
    "fixture/DescribeStreamSummaryResponse.proto"
    kinesis
    (Proxy :: Proxy DescribeStreamSummary)

responseGetShardIterator :: GetShardIteratorResponse -> TestTree
responseGetShardIterator = res
    "GetShardIteratorResponse"
    "fixture/GetShardIteratorResponse.proto"
    kinesis
    (Proxy :: Proxy GetShardIterator)

responseGetRecords :: GetRecordsResponse -> TestTree
responseGetRecords = res
    "GetRecordsResponse"
    "fixture/GetRecordsResponse.proto"
    kinesis
    (Proxy :: Proxy GetRecords)

responseStopStreamEncryption :: StopStreamEncryptionResponse -> TestTree
responseStopStreamEncryption = res
    "StopStreamEncryptionResponse"
    "fixture/StopStreamEncryptionResponse.proto"
    kinesis
    (Proxy :: Proxy StopStreamEncryption)

responseEnableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseEnableEnhancedMonitoring = res
    "EnableEnhancedMonitoringResponse"
    "fixture/EnableEnhancedMonitoringResponse.proto"
    kinesis
    (Proxy :: Proxy EnableEnhancedMonitoring)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits = res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    kinesis
    (Proxy :: Proxy DescribeLimits)

responseDisableEnhancedMonitoring :: EnhancedMonitoringOutput -> TestTree
responseDisableEnhancedMonitoring = res
    "DisableEnhancedMonitoringResponse"
    "fixture/DisableEnhancedMonitoringResponse.proto"
    kinesis
    (Proxy :: Proxy DisableEnhancedMonitoring)

responseUpdateShardCount :: UpdateShardCountResponse -> TestTree
responseUpdateShardCount = res
    "UpdateShardCountResponse"
    "fixture/UpdateShardCountResponse.proto"
    kinesis
    (Proxy :: Proxy UpdateShardCount)

responseListTagsForStream :: ListTagsForStreamResponse -> TestTree
responseListTagsForStream = res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    kinesis
    (Proxy :: Proxy ListTagsForStream)

responseAddTagsToStream :: AddTagsToStreamResponse -> TestTree
responseAddTagsToStream = res
    "AddTagsToStreamResponse"
    "fixture/AddTagsToStreamResponse.proto"
    kinesis
    (Proxy :: Proxy AddTagsToStream)

responsePutRecords :: PutRecordsResponse -> TestTree
responsePutRecords = res
    "PutRecordsResponse"
    "fixture/PutRecordsResponse.proto"
    kinesis
    (Proxy :: Proxy PutRecords)

responseListShards :: ListShardsResponse -> TestTree
responseListShards = res
    "ListShardsResponse"
    "fixture/ListShardsResponse.proto"
    kinesis
    (Proxy :: Proxy ListShards)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream = res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    kinesis
    (Proxy :: Proxy DeleteStream)

responseRemoveTagsFromStream :: RemoveTagsFromStreamResponse -> TestTree
responseRemoveTagsFromStream = res
    "RemoveTagsFromStreamResponse"
    "fixture/RemoveTagsFromStreamResponse.proto"
    kinesis
    (Proxy :: Proxy RemoveTagsFromStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams = res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    kinesis
    (Proxy :: Proxy ListStreams)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream = res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    kinesis
    (Proxy :: Proxy CreateStream)

responseStartStreamEncryption :: StartStreamEncryptionResponse -> TestTree
responseStartStreamEncryption = res
    "StartStreamEncryptionResponse"
    "fixture/StartStreamEncryptionResponse.proto"
    kinesis
    (Proxy :: Proxy StartStreamEncryption)

responseSplitShard :: SplitShardResponse -> TestTree
responseSplitShard = res
    "SplitShardResponse"
    "fixture/SplitShardResponse.proto"
    kinesis
    (Proxy :: Proxy SplitShard)

responseIncreaseStreamRetentionPeriod :: IncreaseStreamRetentionPeriodResponse -> TestTree
responseIncreaseStreamRetentionPeriod = res
    "IncreaseStreamRetentionPeriodResponse"
    "fixture/IncreaseStreamRetentionPeriodResponse.proto"
    kinesis
    (Proxy :: Proxy IncreaseStreamRetentionPeriod)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream = res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    kinesis
    (Proxy :: Proxy DescribeStream)
