{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.XRay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.XRay where

import Data.Proxy
import Network.AWS.XRay
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.XRay.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutEncryptionConfig $
--             putEncryptionConfig
--
--         , requestGetServiceGraph $
--             getServiceGraph
--
--         , requestGetSamplingTargets $
--             getSamplingTargets
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestGetTraceSummaries $
--             getTraceSummaries
--
--         , requestPutTraceSegments $
--             putTraceSegments
--
--         , requestBatchGetTraces $
--             batchGetTraces
--
--         , requestGetInsight $
--             getInsight
--
--         , requestGetTimeSeriesServiceStatistics $
--             getTimeSeriesServiceStatistics
--
--         , requestGetEncryptionConfig $
--             getEncryptionConfig
--
--         , requestGetInsightImpactGraph $
--             getInsightImpactGraph
--
--         , requestUpdateSamplingRule $
--             updateSamplingRule
--
--         , requestDeleteSamplingRule $
--             deleteSamplingRule
--
--         , requestGetInsightEvents $
--             getInsightEvents
--
--         , requestGetGroups $
--             getGroups
--
--         , requestGetInsightSummaries $
--             getInsightSummaries
--
--         , requestPutTelemetryRecords $
--             putTelemetryRecords
--
--         , requestGetSamplingRules $
--             getSamplingRules
--
--         , requestTagResource $
--             tagResource
--
--         , requestGetTraceGraph $
--             getTraceGraph
--
--         , requestCreateGroup $
--             createGroup
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDeleteGroup $
--             deleteGroup
--
--         , requestUpdateGroup $
--             updateGroup
--
--         , requestGetGroup $
--             getGroup
--
--         , requestGetSamplingStatisticSummaries $
--             getSamplingStatisticSummaries
--
--         , requestCreateSamplingRule $
--             createSamplingRule
--
--           ]

--     , testGroup "response"
--         [ responsePutEncryptionConfig $
--             putEncryptionConfigResponse
--
--         , responseGetServiceGraph $
--             getServiceGraphResponse
--
--         , responseGetSamplingTargets $
--             getSamplingTargetsResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseGetTraceSummaries $
--             getTraceSummariesResponse
--
--         , responsePutTraceSegments $
--             putTraceSegmentsResponse
--
--         , responseBatchGetTraces $
--             batchGetTracesResponse
--
--         , responseGetInsight $
--             getInsightResponse
--
--         , responseGetTimeSeriesServiceStatistics $
--             getTimeSeriesServiceStatisticsResponse
--
--         , responseGetEncryptionConfig $
--             getEncryptionConfigResponse
--
--         , responseGetInsightImpactGraph $
--             getInsightImpactGraphResponse
--
--         , responseUpdateSamplingRule $
--             updateSamplingRuleResponse
--
--         , responseDeleteSamplingRule $
--             deleteSamplingRuleResponse
--
--         , responseGetInsightEvents $
--             getInsightEventsResponse
--
--         , responseGetGroups $
--             getGroupsResponse
--
--         , responseGetInsightSummaries $
--             getInsightSummariesResponse
--
--         , responsePutTelemetryRecords $
--             putTelemetryRecordsResponse
--
--         , responseGetSamplingRules $
--             getSamplingRulesResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseGetTraceGraph $
--             getTraceGraphResponse
--
--         , responseCreateGroup $
--             createGroupResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDeleteGroup $
--             deleteGroupResponse
--
--         , responseUpdateGroup $
--             updateGroupResponse
--
--         , responseGetGroup $
--             getGroupResponse
--
--         , responseGetSamplingStatisticSummaries $
--             getSamplingStatisticSummariesResponse
--
--         , responseCreateSamplingRule $
--             createSamplingRuleResponse
--
--           ]
--     ]

-- Requests

requestPutEncryptionConfig :: PutEncryptionConfig -> TestTree
requestPutEncryptionConfig =
  req
    "PutEncryptionConfig"
    "fixture/PutEncryptionConfig.yaml"

requestGetServiceGraph :: GetServiceGraph -> TestTree
requestGetServiceGraph =
  req
    "GetServiceGraph"
    "fixture/GetServiceGraph.yaml"

requestGetSamplingTargets :: GetSamplingTargets -> TestTree
requestGetSamplingTargets =
  req
    "GetSamplingTargets"
    "fixture/GetSamplingTargets.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetTraceSummaries :: GetTraceSummaries -> TestTree
requestGetTraceSummaries =
  req
    "GetTraceSummaries"
    "fixture/GetTraceSummaries.yaml"

requestPutTraceSegments :: PutTraceSegments -> TestTree
requestPutTraceSegments =
  req
    "PutTraceSegments"
    "fixture/PutTraceSegments.yaml"

requestBatchGetTraces :: BatchGetTraces -> TestTree
requestBatchGetTraces =
  req
    "BatchGetTraces"
    "fixture/BatchGetTraces.yaml"

requestGetInsight :: GetInsight -> TestTree
requestGetInsight =
  req
    "GetInsight"
    "fixture/GetInsight.yaml"

requestGetTimeSeriesServiceStatistics :: GetTimeSeriesServiceStatistics -> TestTree
requestGetTimeSeriesServiceStatistics =
  req
    "GetTimeSeriesServiceStatistics"
    "fixture/GetTimeSeriesServiceStatistics.yaml"

requestGetEncryptionConfig :: GetEncryptionConfig -> TestTree
requestGetEncryptionConfig =
  req
    "GetEncryptionConfig"
    "fixture/GetEncryptionConfig.yaml"

requestGetInsightImpactGraph :: GetInsightImpactGraph -> TestTree
requestGetInsightImpactGraph =
  req
    "GetInsightImpactGraph"
    "fixture/GetInsightImpactGraph.yaml"

requestUpdateSamplingRule :: UpdateSamplingRule -> TestTree
requestUpdateSamplingRule =
  req
    "UpdateSamplingRule"
    "fixture/UpdateSamplingRule.yaml"

requestDeleteSamplingRule :: DeleteSamplingRule -> TestTree
requestDeleteSamplingRule =
  req
    "DeleteSamplingRule"
    "fixture/DeleteSamplingRule.yaml"

requestGetInsightEvents :: GetInsightEvents -> TestTree
requestGetInsightEvents =
  req
    "GetInsightEvents"
    "fixture/GetInsightEvents.yaml"

requestGetGroups :: GetGroups -> TestTree
requestGetGroups =
  req
    "GetGroups"
    "fixture/GetGroups.yaml"

requestGetInsightSummaries :: GetInsightSummaries -> TestTree
requestGetInsightSummaries =
  req
    "GetInsightSummaries"
    "fixture/GetInsightSummaries.yaml"

requestPutTelemetryRecords :: PutTelemetryRecords -> TestTree
requestPutTelemetryRecords =
  req
    "PutTelemetryRecords"
    "fixture/PutTelemetryRecords.yaml"

requestGetSamplingRules :: GetSamplingRules -> TestTree
requestGetSamplingRules =
  req
    "GetSamplingRules"
    "fixture/GetSamplingRules.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetTraceGraph :: GetTraceGraph -> TestTree
requestGetTraceGraph =
  req
    "GetTraceGraph"
    "fixture/GetTraceGraph.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestGetSamplingStatisticSummaries :: GetSamplingStatisticSummaries -> TestTree
requestGetSamplingStatisticSummaries =
  req
    "GetSamplingStatisticSummaries"
    "fixture/GetSamplingStatisticSummaries.yaml"

requestCreateSamplingRule :: CreateSamplingRule -> TestTree
requestCreateSamplingRule =
  req
    "CreateSamplingRule"
    "fixture/CreateSamplingRule.yaml"

-- Responses

responsePutEncryptionConfig :: PutEncryptionConfigResponse -> TestTree
responsePutEncryptionConfig =
  res
    "PutEncryptionConfigResponse"
    "fixture/PutEncryptionConfigResponse.proto"
    xRay
    (Proxy :: Proxy PutEncryptionConfig)

responseGetServiceGraph :: GetServiceGraphResponse -> TestTree
responseGetServiceGraph =
  res
    "GetServiceGraphResponse"
    "fixture/GetServiceGraphResponse.proto"
    xRay
    (Proxy :: Proxy GetServiceGraph)

responseGetSamplingTargets :: GetSamplingTargetsResponse -> TestTree
responseGetSamplingTargets =
  res
    "GetSamplingTargetsResponse"
    "fixture/GetSamplingTargetsResponse.proto"
    xRay
    (Proxy :: Proxy GetSamplingTargets)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    xRay
    (Proxy :: Proxy ListTagsForResource)

responseGetTraceSummaries :: GetTraceSummariesResponse -> TestTree
responseGetTraceSummaries =
  res
    "GetTraceSummariesResponse"
    "fixture/GetTraceSummariesResponse.proto"
    xRay
    (Proxy :: Proxy GetTraceSummaries)

responsePutTraceSegments :: PutTraceSegmentsResponse -> TestTree
responsePutTraceSegments =
  res
    "PutTraceSegmentsResponse"
    "fixture/PutTraceSegmentsResponse.proto"
    xRay
    (Proxy :: Proxy PutTraceSegments)

responseBatchGetTraces :: BatchGetTracesResponse -> TestTree
responseBatchGetTraces =
  res
    "BatchGetTracesResponse"
    "fixture/BatchGetTracesResponse.proto"
    xRay
    (Proxy :: Proxy BatchGetTraces)

responseGetInsight :: GetInsightResponse -> TestTree
responseGetInsight =
  res
    "GetInsightResponse"
    "fixture/GetInsightResponse.proto"
    xRay
    (Proxy :: Proxy GetInsight)

responseGetTimeSeriesServiceStatistics :: GetTimeSeriesServiceStatisticsResponse -> TestTree
responseGetTimeSeriesServiceStatistics =
  res
    "GetTimeSeriesServiceStatisticsResponse"
    "fixture/GetTimeSeriesServiceStatisticsResponse.proto"
    xRay
    (Proxy :: Proxy GetTimeSeriesServiceStatistics)

responseGetEncryptionConfig :: GetEncryptionConfigResponse -> TestTree
responseGetEncryptionConfig =
  res
    "GetEncryptionConfigResponse"
    "fixture/GetEncryptionConfigResponse.proto"
    xRay
    (Proxy :: Proxy GetEncryptionConfig)

responseGetInsightImpactGraph :: GetInsightImpactGraphResponse -> TestTree
responseGetInsightImpactGraph =
  res
    "GetInsightImpactGraphResponse"
    "fixture/GetInsightImpactGraphResponse.proto"
    xRay
    (Proxy :: Proxy GetInsightImpactGraph)

responseUpdateSamplingRule :: UpdateSamplingRuleResponse -> TestTree
responseUpdateSamplingRule =
  res
    "UpdateSamplingRuleResponse"
    "fixture/UpdateSamplingRuleResponse.proto"
    xRay
    (Proxy :: Proxy UpdateSamplingRule)

responseDeleteSamplingRule :: DeleteSamplingRuleResponse -> TestTree
responseDeleteSamplingRule =
  res
    "DeleteSamplingRuleResponse"
    "fixture/DeleteSamplingRuleResponse.proto"
    xRay
    (Proxy :: Proxy DeleteSamplingRule)

responseGetInsightEvents :: GetInsightEventsResponse -> TestTree
responseGetInsightEvents =
  res
    "GetInsightEventsResponse"
    "fixture/GetInsightEventsResponse.proto"
    xRay
    (Proxy :: Proxy GetInsightEvents)

responseGetGroups :: GetGroupsResponse -> TestTree
responseGetGroups =
  res
    "GetGroupsResponse"
    "fixture/GetGroupsResponse.proto"
    xRay
    (Proxy :: Proxy GetGroups)

responseGetInsightSummaries :: GetInsightSummariesResponse -> TestTree
responseGetInsightSummaries =
  res
    "GetInsightSummariesResponse"
    "fixture/GetInsightSummariesResponse.proto"
    xRay
    (Proxy :: Proxy GetInsightSummaries)

responsePutTelemetryRecords :: PutTelemetryRecordsResponse -> TestTree
responsePutTelemetryRecords =
  res
    "PutTelemetryRecordsResponse"
    "fixture/PutTelemetryRecordsResponse.proto"
    xRay
    (Proxy :: Proxy PutTelemetryRecords)

responseGetSamplingRules :: GetSamplingRulesResponse -> TestTree
responseGetSamplingRules =
  res
    "GetSamplingRulesResponse"
    "fixture/GetSamplingRulesResponse.proto"
    xRay
    (Proxy :: Proxy GetSamplingRules)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    xRay
    (Proxy :: Proxy TagResource)

responseGetTraceGraph :: GetTraceGraphResponse -> TestTree
responseGetTraceGraph =
  res
    "GetTraceGraphResponse"
    "fixture/GetTraceGraphResponse.proto"
    xRay
    (Proxy :: Proxy GetTraceGraph)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    xRay
    (Proxy :: Proxy CreateGroup)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    xRay
    (Proxy :: Proxy UntagResource)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    xRay
    (Proxy :: Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    xRay
    (Proxy :: Proxy UpdateGroup)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    xRay
    (Proxy :: Proxy GetGroup)

responseGetSamplingStatisticSummaries :: GetSamplingStatisticSummariesResponse -> TestTree
responseGetSamplingStatisticSummaries =
  res
    "GetSamplingStatisticSummariesResponse"
    "fixture/GetSamplingStatisticSummariesResponse.proto"
    xRay
    (Proxy :: Proxy GetSamplingStatisticSummaries)

responseCreateSamplingRule :: CreateSamplingRuleResponse -> TestTree
responseCreateSamplingRule =
  res
    "CreateSamplingRuleResponse"
    "fixture/CreateSamplingRuleResponse.proto"
    xRay
    (Proxy :: Proxy CreateSamplingRule)
