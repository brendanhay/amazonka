{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.XRay
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.XRay where

import Amazonka.XRay
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.XRay.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchGetTraces $
--             newBatchGetTraces
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestCreateSamplingRule $
--             newCreateSamplingRule
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteSamplingRule $
--             newDeleteSamplingRule
--
--         , requestGetEncryptionConfig $
--             newGetEncryptionConfig
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestGetGroups $
--             newGetGroups
--
--         , requestGetInsight $
--             newGetInsight
--
--         , requestGetInsightEvents $
--             newGetInsightEvents
--
--         , requestGetInsightImpactGraph $
--             newGetInsightImpactGraph
--
--         , requestGetInsightSummaries $
--             newGetInsightSummaries
--
--         , requestGetSamplingRules $
--             newGetSamplingRules
--
--         , requestGetSamplingStatisticSummaries $
--             newGetSamplingStatisticSummaries
--
--         , requestGetSamplingTargets $
--             newGetSamplingTargets
--
--         , requestGetServiceGraph $
--             newGetServiceGraph
--
--         , requestGetTimeSeriesServiceStatistics $
--             newGetTimeSeriesServiceStatistics
--
--         , requestGetTraceGraph $
--             newGetTraceGraph
--
--         , requestGetTraceSummaries $
--             newGetTraceSummaries
--
--         , requestListResourcePolicies $
--             newListResourcePolicies
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutEncryptionConfig $
--             newPutEncryptionConfig
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestPutTelemetryRecords $
--             newPutTelemetryRecords
--
--         , requestPutTraceSegments $
--             newPutTraceSegments
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestUpdateSamplingRule $
--             newUpdateSamplingRule
--
--           ]

--     , testGroup "response"
--         [ responseBatchGetTraces $
--             newBatchGetTracesResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseCreateSamplingRule $
--             newCreateSamplingRuleResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteSamplingRule $
--             newDeleteSamplingRuleResponse
--
--         , responseGetEncryptionConfig $
--             newGetEncryptionConfigResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseGetGroups $
--             newGetGroupsResponse
--
--         , responseGetInsight $
--             newGetInsightResponse
--
--         , responseGetInsightEvents $
--             newGetInsightEventsResponse
--
--         , responseGetInsightImpactGraph $
--             newGetInsightImpactGraphResponse
--
--         , responseGetInsightSummaries $
--             newGetInsightSummariesResponse
--
--         , responseGetSamplingRules $
--             newGetSamplingRulesResponse
--
--         , responseGetSamplingStatisticSummaries $
--             newGetSamplingStatisticSummariesResponse
--
--         , responseGetSamplingTargets $
--             newGetSamplingTargetsResponse
--
--         , responseGetServiceGraph $
--             newGetServiceGraphResponse
--
--         , responseGetTimeSeriesServiceStatistics $
--             newGetTimeSeriesServiceStatisticsResponse
--
--         , responseGetTraceGraph $
--             newGetTraceGraphResponse
--
--         , responseGetTraceSummaries $
--             newGetTraceSummariesResponse
--
--         , responseListResourcePolicies $
--             newListResourcePoliciesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutEncryptionConfig $
--             newPutEncryptionConfigResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responsePutTelemetryRecords $
--             newPutTelemetryRecordsResponse
--
--         , responsePutTraceSegments $
--             newPutTraceSegmentsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseUpdateSamplingRule $
--             newUpdateSamplingRuleResponse
--
--           ]
--     ]

-- Requests

requestBatchGetTraces :: BatchGetTraces -> TestTree
requestBatchGetTraces =
  req
    "BatchGetTraces"
    "fixture/BatchGetTraces.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestCreateSamplingRule :: CreateSamplingRule -> TestTree
requestCreateSamplingRule =
  req
    "CreateSamplingRule"
    "fixture/CreateSamplingRule.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteSamplingRule :: DeleteSamplingRule -> TestTree
requestDeleteSamplingRule =
  req
    "DeleteSamplingRule"
    "fixture/DeleteSamplingRule.yaml"

requestGetEncryptionConfig :: GetEncryptionConfig -> TestTree
requestGetEncryptionConfig =
  req
    "GetEncryptionConfig"
    "fixture/GetEncryptionConfig.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestGetGroups :: GetGroups -> TestTree
requestGetGroups =
  req
    "GetGroups"
    "fixture/GetGroups.yaml"

requestGetInsight :: GetInsight -> TestTree
requestGetInsight =
  req
    "GetInsight"
    "fixture/GetInsight.yaml"

requestGetInsightEvents :: GetInsightEvents -> TestTree
requestGetInsightEvents =
  req
    "GetInsightEvents"
    "fixture/GetInsightEvents.yaml"

requestGetInsightImpactGraph :: GetInsightImpactGraph -> TestTree
requestGetInsightImpactGraph =
  req
    "GetInsightImpactGraph"
    "fixture/GetInsightImpactGraph.yaml"

requestGetInsightSummaries :: GetInsightSummaries -> TestTree
requestGetInsightSummaries =
  req
    "GetInsightSummaries"
    "fixture/GetInsightSummaries.yaml"

requestGetSamplingRules :: GetSamplingRules -> TestTree
requestGetSamplingRules =
  req
    "GetSamplingRules"
    "fixture/GetSamplingRules.yaml"

requestGetSamplingStatisticSummaries :: GetSamplingStatisticSummaries -> TestTree
requestGetSamplingStatisticSummaries =
  req
    "GetSamplingStatisticSummaries"
    "fixture/GetSamplingStatisticSummaries.yaml"

requestGetSamplingTargets :: GetSamplingTargets -> TestTree
requestGetSamplingTargets =
  req
    "GetSamplingTargets"
    "fixture/GetSamplingTargets.yaml"

requestGetServiceGraph :: GetServiceGraph -> TestTree
requestGetServiceGraph =
  req
    "GetServiceGraph"
    "fixture/GetServiceGraph.yaml"

requestGetTimeSeriesServiceStatistics :: GetTimeSeriesServiceStatistics -> TestTree
requestGetTimeSeriesServiceStatistics =
  req
    "GetTimeSeriesServiceStatistics"
    "fixture/GetTimeSeriesServiceStatistics.yaml"

requestGetTraceGraph :: GetTraceGraph -> TestTree
requestGetTraceGraph =
  req
    "GetTraceGraph"
    "fixture/GetTraceGraph.yaml"

requestGetTraceSummaries :: GetTraceSummaries -> TestTree
requestGetTraceSummaries =
  req
    "GetTraceSummaries"
    "fixture/GetTraceSummaries.yaml"

requestListResourcePolicies :: ListResourcePolicies -> TestTree
requestListResourcePolicies =
  req
    "ListResourcePolicies"
    "fixture/ListResourcePolicies.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutEncryptionConfig :: PutEncryptionConfig -> TestTree
requestPutEncryptionConfig =
  req
    "PutEncryptionConfig"
    "fixture/PutEncryptionConfig.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestPutTelemetryRecords :: PutTelemetryRecords -> TestTree
requestPutTelemetryRecords =
  req
    "PutTelemetryRecords"
    "fixture/PutTelemetryRecords.yaml"

requestPutTraceSegments :: PutTraceSegments -> TestTree
requestPutTraceSegments =
  req
    "PutTraceSegments"
    "fixture/PutTraceSegments.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestUpdateSamplingRule :: UpdateSamplingRule -> TestTree
requestUpdateSamplingRule =
  req
    "UpdateSamplingRule"
    "fixture/UpdateSamplingRule.yaml"

-- Responses

responseBatchGetTraces :: BatchGetTracesResponse -> TestTree
responseBatchGetTraces =
  res
    "BatchGetTracesResponse"
    "fixture/BatchGetTracesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetTraces)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseCreateSamplingRule :: CreateSamplingRuleResponse -> TestTree
responseCreateSamplingRule =
  res
    "CreateSamplingRuleResponse"
    "fixture/CreateSamplingRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSamplingRule)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteSamplingRule :: DeleteSamplingRuleResponse -> TestTree
responseDeleteSamplingRule =
  res
    "DeleteSamplingRuleResponse"
    "fixture/DeleteSamplingRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSamplingRule)

responseGetEncryptionConfig :: GetEncryptionConfigResponse -> TestTree
responseGetEncryptionConfig =
  res
    "GetEncryptionConfigResponse"
    "fixture/GetEncryptionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEncryptionConfig)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroup)

responseGetGroups :: GetGroupsResponse -> TestTree
responseGetGroups =
  res
    "GetGroupsResponse"
    "fixture/GetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroups)

responseGetInsight :: GetInsightResponse -> TestTree
responseGetInsight =
  res
    "GetInsightResponse"
    "fixture/GetInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsight)

responseGetInsightEvents :: GetInsightEventsResponse -> TestTree
responseGetInsightEvents =
  res
    "GetInsightEventsResponse"
    "fixture/GetInsightEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsightEvents)

responseGetInsightImpactGraph :: GetInsightImpactGraphResponse -> TestTree
responseGetInsightImpactGraph =
  res
    "GetInsightImpactGraphResponse"
    "fixture/GetInsightImpactGraphResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsightImpactGraph)

responseGetInsightSummaries :: GetInsightSummariesResponse -> TestTree
responseGetInsightSummaries =
  res
    "GetInsightSummariesResponse"
    "fixture/GetInsightSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsightSummaries)

responseGetSamplingRules :: GetSamplingRulesResponse -> TestTree
responseGetSamplingRules =
  res
    "GetSamplingRulesResponse"
    "fixture/GetSamplingRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSamplingRules)

responseGetSamplingStatisticSummaries :: GetSamplingStatisticSummariesResponse -> TestTree
responseGetSamplingStatisticSummaries =
  res
    "GetSamplingStatisticSummariesResponse"
    "fixture/GetSamplingStatisticSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSamplingStatisticSummaries)

responseGetSamplingTargets :: GetSamplingTargetsResponse -> TestTree
responseGetSamplingTargets =
  res
    "GetSamplingTargetsResponse"
    "fixture/GetSamplingTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSamplingTargets)

responseGetServiceGraph :: GetServiceGraphResponse -> TestTree
responseGetServiceGraph =
  res
    "GetServiceGraphResponse"
    "fixture/GetServiceGraphResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceGraph)

responseGetTimeSeriesServiceStatistics :: GetTimeSeriesServiceStatisticsResponse -> TestTree
responseGetTimeSeriesServiceStatistics =
  res
    "GetTimeSeriesServiceStatisticsResponse"
    "fixture/GetTimeSeriesServiceStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTimeSeriesServiceStatistics)

responseGetTraceGraph :: GetTraceGraphResponse -> TestTree
responseGetTraceGraph =
  res
    "GetTraceGraphResponse"
    "fixture/GetTraceGraphResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTraceGraph)

responseGetTraceSummaries :: GetTraceSummariesResponse -> TestTree
responseGetTraceSummaries =
  res
    "GetTraceSummariesResponse"
    "fixture/GetTraceSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTraceSummaries)

responseListResourcePolicies :: ListResourcePoliciesResponse -> TestTree
responseListResourcePolicies =
  res
    "ListResourcePoliciesResponse"
    "fixture/ListResourcePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourcePolicies)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutEncryptionConfig :: PutEncryptionConfigResponse -> TestTree
responsePutEncryptionConfig =
  res
    "PutEncryptionConfigResponse"
    "fixture/PutEncryptionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEncryptionConfig)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responsePutTelemetryRecords :: PutTelemetryRecordsResponse -> TestTree
responsePutTelemetryRecords =
  res
    "PutTelemetryRecordsResponse"
    "fixture/PutTelemetryRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutTelemetryRecords)

responsePutTraceSegments :: PutTraceSegmentsResponse -> TestTree
responsePutTraceSegments =
  res
    "PutTraceSegmentsResponse"
    "fixture/PutTraceSegmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutTraceSegments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroup)

responseUpdateSamplingRule :: UpdateSamplingRuleResponse -> TestTree
responseUpdateSamplingRule =
  res
    "UpdateSamplingRuleResponse"
    "fixture/UpdateSamplingRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSamplingRule)
