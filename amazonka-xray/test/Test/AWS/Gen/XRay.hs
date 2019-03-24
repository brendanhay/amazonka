{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.XRay
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.XRay where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.XRay
import Test.AWS.XRay.Internal

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
--         , requestGetTraceSummaries $
--             getTraceSummaries
--
--         , requestPutTraceSegments $
--             putTraceSegments
--
--         , requestBatchGetTraces $
--             batchGetTraces
--
--         , requestGetEncryptionConfig $
--             getEncryptionConfig
--
--         , requestUpdateSamplingRule $
--             updateSamplingRule
--
--         , requestDeleteSamplingRule $
--             deleteSamplingRule
--
--         , requestGetGroups $
--             getGroups
--
--         , requestPutTelemetryRecords $
--             putTelemetryRecords
--
--         , requestGetSamplingRules $
--             getSamplingRules
--
--         , requestGetTraceGraph $
--             getTraceGraph
--
--         , requestCreateGroup $
--             createGroup
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
--         , responseGetTraceSummaries $
--             getTraceSummariesResponse
--
--         , responsePutTraceSegments $
--             putTraceSegmentsResponse
--
--         , responseBatchGetTraces $
--             batchGetTracesResponse
--
--         , responseGetEncryptionConfig $
--             getEncryptionConfigResponse
--
--         , responseUpdateSamplingRule $
--             updateSamplingRuleResponse
--
--         , responseDeleteSamplingRule $
--             deleteSamplingRuleResponse
--
--         , responseGetGroups $
--             getGroupsResponse
--
--         , responsePutTelemetryRecords $
--             putTelemetryRecordsResponse
--
--         , responseGetSamplingRules $
--             getSamplingRulesResponse
--
--         , responseGetTraceGraph $
--             getTraceGraphResponse
--
--         , responseCreateGroup $
--             createGroupResponse
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
requestPutEncryptionConfig = req
    "PutEncryptionConfig"
    "fixture/PutEncryptionConfig.yaml"

requestGetServiceGraph :: GetServiceGraph -> TestTree
requestGetServiceGraph = req
    "GetServiceGraph"
    "fixture/GetServiceGraph.yaml"

requestGetSamplingTargets :: GetSamplingTargets -> TestTree
requestGetSamplingTargets = req
    "GetSamplingTargets"
    "fixture/GetSamplingTargets.yaml"

requestGetTraceSummaries :: GetTraceSummaries -> TestTree
requestGetTraceSummaries = req
    "GetTraceSummaries"
    "fixture/GetTraceSummaries.yaml"

requestPutTraceSegments :: PutTraceSegments -> TestTree
requestPutTraceSegments = req
    "PutTraceSegments"
    "fixture/PutTraceSegments.yaml"

requestBatchGetTraces :: BatchGetTraces -> TestTree
requestBatchGetTraces = req
    "BatchGetTraces"
    "fixture/BatchGetTraces.yaml"

requestGetEncryptionConfig :: GetEncryptionConfig -> TestTree
requestGetEncryptionConfig = req
    "GetEncryptionConfig"
    "fixture/GetEncryptionConfig.yaml"

requestUpdateSamplingRule :: UpdateSamplingRule -> TestTree
requestUpdateSamplingRule = req
    "UpdateSamplingRule"
    "fixture/UpdateSamplingRule.yaml"

requestDeleteSamplingRule :: DeleteSamplingRule -> TestTree
requestDeleteSamplingRule = req
    "DeleteSamplingRule"
    "fixture/DeleteSamplingRule.yaml"

requestGetGroups :: GetGroups -> TestTree
requestGetGroups = req
    "GetGroups"
    "fixture/GetGroups.yaml"

requestPutTelemetryRecords :: PutTelemetryRecords -> TestTree
requestPutTelemetryRecords = req
    "PutTelemetryRecords"
    "fixture/PutTelemetryRecords.yaml"

requestGetSamplingRules :: GetSamplingRules -> TestTree
requestGetSamplingRules = req
    "GetSamplingRules"
    "fixture/GetSamplingRules.yaml"

requestGetTraceGraph :: GetTraceGraph -> TestTree
requestGetTraceGraph = req
    "GetTraceGraph"
    "fixture/GetTraceGraph.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup = req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup = req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup = req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup = req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestGetSamplingStatisticSummaries :: GetSamplingStatisticSummaries -> TestTree
requestGetSamplingStatisticSummaries = req
    "GetSamplingStatisticSummaries"
    "fixture/GetSamplingStatisticSummaries.yaml"

requestCreateSamplingRule :: CreateSamplingRule -> TestTree
requestCreateSamplingRule = req
    "CreateSamplingRule"
    "fixture/CreateSamplingRule.yaml"

-- Responses

responsePutEncryptionConfig :: PutEncryptionConfigResponse -> TestTree
responsePutEncryptionConfig = res
    "PutEncryptionConfigResponse"
    "fixture/PutEncryptionConfigResponse.proto"
    xRay
    (Proxy :: Proxy PutEncryptionConfig)

responseGetServiceGraph :: GetServiceGraphResponse -> TestTree
responseGetServiceGraph = res
    "GetServiceGraphResponse"
    "fixture/GetServiceGraphResponse.proto"
    xRay
    (Proxy :: Proxy GetServiceGraph)

responseGetSamplingTargets :: GetSamplingTargetsResponse -> TestTree
responseGetSamplingTargets = res
    "GetSamplingTargetsResponse"
    "fixture/GetSamplingTargetsResponse.proto"
    xRay
    (Proxy :: Proxy GetSamplingTargets)

responseGetTraceSummaries :: GetTraceSummariesResponse -> TestTree
responseGetTraceSummaries = res
    "GetTraceSummariesResponse"
    "fixture/GetTraceSummariesResponse.proto"
    xRay
    (Proxy :: Proxy GetTraceSummaries)

responsePutTraceSegments :: PutTraceSegmentsResponse -> TestTree
responsePutTraceSegments = res
    "PutTraceSegmentsResponse"
    "fixture/PutTraceSegmentsResponse.proto"
    xRay
    (Proxy :: Proxy PutTraceSegments)

responseBatchGetTraces :: BatchGetTracesResponse -> TestTree
responseBatchGetTraces = res
    "BatchGetTracesResponse"
    "fixture/BatchGetTracesResponse.proto"
    xRay
    (Proxy :: Proxy BatchGetTraces)

responseGetEncryptionConfig :: GetEncryptionConfigResponse -> TestTree
responseGetEncryptionConfig = res
    "GetEncryptionConfigResponse"
    "fixture/GetEncryptionConfigResponse.proto"
    xRay
    (Proxy :: Proxy GetEncryptionConfig)

responseUpdateSamplingRule :: UpdateSamplingRuleResponse -> TestTree
responseUpdateSamplingRule = res
    "UpdateSamplingRuleResponse"
    "fixture/UpdateSamplingRuleResponse.proto"
    xRay
    (Proxy :: Proxy UpdateSamplingRule)

responseDeleteSamplingRule :: DeleteSamplingRuleResponse -> TestTree
responseDeleteSamplingRule = res
    "DeleteSamplingRuleResponse"
    "fixture/DeleteSamplingRuleResponse.proto"
    xRay
    (Proxy :: Proxy DeleteSamplingRule)

responseGetGroups :: GetGroupsResponse -> TestTree
responseGetGroups = res
    "GetGroupsResponse"
    "fixture/GetGroupsResponse.proto"
    xRay
    (Proxy :: Proxy GetGroups)

responsePutTelemetryRecords :: PutTelemetryRecordsResponse -> TestTree
responsePutTelemetryRecords = res
    "PutTelemetryRecordsResponse"
    "fixture/PutTelemetryRecordsResponse.proto"
    xRay
    (Proxy :: Proxy PutTelemetryRecords)

responseGetSamplingRules :: GetSamplingRulesResponse -> TestTree
responseGetSamplingRules = res
    "GetSamplingRulesResponse"
    "fixture/GetSamplingRulesResponse.proto"
    xRay
    (Proxy :: Proxy GetSamplingRules)

responseGetTraceGraph :: GetTraceGraphResponse -> TestTree
responseGetTraceGraph = res
    "GetTraceGraphResponse"
    "fixture/GetTraceGraphResponse.proto"
    xRay
    (Proxy :: Proxy GetTraceGraph)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup = res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    xRay
    (Proxy :: Proxy CreateGroup)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup = res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    xRay
    (Proxy :: Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup = res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    xRay
    (Proxy :: Proxy UpdateGroup)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup = res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    xRay
    (Proxy :: Proxy GetGroup)

responseGetSamplingStatisticSummaries :: GetSamplingStatisticSummariesResponse -> TestTree
responseGetSamplingStatisticSummaries = res
    "GetSamplingStatisticSummariesResponse"
    "fixture/GetSamplingStatisticSummariesResponse.proto"
    xRay
    (Proxy :: Proxy GetSamplingStatisticSummaries)

responseCreateSamplingRule :: CreateSamplingRuleResponse -> TestTree
responseCreateSamplingRule = res
    "CreateSamplingRuleResponse"
    "fixture/CreateSamplingRuleResponse.proto"
    xRay
    (Proxy :: Proxy CreateSamplingRule)
