{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Inspector
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Inspector where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Inspector
import Test.AWS.Inspector.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetTelemetryMetadata $
--             getTelemetryMetadata
--
--         , testListFindings $
--             listFindings
--
--         , testListAssessmentTemplates $
--             listAssessmentTemplates
--
--         , testSubscribeToEvent $
--             subscribeToEvent
--
--         , testListAssessmentRunAgents $
--             listAssessmentRunAgents
--
--         , testStartAssessmentRun $
--             startAssessmentRun
--
--         , testDeleteAssessmentTemplate $
--             deleteAssessmentTemplate
--
--         , testCreateAssessmentTemplate $
--             createAssessmentTemplate
--
--         , testListTagsForResource $
--             listTagsForResource
--
--         , testSetTagsForResource $
--             setTagsForResource
--
--         , testDescribeCrossAccountAccessRole $
--             describeCrossAccountAccessRole
--
--         , testDescribeAssessmentTemplates $
--             describeAssessmentTemplates
--
--         , testDescribeResourceGroups $
--             describeResourceGroups
--
--         , testCreateAssessmentTarget $
--             createAssessmentTarget
--
--         , testListEventSubscriptions $
--             listEventSubscriptions
--
--         , testRegisterCrossAccountAccessRole $
--             registerCrossAccountAccessRole
--
--         , testListAssessmentTargets $
--             listAssessmentTargets
--
--         , testCreateResourceGroup $
--             createResourceGroup
--
--         , testDescribeRulesPackages $
--             describeRulesPackages
--
--         , testStopAssessmentRun $
--             stopAssessmentRun
--
--         , testPreviewAgents $
--             previewAgents
--
--         , testDescribeFindings $
--             describeFindings
--
--         , testAddAttributesToFindings $
--             addAttributesToFindings
--
--         , testUpdateAssessmentTarget $
--             updateAssessmentTarget
--
--         , testDeleteAssessmentTarget $
--             deleteAssessmentTarget
--
--         , testDeleteAssessmentRun $
--             deleteAssessmentRun
--
--         , testListAssessmentRuns $
--             listAssessmentRuns
--
--         , testListRulesPackages $
--             listRulesPackages
--
--         , testDescribeAssessmentRuns $
--             describeAssessmentRuns
--
--         , testUnsubscribeFromEvent $
--             unsubscribeFromEvent
--
--         , testRemoveAttributesFromFindings $
--             removeAttributesFromFindings
--
--         , testDescribeAssessmentTargets $
--             describeAssessmentTargets
--
--           ]

--     , testGroup "response"
--         [ testGetTelemetryMetadataResponse $
--             getTelemetryMetadataResponse
--
--         , testListFindingsResponse $
--             listFindingsResponse
--
--         , testListAssessmentTemplatesResponse $
--             listAssessmentTemplatesResponse
--
--         , testSubscribeToEventResponse $
--             subscribeToEventResponse
--
--         , testListAssessmentRunAgentsResponse $
--             listAssessmentRunAgentsResponse
--
--         , testStartAssessmentRunResponse $
--             startAssessmentRunResponse
--
--         , testDeleteAssessmentTemplateResponse $
--             deleteAssessmentTemplateResponse
--
--         , testCreateAssessmentTemplateResponse $
--             createAssessmentTemplateResponse
--
--         , testListTagsForResourceResponse $
--             listTagsForResourceResponse
--
--         , testSetTagsForResourceResponse $
--             setTagsForResourceResponse
--
--         , testDescribeCrossAccountAccessRoleResponse $
--             describeCrossAccountAccessRoleResponse
--
--         , testDescribeAssessmentTemplatesResponse $
--             describeAssessmentTemplatesResponse
--
--         , testDescribeResourceGroupsResponse $
--             describeResourceGroupsResponse
--
--         , testCreateAssessmentTargetResponse $
--             createAssessmentTargetResponse
--
--         , testListEventSubscriptionsResponse $
--             listEventSubscriptionsResponse
--
--         , testRegisterCrossAccountAccessRoleResponse $
--             registerCrossAccountAccessRoleResponse
--
--         , testListAssessmentTargetsResponse $
--             listAssessmentTargetsResponse
--
--         , testCreateResourceGroupResponse $
--             createResourceGroupResponse
--
--         , testDescribeRulesPackagesResponse $
--             describeRulesPackagesResponse
--
--         , testStopAssessmentRunResponse $
--             stopAssessmentRunResponse
--
--         , testPreviewAgentsResponse $
--             previewAgentsResponse
--
--         , testDescribeFindingsResponse $
--             describeFindingsResponse
--
--         , testAddAttributesToFindingsResponse $
--             addAttributesToFindingsResponse
--
--         , testUpdateAssessmentTargetResponse $
--             updateAssessmentTargetResponse
--
--         , testDeleteAssessmentTargetResponse $
--             deleteAssessmentTargetResponse
--
--         , testDeleteAssessmentRunResponse $
--             deleteAssessmentRunResponse
--
--         , testListAssessmentRunsResponse $
--             listAssessmentRunsResponse
--
--         , testListRulesPackagesResponse $
--             listRulesPackagesResponse
--
--         , testDescribeAssessmentRunsResponse $
--             describeAssessmentRunsResponse
--
--         , testUnsubscribeFromEventResponse $
--             unsubscribeFromEventResponse
--
--         , testRemoveAttributesFromFindingsResponse $
--             removeAttributesFromFindingsResponse
--
--         , testDescribeAssessmentTargetsResponse $
--             describeAssessmentTargetsResponse
--
--           ]
--     ]

-- Requests

testGetTelemetryMetadata :: GetTelemetryMetadata -> TestTree
testGetTelemetryMetadata = req
    "GetTelemetryMetadata"
    "fixture/GetTelemetryMetadata.yaml"

testListFindings :: ListFindings -> TestTree
testListFindings = req
    "ListFindings"
    "fixture/ListFindings.yaml"

testListAssessmentTemplates :: ListAssessmentTemplates -> TestTree
testListAssessmentTemplates = req
    "ListAssessmentTemplates"
    "fixture/ListAssessmentTemplates.yaml"

testSubscribeToEvent :: SubscribeToEvent -> TestTree
testSubscribeToEvent = req
    "SubscribeToEvent"
    "fixture/SubscribeToEvent.yaml"

testListAssessmentRunAgents :: ListAssessmentRunAgents -> TestTree
testListAssessmentRunAgents = req
    "ListAssessmentRunAgents"
    "fixture/ListAssessmentRunAgents.yaml"

testStartAssessmentRun :: StartAssessmentRun -> TestTree
testStartAssessmentRun = req
    "StartAssessmentRun"
    "fixture/StartAssessmentRun.yaml"

testDeleteAssessmentTemplate :: DeleteAssessmentTemplate -> TestTree
testDeleteAssessmentTemplate = req
    "DeleteAssessmentTemplate"
    "fixture/DeleteAssessmentTemplate.yaml"

testCreateAssessmentTemplate :: CreateAssessmentTemplate -> TestTree
testCreateAssessmentTemplate = req
    "CreateAssessmentTemplate"
    "fixture/CreateAssessmentTemplate.yaml"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

testSetTagsForResource :: SetTagsForResource -> TestTree
testSetTagsForResource = req
    "SetTagsForResource"
    "fixture/SetTagsForResource.yaml"

testDescribeCrossAccountAccessRole :: DescribeCrossAccountAccessRole -> TestTree
testDescribeCrossAccountAccessRole = req
    "DescribeCrossAccountAccessRole"
    "fixture/DescribeCrossAccountAccessRole.yaml"

testDescribeAssessmentTemplates :: DescribeAssessmentTemplates -> TestTree
testDescribeAssessmentTemplates = req
    "DescribeAssessmentTemplates"
    "fixture/DescribeAssessmentTemplates.yaml"

testDescribeResourceGroups :: DescribeResourceGroups -> TestTree
testDescribeResourceGroups = req
    "DescribeResourceGroups"
    "fixture/DescribeResourceGroups.yaml"

testCreateAssessmentTarget :: CreateAssessmentTarget -> TestTree
testCreateAssessmentTarget = req
    "CreateAssessmentTarget"
    "fixture/CreateAssessmentTarget.yaml"

testListEventSubscriptions :: ListEventSubscriptions -> TestTree
testListEventSubscriptions = req
    "ListEventSubscriptions"
    "fixture/ListEventSubscriptions.yaml"

testRegisterCrossAccountAccessRole :: RegisterCrossAccountAccessRole -> TestTree
testRegisterCrossAccountAccessRole = req
    "RegisterCrossAccountAccessRole"
    "fixture/RegisterCrossAccountAccessRole.yaml"

testListAssessmentTargets :: ListAssessmentTargets -> TestTree
testListAssessmentTargets = req
    "ListAssessmentTargets"
    "fixture/ListAssessmentTargets.yaml"

testCreateResourceGroup :: CreateResourceGroup -> TestTree
testCreateResourceGroup = req
    "CreateResourceGroup"
    "fixture/CreateResourceGroup.yaml"

testDescribeRulesPackages :: DescribeRulesPackages -> TestTree
testDescribeRulesPackages = req
    "DescribeRulesPackages"
    "fixture/DescribeRulesPackages.yaml"

testStopAssessmentRun :: StopAssessmentRun -> TestTree
testStopAssessmentRun = req
    "StopAssessmentRun"
    "fixture/StopAssessmentRun.yaml"

testPreviewAgents :: PreviewAgents -> TestTree
testPreviewAgents = req
    "PreviewAgents"
    "fixture/PreviewAgents.yaml"

testDescribeFindings :: DescribeFindings -> TestTree
testDescribeFindings = req
    "DescribeFindings"
    "fixture/DescribeFindings.yaml"

testAddAttributesToFindings :: AddAttributesToFindings -> TestTree
testAddAttributesToFindings = req
    "AddAttributesToFindings"
    "fixture/AddAttributesToFindings.yaml"

testUpdateAssessmentTarget :: UpdateAssessmentTarget -> TestTree
testUpdateAssessmentTarget = req
    "UpdateAssessmentTarget"
    "fixture/UpdateAssessmentTarget.yaml"

testDeleteAssessmentTarget :: DeleteAssessmentTarget -> TestTree
testDeleteAssessmentTarget = req
    "DeleteAssessmentTarget"
    "fixture/DeleteAssessmentTarget.yaml"

testDeleteAssessmentRun :: DeleteAssessmentRun -> TestTree
testDeleteAssessmentRun = req
    "DeleteAssessmentRun"
    "fixture/DeleteAssessmentRun.yaml"

testListAssessmentRuns :: ListAssessmentRuns -> TestTree
testListAssessmentRuns = req
    "ListAssessmentRuns"
    "fixture/ListAssessmentRuns.yaml"

testListRulesPackages :: ListRulesPackages -> TestTree
testListRulesPackages = req
    "ListRulesPackages"
    "fixture/ListRulesPackages.yaml"

testDescribeAssessmentRuns :: DescribeAssessmentRuns -> TestTree
testDescribeAssessmentRuns = req
    "DescribeAssessmentRuns"
    "fixture/DescribeAssessmentRuns.yaml"

testUnsubscribeFromEvent :: UnsubscribeFromEvent -> TestTree
testUnsubscribeFromEvent = req
    "UnsubscribeFromEvent"
    "fixture/UnsubscribeFromEvent.yaml"

testRemoveAttributesFromFindings :: RemoveAttributesFromFindings -> TestTree
testRemoveAttributesFromFindings = req
    "RemoveAttributesFromFindings"
    "fixture/RemoveAttributesFromFindings.yaml"

testDescribeAssessmentTargets :: DescribeAssessmentTargets -> TestTree
testDescribeAssessmentTargets = req
    "DescribeAssessmentTargets"
    "fixture/DescribeAssessmentTargets.yaml"

-- Responses

testGetTelemetryMetadataResponse :: GetTelemetryMetadataResponse -> TestTree
testGetTelemetryMetadataResponse = res
    "GetTelemetryMetadataResponse"
    "fixture/GetTelemetryMetadataResponse.proto"
    inspector
    (Proxy :: Proxy GetTelemetryMetadata)

testListFindingsResponse :: ListFindingsResponse -> TestTree
testListFindingsResponse = res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    inspector
    (Proxy :: Proxy ListFindings)

testListAssessmentTemplatesResponse :: ListAssessmentTemplatesResponse -> TestTree
testListAssessmentTemplatesResponse = res
    "ListAssessmentTemplatesResponse"
    "fixture/ListAssessmentTemplatesResponse.proto"
    inspector
    (Proxy :: Proxy ListAssessmentTemplates)

testSubscribeToEventResponse :: SubscribeToEventResponse -> TestTree
testSubscribeToEventResponse = res
    "SubscribeToEventResponse"
    "fixture/SubscribeToEventResponse.proto"
    inspector
    (Proxy :: Proxy SubscribeToEvent)

testListAssessmentRunAgentsResponse :: ListAssessmentRunAgentsResponse -> TestTree
testListAssessmentRunAgentsResponse = res
    "ListAssessmentRunAgentsResponse"
    "fixture/ListAssessmentRunAgentsResponse.proto"
    inspector
    (Proxy :: Proxy ListAssessmentRunAgents)

testStartAssessmentRunResponse :: StartAssessmentRunResponse -> TestTree
testStartAssessmentRunResponse = res
    "StartAssessmentRunResponse"
    "fixture/StartAssessmentRunResponse.proto"
    inspector
    (Proxy :: Proxy StartAssessmentRun)

testDeleteAssessmentTemplateResponse :: DeleteAssessmentTemplateResponse -> TestTree
testDeleteAssessmentTemplateResponse = res
    "DeleteAssessmentTemplateResponse"
    "fixture/DeleteAssessmentTemplateResponse.proto"
    inspector
    (Proxy :: Proxy DeleteAssessmentTemplate)

testCreateAssessmentTemplateResponse :: CreateAssessmentTemplateResponse -> TestTree
testCreateAssessmentTemplateResponse = res
    "CreateAssessmentTemplateResponse"
    "fixture/CreateAssessmentTemplateResponse.proto"
    inspector
    (Proxy :: Proxy CreateAssessmentTemplate)

testListTagsForResourceResponse :: ListTagsForResourceResponse -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    inspector
    (Proxy :: Proxy ListTagsForResource)

testSetTagsForResourceResponse :: SetTagsForResourceResponse -> TestTree
testSetTagsForResourceResponse = res
    "SetTagsForResourceResponse"
    "fixture/SetTagsForResourceResponse.proto"
    inspector
    (Proxy :: Proxy SetTagsForResource)

testDescribeCrossAccountAccessRoleResponse :: DescribeCrossAccountAccessRoleResponse -> TestTree
testDescribeCrossAccountAccessRoleResponse = res
    "DescribeCrossAccountAccessRoleResponse"
    "fixture/DescribeCrossAccountAccessRoleResponse.proto"
    inspector
    (Proxy :: Proxy DescribeCrossAccountAccessRole)

testDescribeAssessmentTemplatesResponse :: DescribeAssessmentTemplatesResponse -> TestTree
testDescribeAssessmentTemplatesResponse = res
    "DescribeAssessmentTemplatesResponse"
    "fixture/DescribeAssessmentTemplatesResponse.proto"
    inspector
    (Proxy :: Proxy DescribeAssessmentTemplates)

testDescribeResourceGroupsResponse :: DescribeResourceGroupsResponse -> TestTree
testDescribeResourceGroupsResponse = res
    "DescribeResourceGroupsResponse"
    "fixture/DescribeResourceGroupsResponse.proto"
    inspector
    (Proxy :: Proxy DescribeResourceGroups)

testCreateAssessmentTargetResponse :: CreateAssessmentTargetResponse -> TestTree
testCreateAssessmentTargetResponse = res
    "CreateAssessmentTargetResponse"
    "fixture/CreateAssessmentTargetResponse.proto"
    inspector
    (Proxy :: Proxy CreateAssessmentTarget)

testListEventSubscriptionsResponse :: ListEventSubscriptionsResponse -> TestTree
testListEventSubscriptionsResponse = res
    "ListEventSubscriptionsResponse"
    "fixture/ListEventSubscriptionsResponse.proto"
    inspector
    (Proxy :: Proxy ListEventSubscriptions)

testRegisterCrossAccountAccessRoleResponse :: RegisterCrossAccountAccessRoleResponse -> TestTree
testRegisterCrossAccountAccessRoleResponse = res
    "RegisterCrossAccountAccessRoleResponse"
    "fixture/RegisterCrossAccountAccessRoleResponse.proto"
    inspector
    (Proxy :: Proxy RegisterCrossAccountAccessRole)

testListAssessmentTargetsResponse :: ListAssessmentTargetsResponse -> TestTree
testListAssessmentTargetsResponse = res
    "ListAssessmentTargetsResponse"
    "fixture/ListAssessmentTargetsResponse.proto"
    inspector
    (Proxy :: Proxy ListAssessmentTargets)

testCreateResourceGroupResponse :: CreateResourceGroupResponse -> TestTree
testCreateResourceGroupResponse = res
    "CreateResourceGroupResponse"
    "fixture/CreateResourceGroupResponse.proto"
    inspector
    (Proxy :: Proxy CreateResourceGroup)

testDescribeRulesPackagesResponse :: DescribeRulesPackagesResponse -> TestTree
testDescribeRulesPackagesResponse = res
    "DescribeRulesPackagesResponse"
    "fixture/DescribeRulesPackagesResponse.proto"
    inspector
    (Proxy :: Proxy DescribeRulesPackages)

testStopAssessmentRunResponse :: StopAssessmentRunResponse -> TestTree
testStopAssessmentRunResponse = res
    "StopAssessmentRunResponse"
    "fixture/StopAssessmentRunResponse.proto"
    inspector
    (Proxy :: Proxy StopAssessmentRun)

testPreviewAgentsResponse :: PreviewAgentsResponse -> TestTree
testPreviewAgentsResponse = res
    "PreviewAgentsResponse"
    "fixture/PreviewAgentsResponse.proto"
    inspector
    (Proxy :: Proxy PreviewAgents)

testDescribeFindingsResponse :: DescribeFindingsResponse -> TestTree
testDescribeFindingsResponse = res
    "DescribeFindingsResponse"
    "fixture/DescribeFindingsResponse.proto"
    inspector
    (Proxy :: Proxy DescribeFindings)

testAddAttributesToFindingsResponse :: AddAttributesToFindingsResponse -> TestTree
testAddAttributesToFindingsResponse = res
    "AddAttributesToFindingsResponse"
    "fixture/AddAttributesToFindingsResponse.proto"
    inspector
    (Proxy :: Proxy AddAttributesToFindings)

testUpdateAssessmentTargetResponse :: UpdateAssessmentTargetResponse -> TestTree
testUpdateAssessmentTargetResponse = res
    "UpdateAssessmentTargetResponse"
    "fixture/UpdateAssessmentTargetResponse.proto"
    inspector
    (Proxy :: Proxy UpdateAssessmentTarget)

testDeleteAssessmentTargetResponse :: DeleteAssessmentTargetResponse -> TestTree
testDeleteAssessmentTargetResponse = res
    "DeleteAssessmentTargetResponse"
    "fixture/DeleteAssessmentTargetResponse.proto"
    inspector
    (Proxy :: Proxy DeleteAssessmentTarget)

testDeleteAssessmentRunResponse :: DeleteAssessmentRunResponse -> TestTree
testDeleteAssessmentRunResponse = res
    "DeleteAssessmentRunResponse"
    "fixture/DeleteAssessmentRunResponse.proto"
    inspector
    (Proxy :: Proxy DeleteAssessmentRun)

testListAssessmentRunsResponse :: ListAssessmentRunsResponse -> TestTree
testListAssessmentRunsResponse = res
    "ListAssessmentRunsResponse"
    "fixture/ListAssessmentRunsResponse.proto"
    inspector
    (Proxy :: Proxy ListAssessmentRuns)

testListRulesPackagesResponse :: ListRulesPackagesResponse -> TestTree
testListRulesPackagesResponse = res
    "ListRulesPackagesResponse"
    "fixture/ListRulesPackagesResponse.proto"
    inspector
    (Proxy :: Proxy ListRulesPackages)

testDescribeAssessmentRunsResponse :: DescribeAssessmentRunsResponse -> TestTree
testDescribeAssessmentRunsResponse = res
    "DescribeAssessmentRunsResponse"
    "fixture/DescribeAssessmentRunsResponse.proto"
    inspector
    (Proxy :: Proxy DescribeAssessmentRuns)

testUnsubscribeFromEventResponse :: UnsubscribeFromEventResponse -> TestTree
testUnsubscribeFromEventResponse = res
    "UnsubscribeFromEventResponse"
    "fixture/UnsubscribeFromEventResponse.proto"
    inspector
    (Proxy :: Proxy UnsubscribeFromEvent)

testRemoveAttributesFromFindingsResponse :: RemoveAttributesFromFindingsResponse -> TestTree
testRemoveAttributesFromFindingsResponse = res
    "RemoveAttributesFromFindingsResponse"
    "fixture/RemoveAttributesFromFindingsResponse.proto"
    inspector
    (Proxy :: Proxy RemoveAttributesFromFindings)

testDescribeAssessmentTargetsResponse :: DescribeAssessmentTargetsResponse -> TestTree
testDescribeAssessmentTargetsResponse = res
    "DescribeAssessmentTargetsResponse"
    "fixture/DescribeAssessmentTargetsResponse.proto"
    inspector
    (Proxy :: Proxy DescribeAssessmentTargets)
