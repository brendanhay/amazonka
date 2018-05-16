{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Inspector
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Inspector where

import Data.Proxy
import Network.AWS.Inspector
import Test.AWS.Fixture
import Test.AWS.Inspector.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetTelemetryMetadata $
--             getTelemetryMetadata
--
--         , requestListFindings $
--             listFindings
--
--         , requestListAssessmentTemplates $
--             listAssessmentTemplates
--
--         , requestSubscribeToEvent $
--             subscribeToEvent
--
--         , requestListAssessmentRunAgents $
--             listAssessmentRunAgents
--
--         , requestStartAssessmentRun $
--             startAssessmentRun
--
--         , requestDeleteAssessmentTemplate $
--             deleteAssessmentTemplate
--
--         , requestCreateAssessmentTemplate $
--             createAssessmentTemplate
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestSetTagsForResource $
--             setTagsForResource
--
--         , requestDescribeCrossAccountAccessRole $
--             describeCrossAccountAccessRole
--
--         , requestDescribeAssessmentTemplates $
--             describeAssessmentTemplates
--
--         , requestDescribeResourceGroups $
--             describeResourceGroups
--
--         , requestCreateAssessmentTarget $
--             createAssessmentTarget
--
--         , requestListEventSubscriptions $
--             listEventSubscriptions
--
--         , requestRegisterCrossAccountAccessRole $
--             registerCrossAccountAccessRole
--
--         , requestListAssessmentTargets $
--             listAssessmentTargets
--
--         , requestCreateResourceGroup $
--             createResourceGroup
--
--         , requestDescribeRulesPackages $
--             describeRulesPackages
--
--         , requestStopAssessmentRun $
--             stopAssessmentRun
--
--         , requestPreviewAgents $
--             previewAgents
--
--         , requestDescribeFindings $
--             describeFindings
--
--         , requestAddAttributesToFindings $
--             addAttributesToFindings
--
--         , requestUpdateAssessmentTarget $
--             updateAssessmentTarget
--
--         , requestDeleteAssessmentTarget $
--             deleteAssessmentTarget
--
--         , requestDeleteAssessmentRun $
--             deleteAssessmentRun
--
--         , requestListAssessmentRuns $
--             listAssessmentRuns
--
--         , requestGetAssessmentReport $
--             getAssessmentReport
--
--         , requestListRulesPackages $
--             listRulesPackages
--
--         , requestDescribeAssessmentRuns $
--             describeAssessmentRuns
--
--         , requestUnsubscribeFromEvent $
--             unsubscribeFromEvent
--
--         , requestRemoveAttributesFromFindings $
--             removeAttributesFromFindings
--
--         , requestDescribeAssessmentTargets $
--             describeAssessmentTargets
--
--           ]

--     , testGroup "response"
--         [ responseGetTelemetryMetadata $
--             getTelemetryMetadataResponse
--
--         , responseListFindings $
--             listFindingsResponse
--
--         , responseListAssessmentTemplates $
--             listAssessmentTemplatesResponse
--
--         , responseSubscribeToEvent $
--             subscribeToEventResponse
--
--         , responseListAssessmentRunAgents $
--             listAssessmentRunAgentsResponse
--
--         , responseStartAssessmentRun $
--             startAssessmentRunResponse
--
--         , responseDeleteAssessmentTemplate $
--             deleteAssessmentTemplateResponse
--
--         , responseCreateAssessmentTemplate $
--             createAssessmentTemplateResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseSetTagsForResource $
--             setTagsForResourceResponse
--
--         , responseDescribeCrossAccountAccessRole $
--             describeCrossAccountAccessRoleResponse
--
--         , responseDescribeAssessmentTemplates $
--             describeAssessmentTemplatesResponse
--
--         , responseDescribeResourceGroups $
--             describeResourceGroupsResponse
--
--         , responseCreateAssessmentTarget $
--             createAssessmentTargetResponse
--
--         , responseListEventSubscriptions $
--             listEventSubscriptionsResponse
--
--         , responseRegisterCrossAccountAccessRole $
--             registerCrossAccountAccessRoleResponse
--
--         , responseListAssessmentTargets $
--             listAssessmentTargetsResponse
--
--         , responseCreateResourceGroup $
--             createResourceGroupResponse
--
--         , responseDescribeRulesPackages $
--             describeRulesPackagesResponse
--
--         , responseStopAssessmentRun $
--             stopAssessmentRunResponse
--
--         , responsePreviewAgents $
--             previewAgentsResponse
--
--         , responseDescribeFindings $
--             describeFindingsResponse
--
--         , responseAddAttributesToFindings $
--             addAttributesToFindingsResponse
--
--         , responseUpdateAssessmentTarget $
--             updateAssessmentTargetResponse
--
--         , responseDeleteAssessmentTarget $
--             deleteAssessmentTargetResponse
--
--         , responseDeleteAssessmentRun $
--             deleteAssessmentRunResponse
--
--         , responseListAssessmentRuns $
--             listAssessmentRunsResponse
--
--         , responseGetAssessmentReport $
--             getAssessmentReportResponse
--
--         , responseListRulesPackages $
--             listRulesPackagesResponse
--
--         , responseDescribeAssessmentRuns $
--             describeAssessmentRunsResponse
--
--         , responseUnsubscribeFromEvent $
--             unsubscribeFromEventResponse
--
--         , responseRemoveAttributesFromFindings $
--             removeAttributesFromFindingsResponse
--
--         , responseDescribeAssessmentTargets $
--             describeAssessmentTargetsResponse
--
--           ]
--     ]

-- Requests

requestGetTelemetryMetadata :: GetTelemetryMetadata -> TestTree
requestGetTelemetryMetadata = req
    "GetTelemetryMetadata"
    "fixture/GetTelemetryMetadata.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings = req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestListAssessmentTemplates :: ListAssessmentTemplates -> TestTree
requestListAssessmentTemplates = req
    "ListAssessmentTemplates"
    "fixture/ListAssessmentTemplates.yaml"

requestSubscribeToEvent :: SubscribeToEvent -> TestTree
requestSubscribeToEvent = req
    "SubscribeToEvent"
    "fixture/SubscribeToEvent.yaml"

requestListAssessmentRunAgents :: ListAssessmentRunAgents -> TestTree
requestListAssessmentRunAgents = req
    "ListAssessmentRunAgents"
    "fixture/ListAssessmentRunAgents.yaml"

requestStartAssessmentRun :: StartAssessmentRun -> TestTree
requestStartAssessmentRun = req
    "StartAssessmentRun"
    "fixture/StartAssessmentRun.yaml"

requestDeleteAssessmentTemplate :: DeleteAssessmentTemplate -> TestTree
requestDeleteAssessmentTemplate = req
    "DeleteAssessmentTemplate"
    "fixture/DeleteAssessmentTemplate.yaml"

requestCreateAssessmentTemplate :: CreateAssessmentTemplate -> TestTree
requestCreateAssessmentTemplate = req
    "CreateAssessmentTemplate"
    "fixture/CreateAssessmentTemplate.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSetTagsForResource :: SetTagsForResource -> TestTree
requestSetTagsForResource = req
    "SetTagsForResource"
    "fixture/SetTagsForResource.yaml"

requestDescribeCrossAccountAccessRole :: DescribeCrossAccountAccessRole -> TestTree
requestDescribeCrossAccountAccessRole = req
    "DescribeCrossAccountAccessRole"
    "fixture/DescribeCrossAccountAccessRole.yaml"

requestDescribeAssessmentTemplates :: DescribeAssessmentTemplates -> TestTree
requestDescribeAssessmentTemplates = req
    "DescribeAssessmentTemplates"
    "fixture/DescribeAssessmentTemplates.yaml"

requestDescribeResourceGroups :: DescribeResourceGroups -> TestTree
requestDescribeResourceGroups = req
    "DescribeResourceGroups"
    "fixture/DescribeResourceGroups.yaml"

requestCreateAssessmentTarget :: CreateAssessmentTarget -> TestTree
requestCreateAssessmentTarget = req
    "CreateAssessmentTarget"
    "fixture/CreateAssessmentTarget.yaml"

requestListEventSubscriptions :: ListEventSubscriptions -> TestTree
requestListEventSubscriptions = req
    "ListEventSubscriptions"
    "fixture/ListEventSubscriptions.yaml"

requestRegisterCrossAccountAccessRole :: RegisterCrossAccountAccessRole -> TestTree
requestRegisterCrossAccountAccessRole = req
    "RegisterCrossAccountAccessRole"
    "fixture/RegisterCrossAccountAccessRole.yaml"

requestListAssessmentTargets :: ListAssessmentTargets -> TestTree
requestListAssessmentTargets = req
    "ListAssessmentTargets"
    "fixture/ListAssessmentTargets.yaml"

requestCreateResourceGroup :: CreateResourceGroup -> TestTree
requestCreateResourceGroup = req
    "CreateResourceGroup"
    "fixture/CreateResourceGroup.yaml"

requestDescribeRulesPackages :: DescribeRulesPackages -> TestTree
requestDescribeRulesPackages = req
    "DescribeRulesPackages"
    "fixture/DescribeRulesPackages.yaml"

requestStopAssessmentRun :: StopAssessmentRun -> TestTree
requestStopAssessmentRun = req
    "StopAssessmentRun"
    "fixture/StopAssessmentRun.yaml"

requestPreviewAgents :: PreviewAgents -> TestTree
requestPreviewAgents = req
    "PreviewAgents"
    "fixture/PreviewAgents.yaml"

requestDescribeFindings :: DescribeFindings -> TestTree
requestDescribeFindings = req
    "DescribeFindings"
    "fixture/DescribeFindings.yaml"

requestAddAttributesToFindings :: AddAttributesToFindings -> TestTree
requestAddAttributesToFindings = req
    "AddAttributesToFindings"
    "fixture/AddAttributesToFindings.yaml"

requestUpdateAssessmentTarget :: UpdateAssessmentTarget -> TestTree
requestUpdateAssessmentTarget = req
    "UpdateAssessmentTarget"
    "fixture/UpdateAssessmentTarget.yaml"

requestDeleteAssessmentTarget :: DeleteAssessmentTarget -> TestTree
requestDeleteAssessmentTarget = req
    "DeleteAssessmentTarget"
    "fixture/DeleteAssessmentTarget.yaml"

requestDeleteAssessmentRun :: DeleteAssessmentRun -> TestTree
requestDeleteAssessmentRun = req
    "DeleteAssessmentRun"
    "fixture/DeleteAssessmentRun.yaml"

requestListAssessmentRuns :: ListAssessmentRuns -> TestTree
requestListAssessmentRuns = req
    "ListAssessmentRuns"
    "fixture/ListAssessmentRuns.yaml"

requestGetAssessmentReport :: GetAssessmentReport -> TestTree
requestGetAssessmentReport = req
    "GetAssessmentReport"
    "fixture/GetAssessmentReport.yaml"

requestListRulesPackages :: ListRulesPackages -> TestTree
requestListRulesPackages = req
    "ListRulesPackages"
    "fixture/ListRulesPackages.yaml"

requestDescribeAssessmentRuns :: DescribeAssessmentRuns -> TestTree
requestDescribeAssessmentRuns = req
    "DescribeAssessmentRuns"
    "fixture/DescribeAssessmentRuns.yaml"

requestUnsubscribeFromEvent :: UnsubscribeFromEvent -> TestTree
requestUnsubscribeFromEvent = req
    "UnsubscribeFromEvent"
    "fixture/UnsubscribeFromEvent.yaml"

requestRemoveAttributesFromFindings :: RemoveAttributesFromFindings -> TestTree
requestRemoveAttributesFromFindings = req
    "RemoveAttributesFromFindings"
    "fixture/RemoveAttributesFromFindings.yaml"

requestDescribeAssessmentTargets :: DescribeAssessmentTargets -> TestTree
requestDescribeAssessmentTargets = req
    "DescribeAssessmentTargets"
    "fixture/DescribeAssessmentTargets.yaml"

-- Responses

responseGetTelemetryMetadata :: GetTelemetryMetadataResponse -> TestTree
responseGetTelemetryMetadata = res
    "GetTelemetryMetadataResponse"
    "fixture/GetTelemetryMetadataResponse.proto"
    inspector
    (Proxy :: Proxy GetTelemetryMetadata)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings = res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    inspector
    (Proxy :: Proxy ListFindings)

responseListAssessmentTemplates :: ListAssessmentTemplatesResponse -> TestTree
responseListAssessmentTemplates = res
    "ListAssessmentTemplatesResponse"
    "fixture/ListAssessmentTemplatesResponse.proto"
    inspector
    (Proxy :: Proxy ListAssessmentTemplates)

responseSubscribeToEvent :: SubscribeToEventResponse -> TestTree
responseSubscribeToEvent = res
    "SubscribeToEventResponse"
    "fixture/SubscribeToEventResponse.proto"
    inspector
    (Proxy :: Proxy SubscribeToEvent)

responseListAssessmentRunAgents :: ListAssessmentRunAgentsResponse -> TestTree
responseListAssessmentRunAgents = res
    "ListAssessmentRunAgentsResponse"
    "fixture/ListAssessmentRunAgentsResponse.proto"
    inspector
    (Proxy :: Proxy ListAssessmentRunAgents)

responseStartAssessmentRun :: StartAssessmentRunResponse -> TestTree
responseStartAssessmentRun = res
    "StartAssessmentRunResponse"
    "fixture/StartAssessmentRunResponse.proto"
    inspector
    (Proxy :: Proxy StartAssessmentRun)

responseDeleteAssessmentTemplate :: DeleteAssessmentTemplateResponse -> TestTree
responseDeleteAssessmentTemplate = res
    "DeleteAssessmentTemplateResponse"
    "fixture/DeleteAssessmentTemplateResponse.proto"
    inspector
    (Proxy :: Proxy DeleteAssessmentTemplate)

responseCreateAssessmentTemplate :: CreateAssessmentTemplateResponse -> TestTree
responseCreateAssessmentTemplate = res
    "CreateAssessmentTemplateResponse"
    "fixture/CreateAssessmentTemplateResponse.proto"
    inspector
    (Proxy :: Proxy CreateAssessmentTemplate)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    inspector
    (Proxy :: Proxy ListTagsForResource)

responseSetTagsForResource :: SetTagsForResourceResponse -> TestTree
responseSetTagsForResource = res
    "SetTagsForResourceResponse"
    "fixture/SetTagsForResourceResponse.proto"
    inspector
    (Proxy :: Proxy SetTagsForResource)

responseDescribeCrossAccountAccessRole :: DescribeCrossAccountAccessRoleResponse -> TestTree
responseDescribeCrossAccountAccessRole = res
    "DescribeCrossAccountAccessRoleResponse"
    "fixture/DescribeCrossAccountAccessRoleResponse.proto"
    inspector
    (Proxy :: Proxy DescribeCrossAccountAccessRole)

responseDescribeAssessmentTemplates :: DescribeAssessmentTemplatesResponse -> TestTree
responseDescribeAssessmentTemplates = res
    "DescribeAssessmentTemplatesResponse"
    "fixture/DescribeAssessmentTemplatesResponse.proto"
    inspector
    (Proxy :: Proxy DescribeAssessmentTemplates)

responseDescribeResourceGroups :: DescribeResourceGroupsResponse -> TestTree
responseDescribeResourceGroups = res
    "DescribeResourceGroupsResponse"
    "fixture/DescribeResourceGroupsResponse.proto"
    inspector
    (Proxy :: Proxy DescribeResourceGroups)

responseCreateAssessmentTarget :: CreateAssessmentTargetResponse -> TestTree
responseCreateAssessmentTarget = res
    "CreateAssessmentTargetResponse"
    "fixture/CreateAssessmentTargetResponse.proto"
    inspector
    (Proxy :: Proxy CreateAssessmentTarget)

responseListEventSubscriptions :: ListEventSubscriptionsResponse -> TestTree
responseListEventSubscriptions = res
    "ListEventSubscriptionsResponse"
    "fixture/ListEventSubscriptionsResponse.proto"
    inspector
    (Proxy :: Proxy ListEventSubscriptions)

responseRegisterCrossAccountAccessRole :: RegisterCrossAccountAccessRoleResponse -> TestTree
responseRegisterCrossAccountAccessRole = res
    "RegisterCrossAccountAccessRoleResponse"
    "fixture/RegisterCrossAccountAccessRoleResponse.proto"
    inspector
    (Proxy :: Proxy RegisterCrossAccountAccessRole)

responseListAssessmentTargets :: ListAssessmentTargetsResponse -> TestTree
responseListAssessmentTargets = res
    "ListAssessmentTargetsResponse"
    "fixture/ListAssessmentTargetsResponse.proto"
    inspector
    (Proxy :: Proxy ListAssessmentTargets)

responseCreateResourceGroup :: CreateResourceGroupResponse -> TestTree
responseCreateResourceGroup = res
    "CreateResourceGroupResponse"
    "fixture/CreateResourceGroupResponse.proto"
    inspector
    (Proxy :: Proxy CreateResourceGroup)

responseDescribeRulesPackages :: DescribeRulesPackagesResponse -> TestTree
responseDescribeRulesPackages = res
    "DescribeRulesPackagesResponse"
    "fixture/DescribeRulesPackagesResponse.proto"
    inspector
    (Proxy :: Proxy DescribeRulesPackages)

responseStopAssessmentRun :: StopAssessmentRunResponse -> TestTree
responseStopAssessmentRun = res
    "StopAssessmentRunResponse"
    "fixture/StopAssessmentRunResponse.proto"
    inspector
    (Proxy :: Proxy StopAssessmentRun)

responsePreviewAgents :: PreviewAgentsResponse -> TestTree
responsePreviewAgents = res
    "PreviewAgentsResponse"
    "fixture/PreviewAgentsResponse.proto"
    inspector
    (Proxy :: Proxy PreviewAgents)

responseDescribeFindings :: DescribeFindingsResponse -> TestTree
responseDescribeFindings = res
    "DescribeFindingsResponse"
    "fixture/DescribeFindingsResponse.proto"
    inspector
    (Proxy :: Proxy DescribeFindings)

responseAddAttributesToFindings :: AddAttributesToFindingsResponse -> TestTree
responseAddAttributesToFindings = res
    "AddAttributesToFindingsResponse"
    "fixture/AddAttributesToFindingsResponse.proto"
    inspector
    (Proxy :: Proxy AddAttributesToFindings)

responseUpdateAssessmentTarget :: UpdateAssessmentTargetResponse -> TestTree
responseUpdateAssessmentTarget = res
    "UpdateAssessmentTargetResponse"
    "fixture/UpdateAssessmentTargetResponse.proto"
    inspector
    (Proxy :: Proxy UpdateAssessmentTarget)

responseDeleteAssessmentTarget :: DeleteAssessmentTargetResponse -> TestTree
responseDeleteAssessmentTarget = res
    "DeleteAssessmentTargetResponse"
    "fixture/DeleteAssessmentTargetResponse.proto"
    inspector
    (Proxy :: Proxy DeleteAssessmentTarget)

responseDeleteAssessmentRun :: DeleteAssessmentRunResponse -> TestTree
responseDeleteAssessmentRun = res
    "DeleteAssessmentRunResponse"
    "fixture/DeleteAssessmentRunResponse.proto"
    inspector
    (Proxy :: Proxy DeleteAssessmentRun)

responseListAssessmentRuns :: ListAssessmentRunsResponse -> TestTree
responseListAssessmentRuns = res
    "ListAssessmentRunsResponse"
    "fixture/ListAssessmentRunsResponse.proto"
    inspector
    (Proxy :: Proxy ListAssessmentRuns)

responseGetAssessmentReport :: GetAssessmentReportResponse -> TestTree
responseGetAssessmentReport = res
    "GetAssessmentReportResponse"
    "fixture/GetAssessmentReportResponse.proto"
    inspector
    (Proxy :: Proxy GetAssessmentReport)

responseListRulesPackages :: ListRulesPackagesResponse -> TestTree
responseListRulesPackages = res
    "ListRulesPackagesResponse"
    "fixture/ListRulesPackagesResponse.proto"
    inspector
    (Proxy :: Proxy ListRulesPackages)

responseDescribeAssessmentRuns :: DescribeAssessmentRunsResponse -> TestTree
responseDescribeAssessmentRuns = res
    "DescribeAssessmentRunsResponse"
    "fixture/DescribeAssessmentRunsResponse.proto"
    inspector
    (Proxy :: Proxy DescribeAssessmentRuns)

responseUnsubscribeFromEvent :: UnsubscribeFromEventResponse -> TestTree
responseUnsubscribeFromEvent = res
    "UnsubscribeFromEventResponse"
    "fixture/UnsubscribeFromEventResponse.proto"
    inspector
    (Proxy :: Proxy UnsubscribeFromEvent)

responseRemoveAttributesFromFindings :: RemoveAttributesFromFindingsResponse -> TestTree
responseRemoveAttributesFromFindings = res
    "RemoveAttributesFromFindingsResponse"
    "fixture/RemoveAttributesFromFindingsResponse.proto"
    inspector
    (Proxy :: Proxy RemoveAttributesFromFindings)

responseDescribeAssessmentTargets :: DescribeAssessmentTargetsResponse -> TestTree
responseDescribeAssessmentTargets = res
    "DescribeAssessmentTargetsResponse"
    "fixture/DescribeAssessmentTargetsResponse.proto"
    inspector
    (Proxy :: Proxy DescribeAssessmentTargets)
