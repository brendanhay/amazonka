{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Inspector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestStartAssessmentRun $
--             newStartAssessmentRun
--
--         , requestDeleteAssessmentTemplate $
--             newDeleteAssessmentTemplate
--
--         , requestSubscribeToEvent $
--             newSubscribeToEvent
--
--         , requestStopAssessmentRun $
--             newStopAssessmentRun
--
--         , requestGetTelemetryMetadata $
--             newGetTelemetryMetadata
--
--         , requestListFindings $
--             newListFindings
--
--         , requestDescribeAssessmentTargets $
--             newDescribeAssessmentTargets
--
--         , requestUpdateAssessmentTarget $
--             newUpdateAssessmentTarget
--
--         , requestListAssessmentTargets $
--             newListAssessmentTargets
--
--         , requestListAssessmentRuns $
--             newListAssessmentRuns
--
--         , requestDeleteAssessmentTarget $
--             newDeleteAssessmentTarget
--
--         , requestAddAttributesToFindings $
--             newAddAttributesToFindings
--
--         , requestCreateAssessmentTarget $
--             newCreateAssessmentTarget
--
--         , requestGetExclusionsPreview $
--             newGetExclusionsPreview
--
--         , requestDescribeResourceGroups $
--             newDescribeResourceGroups
--
--         , requestPreviewAgents $
--             newPreviewAgents
--
--         , requestListExclusions $
--             newListExclusions
--
--         , requestCreateAssessmentTemplate $
--             newCreateAssessmentTemplate
--
--         , requestDescribeCrossAccountAccessRole $
--             newDescribeCrossAccountAccessRole
--
--         , requestSetTagsForResource $
--             newSetTagsForResource
--
--         , requestDescribeExclusions $
--             newDescribeExclusions
--
--         , requestListAssessmentTemplates $
--             newListAssessmentTemplates
--
--         , requestListAssessmentRunAgents $
--             newListAssessmentRunAgents
--
--         , requestDescribeAssessmentRuns $
--             newDescribeAssessmentRuns
--
--         , requestDescribeRulesPackages $
--             newDescribeRulesPackages
--
--         , requestCreateExclusionsPreview $
--             newCreateExclusionsPreview
--
--         , requestCreateResourceGroup $
--             newCreateResourceGroup
--
--         , requestUnsubscribeFromEvent $
--             newUnsubscribeFromEvent
--
--         , requestRemoveAttributesFromFindings $
--             newRemoveAttributesFromFindings
--
--         , requestDeleteAssessmentRun $
--             newDeleteAssessmentRun
--
--         , requestRegisterCrossAccountAccessRole $
--             newRegisterCrossAccountAccessRole
--
--         , requestListEventSubscriptions $
--             newListEventSubscriptions
--
--         , requestGetAssessmentReport $
--             newGetAssessmentReport
--
--         , requestListRulesPackages $
--             newListRulesPackages
--
--         , requestDescribeFindings $
--             newDescribeFindings
--
--         , requestDescribeAssessmentTemplates $
--             newDescribeAssessmentTemplates
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseStartAssessmentRun $
--             newStartAssessmentRunResponse
--
--         , responseDeleteAssessmentTemplate $
--             newDeleteAssessmentTemplateResponse
--
--         , responseSubscribeToEvent $
--             newSubscribeToEventResponse
--
--         , responseStopAssessmentRun $
--             newStopAssessmentRunResponse
--
--         , responseGetTelemetryMetadata $
--             newGetTelemetryMetadataResponse
--
--         , responseListFindings $
--             newListFindingsResponse
--
--         , responseDescribeAssessmentTargets $
--             newDescribeAssessmentTargetsResponse
--
--         , responseUpdateAssessmentTarget $
--             newUpdateAssessmentTargetResponse
--
--         , responseListAssessmentTargets $
--             newListAssessmentTargetsResponse
--
--         , responseListAssessmentRuns $
--             newListAssessmentRunsResponse
--
--         , responseDeleteAssessmentTarget $
--             newDeleteAssessmentTargetResponse
--
--         , responseAddAttributesToFindings $
--             newAddAttributesToFindingsResponse
--
--         , responseCreateAssessmentTarget $
--             newCreateAssessmentTargetResponse
--
--         , responseGetExclusionsPreview $
--             newGetExclusionsPreviewResponse
--
--         , responseDescribeResourceGroups $
--             newDescribeResourceGroupsResponse
--
--         , responsePreviewAgents $
--             newPreviewAgentsResponse
--
--         , responseListExclusions $
--             newListExclusionsResponse
--
--         , responseCreateAssessmentTemplate $
--             newCreateAssessmentTemplateResponse
--
--         , responseDescribeCrossAccountAccessRole $
--             newDescribeCrossAccountAccessRoleResponse
--
--         , responseSetTagsForResource $
--             newSetTagsForResourceResponse
--
--         , responseDescribeExclusions $
--             newDescribeExclusionsResponse
--
--         , responseListAssessmentTemplates $
--             newListAssessmentTemplatesResponse
--
--         , responseListAssessmentRunAgents $
--             newListAssessmentRunAgentsResponse
--
--         , responseDescribeAssessmentRuns $
--             newDescribeAssessmentRunsResponse
--
--         , responseDescribeRulesPackages $
--             newDescribeRulesPackagesResponse
--
--         , responseCreateExclusionsPreview $
--             newCreateExclusionsPreviewResponse
--
--         , responseCreateResourceGroup $
--             newCreateResourceGroupResponse
--
--         , responseUnsubscribeFromEvent $
--             newUnsubscribeFromEventResponse
--
--         , responseRemoveAttributesFromFindings $
--             newRemoveAttributesFromFindingsResponse
--
--         , responseDeleteAssessmentRun $
--             newDeleteAssessmentRunResponse
--
--         , responseRegisterCrossAccountAccessRole $
--             newRegisterCrossAccountAccessRoleResponse
--
--         , responseListEventSubscriptions $
--             newListEventSubscriptionsResponse
--
--         , responseGetAssessmentReport $
--             newGetAssessmentReportResponse
--
--         , responseListRulesPackages $
--             newListRulesPackagesResponse
--
--         , responseDescribeFindings $
--             newDescribeFindingsResponse
--
--         , responseDescribeAssessmentTemplates $
--             newDescribeAssessmentTemplatesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestStartAssessmentRun :: StartAssessmentRun -> TestTree
requestStartAssessmentRun =
  req
    "StartAssessmentRun"
    "fixture/StartAssessmentRun.yaml"

requestDeleteAssessmentTemplate :: DeleteAssessmentTemplate -> TestTree
requestDeleteAssessmentTemplate =
  req
    "DeleteAssessmentTemplate"
    "fixture/DeleteAssessmentTemplate.yaml"

requestSubscribeToEvent :: SubscribeToEvent -> TestTree
requestSubscribeToEvent =
  req
    "SubscribeToEvent"
    "fixture/SubscribeToEvent.yaml"

requestStopAssessmentRun :: StopAssessmentRun -> TestTree
requestStopAssessmentRun =
  req
    "StopAssessmentRun"
    "fixture/StopAssessmentRun.yaml"

requestGetTelemetryMetadata :: GetTelemetryMetadata -> TestTree
requestGetTelemetryMetadata =
  req
    "GetTelemetryMetadata"
    "fixture/GetTelemetryMetadata.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings =
  req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestDescribeAssessmentTargets :: DescribeAssessmentTargets -> TestTree
requestDescribeAssessmentTargets =
  req
    "DescribeAssessmentTargets"
    "fixture/DescribeAssessmentTargets.yaml"

requestUpdateAssessmentTarget :: UpdateAssessmentTarget -> TestTree
requestUpdateAssessmentTarget =
  req
    "UpdateAssessmentTarget"
    "fixture/UpdateAssessmentTarget.yaml"

requestListAssessmentTargets :: ListAssessmentTargets -> TestTree
requestListAssessmentTargets =
  req
    "ListAssessmentTargets"
    "fixture/ListAssessmentTargets.yaml"

requestListAssessmentRuns :: ListAssessmentRuns -> TestTree
requestListAssessmentRuns =
  req
    "ListAssessmentRuns"
    "fixture/ListAssessmentRuns.yaml"

requestDeleteAssessmentTarget :: DeleteAssessmentTarget -> TestTree
requestDeleteAssessmentTarget =
  req
    "DeleteAssessmentTarget"
    "fixture/DeleteAssessmentTarget.yaml"

requestAddAttributesToFindings :: AddAttributesToFindings -> TestTree
requestAddAttributesToFindings =
  req
    "AddAttributesToFindings"
    "fixture/AddAttributesToFindings.yaml"

requestCreateAssessmentTarget :: CreateAssessmentTarget -> TestTree
requestCreateAssessmentTarget =
  req
    "CreateAssessmentTarget"
    "fixture/CreateAssessmentTarget.yaml"

requestGetExclusionsPreview :: GetExclusionsPreview -> TestTree
requestGetExclusionsPreview =
  req
    "GetExclusionsPreview"
    "fixture/GetExclusionsPreview.yaml"

requestDescribeResourceGroups :: DescribeResourceGroups -> TestTree
requestDescribeResourceGroups =
  req
    "DescribeResourceGroups"
    "fixture/DescribeResourceGroups.yaml"

requestPreviewAgents :: PreviewAgents -> TestTree
requestPreviewAgents =
  req
    "PreviewAgents"
    "fixture/PreviewAgents.yaml"

requestListExclusions :: ListExclusions -> TestTree
requestListExclusions =
  req
    "ListExclusions"
    "fixture/ListExclusions.yaml"

requestCreateAssessmentTemplate :: CreateAssessmentTemplate -> TestTree
requestCreateAssessmentTemplate =
  req
    "CreateAssessmentTemplate"
    "fixture/CreateAssessmentTemplate.yaml"

requestDescribeCrossAccountAccessRole :: DescribeCrossAccountAccessRole -> TestTree
requestDescribeCrossAccountAccessRole =
  req
    "DescribeCrossAccountAccessRole"
    "fixture/DescribeCrossAccountAccessRole.yaml"

requestSetTagsForResource :: SetTagsForResource -> TestTree
requestSetTagsForResource =
  req
    "SetTagsForResource"
    "fixture/SetTagsForResource.yaml"

requestDescribeExclusions :: DescribeExclusions -> TestTree
requestDescribeExclusions =
  req
    "DescribeExclusions"
    "fixture/DescribeExclusions.yaml"

requestListAssessmentTemplates :: ListAssessmentTemplates -> TestTree
requestListAssessmentTemplates =
  req
    "ListAssessmentTemplates"
    "fixture/ListAssessmentTemplates.yaml"

requestListAssessmentRunAgents :: ListAssessmentRunAgents -> TestTree
requestListAssessmentRunAgents =
  req
    "ListAssessmentRunAgents"
    "fixture/ListAssessmentRunAgents.yaml"

requestDescribeAssessmentRuns :: DescribeAssessmentRuns -> TestTree
requestDescribeAssessmentRuns =
  req
    "DescribeAssessmentRuns"
    "fixture/DescribeAssessmentRuns.yaml"

requestDescribeRulesPackages :: DescribeRulesPackages -> TestTree
requestDescribeRulesPackages =
  req
    "DescribeRulesPackages"
    "fixture/DescribeRulesPackages.yaml"

requestCreateExclusionsPreview :: CreateExclusionsPreview -> TestTree
requestCreateExclusionsPreview =
  req
    "CreateExclusionsPreview"
    "fixture/CreateExclusionsPreview.yaml"

requestCreateResourceGroup :: CreateResourceGroup -> TestTree
requestCreateResourceGroup =
  req
    "CreateResourceGroup"
    "fixture/CreateResourceGroup.yaml"

requestUnsubscribeFromEvent :: UnsubscribeFromEvent -> TestTree
requestUnsubscribeFromEvent =
  req
    "UnsubscribeFromEvent"
    "fixture/UnsubscribeFromEvent.yaml"

requestRemoveAttributesFromFindings :: RemoveAttributesFromFindings -> TestTree
requestRemoveAttributesFromFindings =
  req
    "RemoveAttributesFromFindings"
    "fixture/RemoveAttributesFromFindings.yaml"

requestDeleteAssessmentRun :: DeleteAssessmentRun -> TestTree
requestDeleteAssessmentRun =
  req
    "DeleteAssessmentRun"
    "fixture/DeleteAssessmentRun.yaml"

requestRegisterCrossAccountAccessRole :: RegisterCrossAccountAccessRole -> TestTree
requestRegisterCrossAccountAccessRole =
  req
    "RegisterCrossAccountAccessRole"
    "fixture/RegisterCrossAccountAccessRole.yaml"

requestListEventSubscriptions :: ListEventSubscriptions -> TestTree
requestListEventSubscriptions =
  req
    "ListEventSubscriptions"
    "fixture/ListEventSubscriptions.yaml"

requestGetAssessmentReport :: GetAssessmentReport -> TestTree
requestGetAssessmentReport =
  req
    "GetAssessmentReport"
    "fixture/GetAssessmentReport.yaml"

requestListRulesPackages :: ListRulesPackages -> TestTree
requestListRulesPackages =
  req
    "ListRulesPackages"
    "fixture/ListRulesPackages.yaml"

requestDescribeFindings :: DescribeFindings -> TestTree
requestDescribeFindings =
  req
    "DescribeFindings"
    "fixture/DescribeFindings.yaml"

requestDescribeAssessmentTemplates :: DescribeAssessmentTemplates -> TestTree
requestDescribeAssessmentTemplates =
  req
    "DescribeAssessmentTemplates"
    "fixture/DescribeAssessmentTemplates.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseStartAssessmentRun :: StartAssessmentRunResponse -> TestTree
responseStartAssessmentRun =
  res
    "StartAssessmentRunResponse"
    "fixture/StartAssessmentRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartAssessmentRun)

responseDeleteAssessmentTemplate :: DeleteAssessmentTemplateResponse -> TestTree
responseDeleteAssessmentTemplate =
  res
    "DeleteAssessmentTemplateResponse"
    "fixture/DeleteAssessmentTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAssessmentTemplate)

responseSubscribeToEvent :: SubscribeToEventResponse -> TestTree
responseSubscribeToEvent =
  res
    "SubscribeToEventResponse"
    "fixture/SubscribeToEventResponse.proto"
    defaultService
    (Proxy :: Proxy SubscribeToEvent)

responseStopAssessmentRun :: StopAssessmentRunResponse -> TestTree
responseStopAssessmentRun =
  res
    "StopAssessmentRunResponse"
    "fixture/StopAssessmentRunResponse.proto"
    defaultService
    (Proxy :: Proxy StopAssessmentRun)

responseGetTelemetryMetadata :: GetTelemetryMetadataResponse -> TestTree
responseGetTelemetryMetadata =
  res
    "GetTelemetryMetadataResponse"
    "fixture/GetTelemetryMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy GetTelemetryMetadata)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings =
  res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFindings)

responseDescribeAssessmentTargets :: DescribeAssessmentTargetsResponse -> TestTree
responseDescribeAssessmentTargets =
  res
    "DescribeAssessmentTargetsResponse"
    "fixture/DescribeAssessmentTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAssessmentTargets)

responseUpdateAssessmentTarget :: UpdateAssessmentTargetResponse -> TestTree
responseUpdateAssessmentTarget =
  res
    "UpdateAssessmentTargetResponse"
    "fixture/UpdateAssessmentTargetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAssessmentTarget)

responseListAssessmentTargets :: ListAssessmentTargetsResponse -> TestTree
responseListAssessmentTargets =
  res
    "ListAssessmentTargetsResponse"
    "fixture/ListAssessmentTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssessmentTargets)

responseListAssessmentRuns :: ListAssessmentRunsResponse -> TestTree
responseListAssessmentRuns =
  res
    "ListAssessmentRunsResponse"
    "fixture/ListAssessmentRunsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssessmentRuns)

responseDeleteAssessmentTarget :: DeleteAssessmentTargetResponse -> TestTree
responseDeleteAssessmentTarget =
  res
    "DeleteAssessmentTargetResponse"
    "fixture/DeleteAssessmentTargetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAssessmentTarget)

responseAddAttributesToFindings :: AddAttributesToFindingsResponse -> TestTree
responseAddAttributesToFindings =
  res
    "AddAttributesToFindingsResponse"
    "fixture/AddAttributesToFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy AddAttributesToFindings)

responseCreateAssessmentTarget :: CreateAssessmentTargetResponse -> TestTree
responseCreateAssessmentTarget =
  res
    "CreateAssessmentTargetResponse"
    "fixture/CreateAssessmentTargetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAssessmentTarget)

responseGetExclusionsPreview :: GetExclusionsPreviewResponse -> TestTree
responseGetExclusionsPreview =
  res
    "GetExclusionsPreviewResponse"
    "fixture/GetExclusionsPreviewResponse.proto"
    defaultService
    (Proxy :: Proxy GetExclusionsPreview)

responseDescribeResourceGroups :: DescribeResourceGroupsResponse -> TestTree
responseDescribeResourceGroups =
  res
    "DescribeResourceGroupsResponse"
    "fixture/DescribeResourceGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResourceGroups)

responsePreviewAgents :: PreviewAgentsResponse -> TestTree
responsePreviewAgents =
  res
    "PreviewAgentsResponse"
    "fixture/PreviewAgentsResponse.proto"
    defaultService
    (Proxy :: Proxy PreviewAgents)

responseListExclusions :: ListExclusionsResponse -> TestTree
responseListExclusions =
  res
    "ListExclusionsResponse"
    "fixture/ListExclusionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListExclusions)

responseCreateAssessmentTemplate :: CreateAssessmentTemplateResponse -> TestTree
responseCreateAssessmentTemplate =
  res
    "CreateAssessmentTemplateResponse"
    "fixture/CreateAssessmentTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAssessmentTemplate)

responseDescribeCrossAccountAccessRole :: DescribeCrossAccountAccessRoleResponse -> TestTree
responseDescribeCrossAccountAccessRole =
  res
    "DescribeCrossAccountAccessRoleResponse"
    "fixture/DescribeCrossAccountAccessRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCrossAccountAccessRole)

responseSetTagsForResource :: SetTagsForResourceResponse -> TestTree
responseSetTagsForResource =
  res
    "SetTagsForResourceResponse"
    "fixture/SetTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy SetTagsForResource)

responseDescribeExclusions :: DescribeExclusionsResponse -> TestTree
responseDescribeExclusions =
  res
    "DescribeExclusionsResponse"
    "fixture/DescribeExclusionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExclusions)

responseListAssessmentTemplates :: ListAssessmentTemplatesResponse -> TestTree
responseListAssessmentTemplates =
  res
    "ListAssessmentTemplatesResponse"
    "fixture/ListAssessmentTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssessmentTemplates)

responseListAssessmentRunAgents :: ListAssessmentRunAgentsResponse -> TestTree
responseListAssessmentRunAgents =
  res
    "ListAssessmentRunAgentsResponse"
    "fixture/ListAssessmentRunAgentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssessmentRunAgents)

responseDescribeAssessmentRuns :: DescribeAssessmentRunsResponse -> TestTree
responseDescribeAssessmentRuns =
  res
    "DescribeAssessmentRunsResponse"
    "fixture/DescribeAssessmentRunsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAssessmentRuns)

responseDescribeRulesPackages :: DescribeRulesPackagesResponse -> TestTree
responseDescribeRulesPackages =
  res
    "DescribeRulesPackagesResponse"
    "fixture/DescribeRulesPackagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRulesPackages)

responseCreateExclusionsPreview :: CreateExclusionsPreviewResponse -> TestTree
responseCreateExclusionsPreview =
  res
    "CreateExclusionsPreviewResponse"
    "fixture/CreateExclusionsPreviewResponse.proto"
    defaultService
    (Proxy :: Proxy CreateExclusionsPreview)

responseCreateResourceGroup :: CreateResourceGroupResponse -> TestTree
responseCreateResourceGroup =
  res
    "CreateResourceGroupResponse"
    "fixture/CreateResourceGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResourceGroup)

responseUnsubscribeFromEvent :: UnsubscribeFromEventResponse -> TestTree
responseUnsubscribeFromEvent =
  res
    "UnsubscribeFromEventResponse"
    "fixture/UnsubscribeFromEventResponse.proto"
    defaultService
    (Proxy :: Proxy UnsubscribeFromEvent)

responseRemoveAttributesFromFindings :: RemoveAttributesFromFindingsResponse -> TestTree
responseRemoveAttributesFromFindings =
  res
    "RemoveAttributesFromFindingsResponse"
    "fixture/RemoveAttributesFromFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveAttributesFromFindings)

responseDeleteAssessmentRun :: DeleteAssessmentRunResponse -> TestTree
responseDeleteAssessmentRun =
  res
    "DeleteAssessmentRunResponse"
    "fixture/DeleteAssessmentRunResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAssessmentRun)

responseRegisterCrossAccountAccessRole :: RegisterCrossAccountAccessRoleResponse -> TestTree
responseRegisterCrossAccountAccessRole =
  res
    "RegisterCrossAccountAccessRoleResponse"
    "fixture/RegisterCrossAccountAccessRoleResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCrossAccountAccessRole)

responseListEventSubscriptions :: ListEventSubscriptionsResponse -> TestTree
responseListEventSubscriptions =
  res
    "ListEventSubscriptionsResponse"
    "fixture/ListEventSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEventSubscriptions)

responseGetAssessmentReport :: GetAssessmentReportResponse -> TestTree
responseGetAssessmentReport =
  res
    "GetAssessmentReportResponse"
    "fixture/GetAssessmentReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssessmentReport)

responseListRulesPackages :: ListRulesPackagesResponse -> TestTree
responseListRulesPackages =
  res
    "ListRulesPackagesResponse"
    "fixture/ListRulesPackagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRulesPackages)

responseDescribeFindings :: DescribeFindingsResponse -> TestTree
responseDescribeFindings =
  res
    "DescribeFindingsResponse"
    "fixture/DescribeFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFindings)

responseDescribeAssessmentTemplates :: DescribeAssessmentTemplatesResponse -> TestTree
responseDescribeAssessmentTemplates =
  res
    "DescribeAssessmentTemplatesResponse"
    "fixture/DescribeAssessmentTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAssessmentTemplates)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
