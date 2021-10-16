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
--         [ requestDeleteAssessmentTemplate $
--             newDeleteAssessmentTemplate
--
--         , requestSubscribeToEvent $
--             newSubscribeToEvent
--
--         , requestStopAssessmentRun $
--             newStopAssessmentRun
--
--         , requestStartAssessmentRun $
--             newStartAssessmentRun
--
--         , requestListFindings $
--             newListFindings
--
--         , requestGetTelemetryMetadata $
--             newGetTelemetryMetadata
--
--         , requestDescribeAssessmentTargets $
--             newDescribeAssessmentTargets
--
--         , requestListAssessmentRuns $
--             newListAssessmentRuns
--
--         , requestDeleteAssessmentTarget $
--             newDeleteAssessmentTarget
--
--         , requestListAssessmentTargets $
--             newListAssessmentTargets
--
--         , requestUpdateAssessmentTarget $
--             newUpdateAssessmentTarget
--
--         , requestDescribeResourceGroups $
--             newDescribeResourceGroups
--
--         , requestGetExclusionsPreview $
--             newGetExclusionsPreview
--
--         , requestAddAttributesToFindings $
--             newAddAttributesToFindings
--
--         , requestCreateAssessmentTarget $
--             newCreateAssessmentTarget
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
--         , requestSetTagsForResource $
--             newSetTagsForResource
--
--         , requestDescribeCrossAccountAccessRole $
--             newDescribeCrossAccountAccessRole
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
--         , requestUnsubscribeFromEvent $
--             newUnsubscribeFromEvent
--
--         , requestDescribeAssessmentRuns $
--             newDescribeAssessmentRuns
--
--         , requestCreateExclusionsPreview $
--             newCreateExclusionsPreview
--
--         , requestDescribeRulesPackages $
--             newDescribeRulesPackages
--
--         , requestRemoveAttributesFromFindings $
--             newRemoveAttributesFromFindings
--
--         , requestCreateResourceGroup $
--             newCreateResourceGroup
--
--         , requestGetAssessmentReport $
--             newGetAssessmentReport
--
--         , requestDeleteAssessmentRun $
--             newDeleteAssessmentRun
--
--         , requestListEventSubscriptions $
--             newListEventSubscriptions
--
--         , requestListRulesPackages $
--             newListRulesPackages
--
--         , requestRegisterCrossAccountAccessRole $
--             newRegisterCrossAccountAccessRole
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
--         [ responseDeleteAssessmentTemplate $
--             newDeleteAssessmentTemplateResponse
--
--         , responseSubscribeToEvent $
--             newSubscribeToEventResponse
--
--         , responseStopAssessmentRun $
--             newStopAssessmentRunResponse
--
--         , responseStartAssessmentRun $
--             newStartAssessmentRunResponse
--
--         , responseListFindings $
--             newListFindingsResponse
--
--         , responseGetTelemetryMetadata $
--             newGetTelemetryMetadataResponse
--
--         , responseDescribeAssessmentTargets $
--             newDescribeAssessmentTargetsResponse
--
--         , responseListAssessmentRuns $
--             newListAssessmentRunsResponse
--
--         , responseDeleteAssessmentTarget $
--             newDeleteAssessmentTargetResponse
--
--         , responseListAssessmentTargets $
--             newListAssessmentTargetsResponse
--
--         , responseUpdateAssessmentTarget $
--             newUpdateAssessmentTargetResponse
--
--         , responseDescribeResourceGroups $
--             newDescribeResourceGroupsResponse
--
--         , responseGetExclusionsPreview $
--             newGetExclusionsPreviewResponse
--
--         , responseAddAttributesToFindings $
--             newAddAttributesToFindingsResponse
--
--         , responseCreateAssessmentTarget $
--             newCreateAssessmentTargetResponse
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
--         , responseSetTagsForResource $
--             newSetTagsForResourceResponse
--
--         , responseDescribeCrossAccountAccessRole $
--             newDescribeCrossAccountAccessRoleResponse
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
--         , responseUnsubscribeFromEvent $
--             newUnsubscribeFromEventResponse
--
--         , responseDescribeAssessmentRuns $
--             newDescribeAssessmentRunsResponse
--
--         , responseCreateExclusionsPreview $
--             newCreateExclusionsPreviewResponse
--
--         , responseDescribeRulesPackages $
--             newDescribeRulesPackagesResponse
--
--         , responseRemoveAttributesFromFindings $
--             newRemoveAttributesFromFindingsResponse
--
--         , responseCreateResourceGroup $
--             newCreateResourceGroupResponse
--
--         , responseGetAssessmentReport $
--             newGetAssessmentReportResponse
--
--         , responseDeleteAssessmentRun $
--             newDeleteAssessmentRunResponse
--
--         , responseListEventSubscriptions $
--             newListEventSubscriptionsResponse
--
--         , responseListRulesPackages $
--             newListRulesPackagesResponse
--
--         , responseRegisterCrossAccountAccessRole $
--             newRegisterCrossAccountAccessRoleResponse
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

requestStartAssessmentRun :: StartAssessmentRun -> TestTree
requestStartAssessmentRun =
  req
    "StartAssessmentRun"
    "fixture/StartAssessmentRun.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings =
  req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestGetTelemetryMetadata :: GetTelemetryMetadata -> TestTree
requestGetTelemetryMetadata =
  req
    "GetTelemetryMetadata"
    "fixture/GetTelemetryMetadata.yaml"

requestDescribeAssessmentTargets :: DescribeAssessmentTargets -> TestTree
requestDescribeAssessmentTargets =
  req
    "DescribeAssessmentTargets"
    "fixture/DescribeAssessmentTargets.yaml"

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

requestListAssessmentTargets :: ListAssessmentTargets -> TestTree
requestListAssessmentTargets =
  req
    "ListAssessmentTargets"
    "fixture/ListAssessmentTargets.yaml"

requestUpdateAssessmentTarget :: UpdateAssessmentTarget -> TestTree
requestUpdateAssessmentTarget =
  req
    "UpdateAssessmentTarget"
    "fixture/UpdateAssessmentTarget.yaml"

requestDescribeResourceGroups :: DescribeResourceGroups -> TestTree
requestDescribeResourceGroups =
  req
    "DescribeResourceGroups"
    "fixture/DescribeResourceGroups.yaml"

requestGetExclusionsPreview :: GetExclusionsPreview -> TestTree
requestGetExclusionsPreview =
  req
    "GetExclusionsPreview"
    "fixture/GetExclusionsPreview.yaml"

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

requestSetTagsForResource :: SetTagsForResource -> TestTree
requestSetTagsForResource =
  req
    "SetTagsForResource"
    "fixture/SetTagsForResource.yaml"

requestDescribeCrossAccountAccessRole :: DescribeCrossAccountAccessRole -> TestTree
requestDescribeCrossAccountAccessRole =
  req
    "DescribeCrossAccountAccessRole"
    "fixture/DescribeCrossAccountAccessRole.yaml"

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

requestUnsubscribeFromEvent :: UnsubscribeFromEvent -> TestTree
requestUnsubscribeFromEvent =
  req
    "UnsubscribeFromEvent"
    "fixture/UnsubscribeFromEvent.yaml"

requestDescribeAssessmentRuns :: DescribeAssessmentRuns -> TestTree
requestDescribeAssessmentRuns =
  req
    "DescribeAssessmentRuns"
    "fixture/DescribeAssessmentRuns.yaml"

requestCreateExclusionsPreview :: CreateExclusionsPreview -> TestTree
requestCreateExclusionsPreview =
  req
    "CreateExclusionsPreview"
    "fixture/CreateExclusionsPreview.yaml"

requestDescribeRulesPackages :: DescribeRulesPackages -> TestTree
requestDescribeRulesPackages =
  req
    "DescribeRulesPackages"
    "fixture/DescribeRulesPackages.yaml"

requestRemoveAttributesFromFindings :: RemoveAttributesFromFindings -> TestTree
requestRemoveAttributesFromFindings =
  req
    "RemoveAttributesFromFindings"
    "fixture/RemoveAttributesFromFindings.yaml"

requestCreateResourceGroup :: CreateResourceGroup -> TestTree
requestCreateResourceGroup =
  req
    "CreateResourceGroup"
    "fixture/CreateResourceGroup.yaml"

requestGetAssessmentReport :: GetAssessmentReport -> TestTree
requestGetAssessmentReport =
  req
    "GetAssessmentReport"
    "fixture/GetAssessmentReport.yaml"

requestDeleteAssessmentRun :: DeleteAssessmentRun -> TestTree
requestDeleteAssessmentRun =
  req
    "DeleteAssessmentRun"
    "fixture/DeleteAssessmentRun.yaml"

requestListEventSubscriptions :: ListEventSubscriptions -> TestTree
requestListEventSubscriptions =
  req
    "ListEventSubscriptions"
    "fixture/ListEventSubscriptions.yaml"

requestListRulesPackages :: ListRulesPackages -> TestTree
requestListRulesPackages =
  req
    "ListRulesPackages"
    "fixture/ListRulesPackages.yaml"

requestRegisterCrossAccountAccessRole :: RegisterCrossAccountAccessRole -> TestTree
requestRegisterCrossAccountAccessRole =
  req
    "RegisterCrossAccountAccessRole"
    "fixture/RegisterCrossAccountAccessRole.yaml"

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

responseStartAssessmentRun :: StartAssessmentRunResponse -> TestTree
responseStartAssessmentRun =
  res
    "StartAssessmentRunResponse"
    "fixture/StartAssessmentRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartAssessmentRun)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings =
  res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFindings)

responseGetTelemetryMetadata :: GetTelemetryMetadataResponse -> TestTree
responseGetTelemetryMetadata =
  res
    "GetTelemetryMetadataResponse"
    "fixture/GetTelemetryMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy GetTelemetryMetadata)

responseDescribeAssessmentTargets :: DescribeAssessmentTargetsResponse -> TestTree
responseDescribeAssessmentTargets =
  res
    "DescribeAssessmentTargetsResponse"
    "fixture/DescribeAssessmentTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAssessmentTargets)

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

responseListAssessmentTargets :: ListAssessmentTargetsResponse -> TestTree
responseListAssessmentTargets =
  res
    "ListAssessmentTargetsResponse"
    "fixture/ListAssessmentTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssessmentTargets)

responseUpdateAssessmentTarget :: UpdateAssessmentTargetResponse -> TestTree
responseUpdateAssessmentTarget =
  res
    "UpdateAssessmentTargetResponse"
    "fixture/UpdateAssessmentTargetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAssessmentTarget)

responseDescribeResourceGroups :: DescribeResourceGroupsResponse -> TestTree
responseDescribeResourceGroups =
  res
    "DescribeResourceGroupsResponse"
    "fixture/DescribeResourceGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResourceGroups)

responseGetExclusionsPreview :: GetExclusionsPreviewResponse -> TestTree
responseGetExclusionsPreview =
  res
    "GetExclusionsPreviewResponse"
    "fixture/GetExclusionsPreviewResponse.proto"
    defaultService
    (Proxy :: Proxy GetExclusionsPreview)

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

responseSetTagsForResource :: SetTagsForResourceResponse -> TestTree
responseSetTagsForResource =
  res
    "SetTagsForResourceResponse"
    "fixture/SetTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy SetTagsForResource)

responseDescribeCrossAccountAccessRole :: DescribeCrossAccountAccessRoleResponse -> TestTree
responseDescribeCrossAccountAccessRole =
  res
    "DescribeCrossAccountAccessRoleResponse"
    "fixture/DescribeCrossAccountAccessRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCrossAccountAccessRole)

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

responseUnsubscribeFromEvent :: UnsubscribeFromEventResponse -> TestTree
responseUnsubscribeFromEvent =
  res
    "UnsubscribeFromEventResponse"
    "fixture/UnsubscribeFromEventResponse.proto"
    defaultService
    (Proxy :: Proxy UnsubscribeFromEvent)

responseDescribeAssessmentRuns :: DescribeAssessmentRunsResponse -> TestTree
responseDescribeAssessmentRuns =
  res
    "DescribeAssessmentRunsResponse"
    "fixture/DescribeAssessmentRunsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAssessmentRuns)

responseCreateExclusionsPreview :: CreateExclusionsPreviewResponse -> TestTree
responseCreateExclusionsPreview =
  res
    "CreateExclusionsPreviewResponse"
    "fixture/CreateExclusionsPreviewResponse.proto"
    defaultService
    (Proxy :: Proxy CreateExclusionsPreview)

responseDescribeRulesPackages :: DescribeRulesPackagesResponse -> TestTree
responseDescribeRulesPackages =
  res
    "DescribeRulesPackagesResponse"
    "fixture/DescribeRulesPackagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRulesPackages)

responseRemoveAttributesFromFindings :: RemoveAttributesFromFindingsResponse -> TestTree
responseRemoveAttributesFromFindings =
  res
    "RemoveAttributesFromFindingsResponse"
    "fixture/RemoveAttributesFromFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveAttributesFromFindings)

responseCreateResourceGroup :: CreateResourceGroupResponse -> TestTree
responseCreateResourceGroup =
  res
    "CreateResourceGroupResponse"
    "fixture/CreateResourceGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResourceGroup)

responseGetAssessmentReport :: GetAssessmentReportResponse -> TestTree
responseGetAssessmentReport =
  res
    "GetAssessmentReportResponse"
    "fixture/GetAssessmentReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssessmentReport)

responseDeleteAssessmentRun :: DeleteAssessmentRunResponse -> TestTree
responseDeleteAssessmentRun =
  res
    "DeleteAssessmentRunResponse"
    "fixture/DeleteAssessmentRunResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAssessmentRun)

responseListEventSubscriptions :: ListEventSubscriptionsResponse -> TestTree
responseListEventSubscriptions =
  res
    "ListEventSubscriptionsResponse"
    "fixture/ListEventSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEventSubscriptions)

responseListRulesPackages :: ListRulesPackagesResponse -> TestTree
responseListRulesPackages =
  res
    "ListRulesPackagesResponse"
    "fixture/ListRulesPackagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRulesPackages)

responseRegisterCrossAccountAccessRole :: RegisterCrossAccountAccessRoleResponse -> TestTree
responseRegisterCrossAccountAccessRole =
  res
    "RegisterCrossAccountAccessRoleResponse"
    "fixture/RegisterCrossAccountAccessRoleResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCrossAccountAccessRole)

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
