{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Inspector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Inspector where

import Amazonka.Inspector
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Inspector.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddAttributesToFindings $
--             newAddAttributesToFindings
--
--         , requestCreateAssessmentTarget $
--             newCreateAssessmentTarget
--
--         , requestCreateAssessmentTemplate $
--             newCreateAssessmentTemplate
--
--         , requestCreateExclusionsPreview $
--             newCreateExclusionsPreview
--
--         , requestCreateResourceGroup $
--             newCreateResourceGroup
--
--         , requestDeleteAssessmentRun $
--             newDeleteAssessmentRun
--
--         , requestDeleteAssessmentTarget $
--             newDeleteAssessmentTarget
--
--         , requestDeleteAssessmentTemplate $
--             newDeleteAssessmentTemplate
--
--         , requestDescribeAssessmentRuns $
--             newDescribeAssessmentRuns
--
--         , requestDescribeAssessmentTargets $
--             newDescribeAssessmentTargets
--
--         , requestDescribeAssessmentTemplates $
--             newDescribeAssessmentTemplates
--
--         , requestDescribeCrossAccountAccessRole $
--             newDescribeCrossAccountAccessRole
--
--         , requestDescribeExclusions $
--             newDescribeExclusions
--
--         , requestDescribeFindings $
--             newDescribeFindings
--
--         , requestDescribeResourceGroups $
--             newDescribeResourceGroups
--
--         , requestDescribeRulesPackages $
--             newDescribeRulesPackages
--
--         , requestGetAssessmentReport $
--             newGetAssessmentReport
--
--         , requestGetExclusionsPreview $
--             newGetExclusionsPreview
--
--         , requestGetTelemetryMetadata $
--             newGetTelemetryMetadata
--
--         , requestListAssessmentRunAgents $
--             newListAssessmentRunAgents
--
--         , requestListAssessmentRuns $
--             newListAssessmentRuns
--
--         , requestListAssessmentTargets $
--             newListAssessmentTargets
--
--         , requestListAssessmentTemplates $
--             newListAssessmentTemplates
--
--         , requestListEventSubscriptions $
--             newListEventSubscriptions
--
--         , requestListExclusions $
--             newListExclusions
--
--         , requestListFindings $
--             newListFindings
--
--         , requestListRulesPackages $
--             newListRulesPackages
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPreviewAgents $
--             newPreviewAgents
--
--         , requestRegisterCrossAccountAccessRole $
--             newRegisterCrossAccountAccessRole
--
--         , requestRemoveAttributesFromFindings $
--             newRemoveAttributesFromFindings
--
--         , requestSetTagsForResource $
--             newSetTagsForResource
--
--         , requestStartAssessmentRun $
--             newStartAssessmentRun
--
--         , requestStopAssessmentRun $
--             newStopAssessmentRun
--
--         , requestSubscribeToEvent $
--             newSubscribeToEvent
--
--         , requestUnsubscribeFromEvent $
--             newUnsubscribeFromEvent
--
--         , requestUpdateAssessmentTarget $
--             newUpdateAssessmentTarget
--
--           ]

--     , testGroup "response"
--         [ responseAddAttributesToFindings $
--             newAddAttributesToFindingsResponse
--
--         , responseCreateAssessmentTarget $
--             newCreateAssessmentTargetResponse
--
--         , responseCreateAssessmentTemplate $
--             newCreateAssessmentTemplateResponse
--
--         , responseCreateExclusionsPreview $
--             newCreateExclusionsPreviewResponse
--
--         , responseCreateResourceGroup $
--             newCreateResourceGroupResponse
--
--         , responseDeleteAssessmentRun $
--             newDeleteAssessmentRunResponse
--
--         , responseDeleteAssessmentTarget $
--             newDeleteAssessmentTargetResponse
--
--         , responseDeleteAssessmentTemplate $
--             newDeleteAssessmentTemplateResponse
--
--         , responseDescribeAssessmentRuns $
--             newDescribeAssessmentRunsResponse
--
--         , responseDescribeAssessmentTargets $
--             newDescribeAssessmentTargetsResponse
--
--         , responseDescribeAssessmentTemplates $
--             newDescribeAssessmentTemplatesResponse
--
--         , responseDescribeCrossAccountAccessRole $
--             newDescribeCrossAccountAccessRoleResponse
--
--         , responseDescribeExclusions $
--             newDescribeExclusionsResponse
--
--         , responseDescribeFindings $
--             newDescribeFindingsResponse
--
--         , responseDescribeResourceGroups $
--             newDescribeResourceGroupsResponse
--
--         , responseDescribeRulesPackages $
--             newDescribeRulesPackagesResponse
--
--         , responseGetAssessmentReport $
--             newGetAssessmentReportResponse
--
--         , responseGetExclusionsPreview $
--             newGetExclusionsPreviewResponse
--
--         , responseGetTelemetryMetadata $
--             newGetTelemetryMetadataResponse
--
--         , responseListAssessmentRunAgents $
--             newListAssessmentRunAgentsResponse
--
--         , responseListAssessmentRuns $
--             newListAssessmentRunsResponse
--
--         , responseListAssessmentTargets $
--             newListAssessmentTargetsResponse
--
--         , responseListAssessmentTemplates $
--             newListAssessmentTemplatesResponse
--
--         , responseListEventSubscriptions $
--             newListEventSubscriptionsResponse
--
--         , responseListExclusions $
--             newListExclusionsResponse
--
--         , responseListFindings $
--             newListFindingsResponse
--
--         , responseListRulesPackages $
--             newListRulesPackagesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePreviewAgents $
--             newPreviewAgentsResponse
--
--         , responseRegisterCrossAccountAccessRole $
--             newRegisterCrossAccountAccessRoleResponse
--
--         , responseRemoveAttributesFromFindings $
--             newRemoveAttributesFromFindingsResponse
--
--         , responseSetTagsForResource $
--             newSetTagsForResourceResponse
--
--         , responseStartAssessmentRun $
--             newStartAssessmentRunResponse
--
--         , responseStopAssessmentRun $
--             newStopAssessmentRunResponse
--
--         , responseSubscribeToEvent $
--             newSubscribeToEventResponse
--
--         , responseUnsubscribeFromEvent $
--             newUnsubscribeFromEventResponse
--
--         , responseUpdateAssessmentTarget $
--             newUpdateAssessmentTargetResponse
--
--           ]
--     ]

-- Requests

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

requestCreateAssessmentTemplate :: CreateAssessmentTemplate -> TestTree
requestCreateAssessmentTemplate =
  req
    "CreateAssessmentTemplate"
    "fixture/CreateAssessmentTemplate.yaml"

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

requestDeleteAssessmentRun :: DeleteAssessmentRun -> TestTree
requestDeleteAssessmentRun =
  req
    "DeleteAssessmentRun"
    "fixture/DeleteAssessmentRun.yaml"

requestDeleteAssessmentTarget :: DeleteAssessmentTarget -> TestTree
requestDeleteAssessmentTarget =
  req
    "DeleteAssessmentTarget"
    "fixture/DeleteAssessmentTarget.yaml"

requestDeleteAssessmentTemplate :: DeleteAssessmentTemplate -> TestTree
requestDeleteAssessmentTemplate =
  req
    "DeleteAssessmentTemplate"
    "fixture/DeleteAssessmentTemplate.yaml"

requestDescribeAssessmentRuns :: DescribeAssessmentRuns -> TestTree
requestDescribeAssessmentRuns =
  req
    "DescribeAssessmentRuns"
    "fixture/DescribeAssessmentRuns.yaml"

requestDescribeAssessmentTargets :: DescribeAssessmentTargets -> TestTree
requestDescribeAssessmentTargets =
  req
    "DescribeAssessmentTargets"
    "fixture/DescribeAssessmentTargets.yaml"

requestDescribeAssessmentTemplates :: DescribeAssessmentTemplates -> TestTree
requestDescribeAssessmentTemplates =
  req
    "DescribeAssessmentTemplates"
    "fixture/DescribeAssessmentTemplates.yaml"

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

requestDescribeFindings :: DescribeFindings -> TestTree
requestDescribeFindings =
  req
    "DescribeFindings"
    "fixture/DescribeFindings.yaml"

requestDescribeResourceGroups :: DescribeResourceGroups -> TestTree
requestDescribeResourceGroups =
  req
    "DescribeResourceGroups"
    "fixture/DescribeResourceGroups.yaml"

requestDescribeRulesPackages :: DescribeRulesPackages -> TestTree
requestDescribeRulesPackages =
  req
    "DescribeRulesPackages"
    "fixture/DescribeRulesPackages.yaml"

requestGetAssessmentReport :: GetAssessmentReport -> TestTree
requestGetAssessmentReport =
  req
    "GetAssessmentReport"
    "fixture/GetAssessmentReport.yaml"

requestGetExclusionsPreview :: GetExclusionsPreview -> TestTree
requestGetExclusionsPreview =
  req
    "GetExclusionsPreview"
    "fixture/GetExclusionsPreview.yaml"

requestGetTelemetryMetadata :: GetTelemetryMetadata -> TestTree
requestGetTelemetryMetadata =
  req
    "GetTelemetryMetadata"
    "fixture/GetTelemetryMetadata.yaml"

requestListAssessmentRunAgents :: ListAssessmentRunAgents -> TestTree
requestListAssessmentRunAgents =
  req
    "ListAssessmentRunAgents"
    "fixture/ListAssessmentRunAgents.yaml"

requestListAssessmentRuns :: ListAssessmentRuns -> TestTree
requestListAssessmentRuns =
  req
    "ListAssessmentRuns"
    "fixture/ListAssessmentRuns.yaml"

requestListAssessmentTargets :: ListAssessmentTargets -> TestTree
requestListAssessmentTargets =
  req
    "ListAssessmentTargets"
    "fixture/ListAssessmentTargets.yaml"

requestListAssessmentTemplates :: ListAssessmentTemplates -> TestTree
requestListAssessmentTemplates =
  req
    "ListAssessmentTemplates"
    "fixture/ListAssessmentTemplates.yaml"

requestListEventSubscriptions :: ListEventSubscriptions -> TestTree
requestListEventSubscriptions =
  req
    "ListEventSubscriptions"
    "fixture/ListEventSubscriptions.yaml"

requestListExclusions :: ListExclusions -> TestTree
requestListExclusions =
  req
    "ListExclusions"
    "fixture/ListExclusions.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings =
  req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestListRulesPackages :: ListRulesPackages -> TestTree
requestListRulesPackages =
  req
    "ListRulesPackages"
    "fixture/ListRulesPackages.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPreviewAgents :: PreviewAgents -> TestTree
requestPreviewAgents =
  req
    "PreviewAgents"
    "fixture/PreviewAgents.yaml"

requestRegisterCrossAccountAccessRole :: RegisterCrossAccountAccessRole -> TestTree
requestRegisterCrossAccountAccessRole =
  req
    "RegisterCrossAccountAccessRole"
    "fixture/RegisterCrossAccountAccessRole.yaml"

requestRemoveAttributesFromFindings :: RemoveAttributesFromFindings -> TestTree
requestRemoveAttributesFromFindings =
  req
    "RemoveAttributesFromFindings"
    "fixture/RemoveAttributesFromFindings.yaml"

requestSetTagsForResource :: SetTagsForResource -> TestTree
requestSetTagsForResource =
  req
    "SetTagsForResource"
    "fixture/SetTagsForResource.yaml"

requestStartAssessmentRun :: StartAssessmentRun -> TestTree
requestStartAssessmentRun =
  req
    "StartAssessmentRun"
    "fixture/StartAssessmentRun.yaml"

requestStopAssessmentRun :: StopAssessmentRun -> TestTree
requestStopAssessmentRun =
  req
    "StopAssessmentRun"
    "fixture/StopAssessmentRun.yaml"

requestSubscribeToEvent :: SubscribeToEvent -> TestTree
requestSubscribeToEvent =
  req
    "SubscribeToEvent"
    "fixture/SubscribeToEvent.yaml"

requestUnsubscribeFromEvent :: UnsubscribeFromEvent -> TestTree
requestUnsubscribeFromEvent =
  req
    "UnsubscribeFromEvent"
    "fixture/UnsubscribeFromEvent.yaml"

requestUpdateAssessmentTarget :: UpdateAssessmentTarget -> TestTree
requestUpdateAssessmentTarget =
  req
    "UpdateAssessmentTarget"
    "fixture/UpdateAssessmentTarget.yaml"

-- Responses

responseAddAttributesToFindings :: AddAttributesToFindingsResponse -> TestTree
responseAddAttributesToFindings =
  res
    "AddAttributesToFindingsResponse"
    "fixture/AddAttributesToFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddAttributesToFindings)

responseCreateAssessmentTarget :: CreateAssessmentTargetResponse -> TestTree
responseCreateAssessmentTarget =
  res
    "CreateAssessmentTargetResponse"
    "fixture/CreateAssessmentTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssessmentTarget)

responseCreateAssessmentTemplate :: CreateAssessmentTemplateResponse -> TestTree
responseCreateAssessmentTemplate =
  res
    "CreateAssessmentTemplateResponse"
    "fixture/CreateAssessmentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssessmentTemplate)

responseCreateExclusionsPreview :: CreateExclusionsPreviewResponse -> TestTree
responseCreateExclusionsPreview =
  res
    "CreateExclusionsPreviewResponse"
    "fixture/CreateExclusionsPreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExclusionsPreview)

responseCreateResourceGroup :: CreateResourceGroupResponse -> TestTree
responseCreateResourceGroup =
  res
    "CreateResourceGroupResponse"
    "fixture/CreateResourceGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceGroup)

responseDeleteAssessmentRun :: DeleteAssessmentRunResponse -> TestTree
responseDeleteAssessmentRun =
  res
    "DeleteAssessmentRunResponse"
    "fixture/DeleteAssessmentRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssessmentRun)

responseDeleteAssessmentTarget :: DeleteAssessmentTargetResponse -> TestTree
responseDeleteAssessmentTarget =
  res
    "DeleteAssessmentTargetResponse"
    "fixture/DeleteAssessmentTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssessmentTarget)

responseDeleteAssessmentTemplate :: DeleteAssessmentTemplateResponse -> TestTree
responseDeleteAssessmentTemplate =
  res
    "DeleteAssessmentTemplateResponse"
    "fixture/DeleteAssessmentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssessmentTemplate)

responseDescribeAssessmentRuns :: DescribeAssessmentRunsResponse -> TestTree
responseDescribeAssessmentRuns =
  res
    "DescribeAssessmentRunsResponse"
    "fixture/DescribeAssessmentRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssessmentRuns)

responseDescribeAssessmentTargets :: DescribeAssessmentTargetsResponse -> TestTree
responseDescribeAssessmentTargets =
  res
    "DescribeAssessmentTargetsResponse"
    "fixture/DescribeAssessmentTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssessmentTargets)

responseDescribeAssessmentTemplates :: DescribeAssessmentTemplatesResponse -> TestTree
responseDescribeAssessmentTemplates =
  res
    "DescribeAssessmentTemplatesResponse"
    "fixture/DescribeAssessmentTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssessmentTemplates)

responseDescribeCrossAccountAccessRole :: DescribeCrossAccountAccessRoleResponse -> TestTree
responseDescribeCrossAccountAccessRole =
  res
    "DescribeCrossAccountAccessRoleResponse"
    "fixture/DescribeCrossAccountAccessRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCrossAccountAccessRole)

responseDescribeExclusions :: DescribeExclusionsResponse -> TestTree
responseDescribeExclusions =
  res
    "DescribeExclusionsResponse"
    "fixture/DescribeExclusionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExclusions)

responseDescribeFindings :: DescribeFindingsResponse -> TestTree
responseDescribeFindings =
  res
    "DescribeFindingsResponse"
    "fixture/DescribeFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFindings)

responseDescribeResourceGroups :: DescribeResourceGroupsResponse -> TestTree
responseDescribeResourceGroups =
  res
    "DescribeResourceGroupsResponse"
    "fixture/DescribeResourceGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourceGroups)

responseDescribeRulesPackages :: DescribeRulesPackagesResponse -> TestTree
responseDescribeRulesPackages =
  res
    "DescribeRulesPackagesResponse"
    "fixture/DescribeRulesPackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRulesPackages)

responseGetAssessmentReport :: GetAssessmentReportResponse -> TestTree
responseGetAssessmentReport =
  res
    "GetAssessmentReportResponse"
    "fixture/GetAssessmentReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssessmentReport)

responseGetExclusionsPreview :: GetExclusionsPreviewResponse -> TestTree
responseGetExclusionsPreview =
  res
    "GetExclusionsPreviewResponse"
    "fixture/GetExclusionsPreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExclusionsPreview)

responseGetTelemetryMetadata :: GetTelemetryMetadataResponse -> TestTree
responseGetTelemetryMetadata =
  res
    "GetTelemetryMetadataResponse"
    "fixture/GetTelemetryMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTelemetryMetadata)

responseListAssessmentRunAgents :: ListAssessmentRunAgentsResponse -> TestTree
responseListAssessmentRunAgents =
  res
    "ListAssessmentRunAgentsResponse"
    "fixture/ListAssessmentRunAgentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessmentRunAgents)

responseListAssessmentRuns :: ListAssessmentRunsResponse -> TestTree
responseListAssessmentRuns =
  res
    "ListAssessmentRunsResponse"
    "fixture/ListAssessmentRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessmentRuns)

responseListAssessmentTargets :: ListAssessmentTargetsResponse -> TestTree
responseListAssessmentTargets =
  res
    "ListAssessmentTargetsResponse"
    "fixture/ListAssessmentTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessmentTargets)

responseListAssessmentTemplates :: ListAssessmentTemplatesResponse -> TestTree
responseListAssessmentTemplates =
  res
    "ListAssessmentTemplatesResponse"
    "fixture/ListAssessmentTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessmentTemplates)

responseListEventSubscriptions :: ListEventSubscriptionsResponse -> TestTree
responseListEventSubscriptions =
  res
    "ListEventSubscriptionsResponse"
    "fixture/ListEventSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventSubscriptions)

responseListExclusions :: ListExclusionsResponse -> TestTree
responseListExclusions =
  res
    "ListExclusionsResponse"
    "fixture/ListExclusionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExclusions)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings =
  res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindings)

responseListRulesPackages :: ListRulesPackagesResponse -> TestTree
responseListRulesPackages =
  res
    "ListRulesPackagesResponse"
    "fixture/ListRulesPackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRulesPackages)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePreviewAgents :: PreviewAgentsResponse -> TestTree
responsePreviewAgents =
  res
    "PreviewAgentsResponse"
    "fixture/PreviewAgentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PreviewAgents)

responseRegisterCrossAccountAccessRole :: RegisterCrossAccountAccessRoleResponse -> TestTree
responseRegisterCrossAccountAccessRole =
  res
    "RegisterCrossAccountAccessRoleResponse"
    "fixture/RegisterCrossAccountAccessRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCrossAccountAccessRole)

responseRemoveAttributesFromFindings :: RemoveAttributesFromFindingsResponse -> TestTree
responseRemoveAttributesFromFindings =
  res
    "RemoveAttributesFromFindingsResponse"
    "fixture/RemoveAttributesFromFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAttributesFromFindings)

responseSetTagsForResource :: SetTagsForResourceResponse -> TestTree
responseSetTagsForResource =
  res
    "SetTagsForResourceResponse"
    "fixture/SetTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTagsForResource)

responseStartAssessmentRun :: StartAssessmentRunResponse -> TestTree
responseStartAssessmentRun =
  res
    "StartAssessmentRunResponse"
    "fixture/StartAssessmentRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAssessmentRun)

responseStopAssessmentRun :: StopAssessmentRunResponse -> TestTree
responseStopAssessmentRun =
  res
    "StopAssessmentRunResponse"
    "fixture/StopAssessmentRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopAssessmentRun)

responseSubscribeToEvent :: SubscribeToEventResponse -> TestTree
responseSubscribeToEvent =
  res
    "SubscribeToEventResponse"
    "fixture/SubscribeToEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubscribeToEvent)

responseUnsubscribeFromEvent :: UnsubscribeFromEventResponse -> TestTree
responseUnsubscribeFromEvent =
  res
    "UnsubscribeFromEventResponse"
    "fixture/UnsubscribeFromEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnsubscribeFromEvent)

responseUpdateAssessmentTarget :: UpdateAssessmentTargetResponse -> TestTree
responseUpdateAssessmentTarget =
  res
    "UpdateAssessmentTargetResponse"
    "fixture/UpdateAssessmentTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessmentTarget)
