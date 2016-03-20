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
--         [ testListFindings $
--             listFindings
--
--         , testDescribeRun $
--             describeRun
--
--         , testDescribeApplication $
--             describeApplication
--
--         , testRunAssessment $
--             runAssessment
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
--         , testListAttachedAssessments $
--             listAttachedAssessments
--
--         , testDescribeFinding $
--             describeFinding
--
--         , testDeleteRun $
--             deleteRun
--
--         , testListRuns $
--             listRuns
--
--         , testDeleteApplication $
--             deleteApplication
--
--         , testUpdateApplication $
--             updateApplication
--
--         , testStartDataCollection $
--             startDataCollection
--
--         , testLocalizeText $
--             localizeText
--
--         , testRegisterCrossAccountAccessRole $
--             registerCrossAccountAccessRole
--
--         , testCreateApplication $
--             createApplication
--
--         , testCreateResourceGroup $
--             createResourceGroup
--
--         , testListAttachedRulesPackages $
--             listAttachedRulesPackages
--
--         , testDeleteAssessment $
--             deleteAssessment
--
--         , testUpdateAssessment $
--             updateAssessment
--
--         , testGetAssessmentTelemetry $
--             getAssessmentTelemetry
--
--         , testListAssessments $
--             listAssessments
--
--         , testDescribeRulesPackage $
--             describeRulesPackage
--
--         , testCreateAssessment $
--             createAssessment
--
--         , testDetachAssessmentAndRulesPackage $
--             detachAssessmentAndRulesPackage
--
--         , testDescribeResourceGroup $
--             describeResourceGroup
--
--         , testListApplications $
--             listApplications
--
--         , testDescribeAssessment $
--             describeAssessment
--
--         , testAddAttributesToFindings $
--             addAttributesToFindings
--
--         , testStopDataCollection $
--             stopDataCollection
--
--         , testPreviewAgentsForResourceGroup $
--             previewAgentsForResourceGroup
--
--         , testListAssessmentAgents $
--             listAssessmentAgents
--
--         , testListRulesPackages $
--             listRulesPackages
--
--         , testRemoveAttributesFromFindings $
--             removeAttributesFromFindings
--
--         , testAttachAssessmentAndRulesPackage $
--             attachAssessmentAndRulesPackage
--
--           ]

--     , testGroup "response"
--         [ testListFindingsResponse $
--             listFindingsResponse
--
--         , testDescribeRunResponse $
--             describeRunResponse
--
--         , testDescribeApplicationResponse $
--             describeApplicationResponse
--
--         , testRunAssessmentResponse $
--             runAssessmentResponse
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
--         , testListAttachedAssessmentsResponse $
--             listAttachedAssessmentsResponse
--
--         , testDescribeFindingResponse $
--             describeFindingResponse
--
--         , testDeleteRunResponse $
--             deleteRunResponse
--
--         , testListRunsResponse $
--             listRunsResponse
--
--         , testDeleteApplicationResponse $
--             deleteApplicationResponse
--
--         , testUpdateApplicationResponse $
--             updateApplicationResponse
--
--         , testStartDataCollectionResponse $
--             startDataCollectionResponse
--
--         , testLocalizeTextResponse $
--             localizeTextResponse
--
--         , testRegisterCrossAccountAccessRoleResponse $
--             registerCrossAccountAccessRoleResponse
--
--         , testCreateApplicationResponse $
--             createApplicationResponse
--
--         , testCreateResourceGroupResponse $
--             createResourceGroupResponse
--
--         , testListAttachedRulesPackagesResponse $
--             listAttachedRulesPackagesResponse
--
--         , testDeleteAssessmentResponse $
--             deleteAssessmentResponse
--
--         , testUpdateAssessmentResponse $
--             updateAssessmentResponse
--
--         , testGetAssessmentTelemetryResponse $
--             getAssessmentTelemetryResponse
--
--         , testListAssessmentsResponse $
--             listAssessmentsResponse
--
--         , testDescribeRulesPackageResponse $
--             describeRulesPackageResponse
--
--         , testCreateAssessmentResponse $
--             createAssessmentResponse
--
--         , testDetachAssessmentAndRulesPackageResponse $
--             detachAssessmentAndRulesPackageResponse
--
--         , testDescribeResourceGroupResponse $
--             describeResourceGroupResponse
--
--         , testListApplicationsResponse $
--             listApplicationsResponse
--
--         , testDescribeAssessmentResponse $
--             describeAssessmentResponse
--
--         , testAddAttributesToFindingsResponse $
--             addAttributesToFindingsResponse
--
--         , testStopDataCollectionResponse $
--             stopDataCollectionResponse
--
--         , testPreviewAgentsForResourceGroupResponse $
--             previewAgentsForResourceGroupResponse
--
--         , testListAssessmentAgentsResponse $
--             listAssessmentAgentsResponse
--
--         , testListRulesPackagesResponse $
--             listRulesPackagesResponse
--
--         , testRemoveAttributesFromFindingsResponse $
--             removeAttributesFromFindingsResponse
--
--         , testAttachAssessmentAndRulesPackageResponse $
--             attachAssessmentAndRulesPackageResponse
--
--           ]
--     ]

-- Requests

testListFindings :: ListFindings -> TestTree
testListFindings = req
    "ListFindings"
    "fixture/ListFindings.yaml"

testDescribeRun :: DescribeRun -> TestTree
testDescribeRun = req
    "DescribeRun"
    "fixture/DescribeRun.yaml"

testDescribeApplication :: DescribeApplication -> TestTree
testDescribeApplication = req
    "DescribeApplication"
    "fixture/DescribeApplication.yaml"

testRunAssessment :: RunAssessment -> TestTree
testRunAssessment = req
    "RunAssessment"
    "fixture/RunAssessment.yaml"

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

testListAttachedAssessments :: ListAttachedAssessments -> TestTree
testListAttachedAssessments = req
    "ListAttachedAssessments"
    "fixture/ListAttachedAssessments.yaml"

testDescribeFinding :: DescribeFinding -> TestTree
testDescribeFinding = req
    "DescribeFinding"
    "fixture/DescribeFinding.yaml"

testDeleteRun :: DeleteRun -> TestTree
testDeleteRun = req
    "DeleteRun"
    "fixture/DeleteRun.yaml"

testListRuns :: ListRuns -> TestTree
testListRuns = req
    "ListRuns"
    "fixture/ListRuns.yaml"

testDeleteApplication :: DeleteApplication -> TestTree
testDeleteApplication = req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

testUpdateApplication :: UpdateApplication -> TestTree
testUpdateApplication = req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

testStartDataCollection :: StartDataCollection -> TestTree
testStartDataCollection = req
    "StartDataCollection"
    "fixture/StartDataCollection.yaml"

testLocalizeText :: LocalizeText -> TestTree
testLocalizeText = req
    "LocalizeText"
    "fixture/LocalizeText.yaml"

testRegisterCrossAccountAccessRole :: RegisterCrossAccountAccessRole -> TestTree
testRegisterCrossAccountAccessRole = req
    "RegisterCrossAccountAccessRole"
    "fixture/RegisterCrossAccountAccessRole.yaml"

testCreateApplication :: CreateApplication -> TestTree
testCreateApplication = req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

testCreateResourceGroup :: CreateResourceGroup -> TestTree
testCreateResourceGroup = req
    "CreateResourceGroup"
    "fixture/CreateResourceGroup.yaml"

testListAttachedRulesPackages :: ListAttachedRulesPackages -> TestTree
testListAttachedRulesPackages = req
    "ListAttachedRulesPackages"
    "fixture/ListAttachedRulesPackages.yaml"

testDeleteAssessment :: DeleteAssessment -> TestTree
testDeleteAssessment = req
    "DeleteAssessment"
    "fixture/DeleteAssessment.yaml"

testUpdateAssessment :: UpdateAssessment -> TestTree
testUpdateAssessment = req
    "UpdateAssessment"
    "fixture/UpdateAssessment.yaml"

testGetAssessmentTelemetry :: GetAssessmentTelemetry -> TestTree
testGetAssessmentTelemetry = req
    "GetAssessmentTelemetry"
    "fixture/GetAssessmentTelemetry.yaml"

testListAssessments :: ListAssessments -> TestTree
testListAssessments = req
    "ListAssessments"
    "fixture/ListAssessments.yaml"

testDescribeRulesPackage :: DescribeRulesPackage -> TestTree
testDescribeRulesPackage = req
    "DescribeRulesPackage"
    "fixture/DescribeRulesPackage.yaml"

testCreateAssessment :: CreateAssessment -> TestTree
testCreateAssessment = req
    "CreateAssessment"
    "fixture/CreateAssessment.yaml"

testDetachAssessmentAndRulesPackage :: DetachAssessmentAndRulesPackage -> TestTree
testDetachAssessmentAndRulesPackage = req
    "DetachAssessmentAndRulesPackage"
    "fixture/DetachAssessmentAndRulesPackage.yaml"

testDescribeResourceGroup :: DescribeResourceGroup -> TestTree
testDescribeResourceGroup = req
    "DescribeResourceGroup"
    "fixture/DescribeResourceGroup.yaml"

testListApplications :: ListApplications -> TestTree
testListApplications = req
    "ListApplications"
    "fixture/ListApplications.yaml"

testDescribeAssessment :: DescribeAssessment -> TestTree
testDescribeAssessment = req
    "DescribeAssessment"
    "fixture/DescribeAssessment.yaml"

testAddAttributesToFindings :: AddAttributesToFindings -> TestTree
testAddAttributesToFindings = req
    "AddAttributesToFindings"
    "fixture/AddAttributesToFindings.yaml"

testStopDataCollection :: StopDataCollection -> TestTree
testStopDataCollection = req
    "StopDataCollection"
    "fixture/StopDataCollection.yaml"

testPreviewAgentsForResourceGroup :: PreviewAgentsForResourceGroup -> TestTree
testPreviewAgentsForResourceGroup = req
    "PreviewAgentsForResourceGroup"
    "fixture/PreviewAgentsForResourceGroup.yaml"

testListAssessmentAgents :: ListAssessmentAgents -> TestTree
testListAssessmentAgents = req
    "ListAssessmentAgents"
    "fixture/ListAssessmentAgents.yaml"

testListRulesPackages :: ListRulesPackages -> TestTree
testListRulesPackages = req
    "ListRulesPackages"
    "fixture/ListRulesPackages.yaml"

testRemoveAttributesFromFindings :: RemoveAttributesFromFindings -> TestTree
testRemoveAttributesFromFindings = req
    "RemoveAttributesFromFindings"
    "fixture/RemoveAttributesFromFindings.yaml"

testAttachAssessmentAndRulesPackage :: AttachAssessmentAndRulesPackage -> TestTree
testAttachAssessmentAndRulesPackage = req
    "AttachAssessmentAndRulesPackage"
    "fixture/AttachAssessmentAndRulesPackage.yaml"

-- Responses

testListFindingsResponse :: ListFindingsResponse -> TestTree
testListFindingsResponse = res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    inspector
    (Proxy :: Proxy ListFindings)

testDescribeRunResponse :: DescribeRunResponse -> TestTree
testDescribeRunResponse = res
    "DescribeRunResponse"
    "fixture/DescribeRunResponse.proto"
    inspector
    (Proxy :: Proxy DescribeRun)

testDescribeApplicationResponse :: DescribeApplicationResponse -> TestTree
testDescribeApplicationResponse = res
    "DescribeApplicationResponse"
    "fixture/DescribeApplicationResponse.proto"
    inspector
    (Proxy :: Proxy DescribeApplication)

testRunAssessmentResponse :: RunAssessmentResponse -> TestTree
testRunAssessmentResponse = res
    "RunAssessmentResponse"
    "fixture/RunAssessmentResponse.proto"
    inspector
    (Proxy :: Proxy RunAssessment)

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

testListAttachedAssessmentsResponse :: ListAttachedAssessmentsResponse -> TestTree
testListAttachedAssessmentsResponse = res
    "ListAttachedAssessmentsResponse"
    "fixture/ListAttachedAssessmentsResponse.proto"
    inspector
    (Proxy :: Proxy ListAttachedAssessments)

testDescribeFindingResponse :: DescribeFindingResponse -> TestTree
testDescribeFindingResponse = res
    "DescribeFindingResponse"
    "fixture/DescribeFindingResponse.proto"
    inspector
    (Proxy :: Proxy DescribeFinding)

testDeleteRunResponse :: DeleteRunResponse -> TestTree
testDeleteRunResponse = res
    "DeleteRunResponse"
    "fixture/DeleteRunResponse.proto"
    inspector
    (Proxy :: Proxy DeleteRun)

testListRunsResponse :: ListRunsResponse -> TestTree
testListRunsResponse = res
    "ListRunsResponse"
    "fixture/ListRunsResponse.proto"
    inspector
    (Proxy :: Proxy ListRuns)

testDeleteApplicationResponse :: DeleteApplicationResponse -> TestTree
testDeleteApplicationResponse = res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    inspector
    (Proxy :: Proxy DeleteApplication)

testUpdateApplicationResponse :: UpdateApplicationResponse -> TestTree
testUpdateApplicationResponse = res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    inspector
    (Proxy :: Proxy UpdateApplication)

testStartDataCollectionResponse :: StartDataCollectionResponse -> TestTree
testStartDataCollectionResponse = res
    "StartDataCollectionResponse"
    "fixture/StartDataCollectionResponse.proto"
    inspector
    (Proxy :: Proxy StartDataCollection)

testLocalizeTextResponse :: LocalizeTextResponse -> TestTree
testLocalizeTextResponse = res
    "LocalizeTextResponse"
    "fixture/LocalizeTextResponse.proto"
    inspector
    (Proxy :: Proxy LocalizeText)

testRegisterCrossAccountAccessRoleResponse :: RegisterCrossAccountAccessRoleResponse -> TestTree
testRegisterCrossAccountAccessRoleResponse = res
    "RegisterCrossAccountAccessRoleResponse"
    "fixture/RegisterCrossAccountAccessRoleResponse.proto"
    inspector
    (Proxy :: Proxy RegisterCrossAccountAccessRole)

testCreateApplicationResponse :: CreateApplicationResponse -> TestTree
testCreateApplicationResponse = res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    inspector
    (Proxy :: Proxy CreateApplication)

testCreateResourceGroupResponse :: CreateResourceGroupResponse -> TestTree
testCreateResourceGroupResponse = res
    "CreateResourceGroupResponse"
    "fixture/CreateResourceGroupResponse.proto"
    inspector
    (Proxy :: Proxy CreateResourceGroup)

testListAttachedRulesPackagesResponse :: ListAttachedRulesPackagesResponse -> TestTree
testListAttachedRulesPackagesResponse = res
    "ListAttachedRulesPackagesResponse"
    "fixture/ListAttachedRulesPackagesResponse.proto"
    inspector
    (Proxy :: Proxy ListAttachedRulesPackages)

testDeleteAssessmentResponse :: DeleteAssessmentResponse -> TestTree
testDeleteAssessmentResponse = res
    "DeleteAssessmentResponse"
    "fixture/DeleteAssessmentResponse.proto"
    inspector
    (Proxy :: Proxy DeleteAssessment)

testUpdateAssessmentResponse :: UpdateAssessmentResponse -> TestTree
testUpdateAssessmentResponse = res
    "UpdateAssessmentResponse"
    "fixture/UpdateAssessmentResponse.proto"
    inspector
    (Proxy :: Proxy UpdateAssessment)

testGetAssessmentTelemetryResponse :: GetAssessmentTelemetryResponse -> TestTree
testGetAssessmentTelemetryResponse = res
    "GetAssessmentTelemetryResponse"
    "fixture/GetAssessmentTelemetryResponse.proto"
    inspector
    (Proxy :: Proxy GetAssessmentTelemetry)

testListAssessmentsResponse :: ListAssessmentsResponse -> TestTree
testListAssessmentsResponse = res
    "ListAssessmentsResponse"
    "fixture/ListAssessmentsResponse.proto"
    inspector
    (Proxy :: Proxy ListAssessments)

testDescribeRulesPackageResponse :: DescribeRulesPackageResponse -> TestTree
testDescribeRulesPackageResponse = res
    "DescribeRulesPackageResponse"
    "fixture/DescribeRulesPackageResponse.proto"
    inspector
    (Proxy :: Proxy DescribeRulesPackage)

testCreateAssessmentResponse :: CreateAssessmentResponse -> TestTree
testCreateAssessmentResponse = res
    "CreateAssessmentResponse"
    "fixture/CreateAssessmentResponse.proto"
    inspector
    (Proxy :: Proxy CreateAssessment)

testDetachAssessmentAndRulesPackageResponse :: DetachAssessmentAndRulesPackageResponse -> TestTree
testDetachAssessmentAndRulesPackageResponse = res
    "DetachAssessmentAndRulesPackageResponse"
    "fixture/DetachAssessmentAndRulesPackageResponse.proto"
    inspector
    (Proxy :: Proxy DetachAssessmentAndRulesPackage)

testDescribeResourceGroupResponse :: DescribeResourceGroupResponse -> TestTree
testDescribeResourceGroupResponse = res
    "DescribeResourceGroupResponse"
    "fixture/DescribeResourceGroupResponse.proto"
    inspector
    (Proxy :: Proxy DescribeResourceGroup)

testListApplicationsResponse :: ListApplicationsResponse -> TestTree
testListApplicationsResponse = res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    inspector
    (Proxy :: Proxy ListApplications)

testDescribeAssessmentResponse :: DescribeAssessmentResponse -> TestTree
testDescribeAssessmentResponse = res
    "DescribeAssessmentResponse"
    "fixture/DescribeAssessmentResponse.proto"
    inspector
    (Proxy :: Proxy DescribeAssessment)

testAddAttributesToFindingsResponse :: AddAttributesToFindingsResponse -> TestTree
testAddAttributesToFindingsResponse = res
    "AddAttributesToFindingsResponse"
    "fixture/AddAttributesToFindingsResponse.proto"
    inspector
    (Proxy :: Proxy AddAttributesToFindings)

testStopDataCollectionResponse :: StopDataCollectionResponse -> TestTree
testStopDataCollectionResponse = res
    "StopDataCollectionResponse"
    "fixture/StopDataCollectionResponse.proto"
    inspector
    (Proxy :: Proxy StopDataCollection)

testPreviewAgentsForResourceGroupResponse :: PreviewAgentsForResourceGroupResponse -> TestTree
testPreviewAgentsForResourceGroupResponse = res
    "PreviewAgentsForResourceGroupResponse"
    "fixture/PreviewAgentsForResourceGroupResponse.proto"
    inspector
    (Proxy :: Proxy PreviewAgentsForResourceGroup)

testListAssessmentAgentsResponse :: ListAssessmentAgentsResponse -> TestTree
testListAssessmentAgentsResponse = res
    "ListAssessmentAgentsResponse"
    "fixture/ListAssessmentAgentsResponse.proto"
    inspector
    (Proxy :: Proxy ListAssessmentAgents)

testListRulesPackagesResponse :: ListRulesPackagesResponse -> TestTree
testListRulesPackagesResponse = res
    "ListRulesPackagesResponse"
    "fixture/ListRulesPackagesResponse.proto"
    inspector
    (Proxy :: Proxy ListRulesPackages)

testRemoveAttributesFromFindingsResponse :: RemoveAttributesFromFindingsResponse -> TestTree
testRemoveAttributesFromFindingsResponse = res
    "RemoveAttributesFromFindingsResponse"
    "fixture/RemoveAttributesFromFindingsResponse.proto"
    inspector
    (Proxy :: Proxy RemoveAttributesFromFindings)

testAttachAssessmentAndRulesPackageResponse :: AttachAssessmentAndRulesPackageResponse -> TestTree
testAttachAssessmentAndRulesPackageResponse = res
    "AttachAssessmentAndRulesPackageResponse"
    "fixture/AttachAssessmentAndRulesPackageResponse.proto"
    inspector
    (Proxy :: Proxy AttachAssessmentAndRulesPackage)
