{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Backup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Backup where

import Amazonka.Backup
import qualified Data.Proxy as Proxy
import Test.Amazonka.Backup.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelLegalHold $
--             newCancelLegalHold
--
--         , requestCreateBackupPlan $
--             newCreateBackupPlan
--
--         , requestCreateBackupSelection $
--             newCreateBackupSelection
--
--         , requestCreateBackupVault $
--             newCreateBackupVault
--
--         , requestCreateFramework $
--             newCreateFramework
--
--         , requestCreateLegalHold $
--             newCreateLegalHold
--
--         , requestCreateReportPlan $
--             newCreateReportPlan
--
--         , requestDeleteBackupPlan $
--             newDeleteBackupPlan
--
--         , requestDeleteBackupSelection $
--             newDeleteBackupSelection
--
--         , requestDeleteBackupVault $
--             newDeleteBackupVault
--
--         , requestDeleteBackupVaultAccessPolicy $
--             newDeleteBackupVaultAccessPolicy
--
--         , requestDeleteBackupVaultLockConfiguration $
--             newDeleteBackupVaultLockConfiguration
--
--         , requestDeleteBackupVaultNotifications $
--             newDeleteBackupVaultNotifications
--
--         , requestDeleteFramework $
--             newDeleteFramework
--
--         , requestDeleteRecoveryPoint $
--             newDeleteRecoveryPoint
--
--         , requestDeleteReportPlan $
--             newDeleteReportPlan
--
--         , requestDescribeBackupJob $
--             newDescribeBackupJob
--
--         , requestDescribeBackupVault $
--             newDescribeBackupVault
--
--         , requestDescribeCopyJob $
--             newDescribeCopyJob
--
--         , requestDescribeFramework $
--             newDescribeFramework
--
--         , requestDescribeGlobalSettings $
--             newDescribeGlobalSettings
--
--         , requestDescribeProtectedResource $
--             newDescribeProtectedResource
--
--         , requestDescribeRecoveryPoint $
--             newDescribeRecoveryPoint
--
--         , requestDescribeRegionSettings $
--             newDescribeRegionSettings
--
--         , requestDescribeReportJob $
--             newDescribeReportJob
--
--         , requestDescribeReportPlan $
--             newDescribeReportPlan
--
--         , requestDescribeRestoreJob $
--             newDescribeRestoreJob
--
--         , requestDisassociateRecoveryPoint $
--             newDisassociateRecoveryPoint
--
--         , requestDisassociateRecoveryPointFromParent $
--             newDisassociateRecoveryPointFromParent
--
--         , requestExportBackupPlanTemplate $
--             newExportBackupPlanTemplate
--
--         , requestGetBackupPlan $
--             newGetBackupPlan
--
--         , requestGetBackupPlanFromJSON $
--             newGetBackupPlanFromJSON
--
--         , requestGetBackupPlanFromTemplate $
--             newGetBackupPlanFromTemplate
--
--         , requestGetBackupSelection $
--             newGetBackupSelection
--
--         , requestGetBackupVaultAccessPolicy $
--             newGetBackupVaultAccessPolicy
--
--         , requestGetBackupVaultNotifications $
--             newGetBackupVaultNotifications
--
--         , requestGetLegalHold $
--             newGetLegalHold
--
--         , requestGetRecoveryPointRestoreMetadata $
--             newGetRecoveryPointRestoreMetadata
--
--         , requestGetSupportedResourceTypes $
--             newGetSupportedResourceTypes
--
--         , requestListBackupJobs $
--             newListBackupJobs
--
--         , requestListBackupPlanTemplates $
--             newListBackupPlanTemplates
--
--         , requestListBackupPlanVersions $
--             newListBackupPlanVersions
--
--         , requestListBackupPlans $
--             newListBackupPlans
--
--         , requestListBackupSelections $
--             newListBackupSelections
--
--         , requestListBackupVaults $
--             newListBackupVaults
--
--         , requestListCopyJobs $
--             newListCopyJobs
--
--         , requestListFrameworks $
--             newListFrameworks
--
--         , requestListLegalHolds $
--             newListLegalHolds
--
--         , requestListProtectedResources $
--             newListProtectedResources
--
--         , requestListRecoveryPointsByBackupVault $
--             newListRecoveryPointsByBackupVault
--
--         , requestListRecoveryPointsByLegalHold $
--             newListRecoveryPointsByLegalHold
--
--         , requestListRecoveryPointsByResource $
--             newListRecoveryPointsByResource
--
--         , requestListReportJobs $
--             newListReportJobs
--
--         , requestListReportPlans $
--             newListReportPlans
--
--         , requestListRestoreJobs $
--             newListRestoreJobs
--
--         , requestListTags $
--             newListTags
--
--         , requestPutBackupVaultAccessPolicy $
--             newPutBackupVaultAccessPolicy
--
--         , requestPutBackupVaultLockConfiguration $
--             newPutBackupVaultLockConfiguration
--
--         , requestPutBackupVaultNotifications $
--             newPutBackupVaultNotifications
--
--         , requestStartBackupJob $
--             newStartBackupJob
--
--         , requestStartCopyJob $
--             newStartCopyJob
--
--         , requestStartReportJob $
--             newStartReportJob
--
--         , requestStartRestoreJob $
--             newStartRestoreJob
--
--         , requestStopBackupJob $
--             newStopBackupJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateBackupPlan $
--             newUpdateBackupPlan
--
--         , requestUpdateFramework $
--             newUpdateFramework
--
--         , requestUpdateGlobalSettings $
--             newUpdateGlobalSettings
--
--         , requestUpdateRecoveryPointLifecycle $
--             newUpdateRecoveryPointLifecycle
--
--         , requestUpdateRegionSettings $
--             newUpdateRegionSettings
--
--         , requestUpdateReportPlan $
--             newUpdateReportPlan
--
--           ]

--     , testGroup "response"
--         [ responseCancelLegalHold $
--             newCancelLegalHoldResponse
--
--         , responseCreateBackupPlan $
--             newCreateBackupPlanResponse
--
--         , responseCreateBackupSelection $
--             newCreateBackupSelectionResponse
--
--         , responseCreateBackupVault $
--             newCreateBackupVaultResponse
--
--         , responseCreateFramework $
--             newCreateFrameworkResponse
--
--         , responseCreateLegalHold $
--             newCreateLegalHoldResponse
--
--         , responseCreateReportPlan $
--             newCreateReportPlanResponse
--
--         , responseDeleteBackupPlan $
--             newDeleteBackupPlanResponse
--
--         , responseDeleteBackupSelection $
--             newDeleteBackupSelectionResponse
--
--         , responseDeleteBackupVault $
--             newDeleteBackupVaultResponse
--
--         , responseDeleteBackupVaultAccessPolicy $
--             newDeleteBackupVaultAccessPolicyResponse
--
--         , responseDeleteBackupVaultLockConfiguration $
--             newDeleteBackupVaultLockConfigurationResponse
--
--         , responseDeleteBackupVaultNotifications $
--             newDeleteBackupVaultNotificationsResponse
--
--         , responseDeleteFramework $
--             newDeleteFrameworkResponse
--
--         , responseDeleteRecoveryPoint $
--             newDeleteRecoveryPointResponse
--
--         , responseDeleteReportPlan $
--             newDeleteReportPlanResponse
--
--         , responseDescribeBackupJob $
--             newDescribeBackupJobResponse
--
--         , responseDescribeBackupVault $
--             newDescribeBackupVaultResponse
--
--         , responseDescribeCopyJob $
--             newDescribeCopyJobResponse
--
--         , responseDescribeFramework $
--             newDescribeFrameworkResponse
--
--         , responseDescribeGlobalSettings $
--             newDescribeGlobalSettingsResponse
--
--         , responseDescribeProtectedResource $
--             newDescribeProtectedResourceResponse
--
--         , responseDescribeRecoveryPoint $
--             newDescribeRecoveryPointResponse
--
--         , responseDescribeRegionSettings $
--             newDescribeRegionSettingsResponse
--
--         , responseDescribeReportJob $
--             newDescribeReportJobResponse
--
--         , responseDescribeReportPlan $
--             newDescribeReportPlanResponse
--
--         , responseDescribeRestoreJob $
--             newDescribeRestoreJobResponse
--
--         , responseDisassociateRecoveryPoint $
--             newDisassociateRecoveryPointResponse
--
--         , responseDisassociateRecoveryPointFromParent $
--             newDisassociateRecoveryPointFromParentResponse
--
--         , responseExportBackupPlanTemplate $
--             newExportBackupPlanTemplateResponse
--
--         , responseGetBackupPlan $
--             newGetBackupPlanResponse
--
--         , responseGetBackupPlanFromJSON $
--             newGetBackupPlanFromJSONResponse
--
--         , responseGetBackupPlanFromTemplate $
--             newGetBackupPlanFromTemplateResponse
--
--         , responseGetBackupSelection $
--             newGetBackupSelectionResponse
--
--         , responseGetBackupVaultAccessPolicy $
--             newGetBackupVaultAccessPolicyResponse
--
--         , responseGetBackupVaultNotifications $
--             newGetBackupVaultNotificationsResponse
--
--         , responseGetLegalHold $
--             newGetLegalHoldResponse
--
--         , responseGetRecoveryPointRestoreMetadata $
--             newGetRecoveryPointRestoreMetadataResponse
--
--         , responseGetSupportedResourceTypes $
--             newGetSupportedResourceTypesResponse
--
--         , responseListBackupJobs $
--             newListBackupJobsResponse
--
--         , responseListBackupPlanTemplates $
--             newListBackupPlanTemplatesResponse
--
--         , responseListBackupPlanVersions $
--             newListBackupPlanVersionsResponse
--
--         , responseListBackupPlans $
--             newListBackupPlansResponse
--
--         , responseListBackupSelections $
--             newListBackupSelectionsResponse
--
--         , responseListBackupVaults $
--             newListBackupVaultsResponse
--
--         , responseListCopyJobs $
--             newListCopyJobsResponse
--
--         , responseListFrameworks $
--             newListFrameworksResponse
--
--         , responseListLegalHolds $
--             newListLegalHoldsResponse
--
--         , responseListProtectedResources $
--             newListProtectedResourcesResponse
--
--         , responseListRecoveryPointsByBackupVault $
--             newListRecoveryPointsByBackupVaultResponse
--
--         , responseListRecoveryPointsByLegalHold $
--             newListRecoveryPointsByLegalHoldResponse
--
--         , responseListRecoveryPointsByResource $
--             newListRecoveryPointsByResourceResponse
--
--         , responseListReportJobs $
--             newListReportJobsResponse
--
--         , responseListReportPlans $
--             newListReportPlansResponse
--
--         , responseListRestoreJobs $
--             newListRestoreJobsResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responsePutBackupVaultAccessPolicy $
--             newPutBackupVaultAccessPolicyResponse
--
--         , responsePutBackupVaultLockConfiguration $
--             newPutBackupVaultLockConfigurationResponse
--
--         , responsePutBackupVaultNotifications $
--             newPutBackupVaultNotificationsResponse
--
--         , responseStartBackupJob $
--             newStartBackupJobResponse
--
--         , responseStartCopyJob $
--             newStartCopyJobResponse
--
--         , responseStartReportJob $
--             newStartReportJobResponse
--
--         , responseStartRestoreJob $
--             newStartRestoreJobResponse
--
--         , responseStopBackupJob $
--             newStopBackupJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateBackupPlan $
--             newUpdateBackupPlanResponse
--
--         , responseUpdateFramework $
--             newUpdateFrameworkResponse
--
--         , responseUpdateGlobalSettings $
--             newUpdateGlobalSettingsResponse
--
--         , responseUpdateRecoveryPointLifecycle $
--             newUpdateRecoveryPointLifecycleResponse
--
--         , responseUpdateRegionSettings $
--             newUpdateRegionSettingsResponse
--
--         , responseUpdateReportPlan $
--             newUpdateReportPlanResponse
--
--           ]
--     ]

-- Requests

requestCancelLegalHold :: CancelLegalHold -> TestTree
requestCancelLegalHold =
  req
    "CancelLegalHold"
    "fixture/CancelLegalHold.yaml"

requestCreateBackupPlan :: CreateBackupPlan -> TestTree
requestCreateBackupPlan =
  req
    "CreateBackupPlan"
    "fixture/CreateBackupPlan.yaml"

requestCreateBackupSelection :: CreateBackupSelection -> TestTree
requestCreateBackupSelection =
  req
    "CreateBackupSelection"
    "fixture/CreateBackupSelection.yaml"

requestCreateBackupVault :: CreateBackupVault -> TestTree
requestCreateBackupVault =
  req
    "CreateBackupVault"
    "fixture/CreateBackupVault.yaml"

requestCreateFramework :: CreateFramework -> TestTree
requestCreateFramework =
  req
    "CreateFramework"
    "fixture/CreateFramework.yaml"

requestCreateLegalHold :: CreateLegalHold -> TestTree
requestCreateLegalHold =
  req
    "CreateLegalHold"
    "fixture/CreateLegalHold.yaml"

requestCreateReportPlan :: CreateReportPlan -> TestTree
requestCreateReportPlan =
  req
    "CreateReportPlan"
    "fixture/CreateReportPlan.yaml"

requestDeleteBackupPlan :: DeleteBackupPlan -> TestTree
requestDeleteBackupPlan =
  req
    "DeleteBackupPlan"
    "fixture/DeleteBackupPlan.yaml"

requestDeleteBackupSelection :: DeleteBackupSelection -> TestTree
requestDeleteBackupSelection =
  req
    "DeleteBackupSelection"
    "fixture/DeleteBackupSelection.yaml"

requestDeleteBackupVault :: DeleteBackupVault -> TestTree
requestDeleteBackupVault =
  req
    "DeleteBackupVault"
    "fixture/DeleteBackupVault.yaml"

requestDeleteBackupVaultAccessPolicy :: DeleteBackupVaultAccessPolicy -> TestTree
requestDeleteBackupVaultAccessPolicy =
  req
    "DeleteBackupVaultAccessPolicy"
    "fixture/DeleteBackupVaultAccessPolicy.yaml"

requestDeleteBackupVaultLockConfiguration :: DeleteBackupVaultLockConfiguration -> TestTree
requestDeleteBackupVaultLockConfiguration =
  req
    "DeleteBackupVaultLockConfiguration"
    "fixture/DeleteBackupVaultLockConfiguration.yaml"

requestDeleteBackupVaultNotifications :: DeleteBackupVaultNotifications -> TestTree
requestDeleteBackupVaultNotifications =
  req
    "DeleteBackupVaultNotifications"
    "fixture/DeleteBackupVaultNotifications.yaml"

requestDeleteFramework :: DeleteFramework -> TestTree
requestDeleteFramework =
  req
    "DeleteFramework"
    "fixture/DeleteFramework.yaml"

requestDeleteRecoveryPoint :: DeleteRecoveryPoint -> TestTree
requestDeleteRecoveryPoint =
  req
    "DeleteRecoveryPoint"
    "fixture/DeleteRecoveryPoint.yaml"

requestDeleteReportPlan :: DeleteReportPlan -> TestTree
requestDeleteReportPlan =
  req
    "DeleteReportPlan"
    "fixture/DeleteReportPlan.yaml"

requestDescribeBackupJob :: DescribeBackupJob -> TestTree
requestDescribeBackupJob =
  req
    "DescribeBackupJob"
    "fixture/DescribeBackupJob.yaml"

requestDescribeBackupVault :: DescribeBackupVault -> TestTree
requestDescribeBackupVault =
  req
    "DescribeBackupVault"
    "fixture/DescribeBackupVault.yaml"

requestDescribeCopyJob :: DescribeCopyJob -> TestTree
requestDescribeCopyJob =
  req
    "DescribeCopyJob"
    "fixture/DescribeCopyJob.yaml"

requestDescribeFramework :: DescribeFramework -> TestTree
requestDescribeFramework =
  req
    "DescribeFramework"
    "fixture/DescribeFramework.yaml"

requestDescribeGlobalSettings :: DescribeGlobalSettings -> TestTree
requestDescribeGlobalSettings =
  req
    "DescribeGlobalSettings"
    "fixture/DescribeGlobalSettings.yaml"

requestDescribeProtectedResource :: DescribeProtectedResource -> TestTree
requestDescribeProtectedResource =
  req
    "DescribeProtectedResource"
    "fixture/DescribeProtectedResource.yaml"

requestDescribeRecoveryPoint :: DescribeRecoveryPoint -> TestTree
requestDescribeRecoveryPoint =
  req
    "DescribeRecoveryPoint"
    "fixture/DescribeRecoveryPoint.yaml"

requestDescribeRegionSettings :: DescribeRegionSettings -> TestTree
requestDescribeRegionSettings =
  req
    "DescribeRegionSettings"
    "fixture/DescribeRegionSettings.yaml"

requestDescribeReportJob :: DescribeReportJob -> TestTree
requestDescribeReportJob =
  req
    "DescribeReportJob"
    "fixture/DescribeReportJob.yaml"

requestDescribeReportPlan :: DescribeReportPlan -> TestTree
requestDescribeReportPlan =
  req
    "DescribeReportPlan"
    "fixture/DescribeReportPlan.yaml"

requestDescribeRestoreJob :: DescribeRestoreJob -> TestTree
requestDescribeRestoreJob =
  req
    "DescribeRestoreJob"
    "fixture/DescribeRestoreJob.yaml"

requestDisassociateRecoveryPoint :: DisassociateRecoveryPoint -> TestTree
requestDisassociateRecoveryPoint =
  req
    "DisassociateRecoveryPoint"
    "fixture/DisassociateRecoveryPoint.yaml"

requestDisassociateRecoveryPointFromParent :: DisassociateRecoveryPointFromParent -> TestTree
requestDisassociateRecoveryPointFromParent =
  req
    "DisassociateRecoveryPointFromParent"
    "fixture/DisassociateRecoveryPointFromParent.yaml"

requestExportBackupPlanTemplate :: ExportBackupPlanTemplate -> TestTree
requestExportBackupPlanTemplate =
  req
    "ExportBackupPlanTemplate"
    "fixture/ExportBackupPlanTemplate.yaml"

requestGetBackupPlan :: GetBackupPlan -> TestTree
requestGetBackupPlan =
  req
    "GetBackupPlan"
    "fixture/GetBackupPlan.yaml"

requestGetBackupPlanFromJSON :: GetBackupPlanFromJSON -> TestTree
requestGetBackupPlanFromJSON =
  req
    "GetBackupPlanFromJSON"
    "fixture/GetBackupPlanFromJSON.yaml"

requestGetBackupPlanFromTemplate :: GetBackupPlanFromTemplate -> TestTree
requestGetBackupPlanFromTemplate =
  req
    "GetBackupPlanFromTemplate"
    "fixture/GetBackupPlanFromTemplate.yaml"

requestGetBackupSelection :: GetBackupSelection -> TestTree
requestGetBackupSelection =
  req
    "GetBackupSelection"
    "fixture/GetBackupSelection.yaml"

requestGetBackupVaultAccessPolicy :: GetBackupVaultAccessPolicy -> TestTree
requestGetBackupVaultAccessPolicy =
  req
    "GetBackupVaultAccessPolicy"
    "fixture/GetBackupVaultAccessPolicy.yaml"

requestGetBackupVaultNotifications :: GetBackupVaultNotifications -> TestTree
requestGetBackupVaultNotifications =
  req
    "GetBackupVaultNotifications"
    "fixture/GetBackupVaultNotifications.yaml"

requestGetLegalHold :: GetLegalHold -> TestTree
requestGetLegalHold =
  req
    "GetLegalHold"
    "fixture/GetLegalHold.yaml"

requestGetRecoveryPointRestoreMetadata :: GetRecoveryPointRestoreMetadata -> TestTree
requestGetRecoveryPointRestoreMetadata =
  req
    "GetRecoveryPointRestoreMetadata"
    "fixture/GetRecoveryPointRestoreMetadata.yaml"

requestGetSupportedResourceTypes :: GetSupportedResourceTypes -> TestTree
requestGetSupportedResourceTypes =
  req
    "GetSupportedResourceTypes"
    "fixture/GetSupportedResourceTypes.yaml"

requestListBackupJobs :: ListBackupJobs -> TestTree
requestListBackupJobs =
  req
    "ListBackupJobs"
    "fixture/ListBackupJobs.yaml"

requestListBackupPlanTemplates :: ListBackupPlanTemplates -> TestTree
requestListBackupPlanTemplates =
  req
    "ListBackupPlanTemplates"
    "fixture/ListBackupPlanTemplates.yaml"

requestListBackupPlanVersions :: ListBackupPlanVersions -> TestTree
requestListBackupPlanVersions =
  req
    "ListBackupPlanVersions"
    "fixture/ListBackupPlanVersions.yaml"

requestListBackupPlans :: ListBackupPlans -> TestTree
requestListBackupPlans =
  req
    "ListBackupPlans"
    "fixture/ListBackupPlans.yaml"

requestListBackupSelections :: ListBackupSelections -> TestTree
requestListBackupSelections =
  req
    "ListBackupSelections"
    "fixture/ListBackupSelections.yaml"

requestListBackupVaults :: ListBackupVaults -> TestTree
requestListBackupVaults =
  req
    "ListBackupVaults"
    "fixture/ListBackupVaults.yaml"

requestListCopyJobs :: ListCopyJobs -> TestTree
requestListCopyJobs =
  req
    "ListCopyJobs"
    "fixture/ListCopyJobs.yaml"

requestListFrameworks :: ListFrameworks -> TestTree
requestListFrameworks =
  req
    "ListFrameworks"
    "fixture/ListFrameworks.yaml"

requestListLegalHolds :: ListLegalHolds -> TestTree
requestListLegalHolds =
  req
    "ListLegalHolds"
    "fixture/ListLegalHolds.yaml"

requestListProtectedResources :: ListProtectedResources -> TestTree
requestListProtectedResources =
  req
    "ListProtectedResources"
    "fixture/ListProtectedResources.yaml"

requestListRecoveryPointsByBackupVault :: ListRecoveryPointsByBackupVault -> TestTree
requestListRecoveryPointsByBackupVault =
  req
    "ListRecoveryPointsByBackupVault"
    "fixture/ListRecoveryPointsByBackupVault.yaml"

requestListRecoveryPointsByLegalHold :: ListRecoveryPointsByLegalHold -> TestTree
requestListRecoveryPointsByLegalHold =
  req
    "ListRecoveryPointsByLegalHold"
    "fixture/ListRecoveryPointsByLegalHold.yaml"

requestListRecoveryPointsByResource :: ListRecoveryPointsByResource -> TestTree
requestListRecoveryPointsByResource =
  req
    "ListRecoveryPointsByResource"
    "fixture/ListRecoveryPointsByResource.yaml"

requestListReportJobs :: ListReportJobs -> TestTree
requestListReportJobs =
  req
    "ListReportJobs"
    "fixture/ListReportJobs.yaml"

requestListReportPlans :: ListReportPlans -> TestTree
requestListReportPlans =
  req
    "ListReportPlans"
    "fixture/ListReportPlans.yaml"

requestListRestoreJobs :: ListRestoreJobs -> TestTree
requestListRestoreJobs =
  req
    "ListRestoreJobs"
    "fixture/ListRestoreJobs.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestPutBackupVaultAccessPolicy :: PutBackupVaultAccessPolicy -> TestTree
requestPutBackupVaultAccessPolicy =
  req
    "PutBackupVaultAccessPolicy"
    "fixture/PutBackupVaultAccessPolicy.yaml"

requestPutBackupVaultLockConfiguration :: PutBackupVaultLockConfiguration -> TestTree
requestPutBackupVaultLockConfiguration =
  req
    "PutBackupVaultLockConfiguration"
    "fixture/PutBackupVaultLockConfiguration.yaml"

requestPutBackupVaultNotifications :: PutBackupVaultNotifications -> TestTree
requestPutBackupVaultNotifications =
  req
    "PutBackupVaultNotifications"
    "fixture/PutBackupVaultNotifications.yaml"

requestStartBackupJob :: StartBackupJob -> TestTree
requestStartBackupJob =
  req
    "StartBackupJob"
    "fixture/StartBackupJob.yaml"

requestStartCopyJob :: StartCopyJob -> TestTree
requestStartCopyJob =
  req
    "StartCopyJob"
    "fixture/StartCopyJob.yaml"

requestStartReportJob :: StartReportJob -> TestTree
requestStartReportJob =
  req
    "StartReportJob"
    "fixture/StartReportJob.yaml"

requestStartRestoreJob :: StartRestoreJob -> TestTree
requestStartRestoreJob =
  req
    "StartRestoreJob"
    "fixture/StartRestoreJob.yaml"

requestStopBackupJob :: StopBackupJob -> TestTree
requestStopBackupJob =
  req
    "StopBackupJob"
    "fixture/StopBackupJob.yaml"

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

requestUpdateBackupPlan :: UpdateBackupPlan -> TestTree
requestUpdateBackupPlan =
  req
    "UpdateBackupPlan"
    "fixture/UpdateBackupPlan.yaml"

requestUpdateFramework :: UpdateFramework -> TestTree
requestUpdateFramework =
  req
    "UpdateFramework"
    "fixture/UpdateFramework.yaml"

requestUpdateGlobalSettings :: UpdateGlobalSettings -> TestTree
requestUpdateGlobalSettings =
  req
    "UpdateGlobalSettings"
    "fixture/UpdateGlobalSettings.yaml"

requestUpdateRecoveryPointLifecycle :: UpdateRecoveryPointLifecycle -> TestTree
requestUpdateRecoveryPointLifecycle =
  req
    "UpdateRecoveryPointLifecycle"
    "fixture/UpdateRecoveryPointLifecycle.yaml"

requestUpdateRegionSettings :: UpdateRegionSettings -> TestTree
requestUpdateRegionSettings =
  req
    "UpdateRegionSettings"
    "fixture/UpdateRegionSettings.yaml"

requestUpdateReportPlan :: UpdateReportPlan -> TestTree
requestUpdateReportPlan =
  req
    "UpdateReportPlan"
    "fixture/UpdateReportPlan.yaml"

-- Responses

responseCancelLegalHold :: CancelLegalHoldResponse -> TestTree
responseCancelLegalHold =
  res
    "CancelLegalHoldResponse"
    "fixture/CancelLegalHoldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelLegalHold)

responseCreateBackupPlan :: CreateBackupPlanResponse -> TestTree
responseCreateBackupPlan =
  res
    "CreateBackupPlanResponse"
    "fixture/CreateBackupPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackupPlan)

responseCreateBackupSelection :: CreateBackupSelectionResponse -> TestTree
responseCreateBackupSelection =
  res
    "CreateBackupSelectionResponse"
    "fixture/CreateBackupSelectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackupSelection)

responseCreateBackupVault :: CreateBackupVaultResponse -> TestTree
responseCreateBackupVault =
  res
    "CreateBackupVaultResponse"
    "fixture/CreateBackupVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackupVault)

responseCreateFramework :: CreateFrameworkResponse -> TestTree
responseCreateFramework =
  res
    "CreateFrameworkResponse"
    "fixture/CreateFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFramework)

responseCreateLegalHold :: CreateLegalHoldResponse -> TestTree
responseCreateLegalHold =
  res
    "CreateLegalHoldResponse"
    "fixture/CreateLegalHoldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLegalHold)

responseCreateReportPlan :: CreateReportPlanResponse -> TestTree
responseCreateReportPlan =
  res
    "CreateReportPlanResponse"
    "fixture/CreateReportPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReportPlan)

responseDeleteBackupPlan :: DeleteBackupPlanResponse -> TestTree
responseDeleteBackupPlan =
  res
    "DeleteBackupPlanResponse"
    "fixture/DeleteBackupPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupPlan)

responseDeleteBackupSelection :: DeleteBackupSelectionResponse -> TestTree
responseDeleteBackupSelection =
  res
    "DeleteBackupSelectionResponse"
    "fixture/DeleteBackupSelectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupSelection)

responseDeleteBackupVault :: DeleteBackupVaultResponse -> TestTree
responseDeleteBackupVault =
  res
    "DeleteBackupVaultResponse"
    "fixture/DeleteBackupVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupVault)

responseDeleteBackupVaultAccessPolicy :: DeleteBackupVaultAccessPolicyResponse -> TestTree
responseDeleteBackupVaultAccessPolicy =
  res
    "DeleteBackupVaultAccessPolicyResponse"
    "fixture/DeleteBackupVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupVaultAccessPolicy)

responseDeleteBackupVaultLockConfiguration :: DeleteBackupVaultLockConfigurationResponse -> TestTree
responseDeleteBackupVaultLockConfiguration =
  res
    "DeleteBackupVaultLockConfigurationResponse"
    "fixture/DeleteBackupVaultLockConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupVaultLockConfiguration)

responseDeleteBackupVaultNotifications :: DeleteBackupVaultNotificationsResponse -> TestTree
responseDeleteBackupVaultNotifications =
  res
    "DeleteBackupVaultNotificationsResponse"
    "fixture/DeleteBackupVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupVaultNotifications)

responseDeleteFramework :: DeleteFrameworkResponse -> TestTree
responseDeleteFramework =
  res
    "DeleteFrameworkResponse"
    "fixture/DeleteFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFramework)

responseDeleteRecoveryPoint :: DeleteRecoveryPointResponse -> TestTree
responseDeleteRecoveryPoint =
  res
    "DeleteRecoveryPointResponse"
    "fixture/DeleteRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecoveryPoint)

responseDeleteReportPlan :: DeleteReportPlanResponse -> TestTree
responseDeleteReportPlan =
  res
    "DeleteReportPlanResponse"
    "fixture/DeleteReportPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReportPlan)

responseDescribeBackupJob :: DescribeBackupJobResponse -> TestTree
responseDescribeBackupJob =
  res
    "DescribeBackupJobResponse"
    "fixture/DescribeBackupJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackupJob)

responseDescribeBackupVault :: DescribeBackupVaultResponse -> TestTree
responseDescribeBackupVault =
  res
    "DescribeBackupVaultResponse"
    "fixture/DescribeBackupVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackupVault)

responseDescribeCopyJob :: DescribeCopyJobResponse -> TestTree
responseDescribeCopyJob =
  res
    "DescribeCopyJobResponse"
    "fixture/DescribeCopyJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCopyJob)

responseDescribeFramework :: DescribeFrameworkResponse -> TestTree
responseDescribeFramework =
  res
    "DescribeFrameworkResponse"
    "fixture/DescribeFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFramework)

responseDescribeGlobalSettings :: DescribeGlobalSettingsResponse -> TestTree
responseDescribeGlobalSettings =
  res
    "DescribeGlobalSettingsResponse"
    "fixture/DescribeGlobalSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalSettings)

responseDescribeProtectedResource :: DescribeProtectedResourceResponse -> TestTree
responseDescribeProtectedResource =
  res
    "DescribeProtectedResourceResponse"
    "fixture/DescribeProtectedResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProtectedResource)

responseDescribeRecoveryPoint :: DescribeRecoveryPointResponse -> TestTree
responseDescribeRecoveryPoint =
  res
    "DescribeRecoveryPointResponse"
    "fixture/DescribeRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecoveryPoint)

responseDescribeRegionSettings :: DescribeRegionSettingsResponse -> TestTree
responseDescribeRegionSettings =
  res
    "DescribeRegionSettingsResponse"
    "fixture/DescribeRegionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegionSettings)

responseDescribeReportJob :: DescribeReportJobResponse -> TestTree
responseDescribeReportJob =
  res
    "DescribeReportJobResponse"
    "fixture/DescribeReportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReportJob)

responseDescribeReportPlan :: DescribeReportPlanResponse -> TestTree
responseDescribeReportPlan =
  res
    "DescribeReportPlanResponse"
    "fixture/DescribeReportPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReportPlan)

responseDescribeRestoreJob :: DescribeRestoreJobResponse -> TestTree
responseDescribeRestoreJob =
  res
    "DescribeRestoreJobResponse"
    "fixture/DescribeRestoreJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRestoreJob)

responseDisassociateRecoveryPoint :: DisassociateRecoveryPointResponse -> TestTree
responseDisassociateRecoveryPoint =
  res
    "DisassociateRecoveryPointResponse"
    "fixture/DisassociateRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRecoveryPoint)

responseDisassociateRecoveryPointFromParent :: DisassociateRecoveryPointFromParentResponse -> TestTree
responseDisassociateRecoveryPointFromParent =
  res
    "DisassociateRecoveryPointFromParentResponse"
    "fixture/DisassociateRecoveryPointFromParentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRecoveryPointFromParent)

responseExportBackupPlanTemplate :: ExportBackupPlanTemplateResponse -> TestTree
responseExportBackupPlanTemplate =
  res
    "ExportBackupPlanTemplateResponse"
    "fixture/ExportBackupPlanTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportBackupPlanTemplate)

responseGetBackupPlan :: GetBackupPlanResponse -> TestTree
responseGetBackupPlan =
  res
    "GetBackupPlanResponse"
    "fixture/GetBackupPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupPlan)

responseGetBackupPlanFromJSON :: GetBackupPlanFromJSONResponse -> TestTree
responseGetBackupPlanFromJSON =
  res
    "GetBackupPlanFromJSONResponse"
    "fixture/GetBackupPlanFromJSONResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupPlanFromJSON)

responseGetBackupPlanFromTemplate :: GetBackupPlanFromTemplateResponse -> TestTree
responseGetBackupPlanFromTemplate =
  res
    "GetBackupPlanFromTemplateResponse"
    "fixture/GetBackupPlanFromTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupPlanFromTemplate)

responseGetBackupSelection :: GetBackupSelectionResponse -> TestTree
responseGetBackupSelection =
  res
    "GetBackupSelectionResponse"
    "fixture/GetBackupSelectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupSelection)

responseGetBackupVaultAccessPolicy :: GetBackupVaultAccessPolicyResponse -> TestTree
responseGetBackupVaultAccessPolicy =
  res
    "GetBackupVaultAccessPolicyResponse"
    "fixture/GetBackupVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupVaultAccessPolicy)

responseGetBackupVaultNotifications :: GetBackupVaultNotificationsResponse -> TestTree
responseGetBackupVaultNotifications =
  res
    "GetBackupVaultNotificationsResponse"
    "fixture/GetBackupVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupVaultNotifications)

responseGetLegalHold :: GetLegalHoldResponse -> TestTree
responseGetLegalHold =
  res
    "GetLegalHoldResponse"
    "fixture/GetLegalHoldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLegalHold)

responseGetRecoveryPointRestoreMetadata :: GetRecoveryPointRestoreMetadataResponse -> TestTree
responseGetRecoveryPointRestoreMetadata =
  res
    "GetRecoveryPointRestoreMetadataResponse"
    "fixture/GetRecoveryPointRestoreMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecoveryPointRestoreMetadata)

responseGetSupportedResourceTypes :: GetSupportedResourceTypesResponse -> TestTree
responseGetSupportedResourceTypes =
  res
    "GetSupportedResourceTypesResponse"
    "fixture/GetSupportedResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSupportedResourceTypes)

responseListBackupJobs :: ListBackupJobsResponse -> TestTree
responseListBackupJobs =
  res
    "ListBackupJobsResponse"
    "fixture/ListBackupJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupJobs)

responseListBackupPlanTemplates :: ListBackupPlanTemplatesResponse -> TestTree
responseListBackupPlanTemplates =
  res
    "ListBackupPlanTemplatesResponse"
    "fixture/ListBackupPlanTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupPlanTemplates)

responseListBackupPlanVersions :: ListBackupPlanVersionsResponse -> TestTree
responseListBackupPlanVersions =
  res
    "ListBackupPlanVersionsResponse"
    "fixture/ListBackupPlanVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupPlanVersions)

responseListBackupPlans :: ListBackupPlansResponse -> TestTree
responseListBackupPlans =
  res
    "ListBackupPlansResponse"
    "fixture/ListBackupPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupPlans)

responseListBackupSelections :: ListBackupSelectionsResponse -> TestTree
responseListBackupSelections =
  res
    "ListBackupSelectionsResponse"
    "fixture/ListBackupSelectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupSelections)

responseListBackupVaults :: ListBackupVaultsResponse -> TestTree
responseListBackupVaults =
  res
    "ListBackupVaultsResponse"
    "fixture/ListBackupVaultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupVaults)

responseListCopyJobs :: ListCopyJobsResponse -> TestTree
responseListCopyJobs =
  res
    "ListCopyJobsResponse"
    "fixture/ListCopyJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCopyJobs)

responseListFrameworks :: ListFrameworksResponse -> TestTree
responseListFrameworks =
  res
    "ListFrameworksResponse"
    "fixture/ListFrameworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFrameworks)

responseListLegalHolds :: ListLegalHoldsResponse -> TestTree
responseListLegalHolds =
  res
    "ListLegalHoldsResponse"
    "fixture/ListLegalHoldsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLegalHolds)

responseListProtectedResources :: ListProtectedResourcesResponse -> TestTree
responseListProtectedResources =
  res
    "ListProtectedResourcesResponse"
    "fixture/ListProtectedResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProtectedResources)

responseListRecoveryPointsByBackupVault :: ListRecoveryPointsByBackupVaultResponse -> TestTree
responseListRecoveryPointsByBackupVault =
  res
    "ListRecoveryPointsByBackupVaultResponse"
    "fixture/ListRecoveryPointsByBackupVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecoveryPointsByBackupVault)

responseListRecoveryPointsByLegalHold :: ListRecoveryPointsByLegalHoldResponse -> TestTree
responseListRecoveryPointsByLegalHold =
  res
    "ListRecoveryPointsByLegalHoldResponse"
    "fixture/ListRecoveryPointsByLegalHoldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecoveryPointsByLegalHold)

responseListRecoveryPointsByResource :: ListRecoveryPointsByResourceResponse -> TestTree
responseListRecoveryPointsByResource =
  res
    "ListRecoveryPointsByResourceResponse"
    "fixture/ListRecoveryPointsByResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecoveryPointsByResource)

responseListReportJobs :: ListReportJobsResponse -> TestTree
responseListReportJobs =
  res
    "ListReportJobsResponse"
    "fixture/ListReportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReportJobs)

responseListReportPlans :: ListReportPlansResponse -> TestTree
responseListReportPlans =
  res
    "ListReportPlansResponse"
    "fixture/ListReportPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReportPlans)

responseListRestoreJobs :: ListRestoreJobsResponse -> TestTree
responseListRestoreJobs =
  res
    "ListRestoreJobsResponse"
    "fixture/ListRestoreJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRestoreJobs)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responsePutBackupVaultAccessPolicy :: PutBackupVaultAccessPolicyResponse -> TestTree
responsePutBackupVaultAccessPolicy =
  res
    "PutBackupVaultAccessPolicyResponse"
    "fixture/PutBackupVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBackupVaultAccessPolicy)

responsePutBackupVaultLockConfiguration :: PutBackupVaultLockConfigurationResponse -> TestTree
responsePutBackupVaultLockConfiguration =
  res
    "PutBackupVaultLockConfigurationResponse"
    "fixture/PutBackupVaultLockConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBackupVaultLockConfiguration)

responsePutBackupVaultNotifications :: PutBackupVaultNotificationsResponse -> TestTree
responsePutBackupVaultNotifications =
  res
    "PutBackupVaultNotificationsResponse"
    "fixture/PutBackupVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBackupVaultNotifications)

responseStartBackupJob :: StartBackupJobResponse -> TestTree
responseStartBackupJob =
  res
    "StartBackupJobResponse"
    "fixture/StartBackupJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBackupJob)

responseStartCopyJob :: StartCopyJobResponse -> TestTree
responseStartCopyJob =
  res
    "StartCopyJobResponse"
    "fixture/StartCopyJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCopyJob)

responseStartReportJob :: StartReportJobResponse -> TestTree
responseStartReportJob =
  res
    "StartReportJobResponse"
    "fixture/StartReportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReportJob)

responseStartRestoreJob :: StartRestoreJobResponse -> TestTree
responseStartRestoreJob =
  res
    "StartRestoreJobResponse"
    "fixture/StartRestoreJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRestoreJob)

responseStopBackupJob :: StopBackupJobResponse -> TestTree
responseStopBackupJob =
  res
    "StopBackupJobResponse"
    "fixture/StopBackupJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBackupJob)

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

responseUpdateBackupPlan :: UpdateBackupPlanResponse -> TestTree
responseUpdateBackupPlan =
  res
    "UpdateBackupPlanResponse"
    "fixture/UpdateBackupPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackupPlan)

responseUpdateFramework :: UpdateFrameworkResponse -> TestTree
responseUpdateFramework =
  res
    "UpdateFrameworkResponse"
    "fixture/UpdateFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFramework)

responseUpdateGlobalSettings :: UpdateGlobalSettingsResponse -> TestTree
responseUpdateGlobalSettings =
  res
    "UpdateGlobalSettingsResponse"
    "fixture/UpdateGlobalSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalSettings)

responseUpdateRecoveryPointLifecycle :: UpdateRecoveryPointLifecycleResponse -> TestTree
responseUpdateRecoveryPointLifecycle =
  res
    "UpdateRecoveryPointLifecycleResponse"
    "fixture/UpdateRecoveryPointLifecycleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecoveryPointLifecycle)

responseUpdateRegionSettings :: UpdateRegionSettingsResponse -> TestTree
responseUpdateRegionSettings =
  res
    "UpdateRegionSettingsResponse"
    "fixture/UpdateRegionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRegionSettings)

responseUpdateReportPlan :: UpdateReportPlanResponse -> TestTree
responseUpdateReportPlan =
  res
    "UpdateReportPlanResponse"
    "fixture/UpdateReportPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReportPlan)
