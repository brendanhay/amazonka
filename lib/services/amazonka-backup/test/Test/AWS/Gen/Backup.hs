{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Backup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Backup where

import Amazonka.Backup
import qualified Data.Proxy as Proxy
import Test.AWS.Backup.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateBackupPlan $
--             newUpdateBackupPlan
--
--         , requestDeleteBackupPlan $
--             newDeleteBackupPlan
--
--         , requestDescribeBackupJob $
--             newDescribeBackupJob
--
--         , requestListBackupPlanTemplates $
--             newListBackupPlanTemplates
--
--         , requestDeleteReportPlan $
--             newDeleteReportPlan
--
--         , requestUpdateReportPlan $
--             newUpdateReportPlan
--
--         , requestDescribeReportJob $
--             newDescribeReportJob
--
--         , requestUpdateRegionSettings $
--             newUpdateRegionSettings
--
--         , requestUpdateGlobalSettings $
--             newUpdateGlobalSettings
--
--         , requestDeleteBackupSelection $
--             newDeleteBackupSelection
--
--         , requestDescribeCopyJob $
--             newDescribeCopyJob
--
--         , requestDescribeRecoveryPoint $
--             newDescribeRecoveryPoint
--
--         , requestDescribeRestoreJob $
--             newDescribeRestoreJob
--
--         , requestStartCopyJob $
--             newStartCopyJob
--
--         , requestGetBackupPlanFromTemplate $
--             newGetBackupPlanFromTemplate
--
--         , requestDisassociateRecoveryPoint $
--             newDisassociateRecoveryPoint
--
--         , requestDeleteBackupVault $
--             newDeleteBackupVault
--
--         , requestDeleteFramework $
--             newDeleteFramework
--
--         , requestUpdateFramework $
--             newUpdateFramework
--
--         , requestListReportJobs $
--             newListReportJobs
--
--         , requestListBackupJobs $
--             newListBackupJobs
--
--         , requestDescribeReportPlan $
--             newDescribeReportPlan
--
--         , requestDescribeRegionSettings $
--             newDescribeRegionSettings
--
--         , requestGetBackupPlan $
--             newGetBackupPlan
--
--         , requestDescribeGlobalSettings $
--             newDescribeGlobalSettings
--
--         , requestListBackupPlanVersions $
--             newListBackupPlanVersions
--
--         , requestListRestoreJobs $
--             newListRestoreJobs
--
--         , requestCreateReportPlan $
--             newCreateReportPlan
--
--         , requestExportBackupPlanTemplate $
--             newExportBackupPlanTemplate
--
--         , requestStartBackupJob $
--             newStartBackupJob
--
--         , requestDescribeFramework $
--             newDescribeFramework
--
--         , requestCreateBackupPlan $
--             newCreateBackupPlan
--
--         , requestListProtectedResources $
--             newListProtectedResources
--
--         , requestStartReportJob $
--             newStartReportJob
--
--         , requestDescribeBackupVault $
--             newDescribeBackupVault
--
--         , requestGetBackupVaultNotifications $
--             newGetBackupVaultNotifications
--
--         , requestListReportPlans $
--             newListReportPlans
--
--         , requestGetRecoveryPointRestoreMetadata $
--             newGetRecoveryPointRestoreMetadata
--
--         , requestListBackupPlans $
--             newListBackupPlans
--
--         , requestStartRestoreJob $
--             newStartRestoreJob
--
--         , requestListBackupSelections $
--             newListBackupSelections
--
--         , requestListRecoveryPointsByResource $
--             newListRecoveryPointsByResource
--
--         , requestCreateBackupSelection $
--             newCreateBackupSelection
--
--         , requestListFrameworks $
--             newListFrameworks
--
--         , requestDescribeProtectedResource $
--             newDescribeProtectedResource
--
--         , requestGetBackupPlanFromJSON $
--             newGetBackupPlanFromJSON
--
--         , requestListBackupVaults $
--             newListBackupVaults
--
--         , requestGetBackupSelection $
--             newGetBackupSelection
--
--         , requestCreateBackupVault $
--             newCreateBackupVault
--
--         , requestUpdateRecoveryPointLifecycle $
--             newUpdateRecoveryPointLifecycle
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateFramework $
--             newCreateFramework
--
--         , requestPutBackupVaultNotifications $
--             newPutBackupVaultNotifications
--
--         , requestDeleteBackupVaultNotifications $
--             newDeleteBackupVaultNotifications
--
--         , requestListTags $
--             newListTags
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListCopyJobs $
--             newListCopyJobs
--
--         , requestDeleteBackupVaultLockConfiguration $
--             newDeleteBackupVaultLockConfiguration
--
--         , requestGetBackupVaultAccessPolicy $
--             newGetBackupVaultAccessPolicy
--
--         , requestDeleteRecoveryPoint $
--             newDeleteRecoveryPoint
--
--         , requestPutBackupVaultLockConfiguration $
--             newPutBackupVaultLockConfiguration
--
--         , requestGetSupportedResourceTypes $
--             newGetSupportedResourceTypes
--
--         , requestStopBackupJob $
--             newStopBackupJob
--
--         , requestListRecoveryPointsByBackupVault $
--             newListRecoveryPointsByBackupVault
--
--         , requestPutBackupVaultAccessPolicy $
--             newPutBackupVaultAccessPolicy
--
--         , requestDeleteBackupVaultAccessPolicy $
--             newDeleteBackupVaultAccessPolicy
--
--           ]

--     , testGroup "response"
--         [ responseUpdateBackupPlan $
--             newUpdateBackupPlanResponse
--
--         , responseDeleteBackupPlan $
--             newDeleteBackupPlanResponse
--
--         , responseDescribeBackupJob $
--             newDescribeBackupJobResponse
--
--         , responseListBackupPlanTemplates $
--             newListBackupPlanTemplatesResponse
--
--         , responseDeleteReportPlan $
--             newDeleteReportPlanResponse
--
--         , responseUpdateReportPlan $
--             newUpdateReportPlanResponse
--
--         , responseDescribeReportJob $
--             newDescribeReportJobResponse
--
--         , responseUpdateRegionSettings $
--             newUpdateRegionSettingsResponse
--
--         , responseUpdateGlobalSettings $
--             newUpdateGlobalSettingsResponse
--
--         , responseDeleteBackupSelection $
--             newDeleteBackupSelectionResponse
--
--         , responseDescribeCopyJob $
--             newDescribeCopyJobResponse
--
--         , responseDescribeRecoveryPoint $
--             newDescribeRecoveryPointResponse
--
--         , responseDescribeRestoreJob $
--             newDescribeRestoreJobResponse
--
--         , responseStartCopyJob $
--             newStartCopyJobResponse
--
--         , responseGetBackupPlanFromTemplate $
--             newGetBackupPlanFromTemplateResponse
--
--         , responseDisassociateRecoveryPoint $
--             newDisassociateRecoveryPointResponse
--
--         , responseDeleteBackupVault $
--             newDeleteBackupVaultResponse
--
--         , responseDeleteFramework $
--             newDeleteFrameworkResponse
--
--         , responseUpdateFramework $
--             newUpdateFrameworkResponse
--
--         , responseListReportJobs $
--             newListReportJobsResponse
--
--         , responseListBackupJobs $
--             newListBackupJobsResponse
--
--         , responseDescribeReportPlan $
--             newDescribeReportPlanResponse
--
--         , responseDescribeRegionSettings $
--             newDescribeRegionSettingsResponse
--
--         , responseGetBackupPlan $
--             newGetBackupPlanResponse
--
--         , responseDescribeGlobalSettings $
--             newDescribeGlobalSettingsResponse
--
--         , responseListBackupPlanVersions $
--             newListBackupPlanVersionsResponse
--
--         , responseListRestoreJobs $
--             newListRestoreJobsResponse
--
--         , responseCreateReportPlan $
--             newCreateReportPlanResponse
--
--         , responseExportBackupPlanTemplate $
--             newExportBackupPlanTemplateResponse
--
--         , responseStartBackupJob $
--             newStartBackupJobResponse
--
--         , responseDescribeFramework $
--             newDescribeFrameworkResponse
--
--         , responseCreateBackupPlan $
--             newCreateBackupPlanResponse
--
--         , responseListProtectedResources $
--             newListProtectedResourcesResponse
--
--         , responseStartReportJob $
--             newStartReportJobResponse
--
--         , responseDescribeBackupVault $
--             newDescribeBackupVaultResponse
--
--         , responseGetBackupVaultNotifications $
--             newGetBackupVaultNotificationsResponse
--
--         , responseListReportPlans $
--             newListReportPlansResponse
--
--         , responseGetRecoveryPointRestoreMetadata $
--             newGetRecoveryPointRestoreMetadataResponse
--
--         , responseListBackupPlans $
--             newListBackupPlansResponse
--
--         , responseStartRestoreJob $
--             newStartRestoreJobResponse
--
--         , responseListBackupSelections $
--             newListBackupSelectionsResponse
--
--         , responseListRecoveryPointsByResource $
--             newListRecoveryPointsByResourceResponse
--
--         , responseCreateBackupSelection $
--             newCreateBackupSelectionResponse
--
--         , responseListFrameworks $
--             newListFrameworksResponse
--
--         , responseDescribeProtectedResource $
--             newDescribeProtectedResourceResponse
--
--         , responseGetBackupPlanFromJSON $
--             newGetBackupPlanFromJSONResponse
--
--         , responseListBackupVaults $
--             newListBackupVaultsResponse
--
--         , responseGetBackupSelection $
--             newGetBackupSelectionResponse
--
--         , responseCreateBackupVault $
--             newCreateBackupVaultResponse
--
--         , responseUpdateRecoveryPointLifecycle $
--             newUpdateRecoveryPointLifecycleResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateFramework $
--             newCreateFrameworkResponse
--
--         , responsePutBackupVaultNotifications $
--             newPutBackupVaultNotificationsResponse
--
--         , responseDeleteBackupVaultNotifications $
--             newDeleteBackupVaultNotificationsResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListCopyJobs $
--             newListCopyJobsResponse
--
--         , responseDeleteBackupVaultLockConfiguration $
--             newDeleteBackupVaultLockConfigurationResponse
--
--         , responseGetBackupVaultAccessPolicy $
--             newGetBackupVaultAccessPolicyResponse
--
--         , responseDeleteRecoveryPoint $
--             newDeleteRecoveryPointResponse
--
--         , responsePutBackupVaultLockConfiguration $
--             newPutBackupVaultLockConfigurationResponse
--
--         , responseGetSupportedResourceTypes $
--             newGetSupportedResourceTypesResponse
--
--         , responseStopBackupJob $
--             newStopBackupJobResponse
--
--         , responseListRecoveryPointsByBackupVault $
--             newListRecoveryPointsByBackupVaultResponse
--
--         , responsePutBackupVaultAccessPolicy $
--             newPutBackupVaultAccessPolicyResponse
--
--         , responseDeleteBackupVaultAccessPolicy $
--             newDeleteBackupVaultAccessPolicyResponse
--
--           ]
--     ]

-- Requests

requestUpdateBackupPlan :: UpdateBackupPlan -> TestTree
requestUpdateBackupPlan =
  req
    "UpdateBackupPlan"
    "fixture/UpdateBackupPlan.yaml"

requestDeleteBackupPlan :: DeleteBackupPlan -> TestTree
requestDeleteBackupPlan =
  req
    "DeleteBackupPlan"
    "fixture/DeleteBackupPlan.yaml"

requestDescribeBackupJob :: DescribeBackupJob -> TestTree
requestDescribeBackupJob =
  req
    "DescribeBackupJob"
    "fixture/DescribeBackupJob.yaml"

requestListBackupPlanTemplates :: ListBackupPlanTemplates -> TestTree
requestListBackupPlanTemplates =
  req
    "ListBackupPlanTemplates"
    "fixture/ListBackupPlanTemplates.yaml"

requestDeleteReportPlan :: DeleteReportPlan -> TestTree
requestDeleteReportPlan =
  req
    "DeleteReportPlan"
    "fixture/DeleteReportPlan.yaml"

requestUpdateReportPlan :: UpdateReportPlan -> TestTree
requestUpdateReportPlan =
  req
    "UpdateReportPlan"
    "fixture/UpdateReportPlan.yaml"

requestDescribeReportJob :: DescribeReportJob -> TestTree
requestDescribeReportJob =
  req
    "DescribeReportJob"
    "fixture/DescribeReportJob.yaml"

requestUpdateRegionSettings :: UpdateRegionSettings -> TestTree
requestUpdateRegionSettings =
  req
    "UpdateRegionSettings"
    "fixture/UpdateRegionSettings.yaml"

requestUpdateGlobalSettings :: UpdateGlobalSettings -> TestTree
requestUpdateGlobalSettings =
  req
    "UpdateGlobalSettings"
    "fixture/UpdateGlobalSettings.yaml"

requestDeleteBackupSelection :: DeleteBackupSelection -> TestTree
requestDeleteBackupSelection =
  req
    "DeleteBackupSelection"
    "fixture/DeleteBackupSelection.yaml"

requestDescribeCopyJob :: DescribeCopyJob -> TestTree
requestDescribeCopyJob =
  req
    "DescribeCopyJob"
    "fixture/DescribeCopyJob.yaml"

requestDescribeRecoveryPoint :: DescribeRecoveryPoint -> TestTree
requestDescribeRecoveryPoint =
  req
    "DescribeRecoveryPoint"
    "fixture/DescribeRecoveryPoint.yaml"

requestDescribeRestoreJob :: DescribeRestoreJob -> TestTree
requestDescribeRestoreJob =
  req
    "DescribeRestoreJob"
    "fixture/DescribeRestoreJob.yaml"

requestStartCopyJob :: StartCopyJob -> TestTree
requestStartCopyJob =
  req
    "StartCopyJob"
    "fixture/StartCopyJob.yaml"

requestGetBackupPlanFromTemplate :: GetBackupPlanFromTemplate -> TestTree
requestGetBackupPlanFromTemplate =
  req
    "GetBackupPlanFromTemplate"
    "fixture/GetBackupPlanFromTemplate.yaml"

requestDisassociateRecoveryPoint :: DisassociateRecoveryPoint -> TestTree
requestDisassociateRecoveryPoint =
  req
    "DisassociateRecoveryPoint"
    "fixture/DisassociateRecoveryPoint.yaml"

requestDeleteBackupVault :: DeleteBackupVault -> TestTree
requestDeleteBackupVault =
  req
    "DeleteBackupVault"
    "fixture/DeleteBackupVault.yaml"

requestDeleteFramework :: DeleteFramework -> TestTree
requestDeleteFramework =
  req
    "DeleteFramework"
    "fixture/DeleteFramework.yaml"

requestUpdateFramework :: UpdateFramework -> TestTree
requestUpdateFramework =
  req
    "UpdateFramework"
    "fixture/UpdateFramework.yaml"

requestListReportJobs :: ListReportJobs -> TestTree
requestListReportJobs =
  req
    "ListReportJobs"
    "fixture/ListReportJobs.yaml"

requestListBackupJobs :: ListBackupJobs -> TestTree
requestListBackupJobs =
  req
    "ListBackupJobs"
    "fixture/ListBackupJobs.yaml"

requestDescribeReportPlan :: DescribeReportPlan -> TestTree
requestDescribeReportPlan =
  req
    "DescribeReportPlan"
    "fixture/DescribeReportPlan.yaml"

requestDescribeRegionSettings :: DescribeRegionSettings -> TestTree
requestDescribeRegionSettings =
  req
    "DescribeRegionSettings"
    "fixture/DescribeRegionSettings.yaml"

requestGetBackupPlan :: GetBackupPlan -> TestTree
requestGetBackupPlan =
  req
    "GetBackupPlan"
    "fixture/GetBackupPlan.yaml"

requestDescribeGlobalSettings :: DescribeGlobalSettings -> TestTree
requestDescribeGlobalSettings =
  req
    "DescribeGlobalSettings"
    "fixture/DescribeGlobalSettings.yaml"

requestListBackupPlanVersions :: ListBackupPlanVersions -> TestTree
requestListBackupPlanVersions =
  req
    "ListBackupPlanVersions"
    "fixture/ListBackupPlanVersions.yaml"

requestListRestoreJobs :: ListRestoreJobs -> TestTree
requestListRestoreJobs =
  req
    "ListRestoreJobs"
    "fixture/ListRestoreJobs.yaml"

requestCreateReportPlan :: CreateReportPlan -> TestTree
requestCreateReportPlan =
  req
    "CreateReportPlan"
    "fixture/CreateReportPlan.yaml"

requestExportBackupPlanTemplate :: ExportBackupPlanTemplate -> TestTree
requestExportBackupPlanTemplate =
  req
    "ExportBackupPlanTemplate"
    "fixture/ExportBackupPlanTemplate.yaml"

requestStartBackupJob :: StartBackupJob -> TestTree
requestStartBackupJob =
  req
    "StartBackupJob"
    "fixture/StartBackupJob.yaml"

requestDescribeFramework :: DescribeFramework -> TestTree
requestDescribeFramework =
  req
    "DescribeFramework"
    "fixture/DescribeFramework.yaml"

requestCreateBackupPlan :: CreateBackupPlan -> TestTree
requestCreateBackupPlan =
  req
    "CreateBackupPlan"
    "fixture/CreateBackupPlan.yaml"

requestListProtectedResources :: ListProtectedResources -> TestTree
requestListProtectedResources =
  req
    "ListProtectedResources"
    "fixture/ListProtectedResources.yaml"

requestStartReportJob :: StartReportJob -> TestTree
requestStartReportJob =
  req
    "StartReportJob"
    "fixture/StartReportJob.yaml"

requestDescribeBackupVault :: DescribeBackupVault -> TestTree
requestDescribeBackupVault =
  req
    "DescribeBackupVault"
    "fixture/DescribeBackupVault.yaml"

requestGetBackupVaultNotifications :: GetBackupVaultNotifications -> TestTree
requestGetBackupVaultNotifications =
  req
    "GetBackupVaultNotifications"
    "fixture/GetBackupVaultNotifications.yaml"

requestListReportPlans :: ListReportPlans -> TestTree
requestListReportPlans =
  req
    "ListReportPlans"
    "fixture/ListReportPlans.yaml"

requestGetRecoveryPointRestoreMetadata :: GetRecoveryPointRestoreMetadata -> TestTree
requestGetRecoveryPointRestoreMetadata =
  req
    "GetRecoveryPointRestoreMetadata"
    "fixture/GetRecoveryPointRestoreMetadata.yaml"

requestListBackupPlans :: ListBackupPlans -> TestTree
requestListBackupPlans =
  req
    "ListBackupPlans"
    "fixture/ListBackupPlans.yaml"

requestStartRestoreJob :: StartRestoreJob -> TestTree
requestStartRestoreJob =
  req
    "StartRestoreJob"
    "fixture/StartRestoreJob.yaml"

requestListBackupSelections :: ListBackupSelections -> TestTree
requestListBackupSelections =
  req
    "ListBackupSelections"
    "fixture/ListBackupSelections.yaml"

requestListRecoveryPointsByResource :: ListRecoveryPointsByResource -> TestTree
requestListRecoveryPointsByResource =
  req
    "ListRecoveryPointsByResource"
    "fixture/ListRecoveryPointsByResource.yaml"

requestCreateBackupSelection :: CreateBackupSelection -> TestTree
requestCreateBackupSelection =
  req
    "CreateBackupSelection"
    "fixture/CreateBackupSelection.yaml"

requestListFrameworks :: ListFrameworks -> TestTree
requestListFrameworks =
  req
    "ListFrameworks"
    "fixture/ListFrameworks.yaml"

requestDescribeProtectedResource :: DescribeProtectedResource -> TestTree
requestDescribeProtectedResource =
  req
    "DescribeProtectedResource"
    "fixture/DescribeProtectedResource.yaml"

requestGetBackupPlanFromJSON :: GetBackupPlanFromJSON -> TestTree
requestGetBackupPlanFromJSON =
  req
    "GetBackupPlanFromJSON"
    "fixture/GetBackupPlanFromJSON.yaml"

requestListBackupVaults :: ListBackupVaults -> TestTree
requestListBackupVaults =
  req
    "ListBackupVaults"
    "fixture/ListBackupVaults.yaml"

requestGetBackupSelection :: GetBackupSelection -> TestTree
requestGetBackupSelection =
  req
    "GetBackupSelection"
    "fixture/GetBackupSelection.yaml"

requestCreateBackupVault :: CreateBackupVault -> TestTree
requestCreateBackupVault =
  req
    "CreateBackupVault"
    "fixture/CreateBackupVault.yaml"

requestUpdateRecoveryPointLifecycle :: UpdateRecoveryPointLifecycle -> TestTree
requestUpdateRecoveryPointLifecycle =
  req
    "UpdateRecoveryPointLifecycle"
    "fixture/UpdateRecoveryPointLifecycle.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateFramework :: CreateFramework -> TestTree
requestCreateFramework =
  req
    "CreateFramework"
    "fixture/CreateFramework.yaml"

requestPutBackupVaultNotifications :: PutBackupVaultNotifications -> TestTree
requestPutBackupVaultNotifications =
  req
    "PutBackupVaultNotifications"
    "fixture/PutBackupVaultNotifications.yaml"

requestDeleteBackupVaultNotifications :: DeleteBackupVaultNotifications -> TestTree
requestDeleteBackupVaultNotifications =
  req
    "DeleteBackupVaultNotifications"
    "fixture/DeleteBackupVaultNotifications.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListCopyJobs :: ListCopyJobs -> TestTree
requestListCopyJobs =
  req
    "ListCopyJobs"
    "fixture/ListCopyJobs.yaml"

requestDeleteBackupVaultLockConfiguration :: DeleteBackupVaultLockConfiguration -> TestTree
requestDeleteBackupVaultLockConfiguration =
  req
    "DeleteBackupVaultLockConfiguration"
    "fixture/DeleteBackupVaultLockConfiguration.yaml"

requestGetBackupVaultAccessPolicy :: GetBackupVaultAccessPolicy -> TestTree
requestGetBackupVaultAccessPolicy =
  req
    "GetBackupVaultAccessPolicy"
    "fixture/GetBackupVaultAccessPolicy.yaml"

requestDeleteRecoveryPoint :: DeleteRecoveryPoint -> TestTree
requestDeleteRecoveryPoint =
  req
    "DeleteRecoveryPoint"
    "fixture/DeleteRecoveryPoint.yaml"

requestPutBackupVaultLockConfiguration :: PutBackupVaultLockConfiguration -> TestTree
requestPutBackupVaultLockConfiguration =
  req
    "PutBackupVaultLockConfiguration"
    "fixture/PutBackupVaultLockConfiguration.yaml"

requestGetSupportedResourceTypes :: GetSupportedResourceTypes -> TestTree
requestGetSupportedResourceTypes =
  req
    "GetSupportedResourceTypes"
    "fixture/GetSupportedResourceTypes.yaml"

requestStopBackupJob :: StopBackupJob -> TestTree
requestStopBackupJob =
  req
    "StopBackupJob"
    "fixture/StopBackupJob.yaml"

requestListRecoveryPointsByBackupVault :: ListRecoveryPointsByBackupVault -> TestTree
requestListRecoveryPointsByBackupVault =
  req
    "ListRecoveryPointsByBackupVault"
    "fixture/ListRecoveryPointsByBackupVault.yaml"

requestPutBackupVaultAccessPolicy :: PutBackupVaultAccessPolicy -> TestTree
requestPutBackupVaultAccessPolicy =
  req
    "PutBackupVaultAccessPolicy"
    "fixture/PutBackupVaultAccessPolicy.yaml"

requestDeleteBackupVaultAccessPolicy :: DeleteBackupVaultAccessPolicy -> TestTree
requestDeleteBackupVaultAccessPolicy =
  req
    "DeleteBackupVaultAccessPolicy"
    "fixture/DeleteBackupVaultAccessPolicy.yaml"

-- Responses

responseUpdateBackupPlan :: UpdateBackupPlanResponse -> TestTree
responseUpdateBackupPlan =
  res
    "UpdateBackupPlanResponse"
    "fixture/UpdateBackupPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackupPlan)

responseDeleteBackupPlan :: DeleteBackupPlanResponse -> TestTree
responseDeleteBackupPlan =
  res
    "DeleteBackupPlanResponse"
    "fixture/DeleteBackupPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupPlan)

responseDescribeBackupJob :: DescribeBackupJobResponse -> TestTree
responseDescribeBackupJob =
  res
    "DescribeBackupJobResponse"
    "fixture/DescribeBackupJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackupJob)

responseListBackupPlanTemplates :: ListBackupPlanTemplatesResponse -> TestTree
responseListBackupPlanTemplates =
  res
    "ListBackupPlanTemplatesResponse"
    "fixture/ListBackupPlanTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupPlanTemplates)

responseDeleteReportPlan :: DeleteReportPlanResponse -> TestTree
responseDeleteReportPlan =
  res
    "DeleteReportPlanResponse"
    "fixture/DeleteReportPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReportPlan)

responseUpdateReportPlan :: UpdateReportPlanResponse -> TestTree
responseUpdateReportPlan =
  res
    "UpdateReportPlanResponse"
    "fixture/UpdateReportPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReportPlan)

responseDescribeReportJob :: DescribeReportJobResponse -> TestTree
responseDescribeReportJob =
  res
    "DescribeReportJobResponse"
    "fixture/DescribeReportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReportJob)

responseUpdateRegionSettings :: UpdateRegionSettingsResponse -> TestTree
responseUpdateRegionSettings =
  res
    "UpdateRegionSettingsResponse"
    "fixture/UpdateRegionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRegionSettings)

responseUpdateGlobalSettings :: UpdateGlobalSettingsResponse -> TestTree
responseUpdateGlobalSettings =
  res
    "UpdateGlobalSettingsResponse"
    "fixture/UpdateGlobalSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalSettings)

responseDeleteBackupSelection :: DeleteBackupSelectionResponse -> TestTree
responseDeleteBackupSelection =
  res
    "DeleteBackupSelectionResponse"
    "fixture/DeleteBackupSelectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupSelection)

responseDescribeCopyJob :: DescribeCopyJobResponse -> TestTree
responseDescribeCopyJob =
  res
    "DescribeCopyJobResponse"
    "fixture/DescribeCopyJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCopyJob)

responseDescribeRecoveryPoint :: DescribeRecoveryPointResponse -> TestTree
responseDescribeRecoveryPoint =
  res
    "DescribeRecoveryPointResponse"
    "fixture/DescribeRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecoveryPoint)

responseDescribeRestoreJob :: DescribeRestoreJobResponse -> TestTree
responseDescribeRestoreJob =
  res
    "DescribeRestoreJobResponse"
    "fixture/DescribeRestoreJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRestoreJob)

responseStartCopyJob :: StartCopyJobResponse -> TestTree
responseStartCopyJob =
  res
    "StartCopyJobResponse"
    "fixture/StartCopyJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCopyJob)

responseGetBackupPlanFromTemplate :: GetBackupPlanFromTemplateResponse -> TestTree
responseGetBackupPlanFromTemplate =
  res
    "GetBackupPlanFromTemplateResponse"
    "fixture/GetBackupPlanFromTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupPlanFromTemplate)

responseDisassociateRecoveryPoint :: DisassociateRecoveryPointResponse -> TestTree
responseDisassociateRecoveryPoint =
  res
    "DisassociateRecoveryPointResponse"
    "fixture/DisassociateRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRecoveryPoint)

responseDeleteBackupVault :: DeleteBackupVaultResponse -> TestTree
responseDeleteBackupVault =
  res
    "DeleteBackupVaultResponse"
    "fixture/DeleteBackupVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupVault)

responseDeleteFramework :: DeleteFrameworkResponse -> TestTree
responseDeleteFramework =
  res
    "DeleteFrameworkResponse"
    "fixture/DeleteFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFramework)

responseUpdateFramework :: UpdateFrameworkResponse -> TestTree
responseUpdateFramework =
  res
    "UpdateFrameworkResponse"
    "fixture/UpdateFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFramework)

responseListReportJobs :: ListReportJobsResponse -> TestTree
responseListReportJobs =
  res
    "ListReportJobsResponse"
    "fixture/ListReportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReportJobs)

responseListBackupJobs :: ListBackupJobsResponse -> TestTree
responseListBackupJobs =
  res
    "ListBackupJobsResponse"
    "fixture/ListBackupJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupJobs)

responseDescribeReportPlan :: DescribeReportPlanResponse -> TestTree
responseDescribeReportPlan =
  res
    "DescribeReportPlanResponse"
    "fixture/DescribeReportPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReportPlan)

responseDescribeRegionSettings :: DescribeRegionSettingsResponse -> TestTree
responseDescribeRegionSettings =
  res
    "DescribeRegionSettingsResponse"
    "fixture/DescribeRegionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegionSettings)

responseGetBackupPlan :: GetBackupPlanResponse -> TestTree
responseGetBackupPlan =
  res
    "GetBackupPlanResponse"
    "fixture/GetBackupPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupPlan)

responseDescribeGlobalSettings :: DescribeGlobalSettingsResponse -> TestTree
responseDescribeGlobalSettings =
  res
    "DescribeGlobalSettingsResponse"
    "fixture/DescribeGlobalSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalSettings)

responseListBackupPlanVersions :: ListBackupPlanVersionsResponse -> TestTree
responseListBackupPlanVersions =
  res
    "ListBackupPlanVersionsResponse"
    "fixture/ListBackupPlanVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupPlanVersions)

responseListRestoreJobs :: ListRestoreJobsResponse -> TestTree
responseListRestoreJobs =
  res
    "ListRestoreJobsResponse"
    "fixture/ListRestoreJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRestoreJobs)

responseCreateReportPlan :: CreateReportPlanResponse -> TestTree
responseCreateReportPlan =
  res
    "CreateReportPlanResponse"
    "fixture/CreateReportPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReportPlan)

responseExportBackupPlanTemplate :: ExportBackupPlanTemplateResponse -> TestTree
responseExportBackupPlanTemplate =
  res
    "ExportBackupPlanTemplateResponse"
    "fixture/ExportBackupPlanTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportBackupPlanTemplate)

responseStartBackupJob :: StartBackupJobResponse -> TestTree
responseStartBackupJob =
  res
    "StartBackupJobResponse"
    "fixture/StartBackupJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBackupJob)

responseDescribeFramework :: DescribeFrameworkResponse -> TestTree
responseDescribeFramework =
  res
    "DescribeFrameworkResponse"
    "fixture/DescribeFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFramework)

responseCreateBackupPlan :: CreateBackupPlanResponse -> TestTree
responseCreateBackupPlan =
  res
    "CreateBackupPlanResponse"
    "fixture/CreateBackupPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackupPlan)

responseListProtectedResources :: ListProtectedResourcesResponse -> TestTree
responseListProtectedResources =
  res
    "ListProtectedResourcesResponse"
    "fixture/ListProtectedResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProtectedResources)

responseStartReportJob :: StartReportJobResponse -> TestTree
responseStartReportJob =
  res
    "StartReportJobResponse"
    "fixture/StartReportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReportJob)

responseDescribeBackupVault :: DescribeBackupVaultResponse -> TestTree
responseDescribeBackupVault =
  res
    "DescribeBackupVaultResponse"
    "fixture/DescribeBackupVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackupVault)

responseGetBackupVaultNotifications :: GetBackupVaultNotificationsResponse -> TestTree
responseGetBackupVaultNotifications =
  res
    "GetBackupVaultNotificationsResponse"
    "fixture/GetBackupVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupVaultNotifications)

responseListReportPlans :: ListReportPlansResponse -> TestTree
responseListReportPlans =
  res
    "ListReportPlansResponse"
    "fixture/ListReportPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReportPlans)

responseGetRecoveryPointRestoreMetadata :: GetRecoveryPointRestoreMetadataResponse -> TestTree
responseGetRecoveryPointRestoreMetadata =
  res
    "GetRecoveryPointRestoreMetadataResponse"
    "fixture/GetRecoveryPointRestoreMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecoveryPointRestoreMetadata)

responseListBackupPlans :: ListBackupPlansResponse -> TestTree
responseListBackupPlans =
  res
    "ListBackupPlansResponse"
    "fixture/ListBackupPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupPlans)

responseStartRestoreJob :: StartRestoreJobResponse -> TestTree
responseStartRestoreJob =
  res
    "StartRestoreJobResponse"
    "fixture/StartRestoreJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRestoreJob)

responseListBackupSelections :: ListBackupSelectionsResponse -> TestTree
responseListBackupSelections =
  res
    "ListBackupSelectionsResponse"
    "fixture/ListBackupSelectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupSelections)

responseListRecoveryPointsByResource :: ListRecoveryPointsByResourceResponse -> TestTree
responseListRecoveryPointsByResource =
  res
    "ListRecoveryPointsByResourceResponse"
    "fixture/ListRecoveryPointsByResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecoveryPointsByResource)

responseCreateBackupSelection :: CreateBackupSelectionResponse -> TestTree
responseCreateBackupSelection =
  res
    "CreateBackupSelectionResponse"
    "fixture/CreateBackupSelectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackupSelection)

responseListFrameworks :: ListFrameworksResponse -> TestTree
responseListFrameworks =
  res
    "ListFrameworksResponse"
    "fixture/ListFrameworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFrameworks)

responseDescribeProtectedResource :: DescribeProtectedResourceResponse -> TestTree
responseDescribeProtectedResource =
  res
    "DescribeProtectedResourceResponse"
    "fixture/DescribeProtectedResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProtectedResource)

responseGetBackupPlanFromJSON :: GetBackupPlanFromJSONResponse -> TestTree
responseGetBackupPlanFromJSON =
  res
    "GetBackupPlanFromJSONResponse"
    "fixture/GetBackupPlanFromJSONResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupPlanFromJSON)

responseListBackupVaults :: ListBackupVaultsResponse -> TestTree
responseListBackupVaults =
  res
    "ListBackupVaultsResponse"
    "fixture/ListBackupVaultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackupVaults)

responseGetBackupSelection :: GetBackupSelectionResponse -> TestTree
responseGetBackupSelection =
  res
    "GetBackupSelectionResponse"
    "fixture/GetBackupSelectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupSelection)

responseCreateBackupVault :: CreateBackupVaultResponse -> TestTree
responseCreateBackupVault =
  res
    "CreateBackupVaultResponse"
    "fixture/CreateBackupVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackupVault)

responseUpdateRecoveryPointLifecycle :: UpdateRecoveryPointLifecycleResponse -> TestTree
responseUpdateRecoveryPointLifecycle =
  res
    "UpdateRecoveryPointLifecycleResponse"
    "fixture/UpdateRecoveryPointLifecycleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecoveryPointLifecycle)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateFramework :: CreateFrameworkResponse -> TestTree
responseCreateFramework =
  res
    "CreateFrameworkResponse"
    "fixture/CreateFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFramework)

responsePutBackupVaultNotifications :: PutBackupVaultNotificationsResponse -> TestTree
responsePutBackupVaultNotifications =
  res
    "PutBackupVaultNotificationsResponse"
    "fixture/PutBackupVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBackupVaultNotifications)

responseDeleteBackupVaultNotifications :: DeleteBackupVaultNotificationsResponse -> TestTree
responseDeleteBackupVaultNotifications =
  res
    "DeleteBackupVaultNotificationsResponse"
    "fixture/DeleteBackupVaultNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupVaultNotifications)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListCopyJobs :: ListCopyJobsResponse -> TestTree
responseListCopyJobs =
  res
    "ListCopyJobsResponse"
    "fixture/ListCopyJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCopyJobs)

responseDeleteBackupVaultLockConfiguration :: DeleteBackupVaultLockConfigurationResponse -> TestTree
responseDeleteBackupVaultLockConfiguration =
  res
    "DeleteBackupVaultLockConfigurationResponse"
    "fixture/DeleteBackupVaultLockConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupVaultLockConfiguration)

responseGetBackupVaultAccessPolicy :: GetBackupVaultAccessPolicyResponse -> TestTree
responseGetBackupVaultAccessPolicy =
  res
    "GetBackupVaultAccessPolicyResponse"
    "fixture/GetBackupVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackupVaultAccessPolicy)

responseDeleteRecoveryPoint :: DeleteRecoveryPointResponse -> TestTree
responseDeleteRecoveryPoint =
  res
    "DeleteRecoveryPointResponse"
    "fixture/DeleteRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecoveryPoint)

responsePutBackupVaultLockConfiguration :: PutBackupVaultLockConfigurationResponse -> TestTree
responsePutBackupVaultLockConfiguration =
  res
    "PutBackupVaultLockConfigurationResponse"
    "fixture/PutBackupVaultLockConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBackupVaultLockConfiguration)

responseGetSupportedResourceTypes :: GetSupportedResourceTypesResponse -> TestTree
responseGetSupportedResourceTypes =
  res
    "GetSupportedResourceTypesResponse"
    "fixture/GetSupportedResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSupportedResourceTypes)

responseStopBackupJob :: StopBackupJobResponse -> TestTree
responseStopBackupJob =
  res
    "StopBackupJobResponse"
    "fixture/StopBackupJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBackupJob)

responseListRecoveryPointsByBackupVault :: ListRecoveryPointsByBackupVaultResponse -> TestTree
responseListRecoveryPointsByBackupVault =
  res
    "ListRecoveryPointsByBackupVaultResponse"
    "fixture/ListRecoveryPointsByBackupVaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecoveryPointsByBackupVault)

responsePutBackupVaultAccessPolicy :: PutBackupVaultAccessPolicyResponse -> TestTree
responsePutBackupVaultAccessPolicy =
  res
    "PutBackupVaultAccessPolicyResponse"
    "fixture/PutBackupVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBackupVaultAccessPolicy)

responseDeleteBackupVaultAccessPolicy :: DeleteBackupVaultAccessPolicyResponse -> TestTree
responseDeleteBackupVaultAccessPolicy =
  res
    "DeleteBackupVaultAccessPolicyResponse"
    "fixture/DeleteBackupVaultAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackupVaultAccessPolicy)
