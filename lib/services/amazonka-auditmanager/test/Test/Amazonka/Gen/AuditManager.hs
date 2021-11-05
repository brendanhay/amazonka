{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AuditManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AuditManager where

import Amazonka.AuditManager
import qualified Data.Proxy as Proxy
import Test.Amazonka.AuditManager.Internal
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
--         [ requestCreateAssessmentReport $
--             newCreateAssessmentReport
--
--         , requestRegisterOrganizationAdminAccount $
--             newRegisterOrganizationAdminAccount
--
--         , requestListNotifications $
--             newListNotifications
--
--         , requestBatchCreateDelegationByAssessment $
--             newBatchCreateDelegationByAssessment
--
--         , requestGetEvidenceFoldersByAssessmentControl $
--             newGetEvidenceFoldersByAssessmentControl
--
--         , requestBatchDeleteDelegationByAssessment $
--             newBatchDeleteDelegationByAssessment
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetEvidence $
--             newGetEvidence
--
--         , requestGetServicesInScope $
--             newGetServicesInScope
--
--         , requestBatchDisassociateAssessmentReportEvidence $
--             newBatchDisassociateAssessmentReportEvidence
--
--         , requestDeregisterOrganizationAdminAccount $
--             newDeregisterOrganizationAdminAccount
--
--         , requestGetAssessmentReportUrl $
--             newGetAssessmentReportUrl
--
--         , requestUpdateAssessmentControl $
--             newUpdateAssessmentControl
--
--         , requestDeleteAssessmentFramework $
--             newDeleteAssessmentFramework
--
--         , requestUpdateAssessmentFramework $
--             newUpdateAssessmentFramework
--
--         , requestBatchAssociateAssessmentReportEvidence $
--             newBatchAssociateAssessmentReportEvidence
--
--         , requestGetEvidenceByEvidenceFolder $
--             newGetEvidenceByEvidenceFolder
--
--         , requestCreateAssessmentFramework $
--             newCreateAssessmentFramework
--
--         , requestListKeywordsForDataSource $
--             newListKeywordsForDataSource
--
--         , requestListAssessmentReports $
--             newListAssessmentReports
--
--         , requestValidateAssessmentReportIntegrity $
--             newValidateAssessmentReportIntegrity
--
--         , requestDeregisterAccount $
--             newDeregisterAccount
--
--         , requestDeleteAssessmentReport $
--             newDeleteAssessmentReport
--
--         , requestUpdateSettings $
--             newUpdateSettings
--
--         , requestGetAssessmentFramework $
--             newGetAssessmentFramework
--
--         , requestDeleteAssessment $
--             newDeleteAssessment
--
--         , requestGetChangeLogs $
--             newGetChangeLogs
--
--         , requestUpdateAssessment $
--             newUpdateAssessment
--
--         , requestGetDelegations $
--             newGetDelegations
--
--         , requestDisassociateAssessmentReportEvidenceFolder $
--             newDisassociateAssessmentReportEvidenceFolder
--
--         , requestListAssessments $
--             newListAssessments
--
--         , requestCreateAssessment $
--             newCreateAssessment
--
--         , requestGetEvidenceFoldersByAssessment $
--             newGetEvidenceFoldersByAssessment
--
--         , requestRegisterAccount $
--             newRegisterAccount
--
--         , requestGetAssessment $
--             newGetAssessment
--
--         , requestBatchImportEvidenceToAssessmentControl $
--             newBatchImportEvidenceToAssessmentControl
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetEvidenceFolder $
--             newGetEvidenceFolder
--
--         , requestListAssessmentFrameworks $
--             newListAssessmentFrameworks
--
--         , requestCreateControl $
--             newCreateControl
--
--         , requestUpdateAssessmentStatus $
--             newUpdateAssessmentStatus
--
--         , requestGetAccountStatus $
--             newGetAccountStatus
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetSettings $
--             newGetSettings
--
--         , requestGetOrganizationAdminAccount $
--             newGetOrganizationAdminAccount
--
--         , requestDeleteControl $
--             newDeleteControl
--
--         , requestUpdateControl $
--             newUpdateControl
--
--         , requestUpdateAssessmentControlSetStatus $
--             newUpdateAssessmentControlSetStatus
--
--         , requestListControls $
--             newListControls
--
--         , requestAssociateAssessmentReportEvidenceFolder $
--             newAssociateAssessmentReportEvidenceFolder
--
--         , requestGetControl $
--             newGetControl
--
--           ]

--     , testGroup "response"
--         [ responseCreateAssessmentReport $
--             newCreateAssessmentReportResponse
--
--         , responseRegisterOrganizationAdminAccount $
--             newRegisterOrganizationAdminAccountResponse
--
--         , responseListNotifications $
--             newListNotificationsResponse
--
--         , responseBatchCreateDelegationByAssessment $
--             newBatchCreateDelegationByAssessmentResponse
--
--         , responseGetEvidenceFoldersByAssessmentControl $
--             newGetEvidenceFoldersByAssessmentControlResponse
--
--         , responseBatchDeleteDelegationByAssessment $
--             newBatchDeleteDelegationByAssessmentResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetEvidence $
--             newGetEvidenceResponse
--
--         , responseGetServicesInScope $
--             newGetServicesInScopeResponse
--
--         , responseBatchDisassociateAssessmentReportEvidence $
--             newBatchDisassociateAssessmentReportEvidenceResponse
--
--         , responseDeregisterOrganizationAdminAccount $
--             newDeregisterOrganizationAdminAccountResponse
--
--         , responseGetAssessmentReportUrl $
--             newGetAssessmentReportUrlResponse
--
--         , responseUpdateAssessmentControl $
--             newUpdateAssessmentControlResponse
--
--         , responseDeleteAssessmentFramework $
--             newDeleteAssessmentFrameworkResponse
--
--         , responseUpdateAssessmentFramework $
--             newUpdateAssessmentFrameworkResponse
--
--         , responseBatchAssociateAssessmentReportEvidence $
--             newBatchAssociateAssessmentReportEvidenceResponse
--
--         , responseGetEvidenceByEvidenceFolder $
--             newGetEvidenceByEvidenceFolderResponse
--
--         , responseCreateAssessmentFramework $
--             newCreateAssessmentFrameworkResponse
--
--         , responseListKeywordsForDataSource $
--             newListKeywordsForDataSourceResponse
--
--         , responseListAssessmentReports $
--             newListAssessmentReportsResponse
--
--         , responseValidateAssessmentReportIntegrity $
--             newValidateAssessmentReportIntegrityResponse
--
--         , responseDeregisterAccount $
--             newDeregisterAccountResponse
--
--         , responseDeleteAssessmentReport $
--             newDeleteAssessmentReportResponse
--
--         , responseUpdateSettings $
--             newUpdateSettingsResponse
--
--         , responseGetAssessmentFramework $
--             newGetAssessmentFrameworkResponse
--
--         , responseDeleteAssessment $
--             newDeleteAssessmentResponse
--
--         , responseGetChangeLogs $
--             newGetChangeLogsResponse
--
--         , responseUpdateAssessment $
--             newUpdateAssessmentResponse
--
--         , responseGetDelegations $
--             newGetDelegationsResponse
--
--         , responseDisassociateAssessmentReportEvidenceFolder $
--             newDisassociateAssessmentReportEvidenceFolderResponse
--
--         , responseListAssessments $
--             newListAssessmentsResponse
--
--         , responseCreateAssessment $
--             newCreateAssessmentResponse
--
--         , responseGetEvidenceFoldersByAssessment $
--             newGetEvidenceFoldersByAssessmentResponse
--
--         , responseRegisterAccount $
--             newRegisterAccountResponse
--
--         , responseGetAssessment $
--             newGetAssessmentResponse
--
--         , responseBatchImportEvidenceToAssessmentControl $
--             newBatchImportEvidenceToAssessmentControlResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetEvidenceFolder $
--             newGetEvidenceFolderResponse
--
--         , responseListAssessmentFrameworks $
--             newListAssessmentFrameworksResponse
--
--         , responseCreateControl $
--             newCreateControlResponse
--
--         , responseUpdateAssessmentStatus $
--             newUpdateAssessmentStatusResponse
--
--         , responseGetAccountStatus $
--             newGetAccountStatusResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetSettings $
--             newGetSettingsResponse
--
--         , responseGetOrganizationAdminAccount $
--             newGetOrganizationAdminAccountResponse
--
--         , responseDeleteControl $
--             newDeleteControlResponse
--
--         , responseUpdateControl $
--             newUpdateControlResponse
--
--         , responseUpdateAssessmentControlSetStatus $
--             newUpdateAssessmentControlSetStatusResponse
--
--         , responseListControls $
--             newListControlsResponse
--
--         , responseAssociateAssessmentReportEvidenceFolder $
--             newAssociateAssessmentReportEvidenceFolderResponse
--
--         , responseGetControl $
--             newGetControlResponse
--
--           ]
--     ]

-- Requests

requestCreateAssessmentReport :: CreateAssessmentReport -> TestTree
requestCreateAssessmentReport =
  req
    "CreateAssessmentReport"
    "fixture/CreateAssessmentReport.yaml"

requestRegisterOrganizationAdminAccount :: RegisterOrganizationAdminAccount -> TestTree
requestRegisterOrganizationAdminAccount =
  req
    "RegisterOrganizationAdminAccount"
    "fixture/RegisterOrganizationAdminAccount.yaml"

requestListNotifications :: ListNotifications -> TestTree
requestListNotifications =
  req
    "ListNotifications"
    "fixture/ListNotifications.yaml"

requestBatchCreateDelegationByAssessment :: BatchCreateDelegationByAssessment -> TestTree
requestBatchCreateDelegationByAssessment =
  req
    "BatchCreateDelegationByAssessment"
    "fixture/BatchCreateDelegationByAssessment.yaml"

requestGetEvidenceFoldersByAssessmentControl :: GetEvidenceFoldersByAssessmentControl -> TestTree
requestGetEvidenceFoldersByAssessmentControl =
  req
    "GetEvidenceFoldersByAssessmentControl"
    "fixture/GetEvidenceFoldersByAssessmentControl.yaml"

requestBatchDeleteDelegationByAssessment :: BatchDeleteDelegationByAssessment -> TestTree
requestBatchDeleteDelegationByAssessment =
  req
    "BatchDeleteDelegationByAssessment"
    "fixture/BatchDeleteDelegationByAssessment.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetEvidence :: GetEvidence -> TestTree
requestGetEvidence =
  req
    "GetEvidence"
    "fixture/GetEvidence.yaml"

requestGetServicesInScope :: GetServicesInScope -> TestTree
requestGetServicesInScope =
  req
    "GetServicesInScope"
    "fixture/GetServicesInScope.yaml"

requestBatchDisassociateAssessmentReportEvidence :: BatchDisassociateAssessmentReportEvidence -> TestTree
requestBatchDisassociateAssessmentReportEvidence =
  req
    "BatchDisassociateAssessmentReportEvidence"
    "fixture/BatchDisassociateAssessmentReportEvidence.yaml"

requestDeregisterOrganizationAdminAccount :: DeregisterOrganizationAdminAccount -> TestTree
requestDeregisterOrganizationAdminAccount =
  req
    "DeregisterOrganizationAdminAccount"
    "fixture/DeregisterOrganizationAdminAccount.yaml"

requestGetAssessmentReportUrl :: GetAssessmentReportUrl -> TestTree
requestGetAssessmentReportUrl =
  req
    "GetAssessmentReportUrl"
    "fixture/GetAssessmentReportUrl.yaml"

requestUpdateAssessmentControl :: UpdateAssessmentControl -> TestTree
requestUpdateAssessmentControl =
  req
    "UpdateAssessmentControl"
    "fixture/UpdateAssessmentControl.yaml"

requestDeleteAssessmentFramework :: DeleteAssessmentFramework -> TestTree
requestDeleteAssessmentFramework =
  req
    "DeleteAssessmentFramework"
    "fixture/DeleteAssessmentFramework.yaml"

requestUpdateAssessmentFramework :: UpdateAssessmentFramework -> TestTree
requestUpdateAssessmentFramework =
  req
    "UpdateAssessmentFramework"
    "fixture/UpdateAssessmentFramework.yaml"

requestBatchAssociateAssessmentReportEvidence :: BatchAssociateAssessmentReportEvidence -> TestTree
requestBatchAssociateAssessmentReportEvidence =
  req
    "BatchAssociateAssessmentReportEvidence"
    "fixture/BatchAssociateAssessmentReportEvidence.yaml"

requestGetEvidenceByEvidenceFolder :: GetEvidenceByEvidenceFolder -> TestTree
requestGetEvidenceByEvidenceFolder =
  req
    "GetEvidenceByEvidenceFolder"
    "fixture/GetEvidenceByEvidenceFolder.yaml"

requestCreateAssessmentFramework :: CreateAssessmentFramework -> TestTree
requestCreateAssessmentFramework =
  req
    "CreateAssessmentFramework"
    "fixture/CreateAssessmentFramework.yaml"

requestListKeywordsForDataSource :: ListKeywordsForDataSource -> TestTree
requestListKeywordsForDataSource =
  req
    "ListKeywordsForDataSource"
    "fixture/ListKeywordsForDataSource.yaml"

requestListAssessmentReports :: ListAssessmentReports -> TestTree
requestListAssessmentReports =
  req
    "ListAssessmentReports"
    "fixture/ListAssessmentReports.yaml"

requestValidateAssessmentReportIntegrity :: ValidateAssessmentReportIntegrity -> TestTree
requestValidateAssessmentReportIntegrity =
  req
    "ValidateAssessmentReportIntegrity"
    "fixture/ValidateAssessmentReportIntegrity.yaml"

requestDeregisterAccount :: DeregisterAccount -> TestTree
requestDeregisterAccount =
  req
    "DeregisterAccount"
    "fixture/DeregisterAccount.yaml"

requestDeleteAssessmentReport :: DeleteAssessmentReport -> TestTree
requestDeleteAssessmentReport =
  req
    "DeleteAssessmentReport"
    "fixture/DeleteAssessmentReport.yaml"

requestUpdateSettings :: UpdateSettings -> TestTree
requestUpdateSettings =
  req
    "UpdateSettings"
    "fixture/UpdateSettings.yaml"

requestGetAssessmentFramework :: GetAssessmentFramework -> TestTree
requestGetAssessmentFramework =
  req
    "GetAssessmentFramework"
    "fixture/GetAssessmentFramework.yaml"

requestDeleteAssessment :: DeleteAssessment -> TestTree
requestDeleteAssessment =
  req
    "DeleteAssessment"
    "fixture/DeleteAssessment.yaml"

requestGetChangeLogs :: GetChangeLogs -> TestTree
requestGetChangeLogs =
  req
    "GetChangeLogs"
    "fixture/GetChangeLogs.yaml"

requestUpdateAssessment :: UpdateAssessment -> TestTree
requestUpdateAssessment =
  req
    "UpdateAssessment"
    "fixture/UpdateAssessment.yaml"

requestGetDelegations :: GetDelegations -> TestTree
requestGetDelegations =
  req
    "GetDelegations"
    "fixture/GetDelegations.yaml"

requestDisassociateAssessmentReportEvidenceFolder :: DisassociateAssessmentReportEvidenceFolder -> TestTree
requestDisassociateAssessmentReportEvidenceFolder =
  req
    "DisassociateAssessmentReportEvidenceFolder"
    "fixture/DisassociateAssessmentReportEvidenceFolder.yaml"

requestListAssessments :: ListAssessments -> TestTree
requestListAssessments =
  req
    "ListAssessments"
    "fixture/ListAssessments.yaml"

requestCreateAssessment :: CreateAssessment -> TestTree
requestCreateAssessment =
  req
    "CreateAssessment"
    "fixture/CreateAssessment.yaml"

requestGetEvidenceFoldersByAssessment :: GetEvidenceFoldersByAssessment -> TestTree
requestGetEvidenceFoldersByAssessment =
  req
    "GetEvidenceFoldersByAssessment"
    "fixture/GetEvidenceFoldersByAssessment.yaml"

requestRegisterAccount :: RegisterAccount -> TestTree
requestRegisterAccount =
  req
    "RegisterAccount"
    "fixture/RegisterAccount.yaml"

requestGetAssessment :: GetAssessment -> TestTree
requestGetAssessment =
  req
    "GetAssessment"
    "fixture/GetAssessment.yaml"

requestBatchImportEvidenceToAssessmentControl :: BatchImportEvidenceToAssessmentControl -> TestTree
requestBatchImportEvidenceToAssessmentControl =
  req
    "BatchImportEvidenceToAssessmentControl"
    "fixture/BatchImportEvidenceToAssessmentControl.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetEvidenceFolder :: GetEvidenceFolder -> TestTree
requestGetEvidenceFolder =
  req
    "GetEvidenceFolder"
    "fixture/GetEvidenceFolder.yaml"

requestListAssessmentFrameworks :: ListAssessmentFrameworks -> TestTree
requestListAssessmentFrameworks =
  req
    "ListAssessmentFrameworks"
    "fixture/ListAssessmentFrameworks.yaml"

requestCreateControl :: CreateControl -> TestTree
requestCreateControl =
  req
    "CreateControl"
    "fixture/CreateControl.yaml"

requestUpdateAssessmentStatus :: UpdateAssessmentStatus -> TestTree
requestUpdateAssessmentStatus =
  req
    "UpdateAssessmentStatus"
    "fixture/UpdateAssessmentStatus.yaml"

requestGetAccountStatus :: GetAccountStatus -> TestTree
requestGetAccountStatus =
  req
    "GetAccountStatus"
    "fixture/GetAccountStatus.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetSettings :: GetSettings -> TestTree
requestGetSettings =
  req
    "GetSettings"
    "fixture/GetSettings.yaml"

requestGetOrganizationAdminAccount :: GetOrganizationAdminAccount -> TestTree
requestGetOrganizationAdminAccount =
  req
    "GetOrganizationAdminAccount"
    "fixture/GetOrganizationAdminAccount.yaml"

requestDeleteControl :: DeleteControl -> TestTree
requestDeleteControl =
  req
    "DeleteControl"
    "fixture/DeleteControl.yaml"

requestUpdateControl :: UpdateControl -> TestTree
requestUpdateControl =
  req
    "UpdateControl"
    "fixture/UpdateControl.yaml"

requestUpdateAssessmentControlSetStatus :: UpdateAssessmentControlSetStatus -> TestTree
requestUpdateAssessmentControlSetStatus =
  req
    "UpdateAssessmentControlSetStatus"
    "fixture/UpdateAssessmentControlSetStatus.yaml"

requestListControls :: ListControls -> TestTree
requestListControls =
  req
    "ListControls"
    "fixture/ListControls.yaml"

requestAssociateAssessmentReportEvidenceFolder :: AssociateAssessmentReportEvidenceFolder -> TestTree
requestAssociateAssessmentReportEvidenceFolder =
  req
    "AssociateAssessmentReportEvidenceFolder"
    "fixture/AssociateAssessmentReportEvidenceFolder.yaml"

requestGetControl :: GetControl -> TestTree
requestGetControl =
  req
    "GetControl"
    "fixture/GetControl.yaml"

-- Responses

responseCreateAssessmentReport :: CreateAssessmentReportResponse -> TestTree
responseCreateAssessmentReport =
  res
    "CreateAssessmentReportResponse"
    "fixture/CreateAssessmentReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssessmentReport)

responseRegisterOrganizationAdminAccount :: RegisterOrganizationAdminAccountResponse -> TestTree
responseRegisterOrganizationAdminAccount =
  res
    "RegisterOrganizationAdminAccountResponse"
    "fixture/RegisterOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterOrganizationAdminAccount)

responseListNotifications :: ListNotificationsResponse -> TestTree
responseListNotifications =
  res
    "ListNotificationsResponse"
    "fixture/ListNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotifications)

responseBatchCreateDelegationByAssessment :: BatchCreateDelegationByAssessmentResponse -> TestTree
responseBatchCreateDelegationByAssessment =
  res
    "BatchCreateDelegationByAssessmentResponse"
    "fixture/BatchCreateDelegationByAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateDelegationByAssessment)

responseGetEvidenceFoldersByAssessmentControl :: GetEvidenceFoldersByAssessmentControlResponse -> TestTree
responseGetEvidenceFoldersByAssessmentControl =
  res
    "GetEvidenceFoldersByAssessmentControlResponse"
    "fixture/GetEvidenceFoldersByAssessmentControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvidenceFoldersByAssessmentControl)

responseBatchDeleteDelegationByAssessment :: BatchDeleteDelegationByAssessmentResponse -> TestTree
responseBatchDeleteDelegationByAssessment =
  res
    "BatchDeleteDelegationByAssessmentResponse"
    "fixture/BatchDeleteDelegationByAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteDelegationByAssessment)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetEvidence :: GetEvidenceResponse -> TestTree
responseGetEvidence =
  res
    "GetEvidenceResponse"
    "fixture/GetEvidenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvidence)

responseGetServicesInScope :: GetServicesInScopeResponse -> TestTree
responseGetServicesInScope =
  res
    "GetServicesInScopeResponse"
    "fixture/GetServicesInScopeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServicesInScope)

responseBatchDisassociateAssessmentReportEvidence :: BatchDisassociateAssessmentReportEvidenceResponse -> TestTree
responseBatchDisassociateAssessmentReportEvidence =
  res
    "BatchDisassociateAssessmentReportEvidenceResponse"
    "fixture/BatchDisassociateAssessmentReportEvidenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateAssessmentReportEvidence)

responseDeregisterOrganizationAdminAccount :: DeregisterOrganizationAdminAccountResponse -> TestTree
responseDeregisterOrganizationAdminAccount =
  res
    "DeregisterOrganizationAdminAccountResponse"
    "fixture/DeregisterOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterOrganizationAdminAccount)

responseGetAssessmentReportUrl :: GetAssessmentReportUrlResponse -> TestTree
responseGetAssessmentReportUrl =
  res
    "GetAssessmentReportUrlResponse"
    "fixture/GetAssessmentReportUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssessmentReportUrl)

responseUpdateAssessmentControl :: UpdateAssessmentControlResponse -> TestTree
responseUpdateAssessmentControl =
  res
    "UpdateAssessmentControlResponse"
    "fixture/UpdateAssessmentControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessmentControl)

responseDeleteAssessmentFramework :: DeleteAssessmentFrameworkResponse -> TestTree
responseDeleteAssessmentFramework =
  res
    "DeleteAssessmentFrameworkResponse"
    "fixture/DeleteAssessmentFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssessmentFramework)

responseUpdateAssessmentFramework :: UpdateAssessmentFrameworkResponse -> TestTree
responseUpdateAssessmentFramework =
  res
    "UpdateAssessmentFrameworkResponse"
    "fixture/UpdateAssessmentFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessmentFramework)

responseBatchAssociateAssessmentReportEvidence :: BatchAssociateAssessmentReportEvidenceResponse -> TestTree
responseBatchAssociateAssessmentReportEvidence =
  res
    "BatchAssociateAssessmentReportEvidenceResponse"
    "fixture/BatchAssociateAssessmentReportEvidenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateAssessmentReportEvidence)

responseGetEvidenceByEvidenceFolder :: GetEvidenceByEvidenceFolderResponse -> TestTree
responseGetEvidenceByEvidenceFolder =
  res
    "GetEvidenceByEvidenceFolderResponse"
    "fixture/GetEvidenceByEvidenceFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvidenceByEvidenceFolder)

responseCreateAssessmentFramework :: CreateAssessmentFrameworkResponse -> TestTree
responseCreateAssessmentFramework =
  res
    "CreateAssessmentFrameworkResponse"
    "fixture/CreateAssessmentFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssessmentFramework)

responseListKeywordsForDataSource :: ListKeywordsForDataSourceResponse -> TestTree
responseListKeywordsForDataSource =
  res
    "ListKeywordsForDataSourceResponse"
    "fixture/ListKeywordsForDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeywordsForDataSource)

responseListAssessmentReports :: ListAssessmentReportsResponse -> TestTree
responseListAssessmentReports =
  res
    "ListAssessmentReportsResponse"
    "fixture/ListAssessmentReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessmentReports)

responseValidateAssessmentReportIntegrity :: ValidateAssessmentReportIntegrityResponse -> TestTree
responseValidateAssessmentReportIntegrity =
  res
    "ValidateAssessmentReportIntegrityResponse"
    "fixture/ValidateAssessmentReportIntegrityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateAssessmentReportIntegrity)

responseDeregisterAccount :: DeregisterAccountResponse -> TestTree
responseDeregisterAccount =
  res
    "DeregisterAccountResponse"
    "fixture/DeregisterAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterAccount)

responseDeleteAssessmentReport :: DeleteAssessmentReportResponse -> TestTree
responseDeleteAssessmentReport =
  res
    "DeleteAssessmentReportResponse"
    "fixture/DeleteAssessmentReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssessmentReport)

responseUpdateSettings :: UpdateSettingsResponse -> TestTree
responseUpdateSettings =
  res
    "UpdateSettingsResponse"
    "fixture/UpdateSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSettings)

responseGetAssessmentFramework :: GetAssessmentFrameworkResponse -> TestTree
responseGetAssessmentFramework =
  res
    "GetAssessmentFrameworkResponse"
    "fixture/GetAssessmentFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssessmentFramework)

responseDeleteAssessment :: DeleteAssessmentResponse -> TestTree
responseDeleteAssessment =
  res
    "DeleteAssessmentResponse"
    "fixture/DeleteAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssessment)

responseGetChangeLogs :: GetChangeLogsResponse -> TestTree
responseGetChangeLogs =
  res
    "GetChangeLogsResponse"
    "fixture/GetChangeLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChangeLogs)

responseUpdateAssessment :: UpdateAssessmentResponse -> TestTree
responseUpdateAssessment =
  res
    "UpdateAssessmentResponse"
    "fixture/UpdateAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessment)

responseGetDelegations :: GetDelegationsResponse -> TestTree
responseGetDelegations =
  res
    "GetDelegationsResponse"
    "fixture/GetDelegationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDelegations)

responseDisassociateAssessmentReportEvidenceFolder :: DisassociateAssessmentReportEvidenceFolderResponse -> TestTree
responseDisassociateAssessmentReportEvidenceFolder =
  res
    "DisassociateAssessmentReportEvidenceFolderResponse"
    "fixture/DisassociateAssessmentReportEvidenceFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAssessmentReportEvidenceFolder)

responseListAssessments :: ListAssessmentsResponse -> TestTree
responseListAssessments =
  res
    "ListAssessmentsResponse"
    "fixture/ListAssessmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessments)

responseCreateAssessment :: CreateAssessmentResponse -> TestTree
responseCreateAssessment =
  res
    "CreateAssessmentResponse"
    "fixture/CreateAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssessment)

responseGetEvidenceFoldersByAssessment :: GetEvidenceFoldersByAssessmentResponse -> TestTree
responseGetEvidenceFoldersByAssessment =
  res
    "GetEvidenceFoldersByAssessmentResponse"
    "fixture/GetEvidenceFoldersByAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvidenceFoldersByAssessment)

responseRegisterAccount :: RegisterAccountResponse -> TestTree
responseRegisterAccount =
  res
    "RegisterAccountResponse"
    "fixture/RegisterAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterAccount)

responseGetAssessment :: GetAssessmentResponse -> TestTree
responseGetAssessment =
  res
    "GetAssessmentResponse"
    "fixture/GetAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssessment)

responseBatchImportEvidenceToAssessmentControl :: BatchImportEvidenceToAssessmentControlResponse -> TestTree
responseBatchImportEvidenceToAssessmentControl =
  res
    "BatchImportEvidenceToAssessmentControlResponse"
    "fixture/BatchImportEvidenceToAssessmentControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchImportEvidenceToAssessmentControl)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetEvidenceFolder :: GetEvidenceFolderResponse -> TestTree
responseGetEvidenceFolder =
  res
    "GetEvidenceFolderResponse"
    "fixture/GetEvidenceFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvidenceFolder)

responseListAssessmentFrameworks :: ListAssessmentFrameworksResponse -> TestTree
responseListAssessmentFrameworks =
  res
    "ListAssessmentFrameworksResponse"
    "fixture/ListAssessmentFrameworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessmentFrameworks)

responseCreateControl :: CreateControlResponse -> TestTree
responseCreateControl =
  res
    "CreateControlResponse"
    "fixture/CreateControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateControl)

responseUpdateAssessmentStatus :: UpdateAssessmentStatusResponse -> TestTree
responseUpdateAssessmentStatus =
  res
    "UpdateAssessmentStatusResponse"
    "fixture/UpdateAssessmentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessmentStatus)

responseGetAccountStatus :: GetAccountStatusResponse -> TestTree
responseGetAccountStatus =
  res
    "GetAccountStatusResponse"
    "fixture/GetAccountStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountStatus)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseGetSettings :: GetSettingsResponse -> TestTree
responseGetSettings =
  res
    "GetSettingsResponse"
    "fixture/GetSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSettings)

responseGetOrganizationAdminAccount :: GetOrganizationAdminAccountResponse -> TestTree
responseGetOrganizationAdminAccount =
  res
    "GetOrganizationAdminAccountResponse"
    "fixture/GetOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOrganizationAdminAccount)

responseDeleteControl :: DeleteControlResponse -> TestTree
responseDeleteControl =
  res
    "DeleteControlResponse"
    "fixture/DeleteControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteControl)

responseUpdateControl :: UpdateControlResponse -> TestTree
responseUpdateControl =
  res
    "UpdateControlResponse"
    "fixture/UpdateControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateControl)

responseUpdateAssessmentControlSetStatus :: UpdateAssessmentControlSetStatusResponse -> TestTree
responseUpdateAssessmentControlSetStatus =
  res
    "UpdateAssessmentControlSetStatusResponse"
    "fixture/UpdateAssessmentControlSetStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessmentControlSetStatus)

responseListControls :: ListControlsResponse -> TestTree
responseListControls =
  res
    "ListControlsResponse"
    "fixture/ListControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListControls)

responseAssociateAssessmentReportEvidenceFolder :: AssociateAssessmentReportEvidenceFolderResponse -> TestTree
responseAssociateAssessmentReportEvidenceFolder =
  res
    "AssociateAssessmentReportEvidenceFolderResponse"
    "fixture/AssociateAssessmentReportEvidenceFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAssessmentReportEvidenceFolder)

responseGetControl :: GetControlResponse -> TestTree
responseGetControl =
  res
    "GetControlResponse"
    "fixture/GetControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetControl)
