{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AuditManager
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestAssociateAssessmentReportEvidenceFolder $
--             newAssociateAssessmentReportEvidenceFolder
--
--         , requestBatchAssociateAssessmentReportEvidence $
--             newBatchAssociateAssessmentReportEvidence
--
--         , requestBatchCreateDelegationByAssessment $
--             newBatchCreateDelegationByAssessment
--
--         , requestBatchDeleteDelegationByAssessment $
--             newBatchDeleteDelegationByAssessment
--
--         , requestBatchDisassociateAssessmentReportEvidence $
--             newBatchDisassociateAssessmentReportEvidence
--
--         , requestBatchImportEvidenceToAssessmentControl $
--             newBatchImportEvidenceToAssessmentControl
--
--         , requestCreateAssessment $
--             newCreateAssessment
--
--         , requestCreateAssessmentFramework $
--             newCreateAssessmentFramework
--
--         , requestCreateAssessmentReport $
--             newCreateAssessmentReport
--
--         , requestCreateControl $
--             newCreateControl
--
--         , requestDeleteAssessment $
--             newDeleteAssessment
--
--         , requestDeleteAssessmentFramework $
--             newDeleteAssessmentFramework
--
--         , requestDeleteAssessmentFrameworkShare $
--             newDeleteAssessmentFrameworkShare
--
--         , requestDeleteAssessmentReport $
--             newDeleteAssessmentReport
--
--         , requestDeleteControl $
--             newDeleteControl
--
--         , requestDeregisterAccount $
--             newDeregisterAccount
--
--         , requestDeregisterOrganizationAdminAccount $
--             newDeregisterOrganizationAdminAccount
--
--         , requestDisassociateAssessmentReportEvidenceFolder $
--             newDisassociateAssessmentReportEvidenceFolder
--
--         , requestGetAccountStatus $
--             newGetAccountStatus
--
--         , requestGetAssessment $
--             newGetAssessment
--
--         , requestGetAssessmentFramework $
--             newGetAssessmentFramework
--
--         , requestGetAssessmentReportUrl $
--             newGetAssessmentReportUrl
--
--         , requestGetChangeLogs $
--             newGetChangeLogs
--
--         , requestGetControl $
--             newGetControl
--
--         , requestGetDelegations $
--             newGetDelegations
--
--         , requestGetEvidence $
--             newGetEvidence
--
--         , requestGetEvidenceByEvidenceFolder $
--             newGetEvidenceByEvidenceFolder
--
--         , requestGetEvidenceFolder $
--             newGetEvidenceFolder
--
--         , requestGetEvidenceFoldersByAssessment $
--             newGetEvidenceFoldersByAssessment
--
--         , requestGetEvidenceFoldersByAssessmentControl $
--             newGetEvidenceFoldersByAssessmentControl
--
--         , requestGetInsights $
--             newGetInsights
--
--         , requestGetInsightsByAssessment $
--             newGetInsightsByAssessment
--
--         , requestGetOrganizationAdminAccount $
--             newGetOrganizationAdminAccount
--
--         , requestGetServicesInScope $
--             newGetServicesInScope
--
--         , requestGetSettings $
--             newGetSettings
--
--         , requestListAssessmentControlInsightsByControlDomain $
--             newListAssessmentControlInsightsByControlDomain
--
--         , requestListAssessmentFrameworkShareRequests $
--             newListAssessmentFrameworkShareRequests
--
--         , requestListAssessmentFrameworks $
--             newListAssessmentFrameworks
--
--         , requestListAssessmentReports $
--             newListAssessmentReports
--
--         , requestListAssessments $
--             newListAssessments
--
--         , requestListControlDomainInsights $
--             newListControlDomainInsights
--
--         , requestListControlDomainInsightsByAssessment $
--             newListControlDomainInsightsByAssessment
--
--         , requestListControlInsightsByControlDomain $
--             newListControlInsightsByControlDomain
--
--         , requestListControls $
--             newListControls
--
--         , requestListKeywordsForDataSource $
--             newListKeywordsForDataSource
--
--         , requestListNotifications $
--             newListNotifications
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRegisterAccount $
--             newRegisterAccount
--
--         , requestRegisterOrganizationAdminAccount $
--             newRegisterOrganizationAdminAccount
--
--         , requestStartAssessmentFrameworkShare $
--             newStartAssessmentFrameworkShare
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAssessment $
--             newUpdateAssessment
--
--         , requestUpdateAssessmentControl $
--             newUpdateAssessmentControl
--
--         , requestUpdateAssessmentControlSetStatus $
--             newUpdateAssessmentControlSetStatus
--
--         , requestUpdateAssessmentFramework $
--             newUpdateAssessmentFramework
--
--         , requestUpdateAssessmentFrameworkShare $
--             newUpdateAssessmentFrameworkShare
--
--         , requestUpdateAssessmentStatus $
--             newUpdateAssessmentStatus
--
--         , requestUpdateControl $
--             newUpdateControl
--
--         , requestUpdateSettings $
--             newUpdateSettings
--
--         , requestValidateAssessmentReportIntegrity $
--             newValidateAssessmentReportIntegrity
--
--           ]

--     , testGroup "response"
--         [ responseAssociateAssessmentReportEvidenceFolder $
--             newAssociateAssessmentReportEvidenceFolderResponse
--
--         , responseBatchAssociateAssessmentReportEvidence $
--             newBatchAssociateAssessmentReportEvidenceResponse
--
--         , responseBatchCreateDelegationByAssessment $
--             newBatchCreateDelegationByAssessmentResponse
--
--         , responseBatchDeleteDelegationByAssessment $
--             newBatchDeleteDelegationByAssessmentResponse
--
--         , responseBatchDisassociateAssessmentReportEvidence $
--             newBatchDisassociateAssessmentReportEvidenceResponse
--
--         , responseBatchImportEvidenceToAssessmentControl $
--             newBatchImportEvidenceToAssessmentControlResponse
--
--         , responseCreateAssessment $
--             newCreateAssessmentResponse
--
--         , responseCreateAssessmentFramework $
--             newCreateAssessmentFrameworkResponse
--
--         , responseCreateAssessmentReport $
--             newCreateAssessmentReportResponse
--
--         , responseCreateControl $
--             newCreateControlResponse
--
--         , responseDeleteAssessment $
--             newDeleteAssessmentResponse
--
--         , responseDeleteAssessmentFramework $
--             newDeleteAssessmentFrameworkResponse
--
--         , responseDeleteAssessmentFrameworkShare $
--             newDeleteAssessmentFrameworkShareResponse
--
--         , responseDeleteAssessmentReport $
--             newDeleteAssessmentReportResponse
--
--         , responseDeleteControl $
--             newDeleteControlResponse
--
--         , responseDeregisterAccount $
--             newDeregisterAccountResponse
--
--         , responseDeregisterOrganizationAdminAccount $
--             newDeregisterOrganizationAdminAccountResponse
--
--         , responseDisassociateAssessmentReportEvidenceFolder $
--             newDisassociateAssessmentReportEvidenceFolderResponse
--
--         , responseGetAccountStatus $
--             newGetAccountStatusResponse
--
--         , responseGetAssessment $
--             newGetAssessmentResponse
--
--         , responseGetAssessmentFramework $
--             newGetAssessmentFrameworkResponse
--
--         , responseGetAssessmentReportUrl $
--             newGetAssessmentReportUrlResponse
--
--         , responseGetChangeLogs $
--             newGetChangeLogsResponse
--
--         , responseGetControl $
--             newGetControlResponse
--
--         , responseGetDelegations $
--             newGetDelegationsResponse
--
--         , responseGetEvidence $
--             newGetEvidenceResponse
--
--         , responseGetEvidenceByEvidenceFolder $
--             newGetEvidenceByEvidenceFolderResponse
--
--         , responseGetEvidenceFolder $
--             newGetEvidenceFolderResponse
--
--         , responseGetEvidenceFoldersByAssessment $
--             newGetEvidenceFoldersByAssessmentResponse
--
--         , responseGetEvidenceFoldersByAssessmentControl $
--             newGetEvidenceFoldersByAssessmentControlResponse
--
--         , responseGetInsights $
--             newGetInsightsResponse
--
--         , responseGetInsightsByAssessment $
--             newGetInsightsByAssessmentResponse
--
--         , responseGetOrganizationAdminAccount $
--             newGetOrganizationAdminAccountResponse
--
--         , responseGetServicesInScope $
--             newGetServicesInScopeResponse
--
--         , responseGetSettings $
--             newGetSettingsResponse
--
--         , responseListAssessmentControlInsightsByControlDomain $
--             newListAssessmentControlInsightsByControlDomainResponse
--
--         , responseListAssessmentFrameworkShareRequests $
--             newListAssessmentFrameworkShareRequestsResponse
--
--         , responseListAssessmentFrameworks $
--             newListAssessmentFrameworksResponse
--
--         , responseListAssessmentReports $
--             newListAssessmentReportsResponse
--
--         , responseListAssessments $
--             newListAssessmentsResponse
--
--         , responseListControlDomainInsights $
--             newListControlDomainInsightsResponse
--
--         , responseListControlDomainInsightsByAssessment $
--             newListControlDomainInsightsByAssessmentResponse
--
--         , responseListControlInsightsByControlDomain $
--             newListControlInsightsByControlDomainResponse
--
--         , responseListControls $
--             newListControlsResponse
--
--         , responseListKeywordsForDataSource $
--             newListKeywordsForDataSourceResponse
--
--         , responseListNotifications $
--             newListNotificationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRegisterAccount $
--             newRegisterAccountResponse
--
--         , responseRegisterOrganizationAdminAccount $
--             newRegisterOrganizationAdminAccountResponse
--
--         , responseStartAssessmentFrameworkShare $
--             newStartAssessmentFrameworkShareResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAssessment $
--             newUpdateAssessmentResponse
--
--         , responseUpdateAssessmentControl $
--             newUpdateAssessmentControlResponse
--
--         , responseUpdateAssessmentControlSetStatus $
--             newUpdateAssessmentControlSetStatusResponse
--
--         , responseUpdateAssessmentFramework $
--             newUpdateAssessmentFrameworkResponse
--
--         , responseUpdateAssessmentFrameworkShare $
--             newUpdateAssessmentFrameworkShareResponse
--
--         , responseUpdateAssessmentStatus $
--             newUpdateAssessmentStatusResponse
--
--         , responseUpdateControl $
--             newUpdateControlResponse
--
--         , responseUpdateSettings $
--             newUpdateSettingsResponse
--
--         , responseValidateAssessmentReportIntegrity $
--             newValidateAssessmentReportIntegrityResponse
--
--           ]
--     ]

-- Requests

requestAssociateAssessmentReportEvidenceFolder :: AssociateAssessmentReportEvidenceFolder -> TestTree
requestAssociateAssessmentReportEvidenceFolder =
  req
    "AssociateAssessmentReportEvidenceFolder"
    "fixture/AssociateAssessmentReportEvidenceFolder.yaml"

requestBatchAssociateAssessmentReportEvidence :: BatchAssociateAssessmentReportEvidence -> TestTree
requestBatchAssociateAssessmentReportEvidence =
  req
    "BatchAssociateAssessmentReportEvidence"
    "fixture/BatchAssociateAssessmentReportEvidence.yaml"

requestBatchCreateDelegationByAssessment :: BatchCreateDelegationByAssessment -> TestTree
requestBatchCreateDelegationByAssessment =
  req
    "BatchCreateDelegationByAssessment"
    "fixture/BatchCreateDelegationByAssessment.yaml"

requestBatchDeleteDelegationByAssessment :: BatchDeleteDelegationByAssessment -> TestTree
requestBatchDeleteDelegationByAssessment =
  req
    "BatchDeleteDelegationByAssessment"
    "fixture/BatchDeleteDelegationByAssessment.yaml"

requestBatchDisassociateAssessmentReportEvidence :: BatchDisassociateAssessmentReportEvidence -> TestTree
requestBatchDisassociateAssessmentReportEvidence =
  req
    "BatchDisassociateAssessmentReportEvidence"
    "fixture/BatchDisassociateAssessmentReportEvidence.yaml"

requestBatchImportEvidenceToAssessmentControl :: BatchImportEvidenceToAssessmentControl -> TestTree
requestBatchImportEvidenceToAssessmentControl =
  req
    "BatchImportEvidenceToAssessmentControl"
    "fixture/BatchImportEvidenceToAssessmentControl.yaml"

requestCreateAssessment :: CreateAssessment -> TestTree
requestCreateAssessment =
  req
    "CreateAssessment"
    "fixture/CreateAssessment.yaml"

requestCreateAssessmentFramework :: CreateAssessmentFramework -> TestTree
requestCreateAssessmentFramework =
  req
    "CreateAssessmentFramework"
    "fixture/CreateAssessmentFramework.yaml"

requestCreateAssessmentReport :: CreateAssessmentReport -> TestTree
requestCreateAssessmentReport =
  req
    "CreateAssessmentReport"
    "fixture/CreateAssessmentReport.yaml"

requestCreateControl :: CreateControl -> TestTree
requestCreateControl =
  req
    "CreateControl"
    "fixture/CreateControl.yaml"

requestDeleteAssessment :: DeleteAssessment -> TestTree
requestDeleteAssessment =
  req
    "DeleteAssessment"
    "fixture/DeleteAssessment.yaml"

requestDeleteAssessmentFramework :: DeleteAssessmentFramework -> TestTree
requestDeleteAssessmentFramework =
  req
    "DeleteAssessmentFramework"
    "fixture/DeleteAssessmentFramework.yaml"

requestDeleteAssessmentFrameworkShare :: DeleteAssessmentFrameworkShare -> TestTree
requestDeleteAssessmentFrameworkShare =
  req
    "DeleteAssessmentFrameworkShare"
    "fixture/DeleteAssessmentFrameworkShare.yaml"

requestDeleteAssessmentReport :: DeleteAssessmentReport -> TestTree
requestDeleteAssessmentReport =
  req
    "DeleteAssessmentReport"
    "fixture/DeleteAssessmentReport.yaml"

requestDeleteControl :: DeleteControl -> TestTree
requestDeleteControl =
  req
    "DeleteControl"
    "fixture/DeleteControl.yaml"

requestDeregisterAccount :: DeregisterAccount -> TestTree
requestDeregisterAccount =
  req
    "DeregisterAccount"
    "fixture/DeregisterAccount.yaml"

requestDeregisterOrganizationAdminAccount :: DeregisterOrganizationAdminAccount -> TestTree
requestDeregisterOrganizationAdminAccount =
  req
    "DeregisterOrganizationAdminAccount"
    "fixture/DeregisterOrganizationAdminAccount.yaml"

requestDisassociateAssessmentReportEvidenceFolder :: DisassociateAssessmentReportEvidenceFolder -> TestTree
requestDisassociateAssessmentReportEvidenceFolder =
  req
    "DisassociateAssessmentReportEvidenceFolder"
    "fixture/DisassociateAssessmentReportEvidenceFolder.yaml"

requestGetAccountStatus :: GetAccountStatus -> TestTree
requestGetAccountStatus =
  req
    "GetAccountStatus"
    "fixture/GetAccountStatus.yaml"

requestGetAssessment :: GetAssessment -> TestTree
requestGetAssessment =
  req
    "GetAssessment"
    "fixture/GetAssessment.yaml"

requestGetAssessmentFramework :: GetAssessmentFramework -> TestTree
requestGetAssessmentFramework =
  req
    "GetAssessmentFramework"
    "fixture/GetAssessmentFramework.yaml"

requestGetAssessmentReportUrl :: GetAssessmentReportUrl -> TestTree
requestGetAssessmentReportUrl =
  req
    "GetAssessmentReportUrl"
    "fixture/GetAssessmentReportUrl.yaml"

requestGetChangeLogs :: GetChangeLogs -> TestTree
requestGetChangeLogs =
  req
    "GetChangeLogs"
    "fixture/GetChangeLogs.yaml"

requestGetControl :: GetControl -> TestTree
requestGetControl =
  req
    "GetControl"
    "fixture/GetControl.yaml"

requestGetDelegations :: GetDelegations -> TestTree
requestGetDelegations =
  req
    "GetDelegations"
    "fixture/GetDelegations.yaml"

requestGetEvidence :: GetEvidence -> TestTree
requestGetEvidence =
  req
    "GetEvidence"
    "fixture/GetEvidence.yaml"

requestGetEvidenceByEvidenceFolder :: GetEvidenceByEvidenceFolder -> TestTree
requestGetEvidenceByEvidenceFolder =
  req
    "GetEvidenceByEvidenceFolder"
    "fixture/GetEvidenceByEvidenceFolder.yaml"

requestGetEvidenceFolder :: GetEvidenceFolder -> TestTree
requestGetEvidenceFolder =
  req
    "GetEvidenceFolder"
    "fixture/GetEvidenceFolder.yaml"

requestGetEvidenceFoldersByAssessment :: GetEvidenceFoldersByAssessment -> TestTree
requestGetEvidenceFoldersByAssessment =
  req
    "GetEvidenceFoldersByAssessment"
    "fixture/GetEvidenceFoldersByAssessment.yaml"

requestGetEvidenceFoldersByAssessmentControl :: GetEvidenceFoldersByAssessmentControl -> TestTree
requestGetEvidenceFoldersByAssessmentControl =
  req
    "GetEvidenceFoldersByAssessmentControl"
    "fixture/GetEvidenceFoldersByAssessmentControl.yaml"

requestGetInsights :: GetInsights -> TestTree
requestGetInsights =
  req
    "GetInsights"
    "fixture/GetInsights.yaml"

requestGetInsightsByAssessment :: GetInsightsByAssessment -> TestTree
requestGetInsightsByAssessment =
  req
    "GetInsightsByAssessment"
    "fixture/GetInsightsByAssessment.yaml"

requestGetOrganizationAdminAccount :: GetOrganizationAdminAccount -> TestTree
requestGetOrganizationAdminAccount =
  req
    "GetOrganizationAdminAccount"
    "fixture/GetOrganizationAdminAccount.yaml"

requestGetServicesInScope :: GetServicesInScope -> TestTree
requestGetServicesInScope =
  req
    "GetServicesInScope"
    "fixture/GetServicesInScope.yaml"

requestGetSettings :: GetSettings -> TestTree
requestGetSettings =
  req
    "GetSettings"
    "fixture/GetSettings.yaml"

requestListAssessmentControlInsightsByControlDomain :: ListAssessmentControlInsightsByControlDomain -> TestTree
requestListAssessmentControlInsightsByControlDomain =
  req
    "ListAssessmentControlInsightsByControlDomain"
    "fixture/ListAssessmentControlInsightsByControlDomain.yaml"

requestListAssessmentFrameworkShareRequests :: ListAssessmentFrameworkShareRequests -> TestTree
requestListAssessmentFrameworkShareRequests =
  req
    "ListAssessmentFrameworkShareRequests"
    "fixture/ListAssessmentFrameworkShareRequests.yaml"

requestListAssessmentFrameworks :: ListAssessmentFrameworks -> TestTree
requestListAssessmentFrameworks =
  req
    "ListAssessmentFrameworks"
    "fixture/ListAssessmentFrameworks.yaml"

requestListAssessmentReports :: ListAssessmentReports -> TestTree
requestListAssessmentReports =
  req
    "ListAssessmentReports"
    "fixture/ListAssessmentReports.yaml"

requestListAssessments :: ListAssessments -> TestTree
requestListAssessments =
  req
    "ListAssessments"
    "fixture/ListAssessments.yaml"

requestListControlDomainInsights :: ListControlDomainInsights -> TestTree
requestListControlDomainInsights =
  req
    "ListControlDomainInsights"
    "fixture/ListControlDomainInsights.yaml"

requestListControlDomainInsightsByAssessment :: ListControlDomainInsightsByAssessment -> TestTree
requestListControlDomainInsightsByAssessment =
  req
    "ListControlDomainInsightsByAssessment"
    "fixture/ListControlDomainInsightsByAssessment.yaml"

requestListControlInsightsByControlDomain :: ListControlInsightsByControlDomain -> TestTree
requestListControlInsightsByControlDomain =
  req
    "ListControlInsightsByControlDomain"
    "fixture/ListControlInsightsByControlDomain.yaml"

requestListControls :: ListControls -> TestTree
requestListControls =
  req
    "ListControls"
    "fixture/ListControls.yaml"

requestListKeywordsForDataSource :: ListKeywordsForDataSource -> TestTree
requestListKeywordsForDataSource =
  req
    "ListKeywordsForDataSource"
    "fixture/ListKeywordsForDataSource.yaml"

requestListNotifications :: ListNotifications -> TestTree
requestListNotifications =
  req
    "ListNotifications"
    "fixture/ListNotifications.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRegisterAccount :: RegisterAccount -> TestTree
requestRegisterAccount =
  req
    "RegisterAccount"
    "fixture/RegisterAccount.yaml"

requestRegisterOrganizationAdminAccount :: RegisterOrganizationAdminAccount -> TestTree
requestRegisterOrganizationAdminAccount =
  req
    "RegisterOrganizationAdminAccount"
    "fixture/RegisterOrganizationAdminAccount.yaml"

requestStartAssessmentFrameworkShare :: StartAssessmentFrameworkShare -> TestTree
requestStartAssessmentFrameworkShare =
  req
    "StartAssessmentFrameworkShare"
    "fixture/StartAssessmentFrameworkShare.yaml"

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

requestUpdateAssessment :: UpdateAssessment -> TestTree
requestUpdateAssessment =
  req
    "UpdateAssessment"
    "fixture/UpdateAssessment.yaml"

requestUpdateAssessmentControl :: UpdateAssessmentControl -> TestTree
requestUpdateAssessmentControl =
  req
    "UpdateAssessmentControl"
    "fixture/UpdateAssessmentControl.yaml"

requestUpdateAssessmentControlSetStatus :: UpdateAssessmentControlSetStatus -> TestTree
requestUpdateAssessmentControlSetStatus =
  req
    "UpdateAssessmentControlSetStatus"
    "fixture/UpdateAssessmentControlSetStatus.yaml"

requestUpdateAssessmentFramework :: UpdateAssessmentFramework -> TestTree
requestUpdateAssessmentFramework =
  req
    "UpdateAssessmentFramework"
    "fixture/UpdateAssessmentFramework.yaml"

requestUpdateAssessmentFrameworkShare :: UpdateAssessmentFrameworkShare -> TestTree
requestUpdateAssessmentFrameworkShare =
  req
    "UpdateAssessmentFrameworkShare"
    "fixture/UpdateAssessmentFrameworkShare.yaml"

requestUpdateAssessmentStatus :: UpdateAssessmentStatus -> TestTree
requestUpdateAssessmentStatus =
  req
    "UpdateAssessmentStatus"
    "fixture/UpdateAssessmentStatus.yaml"

requestUpdateControl :: UpdateControl -> TestTree
requestUpdateControl =
  req
    "UpdateControl"
    "fixture/UpdateControl.yaml"

requestUpdateSettings :: UpdateSettings -> TestTree
requestUpdateSettings =
  req
    "UpdateSettings"
    "fixture/UpdateSettings.yaml"

requestValidateAssessmentReportIntegrity :: ValidateAssessmentReportIntegrity -> TestTree
requestValidateAssessmentReportIntegrity =
  req
    "ValidateAssessmentReportIntegrity"
    "fixture/ValidateAssessmentReportIntegrity.yaml"

-- Responses

responseAssociateAssessmentReportEvidenceFolder :: AssociateAssessmentReportEvidenceFolderResponse -> TestTree
responseAssociateAssessmentReportEvidenceFolder =
  res
    "AssociateAssessmentReportEvidenceFolderResponse"
    "fixture/AssociateAssessmentReportEvidenceFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAssessmentReportEvidenceFolder)

responseBatchAssociateAssessmentReportEvidence :: BatchAssociateAssessmentReportEvidenceResponse -> TestTree
responseBatchAssociateAssessmentReportEvidence =
  res
    "BatchAssociateAssessmentReportEvidenceResponse"
    "fixture/BatchAssociateAssessmentReportEvidenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateAssessmentReportEvidence)

responseBatchCreateDelegationByAssessment :: BatchCreateDelegationByAssessmentResponse -> TestTree
responseBatchCreateDelegationByAssessment =
  res
    "BatchCreateDelegationByAssessmentResponse"
    "fixture/BatchCreateDelegationByAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateDelegationByAssessment)

responseBatchDeleteDelegationByAssessment :: BatchDeleteDelegationByAssessmentResponse -> TestTree
responseBatchDeleteDelegationByAssessment =
  res
    "BatchDeleteDelegationByAssessmentResponse"
    "fixture/BatchDeleteDelegationByAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteDelegationByAssessment)

responseBatchDisassociateAssessmentReportEvidence :: BatchDisassociateAssessmentReportEvidenceResponse -> TestTree
responseBatchDisassociateAssessmentReportEvidence =
  res
    "BatchDisassociateAssessmentReportEvidenceResponse"
    "fixture/BatchDisassociateAssessmentReportEvidenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateAssessmentReportEvidence)

responseBatchImportEvidenceToAssessmentControl :: BatchImportEvidenceToAssessmentControlResponse -> TestTree
responseBatchImportEvidenceToAssessmentControl =
  res
    "BatchImportEvidenceToAssessmentControlResponse"
    "fixture/BatchImportEvidenceToAssessmentControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchImportEvidenceToAssessmentControl)

responseCreateAssessment :: CreateAssessmentResponse -> TestTree
responseCreateAssessment =
  res
    "CreateAssessmentResponse"
    "fixture/CreateAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssessment)

responseCreateAssessmentFramework :: CreateAssessmentFrameworkResponse -> TestTree
responseCreateAssessmentFramework =
  res
    "CreateAssessmentFrameworkResponse"
    "fixture/CreateAssessmentFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssessmentFramework)

responseCreateAssessmentReport :: CreateAssessmentReportResponse -> TestTree
responseCreateAssessmentReport =
  res
    "CreateAssessmentReportResponse"
    "fixture/CreateAssessmentReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssessmentReport)

responseCreateControl :: CreateControlResponse -> TestTree
responseCreateControl =
  res
    "CreateControlResponse"
    "fixture/CreateControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateControl)

responseDeleteAssessment :: DeleteAssessmentResponse -> TestTree
responseDeleteAssessment =
  res
    "DeleteAssessmentResponse"
    "fixture/DeleteAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssessment)

responseDeleteAssessmentFramework :: DeleteAssessmentFrameworkResponse -> TestTree
responseDeleteAssessmentFramework =
  res
    "DeleteAssessmentFrameworkResponse"
    "fixture/DeleteAssessmentFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssessmentFramework)

responseDeleteAssessmentFrameworkShare :: DeleteAssessmentFrameworkShareResponse -> TestTree
responseDeleteAssessmentFrameworkShare =
  res
    "DeleteAssessmentFrameworkShareResponse"
    "fixture/DeleteAssessmentFrameworkShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssessmentFrameworkShare)

responseDeleteAssessmentReport :: DeleteAssessmentReportResponse -> TestTree
responseDeleteAssessmentReport =
  res
    "DeleteAssessmentReportResponse"
    "fixture/DeleteAssessmentReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssessmentReport)

responseDeleteControl :: DeleteControlResponse -> TestTree
responseDeleteControl =
  res
    "DeleteControlResponse"
    "fixture/DeleteControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteControl)

responseDeregisterAccount :: DeregisterAccountResponse -> TestTree
responseDeregisterAccount =
  res
    "DeregisterAccountResponse"
    "fixture/DeregisterAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterAccount)

responseDeregisterOrganizationAdminAccount :: DeregisterOrganizationAdminAccountResponse -> TestTree
responseDeregisterOrganizationAdminAccount =
  res
    "DeregisterOrganizationAdminAccountResponse"
    "fixture/DeregisterOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterOrganizationAdminAccount)

responseDisassociateAssessmentReportEvidenceFolder :: DisassociateAssessmentReportEvidenceFolderResponse -> TestTree
responseDisassociateAssessmentReportEvidenceFolder =
  res
    "DisassociateAssessmentReportEvidenceFolderResponse"
    "fixture/DisassociateAssessmentReportEvidenceFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAssessmentReportEvidenceFolder)

responseGetAccountStatus :: GetAccountStatusResponse -> TestTree
responseGetAccountStatus =
  res
    "GetAccountStatusResponse"
    "fixture/GetAccountStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountStatus)

responseGetAssessment :: GetAssessmentResponse -> TestTree
responseGetAssessment =
  res
    "GetAssessmentResponse"
    "fixture/GetAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssessment)

responseGetAssessmentFramework :: GetAssessmentFrameworkResponse -> TestTree
responseGetAssessmentFramework =
  res
    "GetAssessmentFrameworkResponse"
    "fixture/GetAssessmentFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssessmentFramework)

responseGetAssessmentReportUrl :: GetAssessmentReportUrlResponse -> TestTree
responseGetAssessmentReportUrl =
  res
    "GetAssessmentReportUrlResponse"
    "fixture/GetAssessmentReportUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssessmentReportUrl)

responseGetChangeLogs :: GetChangeLogsResponse -> TestTree
responseGetChangeLogs =
  res
    "GetChangeLogsResponse"
    "fixture/GetChangeLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChangeLogs)

responseGetControl :: GetControlResponse -> TestTree
responseGetControl =
  res
    "GetControlResponse"
    "fixture/GetControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetControl)

responseGetDelegations :: GetDelegationsResponse -> TestTree
responseGetDelegations =
  res
    "GetDelegationsResponse"
    "fixture/GetDelegationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDelegations)

responseGetEvidence :: GetEvidenceResponse -> TestTree
responseGetEvidence =
  res
    "GetEvidenceResponse"
    "fixture/GetEvidenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvidence)

responseGetEvidenceByEvidenceFolder :: GetEvidenceByEvidenceFolderResponse -> TestTree
responseGetEvidenceByEvidenceFolder =
  res
    "GetEvidenceByEvidenceFolderResponse"
    "fixture/GetEvidenceByEvidenceFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvidenceByEvidenceFolder)

responseGetEvidenceFolder :: GetEvidenceFolderResponse -> TestTree
responseGetEvidenceFolder =
  res
    "GetEvidenceFolderResponse"
    "fixture/GetEvidenceFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvidenceFolder)

responseGetEvidenceFoldersByAssessment :: GetEvidenceFoldersByAssessmentResponse -> TestTree
responseGetEvidenceFoldersByAssessment =
  res
    "GetEvidenceFoldersByAssessmentResponse"
    "fixture/GetEvidenceFoldersByAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvidenceFoldersByAssessment)

responseGetEvidenceFoldersByAssessmentControl :: GetEvidenceFoldersByAssessmentControlResponse -> TestTree
responseGetEvidenceFoldersByAssessmentControl =
  res
    "GetEvidenceFoldersByAssessmentControlResponse"
    "fixture/GetEvidenceFoldersByAssessmentControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvidenceFoldersByAssessmentControl)

responseGetInsights :: GetInsightsResponse -> TestTree
responseGetInsights =
  res
    "GetInsightsResponse"
    "fixture/GetInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsights)

responseGetInsightsByAssessment :: GetInsightsByAssessmentResponse -> TestTree
responseGetInsightsByAssessment =
  res
    "GetInsightsByAssessmentResponse"
    "fixture/GetInsightsByAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsightsByAssessment)

responseGetOrganizationAdminAccount :: GetOrganizationAdminAccountResponse -> TestTree
responseGetOrganizationAdminAccount =
  res
    "GetOrganizationAdminAccountResponse"
    "fixture/GetOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOrganizationAdminAccount)

responseGetServicesInScope :: GetServicesInScopeResponse -> TestTree
responseGetServicesInScope =
  res
    "GetServicesInScopeResponse"
    "fixture/GetServicesInScopeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServicesInScope)

responseGetSettings :: GetSettingsResponse -> TestTree
responseGetSettings =
  res
    "GetSettingsResponse"
    "fixture/GetSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSettings)

responseListAssessmentControlInsightsByControlDomain :: ListAssessmentControlInsightsByControlDomainResponse -> TestTree
responseListAssessmentControlInsightsByControlDomain =
  res
    "ListAssessmentControlInsightsByControlDomainResponse"
    "fixture/ListAssessmentControlInsightsByControlDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessmentControlInsightsByControlDomain)

responseListAssessmentFrameworkShareRequests :: ListAssessmentFrameworkShareRequestsResponse -> TestTree
responseListAssessmentFrameworkShareRequests =
  res
    "ListAssessmentFrameworkShareRequestsResponse"
    "fixture/ListAssessmentFrameworkShareRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessmentFrameworkShareRequests)

responseListAssessmentFrameworks :: ListAssessmentFrameworksResponse -> TestTree
responseListAssessmentFrameworks =
  res
    "ListAssessmentFrameworksResponse"
    "fixture/ListAssessmentFrameworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessmentFrameworks)

responseListAssessmentReports :: ListAssessmentReportsResponse -> TestTree
responseListAssessmentReports =
  res
    "ListAssessmentReportsResponse"
    "fixture/ListAssessmentReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessmentReports)

responseListAssessments :: ListAssessmentsResponse -> TestTree
responseListAssessments =
  res
    "ListAssessmentsResponse"
    "fixture/ListAssessmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssessments)

responseListControlDomainInsights :: ListControlDomainInsightsResponse -> TestTree
responseListControlDomainInsights =
  res
    "ListControlDomainInsightsResponse"
    "fixture/ListControlDomainInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListControlDomainInsights)

responseListControlDomainInsightsByAssessment :: ListControlDomainInsightsByAssessmentResponse -> TestTree
responseListControlDomainInsightsByAssessment =
  res
    "ListControlDomainInsightsByAssessmentResponse"
    "fixture/ListControlDomainInsightsByAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListControlDomainInsightsByAssessment)

responseListControlInsightsByControlDomain :: ListControlInsightsByControlDomainResponse -> TestTree
responseListControlInsightsByControlDomain =
  res
    "ListControlInsightsByControlDomainResponse"
    "fixture/ListControlInsightsByControlDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListControlInsightsByControlDomain)

responseListControls :: ListControlsResponse -> TestTree
responseListControls =
  res
    "ListControlsResponse"
    "fixture/ListControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListControls)

responseListKeywordsForDataSource :: ListKeywordsForDataSourceResponse -> TestTree
responseListKeywordsForDataSource =
  res
    "ListKeywordsForDataSourceResponse"
    "fixture/ListKeywordsForDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeywordsForDataSource)

responseListNotifications :: ListNotificationsResponse -> TestTree
responseListNotifications =
  res
    "ListNotificationsResponse"
    "fixture/ListNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotifications)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRegisterAccount :: RegisterAccountResponse -> TestTree
responseRegisterAccount =
  res
    "RegisterAccountResponse"
    "fixture/RegisterAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterAccount)

responseRegisterOrganizationAdminAccount :: RegisterOrganizationAdminAccountResponse -> TestTree
responseRegisterOrganizationAdminAccount =
  res
    "RegisterOrganizationAdminAccountResponse"
    "fixture/RegisterOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterOrganizationAdminAccount)

responseStartAssessmentFrameworkShare :: StartAssessmentFrameworkShareResponse -> TestTree
responseStartAssessmentFrameworkShare =
  res
    "StartAssessmentFrameworkShareResponse"
    "fixture/StartAssessmentFrameworkShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAssessmentFrameworkShare)

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

responseUpdateAssessment :: UpdateAssessmentResponse -> TestTree
responseUpdateAssessment =
  res
    "UpdateAssessmentResponse"
    "fixture/UpdateAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessment)

responseUpdateAssessmentControl :: UpdateAssessmentControlResponse -> TestTree
responseUpdateAssessmentControl =
  res
    "UpdateAssessmentControlResponse"
    "fixture/UpdateAssessmentControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessmentControl)

responseUpdateAssessmentControlSetStatus :: UpdateAssessmentControlSetStatusResponse -> TestTree
responseUpdateAssessmentControlSetStatus =
  res
    "UpdateAssessmentControlSetStatusResponse"
    "fixture/UpdateAssessmentControlSetStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessmentControlSetStatus)

responseUpdateAssessmentFramework :: UpdateAssessmentFrameworkResponse -> TestTree
responseUpdateAssessmentFramework =
  res
    "UpdateAssessmentFrameworkResponse"
    "fixture/UpdateAssessmentFrameworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessmentFramework)

responseUpdateAssessmentFrameworkShare :: UpdateAssessmentFrameworkShareResponse -> TestTree
responseUpdateAssessmentFrameworkShare =
  res
    "UpdateAssessmentFrameworkShareResponse"
    "fixture/UpdateAssessmentFrameworkShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessmentFrameworkShare)

responseUpdateAssessmentStatus :: UpdateAssessmentStatusResponse -> TestTree
responseUpdateAssessmentStatus =
  res
    "UpdateAssessmentStatusResponse"
    "fixture/UpdateAssessmentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssessmentStatus)

responseUpdateControl :: UpdateControlResponse -> TestTree
responseUpdateControl =
  res
    "UpdateControlResponse"
    "fixture/UpdateControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateControl)

responseUpdateSettings :: UpdateSettingsResponse -> TestTree
responseUpdateSettings =
  res
    "UpdateSettingsResponse"
    "fixture/UpdateSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSettings)

responseValidateAssessmentReportIntegrity :: ValidateAssessmentReportIntegrityResponse -> TestTree
responseValidateAssessmentReportIntegrity =
  res
    "ValidateAssessmentReportIntegrityResponse"
    "fixture/ValidateAssessmentReportIntegrityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateAssessmentReportIntegrity)
