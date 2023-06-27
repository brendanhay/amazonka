{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Inspector2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Inspector2 where

import Amazonka.Inspector2
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Inspector2.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateMember $
--             newAssociateMember
--
--         , requestBatchGetAccountStatus $
--             newBatchGetAccountStatus
--
--         , requestBatchGetCodeSnippet $
--             newBatchGetCodeSnippet
--
--         , requestBatchGetFreeTrialInfo $
--             newBatchGetFreeTrialInfo
--
--         , requestBatchGetMemberEc2DeepInspectionStatus $
--             newBatchGetMemberEc2DeepInspectionStatus
--
--         , requestBatchUpdateMemberEc2DeepInspectionStatus $
--             newBatchUpdateMemberEc2DeepInspectionStatus
--
--         , requestCancelFindingsReport $
--             newCancelFindingsReport
--
--         , requestCancelSbomExport $
--             newCancelSbomExport
--
--         , requestCreateFilter $
--             newCreateFilter
--
--         , requestCreateFindingsReport $
--             newCreateFindingsReport
--
--         , requestCreateSbomExport $
--             newCreateSbomExport
--
--         , requestDeleteFilter $
--             newDeleteFilter
--
--         , requestDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfiguration
--
--         , requestDisable $
--             newDisable
--
--         , requestDisableDelegatedAdminAccount $
--             newDisableDelegatedAdminAccount
--
--         , requestDisassociateMember $
--             newDisassociateMember
--
--         , requestEnable $
--             newEnable
--
--         , requestEnableDelegatedAdminAccount $
--             newEnableDelegatedAdminAccount
--
--         , requestGetConfiguration $
--             newGetConfiguration
--
--         , requestGetDelegatedAdminAccount $
--             newGetDelegatedAdminAccount
--
--         , requestGetEc2DeepInspectionConfiguration $
--             newGetEc2DeepInspectionConfiguration
--
--         , requestGetEncryptionKey $
--             newGetEncryptionKey
--
--         , requestGetFindingsReportStatus $
--             newGetFindingsReportStatus
--
--         , requestGetMember $
--             newGetMember
--
--         , requestGetSbomExport $
--             newGetSbomExport
--
--         , requestListAccountPermissions $
--             newListAccountPermissions
--
--         , requestListCoverage $
--             newListCoverage
--
--         , requestListCoverageStatistics $
--             newListCoverageStatistics
--
--         , requestListDelegatedAdminAccounts $
--             newListDelegatedAdminAccounts
--
--         , requestListFilters $
--             newListFilters
--
--         , requestListFindingAggregations $
--             newListFindingAggregations
--
--         , requestListFindings $
--             newListFindings
--
--         , requestListMembers $
--             newListMembers
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListUsageTotals $
--             newListUsageTotals
--
--         , requestResetEncryptionKey $
--             newResetEncryptionKey
--
--         , requestSearchVulnerabilities $
--             newSearchVulnerabilities
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateConfiguration $
--             newUpdateConfiguration
--
--         , requestUpdateEc2DeepInspectionConfiguration $
--             newUpdateEc2DeepInspectionConfiguration
--
--         , requestUpdateEncryptionKey $
--             newUpdateEncryptionKey
--
--         , requestUpdateFilter $
--             newUpdateFilter
--
--         , requestUpdateOrgEc2DeepInspectionConfiguration $
--             newUpdateOrgEc2DeepInspectionConfiguration
--
--         , requestUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseAssociateMember $
--             newAssociateMemberResponse
--
--         , responseBatchGetAccountStatus $
--             newBatchGetAccountStatusResponse
--
--         , responseBatchGetCodeSnippet $
--             newBatchGetCodeSnippetResponse
--
--         , responseBatchGetFreeTrialInfo $
--             newBatchGetFreeTrialInfoResponse
--
--         , responseBatchGetMemberEc2DeepInspectionStatus $
--             newBatchGetMemberEc2DeepInspectionStatusResponse
--
--         , responseBatchUpdateMemberEc2DeepInspectionStatus $
--             newBatchUpdateMemberEc2DeepInspectionStatusResponse
--
--         , responseCancelFindingsReport $
--             newCancelFindingsReportResponse
--
--         , responseCancelSbomExport $
--             newCancelSbomExportResponse
--
--         , responseCreateFilter $
--             newCreateFilterResponse
--
--         , responseCreateFindingsReport $
--             newCreateFindingsReportResponse
--
--         , responseCreateSbomExport $
--             newCreateSbomExportResponse
--
--         , responseDeleteFilter $
--             newDeleteFilterResponse
--
--         , responseDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfigurationResponse
--
--         , responseDisable $
--             newDisableResponse
--
--         , responseDisableDelegatedAdminAccount $
--             newDisableDelegatedAdminAccountResponse
--
--         , responseDisassociateMember $
--             newDisassociateMemberResponse
--
--         , responseEnable $
--             newEnableResponse
--
--         , responseEnableDelegatedAdminAccount $
--             newEnableDelegatedAdminAccountResponse
--
--         , responseGetConfiguration $
--             newGetConfigurationResponse
--
--         , responseGetDelegatedAdminAccount $
--             newGetDelegatedAdminAccountResponse
--
--         , responseGetEc2DeepInspectionConfiguration $
--             newGetEc2DeepInspectionConfigurationResponse
--
--         , responseGetEncryptionKey $
--             newGetEncryptionKeyResponse
--
--         , responseGetFindingsReportStatus $
--             newGetFindingsReportStatusResponse
--
--         , responseGetMember $
--             newGetMemberResponse
--
--         , responseGetSbomExport $
--             newGetSbomExportResponse
--
--         , responseListAccountPermissions $
--             newListAccountPermissionsResponse
--
--         , responseListCoverage $
--             newListCoverageResponse
--
--         , responseListCoverageStatistics $
--             newListCoverageStatisticsResponse
--
--         , responseListDelegatedAdminAccounts $
--             newListDelegatedAdminAccountsResponse
--
--         , responseListFilters $
--             newListFiltersResponse
--
--         , responseListFindingAggregations $
--             newListFindingAggregationsResponse
--
--         , responseListFindings $
--             newListFindingsResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListUsageTotals $
--             newListUsageTotalsResponse
--
--         , responseResetEncryptionKey $
--             newResetEncryptionKeyResponse
--
--         , responseSearchVulnerabilities $
--             newSearchVulnerabilitiesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateConfiguration $
--             newUpdateConfigurationResponse
--
--         , responseUpdateEc2DeepInspectionConfiguration $
--             newUpdateEc2DeepInspectionConfigurationResponse
--
--         , responseUpdateEncryptionKey $
--             newUpdateEncryptionKeyResponse
--
--         , responseUpdateFilter $
--             newUpdateFilterResponse
--
--         , responseUpdateOrgEc2DeepInspectionConfiguration $
--             newUpdateOrgEc2DeepInspectionConfigurationResponse
--
--         , responseUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfigurationResponse
--
--           ]
--     ]

-- Requests

requestAssociateMember :: AssociateMember -> TestTree
requestAssociateMember =
  req
    "AssociateMember"
    "fixture/AssociateMember.yaml"

requestBatchGetAccountStatus :: BatchGetAccountStatus -> TestTree
requestBatchGetAccountStatus =
  req
    "BatchGetAccountStatus"
    "fixture/BatchGetAccountStatus.yaml"

requestBatchGetCodeSnippet :: BatchGetCodeSnippet -> TestTree
requestBatchGetCodeSnippet =
  req
    "BatchGetCodeSnippet"
    "fixture/BatchGetCodeSnippet.yaml"

requestBatchGetFreeTrialInfo :: BatchGetFreeTrialInfo -> TestTree
requestBatchGetFreeTrialInfo =
  req
    "BatchGetFreeTrialInfo"
    "fixture/BatchGetFreeTrialInfo.yaml"

requestBatchGetMemberEc2DeepInspectionStatus :: BatchGetMemberEc2DeepInspectionStatus -> TestTree
requestBatchGetMemberEc2DeepInspectionStatus =
  req
    "BatchGetMemberEc2DeepInspectionStatus"
    "fixture/BatchGetMemberEc2DeepInspectionStatus.yaml"

requestBatchUpdateMemberEc2DeepInspectionStatus :: BatchUpdateMemberEc2DeepInspectionStatus -> TestTree
requestBatchUpdateMemberEc2DeepInspectionStatus =
  req
    "BatchUpdateMemberEc2DeepInspectionStatus"
    "fixture/BatchUpdateMemberEc2DeepInspectionStatus.yaml"

requestCancelFindingsReport :: CancelFindingsReport -> TestTree
requestCancelFindingsReport =
  req
    "CancelFindingsReport"
    "fixture/CancelFindingsReport.yaml"

requestCancelSbomExport :: CancelSbomExport -> TestTree
requestCancelSbomExport =
  req
    "CancelSbomExport"
    "fixture/CancelSbomExport.yaml"

requestCreateFilter :: CreateFilter -> TestTree
requestCreateFilter =
  req
    "CreateFilter"
    "fixture/CreateFilter.yaml"

requestCreateFindingsReport :: CreateFindingsReport -> TestTree
requestCreateFindingsReport =
  req
    "CreateFindingsReport"
    "fixture/CreateFindingsReport.yaml"

requestCreateSbomExport :: CreateSbomExport -> TestTree
requestCreateSbomExport =
  req
    "CreateSbomExport"
    "fixture/CreateSbomExport.yaml"

requestDeleteFilter :: DeleteFilter -> TestTree
requestDeleteFilter =
  req
    "DeleteFilter"
    "fixture/DeleteFilter.yaml"

requestDescribeOrganizationConfiguration :: DescribeOrganizationConfiguration -> TestTree
requestDescribeOrganizationConfiguration =
  req
    "DescribeOrganizationConfiguration"
    "fixture/DescribeOrganizationConfiguration.yaml"

requestDisable :: Disable -> TestTree
requestDisable =
  req
    "Disable"
    "fixture/Disable.yaml"

requestDisableDelegatedAdminAccount :: DisableDelegatedAdminAccount -> TestTree
requestDisableDelegatedAdminAccount =
  req
    "DisableDelegatedAdminAccount"
    "fixture/DisableDelegatedAdminAccount.yaml"

requestDisassociateMember :: DisassociateMember -> TestTree
requestDisassociateMember =
  req
    "DisassociateMember"
    "fixture/DisassociateMember.yaml"

requestEnable :: Enable -> TestTree
requestEnable =
  req
    "Enable"
    "fixture/Enable.yaml"

requestEnableDelegatedAdminAccount :: EnableDelegatedAdminAccount -> TestTree
requestEnableDelegatedAdminAccount =
  req
    "EnableDelegatedAdminAccount"
    "fixture/EnableDelegatedAdminAccount.yaml"

requestGetConfiguration :: GetConfiguration -> TestTree
requestGetConfiguration =
  req
    "GetConfiguration"
    "fixture/GetConfiguration.yaml"

requestGetDelegatedAdminAccount :: GetDelegatedAdminAccount -> TestTree
requestGetDelegatedAdminAccount =
  req
    "GetDelegatedAdminAccount"
    "fixture/GetDelegatedAdminAccount.yaml"

requestGetEc2DeepInspectionConfiguration :: GetEc2DeepInspectionConfiguration -> TestTree
requestGetEc2DeepInspectionConfiguration =
  req
    "GetEc2DeepInspectionConfiguration"
    "fixture/GetEc2DeepInspectionConfiguration.yaml"

requestGetEncryptionKey :: GetEncryptionKey -> TestTree
requestGetEncryptionKey =
  req
    "GetEncryptionKey"
    "fixture/GetEncryptionKey.yaml"

requestGetFindingsReportStatus :: GetFindingsReportStatus -> TestTree
requestGetFindingsReportStatus =
  req
    "GetFindingsReportStatus"
    "fixture/GetFindingsReportStatus.yaml"

requestGetMember :: GetMember -> TestTree
requestGetMember =
  req
    "GetMember"
    "fixture/GetMember.yaml"

requestGetSbomExport :: GetSbomExport -> TestTree
requestGetSbomExport =
  req
    "GetSbomExport"
    "fixture/GetSbomExport.yaml"

requestListAccountPermissions :: ListAccountPermissions -> TestTree
requestListAccountPermissions =
  req
    "ListAccountPermissions"
    "fixture/ListAccountPermissions.yaml"

requestListCoverage :: ListCoverage -> TestTree
requestListCoverage =
  req
    "ListCoverage"
    "fixture/ListCoverage.yaml"

requestListCoverageStatistics :: ListCoverageStatistics -> TestTree
requestListCoverageStatistics =
  req
    "ListCoverageStatistics"
    "fixture/ListCoverageStatistics.yaml"

requestListDelegatedAdminAccounts :: ListDelegatedAdminAccounts -> TestTree
requestListDelegatedAdminAccounts =
  req
    "ListDelegatedAdminAccounts"
    "fixture/ListDelegatedAdminAccounts.yaml"

requestListFilters :: ListFilters -> TestTree
requestListFilters =
  req
    "ListFilters"
    "fixture/ListFilters.yaml"

requestListFindingAggregations :: ListFindingAggregations -> TestTree
requestListFindingAggregations =
  req
    "ListFindingAggregations"
    "fixture/ListFindingAggregations.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings =
  req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers =
  req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListUsageTotals :: ListUsageTotals -> TestTree
requestListUsageTotals =
  req
    "ListUsageTotals"
    "fixture/ListUsageTotals.yaml"

requestResetEncryptionKey :: ResetEncryptionKey -> TestTree
requestResetEncryptionKey =
  req
    "ResetEncryptionKey"
    "fixture/ResetEncryptionKey.yaml"

requestSearchVulnerabilities :: SearchVulnerabilities -> TestTree
requestSearchVulnerabilities =
  req
    "SearchVulnerabilities"
    "fixture/SearchVulnerabilities.yaml"

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

requestUpdateConfiguration :: UpdateConfiguration -> TestTree
requestUpdateConfiguration =
  req
    "UpdateConfiguration"
    "fixture/UpdateConfiguration.yaml"

requestUpdateEc2DeepInspectionConfiguration :: UpdateEc2DeepInspectionConfiguration -> TestTree
requestUpdateEc2DeepInspectionConfiguration =
  req
    "UpdateEc2DeepInspectionConfiguration"
    "fixture/UpdateEc2DeepInspectionConfiguration.yaml"

requestUpdateEncryptionKey :: UpdateEncryptionKey -> TestTree
requestUpdateEncryptionKey =
  req
    "UpdateEncryptionKey"
    "fixture/UpdateEncryptionKey.yaml"

requestUpdateFilter :: UpdateFilter -> TestTree
requestUpdateFilter =
  req
    "UpdateFilter"
    "fixture/UpdateFilter.yaml"

requestUpdateOrgEc2DeepInspectionConfiguration :: UpdateOrgEc2DeepInspectionConfiguration -> TestTree
requestUpdateOrgEc2DeepInspectionConfiguration =
  req
    "UpdateOrgEc2DeepInspectionConfiguration"
    "fixture/UpdateOrgEc2DeepInspectionConfiguration.yaml"

requestUpdateOrganizationConfiguration :: UpdateOrganizationConfiguration -> TestTree
requestUpdateOrganizationConfiguration =
  req
    "UpdateOrganizationConfiguration"
    "fixture/UpdateOrganizationConfiguration.yaml"

-- Responses

responseAssociateMember :: AssociateMemberResponse -> TestTree
responseAssociateMember =
  res
    "AssociateMemberResponse"
    "fixture/AssociateMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateMember)

responseBatchGetAccountStatus :: BatchGetAccountStatusResponse -> TestTree
responseBatchGetAccountStatus =
  res
    "BatchGetAccountStatusResponse"
    "fixture/BatchGetAccountStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetAccountStatus)

responseBatchGetCodeSnippet :: BatchGetCodeSnippetResponse -> TestTree
responseBatchGetCodeSnippet =
  res
    "BatchGetCodeSnippetResponse"
    "fixture/BatchGetCodeSnippetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetCodeSnippet)

responseBatchGetFreeTrialInfo :: BatchGetFreeTrialInfoResponse -> TestTree
responseBatchGetFreeTrialInfo =
  res
    "BatchGetFreeTrialInfoResponse"
    "fixture/BatchGetFreeTrialInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetFreeTrialInfo)

responseBatchGetMemberEc2DeepInspectionStatus :: BatchGetMemberEc2DeepInspectionStatusResponse -> TestTree
responseBatchGetMemberEc2DeepInspectionStatus =
  res
    "BatchGetMemberEc2DeepInspectionStatusResponse"
    "fixture/BatchGetMemberEc2DeepInspectionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetMemberEc2DeepInspectionStatus)

responseBatchUpdateMemberEc2DeepInspectionStatus :: BatchUpdateMemberEc2DeepInspectionStatusResponse -> TestTree
responseBatchUpdateMemberEc2DeepInspectionStatus =
  res
    "BatchUpdateMemberEc2DeepInspectionStatusResponse"
    "fixture/BatchUpdateMemberEc2DeepInspectionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateMemberEc2DeepInspectionStatus)

responseCancelFindingsReport :: CancelFindingsReportResponse -> TestTree
responseCancelFindingsReport =
  res
    "CancelFindingsReportResponse"
    "fixture/CancelFindingsReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelFindingsReport)

responseCancelSbomExport :: CancelSbomExportResponse -> TestTree
responseCancelSbomExport =
  res
    "CancelSbomExportResponse"
    "fixture/CancelSbomExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSbomExport)

responseCreateFilter :: CreateFilterResponse -> TestTree
responseCreateFilter =
  res
    "CreateFilterResponse"
    "fixture/CreateFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFilter)

responseCreateFindingsReport :: CreateFindingsReportResponse -> TestTree
responseCreateFindingsReport =
  res
    "CreateFindingsReportResponse"
    "fixture/CreateFindingsReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFindingsReport)

responseCreateSbomExport :: CreateSbomExportResponse -> TestTree
responseCreateSbomExport =
  res
    "CreateSbomExportResponse"
    "fixture/CreateSbomExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSbomExport)

responseDeleteFilter :: DeleteFilterResponse -> TestTree
responseDeleteFilter =
  res
    "DeleteFilterResponse"
    "fixture/DeleteFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFilter)

responseDescribeOrganizationConfiguration :: DescribeOrganizationConfigurationResponse -> TestTree
responseDescribeOrganizationConfiguration =
  res
    "DescribeOrganizationConfigurationResponse"
    "fixture/DescribeOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfiguration)

responseDisable :: DisableResponse -> TestTree
responseDisable =
  res
    "DisableResponse"
    "fixture/DisableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Disable)

responseDisableDelegatedAdminAccount :: DisableDelegatedAdminAccountResponse -> TestTree
responseDisableDelegatedAdminAccount =
  res
    "DisableDelegatedAdminAccountResponse"
    "fixture/DisableDelegatedAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableDelegatedAdminAccount)

responseDisassociateMember :: DisassociateMemberResponse -> TestTree
responseDisassociateMember =
  res
    "DisassociateMemberResponse"
    "fixture/DisassociateMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMember)

responseEnable :: EnableResponse -> TestTree
responseEnable =
  res
    "EnableResponse"
    "fixture/EnableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Enable)

responseEnableDelegatedAdminAccount :: EnableDelegatedAdminAccountResponse -> TestTree
responseEnableDelegatedAdminAccount =
  res
    "EnableDelegatedAdminAccountResponse"
    "fixture/EnableDelegatedAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableDelegatedAdminAccount)

responseGetConfiguration :: GetConfigurationResponse -> TestTree
responseGetConfiguration =
  res
    "GetConfigurationResponse"
    "fixture/GetConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfiguration)

responseGetDelegatedAdminAccount :: GetDelegatedAdminAccountResponse -> TestTree
responseGetDelegatedAdminAccount =
  res
    "GetDelegatedAdminAccountResponse"
    "fixture/GetDelegatedAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDelegatedAdminAccount)

responseGetEc2DeepInspectionConfiguration :: GetEc2DeepInspectionConfigurationResponse -> TestTree
responseGetEc2DeepInspectionConfiguration =
  res
    "GetEc2DeepInspectionConfigurationResponse"
    "fixture/GetEc2DeepInspectionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEc2DeepInspectionConfiguration)

responseGetEncryptionKey :: GetEncryptionKeyResponse -> TestTree
responseGetEncryptionKey =
  res
    "GetEncryptionKeyResponse"
    "fixture/GetEncryptionKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEncryptionKey)

responseGetFindingsReportStatus :: GetFindingsReportStatusResponse -> TestTree
responseGetFindingsReportStatus =
  res
    "GetFindingsReportStatusResponse"
    "fixture/GetFindingsReportStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingsReportStatus)

responseGetMember :: GetMemberResponse -> TestTree
responseGetMember =
  res
    "GetMemberResponse"
    "fixture/GetMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMember)

responseGetSbomExport :: GetSbomExportResponse -> TestTree
responseGetSbomExport =
  res
    "GetSbomExportResponse"
    "fixture/GetSbomExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSbomExport)

responseListAccountPermissions :: ListAccountPermissionsResponse -> TestTree
responseListAccountPermissions =
  res
    "ListAccountPermissionsResponse"
    "fixture/ListAccountPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountPermissions)

responseListCoverage :: ListCoverageResponse -> TestTree
responseListCoverage =
  res
    "ListCoverageResponse"
    "fixture/ListCoverageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCoverage)

responseListCoverageStatistics :: ListCoverageStatisticsResponse -> TestTree
responseListCoverageStatistics =
  res
    "ListCoverageStatisticsResponse"
    "fixture/ListCoverageStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCoverageStatistics)

responseListDelegatedAdminAccounts :: ListDelegatedAdminAccountsResponse -> TestTree
responseListDelegatedAdminAccounts =
  res
    "ListDelegatedAdminAccountsResponse"
    "fixture/ListDelegatedAdminAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDelegatedAdminAccounts)

responseListFilters :: ListFiltersResponse -> TestTree
responseListFilters =
  res
    "ListFiltersResponse"
    "fixture/ListFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFilters)

responseListFindingAggregations :: ListFindingAggregationsResponse -> TestTree
responseListFindingAggregations =
  res
    "ListFindingAggregationsResponse"
    "fixture/ListFindingAggregationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindingAggregations)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings =
  res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindings)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers =
  res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMembers)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListUsageTotals :: ListUsageTotalsResponse -> TestTree
responseListUsageTotals =
  res
    "ListUsageTotalsResponse"
    "fixture/ListUsageTotalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsageTotals)

responseResetEncryptionKey :: ResetEncryptionKeyResponse -> TestTree
responseResetEncryptionKey =
  res
    "ResetEncryptionKeyResponse"
    "fixture/ResetEncryptionKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetEncryptionKey)

responseSearchVulnerabilities :: SearchVulnerabilitiesResponse -> TestTree
responseSearchVulnerabilities =
  res
    "SearchVulnerabilitiesResponse"
    "fixture/SearchVulnerabilitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchVulnerabilities)

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

responseUpdateConfiguration :: UpdateConfigurationResponse -> TestTree
responseUpdateConfiguration =
  res
    "UpdateConfigurationResponse"
    "fixture/UpdateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfiguration)

responseUpdateEc2DeepInspectionConfiguration :: UpdateEc2DeepInspectionConfigurationResponse -> TestTree
responseUpdateEc2DeepInspectionConfiguration =
  res
    "UpdateEc2DeepInspectionConfigurationResponse"
    "fixture/UpdateEc2DeepInspectionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEc2DeepInspectionConfiguration)

responseUpdateEncryptionKey :: UpdateEncryptionKeyResponse -> TestTree
responseUpdateEncryptionKey =
  res
    "UpdateEncryptionKeyResponse"
    "fixture/UpdateEncryptionKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEncryptionKey)

responseUpdateFilter :: UpdateFilterResponse -> TestTree
responseUpdateFilter =
  res
    "UpdateFilterResponse"
    "fixture/UpdateFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFilter)

responseUpdateOrgEc2DeepInspectionConfiguration :: UpdateOrgEc2DeepInspectionConfigurationResponse -> TestTree
responseUpdateOrgEc2DeepInspectionConfiguration =
  res
    "UpdateOrgEc2DeepInspectionConfigurationResponse"
    "fixture/UpdateOrgEc2DeepInspectionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOrgEc2DeepInspectionConfiguration)

responseUpdateOrganizationConfiguration :: UpdateOrganizationConfigurationResponse -> TestTree
responseUpdateOrganizationConfiguration =
  res
    "UpdateOrganizationConfigurationResponse"
    "fixture/UpdateOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOrganizationConfiguration)
