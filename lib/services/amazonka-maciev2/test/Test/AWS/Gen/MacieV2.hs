{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MacieV2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MacieV2 where

import Amazonka.MacieV2
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.MacieV2.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateMember $
--             newCreateMember
--
--         , requestEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccount
--
--         , requestDescribeClassificationJob $
--             newDescribeClassificationJob
--
--         , requestListFindings $
--             newListFindings
--
--         , requestGetAdministratorAccount $
--             newGetAdministratorAccount
--
--         , requestListOrganizationAdminAccounts $
--             newListOrganizationAdminAccounts
--
--         , requestSearchResources $
--             newSearchResources
--
--         , requestDisableMacie $
--             newDisableMacie
--
--         , requestUpdateFindingsFilter $
--             newUpdateFindingsFilter
--
--         , requestDeleteFindingsFilter $
--             newDeleteFindingsFilter
--
--         , requestListFindingsFilters $
--             newListFindingsFilters
--
--         , requestEnableMacie $
--             newEnableMacie
--
--         , requestGetUsageTotals $
--             newGetUsageTotals
--
--         , requestCreateFindingsFilter $
--             newCreateFindingsFilter
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListInvitations $
--             newListInvitations
--
--         , requestDescribeBuckets $
--             newDescribeBuckets
--
--         , requestListClassificationJobs $
--             newListClassificationJobs
--
--         , requestGetFindingsFilter $
--             newGetFindingsFilter
--
--         , requestUpdateClassificationJob $
--             newUpdateClassificationJob
--
--         , requestDeleteInvitations $
--             newDeleteInvitations
--
--         , requestGetMasterAccount $
--             newGetMasterAccount
--
--         , requestPutClassificationExportConfiguration $
--             newPutClassificationExportConfiguration
--
--         , requestGetCustomDataIdentifier $
--             newGetCustomDataIdentifier
--
--         , requestGetUsageStatistics $
--             newGetUsageStatistics
--
--         , requestDeclineInvitations $
--             newDeclineInvitations
--
--         , requestTestCustomDataIdentifier $
--             newTestCustomDataIdentifier
--
--         , requestCreateInvitations $
--             newCreateInvitations
--
--         , requestDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfiguration
--
--         , requestBatchGetCustomDataIdentifiers $
--             newBatchGetCustomDataIdentifiers
--
--         , requestDeleteMember $
--             newDeleteMember
--
--         , requestDisassociateFromMasterAccount $
--             newDisassociateFromMasterAccount
--
--         , requestAcceptInvitation $
--             newAcceptInvitation
--
--         , requestListMembers $
--             newListMembers
--
--         , requestUpdateMacieSession $
--             newUpdateMacieSession
--
--         , requestGetClassificationExportConfiguration $
--             newGetClassificationExportConfiguration
--
--         , requestGetFindingsPublicationConfiguration $
--             newGetFindingsPublicationConfiguration
--
--         , requestCreateCustomDataIdentifier $
--             newCreateCustomDataIdentifier
--
--         , requestCreateSampleFindings $
--             newCreateSampleFindings
--
--         , requestListManagedDataIdentifiers $
--             newListManagedDataIdentifiers
--
--         , requestUpdateMemberSession $
--             newUpdateMemberSession
--
--         , requestGetInvitationsCount $
--             newGetInvitationsCount
--
--         , requestUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfiguration
--
--         , requestDisassociateMember $
--             newDisassociateMember
--
--         , requestCreateClassificationJob $
--             newCreateClassificationJob
--
--         , requestGetBucketStatistics $
--             newGetBucketStatistics
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetFindings $
--             newGetFindings
--
--         , requestPutFindingsPublicationConfiguration $
--             newPutFindingsPublicationConfiguration
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetMacieSession $
--             newGetMacieSession
--
--         , requestGetFindingStatistics $
--             newGetFindingStatistics
--
--         , requestGetMember $
--             newGetMember
--
--         , requestDisassociateFromAdministratorAccount $
--             newDisassociateFromAdministratorAccount
--
--         , requestDeleteCustomDataIdentifier $
--             newDeleteCustomDataIdentifier
--
--         , requestDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccount
--
--         , requestListCustomDataIdentifiers $
--             newListCustomDataIdentifiers
--
--           ]

--     , testGroup "response"
--         [ responseCreateMember $
--             newCreateMemberResponse
--
--         , responseEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccountResponse
--
--         , responseDescribeClassificationJob $
--             newDescribeClassificationJobResponse
--
--         , responseListFindings $
--             newListFindingsResponse
--
--         , responseGetAdministratorAccount $
--             newGetAdministratorAccountResponse
--
--         , responseListOrganizationAdminAccounts $
--             newListOrganizationAdminAccountsResponse
--
--         , responseSearchResources $
--             newSearchResourcesResponse
--
--         , responseDisableMacie $
--             newDisableMacieResponse
--
--         , responseUpdateFindingsFilter $
--             newUpdateFindingsFilterResponse
--
--         , responseDeleteFindingsFilter $
--             newDeleteFindingsFilterResponse
--
--         , responseListFindingsFilters $
--             newListFindingsFiltersResponse
--
--         , responseEnableMacie $
--             newEnableMacieResponse
--
--         , responseGetUsageTotals $
--             newGetUsageTotalsResponse
--
--         , responseCreateFindingsFilter $
--             newCreateFindingsFilterResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListInvitations $
--             newListInvitationsResponse
--
--         , responseDescribeBuckets $
--             newDescribeBucketsResponse
--
--         , responseListClassificationJobs $
--             newListClassificationJobsResponse
--
--         , responseGetFindingsFilter $
--             newGetFindingsFilterResponse
--
--         , responseUpdateClassificationJob $
--             newUpdateClassificationJobResponse
--
--         , responseDeleteInvitations $
--             newDeleteInvitationsResponse
--
--         , responseGetMasterAccount $
--             newGetMasterAccountResponse
--
--         , responsePutClassificationExportConfiguration $
--             newPutClassificationExportConfigurationResponse
--
--         , responseGetCustomDataIdentifier $
--             newGetCustomDataIdentifierResponse
--
--         , responseGetUsageStatistics $
--             newGetUsageStatisticsResponse
--
--         , responseDeclineInvitations $
--             newDeclineInvitationsResponse
--
--         , responseTestCustomDataIdentifier $
--             newTestCustomDataIdentifierResponse
--
--         , responseCreateInvitations $
--             newCreateInvitationsResponse
--
--         , responseDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfigurationResponse
--
--         , responseBatchGetCustomDataIdentifiers $
--             newBatchGetCustomDataIdentifiersResponse
--
--         , responseDeleteMember $
--             newDeleteMemberResponse
--
--         , responseDisassociateFromMasterAccount $
--             newDisassociateFromMasterAccountResponse
--
--         , responseAcceptInvitation $
--             newAcceptInvitationResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseUpdateMacieSession $
--             newUpdateMacieSessionResponse
--
--         , responseGetClassificationExportConfiguration $
--             newGetClassificationExportConfigurationResponse
--
--         , responseGetFindingsPublicationConfiguration $
--             newGetFindingsPublicationConfigurationResponse
--
--         , responseCreateCustomDataIdentifier $
--             newCreateCustomDataIdentifierResponse
--
--         , responseCreateSampleFindings $
--             newCreateSampleFindingsResponse
--
--         , responseListManagedDataIdentifiers $
--             newListManagedDataIdentifiersResponse
--
--         , responseUpdateMemberSession $
--             newUpdateMemberSessionResponse
--
--         , responseGetInvitationsCount $
--             newGetInvitationsCountResponse
--
--         , responseUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfigurationResponse
--
--         , responseDisassociateMember $
--             newDisassociateMemberResponse
--
--         , responseCreateClassificationJob $
--             newCreateClassificationJobResponse
--
--         , responseGetBucketStatistics $
--             newGetBucketStatisticsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetFindings $
--             newGetFindingsResponse
--
--         , responsePutFindingsPublicationConfiguration $
--             newPutFindingsPublicationConfigurationResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetMacieSession $
--             newGetMacieSessionResponse
--
--         , responseGetFindingStatistics $
--             newGetFindingStatisticsResponse
--
--         , responseGetMember $
--             newGetMemberResponse
--
--         , responseDisassociateFromAdministratorAccount $
--             newDisassociateFromAdministratorAccountResponse
--
--         , responseDeleteCustomDataIdentifier $
--             newDeleteCustomDataIdentifierResponse
--
--         , responseDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccountResponse
--
--         , responseListCustomDataIdentifiers $
--             newListCustomDataIdentifiersResponse
--
--           ]
--     ]

-- Requests

requestCreateMember :: CreateMember -> TestTree
requestCreateMember =
  req
    "CreateMember"
    "fixture/CreateMember.yaml"

requestEnableOrganizationAdminAccount :: EnableOrganizationAdminAccount -> TestTree
requestEnableOrganizationAdminAccount =
  req
    "EnableOrganizationAdminAccount"
    "fixture/EnableOrganizationAdminAccount.yaml"

requestDescribeClassificationJob :: DescribeClassificationJob -> TestTree
requestDescribeClassificationJob =
  req
    "DescribeClassificationJob"
    "fixture/DescribeClassificationJob.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings =
  req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestGetAdministratorAccount :: GetAdministratorAccount -> TestTree
requestGetAdministratorAccount =
  req
    "GetAdministratorAccount"
    "fixture/GetAdministratorAccount.yaml"

requestListOrganizationAdminAccounts :: ListOrganizationAdminAccounts -> TestTree
requestListOrganizationAdminAccounts =
  req
    "ListOrganizationAdminAccounts"
    "fixture/ListOrganizationAdminAccounts.yaml"

requestSearchResources :: SearchResources -> TestTree
requestSearchResources =
  req
    "SearchResources"
    "fixture/SearchResources.yaml"

requestDisableMacie :: DisableMacie -> TestTree
requestDisableMacie =
  req
    "DisableMacie"
    "fixture/DisableMacie.yaml"

requestUpdateFindingsFilter :: UpdateFindingsFilter -> TestTree
requestUpdateFindingsFilter =
  req
    "UpdateFindingsFilter"
    "fixture/UpdateFindingsFilter.yaml"

requestDeleteFindingsFilter :: DeleteFindingsFilter -> TestTree
requestDeleteFindingsFilter =
  req
    "DeleteFindingsFilter"
    "fixture/DeleteFindingsFilter.yaml"

requestListFindingsFilters :: ListFindingsFilters -> TestTree
requestListFindingsFilters =
  req
    "ListFindingsFilters"
    "fixture/ListFindingsFilters.yaml"

requestEnableMacie :: EnableMacie -> TestTree
requestEnableMacie =
  req
    "EnableMacie"
    "fixture/EnableMacie.yaml"

requestGetUsageTotals :: GetUsageTotals -> TestTree
requestGetUsageTotals =
  req
    "GetUsageTotals"
    "fixture/GetUsageTotals.yaml"

requestCreateFindingsFilter :: CreateFindingsFilter -> TestTree
requestCreateFindingsFilter =
  req
    "CreateFindingsFilter"
    "fixture/CreateFindingsFilter.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListInvitations :: ListInvitations -> TestTree
requestListInvitations =
  req
    "ListInvitations"
    "fixture/ListInvitations.yaml"

requestDescribeBuckets :: DescribeBuckets -> TestTree
requestDescribeBuckets =
  req
    "DescribeBuckets"
    "fixture/DescribeBuckets.yaml"

requestListClassificationJobs :: ListClassificationJobs -> TestTree
requestListClassificationJobs =
  req
    "ListClassificationJobs"
    "fixture/ListClassificationJobs.yaml"

requestGetFindingsFilter :: GetFindingsFilter -> TestTree
requestGetFindingsFilter =
  req
    "GetFindingsFilter"
    "fixture/GetFindingsFilter.yaml"

requestUpdateClassificationJob :: UpdateClassificationJob -> TestTree
requestUpdateClassificationJob =
  req
    "UpdateClassificationJob"
    "fixture/UpdateClassificationJob.yaml"

requestDeleteInvitations :: DeleteInvitations -> TestTree
requestDeleteInvitations =
  req
    "DeleteInvitations"
    "fixture/DeleteInvitations.yaml"

requestGetMasterAccount :: GetMasterAccount -> TestTree
requestGetMasterAccount =
  req
    "GetMasterAccount"
    "fixture/GetMasterAccount.yaml"

requestPutClassificationExportConfiguration :: PutClassificationExportConfiguration -> TestTree
requestPutClassificationExportConfiguration =
  req
    "PutClassificationExportConfiguration"
    "fixture/PutClassificationExportConfiguration.yaml"

requestGetCustomDataIdentifier :: GetCustomDataIdentifier -> TestTree
requestGetCustomDataIdentifier =
  req
    "GetCustomDataIdentifier"
    "fixture/GetCustomDataIdentifier.yaml"

requestGetUsageStatistics :: GetUsageStatistics -> TestTree
requestGetUsageStatistics =
  req
    "GetUsageStatistics"
    "fixture/GetUsageStatistics.yaml"

requestDeclineInvitations :: DeclineInvitations -> TestTree
requestDeclineInvitations =
  req
    "DeclineInvitations"
    "fixture/DeclineInvitations.yaml"

requestTestCustomDataIdentifier :: TestCustomDataIdentifier -> TestTree
requestTestCustomDataIdentifier =
  req
    "TestCustomDataIdentifier"
    "fixture/TestCustomDataIdentifier.yaml"

requestCreateInvitations :: CreateInvitations -> TestTree
requestCreateInvitations =
  req
    "CreateInvitations"
    "fixture/CreateInvitations.yaml"

requestDescribeOrganizationConfiguration :: DescribeOrganizationConfiguration -> TestTree
requestDescribeOrganizationConfiguration =
  req
    "DescribeOrganizationConfiguration"
    "fixture/DescribeOrganizationConfiguration.yaml"

requestBatchGetCustomDataIdentifiers :: BatchGetCustomDataIdentifiers -> TestTree
requestBatchGetCustomDataIdentifiers =
  req
    "BatchGetCustomDataIdentifiers"
    "fixture/BatchGetCustomDataIdentifiers.yaml"

requestDeleteMember :: DeleteMember -> TestTree
requestDeleteMember =
  req
    "DeleteMember"
    "fixture/DeleteMember.yaml"

requestDisassociateFromMasterAccount :: DisassociateFromMasterAccount -> TestTree
requestDisassociateFromMasterAccount =
  req
    "DisassociateFromMasterAccount"
    "fixture/DisassociateFromMasterAccount.yaml"

requestAcceptInvitation :: AcceptInvitation -> TestTree
requestAcceptInvitation =
  req
    "AcceptInvitation"
    "fixture/AcceptInvitation.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers =
  req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestUpdateMacieSession :: UpdateMacieSession -> TestTree
requestUpdateMacieSession =
  req
    "UpdateMacieSession"
    "fixture/UpdateMacieSession.yaml"

requestGetClassificationExportConfiguration :: GetClassificationExportConfiguration -> TestTree
requestGetClassificationExportConfiguration =
  req
    "GetClassificationExportConfiguration"
    "fixture/GetClassificationExportConfiguration.yaml"

requestGetFindingsPublicationConfiguration :: GetFindingsPublicationConfiguration -> TestTree
requestGetFindingsPublicationConfiguration =
  req
    "GetFindingsPublicationConfiguration"
    "fixture/GetFindingsPublicationConfiguration.yaml"

requestCreateCustomDataIdentifier :: CreateCustomDataIdentifier -> TestTree
requestCreateCustomDataIdentifier =
  req
    "CreateCustomDataIdentifier"
    "fixture/CreateCustomDataIdentifier.yaml"

requestCreateSampleFindings :: CreateSampleFindings -> TestTree
requestCreateSampleFindings =
  req
    "CreateSampleFindings"
    "fixture/CreateSampleFindings.yaml"

requestListManagedDataIdentifiers :: ListManagedDataIdentifiers -> TestTree
requestListManagedDataIdentifiers =
  req
    "ListManagedDataIdentifiers"
    "fixture/ListManagedDataIdentifiers.yaml"

requestUpdateMemberSession :: UpdateMemberSession -> TestTree
requestUpdateMemberSession =
  req
    "UpdateMemberSession"
    "fixture/UpdateMemberSession.yaml"

requestGetInvitationsCount :: GetInvitationsCount -> TestTree
requestGetInvitationsCount =
  req
    "GetInvitationsCount"
    "fixture/GetInvitationsCount.yaml"

requestUpdateOrganizationConfiguration :: UpdateOrganizationConfiguration -> TestTree
requestUpdateOrganizationConfiguration =
  req
    "UpdateOrganizationConfiguration"
    "fixture/UpdateOrganizationConfiguration.yaml"

requestDisassociateMember :: DisassociateMember -> TestTree
requestDisassociateMember =
  req
    "DisassociateMember"
    "fixture/DisassociateMember.yaml"

requestCreateClassificationJob :: CreateClassificationJob -> TestTree
requestCreateClassificationJob =
  req
    "CreateClassificationJob"
    "fixture/CreateClassificationJob.yaml"

requestGetBucketStatistics :: GetBucketStatistics -> TestTree
requestGetBucketStatistics =
  req
    "GetBucketStatistics"
    "fixture/GetBucketStatistics.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetFindings :: GetFindings -> TestTree
requestGetFindings =
  req
    "GetFindings"
    "fixture/GetFindings.yaml"

requestPutFindingsPublicationConfiguration :: PutFindingsPublicationConfiguration -> TestTree
requestPutFindingsPublicationConfiguration =
  req
    "PutFindingsPublicationConfiguration"
    "fixture/PutFindingsPublicationConfiguration.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetMacieSession :: GetMacieSession -> TestTree
requestGetMacieSession =
  req
    "GetMacieSession"
    "fixture/GetMacieSession.yaml"

requestGetFindingStatistics :: GetFindingStatistics -> TestTree
requestGetFindingStatistics =
  req
    "GetFindingStatistics"
    "fixture/GetFindingStatistics.yaml"

requestGetMember :: GetMember -> TestTree
requestGetMember =
  req
    "GetMember"
    "fixture/GetMember.yaml"

requestDisassociateFromAdministratorAccount :: DisassociateFromAdministratorAccount -> TestTree
requestDisassociateFromAdministratorAccount =
  req
    "DisassociateFromAdministratorAccount"
    "fixture/DisassociateFromAdministratorAccount.yaml"

requestDeleteCustomDataIdentifier :: DeleteCustomDataIdentifier -> TestTree
requestDeleteCustomDataIdentifier =
  req
    "DeleteCustomDataIdentifier"
    "fixture/DeleteCustomDataIdentifier.yaml"

requestDisableOrganizationAdminAccount :: DisableOrganizationAdminAccount -> TestTree
requestDisableOrganizationAdminAccount =
  req
    "DisableOrganizationAdminAccount"
    "fixture/DisableOrganizationAdminAccount.yaml"

requestListCustomDataIdentifiers :: ListCustomDataIdentifiers -> TestTree
requestListCustomDataIdentifiers =
  req
    "ListCustomDataIdentifiers"
    "fixture/ListCustomDataIdentifiers.yaml"

-- Responses

responseCreateMember :: CreateMemberResponse -> TestTree
responseCreateMember =
  res
    "CreateMemberResponse"
    "fixture/CreateMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMember)

responseEnableOrganizationAdminAccount :: EnableOrganizationAdminAccountResponse -> TestTree
responseEnableOrganizationAdminAccount =
  res
    "EnableOrganizationAdminAccountResponse"
    "fixture/EnableOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableOrganizationAdminAccount)

responseDescribeClassificationJob :: DescribeClassificationJobResponse -> TestTree
responseDescribeClassificationJob =
  res
    "DescribeClassificationJobResponse"
    "fixture/DescribeClassificationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClassificationJob)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings =
  res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindings)

responseGetAdministratorAccount :: GetAdministratorAccountResponse -> TestTree
responseGetAdministratorAccount =
  res
    "GetAdministratorAccountResponse"
    "fixture/GetAdministratorAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAdministratorAccount)

responseListOrganizationAdminAccounts :: ListOrganizationAdminAccountsResponse -> TestTree
responseListOrganizationAdminAccounts =
  res
    "ListOrganizationAdminAccountsResponse"
    "fixture/ListOrganizationAdminAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationAdminAccounts)

responseSearchResources :: SearchResourcesResponse -> TestTree
responseSearchResources =
  res
    "SearchResourcesResponse"
    "fixture/SearchResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchResources)

responseDisableMacie :: DisableMacieResponse -> TestTree
responseDisableMacie =
  res
    "DisableMacieResponse"
    "fixture/DisableMacieResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableMacie)

responseUpdateFindingsFilter :: UpdateFindingsFilterResponse -> TestTree
responseUpdateFindingsFilter =
  res
    "UpdateFindingsFilterResponse"
    "fixture/UpdateFindingsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFindingsFilter)

responseDeleteFindingsFilter :: DeleteFindingsFilterResponse -> TestTree
responseDeleteFindingsFilter =
  res
    "DeleteFindingsFilterResponse"
    "fixture/DeleteFindingsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFindingsFilter)

responseListFindingsFilters :: ListFindingsFiltersResponse -> TestTree
responseListFindingsFilters =
  res
    "ListFindingsFiltersResponse"
    "fixture/ListFindingsFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindingsFilters)

responseEnableMacie :: EnableMacieResponse -> TestTree
responseEnableMacie =
  res
    "EnableMacieResponse"
    "fixture/EnableMacieResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableMacie)

responseGetUsageTotals :: GetUsageTotalsResponse -> TestTree
responseGetUsageTotals =
  res
    "GetUsageTotalsResponse"
    "fixture/GetUsageTotalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsageTotals)

responseCreateFindingsFilter :: CreateFindingsFilterResponse -> TestTree
responseCreateFindingsFilter =
  res
    "CreateFindingsFilterResponse"
    "fixture/CreateFindingsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFindingsFilter)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListInvitations :: ListInvitationsResponse -> TestTree
responseListInvitations =
  res
    "ListInvitationsResponse"
    "fixture/ListInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInvitations)

responseDescribeBuckets :: DescribeBucketsResponse -> TestTree
responseDescribeBuckets =
  res
    "DescribeBucketsResponse"
    "fixture/DescribeBucketsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBuckets)

responseListClassificationJobs :: ListClassificationJobsResponse -> TestTree
responseListClassificationJobs =
  res
    "ListClassificationJobsResponse"
    "fixture/ListClassificationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClassificationJobs)

responseGetFindingsFilter :: GetFindingsFilterResponse -> TestTree
responseGetFindingsFilter =
  res
    "GetFindingsFilterResponse"
    "fixture/GetFindingsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingsFilter)

responseUpdateClassificationJob :: UpdateClassificationJobResponse -> TestTree
responseUpdateClassificationJob =
  res
    "UpdateClassificationJobResponse"
    "fixture/UpdateClassificationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClassificationJob)

responseDeleteInvitations :: DeleteInvitationsResponse -> TestTree
responseDeleteInvitations =
  res
    "DeleteInvitationsResponse"
    "fixture/DeleteInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInvitations)

responseGetMasterAccount :: GetMasterAccountResponse -> TestTree
responseGetMasterAccount =
  res
    "GetMasterAccountResponse"
    "fixture/GetMasterAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMasterAccount)

responsePutClassificationExportConfiguration :: PutClassificationExportConfigurationResponse -> TestTree
responsePutClassificationExportConfiguration =
  res
    "PutClassificationExportConfigurationResponse"
    "fixture/PutClassificationExportConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutClassificationExportConfiguration)

responseGetCustomDataIdentifier :: GetCustomDataIdentifierResponse -> TestTree
responseGetCustomDataIdentifier =
  res
    "GetCustomDataIdentifierResponse"
    "fixture/GetCustomDataIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCustomDataIdentifier)

responseGetUsageStatistics :: GetUsageStatisticsResponse -> TestTree
responseGetUsageStatistics =
  res
    "GetUsageStatisticsResponse"
    "fixture/GetUsageStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsageStatistics)

responseDeclineInvitations :: DeclineInvitationsResponse -> TestTree
responseDeclineInvitations =
  res
    "DeclineInvitationsResponse"
    "fixture/DeclineInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeclineInvitations)

responseTestCustomDataIdentifier :: TestCustomDataIdentifierResponse -> TestTree
responseTestCustomDataIdentifier =
  res
    "TestCustomDataIdentifierResponse"
    "fixture/TestCustomDataIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestCustomDataIdentifier)

responseCreateInvitations :: CreateInvitationsResponse -> TestTree
responseCreateInvitations =
  res
    "CreateInvitationsResponse"
    "fixture/CreateInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInvitations)

responseDescribeOrganizationConfiguration :: DescribeOrganizationConfigurationResponse -> TestTree
responseDescribeOrganizationConfiguration =
  res
    "DescribeOrganizationConfigurationResponse"
    "fixture/DescribeOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfiguration)

responseBatchGetCustomDataIdentifiers :: BatchGetCustomDataIdentifiersResponse -> TestTree
responseBatchGetCustomDataIdentifiers =
  res
    "BatchGetCustomDataIdentifiersResponse"
    "fixture/BatchGetCustomDataIdentifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetCustomDataIdentifiers)

responseDeleteMember :: DeleteMemberResponse -> TestTree
responseDeleteMember =
  res
    "DeleteMemberResponse"
    "fixture/DeleteMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMember)

responseDisassociateFromMasterAccount :: DisassociateFromMasterAccountResponse -> TestTree
responseDisassociateFromMasterAccount =
  res
    "DisassociateFromMasterAccountResponse"
    "fixture/DisassociateFromMasterAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFromMasterAccount)

responseAcceptInvitation :: AcceptInvitationResponse -> TestTree
responseAcceptInvitation =
  res
    "AcceptInvitationResponse"
    "fixture/AcceptInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInvitation)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers =
  res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMembers)

responseUpdateMacieSession :: UpdateMacieSessionResponse -> TestTree
responseUpdateMacieSession =
  res
    "UpdateMacieSessionResponse"
    "fixture/UpdateMacieSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMacieSession)

responseGetClassificationExportConfiguration :: GetClassificationExportConfigurationResponse -> TestTree
responseGetClassificationExportConfiguration =
  res
    "GetClassificationExportConfigurationResponse"
    "fixture/GetClassificationExportConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClassificationExportConfiguration)

responseGetFindingsPublicationConfiguration :: GetFindingsPublicationConfigurationResponse -> TestTree
responseGetFindingsPublicationConfiguration =
  res
    "GetFindingsPublicationConfigurationResponse"
    "fixture/GetFindingsPublicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingsPublicationConfiguration)

responseCreateCustomDataIdentifier :: CreateCustomDataIdentifierResponse -> TestTree
responseCreateCustomDataIdentifier =
  res
    "CreateCustomDataIdentifierResponse"
    "fixture/CreateCustomDataIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomDataIdentifier)

responseCreateSampleFindings :: CreateSampleFindingsResponse -> TestTree
responseCreateSampleFindings =
  res
    "CreateSampleFindingsResponse"
    "fixture/CreateSampleFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSampleFindings)

responseListManagedDataIdentifiers :: ListManagedDataIdentifiersResponse -> TestTree
responseListManagedDataIdentifiers =
  res
    "ListManagedDataIdentifiersResponse"
    "fixture/ListManagedDataIdentifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedDataIdentifiers)

responseUpdateMemberSession :: UpdateMemberSessionResponse -> TestTree
responseUpdateMemberSession =
  res
    "UpdateMemberSessionResponse"
    "fixture/UpdateMemberSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMemberSession)

responseGetInvitationsCount :: GetInvitationsCountResponse -> TestTree
responseGetInvitationsCount =
  res
    "GetInvitationsCountResponse"
    "fixture/GetInvitationsCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInvitationsCount)

responseUpdateOrganizationConfiguration :: UpdateOrganizationConfigurationResponse -> TestTree
responseUpdateOrganizationConfiguration =
  res
    "UpdateOrganizationConfigurationResponse"
    "fixture/UpdateOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOrganizationConfiguration)

responseDisassociateMember :: DisassociateMemberResponse -> TestTree
responseDisassociateMember =
  res
    "DisassociateMemberResponse"
    "fixture/DisassociateMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMember)

responseCreateClassificationJob :: CreateClassificationJobResponse -> TestTree
responseCreateClassificationJob =
  res
    "CreateClassificationJobResponse"
    "fixture/CreateClassificationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClassificationJob)

responseGetBucketStatistics :: GetBucketStatisticsResponse -> TestTree
responseGetBucketStatistics =
  res
    "GetBucketStatisticsResponse"
    "fixture/GetBucketStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketStatistics)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetFindings :: GetFindingsResponse -> TestTree
responseGetFindings =
  res
    "GetFindingsResponse"
    "fixture/GetFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindings)

responsePutFindingsPublicationConfiguration :: PutFindingsPublicationConfigurationResponse -> TestTree
responsePutFindingsPublicationConfiguration =
  res
    "PutFindingsPublicationConfigurationResponse"
    "fixture/PutFindingsPublicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFindingsPublicationConfiguration)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseGetMacieSession :: GetMacieSessionResponse -> TestTree
responseGetMacieSession =
  res
    "GetMacieSessionResponse"
    "fixture/GetMacieSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMacieSession)

responseGetFindingStatistics :: GetFindingStatisticsResponse -> TestTree
responseGetFindingStatistics =
  res
    "GetFindingStatisticsResponse"
    "fixture/GetFindingStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingStatistics)

responseGetMember :: GetMemberResponse -> TestTree
responseGetMember =
  res
    "GetMemberResponse"
    "fixture/GetMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMember)

responseDisassociateFromAdministratorAccount :: DisassociateFromAdministratorAccountResponse -> TestTree
responseDisassociateFromAdministratorAccount =
  res
    "DisassociateFromAdministratorAccountResponse"
    "fixture/DisassociateFromAdministratorAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFromAdministratorAccount)

responseDeleteCustomDataIdentifier :: DeleteCustomDataIdentifierResponse -> TestTree
responseDeleteCustomDataIdentifier =
  res
    "DeleteCustomDataIdentifierResponse"
    "fixture/DeleteCustomDataIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomDataIdentifier)

responseDisableOrganizationAdminAccount :: DisableOrganizationAdminAccountResponse -> TestTree
responseDisableOrganizationAdminAccount =
  res
    "DisableOrganizationAdminAccountResponse"
    "fixture/DisableOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableOrganizationAdminAccount)

responseListCustomDataIdentifiers :: ListCustomDataIdentifiersResponse -> TestTree
responseListCustomDataIdentifiers =
  res
    "ListCustomDataIdentifiersResponse"
    "fixture/ListCustomDataIdentifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomDataIdentifiers)
