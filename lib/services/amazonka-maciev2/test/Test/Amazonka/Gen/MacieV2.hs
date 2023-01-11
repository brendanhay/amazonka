{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MacieV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MacieV2 where

import Amazonka.MacieV2
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MacieV2.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptInvitation $
--             newAcceptInvitation
--
--         , requestBatchGetCustomDataIdentifiers $
--             newBatchGetCustomDataIdentifiers
--
--         , requestCreateAllowList $
--             newCreateAllowList
--
--         , requestCreateClassificationJob $
--             newCreateClassificationJob
--
--         , requestCreateCustomDataIdentifier $
--             newCreateCustomDataIdentifier
--
--         , requestCreateFindingsFilter $
--             newCreateFindingsFilter
--
--         , requestCreateInvitations $
--             newCreateInvitations
--
--         , requestCreateMember $
--             newCreateMember
--
--         , requestCreateSampleFindings $
--             newCreateSampleFindings
--
--         , requestDeclineInvitations $
--             newDeclineInvitations
--
--         , requestDeleteAllowList $
--             newDeleteAllowList
--
--         , requestDeleteCustomDataIdentifier $
--             newDeleteCustomDataIdentifier
--
--         , requestDeleteFindingsFilter $
--             newDeleteFindingsFilter
--
--         , requestDeleteInvitations $
--             newDeleteInvitations
--
--         , requestDeleteMember $
--             newDeleteMember
--
--         , requestDescribeBuckets $
--             newDescribeBuckets
--
--         , requestDescribeClassificationJob $
--             newDescribeClassificationJob
--
--         , requestDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfiguration
--
--         , requestDisableMacie $
--             newDisableMacie
--
--         , requestDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccount
--
--         , requestDisassociateFromAdministratorAccount $
--             newDisassociateFromAdministratorAccount
--
--         , requestDisassociateFromMasterAccount $
--             newDisassociateFromMasterAccount
--
--         , requestDisassociateMember $
--             newDisassociateMember
--
--         , requestEnableMacie $
--             newEnableMacie
--
--         , requestEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccount
--
--         , requestGetAdministratorAccount $
--             newGetAdministratorAccount
--
--         , requestGetAllowList $
--             newGetAllowList
--
--         , requestGetAutomatedDiscoveryConfiguration $
--             newGetAutomatedDiscoveryConfiguration
--
--         , requestGetBucketStatistics $
--             newGetBucketStatistics
--
--         , requestGetClassificationExportConfiguration $
--             newGetClassificationExportConfiguration
--
--         , requestGetClassificationScope $
--             newGetClassificationScope
--
--         , requestGetCustomDataIdentifier $
--             newGetCustomDataIdentifier
--
--         , requestGetFindingStatistics $
--             newGetFindingStatistics
--
--         , requestGetFindings $
--             newGetFindings
--
--         , requestGetFindingsFilter $
--             newGetFindingsFilter
--
--         , requestGetFindingsPublicationConfiguration $
--             newGetFindingsPublicationConfiguration
--
--         , requestGetInvitationsCount $
--             newGetInvitationsCount
--
--         , requestGetMacieSession $
--             newGetMacieSession
--
--         , requestGetMasterAccount $
--             newGetMasterAccount
--
--         , requestGetMember $
--             newGetMember
--
--         , requestGetResourceProfile $
--             newGetResourceProfile
--
--         , requestGetRevealConfiguration $
--             newGetRevealConfiguration
--
--         , requestGetSensitiveDataOccurrences $
--             newGetSensitiveDataOccurrences
--
--         , requestGetSensitiveDataOccurrencesAvailability $
--             newGetSensitiveDataOccurrencesAvailability
--
--         , requestGetSensitivityInspectionTemplate $
--             newGetSensitivityInspectionTemplate
--
--         , requestGetUsageStatistics $
--             newGetUsageStatistics
--
--         , requestGetUsageTotals $
--             newGetUsageTotals
--
--         , requestListAllowLists $
--             newListAllowLists
--
--         , requestListClassificationJobs $
--             newListClassificationJobs
--
--         , requestListClassificationScopes $
--             newListClassificationScopes
--
--         , requestListCustomDataIdentifiers $
--             newListCustomDataIdentifiers
--
--         , requestListFindings $
--             newListFindings
--
--         , requestListFindingsFilters $
--             newListFindingsFilters
--
--         , requestListInvitations $
--             newListInvitations
--
--         , requestListManagedDataIdentifiers $
--             newListManagedDataIdentifiers
--
--         , requestListMembers $
--             newListMembers
--
--         , requestListOrganizationAdminAccounts $
--             newListOrganizationAdminAccounts
--
--         , requestListResourceProfileArtifacts $
--             newListResourceProfileArtifacts
--
--         , requestListResourceProfileDetections $
--             newListResourceProfileDetections
--
--         , requestListSensitivityInspectionTemplates $
--             newListSensitivityInspectionTemplates
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutClassificationExportConfiguration $
--             newPutClassificationExportConfiguration
--
--         , requestPutFindingsPublicationConfiguration $
--             newPutFindingsPublicationConfiguration
--
--         , requestSearchResources $
--             newSearchResources
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestCustomDataIdentifier $
--             newTestCustomDataIdentifier
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAllowList $
--             newUpdateAllowList
--
--         , requestUpdateAutomatedDiscoveryConfiguration $
--             newUpdateAutomatedDiscoveryConfiguration
--
--         , requestUpdateClassificationJob $
--             newUpdateClassificationJob
--
--         , requestUpdateClassificationScope $
--             newUpdateClassificationScope
--
--         , requestUpdateFindingsFilter $
--             newUpdateFindingsFilter
--
--         , requestUpdateMacieSession $
--             newUpdateMacieSession
--
--         , requestUpdateMemberSession $
--             newUpdateMemberSession
--
--         , requestUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfiguration
--
--         , requestUpdateResourceProfile $
--             newUpdateResourceProfile
--
--         , requestUpdateResourceProfileDetections $
--             newUpdateResourceProfileDetections
--
--         , requestUpdateRevealConfiguration $
--             newUpdateRevealConfiguration
--
--         , requestUpdateSensitivityInspectionTemplate $
--             newUpdateSensitivityInspectionTemplate
--
--           ]

--     , testGroup "response"
--         [ responseAcceptInvitation $
--             newAcceptInvitationResponse
--
--         , responseBatchGetCustomDataIdentifiers $
--             newBatchGetCustomDataIdentifiersResponse
--
--         , responseCreateAllowList $
--             newCreateAllowListResponse
--
--         , responseCreateClassificationJob $
--             newCreateClassificationJobResponse
--
--         , responseCreateCustomDataIdentifier $
--             newCreateCustomDataIdentifierResponse
--
--         , responseCreateFindingsFilter $
--             newCreateFindingsFilterResponse
--
--         , responseCreateInvitations $
--             newCreateInvitationsResponse
--
--         , responseCreateMember $
--             newCreateMemberResponse
--
--         , responseCreateSampleFindings $
--             newCreateSampleFindingsResponse
--
--         , responseDeclineInvitations $
--             newDeclineInvitationsResponse
--
--         , responseDeleteAllowList $
--             newDeleteAllowListResponse
--
--         , responseDeleteCustomDataIdentifier $
--             newDeleteCustomDataIdentifierResponse
--
--         , responseDeleteFindingsFilter $
--             newDeleteFindingsFilterResponse
--
--         , responseDeleteInvitations $
--             newDeleteInvitationsResponse
--
--         , responseDeleteMember $
--             newDeleteMemberResponse
--
--         , responseDescribeBuckets $
--             newDescribeBucketsResponse
--
--         , responseDescribeClassificationJob $
--             newDescribeClassificationJobResponse
--
--         , responseDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfigurationResponse
--
--         , responseDisableMacie $
--             newDisableMacieResponse
--
--         , responseDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccountResponse
--
--         , responseDisassociateFromAdministratorAccount $
--             newDisassociateFromAdministratorAccountResponse
--
--         , responseDisassociateFromMasterAccount $
--             newDisassociateFromMasterAccountResponse
--
--         , responseDisassociateMember $
--             newDisassociateMemberResponse
--
--         , responseEnableMacie $
--             newEnableMacieResponse
--
--         , responseEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccountResponse
--
--         , responseGetAdministratorAccount $
--             newGetAdministratorAccountResponse
--
--         , responseGetAllowList $
--             newGetAllowListResponse
--
--         , responseGetAutomatedDiscoveryConfiguration $
--             newGetAutomatedDiscoveryConfigurationResponse
--
--         , responseGetBucketStatistics $
--             newGetBucketStatisticsResponse
--
--         , responseGetClassificationExportConfiguration $
--             newGetClassificationExportConfigurationResponse
--
--         , responseGetClassificationScope $
--             newGetClassificationScopeResponse
--
--         , responseGetCustomDataIdentifier $
--             newGetCustomDataIdentifierResponse
--
--         , responseGetFindingStatistics $
--             newGetFindingStatisticsResponse
--
--         , responseGetFindings $
--             newGetFindingsResponse
--
--         , responseGetFindingsFilter $
--             newGetFindingsFilterResponse
--
--         , responseGetFindingsPublicationConfiguration $
--             newGetFindingsPublicationConfigurationResponse
--
--         , responseGetInvitationsCount $
--             newGetInvitationsCountResponse
--
--         , responseGetMacieSession $
--             newGetMacieSessionResponse
--
--         , responseGetMasterAccount $
--             newGetMasterAccountResponse
--
--         , responseGetMember $
--             newGetMemberResponse
--
--         , responseGetResourceProfile $
--             newGetResourceProfileResponse
--
--         , responseGetRevealConfiguration $
--             newGetRevealConfigurationResponse
--
--         , responseGetSensitiveDataOccurrences $
--             newGetSensitiveDataOccurrencesResponse
--
--         , responseGetSensitiveDataOccurrencesAvailability $
--             newGetSensitiveDataOccurrencesAvailabilityResponse
--
--         , responseGetSensitivityInspectionTemplate $
--             newGetSensitivityInspectionTemplateResponse
--
--         , responseGetUsageStatistics $
--             newGetUsageStatisticsResponse
--
--         , responseGetUsageTotals $
--             newGetUsageTotalsResponse
--
--         , responseListAllowLists $
--             newListAllowListsResponse
--
--         , responseListClassificationJobs $
--             newListClassificationJobsResponse
--
--         , responseListClassificationScopes $
--             newListClassificationScopesResponse
--
--         , responseListCustomDataIdentifiers $
--             newListCustomDataIdentifiersResponse
--
--         , responseListFindings $
--             newListFindingsResponse
--
--         , responseListFindingsFilters $
--             newListFindingsFiltersResponse
--
--         , responseListInvitations $
--             newListInvitationsResponse
--
--         , responseListManagedDataIdentifiers $
--             newListManagedDataIdentifiersResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseListOrganizationAdminAccounts $
--             newListOrganizationAdminAccountsResponse
--
--         , responseListResourceProfileArtifacts $
--             newListResourceProfileArtifactsResponse
--
--         , responseListResourceProfileDetections $
--             newListResourceProfileDetectionsResponse
--
--         , responseListSensitivityInspectionTemplates $
--             newListSensitivityInspectionTemplatesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutClassificationExportConfiguration $
--             newPutClassificationExportConfigurationResponse
--
--         , responsePutFindingsPublicationConfiguration $
--             newPutFindingsPublicationConfigurationResponse
--
--         , responseSearchResources $
--             newSearchResourcesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestCustomDataIdentifier $
--             newTestCustomDataIdentifierResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAllowList $
--             newUpdateAllowListResponse
--
--         , responseUpdateAutomatedDiscoveryConfiguration $
--             newUpdateAutomatedDiscoveryConfigurationResponse
--
--         , responseUpdateClassificationJob $
--             newUpdateClassificationJobResponse
--
--         , responseUpdateClassificationScope $
--             newUpdateClassificationScopeResponse
--
--         , responseUpdateFindingsFilter $
--             newUpdateFindingsFilterResponse
--
--         , responseUpdateMacieSession $
--             newUpdateMacieSessionResponse
--
--         , responseUpdateMemberSession $
--             newUpdateMemberSessionResponse
--
--         , responseUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfigurationResponse
--
--         , responseUpdateResourceProfile $
--             newUpdateResourceProfileResponse
--
--         , responseUpdateResourceProfileDetections $
--             newUpdateResourceProfileDetectionsResponse
--
--         , responseUpdateRevealConfiguration $
--             newUpdateRevealConfigurationResponse
--
--         , responseUpdateSensitivityInspectionTemplate $
--             newUpdateSensitivityInspectionTemplateResponse
--
--           ]
--     ]

-- Requests

requestAcceptInvitation :: AcceptInvitation -> TestTree
requestAcceptInvitation =
  req
    "AcceptInvitation"
    "fixture/AcceptInvitation.yaml"

requestBatchGetCustomDataIdentifiers :: BatchGetCustomDataIdentifiers -> TestTree
requestBatchGetCustomDataIdentifiers =
  req
    "BatchGetCustomDataIdentifiers"
    "fixture/BatchGetCustomDataIdentifiers.yaml"

requestCreateAllowList :: CreateAllowList -> TestTree
requestCreateAllowList =
  req
    "CreateAllowList"
    "fixture/CreateAllowList.yaml"

requestCreateClassificationJob :: CreateClassificationJob -> TestTree
requestCreateClassificationJob =
  req
    "CreateClassificationJob"
    "fixture/CreateClassificationJob.yaml"

requestCreateCustomDataIdentifier :: CreateCustomDataIdentifier -> TestTree
requestCreateCustomDataIdentifier =
  req
    "CreateCustomDataIdentifier"
    "fixture/CreateCustomDataIdentifier.yaml"

requestCreateFindingsFilter :: CreateFindingsFilter -> TestTree
requestCreateFindingsFilter =
  req
    "CreateFindingsFilter"
    "fixture/CreateFindingsFilter.yaml"

requestCreateInvitations :: CreateInvitations -> TestTree
requestCreateInvitations =
  req
    "CreateInvitations"
    "fixture/CreateInvitations.yaml"

requestCreateMember :: CreateMember -> TestTree
requestCreateMember =
  req
    "CreateMember"
    "fixture/CreateMember.yaml"

requestCreateSampleFindings :: CreateSampleFindings -> TestTree
requestCreateSampleFindings =
  req
    "CreateSampleFindings"
    "fixture/CreateSampleFindings.yaml"

requestDeclineInvitations :: DeclineInvitations -> TestTree
requestDeclineInvitations =
  req
    "DeclineInvitations"
    "fixture/DeclineInvitations.yaml"

requestDeleteAllowList :: DeleteAllowList -> TestTree
requestDeleteAllowList =
  req
    "DeleteAllowList"
    "fixture/DeleteAllowList.yaml"

requestDeleteCustomDataIdentifier :: DeleteCustomDataIdentifier -> TestTree
requestDeleteCustomDataIdentifier =
  req
    "DeleteCustomDataIdentifier"
    "fixture/DeleteCustomDataIdentifier.yaml"

requestDeleteFindingsFilter :: DeleteFindingsFilter -> TestTree
requestDeleteFindingsFilter =
  req
    "DeleteFindingsFilter"
    "fixture/DeleteFindingsFilter.yaml"

requestDeleteInvitations :: DeleteInvitations -> TestTree
requestDeleteInvitations =
  req
    "DeleteInvitations"
    "fixture/DeleteInvitations.yaml"

requestDeleteMember :: DeleteMember -> TestTree
requestDeleteMember =
  req
    "DeleteMember"
    "fixture/DeleteMember.yaml"

requestDescribeBuckets :: DescribeBuckets -> TestTree
requestDescribeBuckets =
  req
    "DescribeBuckets"
    "fixture/DescribeBuckets.yaml"

requestDescribeClassificationJob :: DescribeClassificationJob -> TestTree
requestDescribeClassificationJob =
  req
    "DescribeClassificationJob"
    "fixture/DescribeClassificationJob.yaml"

requestDescribeOrganizationConfiguration :: DescribeOrganizationConfiguration -> TestTree
requestDescribeOrganizationConfiguration =
  req
    "DescribeOrganizationConfiguration"
    "fixture/DescribeOrganizationConfiguration.yaml"

requestDisableMacie :: DisableMacie -> TestTree
requestDisableMacie =
  req
    "DisableMacie"
    "fixture/DisableMacie.yaml"

requestDisableOrganizationAdminAccount :: DisableOrganizationAdminAccount -> TestTree
requestDisableOrganizationAdminAccount =
  req
    "DisableOrganizationAdminAccount"
    "fixture/DisableOrganizationAdminAccount.yaml"

requestDisassociateFromAdministratorAccount :: DisassociateFromAdministratorAccount -> TestTree
requestDisassociateFromAdministratorAccount =
  req
    "DisassociateFromAdministratorAccount"
    "fixture/DisassociateFromAdministratorAccount.yaml"

requestDisassociateFromMasterAccount :: DisassociateFromMasterAccount -> TestTree
requestDisassociateFromMasterAccount =
  req
    "DisassociateFromMasterAccount"
    "fixture/DisassociateFromMasterAccount.yaml"

requestDisassociateMember :: DisassociateMember -> TestTree
requestDisassociateMember =
  req
    "DisassociateMember"
    "fixture/DisassociateMember.yaml"

requestEnableMacie :: EnableMacie -> TestTree
requestEnableMacie =
  req
    "EnableMacie"
    "fixture/EnableMacie.yaml"

requestEnableOrganizationAdminAccount :: EnableOrganizationAdminAccount -> TestTree
requestEnableOrganizationAdminAccount =
  req
    "EnableOrganizationAdminAccount"
    "fixture/EnableOrganizationAdminAccount.yaml"

requestGetAdministratorAccount :: GetAdministratorAccount -> TestTree
requestGetAdministratorAccount =
  req
    "GetAdministratorAccount"
    "fixture/GetAdministratorAccount.yaml"

requestGetAllowList :: GetAllowList -> TestTree
requestGetAllowList =
  req
    "GetAllowList"
    "fixture/GetAllowList.yaml"

requestGetAutomatedDiscoveryConfiguration :: GetAutomatedDiscoveryConfiguration -> TestTree
requestGetAutomatedDiscoveryConfiguration =
  req
    "GetAutomatedDiscoveryConfiguration"
    "fixture/GetAutomatedDiscoveryConfiguration.yaml"

requestGetBucketStatistics :: GetBucketStatistics -> TestTree
requestGetBucketStatistics =
  req
    "GetBucketStatistics"
    "fixture/GetBucketStatistics.yaml"

requestGetClassificationExportConfiguration :: GetClassificationExportConfiguration -> TestTree
requestGetClassificationExportConfiguration =
  req
    "GetClassificationExportConfiguration"
    "fixture/GetClassificationExportConfiguration.yaml"

requestGetClassificationScope :: GetClassificationScope -> TestTree
requestGetClassificationScope =
  req
    "GetClassificationScope"
    "fixture/GetClassificationScope.yaml"

requestGetCustomDataIdentifier :: GetCustomDataIdentifier -> TestTree
requestGetCustomDataIdentifier =
  req
    "GetCustomDataIdentifier"
    "fixture/GetCustomDataIdentifier.yaml"

requestGetFindingStatistics :: GetFindingStatistics -> TestTree
requestGetFindingStatistics =
  req
    "GetFindingStatistics"
    "fixture/GetFindingStatistics.yaml"

requestGetFindings :: GetFindings -> TestTree
requestGetFindings =
  req
    "GetFindings"
    "fixture/GetFindings.yaml"

requestGetFindingsFilter :: GetFindingsFilter -> TestTree
requestGetFindingsFilter =
  req
    "GetFindingsFilter"
    "fixture/GetFindingsFilter.yaml"

requestGetFindingsPublicationConfiguration :: GetFindingsPublicationConfiguration -> TestTree
requestGetFindingsPublicationConfiguration =
  req
    "GetFindingsPublicationConfiguration"
    "fixture/GetFindingsPublicationConfiguration.yaml"

requestGetInvitationsCount :: GetInvitationsCount -> TestTree
requestGetInvitationsCount =
  req
    "GetInvitationsCount"
    "fixture/GetInvitationsCount.yaml"

requestGetMacieSession :: GetMacieSession -> TestTree
requestGetMacieSession =
  req
    "GetMacieSession"
    "fixture/GetMacieSession.yaml"

requestGetMasterAccount :: GetMasterAccount -> TestTree
requestGetMasterAccount =
  req
    "GetMasterAccount"
    "fixture/GetMasterAccount.yaml"

requestGetMember :: GetMember -> TestTree
requestGetMember =
  req
    "GetMember"
    "fixture/GetMember.yaml"

requestGetResourceProfile :: GetResourceProfile -> TestTree
requestGetResourceProfile =
  req
    "GetResourceProfile"
    "fixture/GetResourceProfile.yaml"

requestGetRevealConfiguration :: GetRevealConfiguration -> TestTree
requestGetRevealConfiguration =
  req
    "GetRevealConfiguration"
    "fixture/GetRevealConfiguration.yaml"

requestGetSensitiveDataOccurrences :: GetSensitiveDataOccurrences -> TestTree
requestGetSensitiveDataOccurrences =
  req
    "GetSensitiveDataOccurrences"
    "fixture/GetSensitiveDataOccurrences.yaml"

requestGetSensitiveDataOccurrencesAvailability :: GetSensitiveDataOccurrencesAvailability -> TestTree
requestGetSensitiveDataOccurrencesAvailability =
  req
    "GetSensitiveDataOccurrencesAvailability"
    "fixture/GetSensitiveDataOccurrencesAvailability.yaml"

requestGetSensitivityInspectionTemplate :: GetSensitivityInspectionTemplate -> TestTree
requestGetSensitivityInspectionTemplate =
  req
    "GetSensitivityInspectionTemplate"
    "fixture/GetSensitivityInspectionTemplate.yaml"

requestGetUsageStatistics :: GetUsageStatistics -> TestTree
requestGetUsageStatistics =
  req
    "GetUsageStatistics"
    "fixture/GetUsageStatistics.yaml"

requestGetUsageTotals :: GetUsageTotals -> TestTree
requestGetUsageTotals =
  req
    "GetUsageTotals"
    "fixture/GetUsageTotals.yaml"

requestListAllowLists :: ListAllowLists -> TestTree
requestListAllowLists =
  req
    "ListAllowLists"
    "fixture/ListAllowLists.yaml"

requestListClassificationJobs :: ListClassificationJobs -> TestTree
requestListClassificationJobs =
  req
    "ListClassificationJobs"
    "fixture/ListClassificationJobs.yaml"

requestListClassificationScopes :: ListClassificationScopes -> TestTree
requestListClassificationScopes =
  req
    "ListClassificationScopes"
    "fixture/ListClassificationScopes.yaml"

requestListCustomDataIdentifiers :: ListCustomDataIdentifiers -> TestTree
requestListCustomDataIdentifiers =
  req
    "ListCustomDataIdentifiers"
    "fixture/ListCustomDataIdentifiers.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings =
  req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestListFindingsFilters :: ListFindingsFilters -> TestTree
requestListFindingsFilters =
  req
    "ListFindingsFilters"
    "fixture/ListFindingsFilters.yaml"

requestListInvitations :: ListInvitations -> TestTree
requestListInvitations =
  req
    "ListInvitations"
    "fixture/ListInvitations.yaml"

requestListManagedDataIdentifiers :: ListManagedDataIdentifiers -> TestTree
requestListManagedDataIdentifiers =
  req
    "ListManagedDataIdentifiers"
    "fixture/ListManagedDataIdentifiers.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers =
  req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestListOrganizationAdminAccounts :: ListOrganizationAdminAccounts -> TestTree
requestListOrganizationAdminAccounts =
  req
    "ListOrganizationAdminAccounts"
    "fixture/ListOrganizationAdminAccounts.yaml"

requestListResourceProfileArtifacts :: ListResourceProfileArtifacts -> TestTree
requestListResourceProfileArtifacts =
  req
    "ListResourceProfileArtifacts"
    "fixture/ListResourceProfileArtifacts.yaml"

requestListResourceProfileDetections :: ListResourceProfileDetections -> TestTree
requestListResourceProfileDetections =
  req
    "ListResourceProfileDetections"
    "fixture/ListResourceProfileDetections.yaml"

requestListSensitivityInspectionTemplates :: ListSensitivityInspectionTemplates -> TestTree
requestListSensitivityInspectionTemplates =
  req
    "ListSensitivityInspectionTemplates"
    "fixture/ListSensitivityInspectionTemplates.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutClassificationExportConfiguration :: PutClassificationExportConfiguration -> TestTree
requestPutClassificationExportConfiguration =
  req
    "PutClassificationExportConfiguration"
    "fixture/PutClassificationExportConfiguration.yaml"

requestPutFindingsPublicationConfiguration :: PutFindingsPublicationConfiguration -> TestTree
requestPutFindingsPublicationConfiguration =
  req
    "PutFindingsPublicationConfiguration"
    "fixture/PutFindingsPublicationConfiguration.yaml"

requestSearchResources :: SearchResources -> TestTree
requestSearchResources =
  req
    "SearchResources"
    "fixture/SearchResources.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestCustomDataIdentifier :: TestCustomDataIdentifier -> TestTree
requestTestCustomDataIdentifier =
  req
    "TestCustomDataIdentifier"
    "fixture/TestCustomDataIdentifier.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAllowList :: UpdateAllowList -> TestTree
requestUpdateAllowList =
  req
    "UpdateAllowList"
    "fixture/UpdateAllowList.yaml"

requestUpdateAutomatedDiscoveryConfiguration :: UpdateAutomatedDiscoveryConfiguration -> TestTree
requestUpdateAutomatedDiscoveryConfiguration =
  req
    "UpdateAutomatedDiscoveryConfiguration"
    "fixture/UpdateAutomatedDiscoveryConfiguration.yaml"

requestUpdateClassificationJob :: UpdateClassificationJob -> TestTree
requestUpdateClassificationJob =
  req
    "UpdateClassificationJob"
    "fixture/UpdateClassificationJob.yaml"

requestUpdateClassificationScope :: UpdateClassificationScope -> TestTree
requestUpdateClassificationScope =
  req
    "UpdateClassificationScope"
    "fixture/UpdateClassificationScope.yaml"

requestUpdateFindingsFilter :: UpdateFindingsFilter -> TestTree
requestUpdateFindingsFilter =
  req
    "UpdateFindingsFilter"
    "fixture/UpdateFindingsFilter.yaml"

requestUpdateMacieSession :: UpdateMacieSession -> TestTree
requestUpdateMacieSession =
  req
    "UpdateMacieSession"
    "fixture/UpdateMacieSession.yaml"

requestUpdateMemberSession :: UpdateMemberSession -> TestTree
requestUpdateMemberSession =
  req
    "UpdateMemberSession"
    "fixture/UpdateMemberSession.yaml"

requestUpdateOrganizationConfiguration :: UpdateOrganizationConfiguration -> TestTree
requestUpdateOrganizationConfiguration =
  req
    "UpdateOrganizationConfiguration"
    "fixture/UpdateOrganizationConfiguration.yaml"

requestUpdateResourceProfile :: UpdateResourceProfile -> TestTree
requestUpdateResourceProfile =
  req
    "UpdateResourceProfile"
    "fixture/UpdateResourceProfile.yaml"

requestUpdateResourceProfileDetections :: UpdateResourceProfileDetections -> TestTree
requestUpdateResourceProfileDetections =
  req
    "UpdateResourceProfileDetections"
    "fixture/UpdateResourceProfileDetections.yaml"

requestUpdateRevealConfiguration :: UpdateRevealConfiguration -> TestTree
requestUpdateRevealConfiguration =
  req
    "UpdateRevealConfiguration"
    "fixture/UpdateRevealConfiguration.yaml"

requestUpdateSensitivityInspectionTemplate :: UpdateSensitivityInspectionTemplate -> TestTree
requestUpdateSensitivityInspectionTemplate =
  req
    "UpdateSensitivityInspectionTemplate"
    "fixture/UpdateSensitivityInspectionTemplate.yaml"

-- Responses

responseAcceptInvitation :: AcceptInvitationResponse -> TestTree
responseAcceptInvitation =
  res
    "AcceptInvitationResponse"
    "fixture/AcceptInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInvitation)

responseBatchGetCustomDataIdentifiers :: BatchGetCustomDataIdentifiersResponse -> TestTree
responseBatchGetCustomDataIdentifiers =
  res
    "BatchGetCustomDataIdentifiersResponse"
    "fixture/BatchGetCustomDataIdentifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetCustomDataIdentifiers)

responseCreateAllowList :: CreateAllowListResponse -> TestTree
responseCreateAllowList =
  res
    "CreateAllowListResponse"
    "fixture/CreateAllowListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAllowList)

responseCreateClassificationJob :: CreateClassificationJobResponse -> TestTree
responseCreateClassificationJob =
  res
    "CreateClassificationJobResponse"
    "fixture/CreateClassificationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClassificationJob)

responseCreateCustomDataIdentifier :: CreateCustomDataIdentifierResponse -> TestTree
responseCreateCustomDataIdentifier =
  res
    "CreateCustomDataIdentifierResponse"
    "fixture/CreateCustomDataIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomDataIdentifier)

responseCreateFindingsFilter :: CreateFindingsFilterResponse -> TestTree
responseCreateFindingsFilter =
  res
    "CreateFindingsFilterResponse"
    "fixture/CreateFindingsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFindingsFilter)

responseCreateInvitations :: CreateInvitationsResponse -> TestTree
responseCreateInvitations =
  res
    "CreateInvitationsResponse"
    "fixture/CreateInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInvitations)

responseCreateMember :: CreateMemberResponse -> TestTree
responseCreateMember =
  res
    "CreateMemberResponse"
    "fixture/CreateMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMember)

responseCreateSampleFindings :: CreateSampleFindingsResponse -> TestTree
responseCreateSampleFindings =
  res
    "CreateSampleFindingsResponse"
    "fixture/CreateSampleFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSampleFindings)

responseDeclineInvitations :: DeclineInvitationsResponse -> TestTree
responseDeclineInvitations =
  res
    "DeclineInvitationsResponse"
    "fixture/DeclineInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeclineInvitations)

responseDeleteAllowList :: DeleteAllowListResponse -> TestTree
responseDeleteAllowList =
  res
    "DeleteAllowListResponse"
    "fixture/DeleteAllowListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAllowList)

responseDeleteCustomDataIdentifier :: DeleteCustomDataIdentifierResponse -> TestTree
responseDeleteCustomDataIdentifier =
  res
    "DeleteCustomDataIdentifierResponse"
    "fixture/DeleteCustomDataIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomDataIdentifier)

responseDeleteFindingsFilter :: DeleteFindingsFilterResponse -> TestTree
responseDeleteFindingsFilter =
  res
    "DeleteFindingsFilterResponse"
    "fixture/DeleteFindingsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFindingsFilter)

responseDeleteInvitations :: DeleteInvitationsResponse -> TestTree
responseDeleteInvitations =
  res
    "DeleteInvitationsResponse"
    "fixture/DeleteInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInvitations)

responseDeleteMember :: DeleteMemberResponse -> TestTree
responseDeleteMember =
  res
    "DeleteMemberResponse"
    "fixture/DeleteMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMember)

responseDescribeBuckets :: DescribeBucketsResponse -> TestTree
responseDescribeBuckets =
  res
    "DescribeBucketsResponse"
    "fixture/DescribeBucketsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBuckets)

responseDescribeClassificationJob :: DescribeClassificationJobResponse -> TestTree
responseDescribeClassificationJob =
  res
    "DescribeClassificationJobResponse"
    "fixture/DescribeClassificationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClassificationJob)

responseDescribeOrganizationConfiguration :: DescribeOrganizationConfigurationResponse -> TestTree
responseDescribeOrganizationConfiguration =
  res
    "DescribeOrganizationConfigurationResponse"
    "fixture/DescribeOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfiguration)

responseDisableMacie :: DisableMacieResponse -> TestTree
responseDisableMacie =
  res
    "DisableMacieResponse"
    "fixture/DisableMacieResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableMacie)

responseDisableOrganizationAdminAccount :: DisableOrganizationAdminAccountResponse -> TestTree
responseDisableOrganizationAdminAccount =
  res
    "DisableOrganizationAdminAccountResponse"
    "fixture/DisableOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableOrganizationAdminAccount)

responseDisassociateFromAdministratorAccount :: DisassociateFromAdministratorAccountResponse -> TestTree
responseDisassociateFromAdministratorAccount =
  res
    "DisassociateFromAdministratorAccountResponse"
    "fixture/DisassociateFromAdministratorAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFromAdministratorAccount)

responseDisassociateFromMasterAccount :: DisassociateFromMasterAccountResponse -> TestTree
responseDisassociateFromMasterAccount =
  res
    "DisassociateFromMasterAccountResponse"
    "fixture/DisassociateFromMasterAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFromMasterAccount)

responseDisassociateMember :: DisassociateMemberResponse -> TestTree
responseDisassociateMember =
  res
    "DisassociateMemberResponse"
    "fixture/DisassociateMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMember)

responseEnableMacie :: EnableMacieResponse -> TestTree
responseEnableMacie =
  res
    "EnableMacieResponse"
    "fixture/EnableMacieResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableMacie)

responseEnableOrganizationAdminAccount :: EnableOrganizationAdminAccountResponse -> TestTree
responseEnableOrganizationAdminAccount =
  res
    "EnableOrganizationAdminAccountResponse"
    "fixture/EnableOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableOrganizationAdminAccount)

responseGetAdministratorAccount :: GetAdministratorAccountResponse -> TestTree
responseGetAdministratorAccount =
  res
    "GetAdministratorAccountResponse"
    "fixture/GetAdministratorAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAdministratorAccount)

responseGetAllowList :: GetAllowListResponse -> TestTree
responseGetAllowList =
  res
    "GetAllowListResponse"
    "fixture/GetAllowListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAllowList)

responseGetAutomatedDiscoveryConfiguration :: GetAutomatedDiscoveryConfigurationResponse -> TestTree
responseGetAutomatedDiscoveryConfiguration =
  res
    "GetAutomatedDiscoveryConfigurationResponse"
    "fixture/GetAutomatedDiscoveryConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAutomatedDiscoveryConfiguration)

responseGetBucketStatistics :: GetBucketStatisticsResponse -> TestTree
responseGetBucketStatistics =
  res
    "GetBucketStatisticsResponse"
    "fixture/GetBucketStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketStatistics)

responseGetClassificationExportConfiguration :: GetClassificationExportConfigurationResponse -> TestTree
responseGetClassificationExportConfiguration =
  res
    "GetClassificationExportConfigurationResponse"
    "fixture/GetClassificationExportConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClassificationExportConfiguration)

responseGetClassificationScope :: GetClassificationScopeResponse -> TestTree
responseGetClassificationScope =
  res
    "GetClassificationScopeResponse"
    "fixture/GetClassificationScopeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClassificationScope)

responseGetCustomDataIdentifier :: GetCustomDataIdentifierResponse -> TestTree
responseGetCustomDataIdentifier =
  res
    "GetCustomDataIdentifierResponse"
    "fixture/GetCustomDataIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCustomDataIdentifier)

responseGetFindingStatistics :: GetFindingStatisticsResponse -> TestTree
responseGetFindingStatistics =
  res
    "GetFindingStatisticsResponse"
    "fixture/GetFindingStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingStatistics)

responseGetFindings :: GetFindingsResponse -> TestTree
responseGetFindings =
  res
    "GetFindingsResponse"
    "fixture/GetFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindings)

responseGetFindingsFilter :: GetFindingsFilterResponse -> TestTree
responseGetFindingsFilter =
  res
    "GetFindingsFilterResponse"
    "fixture/GetFindingsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingsFilter)

responseGetFindingsPublicationConfiguration :: GetFindingsPublicationConfigurationResponse -> TestTree
responseGetFindingsPublicationConfiguration =
  res
    "GetFindingsPublicationConfigurationResponse"
    "fixture/GetFindingsPublicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingsPublicationConfiguration)

responseGetInvitationsCount :: GetInvitationsCountResponse -> TestTree
responseGetInvitationsCount =
  res
    "GetInvitationsCountResponse"
    "fixture/GetInvitationsCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInvitationsCount)

responseGetMacieSession :: GetMacieSessionResponse -> TestTree
responseGetMacieSession =
  res
    "GetMacieSessionResponse"
    "fixture/GetMacieSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMacieSession)

responseGetMasterAccount :: GetMasterAccountResponse -> TestTree
responseGetMasterAccount =
  res
    "GetMasterAccountResponse"
    "fixture/GetMasterAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMasterAccount)

responseGetMember :: GetMemberResponse -> TestTree
responseGetMember =
  res
    "GetMemberResponse"
    "fixture/GetMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMember)

responseGetResourceProfile :: GetResourceProfileResponse -> TestTree
responseGetResourceProfile =
  res
    "GetResourceProfileResponse"
    "fixture/GetResourceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceProfile)

responseGetRevealConfiguration :: GetRevealConfigurationResponse -> TestTree
responseGetRevealConfiguration =
  res
    "GetRevealConfigurationResponse"
    "fixture/GetRevealConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRevealConfiguration)

responseGetSensitiveDataOccurrences :: GetSensitiveDataOccurrencesResponse -> TestTree
responseGetSensitiveDataOccurrences =
  res
    "GetSensitiveDataOccurrencesResponse"
    "fixture/GetSensitiveDataOccurrencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSensitiveDataOccurrences)

responseGetSensitiveDataOccurrencesAvailability :: GetSensitiveDataOccurrencesAvailabilityResponse -> TestTree
responseGetSensitiveDataOccurrencesAvailability =
  res
    "GetSensitiveDataOccurrencesAvailabilityResponse"
    "fixture/GetSensitiveDataOccurrencesAvailabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSensitiveDataOccurrencesAvailability)

responseGetSensitivityInspectionTemplate :: GetSensitivityInspectionTemplateResponse -> TestTree
responseGetSensitivityInspectionTemplate =
  res
    "GetSensitivityInspectionTemplateResponse"
    "fixture/GetSensitivityInspectionTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSensitivityInspectionTemplate)

responseGetUsageStatistics :: GetUsageStatisticsResponse -> TestTree
responseGetUsageStatistics =
  res
    "GetUsageStatisticsResponse"
    "fixture/GetUsageStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsageStatistics)

responseGetUsageTotals :: GetUsageTotalsResponse -> TestTree
responseGetUsageTotals =
  res
    "GetUsageTotalsResponse"
    "fixture/GetUsageTotalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsageTotals)

responseListAllowLists :: ListAllowListsResponse -> TestTree
responseListAllowLists =
  res
    "ListAllowListsResponse"
    "fixture/ListAllowListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAllowLists)

responseListClassificationJobs :: ListClassificationJobsResponse -> TestTree
responseListClassificationJobs =
  res
    "ListClassificationJobsResponse"
    "fixture/ListClassificationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClassificationJobs)

responseListClassificationScopes :: ListClassificationScopesResponse -> TestTree
responseListClassificationScopes =
  res
    "ListClassificationScopesResponse"
    "fixture/ListClassificationScopesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClassificationScopes)

responseListCustomDataIdentifiers :: ListCustomDataIdentifiersResponse -> TestTree
responseListCustomDataIdentifiers =
  res
    "ListCustomDataIdentifiersResponse"
    "fixture/ListCustomDataIdentifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomDataIdentifiers)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings =
  res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindings)

responseListFindingsFilters :: ListFindingsFiltersResponse -> TestTree
responseListFindingsFilters =
  res
    "ListFindingsFiltersResponse"
    "fixture/ListFindingsFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindingsFilters)

responseListInvitations :: ListInvitationsResponse -> TestTree
responseListInvitations =
  res
    "ListInvitationsResponse"
    "fixture/ListInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInvitations)

responseListManagedDataIdentifiers :: ListManagedDataIdentifiersResponse -> TestTree
responseListManagedDataIdentifiers =
  res
    "ListManagedDataIdentifiersResponse"
    "fixture/ListManagedDataIdentifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedDataIdentifiers)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers =
  res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMembers)

responseListOrganizationAdminAccounts :: ListOrganizationAdminAccountsResponse -> TestTree
responseListOrganizationAdminAccounts =
  res
    "ListOrganizationAdminAccountsResponse"
    "fixture/ListOrganizationAdminAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationAdminAccounts)

responseListResourceProfileArtifacts :: ListResourceProfileArtifactsResponse -> TestTree
responseListResourceProfileArtifacts =
  res
    "ListResourceProfileArtifactsResponse"
    "fixture/ListResourceProfileArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceProfileArtifacts)

responseListResourceProfileDetections :: ListResourceProfileDetectionsResponse -> TestTree
responseListResourceProfileDetections =
  res
    "ListResourceProfileDetectionsResponse"
    "fixture/ListResourceProfileDetectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceProfileDetections)

responseListSensitivityInspectionTemplates :: ListSensitivityInspectionTemplatesResponse -> TestTree
responseListSensitivityInspectionTemplates =
  res
    "ListSensitivityInspectionTemplatesResponse"
    "fixture/ListSensitivityInspectionTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSensitivityInspectionTemplates)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutClassificationExportConfiguration :: PutClassificationExportConfigurationResponse -> TestTree
responsePutClassificationExportConfiguration =
  res
    "PutClassificationExportConfigurationResponse"
    "fixture/PutClassificationExportConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutClassificationExportConfiguration)

responsePutFindingsPublicationConfiguration :: PutFindingsPublicationConfigurationResponse -> TestTree
responsePutFindingsPublicationConfiguration =
  res
    "PutFindingsPublicationConfigurationResponse"
    "fixture/PutFindingsPublicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFindingsPublicationConfiguration)

responseSearchResources :: SearchResourcesResponse -> TestTree
responseSearchResources =
  res
    "SearchResourcesResponse"
    "fixture/SearchResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchResources)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestCustomDataIdentifier :: TestCustomDataIdentifierResponse -> TestTree
responseTestCustomDataIdentifier =
  res
    "TestCustomDataIdentifierResponse"
    "fixture/TestCustomDataIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestCustomDataIdentifier)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAllowList :: UpdateAllowListResponse -> TestTree
responseUpdateAllowList =
  res
    "UpdateAllowListResponse"
    "fixture/UpdateAllowListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAllowList)

responseUpdateAutomatedDiscoveryConfiguration :: UpdateAutomatedDiscoveryConfigurationResponse -> TestTree
responseUpdateAutomatedDiscoveryConfiguration =
  res
    "UpdateAutomatedDiscoveryConfigurationResponse"
    "fixture/UpdateAutomatedDiscoveryConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAutomatedDiscoveryConfiguration)

responseUpdateClassificationJob :: UpdateClassificationJobResponse -> TestTree
responseUpdateClassificationJob =
  res
    "UpdateClassificationJobResponse"
    "fixture/UpdateClassificationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClassificationJob)

responseUpdateClassificationScope :: UpdateClassificationScopeResponse -> TestTree
responseUpdateClassificationScope =
  res
    "UpdateClassificationScopeResponse"
    "fixture/UpdateClassificationScopeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClassificationScope)

responseUpdateFindingsFilter :: UpdateFindingsFilterResponse -> TestTree
responseUpdateFindingsFilter =
  res
    "UpdateFindingsFilterResponse"
    "fixture/UpdateFindingsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFindingsFilter)

responseUpdateMacieSession :: UpdateMacieSessionResponse -> TestTree
responseUpdateMacieSession =
  res
    "UpdateMacieSessionResponse"
    "fixture/UpdateMacieSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMacieSession)

responseUpdateMemberSession :: UpdateMemberSessionResponse -> TestTree
responseUpdateMemberSession =
  res
    "UpdateMemberSessionResponse"
    "fixture/UpdateMemberSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMemberSession)

responseUpdateOrganizationConfiguration :: UpdateOrganizationConfigurationResponse -> TestTree
responseUpdateOrganizationConfiguration =
  res
    "UpdateOrganizationConfigurationResponse"
    "fixture/UpdateOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOrganizationConfiguration)

responseUpdateResourceProfile :: UpdateResourceProfileResponse -> TestTree
responseUpdateResourceProfile =
  res
    "UpdateResourceProfileResponse"
    "fixture/UpdateResourceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceProfile)

responseUpdateResourceProfileDetections :: UpdateResourceProfileDetectionsResponse -> TestTree
responseUpdateResourceProfileDetections =
  res
    "UpdateResourceProfileDetectionsResponse"
    "fixture/UpdateResourceProfileDetectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceProfileDetections)

responseUpdateRevealConfiguration :: UpdateRevealConfigurationResponse -> TestTree
responseUpdateRevealConfiguration =
  res
    "UpdateRevealConfigurationResponse"
    "fixture/UpdateRevealConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRevealConfiguration)

responseUpdateSensitivityInspectionTemplate :: UpdateSensitivityInspectionTemplateResponse -> TestTree
responseUpdateSensitivityInspectionTemplate =
  res
    "UpdateSensitivityInspectionTemplateResponse"
    "fixture/UpdateSensitivityInspectionTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSensitivityInspectionTemplate)
