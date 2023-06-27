{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.GuardDuty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.GuardDuty where

import Amazonka.GuardDuty
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.GuardDuty.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptAdministratorInvitation $
--             newAcceptAdministratorInvitation
--
--         , requestArchiveFindings $
--             newArchiveFindings
--
--         , requestCreateDetector $
--             newCreateDetector
--
--         , requestCreateFilter $
--             newCreateFilter
--
--         , requestCreateIPSet $
--             newCreateIPSet
--
--         , requestCreateMembers $
--             newCreateMembers
--
--         , requestCreatePublishingDestination $
--             newCreatePublishingDestination
--
--         , requestCreateSampleFindings $
--             newCreateSampleFindings
--
--         , requestCreateThreatIntelSet $
--             newCreateThreatIntelSet
--
--         , requestDeclineInvitations $
--             newDeclineInvitations
--
--         , requestDeleteDetector $
--             newDeleteDetector
--
--         , requestDeleteFilter $
--             newDeleteFilter
--
--         , requestDeleteIPSet $
--             newDeleteIPSet
--
--         , requestDeleteInvitations $
--             newDeleteInvitations
--
--         , requestDeleteMembers $
--             newDeleteMembers
--
--         , requestDeletePublishingDestination $
--             newDeletePublishingDestination
--
--         , requestDeleteThreatIntelSet $
--             newDeleteThreatIntelSet
--
--         , requestDescribeMalwareScans $
--             newDescribeMalwareScans
--
--         , requestDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfiguration
--
--         , requestDescribePublishingDestination $
--             newDescribePublishingDestination
--
--         , requestDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccount
--
--         , requestDisassociateFromAdministratorAccount $
--             newDisassociateFromAdministratorAccount
--
--         , requestDisassociateMembers $
--             newDisassociateMembers
--
--         , requestEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccount
--
--         , requestGetAdministratorAccount $
--             newGetAdministratorAccount
--
--         , requestGetCoverageStatistics $
--             newGetCoverageStatistics
--
--         , requestGetDetector $
--             newGetDetector
--
--         , requestGetFilter $
--             newGetFilter
--
--         , requestGetFindings $
--             newGetFindings
--
--         , requestGetFindingsStatistics $
--             newGetFindingsStatistics
--
--         , requestGetIPSet $
--             newGetIPSet
--
--         , requestGetInvitationsCount $
--             newGetInvitationsCount
--
--         , requestGetMalwareScanSettings $
--             newGetMalwareScanSettings
--
--         , requestGetMemberDetectors $
--             newGetMemberDetectors
--
--         , requestGetMembers $
--             newGetMembers
--
--         , requestGetRemainingFreeTrialDays $
--             newGetRemainingFreeTrialDays
--
--         , requestGetThreatIntelSet $
--             newGetThreatIntelSet
--
--         , requestGetUsageStatistics $
--             newGetUsageStatistics
--
--         , requestInviteMembers $
--             newInviteMembers
--
--         , requestListCoverage $
--             newListCoverage
--
--         , requestListDetectors $
--             newListDetectors
--
--         , requestListFilters $
--             newListFilters
--
--         , requestListFindings $
--             newListFindings
--
--         , requestListIPSets $
--             newListIPSets
--
--         , requestListInvitations $
--             newListInvitations
--
--         , requestListMembers $
--             newListMembers
--
--         , requestListOrganizationAdminAccounts $
--             newListOrganizationAdminAccounts
--
--         , requestListPublishingDestinations $
--             newListPublishingDestinations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListThreatIntelSets $
--             newListThreatIntelSets
--
--         , requestStartMalwareScan $
--             newStartMalwareScan
--
--         , requestStartMonitoringMembers $
--             newStartMonitoringMembers
--
--         , requestStopMonitoringMembers $
--             newStopMonitoringMembers
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUnarchiveFindings $
--             newUnarchiveFindings
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDetector $
--             newUpdateDetector
--
--         , requestUpdateFilter $
--             newUpdateFilter
--
--         , requestUpdateFindingsFeedback $
--             newUpdateFindingsFeedback
--
--         , requestUpdateIPSet $
--             newUpdateIPSet
--
--         , requestUpdateMalwareScanSettings $
--             newUpdateMalwareScanSettings
--
--         , requestUpdateMemberDetectors $
--             newUpdateMemberDetectors
--
--         , requestUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfiguration
--
--         , requestUpdatePublishingDestination $
--             newUpdatePublishingDestination
--
--         , requestUpdateThreatIntelSet $
--             newUpdateThreatIntelSet
--
--           ]

--     , testGroup "response"
--         [ responseAcceptAdministratorInvitation $
--             newAcceptAdministratorInvitationResponse
--
--         , responseArchiveFindings $
--             newArchiveFindingsResponse
--
--         , responseCreateDetector $
--             newCreateDetectorResponse
--
--         , responseCreateFilter $
--             newCreateFilterResponse
--
--         , responseCreateIPSet $
--             newCreateIPSetResponse
--
--         , responseCreateMembers $
--             newCreateMembersResponse
--
--         , responseCreatePublishingDestination $
--             newCreatePublishingDestinationResponse
--
--         , responseCreateSampleFindings $
--             newCreateSampleFindingsResponse
--
--         , responseCreateThreatIntelSet $
--             newCreateThreatIntelSetResponse
--
--         , responseDeclineInvitations $
--             newDeclineInvitationsResponse
--
--         , responseDeleteDetector $
--             newDeleteDetectorResponse
--
--         , responseDeleteFilter $
--             newDeleteFilterResponse
--
--         , responseDeleteIPSet $
--             newDeleteIPSetResponse
--
--         , responseDeleteInvitations $
--             newDeleteInvitationsResponse
--
--         , responseDeleteMembers $
--             newDeleteMembersResponse
--
--         , responseDeletePublishingDestination $
--             newDeletePublishingDestinationResponse
--
--         , responseDeleteThreatIntelSet $
--             newDeleteThreatIntelSetResponse
--
--         , responseDescribeMalwareScans $
--             newDescribeMalwareScansResponse
--
--         , responseDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfigurationResponse
--
--         , responseDescribePublishingDestination $
--             newDescribePublishingDestinationResponse
--
--         , responseDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccountResponse
--
--         , responseDisassociateFromAdministratorAccount $
--             newDisassociateFromAdministratorAccountResponse
--
--         , responseDisassociateMembers $
--             newDisassociateMembersResponse
--
--         , responseEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccountResponse
--
--         , responseGetAdministratorAccount $
--             newGetAdministratorAccountResponse
--
--         , responseGetCoverageStatistics $
--             newGetCoverageStatisticsResponse
--
--         , responseGetDetector $
--             newGetDetectorResponse
--
--         , responseGetFilter $
--             newGetFilterResponse
--
--         , responseGetFindings $
--             newGetFindingsResponse
--
--         , responseGetFindingsStatistics $
--             newGetFindingsStatisticsResponse
--
--         , responseGetIPSet $
--             newGetIPSetResponse
--
--         , responseGetInvitationsCount $
--             newGetInvitationsCountResponse
--
--         , responseGetMalwareScanSettings $
--             newGetMalwareScanSettingsResponse
--
--         , responseGetMemberDetectors $
--             newGetMemberDetectorsResponse
--
--         , responseGetMembers $
--             newGetMembersResponse
--
--         , responseGetRemainingFreeTrialDays $
--             newGetRemainingFreeTrialDaysResponse
--
--         , responseGetThreatIntelSet $
--             newGetThreatIntelSetResponse
--
--         , responseGetUsageStatistics $
--             newGetUsageStatisticsResponse
--
--         , responseInviteMembers $
--             newInviteMembersResponse
--
--         , responseListCoverage $
--             newListCoverageResponse
--
--         , responseListDetectors $
--             newListDetectorsResponse
--
--         , responseListFilters $
--             newListFiltersResponse
--
--         , responseListFindings $
--             newListFindingsResponse
--
--         , responseListIPSets $
--             newListIPSetsResponse
--
--         , responseListInvitations $
--             newListInvitationsResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseListOrganizationAdminAccounts $
--             newListOrganizationAdminAccountsResponse
--
--         , responseListPublishingDestinations $
--             newListPublishingDestinationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListThreatIntelSets $
--             newListThreatIntelSetsResponse
--
--         , responseStartMalwareScan $
--             newStartMalwareScanResponse
--
--         , responseStartMonitoringMembers $
--             newStartMonitoringMembersResponse
--
--         , responseStopMonitoringMembers $
--             newStopMonitoringMembersResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUnarchiveFindings $
--             newUnarchiveFindingsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDetector $
--             newUpdateDetectorResponse
--
--         , responseUpdateFilter $
--             newUpdateFilterResponse
--
--         , responseUpdateFindingsFeedback $
--             newUpdateFindingsFeedbackResponse
--
--         , responseUpdateIPSet $
--             newUpdateIPSetResponse
--
--         , responseUpdateMalwareScanSettings $
--             newUpdateMalwareScanSettingsResponse
--
--         , responseUpdateMemberDetectors $
--             newUpdateMemberDetectorsResponse
--
--         , responseUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfigurationResponse
--
--         , responseUpdatePublishingDestination $
--             newUpdatePublishingDestinationResponse
--
--         , responseUpdateThreatIntelSet $
--             newUpdateThreatIntelSetResponse
--
--           ]
--     ]

-- Requests

requestAcceptAdministratorInvitation :: AcceptAdministratorInvitation -> TestTree
requestAcceptAdministratorInvitation =
  req
    "AcceptAdministratorInvitation"
    "fixture/AcceptAdministratorInvitation.yaml"

requestArchiveFindings :: ArchiveFindings -> TestTree
requestArchiveFindings =
  req
    "ArchiveFindings"
    "fixture/ArchiveFindings.yaml"

requestCreateDetector :: CreateDetector -> TestTree
requestCreateDetector =
  req
    "CreateDetector"
    "fixture/CreateDetector.yaml"

requestCreateFilter :: CreateFilter -> TestTree
requestCreateFilter =
  req
    "CreateFilter"
    "fixture/CreateFilter.yaml"

requestCreateIPSet :: CreateIPSet -> TestTree
requestCreateIPSet =
  req
    "CreateIPSet"
    "fixture/CreateIPSet.yaml"

requestCreateMembers :: CreateMembers -> TestTree
requestCreateMembers =
  req
    "CreateMembers"
    "fixture/CreateMembers.yaml"

requestCreatePublishingDestination :: CreatePublishingDestination -> TestTree
requestCreatePublishingDestination =
  req
    "CreatePublishingDestination"
    "fixture/CreatePublishingDestination.yaml"

requestCreateSampleFindings :: CreateSampleFindings -> TestTree
requestCreateSampleFindings =
  req
    "CreateSampleFindings"
    "fixture/CreateSampleFindings.yaml"

requestCreateThreatIntelSet :: CreateThreatIntelSet -> TestTree
requestCreateThreatIntelSet =
  req
    "CreateThreatIntelSet"
    "fixture/CreateThreatIntelSet.yaml"

requestDeclineInvitations :: DeclineInvitations -> TestTree
requestDeclineInvitations =
  req
    "DeclineInvitations"
    "fixture/DeclineInvitations.yaml"

requestDeleteDetector :: DeleteDetector -> TestTree
requestDeleteDetector =
  req
    "DeleteDetector"
    "fixture/DeleteDetector.yaml"

requestDeleteFilter :: DeleteFilter -> TestTree
requestDeleteFilter =
  req
    "DeleteFilter"
    "fixture/DeleteFilter.yaml"

requestDeleteIPSet :: DeleteIPSet -> TestTree
requestDeleteIPSet =
  req
    "DeleteIPSet"
    "fixture/DeleteIPSet.yaml"

requestDeleteInvitations :: DeleteInvitations -> TestTree
requestDeleteInvitations =
  req
    "DeleteInvitations"
    "fixture/DeleteInvitations.yaml"

requestDeleteMembers :: DeleteMembers -> TestTree
requestDeleteMembers =
  req
    "DeleteMembers"
    "fixture/DeleteMembers.yaml"

requestDeletePublishingDestination :: DeletePublishingDestination -> TestTree
requestDeletePublishingDestination =
  req
    "DeletePublishingDestination"
    "fixture/DeletePublishingDestination.yaml"

requestDeleteThreatIntelSet :: DeleteThreatIntelSet -> TestTree
requestDeleteThreatIntelSet =
  req
    "DeleteThreatIntelSet"
    "fixture/DeleteThreatIntelSet.yaml"

requestDescribeMalwareScans :: DescribeMalwareScans -> TestTree
requestDescribeMalwareScans =
  req
    "DescribeMalwareScans"
    "fixture/DescribeMalwareScans.yaml"

requestDescribeOrganizationConfiguration :: DescribeOrganizationConfiguration -> TestTree
requestDescribeOrganizationConfiguration =
  req
    "DescribeOrganizationConfiguration"
    "fixture/DescribeOrganizationConfiguration.yaml"

requestDescribePublishingDestination :: DescribePublishingDestination -> TestTree
requestDescribePublishingDestination =
  req
    "DescribePublishingDestination"
    "fixture/DescribePublishingDestination.yaml"

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

requestDisassociateMembers :: DisassociateMembers -> TestTree
requestDisassociateMembers =
  req
    "DisassociateMembers"
    "fixture/DisassociateMembers.yaml"

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

requestGetCoverageStatistics :: GetCoverageStatistics -> TestTree
requestGetCoverageStatistics =
  req
    "GetCoverageStatistics"
    "fixture/GetCoverageStatistics.yaml"

requestGetDetector :: GetDetector -> TestTree
requestGetDetector =
  req
    "GetDetector"
    "fixture/GetDetector.yaml"

requestGetFilter :: GetFilter -> TestTree
requestGetFilter =
  req
    "GetFilter"
    "fixture/GetFilter.yaml"

requestGetFindings :: GetFindings -> TestTree
requestGetFindings =
  req
    "GetFindings"
    "fixture/GetFindings.yaml"

requestGetFindingsStatistics :: GetFindingsStatistics -> TestTree
requestGetFindingsStatistics =
  req
    "GetFindingsStatistics"
    "fixture/GetFindingsStatistics.yaml"

requestGetIPSet :: GetIPSet -> TestTree
requestGetIPSet =
  req
    "GetIPSet"
    "fixture/GetIPSet.yaml"

requestGetInvitationsCount :: GetInvitationsCount -> TestTree
requestGetInvitationsCount =
  req
    "GetInvitationsCount"
    "fixture/GetInvitationsCount.yaml"

requestGetMalwareScanSettings :: GetMalwareScanSettings -> TestTree
requestGetMalwareScanSettings =
  req
    "GetMalwareScanSettings"
    "fixture/GetMalwareScanSettings.yaml"

requestGetMemberDetectors :: GetMemberDetectors -> TestTree
requestGetMemberDetectors =
  req
    "GetMemberDetectors"
    "fixture/GetMemberDetectors.yaml"

requestGetMembers :: GetMembers -> TestTree
requestGetMembers =
  req
    "GetMembers"
    "fixture/GetMembers.yaml"

requestGetRemainingFreeTrialDays :: GetRemainingFreeTrialDays -> TestTree
requestGetRemainingFreeTrialDays =
  req
    "GetRemainingFreeTrialDays"
    "fixture/GetRemainingFreeTrialDays.yaml"

requestGetThreatIntelSet :: GetThreatIntelSet -> TestTree
requestGetThreatIntelSet =
  req
    "GetThreatIntelSet"
    "fixture/GetThreatIntelSet.yaml"

requestGetUsageStatistics :: GetUsageStatistics -> TestTree
requestGetUsageStatistics =
  req
    "GetUsageStatistics"
    "fixture/GetUsageStatistics.yaml"

requestInviteMembers :: InviteMembers -> TestTree
requestInviteMembers =
  req
    "InviteMembers"
    "fixture/InviteMembers.yaml"

requestListCoverage :: ListCoverage -> TestTree
requestListCoverage =
  req
    "ListCoverage"
    "fixture/ListCoverage.yaml"

requestListDetectors :: ListDetectors -> TestTree
requestListDetectors =
  req
    "ListDetectors"
    "fixture/ListDetectors.yaml"

requestListFilters :: ListFilters -> TestTree
requestListFilters =
  req
    "ListFilters"
    "fixture/ListFilters.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings =
  req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestListIPSets :: ListIPSets -> TestTree
requestListIPSets =
  req
    "ListIPSets"
    "fixture/ListIPSets.yaml"

requestListInvitations :: ListInvitations -> TestTree
requestListInvitations =
  req
    "ListInvitations"
    "fixture/ListInvitations.yaml"

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

requestListPublishingDestinations :: ListPublishingDestinations -> TestTree
requestListPublishingDestinations =
  req
    "ListPublishingDestinations"
    "fixture/ListPublishingDestinations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListThreatIntelSets :: ListThreatIntelSets -> TestTree
requestListThreatIntelSets =
  req
    "ListThreatIntelSets"
    "fixture/ListThreatIntelSets.yaml"

requestStartMalwareScan :: StartMalwareScan -> TestTree
requestStartMalwareScan =
  req
    "StartMalwareScan"
    "fixture/StartMalwareScan.yaml"

requestStartMonitoringMembers :: StartMonitoringMembers -> TestTree
requestStartMonitoringMembers =
  req
    "StartMonitoringMembers"
    "fixture/StartMonitoringMembers.yaml"

requestStopMonitoringMembers :: StopMonitoringMembers -> TestTree
requestStopMonitoringMembers =
  req
    "StopMonitoringMembers"
    "fixture/StopMonitoringMembers.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUnarchiveFindings :: UnarchiveFindings -> TestTree
requestUnarchiveFindings =
  req
    "UnarchiveFindings"
    "fixture/UnarchiveFindings.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateDetector :: UpdateDetector -> TestTree
requestUpdateDetector =
  req
    "UpdateDetector"
    "fixture/UpdateDetector.yaml"

requestUpdateFilter :: UpdateFilter -> TestTree
requestUpdateFilter =
  req
    "UpdateFilter"
    "fixture/UpdateFilter.yaml"

requestUpdateFindingsFeedback :: UpdateFindingsFeedback -> TestTree
requestUpdateFindingsFeedback =
  req
    "UpdateFindingsFeedback"
    "fixture/UpdateFindingsFeedback.yaml"

requestUpdateIPSet :: UpdateIPSet -> TestTree
requestUpdateIPSet =
  req
    "UpdateIPSet"
    "fixture/UpdateIPSet.yaml"

requestUpdateMalwareScanSettings :: UpdateMalwareScanSettings -> TestTree
requestUpdateMalwareScanSettings =
  req
    "UpdateMalwareScanSettings"
    "fixture/UpdateMalwareScanSettings.yaml"

requestUpdateMemberDetectors :: UpdateMemberDetectors -> TestTree
requestUpdateMemberDetectors =
  req
    "UpdateMemberDetectors"
    "fixture/UpdateMemberDetectors.yaml"

requestUpdateOrganizationConfiguration :: UpdateOrganizationConfiguration -> TestTree
requestUpdateOrganizationConfiguration =
  req
    "UpdateOrganizationConfiguration"
    "fixture/UpdateOrganizationConfiguration.yaml"

requestUpdatePublishingDestination :: UpdatePublishingDestination -> TestTree
requestUpdatePublishingDestination =
  req
    "UpdatePublishingDestination"
    "fixture/UpdatePublishingDestination.yaml"

requestUpdateThreatIntelSet :: UpdateThreatIntelSet -> TestTree
requestUpdateThreatIntelSet =
  req
    "UpdateThreatIntelSet"
    "fixture/UpdateThreatIntelSet.yaml"

-- Responses

responseAcceptAdministratorInvitation :: AcceptAdministratorInvitationResponse -> TestTree
responseAcceptAdministratorInvitation =
  res
    "AcceptAdministratorInvitationResponse"
    "fixture/AcceptAdministratorInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptAdministratorInvitation)

responseArchiveFindings :: ArchiveFindingsResponse -> TestTree
responseArchiveFindings =
  res
    "ArchiveFindingsResponse"
    "fixture/ArchiveFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ArchiveFindings)

responseCreateDetector :: CreateDetectorResponse -> TestTree
responseCreateDetector =
  res
    "CreateDetectorResponse"
    "fixture/CreateDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDetector)

responseCreateFilter :: CreateFilterResponse -> TestTree
responseCreateFilter =
  res
    "CreateFilterResponse"
    "fixture/CreateFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFilter)

responseCreateIPSet :: CreateIPSetResponse -> TestTree
responseCreateIPSet =
  res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIPSet)

responseCreateMembers :: CreateMembersResponse -> TestTree
responseCreateMembers =
  res
    "CreateMembersResponse"
    "fixture/CreateMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMembers)

responseCreatePublishingDestination :: CreatePublishingDestinationResponse -> TestTree
responseCreatePublishingDestination =
  res
    "CreatePublishingDestinationResponse"
    "fixture/CreatePublishingDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePublishingDestination)

responseCreateSampleFindings :: CreateSampleFindingsResponse -> TestTree
responseCreateSampleFindings =
  res
    "CreateSampleFindingsResponse"
    "fixture/CreateSampleFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSampleFindings)

responseCreateThreatIntelSet :: CreateThreatIntelSetResponse -> TestTree
responseCreateThreatIntelSet =
  res
    "CreateThreatIntelSetResponse"
    "fixture/CreateThreatIntelSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThreatIntelSet)

responseDeclineInvitations :: DeclineInvitationsResponse -> TestTree
responseDeclineInvitations =
  res
    "DeclineInvitationsResponse"
    "fixture/DeclineInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeclineInvitations)

responseDeleteDetector :: DeleteDetectorResponse -> TestTree
responseDeleteDetector =
  res
    "DeleteDetectorResponse"
    "fixture/DeleteDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDetector)

responseDeleteFilter :: DeleteFilterResponse -> TestTree
responseDeleteFilter =
  res
    "DeleteFilterResponse"
    "fixture/DeleteFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFilter)

responseDeleteIPSet :: DeleteIPSetResponse -> TestTree
responseDeleteIPSet =
  res
    "DeleteIPSetResponse"
    "fixture/DeleteIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIPSet)

responseDeleteInvitations :: DeleteInvitationsResponse -> TestTree
responseDeleteInvitations =
  res
    "DeleteInvitationsResponse"
    "fixture/DeleteInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInvitations)

responseDeleteMembers :: DeleteMembersResponse -> TestTree
responseDeleteMembers =
  res
    "DeleteMembersResponse"
    "fixture/DeleteMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMembers)

responseDeletePublishingDestination :: DeletePublishingDestinationResponse -> TestTree
responseDeletePublishingDestination =
  res
    "DeletePublishingDestinationResponse"
    "fixture/DeletePublishingDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePublishingDestination)

responseDeleteThreatIntelSet :: DeleteThreatIntelSetResponse -> TestTree
responseDeleteThreatIntelSet =
  res
    "DeleteThreatIntelSetResponse"
    "fixture/DeleteThreatIntelSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThreatIntelSet)

responseDescribeMalwareScans :: DescribeMalwareScansResponse -> TestTree
responseDescribeMalwareScans =
  res
    "DescribeMalwareScansResponse"
    "fixture/DescribeMalwareScansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMalwareScans)

responseDescribeOrganizationConfiguration :: DescribeOrganizationConfigurationResponse -> TestTree
responseDescribeOrganizationConfiguration =
  res
    "DescribeOrganizationConfigurationResponse"
    "fixture/DescribeOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfiguration)

responseDescribePublishingDestination :: DescribePublishingDestinationResponse -> TestTree
responseDescribePublishingDestination =
  res
    "DescribePublishingDestinationResponse"
    "fixture/DescribePublishingDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePublishingDestination)

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

responseDisassociateMembers :: DisassociateMembersResponse -> TestTree
responseDisassociateMembers =
  res
    "DisassociateMembersResponse"
    "fixture/DisassociateMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMembers)

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

responseGetCoverageStatistics :: GetCoverageStatisticsResponse -> TestTree
responseGetCoverageStatistics =
  res
    "GetCoverageStatisticsResponse"
    "fixture/GetCoverageStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoverageStatistics)

responseGetDetector :: GetDetectorResponse -> TestTree
responseGetDetector =
  res
    "GetDetectorResponse"
    "fixture/GetDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDetector)

responseGetFilter :: GetFilterResponse -> TestTree
responseGetFilter =
  res
    "GetFilterResponse"
    "fixture/GetFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFilter)

responseGetFindings :: GetFindingsResponse -> TestTree
responseGetFindings =
  res
    "GetFindingsResponse"
    "fixture/GetFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindings)

responseGetFindingsStatistics :: GetFindingsStatisticsResponse -> TestTree
responseGetFindingsStatistics =
  res
    "GetFindingsStatisticsResponse"
    "fixture/GetFindingsStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingsStatistics)

responseGetIPSet :: GetIPSetResponse -> TestTree
responseGetIPSet =
  res
    "GetIPSetResponse"
    "fixture/GetIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIPSet)

responseGetInvitationsCount :: GetInvitationsCountResponse -> TestTree
responseGetInvitationsCount =
  res
    "GetInvitationsCountResponse"
    "fixture/GetInvitationsCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInvitationsCount)

responseGetMalwareScanSettings :: GetMalwareScanSettingsResponse -> TestTree
responseGetMalwareScanSettings =
  res
    "GetMalwareScanSettingsResponse"
    "fixture/GetMalwareScanSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMalwareScanSettings)

responseGetMemberDetectors :: GetMemberDetectorsResponse -> TestTree
responseGetMemberDetectors =
  res
    "GetMemberDetectorsResponse"
    "fixture/GetMemberDetectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMemberDetectors)

responseGetMembers :: GetMembersResponse -> TestTree
responseGetMembers =
  res
    "GetMembersResponse"
    "fixture/GetMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMembers)

responseGetRemainingFreeTrialDays :: GetRemainingFreeTrialDaysResponse -> TestTree
responseGetRemainingFreeTrialDays =
  res
    "GetRemainingFreeTrialDaysResponse"
    "fixture/GetRemainingFreeTrialDaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRemainingFreeTrialDays)

responseGetThreatIntelSet :: GetThreatIntelSetResponse -> TestTree
responseGetThreatIntelSet =
  res
    "GetThreatIntelSetResponse"
    "fixture/GetThreatIntelSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetThreatIntelSet)

responseGetUsageStatistics :: GetUsageStatisticsResponse -> TestTree
responseGetUsageStatistics =
  res
    "GetUsageStatisticsResponse"
    "fixture/GetUsageStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsageStatistics)

responseInviteMembers :: InviteMembersResponse -> TestTree
responseInviteMembers =
  res
    "InviteMembersResponse"
    "fixture/InviteMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InviteMembers)

responseListCoverage :: ListCoverageResponse -> TestTree
responseListCoverage =
  res
    "ListCoverageResponse"
    "fixture/ListCoverageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCoverage)

responseListDetectors :: ListDetectorsResponse -> TestTree
responseListDetectors =
  res
    "ListDetectorsResponse"
    "fixture/ListDetectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectors)

responseListFilters :: ListFiltersResponse -> TestTree
responseListFilters =
  res
    "ListFiltersResponse"
    "fixture/ListFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFilters)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings =
  res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindings)

responseListIPSets :: ListIPSetsResponse -> TestTree
responseListIPSets =
  res
    "ListIPSetsResponse"
    "fixture/ListIPSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIPSets)

responseListInvitations :: ListInvitationsResponse -> TestTree
responseListInvitations =
  res
    "ListInvitationsResponse"
    "fixture/ListInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInvitations)

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

responseListPublishingDestinations :: ListPublishingDestinationsResponse -> TestTree
responseListPublishingDestinations =
  res
    "ListPublishingDestinationsResponse"
    "fixture/ListPublishingDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPublishingDestinations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListThreatIntelSets :: ListThreatIntelSetsResponse -> TestTree
responseListThreatIntelSets =
  res
    "ListThreatIntelSetsResponse"
    "fixture/ListThreatIntelSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThreatIntelSets)

responseStartMalwareScan :: StartMalwareScanResponse -> TestTree
responseStartMalwareScan =
  res
    "StartMalwareScanResponse"
    "fixture/StartMalwareScanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMalwareScan)

responseStartMonitoringMembers :: StartMonitoringMembersResponse -> TestTree
responseStartMonitoringMembers =
  res
    "StartMonitoringMembersResponse"
    "fixture/StartMonitoringMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMonitoringMembers)

responseStopMonitoringMembers :: StopMonitoringMembersResponse -> TestTree
responseStopMonitoringMembers =
  res
    "StopMonitoringMembersResponse"
    "fixture/StopMonitoringMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopMonitoringMembers)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUnarchiveFindings :: UnarchiveFindingsResponse -> TestTree
responseUnarchiveFindings =
  res
    "UnarchiveFindingsResponse"
    "fixture/UnarchiveFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnarchiveFindings)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateDetector :: UpdateDetectorResponse -> TestTree
responseUpdateDetector =
  res
    "UpdateDetectorResponse"
    "fixture/UpdateDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDetector)

responseUpdateFilter :: UpdateFilterResponse -> TestTree
responseUpdateFilter =
  res
    "UpdateFilterResponse"
    "fixture/UpdateFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFilter)

responseUpdateFindingsFeedback :: UpdateFindingsFeedbackResponse -> TestTree
responseUpdateFindingsFeedback =
  res
    "UpdateFindingsFeedbackResponse"
    "fixture/UpdateFindingsFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFindingsFeedback)

responseUpdateIPSet :: UpdateIPSetResponse -> TestTree
responseUpdateIPSet =
  res
    "UpdateIPSetResponse"
    "fixture/UpdateIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIPSet)

responseUpdateMalwareScanSettings :: UpdateMalwareScanSettingsResponse -> TestTree
responseUpdateMalwareScanSettings =
  res
    "UpdateMalwareScanSettingsResponse"
    "fixture/UpdateMalwareScanSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMalwareScanSettings)

responseUpdateMemberDetectors :: UpdateMemberDetectorsResponse -> TestTree
responseUpdateMemberDetectors =
  res
    "UpdateMemberDetectorsResponse"
    "fixture/UpdateMemberDetectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMemberDetectors)

responseUpdateOrganizationConfiguration :: UpdateOrganizationConfigurationResponse -> TestTree
responseUpdateOrganizationConfiguration =
  res
    "UpdateOrganizationConfigurationResponse"
    "fixture/UpdateOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOrganizationConfiguration)

responseUpdatePublishingDestination :: UpdatePublishingDestinationResponse -> TestTree
responseUpdatePublishingDestination =
  res
    "UpdatePublishingDestinationResponse"
    "fixture/UpdatePublishingDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePublishingDestination)

responseUpdateThreatIntelSet :: UpdateThreatIntelSetResponse -> TestTree
responseUpdateThreatIntelSet =
  res
    "UpdateThreatIntelSetResponse"
    "fixture/UpdateThreatIntelSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThreatIntelSet)
