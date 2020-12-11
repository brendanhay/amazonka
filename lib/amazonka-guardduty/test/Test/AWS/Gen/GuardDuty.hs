{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.GuardDuty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.GuardDuty where

import Data.Proxy
import Network.AWS.GuardDuty
import Test.AWS.Fixture
import Test.AWS.GuardDuty.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateFilter $
--             mkCreateFilter
--
--         , requestEnableOrganizationAdminAccount $
--             mkEnableOrganizationAdminAccount
--
--         , requestListFindings $
--             mkListFindings
--
--         , requestListOrganizationAdminAccounts $
--             mkListOrganizationAdminAccounts
--
--         , requestCreateIPSet $
--             mkCreateIPSet
--
--         , requestDeleteThreatIntelSet $
--             mkDeleteThreatIntelSet
--
--         , requestUpdateThreatIntelSet $
--             mkUpdateThreatIntelSet
--
--         , requestStopMonitoringMembers $
--             mkStopMonitoringMembers
--
--         , requestListThreatIntelSets $
--             mkListThreatIntelSets
--
--         , requestCreateThreatIntelSet $
--             mkCreateThreatIntelSet
--
--         , requestDeleteMembers $
--             mkDeleteMembers
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetFindingsStatistics $
--             mkGetFindingsStatistics
--
--         , requestGetIPSet $
--             mkGetIPSet
--
--         , requestListInvitations $
--             mkListInvitations
--
--         , requestUpdateMemberDetectors $
--             mkUpdateMemberDetectors
--
--         , requestGetThreatIntelSet $
--             mkGetThreatIntelSet
--
--         , requestDeleteInvitations $
--             mkDeleteInvitations
--
--         , requestGetMasterAccount $
--             mkGetMasterAccount
--
--         , requestGetUsageStatistics $
--             mkGetUsageStatistics
--
--         , requestCreateDetector $
--             mkCreateDetector
--
--         , requestDeclineInvitations $
--             mkDeclineInvitations
--
--         , requestDescribeOrganizationConfiguration $
--             mkDescribeOrganizationConfiguration
--
--         , requestCreatePublishingDestination $
--             mkCreatePublishingDestination
--
--         , requestUpdateFilter $
--             mkUpdateFilter
--
--         , requestDeleteFilter $
--             mkDeleteFilter
--
--         , requestDisassociateMembers $
--             mkDisassociateMembers
--
--         , requestDisassociateFromMasterAccount $
--             mkDisassociateFromMasterAccount
--
--         , requestAcceptInvitation $
--             mkAcceptInvitation
--
--         , requestListFilters $
--             mkListFilters
--
--         , requestListMembers $
--             mkListMembers
--
--         , requestListPublishingDestinations $
--             mkListPublishingDestinations
--
--         , requestDeletePublishingDestination $
--             mkDeletePublishingDestination
--
--         , requestUpdatePublishingDestination $
--             mkUpdatePublishingDestination
--
--         , requestGetDetector $
--             mkGetDetector
--
--         , requestCreateSampleFindings $
--             mkCreateSampleFindings
--
--         , requestArchiveFindings $
--             mkArchiveFindings
--
--         , requestCreateMembers $
--             mkCreateMembers
--
--         , requestUnarchiveFindings $
--             mkUnarchiveFindings
--
--         , requestGetMemberDetectors $
--             mkGetMemberDetectors
--
--         , requestGetInvitationsCount $
--             mkGetInvitationsCount
--
--         , requestStartMonitoringMembers $
--             mkStartMonitoringMembers
--
--         , requestUpdateOrganizationConfiguration $
--             mkUpdateOrganizationConfiguration
--
--         , requestInviteMembers $
--             mkInviteMembers
--
--         , requestDeleteIPSet $
--             mkDeleteIPSet
--
--         , requestUpdateIPSet $
--             mkUpdateIPSet
--
--         , requestListIPSets $
--             mkListIPSets
--
--         , requestGetMembers $
--             mkGetMembers
--
--         , requestDescribePublishingDestination $
--             mkDescribePublishingDestination
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestGetFindings $
--             mkGetFindings
--
--         , requestListDetectors $
--             mkListDetectors
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestUpdateDetector $
--             mkUpdateDetector
--
--         , requestDeleteDetector $
--             mkDeleteDetector
--
--         , requestUpdateFindingsFeedback $
--             mkUpdateFindingsFeedback
--
--         , requestGetFilter $
--             mkGetFilter
--
--         , requestDisableOrganizationAdminAccount $
--             mkDisableOrganizationAdminAccount
--
--           ]

--     , testGroup "response"
--         [ responseCreateFilter $
--             mkCreateFilterResponse
--
--         , responseEnableOrganizationAdminAccount $
--             mkEnableOrganizationAdminAccountResponse
--
--         , responseListFindings $
--             mkListFindingsResponse
--
--         , responseListOrganizationAdminAccounts $
--             mkListOrganizationAdminAccountsResponse
--
--         , responseCreateIPSet $
--             mkCreateIPSetResponse
--
--         , responseDeleteThreatIntelSet $
--             mkDeleteThreatIntelSetResponse
--
--         , responseUpdateThreatIntelSet $
--             mkUpdateThreatIntelSetResponse
--
--         , responseStopMonitoringMembers $
--             mkStopMonitoringMembersResponse
--
--         , responseListThreatIntelSets $
--             mkListThreatIntelSetsResponse
--
--         , responseCreateThreatIntelSet $
--             mkCreateThreatIntelSetResponse
--
--         , responseDeleteMembers $
--             mkDeleteMembersResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetFindingsStatistics $
--             mkGetFindingsStatisticsResponse
--
--         , responseGetIPSet $
--             mkGetIPSetResponse
--
--         , responseListInvitations $
--             mkListInvitationsResponse
--
--         , responseUpdateMemberDetectors $
--             mkUpdateMemberDetectorsResponse
--
--         , responseGetThreatIntelSet $
--             mkGetThreatIntelSetResponse
--
--         , responseDeleteInvitations $
--             mkDeleteInvitationsResponse
--
--         , responseGetMasterAccount $
--             mkGetMasterAccountResponse
--
--         , responseGetUsageStatistics $
--             mkGetUsageStatisticsResponse
--
--         , responseCreateDetector $
--             mkCreateDetectorResponse
--
--         , responseDeclineInvitations $
--             mkDeclineInvitationsResponse
--
--         , responseDescribeOrganizationConfiguration $
--             mkDescribeOrganizationConfigurationResponse
--
--         , responseCreatePublishingDestination $
--             mkCreatePublishingDestinationResponse
--
--         , responseUpdateFilter $
--             mkUpdateFilterResponse
--
--         , responseDeleteFilter $
--             mkDeleteFilterResponse
--
--         , responseDisassociateMembers $
--             mkDisassociateMembersResponse
--
--         , responseDisassociateFromMasterAccount $
--             mkDisassociateFromMasterAccountResponse
--
--         , responseAcceptInvitation $
--             mkAcceptInvitationResponse
--
--         , responseListFilters $
--             mkListFiltersResponse
--
--         , responseListMembers $
--             mkListMembersResponse
--
--         , responseListPublishingDestinations $
--             mkListPublishingDestinationsResponse
--
--         , responseDeletePublishingDestination $
--             mkDeletePublishingDestinationResponse
--
--         , responseUpdatePublishingDestination $
--             mkUpdatePublishingDestinationResponse
--
--         , responseGetDetector $
--             mkGetDetectorResponse
--
--         , responseCreateSampleFindings $
--             mkCreateSampleFindingsResponse
--
--         , responseArchiveFindings $
--             mkArchiveFindingsResponse
--
--         , responseCreateMembers $
--             mkCreateMembersResponse
--
--         , responseUnarchiveFindings $
--             mkUnarchiveFindingsResponse
--
--         , responseGetMemberDetectors $
--             mkGetMemberDetectorsResponse
--
--         , responseGetInvitationsCount $
--             mkGetInvitationsCountResponse
--
--         , responseStartMonitoringMembers $
--             mkStartMonitoringMembersResponse
--
--         , responseUpdateOrganizationConfiguration $
--             mkUpdateOrganizationConfigurationResponse
--
--         , responseInviteMembers $
--             mkInviteMembersResponse
--
--         , responseDeleteIPSet $
--             mkDeleteIPSetResponse
--
--         , responseUpdateIPSet $
--             mkUpdateIPSetResponse
--
--         , responseListIPSets $
--             mkListIPSetsResponse
--
--         , responseGetMembers $
--             mkGetMembersResponse
--
--         , responseDescribePublishingDestination $
--             mkDescribePublishingDestinationResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseGetFindings $
--             mkGetFindingsResponse
--
--         , responseListDetectors $
--             mkListDetectorsResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseUpdateDetector $
--             mkUpdateDetectorResponse
--
--         , responseDeleteDetector $
--             mkDeleteDetectorResponse
--
--         , responseUpdateFindingsFeedback $
--             mkUpdateFindingsFeedbackResponse
--
--         , responseGetFilter $
--             mkGetFilterResponse
--
--         , responseDisableOrganizationAdminAccount $
--             mkDisableOrganizationAdminAccountResponse
--
--           ]
--     ]

-- Requests

requestCreateFilter :: CreateFilter -> TestTree
requestCreateFilter =
  req
    "CreateFilter"
    "fixture/CreateFilter.yaml"

requestEnableOrganizationAdminAccount :: EnableOrganizationAdminAccount -> TestTree
requestEnableOrganizationAdminAccount =
  req
    "EnableOrganizationAdminAccount"
    "fixture/EnableOrganizationAdminAccount.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings =
  req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestListOrganizationAdminAccounts :: ListOrganizationAdminAccounts -> TestTree
requestListOrganizationAdminAccounts =
  req
    "ListOrganizationAdminAccounts"
    "fixture/ListOrganizationAdminAccounts.yaml"

requestCreateIPSet :: CreateIPSet -> TestTree
requestCreateIPSet =
  req
    "CreateIPSet"
    "fixture/CreateIPSet.yaml"

requestDeleteThreatIntelSet :: DeleteThreatIntelSet -> TestTree
requestDeleteThreatIntelSet =
  req
    "DeleteThreatIntelSet"
    "fixture/DeleteThreatIntelSet.yaml"

requestUpdateThreatIntelSet :: UpdateThreatIntelSet -> TestTree
requestUpdateThreatIntelSet =
  req
    "UpdateThreatIntelSet"
    "fixture/UpdateThreatIntelSet.yaml"

requestStopMonitoringMembers :: StopMonitoringMembers -> TestTree
requestStopMonitoringMembers =
  req
    "StopMonitoringMembers"
    "fixture/StopMonitoringMembers.yaml"

requestListThreatIntelSets :: ListThreatIntelSets -> TestTree
requestListThreatIntelSets =
  req
    "ListThreatIntelSets"
    "fixture/ListThreatIntelSets.yaml"

requestCreateThreatIntelSet :: CreateThreatIntelSet -> TestTree
requestCreateThreatIntelSet =
  req
    "CreateThreatIntelSet"
    "fixture/CreateThreatIntelSet.yaml"

requestDeleteMembers :: DeleteMembers -> TestTree
requestDeleteMembers =
  req
    "DeleteMembers"
    "fixture/DeleteMembers.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestListInvitations :: ListInvitations -> TestTree
requestListInvitations =
  req
    "ListInvitations"
    "fixture/ListInvitations.yaml"

requestUpdateMemberDetectors :: UpdateMemberDetectors -> TestTree
requestUpdateMemberDetectors =
  req
    "UpdateMemberDetectors"
    "fixture/UpdateMemberDetectors.yaml"

requestGetThreatIntelSet :: GetThreatIntelSet -> TestTree
requestGetThreatIntelSet =
  req
    "GetThreatIntelSet"
    "fixture/GetThreatIntelSet.yaml"

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

requestGetUsageStatistics :: GetUsageStatistics -> TestTree
requestGetUsageStatistics =
  req
    "GetUsageStatistics"
    "fixture/GetUsageStatistics.yaml"

requestCreateDetector :: CreateDetector -> TestTree
requestCreateDetector =
  req
    "CreateDetector"
    "fixture/CreateDetector.yaml"

requestDeclineInvitations :: DeclineInvitations -> TestTree
requestDeclineInvitations =
  req
    "DeclineInvitations"
    "fixture/DeclineInvitations.yaml"

requestDescribeOrganizationConfiguration :: DescribeOrganizationConfiguration -> TestTree
requestDescribeOrganizationConfiguration =
  req
    "DescribeOrganizationConfiguration"
    "fixture/DescribeOrganizationConfiguration.yaml"

requestCreatePublishingDestination :: CreatePublishingDestination -> TestTree
requestCreatePublishingDestination =
  req
    "CreatePublishingDestination"
    "fixture/CreatePublishingDestination.yaml"

requestUpdateFilter :: UpdateFilter -> TestTree
requestUpdateFilter =
  req
    "UpdateFilter"
    "fixture/UpdateFilter.yaml"

requestDeleteFilter :: DeleteFilter -> TestTree
requestDeleteFilter =
  req
    "DeleteFilter"
    "fixture/DeleteFilter.yaml"

requestDisassociateMembers :: DisassociateMembers -> TestTree
requestDisassociateMembers =
  req
    "DisassociateMembers"
    "fixture/DisassociateMembers.yaml"

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

requestListFilters :: ListFilters -> TestTree
requestListFilters =
  req
    "ListFilters"
    "fixture/ListFilters.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers =
  req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestListPublishingDestinations :: ListPublishingDestinations -> TestTree
requestListPublishingDestinations =
  req
    "ListPublishingDestinations"
    "fixture/ListPublishingDestinations.yaml"

requestDeletePublishingDestination :: DeletePublishingDestination -> TestTree
requestDeletePublishingDestination =
  req
    "DeletePublishingDestination"
    "fixture/DeletePublishingDestination.yaml"

requestUpdatePublishingDestination :: UpdatePublishingDestination -> TestTree
requestUpdatePublishingDestination =
  req
    "UpdatePublishingDestination"
    "fixture/UpdatePublishingDestination.yaml"

requestGetDetector :: GetDetector -> TestTree
requestGetDetector =
  req
    "GetDetector"
    "fixture/GetDetector.yaml"

requestCreateSampleFindings :: CreateSampleFindings -> TestTree
requestCreateSampleFindings =
  req
    "CreateSampleFindings"
    "fixture/CreateSampleFindings.yaml"

requestArchiveFindings :: ArchiveFindings -> TestTree
requestArchiveFindings =
  req
    "ArchiveFindings"
    "fixture/ArchiveFindings.yaml"

requestCreateMembers :: CreateMembers -> TestTree
requestCreateMembers =
  req
    "CreateMembers"
    "fixture/CreateMembers.yaml"

requestUnarchiveFindings :: UnarchiveFindings -> TestTree
requestUnarchiveFindings =
  req
    "UnarchiveFindings"
    "fixture/UnarchiveFindings.yaml"

requestGetMemberDetectors :: GetMemberDetectors -> TestTree
requestGetMemberDetectors =
  req
    "GetMemberDetectors"
    "fixture/GetMemberDetectors.yaml"

requestGetInvitationsCount :: GetInvitationsCount -> TestTree
requestGetInvitationsCount =
  req
    "GetInvitationsCount"
    "fixture/GetInvitationsCount.yaml"

requestStartMonitoringMembers :: StartMonitoringMembers -> TestTree
requestStartMonitoringMembers =
  req
    "StartMonitoringMembers"
    "fixture/StartMonitoringMembers.yaml"

requestUpdateOrganizationConfiguration :: UpdateOrganizationConfiguration -> TestTree
requestUpdateOrganizationConfiguration =
  req
    "UpdateOrganizationConfiguration"
    "fixture/UpdateOrganizationConfiguration.yaml"

requestInviteMembers :: InviteMembers -> TestTree
requestInviteMembers =
  req
    "InviteMembers"
    "fixture/InviteMembers.yaml"

requestDeleteIPSet :: DeleteIPSet -> TestTree
requestDeleteIPSet =
  req
    "DeleteIPSet"
    "fixture/DeleteIPSet.yaml"

requestUpdateIPSet :: UpdateIPSet -> TestTree
requestUpdateIPSet =
  req
    "UpdateIPSet"
    "fixture/UpdateIPSet.yaml"

requestListIPSets :: ListIPSets -> TestTree
requestListIPSets =
  req
    "ListIPSets"
    "fixture/ListIPSets.yaml"

requestGetMembers :: GetMembers -> TestTree
requestGetMembers =
  req
    "GetMembers"
    "fixture/GetMembers.yaml"

requestDescribePublishingDestination :: DescribePublishingDestination -> TestTree
requestDescribePublishingDestination =
  req
    "DescribePublishingDestination"
    "fixture/DescribePublishingDestination.yaml"

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

requestListDetectors :: ListDetectors -> TestTree
requestListDetectors =
  req
    "ListDetectors"
    "fixture/ListDetectors.yaml"

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

requestDeleteDetector :: DeleteDetector -> TestTree
requestDeleteDetector =
  req
    "DeleteDetector"
    "fixture/DeleteDetector.yaml"

requestUpdateFindingsFeedback :: UpdateFindingsFeedback -> TestTree
requestUpdateFindingsFeedback =
  req
    "UpdateFindingsFeedback"
    "fixture/UpdateFindingsFeedback.yaml"

requestGetFilter :: GetFilter -> TestTree
requestGetFilter =
  req
    "GetFilter"
    "fixture/GetFilter.yaml"

requestDisableOrganizationAdminAccount :: DisableOrganizationAdminAccount -> TestTree
requestDisableOrganizationAdminAccount =
  req
    "DisableOrganizationAdminAccount"
    "fixture/DisableOrganizationAdminAccount.yaml"

-- Responses

responseCreateFilter :: CreateFilterResponse -> TestTree
responseCreateFilter =
  res
    "CreateFilterResponse"
    "fixture/CreateFilterResponse.proto"
    guardDutyService
    (Proxy :: Proxy CreateFilter)

responseEnableOrganizationAdminAccount :: EnableOrganizationAdminAccountResponse -> TestTree
responseEnableOrganizationAdminAccount =
  res
    "EnableOrganizationAdminAccountResponse"
    "fixture/EnableOrganizationAdminAccountResponse.proto"
    guardDutyService
    (Proxy :: Proxy EnableOrganizationAdminAccount)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings =
  res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    guardDutyService
    (Proxy :: Proxy ListFindings)

responseListOrganizationAdminAccounts :: ListOrganizationAdminAccountsResponse -> TestTree
responseListOrganizationAdminAccounts =
  res
    "ListOrganizationAdminAccountsResponse"
    "fixture/ListOrganizationAdminAccountsResponse.proto"
    guardDutyService
    (Proxy :: Proxy ListOrganizationAdminAccounts)

responseCreateIPSet :: CreateIPSetResponse -> TestTree
responseCreateIPSet =
  res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    guardDutyService
    (Proxy :: Proxy CreateIPSet)

responseDeleteThreatIntelSet :: DeleteThreatIntelSetResponse -> TestTree
responseDeleteThreatIntelSet =
  res
    "DeleteThreatIntelSetResponse"
    "fixture/DeleteThreatIntelSetResponse.proto"
    guardDutyService
    (Proxy :: Proxy DeleteThreatIntelSet)

responseUpdateThreatIntelSet :: UpdateThreatIntelSetResponse -> TestTree
responseUpdateThreatIntelSet =
  res
    "UpdateThreatIntelSetResponse"
    "fixture/UpdateThreatIntelSetResponse.proto"
    guardDutyService
    (Proxy :: Proxy UpdateThreatIntelSet)

responseStopMonitoringMembers :: StopMonitoringMembersResponse -> TestTree
responseStopMonitoringMembers =
  res
    "StopMonitoringMembersResponse"
    "fixture/StopMonitoringMembersResponse.proto"
    guardDutyService
    (Proxy :: Proxy StopMonitoringMembers)

responseListThreatIntelSets :: ListThreatIntelSetsResponse -> TestTree
responseListThreatIntelSets =
  res
    "ListThreatIntelSetsResponse"
    "fixture/ListThreatIntelSetsResponse.proto"
    guardDutyService
    (Proxy :: Proxy ListThreatIntelSets)

responseCreateThreatIntelSet :: CreateThreatIntelSetResponse -> TestTree
responseCreateThreatIntelSet =
  res
    "CreateThreatIntelSetResponse"
    "fixture/CreateThreatIntelSetResponse.proto"
    guardDutyService
    (Proxy :: Proxy CreateThreatIntelSet)

responseDeleteMembers :: DeleteMembersResponse -> TestTree
responseDeleteMembers =
  res
    "DeleteMembersResponse"
    "fixture/DeleteMembersResponse.proto"
    guardDutyService
    (Proxy :: Proxy DeleteMembers)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    guardDutyService
    (Proxy :: Proxy ListTagsForResource)

responseGetFindingsStatistics :: GetFindingsStatisticsResponse -> TestTree
responseGetFindingsStatistics =
  res
    "GetFindingsStatisticsResponse"
    "fixture/GetFindingsStatisticsResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetFindingsStatistics)

responseGetIPSet :: GetIPSetResponse -> TestTree
responseGetIPSet =
  res
    "GetIPSetResponse"
    "fixture/GetIPSetResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetIPSet)

responseListInvitations :: ListInvitationsResponse -> TestTree
responseListInvitations =
  res
    "ListInvitationsResponse"
    "fixture/ListInvitationsResponse.proto"
    guardDutyService
    (Proxy :: Proxy ListInvitations)

responseUpdateMemberDetectors :: UpdateMemberDetectorsResponse -> TestTree
responseUpdateMemberDetectors =
  res
    "UpdateMemberDetectorsResponse"
    "fixture/UpdateMemberDetectorsResponse.proto"
    guardDutyService
    (Proxy :: Proxy UpdateMemberDetectors)

responseGetThreatIntelSet :: GetThreatIntelSetResponse -> TestTree
responseGetThreatIntelSet =
  res
    "GetThreatIntelSetResponse"
    "fixture/GetThreatIntelSetResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetThreatIntelSet)

responseDeleteInvitations :: DeleteInvitationsResponse -> TestTree
responseDeleteInvitations =
  res
    "DeleteInvitationsResponse"
    "fixture/DeleteInvitationsResponse.proto"
    guardDutyService
    (Proxy :: Proxy DeleteInvitations)

responseGetMasterAccount :: GetMasterAccountResponse -> TestTree
responseGetMasterAccount =
  res
    "GetMasterAccountResponse"
    "fixture/GetMasterAccountResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetMasterAccount)

responseGetUsageStatistics :: GetUsageStatisticsResponse -> TestTree
responseGetUsageStatistics =
  res
    "GetUsageStatisticsResponse"
    "fixture/GetUsageStatisticsResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetUsageStatistics)

responseCreateDetector :: CreateDetectorResponse -> TestTree
responseCreateDetector =
  res
    "CreateDetectorResponse"
    "fixture/CreateDetectorResponse.proto"
    guardDutyService
    (Proxy :: Proxy CreateDetector)

responseDeclineInvitations :: DeclineInvitationsResponse -> TestTree
responseDeclineInvitations =
  res
    "DeclineInvitationsResponse"
    "fixture/DeclineInvitationsResponse.proto"
    guardDutyService
    (Proxy :: Proxy DeclineInvitations)

responseDescribeOrganizationConfiguration :: DescribeOrganizationConfigurationResponse -> TestTree
responseDescribeOrganizationConfiguration =
  res
    "DescribeOrganizationConfigurationResponse"
    "fixture/DescribeOrganizationConfigurationResponse.proto"
    guardDutyService
    (Proxy :: Proxy DescribeOrganizationConfiguration)

responseCreatePublishingDestination :: CreatePublishingDestinationResponse -> TestTree
responseCreatePublishingDestination =
  res
    "CreatePublishingDestinationResponse"
    "fixture/CreatePublishingDestinationResponse.proto"
    guardDutyService
    (Proxy :: Proxy CreatePublishingDestination)

responseUpdateFilter :: UpdateFilterResponse -> TestTree
responseUpdateFilter =
  res
    "UpdateFilterResponse"
    "fixture/UpdateFilterResponse.proto"
    guardDutyService
    (Proxy :: Proxy UpdateFilter)

responseDeleteFilter :: DeleteFilterResponse -> TestTree
responseDeleteFilter =
  res
    "DeleteFilterResponse"
    "fixture/DeleteFilterResponse.proto"
    guardDutyService
    (Proxy :: Proxy DeleteFilter)

responseDisassociateMembers :: DisassociateMembersResponse -> TestTree
responseDisassociateMembers =
  res
    "DisassociateMembersResponse"
    "fixture/DisassociateMembersResponse.proto"
    guardDutyService
    (Proxy :: Proxy DisassociateMembers)

responseDisassociateFromMasterAccount :: DisassociateFromMasterAccountResponse -> TestTree
responseDisassociateFromMasterAccount =
  res
    "DisassociateFromMasterAccountResponse"
    "fixture/DisassociateFromMasterAccountResponse.proto"
    guardDutyService
    (Proxy :: Proxy DisassociateFromMasterAccount)

responseAcceptInvitation :: AcceptInvitationResponse -> TestTree
responseAcceptInvitation =
  res
    "AcceptInvitationResponse"
    "fixture/AcceptInvitationResponse.proto"
    guardDutyService
    (Proxy :: Proxy AcceptInvitation)

responseListFilters :: ListFiltersResponse -> TestTree
responseListFilters =
  res
    "ListFiltersResponse"
    "fixture/ListFiltersResponse.proto"
    guardDutyService
    (Proxy :: Proxy ListFilters)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers =
  res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    guardDutyService
    (Proxy :: Proxy ListMembers)

responseListPublishingDestinations :: ListPublishingDestinationsResponse -> TestTree
responseListPublishingDestinations =
  res
    "ListPublishingDestinationsResponse"
    "fixture/ListPublishingDestinationsResponse.proto"
    guardDutyService
    (Proxy :: Proxy ListPublishingDestinations)

responseDeletePublishingDestination :: DeletePublishingDestinationResponse -> TestTree
responseDeletePublishingDestination =
  res
    "DeletePublishingDestinationResponse"
    "fixture/DeletePublishingDestinationResponse.proto"
    guardDutyService
    (Proxy :: Proxy DeletePublishingDestination)

responseUpdatePublishingDestination :: UpdatePublishingDestinationResponse -> TestTree
responseUpdatePublishingDestination =
  res
    "UpdatePublishingDestinationResponse"
    "fixture/UpdatePublishingDestinationResponse.proto"
    guardDutyService
    (Proxy :: Proxy UpdatePublishingDestination)

responseGetDetector :: GetDetectorResponse -> TestTree
responseGetDetector =
  res
    "GetDetectorResponse"
    "fixture/GetDetectorResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetDetector)

responseCreateSampleFindings :: CreateSampleFindingsResponse -> TestTree
responseCreateSampleFindings =
  res
    "CreateSampleFindingsResponse"
    "fixture/CreateSampleFindingsResponse.proto"
    guardDutyService
    (Proxy :: Proxy CreateSampleFindings)

responseArchiveFindings :: ArchiveFindingsResponse -> TestTree
responseArchiveFindings =
  res
    "ArchiveFindingsResponse"
    "fixture/ArchiveFindingsResponse.proto"
    guardDutyService
    (Proxy :: Proxy ArchiveFindings)

responseCreateMembers :: CreateMembersResponse -> TestTree
responseCreateMembers =
  res
    "CreateMembersResponse"
    "fixture/CreateMembersResponse.proto"
    guardDutyService
    (Proxy :: Proxy CreateMembers)

responseUnarchiveFindings :: UnarchiveFindingsResponse -> TestTree
responseUnarchiveFindings =
  res
    "UnarchiveFindingsResponse"
    "fixture/UnarchiveFindingsResponse.proto"
    guardDutyService
    (Proxy :: Proxy UnarchiveFindings)

responseGetMemberDetectors :: GetMemberDetectorsResponse -> TestTree
responseGetMemberDetectors =
  res
    "GetMemberDetectorsResponse"
    "fixture/GetMemberDetectorsResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetMemberDetectors)

responseGetInvitationsCount :: GetInvitationsCountResponse -> TestTree
responseGetInvitationsCount =
  res
    "GetInvitationsCountResponse"
    "fixture/GetInvitationsCountResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetInvitationsCount)

responseStartMonitoringMembers :: StartMonitoringMembersResponse -> TestTree
responseStartMonitoringMembers =
  res
    "StartMonitoringMembersResponse"
    "fixture/StartMonitoringMembersResponse.proto"
    guardDutyService
    (Proxy :: Proxy StartMonitoringMembers)

responseUpdateOrganizationConfiguration :: UpdateOrganizationConfigurationResponse -> TestTree
responseUpdateOrganizationConfiguration =
  res
    "UpdateOrganizationConfigurationResponse"
    "fixture/UpdateOrganizationConfigurationResponse.proto"
    guardDutyService
    (Proxy :: Proxy UpdateOrganizationConfiguration)

responseInviteMembers :: InviteMembersResponse -> TestTree
responseInviteMembers =
  res
    "InviteMembersResponse"
    "fixture/InviteMembersResponse.proto"
    guardDutyService
    (Proxy :: Proxy InviteMembers)

responseDeleteIPSet :: DeleteIPSetResponse -> TestTree
responseDeleteIPSet =
  res
    "DeleteIPSetResponse"
    "fixture/DeleteIPSetResponse.proto"
    guardDutyService
    (Proxy :: Proxy DeleteIPSet)

responseUpdateIPSet :: UpdateIPSetResponse -> TestTree
responseUpdateIPSet =
  res
    "UpdateIPSetResponse"
    "fixture/UpdateIPSetResponse.proto"
    guardDutyService
    (Proxy :: Proxy UpdateIPSet)

responseListIPSets :: ListIPSetsResponse -> TestTree
responseListIPSets =
  res
    "ListIPSetsResponse"
    "fixture/ListIPSetsResponse.proto"
    guardDutyService
    (Proxy :: Proxy ListIPSets)

responseGetMembers :: GetMembersResponse -> TestTree
responseGetMembers =
  res
    "GetMembersResponse"
    "fixture/GetMembersResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetMembers)

responseDescribePublishingDestination :: DescribePublishingDestinationResponse -> TestTree
responseDescribePublishingDestination =
  res
    "DescribePublishingDestinationResponse"
    "fixture/DescribePublishingDestinationResponse.proto"
    guardDutyService
    (Proxy :: Proxy DescribePublishingDestination)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    guardDutyService
    (Proxy :: Proxy TagResource)

responseGetFindings :: GetFindingsResponse -> TestTree
responseGetFindings =
  res
    "GetFindingsResponse"
    "fixture/GetFindingsResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetFindings)

responseListDetectors :: ListDetectorsResponse -> TestTree
responseListDetectors =
  res
    "ListDetectorsResponse"
    "fixture/ListDetectorsResponse.proto"
    guardDutyService
    (Proxy :: Proxy ListDetectors)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    guardDutyService
    (Proxy :: Proxy UntagResource)

responseUpdateDetector :: UpdateDetectorResponse -> TestTree
responseUpdateDetector =
  res
    "UpdateDetectorResponse"
    "fixture/UpdateDetectorResponse.proto"
    guardDutyService
    (Proxy :: Proxy UpdateDetector)

responseDeleteDetector :: DeleteDetectorResponse -> TestTree
responseDeleteDetector =
  res
    "DeleteDetectorResponse"
    "fixture/DeleteDetectorResponse.proto"
    guardDutyService
    (Proxy :: Proxy DeleteDetector)

responseUpdateFindingsFeedback :: UpdateFindingsFeedbackResponse -> TestTree
responseUpdateFindingsFeedback =
  res
    "UpdateFindingsFeedbackResponse"
    "fixture/UpdateFindingsFeedbackResponse.proto"
    guardDutyService
    (Proxy :: Proxy UpdateFindingsFeedback)

responseGetFilter :: GetFilterResponse -> TestTree
responseGetFilter =
  res
    "GetFilterResponse"
    "fixture/GetFilterResponse.proto"
    guardDutyService
    (Proxy :: Proxy GetFilter)

responseDisableOrganizationAdminAccount :: DisableOrganizationAdminAccountResponse -> TestTree
responseDisableOrganizationAdminAccount =
  res
    "DisableOrganizationAdminAccountResponse"
    "fixture/DisableOrganizationAdminAccountResponse.proto"
    guardDutyService
    (Proxy :: Proxy DisableOrganizationAdminAccount)
