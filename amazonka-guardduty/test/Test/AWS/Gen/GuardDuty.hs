{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.GuardDuty
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             createFilter
--
--         , requestListFindings $
--             listFindings
--
--         , requestCreateIPSet $
--             createIPSet
--
--         , requestDeleteThreatIntelSet $
--             deleteThreatIntelSet
--
--         , requestUpdateThreatIntelSet $
--             updateThreatIntelSet
--
--         , requestStopMonitoringMembers $
--             stopMonitoringMembers
--
--         , requestListThreatIntelSets $
--             listThreatIntelSets
--
--         , requestCreateThreatIntelSet $
--             createThreatIntelSet
--
--         , requestDeleteMembers $
--             deleteMembers
--
--         , requestGetFindingsStatistics $
--             getFindingsStatistics
--
--         , requestGetIPSet $
--             getIPSet
--
--         , requestListInvitations $
--             listInvitations
--
--         , requestGetThreatIntelSet $
--             getThreatIntelSet
--
--         , requestDeleteInvitations $
--             deleteInvitations
--
--         , requestGetMasterAccount $
--             getMasterAccount
--
--         , requestCreateDetector $
--             createDetector
--
--         , requestDeclineInvitations $
--             declineInvitations
--
--         , requestUpdateFilter $
--             updateFilter
--
--         , requestDeleteFilter $
--             deleteFilter
--
--         , requestDisassociateMembers $
--             disassociateMembers
--
--         , requestDisassociateFromMasterAccount $
--             disassociateFromMasterAccount
--
--         , requestAcceptInvitation $
--             acceptInvitation
--
--         , requestListFilters $
--             listFilters
--
--         , requestListMembers $
--             listMembers
--
--         , requestGetDetector $
--             getDetector
--
--         , requestCreateSampleFindings $
--             createSampleFindings
--
--         , requestArchiveFindings $
--             archiveFindings
--
--         , requestCreateMembers $
--             createMembers
--
--         , requestUnarchiveFindings $
--             unarchiveFindings
--
--         , requestGetInvitationsCount $
--             getInvitationsCount
--
--         , requestStartMonitoringMembers $
--             startMonitoringMembers
--
--         , requestInviteMembers $
--             inviteMembers
--
--         , requestDeleteIPSet $
--             deleteIPSet
--
--         , requestUpdateIPSet $
--             updateIPSet
--
--         , requestListIPSets $
--             listIPSets
--
--         , requestGetMembers $
--             getMembers
--
--         , requestGetFindings $
--             getFindings
--
--         , requestListDetectors $
--             listDetectors
--
--         , requestUpdateDetector $
--             updateDetector
--
--         , requestDeleteDetector $
--             deleteDetector
--
--         , requestUpdateFindingsFeedback $
--             updateFindingsFeedback
--
--         , requestGetFilter $
--             getFilter
--
--           ]

--     , testGroup "response"
--         [ responseCreateFilter $
--             createFilterResponse
--
--         , responseListFindings $
--             listFindingsResponse
--
--         , responseCreateIPSet $
--             createIPSetResponse
--
--         , responseDeleteThreatIntelSet $
--             deleteThreatIntelSetResponse
--
--         , responseUpdateThreatIntelSet $
--             updateThreatIntelSetResponse
--
--         , responseStopMonitoringMembers $
--             stopMonitoringMembersResponse
--
--         , responseListThreatIntelSets $
--             listThreatIntelSetsResponse
--
--         , responseCreateThreatIntelSet $
--             createThreatIntelSetResponse
--
--         , responseDeleteMembers $
--             deleteMembersResponse
--
--         , responseGetFindingsStatistics $
--             getFindingsStatisticsResponse
--
--         , responseGetIPSet $
--             getIPSetResponse
--
--         , responseListInvitations $
--             listInvitationsResponse
--
--         , responseGetThreatIntelSet $
--             getThreatIntelSetResponse
--
--         , responseDeleteInvitations $
--             deleteInvitationsResponse
--
--         , responseGetMasterAccount $
--             getMasterAccountResponse
--
--         , responseCreateDetector $
--             createDetectorResponse
--
--         , responseDeclineInvitations $
--             declineInvitationsResponse
--
--         , responseUpdateFilter $
--             updateFilterResponse
--
--         , responseDeleteFilter $
--             deleteFilterResponse
--
--         , responseDisassociateMembers $
--             disassociateMembersResponse
--
--         , responseDisassociateFromMasterAccount $
--             disassociateFromMasterAccountResponse
--
--         , responseAcceptInvitation $
--             acceptInvitationResponse
--
--         , responseListFilters $
--             listFiltersResponse
--
--         , responseListMembers $
--             listMembersResponse
--
--         , responseGetDetector $
--             getDetectorResponse
--
--         , responseCreateSampleFindings $
--             createSampleFindingsResponse
--
--         , responseArchiveFindings $
--             archiveFindingsResponse
--
--         , responseCreateMembers $
--             createMembersResponse
--
--         , responseUnarchiveFindings $
--             unarchiveFindingsResponse
--
--         , responseGetInvitationsCount $
--             getInvitationsCountResponse
--
--         , responseStartMonitoringMembers $
--             startMonitoringMembersResponse
--
--         , responseInviteMembers $
--             inviteMembersResponse
--
--         , responseDeleteIPSet $
--             deleteIPSetResponse
--
--         , responseUpdateIPSet $
--             updateIPSetResponse
--
--         , responseListIPSets $
--             listIPSetsResponse
--
--         , responseGetMembers $
--             getMembersResponse
--
--         , responseGetFindings $
--             getFindingsResponse
--
--         , responseListDetectors $
--             listDetectorsResponse
--
--         , responseUpdateDetector $
--             updateDetectorResponse
--
--         , responseDeleteDetector $
--             deleteDetectorResponse
--
--         , responseUpdateFindingsFeedback $
--             updateFindingsFeedbackResponse
--
--         , responseGetFilter $
--             getFilterResponse
--
--           ]
--     ]

-- Requests

requestCreateFilter :: CreateFilter -> TestTree
requestCreateFilter = req
    "CreateFilter"
    "fixture/CreateFilter.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings = req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestCreateIPSet :: CreateIPSet -> TestTree
requestCreateIPSet = req
    "CreateIPSet"
    "fixture/CreateIPSet.yaml"

requestDeleteThreatIntelSet :: DeleteThreatIntelSet -> TestTree
requestDeleteThreatIntelSet = req
    "DeleteThreatIntelSet"
    "fixture/DeleteThreatIntelSet.yaml"

requestUpdateThreatIntelSet :: UpdateThreatIntelSet -> TestTree
requestUpdateThreatIntelSet = req
    "UpdateThreatIntelSet"
    "fixture/UpdateThreatIntelSet.yaml"

requestStopMonitoringMembers :: StopMonitoringMembers -> TestTree
requestStopMonitoringMembers = req
    "StopMonitoringMembers"
    "fixture/StopMonitoringMembers.yaml"

requestListThreatIntelSets :: ListThreatIntelSets -> TestTree
requestListThreatIntelSets = req
    "ListThreatIntelSets"
    "fixture/ListThreatIntelSets.yaml"

requestCreateThreatIntelSet :: CreateThreatIntelSet -> TestTree
requestCreateThreatIntelSet = req
    "CreateThreatIntelSet"
    "fixture/CreateThreatIntelSet.yaml"

requestDeleteMembers :: DeleteMembers -> TestTree
requestDeleteMembers = req
    "DeleteMembers"
    "fixture/DeleteMembers.yaml"

requestGetFindingsStatistics :: GetFindingsStatistics -> TestTree
requestGetFindingsStatistics = req
    "GetFindingsStatistics"
    "fixture/GetFindingsStatistics.yaml"

requestGetIPSet :: GetIPSet -> TestTree
requestGetIPSet = req
    "GetIPSet"
    "fixture/GetIPSet.yaml"

requestListInvitations :: ListInvitations -> TestTree
requestListInvitations = req
    "ListInvitations"
    "fixture/ListInvitations.yaml"

requestGetThreatIntelSet :: GetThreatIntelSet -> TestTree
requestGetThreatIntelSet = req
    "GetThreatIntelSet"
    "fixture/GetThreatIntelSet.yaml"

requestDeleteInvitations :: DeleteInvitations -> TestTree
requestDeleteInvitations = req
    "DeleteInvitations"
    "fixture/DeleteInvitations.yaml"

requestGetMasterAccount :: GetMasterAccount -> TestTree
requestGetMasterAccount = req
    "GetMasterAccount"
    "fixture/GetMasterAccount.yaml"

requestCreateDetector :: CreateDetector -> TestTree
requestCreateDetector = req
    "CreateDetector"
    "fixture/CreateDetector.yaml"

requestDeclineInvitations :: DeclineInvitations -> TestTree
requestDeclineInvitations = req
    "DeclineInvitations"
    "fixture/DeclineInvitations.yaml"

requestUpdateFilter :: UpdateFilter -> TestTree
requestUpdateFilter = req
    "UpdateFilter"
    "fixture/UpdateFilter.yaml"

requestDeleteFilter :: DeleteFilter -> TestTree
requestDeleteFilter = req
    "DeleteFilter"
    "fixture/DeleteFilter.yaml"

requestDisassociateMembers :: DisassociateMembers -> TestTree
requestDisassociateMembers = req
    "DisassociateMembers"
    "fixture/DisassociateMembers.yaml"

requestDisassociateFromMasterAccount :: DisassociateFromMasterAccount -> TestTree
requestDisassociateFromMasterAccount = req
    "DisassociateFromMasterAccount"
    "fixture/DisassociateFromMasterAccount.yaml"

requestAcceptInvitation :: AcceptInvitation -> TestTree
requestAcceptInvitation = req
    "AcceptInvitation"
    "fixture/AcceptInvitation.yaml"

requestListFilters :: ListFilters -> TestTree
requestListFilters = req
    "ListFilters"
    "fixture/ListFilters.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers = req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestGetDetector :: GetDetector -> TestTree
requestGetDetector = req
    "GetDetector"
    "fixture/GetDetector.yaml"

requestCreateSampleFindings :: CreateSampleFindings -> TestTree
requestCreateSampleFindings = req
    "CreateSampleFindings"
    "fixture/CreateSampleFindings.yaml"

requestArchiveFindings :: ArchiveFindings -> TestTree
requestArchiveFindings = req
    "ArchiveFindings"
    "fixture/ArchiveFindings.yaml"

requestCreateMembers :: CreateMembers -> TestTree
requestCreateMembers = req
    "CreateMembers"
    "fixture/CreateMembers.yaml"

requestUnarchiveFindings :: UnarchiveFindings -> TestTree
requestUnarchiveFindings = req
    "UnarchiveFindings"
    "fixture/UnarchiveFindings.yaml"

requestGetInvitationsCount :: GetInvitationsCount -> TestTree
requestGetInvitationsCount = req
    "GetInvitationsCount"
    "fixture/GetInvitationsCount.yaml"

requestStartMonitoringMembers :: StartMonitoringMembers -> TestTree
requestStartMonitoringMembers = req
    "StartMonitoringMembers"
    "fixture/StartMonitoringMembers.yaml"

requestInviteMembers :: InviteMembers -> TestTree
requestInviteMembers = req
    "InviteMembers"
    "fixture/InviteMembers.yaml"

requestDeleteIPSet :: DeleteIPSet -> TestTree
requestDeleteIPSet = req
    "DeleteIPSet"
    "fixture/DeleteIPSet.yaml"

requestUpdateIPSet :: UpdateIPSet -> TestTree
requestUpdateIPSet = req
    "UpdateIPSet"
    "fixture/UpdateIPSet.yaml"

requestListIPSets :: ListIPSets -> TestTree
requestListIPSets = req
    "ListIPSets"
    "fixture/ListIPSets.yaml"

requestGetMembers :: GetMembers -> TestTree
requestGetMembers = req
    "GetMembers"
    "fixture/GetMembers.yaml"

requestGetFindings :: GetFindings -> TestTree
requestGetFindings = req
    "GetFindings"
    "fixture/GetFindings.yaml"

requestListDetectors :: ListDetectors -> TestTree
requestListDetectors = req
    "ListDetectors"
    "fixture/ListDetectors.yaml"

requestUpdateDetector :: UpdateDetector -> TestTree
requestUpdateDetector = req
    "UpdateDetector"
    "fixture/UpdateDetector.yaml"

requestDeleteDetector :: DeleteDetector -> TestTree
requestDeleteDetector = req
    "DeleteDetector"
    "fixture/DeleteDetector.yaml"

requestUpdateFindingsFeedback :: UpdateFindingsFeedback -> TestTree
requestUpdateFindingsFeedback = req
    "UpdateFindingsFeedback"
    "fixture/UpdateFindingsFeedback.yaml"

requestGetFilter :: GetFilter -> TestTree
requestGetFilter = req
    "GetFilter"
    "fixture/GetFilter.yaml"

-- Responses

responseCreateFilter :: CreateFilterResponse -> TestTree
responseCreateFilter = res
    "CreateFilterResponse"
    "fixture/CreateFilterResponse.proto"
    guardDuty
    (Proxy :: Proxy CreateFilter)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings = res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    guardDuty
    (Proxy :: Proxy ListFindings)

responseCreateIPSet :: CreateIPSetResponse -> TestTree
responseCreateIPSet = res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    guardDuty
    (Proxy :: Proxy CreateIPSet)

responseDeleteThreatIntelSet :: DeleteThreatIntelSetResponse -> TestTree
responseDeleteThreatIntelSet = res
    "DeleteThreatIntelSetResponse"
    "fixture/DeleteThreatIntelSetResponse.proto"
    guardDuty
    (Proxy :: Proxy DeleteThreatIntelSet)

responseUpdateThreatIntelSet :: UpdateThreatIntelSetResponse -> TestTree
responseUpdateThreatIntelSet = res
    "UpdateThreatIntelSetResponse"
    "fixture/UpdateThreatIntelSetResponse.proto"
    guardDuty
    (Proxy :: Proxy UpdateThreatIntelSet)

responseStopMonitoringMembers :: StopMonitoringMembersResponse -> TestTree
responseStopMonitoringMembers = res
    "StopMonitoringMembersResponse"
    "fixture/StopMonitoringMembersResponse.proto"
    guardDuty
    (Proxy :: Proxy StopMonitoringMembers)

responseListThreatIntelSets :: ListThreatIntelSetsResponse -> TestTree
responseListThreatIntelSets = res
    "ListThreatIntelSetsResponse"
    "fixture/ListThreatIntelSetsResponse.proto"
    guardDuty
    (Proxy :: Proxy ListThreatIntelSets)

responseCreateThreatIntelSet :: CreateThreatIntelSetResponse -> TestTree
responseCreateThreatIntelSet = res
    "CreateThreatIntelSetResponse"
    "fixture/CreateThreatIntelSetResponse.proto"
    guardDuty
    (Proxy :: Proxy CreateThreatIntelSet)

responseDeleteMembers :: DeleteMembersResponse -> TestTree
responseDeleteMembers = res
    "DeleteMembersResponse"
    "fixture/DeleteMembersResponse.proto"
    guardDuty
    (Proxy :: Proxy DeleteMembers)

responseGetFindingsStatistics :: GetFindingsStatisticsResponse -> TestTree
responseGetFindingsStatistics = res
    "GetFindingsStatisticsResponse"
    "fixture/GetFindingsStatisticsResponse.proto"
    guardDuty
    (Proxy :: Proxy GetFindingsStatistics)

responseGetIPSet :: GetIPSetResponse -> TestTree
responseGetIPSet = res
    "GetIPSetResponse"
    "fixture/GetIPSetResponse.proto"
    guardDuty
    (Proxy :: Proxy GetIPSet)

responseListInvitations :: ListInvitationsResponse -> TestTree
responseListInvitations = res
    "ListInvitationsResponse"
    "fixture/ListInvitationsResponse.proto"
    guardDuty
    (Proxy :: Proxy ListInvitations)

responseGetThreatIntelSet :: GetThreatIntelSetResponse -> TestTree
responseGetThreatIntelSet = res
    "GetThreatIntelSetResponse"
    "fixture/GetThreatIntelSetResponse.proto"
    guardDuty
    (Proxy :: Proxy GetThreatIntelSet)

responseDeleteInvitations :: DeleteInvitationsResponse -> TestTree
responseDeleteInvitations = res
    "DeleteInvitationsResponse"
    "fixture/DeleteInvitationsResponse.proto"
    guardDuty
    (Proxy :: Proxy DeleteInvitations)

responseGetMasterAccount :: GetMasterAccountResponse -> TestTree
responseGetMasterAccount = res
    "GetMasterAccountResponse"
    "fixture/GetMasterAccountResponse.proto"
    guardDuty
    (Proxy :: Proxy GetMasterAccount)

responseCreateDetector :: CreateDetectorResponse -> TestTree
responseCreateDetector = res
    "CreateDetectorResponse"
    "fixture/CreateDetectorResponse.proto"
    guardDuty
    (Proxy :: Proxy CreateDetector)

responseDeclineInvitations :: DeclineInvitationsResponse -> TestTree
responseDeclineInvitations = res
    "DeclineInvitationsResponse"
    "fixture/DeclineInvitationsResponse.proto"
    guardDuty
    (Proxy :: Proxy DeclineInvitations)

responseUpdateFilter :: UpdateFilterResponse -> TestTree
responseUpdateFilter = res
    "UpdateFilterResponse"
    "fixture/UpdateFilterResponse.proto"
    guardDuty
    (Proxy :: Proxy UpdateFilter)

responseDeleteFilter :: DeleteFilterResponse -> TestTree
responseDeleteFilter = res
    "DeleteFilterResponse"
    "fixture/DeleteFilterResponse.proto"
    guardDuty
    (Proxy :: Proxy DeleteFilter)

responseDisassociateMembers :: DisassociateMembersResponse -> TestTree
responseDisassociateMembers = res
    "DisassociateMembersResponse"
    "fixture/DisassociateMembersResponse.proto"
    guardDuty
    (Proxy :: Proxy DisassociateMembers)

responseDisassociateFromMasterAccount :: DisassociateFromMasterAccountResponse -> TestTree
responseDisassociateFromMasterAccount = res
    "DisassociateFromMasterAccountResponse"
    "fixture/DisassociateFromMasterAccountResponse.proto"
    guardDuty
    (Proxy :: Proxy DisassociateFromMasterAccount)

responseAcceptInvitation :: AcceptInvitationResponse -> TestTree
responseAcceptInvitation = res
    "AcceptInvitationResponse"
    "fixture/AcceptInvitationResponse.proto"
    guardDuty
    (Proxy :: Proxy AcceptInvitation)

responseListFilters :: ListFiltersResponse -> TestTree
responseListFilters = res
    "ListFiltersResponse"
    "fixture/ListFiltersResponse.proto"
    guardDuty
    (Proxy :: Proxy ListFilters)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers = res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    guardDuty
    (Proxy :: Proxy ListMembers)

responseGetDetector :: GetDetectorResponse -> TestTree
responseGetDetector = res
    "GetDetectorResponse"
    "fixture/GetDetectorResponse.proto"
    guardDuty
    (Proxy :: Proxy GetDetector)

responseCreateSampleFindings :: CreateSampleFindingsResponse -> TestTree
responseCreateSampleFindings = res
    "CreateSampleFindingsResponse"
    "fixture/CreateSampleFindingsResponse.proto"
    guardDuty
    (Proxy :: Proxy CreateSampleFindings)

responseArchiveFindings :: ArchiveFindingsResponse -> TestTree
responseArchiveFindings = res
    "ArchiveFindingsResponse"
    "fixture/ArchiveFindingsResponse.proto"
    guardDuty
    (Proxy :: Proxy ArchiveFindings)

responseCreateMembers :: CreateMembersResponse -> TestTree
responseCreateMembers = res
    "CreateMembersResponse"
    "fixture/CreateMembersResponse.proto"
    guardDuty
    (Proxy :: Proxy CreateMembers)

responseUnarchiveFindings :: UnarchiveFindingsResponse -> TestTree
responseUnarchiveFindings = res
    "UnarchiveFindingsResponse"
    "fixture/UnarchiveFindingsResponse.proto"
    guardDuty
    (Proxy :: Proxy UnarchiveFindings)

responseGetInvitationsCount :: GetInvitationsCountResponse -> TestTree
responseGetInvitationsCount = res
    "GetInvitationsCountResponse"
    "fixture/GetInvitationsCountResponse.proto"
    guardDuty
    (Proxy :: Proxy GetInvitationsCount)

responseStartMonitoringMembers :: StartMonitoringMembersResponse -> TestTree
responseStartMonitoringMembers = res
    "StartMonitoringMembersResponse"
    "fixture/StartMonitoringMembersResponse.proto"
    guardDuty
    (Proxy :: Proxy StartMonitoringMembers)

responseInviteMembers :: InviteMembersResponse -> TestTree
responseInviteMembers = res
    "InviteMembersResponse"
    "fixture/InviteMembersResponse.proto"
    guardDuty
    (Proxy :: Proxy InviteMembers)

responseDeleteIPSet :: DeleteIPSetResponse -> TestTree
responseDeleteIPSet = res
    "DeleteIPSetResponse"
    "fixture/DeleteIPSetResponse.proto"
    guardDuty
    (Proxy :: Proxy DeleteIPSet)

responseUpdateIPSet :: UpdateIPSetResponse -> TestTree
responseUpdateIPSet = res
    "UpdateIPSetResponse"
    "fixture/UpdateIPSetResponse.proto"
    guardDuty
    (Proxy :: Proxy UpdateIPSet)

responseListIPSets :: ListIPSetsResponse -> TestTree
responseListIPSets = res
    "ListIPSetsResponse"
    "fixture/ListIPSetsResponse.proto"
    guardDuty
    (Proxy :: Proxy ListIPSets)

responseGetMembers :: GetMembersResponse -> TestTree
responseGetMembers = res
    "GetMembersResponse"
    "fixture/GetMembersResponse.proto"
    guardDuty
    (Proxy :: Proxy GetMembers)

responseGetFindings :: GetFindingsResponse -> TestTree
responseGetFindings = res
    "GetFindingsResponse"
    "fixture/GetFindingsResponse.proto"
    guardDuty
    (Proxy :: Proxy GetFindings)

responseListDetectors :: ListDetectorsResponse -> TestTree
responseListDetectors = res
    "ListDetectorsResponse"
    "fixture/ListDetectorsResponse.proto"
    guardDuty
    (Proxy :: Proxy ListDetectors)

responseUpdateDetector :: UpdateDetectorResponse -> TestTree
responseUpdateDetector = res
    "UpdateDetectorResponse"
    "fixture/UpdateDetectorResponse.proto"
    guardDuty
    (Proxy :: Proxy UpdateDetector)

responseDeleteDetector :: DeleteDetectorResponse -> TestTree
responseDeleteDetector = res
    "DeleteDetectorResponse"
    "fixture/DeleteDetectorResponse.proto"
    guardDuty
    (Proxy :: Proxy DeleteDetector)

responseUpdateFindingsFeedback :: UpdateFindingsFeedbackResponse -> TestTree
responseUpdateFindingsFeedback = res
    "UpdateFindingsFeedbackResponse"
    "fixture/UpdateFindingsFeedbackResponse.proto"
    guardDuty
    (Proxy :: Proxy UpdateFindingsFeedback)

responseGetFilter :: GetFilterResponse -> TestTree
responseGetFilter = res
    "GetFilterResponse"
    "fixture/GetFilterResponse.proto"
    guardDuty
    (Proxy :: Proxy GetFilter)
