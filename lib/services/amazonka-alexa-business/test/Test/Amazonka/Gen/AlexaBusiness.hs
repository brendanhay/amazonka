{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AlexaBusiness
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AlexaBusiness where

import Amazonka.AlexaBusiness
import qualified Data.Proxy as Proxy
import Test.Amazonka.AlexaBusiness.Internal
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
--         [ requestApproveSkill $
--             newApproveSkill
--
--         , requestAssociateContactWithAddressBook $
--             newAssociateContactWithAddressBook
--
--         , requestAssociateDeviceWithNetworkProfile $
--             newAssociateDeviceWithNetworkProfile
--
--         , requestAssociateDeviceWithRoom $
--             newAssociateDeviceWithRoom
--
--         , requestAssociateSkillGroupWithRoom $
--             newAssociateSkillGroupWithRoom
--
--         , requestAssociateSkillWithSkillGroup $
--             newAssociateSkillWithSkillGroup
--
--         , requestAssociateSkillWithUsers $
--             newAssociateSkillWithUsers
--
--         , requestCreateAddressBook $
--             newCreateAddressBook
--
--         , requestCreateBusinessReportSchedule $
--             newCreateBusinessReportSchedule
--
--         , requestCreateConferenceProvider $
--             newCreateConferenceProvider
--
--         , requestCreateContact $
--             newCreateContact
--
--         , requestCreateGatewayGroup $
--             newCreateGatewayGroup
--
--         , requestCreateNetworkProfile $
--             newCreateNetworkProfile
--
--         , requestCreateProfile $
--             newCreateProfile
--
--         , requestCreateRoom $
--             newCreateRoom
--
--         , requestCreateSkillGroup $
--             newCreateSkillGroup
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteAddressBook $
--             newDeleteAddressBook
--
--         , requestDeleteBusinessReportSchedule $
--             newDeleteBusinessReportSchedule
--
--         , requestDeleteConferenceProvider $
--             newDeleteConferenceProvider
--
--         , requestDeleteContact $
--             newDeleteContact
--
--         , requestDeleteDevice $
--             newDeleteDevice
--
--         , requestDeleteDeviceUsageData $
--             newDeleteDeviceUsageData
--
--         , requestDeleteGatewayGroup $
--             newDeleteGatewayGroup
--
--         , requestDeleteNetworkProfile $
--             newDeleteNetworkProfile
--
--         , requestDeleteProfile $
--             newDeleteProfile
--
--         , requestDeleteRoom $
--             newDeleteRoom
--
--         , requestDeleteRoomSkillParameter $
--             newDeleteRoomSkillParameter
--
--         , requestDeleteSkillAuthorization $
--             newDeleteSkillAuthorization
--
--         , requestDeleteSkillGroup $
--             newDeleteSkillGroup
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDisassociateContactFromAddressBook $
--             newDisassociateContactFromAddressBook
--
--         , requestDisassociateDeviceFromRoom $
--             newDisassociateDeviceFromRoom
--
--         , requestDisassociateSkillFromSkillGroup $
--             newDisassociateSkillFromSkillGroup
--
--         , requestDisassociateSkillFromUsers $
--             newDisassociateSkillFromUsers
--
--         , requestDisassociateSkillGroupFromRoom $
--             newDisassociateSkillGroupFromRoom
--
--         , requestForgetSmartHomeAppliances $
--             newForgetSmartHomeAppliances
--
--         , requestGetAddressBook $
--             newGetAddressBook
--
--         , requestGetConferencePreference $
--             newGetConferencePreference
--
--         , requestGetConferenceProvider $
--             newGetConferenceProvider
--
--         , requestGetContact $
--             newGetContact
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestGetGateway $
--             newGetGateway
--
--         , requestGetGatewayGroup $
--             newGetGatewayGroup
--
--         , requestGetInvitationConfiguration $
--             newGetInvitationConfiguration
--
--         , requestGetNetworkProfile $
--             newGetNetworkProfile
--
--         , requestGetProfile $
--             newGetProfile
--
--         , requestGetRoom $
--             newGetRoom
--
--         , requestGetRoomSkillParameter $
--             newGetRoomSkillParameter
--
--         , requestGetSkillGroup $
--             newGetSkillGroup
--
--         , requestListBusinessReportSchedules $
--             newListBusinessReportSchedules
--
--         , requestListConferenceProviders $
--             newListConferenceProviders
--
--         , requestListDeviceEvents $
--             newListDeviceEvents
--
--         , requestListGatewayGroups $
--             newListGatewayGroups
--
--         , requestListGateways $
--             newListGateways
--
--         , requestListSkills $
--             newListSkills
--
--         , requestListSkillsStoreCategories $
--             newListSkillsStoreCategories
--
--         , requestListSkillsStoreSkillsByCategory $
--             newListSkillsStoreSkillsByCategory
--
--         , requestListSmartHomeAppliances $
--             newListSmartHomeAppliances
--
--         , requestListTags $
--             newListTags
--
--         , requestPutConferencePreference $
--             newPutConferencePreference
--
--         , requestPutInvitationConfiguration $
--             newPutInvitationConfiguration
--
--         , requestPutRoomSkillParameter $
--             newPutRoomSkillParameter
--
--         , requestPutSkillAuthorization $
--             newPutSkillAuthorization
--
--         , requestRegisterAVSDevice $
--             newRegisterAVSDevice
--
--         , requestRejectSkill $
--             newRejectSkill
--
--         , requestResolveRoom $
--             newResolveRoom
--
--         , requestRevokeInvitation $
--             newRevokeInvitation
--
--         , requestSearchAddressBooks $
--             newSearchAddressBooks
--
--         , requestSearchContacts $
--             newSearchContacts
--
--         , requestSearchDevices $
--             newSearchDevices
--
--         , requestSearchNetworkProfiles $
--             newSearchNetworkProfiles
--
--         , requestSearchProfiles $
--             newSearchProfiles
--
--         , requestSearchRooms $
--             newSearchRooms
--
--         , requestSearchSkillGroups $
--             newSearchSkillGroups
--
--         , requestSearchUsers $
--             newSearchUsers
--
--         , requestSendAnnouncement $
--             newSendAnnouncement
--
--         , requestSendInvitation $
--             newSendInvitation
--
--         , requestStartDeviceSync $
--             newStartDeviceSync
--
--         , requestStartSmartHomeApplianceDiscovery $
--             newStartSmartHomeApplianceDiscovery
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAddressBook $
--             newUpdateAddressBook
--
--         , requestUpdateBusinessReportSchedule $
--             newUpdateBusinessReportSchedule
--
--         , requestUpdateConferenceProvider $
--             newUpdateConferenceProvider
--
--         , requestUpdateContact $
--             newUpdateContact
--
--         , requestUpdateDevice $
--             newUpdateDevice
--
--         , requestUpdateGateway $
--             newUpdateGateway
--
--         , requestUpdateGatewayGroup $
--             newUpdateGatewayGroup
--
--         , requestUpdateNetworkProfile $
--             newUpdateNetworkProfile
--
--         , requestUpdateProfile $
--             newUpdateProfile
--
--         , requestUpdateRoom $
--             newUpdateRoom
--
--         , requestUpdateSkillGroup $
--             newUpdateSkillGroup
--
--           ]

--     , testGroup "response"
--         [ responseApproveSkill $
--             newApproveSkillResponse
--
--         , responseAssociateContactWithAddressBook $
--             newAssociateContactWithAddressBookResponse
--
--         , responseAssociateDeviceWithNetworkProfile $
--             newAssociateDeviceWithNetworkProfileResponse
--
--         , responseAssociateDeviceWithRoom $
--             newAssociateDeviceWithRoomResponse
--
--         , responseAssociateSkillGroupWithRoom $
--             newAssociateSkillGroupWithRoomResponse
--
--         , responseAssociateSkillWithSkillGroup $
--             newAssociateSkillWithSkillGroupResponse
--
--         , responseAssociateSkillWithUsers $
--             newAssociateSkillWithUsersResponse
--
--         , responseCreateAddressBook $
--             newCreateAddressBookResponse
--
--         , responseCreateBusinessReportSchedule $
--             newCreateBusinessReportScheduleResponse
--
--         , responseCreateConferenceProvider $
--             newCreateConferenceProviderResponse
--
--         , responseCreateContact $
--             newCreateContactResponse
--
--         , responseCreateGatewayGroup $
--             newCreateGatewayGroupResponse
--
--         , responseCreateNetworkProfile $
--             newCreateNetworkProfileResponse
--
--         , responseCreateProfile $
--             newCreateProfileResponse
--
--         , responseCreateRoom $
--             newCreateRoomResponse
--
--         , responseCreateSkillGroup $
--             newCreateSkillGroupResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteAddressBook $
--             newDeleteAddressBookResponse
--
--         , responseDeleteBusinessReportSchedule $
--             newDeleteBusinessReportScheduleResponse
--
--         , responseDeleteConferenceProvider $
--             newDeleteConferenceProviderResponse
--
--         , responseDeleteContact $
--             newDeleteContactResponse
--
--         , responseDeleteDevice $
--             newDeleteDeviceResponse
--
--         , responseDeleteDeviceUsageData $
--             newDeleteDeviceUsageDataResponse
--
--         , responseDeleteGatewayGroup $
--             newDeleteGatewayGroupResponse
--
--         , responseDeleteNetworkProfile $
--             newDeleteNetworkProfileResponse
--
--         , responseDeleteProfile $
--             newDeleteProfileResponse
--
--         , responseDeleteRoom $
--             newDeleteRoomResponse
--
--         , responseDeleteRoomSkillParameter $
--             newDeleteRoomSkillParameterResponse
--
--         , responseDeleteSkillAuthorization $
--             newDeleteSkillAuthorizationResponse
--
--         , responseDeleteSkillGroup $
--             newDeleteSkillGroupResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDisassociateContactFromAddressBook $
--             newDisassociateContactFromAddressBookResponse
--
--         , responseDisassociateDeviceFromRoom $
--             newDisassociateDeviceFromRoomResponse
--
--         , responseDisassociateSkillFromSkillGroup $
--             newDisassociateSkillFromSkillGroupResponse
--
--         , responseDisassociateSkillFromUsers $
--             newDisassociateSkillFromUsersResponse
--
--         , responseDisassociateSkillGroupFromRoom $
--             newDisassociateSkillGroupFromRoomResponse
--
--         , responseForgetSmartHomeAppliances $
--             newForgetSmartHomeAppliancesResponse
--
--         , responseGetAddressBook $
--             newGetAddressBookResponse
--
--         , responseGetConferencePreference $
--             newGetConferencePreferenceResponse
--
--         , responseGetConferenceProvider $
--             newGetConferenceProviderResponse
--
--         , responseGetContact $
--             newGetContactResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseGetGateway $
--             newGetGatewayResponse
--
--         , responseGetGatewayGroup $
--             newGetGatewayGroupResponse
--
--         , responseGetInvitationConfiguration $
--             newGetInvitationConfigurationResponse
--
--         , responseGetNetworkProfile $
--             newGetNetworkProfileResponse
--
--         , responseGetProfile $
--             newGetProfileResponse
--
--         , responseGetRoom $
--             newGetRoomResponse
--
--         , responseGetRoomSkillParameter $
--             newGetRoomSkillParameterResponse
--
--         , responseGetSkillGroup $
--             newGetSkillGroupResponse
--
--         , responseListBusinessReportSchedules $
--             newListBusinessReportSchedulesResponse
--
--         , responseListConferenceProviders $
--             newListConferenceProvidersResponse
--
--         , responseListDeviceEvents $
--             newListDeviceEventsResponse
--
--         , responseListGatewayGroups $
--             newListGatewayGroupsResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseListSkills $
--             newListSkillsResponse
--
--         , responseListSkillsStoreCategories $
--             newListSkillsStoreCategoriesResponse
--
--         , responseListSkillsStoreSkillsByCategory $
--             newListSkillsStoreSkillsByCategoryResponse
--
--         , responseListSmartHomeAppliances $
--             newListSmartHomeAppliancesResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responsePutConferencePreference $
--             newPutConferencePreferenceResponse
--
--         , responsePutInvitationConfiguration $
--             newPutInvitationConfigurationResponse
--
--         , responsePutRoomSkillParameter $
--             newPutRoomSkillParameterResponse
--
--         , responsePutSkillAuthorization $
--             newPutSkillAuthorizationResponse
--
--         , responseRegisterAVSDevice $
--             newRegisterAVSDeviceResponse
--
--         , responseRejectSkill $
--             newRejectSkillResponse
--
--         , responseResolveRoom $
--             newResolveRoomResponse
--
--         , responseRevokeInvitation $
--             newRevokeInvitationResponse
--
--         , responseSearchAddressBooks $
--             newSearchAddressBooksResponse
--
--         , responseSearchContacts $
--             newSearchContactsResponse
--
--         , responseSearchDevices $
--             newSearchDevicesResponse
--
--         , responseSearchNetworkProfiles $
--             newSearchNetworkProfilesResponse
--
--         , responseSearchProfiles $
--             newSearchProfilesResponse
--
--         , responseSearchRooms $
--             newSearchRoomsResponse
--
--         , responseSearchSkillGroups $
--             newSearchSkillGroupsResponse
--
--         , responseSearchUsers $
--             newSearchUsersResponse
--
--         , responseSendAnnouncement $
--             newSendAnnouncementResponse
--
--         , responseSendInvitation $
--             newSendInvitationResponse
--
--         , responseStartDeviceSync $
--             newStartDeviceSyncResponse
--
--         , responseStartSmartHomeApplianceDiscovery $
--             newStartSmartHomeApplianceDiscoveryResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAddressBook $
--             newUpdateAddressBookResponse
--
--         , responseUpdateBusinessReportSchedule $
--             newUpdateBusinessReportScheduleResponse
--
--         , responseUpdateConferenceProvider $
--             newUpdateConferenceProviderResponse
--
--         , responseUpdateContact $
--             newUpdateContactResponse
--
--         , responseUpdateDevice $
--             newUpdateDeviceResponse
--
--         , responseUpdateGateway $
--             newUpdateGatewayResponse
--
--         , responseUpdateGatewayGroup $
--             newUpdateGatewayGroupResponse
--
--         , responseUpdateNetworkProfile $
--             newUpdateNetworkProfileResponse
--
--         , responseUpdateProfile $
--             newUpdateProfileResponse
--
--         , responseUpdateRoom $
--             newUpdateRoomResponse
--
--         , responseUpdateSkillGroup $
--             newUpdateSkillGroupResponse
--
--           ]
--     ]

-- Requests

requestApproveSkill :: ApproveSkill -> TestTree
requestApproveSkill =
  req
    "ApproveSkill"
    "fixture/ApproveSkill.yaml"

requestAssociateContactWithAddressBook :: AssociateContactWithAddressBook -> TestTree
requestAssociateContactWithAddressBook =
  req
    "AssociateContactWithAddressBook"
    "fixture/AssociateContactWithAddressBook.yaml"

requestAssociateDeviceWithNetworkProfile :: AssociateDeviceWithNetworkProfile -> TestTree
requestAssociateDeviceWithNetworkProfile =
  req
    "AssociateDeviceWithNetworkProfile"
    "fixture/AssociateDeviceWithNetworkProfile.yaml"

requestAssociateDeviceWithRoom :: AssociateDeviceWithRoom -> TestTree
requestAssociateDeviceWithRoom =
  req
    "AssociateDeviceWithRoom"
    "fixture/AssociateDeviceWithRoom.yaml"

requestAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoom -> TestTree
requestAssociateSkillGroupWithRoom =
  req
    "AssociateSkillGroupWithRoom"
    "fixture/AssociateSkillGroupWithRoom.yaml"

requestAssociateSkillWithSkillGroup :: AssociateSkillWithSkillGroup -> TestTree
requestAssociateSkillWithSkillGroup =
  req
    "AssociateSkillWithSkillGroup"
    "fixture/AssociateSkillWithSkillGroup.yaml"

requestAssociateSkillWithUsers :: AssociateSkillWithUsers -> TestTree
requestAssociateSkillWithUsers =
  req
    "AssociateSkillWithUsers"
    "fixture/AssociateSkillWithUsers.yaml"

requestCreateAddressBook :: CreateAddressBook -> TestTree
requestCreateAddressBook =
  req
    "CreateAddressBook"
    "fixture/CreateAddressBook.yaml"

requestCreateBusinessReportSchedule :: CreateBusinessReportSchedule -> TestTree
requestCreateBusinessReportSchedule =
  req
    "CreateBusinessReportSchedule"
    "fixture/CreateBusinessReportSchedule.yaml"

requestCreateConferenceProvider :: CreateConferenceProvider -> TestTree
requestCreateConferenceProvider =
  req
    "CreateConferenceProvider"
    "fixture/CreateConferenceProvider.yaml"

requestCreateContact :: CreateContact -> TestTree
requestCreateContact =
  req
    "CreateContact"
    "fixture/CreateContact.yaml"

requestCreateGatewayGroup :: CreateGatewayGroup -> TestTree
requestCreateGatewayGroup =
  req
    "CreateGatewayGroup"
    "fixture/CreateGatewayGroup.yaml"

requestCreateNetworkProfile :: CreateNetworkProfile -> TestTree
requestCreateNetworkProfile =
  req
    "CreateNetworkProfile"
    "fixture/CreateNetworkProfile.yaml"

requestCreateProfile :: CreateProfile -> TestTree
requestCreateProfile =
  req
    "CreateProfile"
    "fixture/CreateProfile.yaml"

requestCreateRoom :: CreateRoom -> TestTree
requestCreateRoom =
  req
    "CreateRoom"
    "fixture/CreateRoom.yaml"

requestCreateSkillGroup :: CreateSkillGroup -> TestTree
requestCreateSkillGroup =
  req
    "CreateSkillGroup"
    "fixture/CreateSkillGroup.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteAddressBook :: DeleteAddressBook -> TestTree
requestDeleteAddressBook =
  req
    "DeleteAddressBook"
    "fixture/DeleteAddressBook.yaml"

requestDeleteBusinessReportSchedule :: DeleteBusinessReportSchedule -> TestTree
requestDeleteBusinessReportSchedule =
  req
    "DeleteBusinessReportSchedule"
    "fixture/DeleteBusinessReportSchedule.yaml"

requestDeleteConferenceProvider :: DeleteConferenceProvider -> TestTree
requestDeleteConferenceProvider =
  req
    "DeleteConferenceProvider"
    "fixture/DeleteConferenceProvider.yaml"

requestDeleteContact :: DeleteContact -> TestTree
requestDeleteContact =
  req
    "DeleteContact"
    "fixture/DeleteContact.yaml"

requestDeleteDevice :: DeleteDevice -> TestTree
requestDeleteDevice =
  req
    "DeleteDevice"
    "fixture/DeleteDevice.yaml"

requestDeleteDeviceUsageData :: DeleteDeviceUsageData -> TestTree
requestDeleteDeviceUsageData =
  req
    "DeleteDeviceUsageData"
    "fixture/DeleteDeviceUsageData.yaml"

requestDeleteGatewayGroup :: DeleteGatewayGroup -> TestTree
requestDeleteGatewayGroup =
  req
    "DeleteGatewayGroup"
    "fixture/DeleteGatewayGroup.yaml"

requestDeleteNetworkProfile :: DeleteNetworkProfile -> TestTree
requestDeleteNetworkProfile =
  req
    "DeleteNetworkProfile"
    "fixture/DeleteNetworkProfile.yaml"

requestDeleteProfile :: DeleteProfile -> TestTree
requestDeleteProfile =
  req
    "DeleteProfile"
    "fixture/DeleteProfile.yaml"

requestDeleteRoom :: DeleteRoom -> TestTree
requestDeleteRoom =
  req
    "DeleteRoom"
    "fixture/DeleteRoom.yaml"

requestDeleteRoomSkillParameter :: DeleteRoomSkillParameter -> TestTree
requestDeleteRoomSkillParameter =
  req
    "DeleteRoomSkillParameter"
    "fixture/DeleteRoomSkillParameter.yaml"

requestDeleteSkillAuthorization :: DeleteSkillAuthorization -> TestTree
requestDeleteSkillAuthorization =
  req
    "DeleteSkillAuthorization"
    "fixture/DeleteSkillAuthorization.yaml"

requestDeleteSkillGroup :: DeleteSkillGroup -> TestTree
requestDeleteSkillGroup =
  req
    "DeleteSkillGroup"
    "fixture/DeleteSkillGroup.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDisassociateContactFromAddressBook :: DisassociateContactFromAddressBook -> TestTree
requestDisassociateContactFromAddressBook =
  req
    "DisassociateContactFromAddressBook"
    "fixture/DisassociateContactFromAddressBook.yaml"

requestDisassociateDeviceFromRoom :: DisassociateDeviceFromRoom -> TestTree
requestDisassociateDeviceFromRoom =
  req
    "DisassociateDeviceFromRoom"
    "fixture/DisassociateDeviceFromRoom.yaml"

requestDisassociateSkillFromSkillGroup :: DisassociateSkillFromSkillGroup -> TestTree
requestDisassociateSkillFromSkillGroup =
  req
    "DisassociateSkillFromSkillGroup"
    "fixture/DisassociateSkillFromSkillGroup.yaml"

requestDisassociateSkillFromUsers :: DisassociateSkillFromUsers -> TestTree
requestDisassociateSkillFromUsers =
  req
    "DisassociateSkillFromUsers"
    "fixture/DisassociateSkillFromUsers.yaml"

requestDisassociateSkillGroupFromRoom :: DisassociateSkillGroupFromRoom -> TestTree
requestDisassociateSkillGroupFromRoom =
  req
    "DisassociateSkillGroupFromRoom"
    "fixture/DisassociateSkillGroupFromRoom.yaml"

requestForgetSmartHomeAppliances :: ForgetSmartHomeAppliances -> TestTree
requestForgetSmartHomeAppliances =
  req
    "ForgetSmartHomeAppliances"
    "fixture/ForgetSmartHomeAppliances.yaml"

requestGetAddressBook :: GetAddressBook -> TestTree
requestGetAddressBook =
  req
    "GetAddressBook"
    "fixture/GetAddressBook.yaml"

requestGetConferencePreference :: GetConferencePreference -> TestTree
requestGetConferencePreference =
  req
    "GetConferencePreference"
    "fixture/GetConferencePreference.yaml"

requestGetConferenceProvider :: GetConferenceProvider -> TestTree
requestGetConferenceProvider =
  req
    "GetConferenceProvider"
    "fixture/GetConferenceProvider.yaml"

requestGetContact :: GetContact -> TestTree
requestGetContact =
  req
    "GetContact"
    "fixture/GetContact.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice =
  req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestGetGateway :: GetGateway -> TestTree
requestGetGateway =
  req
    "GetGateway"
    "fixture/GetGateway.yaml"

requestGetGatewayGroup :: GetGatewayGroup -> TestTree
requestGetGatewayGroup =
  req
    "GetGatewayGroup"
    "fixture/GetGatewayGroup.yaml"

requestGetInvitationConfiguration :: GetInvitationConfiguration -> TestTree
requestGetInvitationConfiguration =
  req
    "GetInvitationConfiguration"
    "fixture/GetInvitationConfiguration.yaml"

requestGetNetworkProfile :: GetNetworkProfile -> TestTree
requestGetNetworkProfile =
  req
    "GetNetworkProfile"
    "fixture/GetNetworkProfile.yaml"

requestGetProfile :: GetProfile -> TestTree
requestGetProfile =
  req
    "GetProfile"
    "fixture/GetProfile.yaml"

requestGetRoom :: GetRoom -> TestTree
requestGetRoom =
  req
    "GetRoom"
    "fixture/GetRoom.yaml"

requestGetRoomSkillParameter :: GetRoomSkillParameter -> TestTree
requestGetRoomSkillParameter =
  req
    "GetRoomSkillParameter"
    "fixture/GetRoomSkillParameter.yaml"

requestGetSkillGroup :: GetSkillGroup -> TestTree
requestGetSkillGroup =
  req
    "GetSkillGroup"
    "fixture/GetSkillGroup.yaml"

requestListBusinessReportSchedules :: ListBusinessReportSchedules -> TestTree
requestListBusinessReportSchedules =
  req
    "ListBusinessReportSchedules"
    "fixture/ListBusinessReportSchedules.yaml"

requestListConferenceProviders :: ListConferenceProviders -> TestTree
requestListConferenceProviders =
  req
    "ListConferenceProviders"
    "fixture/ListConferenceProviders.yaml"

requestListDeviceEvents :: ListDeviceEvents -> TestTree
requestListDeviceEvents =
  req
    "ListDeviceEvents"
    "fixture/ListDeviceEvents.yaml"

requestListGatewayGroups :: ListGatewayGroups -> TestTree
requestListGatewayGroups =
  req
    "ListGatewayGroups"
    "fixture/ListGatewayGroups.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways =
  req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestListSkills :: ListSkills -> TestTree
requestListSkills =
  req
    "ListSkills"
    "fixture/ListSkills.yaml"

requestListSkillsStoreCategories :: ListSkillsStoreCategories -> TestTree
requestListSkillsStoreCategories =
  req
    "ListSkillsStoreCategories"
    "fixture/ListSkillsStoreCategories.yaml"

requestListSkillsStoreSkillsByCategory :: ListSkillsStoreSkillsByCategory -> TestTree
requestListSkillsStoreSkillsByCategory =
  req
    "ListSkillsStoreSkillsByCategory"
    "fixture/ListSkillsStoreSkillsByCategory.yaml"

requestListSmartHomeAppliances :: ListSmartHomeAppliances -> TestTree
requestListSmartHomeAppliances =
  req
    "ListSmartHomeAppliances"
    "fixture/ListSmartHomeAppliances.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestPutConferencePreference :: PutConferencePreference -> TestTree
requestPutConferencePreference =
  req
    "PutConferencePreference"
    "fixture/PutConferencePreference.yaml"

requestPutInvitationConfiguration :: PutInvitationConfiguration -> TestTree
requestPutInvitationConfiguration =
  req
    "PutInvitationConfiguration"
    "fixture/PutInvitationConfiguration.yaml"

requestPutRoomSkillParameter :: PutRoomSkillParameter -> TestTree
requestPutRoomSkillParameter =
  req
    "PutRoomSkillParameter"
    "fixture/PutRoomSkillParameter.yaml"

requestPutSkillAuthorization :: PutSkillAuthorization -> TestTree
requestPutSkillAuthorization =
  req
    "PutSkillAuthorization"
    "fixture/PutSkillAuthorization.yaml"

requestRegisterAVSDevice :: RegisterAVSDevice -> TestTree
requestRegisterAVSDevice =
  req
    "RegisterAVSDevice"
    "fixture/RegisterAVSDevice.yaml"

requestRejectSkill :: RejectSkill -> TestTree
requestRejectSkill =
  req
    "RejectSkill"
    "fixture/RejectSkill.yaml"

requestResolveRoom :: ResolveRoom -> TestTree
requestResolveRoom =
  req
    "ResolveRoom"
    "fixture/ResolveRoom.yaml"

requestRevokeInvitation :: RevokeInvitation -> TestTree
requestRevokeInvitation =
  req
    "RevokeInvitation"
    "fixture/RevokeInvitation.yaml"

requestSearchAddressBooks :: SearchAddressBooks -> TestTree
requestSearchAddressBooks =
  req
    "SearchAddressBooks"
    "fixture/SearchAddressBooks.yaml"

requestSearchContacts :: SearchContacts -> TestTree
requestSearchContacts =
  req
    "SearchContacts"
    "fixture/SearchContacts.yaml"

requestSearchDevices :: SearchDevices -> TestTree
requestSearchDevices =
  req
    "SearchDevices"
    "fixture/SearchDevices.yaml"

requestSearchNetworkProfiles :: SearchNetworkProfiles -> TestTree
requestSearchNetworkProfiles =
  req
    "SearchNetworkProfiles"
    "fixture/SearchNetworkProfiles.yaml"

requestSearchProfiles :: SearchProfiles -> TestTree
requestSearchProfiles =
  req
    "SearchProfiles"
    "fixture/SearchProfiles.yaml"

requestSearchRooms :: SearchRooms -> TestTree
requestSearchRooms =
  req
    "SearchRooms"
    "fixture/SearchRooms.yaml"

requestSearchSkillGroups :: SearchSkillGroups -> TestTree
requestSearchSkillGroups =
  req
    "SearchSkillGroups"
    "fixture/SearchSkillGroups.yaml"

requestSearchUsers :: SearchUsers -> TestTree
requestSearchUsers =
  req
    "SearchUsers"
    "fixture/SearchUsers.yaml"

requestSendAnnouncement :: SendAnnouncement -> TestTree
requestSendAnnouncement =
  req
    "SendAnnouncement"
    "fixture/SendAnnouncement.yaml"

requestSendInvitation :: SendInvitation -> TestTree
requestSendInvitation =
  req
    "SendInvitation"
    "fixture/SendInvitation.yaml"

requestStartDeviceSync :: StartDeviceSync -> TestTree
requestStartDeviceSync =
  req
    "StartDeviceSync"
    "fixture/StartDeviceSync.yaml"

requestStartSmartHomeApplianceDiscovery :: StartSmartHomeApplianceDiscovery -> TestTree
requestStartSmartHomeApplianceDiscovery =
  req
    "StartSmartHomeApplianceDiscovery"
    "fixture/StartSmartHomeApplianceDiscovery.yaml"

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

requestUpdateAddressBook :: UpdateAddressBook -> TestTree
requestUpdateAddressBook =
  req
    "UpdateAddressBook"
    "fixture/UpdateAddressBook.yaml"

requestUpdateBusinessReportSchedule :: UpdateBusinessReportSchedule -> TestTree
requestUpdateBusinessReportSchedule =
  req
    "UpdateBusinessReportSchedule"
    "fixture/UpdateBusinessReportSchedule.yaml"

requestUpdateConferenceProvider :: UpdateConferenceProvider -> TestTree
requestUpdateConferenceProvider =
  req
    "UpdateConferenceProvider"
    "fixture/UpdateConferenceProvider.yaml"

requestUpdateContact :: UpdateContact -> TestTree
requestUpdateContact =
  req
    "UpdateContact"
    "fixture/UpdateContact.yaml"

requestUpdateDevice :: UpdateDevice -> TestTree
requestUpdateDevice =
  req
    "UpdateDevice"
    "fixture/UpdateDevice.yaml"

requestUpdateGateway :: UpdateGateway -> TestTree
requestUpdateGateway =
  req
    "UpdateGateway"
    "fixture/UpdateGateway.yaml"

requestUpdateGatewayGroup :: UpdateGatewayGroup -> TestTree
requestUpdateGatewayGroup =
  req
    "UpdateGatewayGroup"
    "fixture/UpdateGatewayGroup.yaml"

requestUpdateNetworkProfile :: UpdateNetworkProfile -> TestTree
requestUpdateNetworkProfile =
  req
    "UpdateNetworkProfile"
    "fixture/UpdateNetworkProfile.yaml"

requestUpdateProfile :: UpdateProfile -> TestTree
requestUpdateProfile =
  req
    "UpdateProfile"
    "fixture/UpdateProfile.yaml"

requestUpdateRoom :: UpdateRoom -> TestTree
requestUpdateRoom =
  req
    "UpdateRoom"
    "fixture/UpdateRoom.yaml"

requestUpdateSkillGroup :: UpdateSkillGroup -> TestTree
requestUpdateSkillGroup =
  req
    "UpdateSkillGroup"
    "fixture/UpdateSkillGroup.yaml"

-- Responses

responseApproveSkill :: ApproveSkillResponse -> TestTree
responseApproveSkill =
  res
    "ApproveSkillResponse"
    "fixture/ApproveSkillResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApproveSkill)

responseAssociateContactWithAddressBook :: AssociateContactWithAddressBookResponse -> TestTree
responseAssociateContactWithAddressBook =
  res
    "AssociateContactWithAddressBookResponse"
    "fixture/AssociateContactWithAddressBookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateContactWithAddressBook)

responseAssociateDeviceWithNetworkProfile :: AssociateDeviceWithNetworkProfileResponse -> TestTree
responseAssociateDeviceWithNetworkProfile =
  res
    "AssociateDeviceWithNetworkProfileResponse"
    "fixture/AssociateDeviceWithNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDeviceWithNetworkProfile)

responseAssociateDeviceWithRoom :: AssociateDeviceWithRoomResponse -> TestTree
responseAssociateDeviceWithRoom =
  res
    "AssociateDeviceWithRoomResponse"
    "fixture/AssociateDeviceWithRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDeviceWithRoom)

responseAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoomResponse -> TestTree
responseAssociateSkillGroupWithRoom =
  res
    "AssociateSkillGroupWithRoomResponse"
    "fixture/AssociateSkillGroupWithRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSkillGroupWithRoom)

responseAssociateSkillWithSkillGroup :: AssociateSkillWithSkillGroupResponse -> TestTree
responseAssociateSkillWithSkillGroup =
  res
    "AssociateSkillWithSkillGroupResponse"
    "fixture/AssociateSkillWithSkillGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSkillWithSkillGroup)

responseAssociateSkillWithUsers :: AssociateSkillWithUsersResponse -> TestTree
responseAssociateSkillWithUsers =
  res
    "AssociateSkillWithUsersResponse"
    "fixture/AssociateSkillWithUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSkillWithUsers)

responseCreateAddressBook :: CreateAddressBookResponse -> TestTree
responseCreateAddressBook =
  res
    "CreateAddressBookResponse"
    "fixture/CreateAddressBookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAddressBook)

responseCreateBusinessReportSchedule :: CreateBusinessReportScheduleResponse -> TestTree
responseCreateBusinessReportSchedule =
  res
    "CreateBusinessReportScheduleResponse"
    "fixture/CreateBusinessReportScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBusinessReportSchedule)

responseCreateConferenceProvider :: CreateConferenceProviderResponse -> TestTree
responseCreateConferenceProvider =
  res
    "CreateConferenceProviderResponse"
    "fixture/CreateConferenceProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConferenceProvider)

responseCreateContact :: CreateContactResponse -> TestTree
responseCreateContact =
  res
    "CreateContactResponse"
    "fixture/CreateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContact)

responseCreateGatewayGroup :: CreateGatewayGroupResponse -> TestTree
responseCreateGatewayGroup =
  res
    "CreateGatewayGroupResponse"
    "fixture/CreateGatewayGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGatewayGroup)

responseCreateNetworkProfile :: CreateNetworkProfileResponse -> TestTree
responseCreateNetworkProfile =
  res
    "CreateNetworkProfileResponse"
    "fixture/CreateNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkProfile)

responseCreateProfile :: CreateProfileResponse -> TestTree
responseCreateProfile =
  res
    "CreateProfileResponse"
    "fixture/CreateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfile)

responseCreateRoom :: CreateRoomResponse -> TestTree
responseCreateRoom =
  res
    "CreateRoomResponse"
    "fixture/CreateRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoom)

responseCreateSkillGroup :: CreateSkillGroupResponse -> TestTree
responseCreateSkillGroup =
  res
    "CreateSkillGroupResponse"
    "fixture/CreateSkillGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSkillGroup)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseDeleteAddressBook :: DeleteAddressBookResponse -> TestTree
responseDeleteAddressBook =
  res
    "DeleteAddressBookResponse"
    "fixture/DeleteAddressBookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAddressBook)

responseDeleteBusinessReportSchedule :: DeleteBusinessReportScheduleResponse -> TestTree
responseDeleteBusinessReportSchedule =
  res
    "DeleteBusinessReportScheduleResponse"
    "fixture/DeleteBusinessReportScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBusinessReportSchedule)

responseDeleteConferenceProvider :: DeleteConferenceProviderResponse -> TestTree
responseDeleteConferenceProvider =
  res
    "DeleteConferenceProviderResponse"
    "fixture/DeleteConferenceProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConferenceProvider)

responseDeleteContact :: DeleteContactResponse -> TestTree
responseDeleteContact =
  res
    "DeleteContactResponse"
    "fixture/DeleteContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContact)

responseDeleteDevice :: DeleteDeviceResponse -> TestTree
responseDeleteDevice =
  res
    "DeleteDeviceResponse"
    "fixture/DeleteDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDevice)

responseDeleteDeviceUsageData :: DeleteDeviceUsageDataResponse -> TestTree
responseDeleteDeviceUsageData =
  res
    "DeleteDeviceUsageDataResponse"
    "fixture/DeleteDeviceUsageDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeviceUsageData)

responseDeleteGatewayGroup :: DeleteGatewayGroupResponse -> TestTree
responseDeleteGatewayGroup =
  res
    "DeleteGatewayGroupResponse"
    "fixture/DeleteGatewayGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGatewayGroup)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile =
  res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkProfile)

responseDeleteProfile :: DeleteProfileResponse -> TestTree
responseDeleteProfile =
  res
    "DeleteProfileResponse"
    "fixture/DeleteProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfile)

responseDeleteRoom :: DeleteRoomResponse -> TestTree
responseDeleteRoom =
  res
    "DeleteRoomResponse"
    "fixture/DeleteRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoom)

responseDeleteRoomSkillParameter :: DeleteRoomSkillParameterResponse -> TestTree
responseDeleteRoomSkillParameter =
  res
    "DeleteRoomSkillParameterResponse"
    "fixture/DeleteRoomSkillParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoomSkillParameter)

responseDeleteSkillAuthorization :: DeleteSkillAuthorizationResponse -> TestTree
responseDeleteSkillAuthorization =
  res
    "DeleteSkillAuthorizationResponse"
    "fixture/DeleteSkillAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSkillAuthorization)

responseDeleteSkillGroup :: DeleteSkillGroupResponse -> TestTree
responseDeleteSkillGroup =
  res
    "DeleteSkillGroupResponse"
    "fixture/DeleteSkillGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSkillGroup)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDisassociateContactFromAddressBook :: DisassociateContactFromAddressBookResponse -> TestTree
responseDisassociateContactFromAddressBook =
  res
    "DisassociateContactFromAddressBookResponse"
    "fixture/DisassociateContactFromAddressBookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateContactFromAddressBook)

responseDisassociateDeviceFromRoom :: DisassociateDeviceFromRoomResponse -> TestTree
responseDisassociateDeviceFromRoom =
  res
    "DisassociateDeviceFromRoomResponse"
    "fixture/DisassociateDeviceFromRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDeviceFromRoom)

responseDisassociateSkillFromSkillGroup :: DisassociateSkillFromSkillGroupResponse -> TestTree
responseDisassociateSkillFromSkillGroup =
  res
    "DisassociateSkillFromSkillGroupResponse"
    "fixture/DisassociateSkillFromSkillGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSkillFromSkillGroup)

responseDisassociateSkillFromUsers :: DisassociateSkillFromUsersResponse -> TestTree
responseDisassociateSkillFromUsers =
  res
    "DisassociateSkillFromUsersResponse"
    "fixture/DisassociateSkillFromUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSkillFromUsers)

responseDisassociateSkillGroupFromRoom :: DisassociateSkillGroupFromRoomResponse -> TestTree
responseDisassociateSkillGroupFromRoom =
  res
    "DisassociateSkillGroupFromRoomResponse"
    "fixture/DisassociateSkillGroupFromRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSkillGroupFromRoom)

responseForgetSmartHomeAppliances :: ForgetSmartHomeAppliancesResponse -> TestTree
responseForgetSmartHomeAppliances =
  res
    "ForgetSmartHomeAppliancesResponse"
    "fixture/ForgetSmartHomeAppliancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ForgetSmartHomeAppliances)

responseGetAddressBook :: GetAddressBookResponse -> TestTree
responseGetAddressBook =
  res
    "GetAddressBookResponse"
    "fixture/GetAddressBookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAddressBook)

responseGetConferencePreference :: GetConferencePreferenceResponse -> TestTree
responseGetConferencePreference =
  res
    "GetConferencePreferenceResponse"
    "fixture/GetConferencePreferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConferencePreference)

responseGetConferenceProvider :: GetConferenceProviderResponse -> TestTree
responseGetConferenceProvider =
  res
    "GetConferenceProviderResponse"
    "fixture/GetConferenceProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConferenceProvider)

responseGetContact :: GetContactResponse -> TestTree
responseGetContact =
  res
    "GetContactResponse"
    "fixture/GetContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContact)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevice)

responseGetGateway :: GetGatewayResponse -> TestTree
responseGetGateway =
  res
    "GetGatewayResponse"
    "fixture/GetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGateway)

responseGetGatewayGroup :: GetGatewayGroupResponse -> TestTree
responseGetGatewayGroup =
  res
    "GetGatewayGroupResponse"
    "fixture/GetGatewayGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGatewayGroup)

responseGetInvitationConfiguration :: GetInvitationConfigurationResponse -> TestTree
responseGetInvitationConfiguration =
  res
    "GetInvitationConfigurationResponse"
    "fixture/GetInvitationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInvitationConfiguration)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile =
  res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkProfile)

responseGetProfile :: GetProfileResponse -> TestTree
responseGetProfile =
  res
    "GetProfileResponse"
    "fixture/GetProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProfile)

responseGetRoom :: GetRoomResponse -> TestTree
responseGetRoom =
  res
    "GetRoomResponse"
    "fixture/GetRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoom)

responseGetRoomSkillParameter :: GetRoomSkillParameterResponse -> TestTree
responseGetRoomSkillParameter =
  res
    "GetRoomSkillParameterResponse"
    "fixture/GetRoomSkillParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoomSkillParameter)

responseGetSkillGroup :: GetSkillGroupResponse -> TestTree
responseGetSkillGroup =
  res
    "GetSkillGroupResponse"
    "fixture/GetSkillGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSkillGroup)

responseListBusinessReportSchedules :: ListBusinessReportSchedulesResponse -> TestTree
responseListBusinessReportSchedules =
  res
    "ListBusinessReportSchedulesResponse"
    "fixture/ListBusinessReportSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBusinessReportSchedules)

responseListConferenceProviders :: ListConferenceProvidersResponse -> TestTree
responseListConferenceProviders =
  res
    "ListConferenceProvidersResponse"
    "fixture/ListConferenceProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConferenceProviders)

responseListDeviceEvents :: ListDeviceEventsResponse -> TestTree
responseListDeviceEvents =
  res
    "ListDeviceEventsResponse"
    "fixture/ListDeviceEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceEvents)

responseListGatewayGroups :: ListGatewayGroupsResponse -> TestTree
responseListGatewayGroups =
  res
    "ListGatewayGroupsResponse"
    "fixture/ListGatewayGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGatewayGroups)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGateways)

responseListSkills :: ListSkillsResponse -> TestTree
responseListSkills =
  res
    "ListSkillsResponse"
    "fixture/ListSkillsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSkills)

responseListSkillsStoreCategories :: ListSkillsStoreCategoriesResponse -> TestTree
responseListSkillsStoreCategories =
  res
    "ListSkillsStoreCategoriesResponse"
    "fixture/ListSkillsStoreCategoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSkillsStoreCategories)

responseListSkillsStoreSkillsByCategory :: ListSkillsStoreSkillsByCategoryResponse -> TestTree
responseListSkillsStoreSkillsByCategory =
  res
    "ListSkillsStoreSkillsByCategoryResponse"
    "fixture/ListSkillsStoreSkillsByCategoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSkillsStoreSkillsByCategory)

responseListSmartHomeAppliances :: ListSmartHomeAppliancesResponse -> TestTree
responseListSmartHomeAppliances =
  res
    "ListSmartHomeAppliancesResponse"
    "fixture/ListSmartHomeAppliancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSmartHomeAppliances)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responsePutConferencePreference :: PutConferencePreferenceResponse -> TestTree
responsePutConferencePreference =
  res
    "PutConferencePreferenceResponse"
    "fixture/PutConferencePreferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConferencePreference)

responsePutInvitationConfiguration :: PutInvitationConfigurationResponse -> TestTree
responsePutInvitationConfiguration =
  res
    "PutInvitationConfigurationResponse"
    "fixture/PutInvitationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInvitationConfiguration)

responsePutRoomSkillParameter :: PutRoomSkillParameterResponse -> TestTree
responsePutRoomSkillParameter =
  res
    "PutRoomSkillParameterResponse"
    "fixture/PutRoomSkillParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRoomSkillParameter)

responsePutSkillAuthorization :: PutSkillAuthorizationResponse -> TestTree
responsePutSkillAuthorization =
  res
    "PutSkillAuthorizationResponse"
    "fixture/PutSkillAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSkillAuthorization)

responseRegisterAVSDevice :: RegisterAVSDeviceResponse -> TestTree
responseRegisterAVSDevice =
  res
    "RegisterAVSDeviceResponse"
    "fixture/RegisterAVSDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterAVSDevice)

responseRejectSkill :: RejectSkillResponse -> TestTree
responseRejectSkill =
  res
    "RejectSkillResponse"
    "fixture/RejectSkillResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectSkill)

responseResolveRoom :: ResolveRoomResponse -> TestTree
responseResolveRoom =
  res
    "ResolveRoomResponse"
    "fixture/ResolveRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResolveRoom)

responseRevokeInvitation :: RevokeInvitationResponse -> TestTree
responseRevokeInvitation =
  res
    "RevokeInvitationResponse"
    "fixture/RevokeInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeInvitation)

responseSearchAddressBooks :: SearchAddressBooksResponse -> TestTree
responseSearchAddressBooks =
  res
    "SearchAddressBooksResponse"
    "fixture/SearchAddressBooksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchAddressBooks)

responseSearchContacts :: SearchContactsResponse -> TestTree
responseSearchContacts =
  res
    "SearchContactsResponse"
    "fixture/SearchContactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchContacts)

responseSearchDevices :: SearchDevicesResponse -> TestTree
responseSearchDevices =
  res
    "SearchDevicesResponse"
    "fixture/SearchDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchDevices)

responseSearchNetworkProfiles :: SearchNetworkProfilesResponse -> TestTree
responseSearchNetworkProfiles =
  res
    "SearchNetworkProfilesResponse"
    "fixture/SearchNetworkProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchNetworkProfiles)

responseSearchProfiles :: SearchProfilesResponse -> TestTree
responseSearchProfiles =
  res
    "SearchProfilesResponse"
    "fixture/SearchProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchProfiles)

responseSearchRooms :: SearchRoomsResponse -> TestTree
responseSearchRooms =
  res
    "SearchRoomsResponse"
    "fixture/SearchRoomsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchRooms)

responseSearchSkillGroups :: SearchSkillGroupsResponse -> TestTree
responseSearchSkillGroups =
  res
    "SearchSkillGroupsResponse"
    "fixture/SearchSkillGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchSkillGroups)

responseSearchUsers :: SearchUsersResponse -> TestTree
responseSearchUsers =
  res
    "SearchUsersResponse"
    "fixture/SearchUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchUsers)

responseSendAnnouncement :: SendAnnouncementResponse -> TestTree
responseSendAnnouncement =
  res
    "SendAnnouncementResponse"
    "fixture/SendAnnouncementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendAnnouncement)

responseSendInvitation :: SendInvitationResponse -> TestTree
responseSendInvitation =
  res
    "SendInvitationResponse"
    "fixture/SendInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendInvitation)

responseStartDeviceSync :: StartDeviceSyncResponse -> TestTree
responseStartDeviceSync =
  res
    "StartDeviceSyncResponse"
    "fixture/StartDeviceSyncResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDeviceSync)

responseStartSmartHomeApplianceDiscovery :: StartSmartHomeApplianceDiscoveryResponse -> TestTree
responseStartSmartHomeApplianceDiscovery =
  res
    "StartSmartHomeApplianceDiscoveryResponse"
    "fixture/StartSmartHomeApplianceDiscoveryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSmartHomeApplianceDiscovery)

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

responseUpdateAddressBook :: UpdateAddressBookResponse -> TestTree
responseUpdateAddressBook =
  res
    "UpdateAddressBookResponse"
    "fixture/UpdateAddressBookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAddressBook)

responseUpdateBusinessReportSchedule :: UpdateBusinessReportScheduleResponse -> TestTree
responseUpdateBusinessReportSchedule =
  res
    "UpdateBusinessReportScheduleResponse"
    "fixture/UpdateBusinessReportScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBusinessReportSchedule)

responseUpdateConferenceProvider :: UpdateConferenceProviderResponse -> TestTree
responseUpdateConferenceProvider =
  res
    "UpdateConferenceProviderResponse"
    "fixture/UpdateConferenceProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConferenceProvider)

responseUpdateContact :: UpdateContactResponse -> TestTree
responseUpdateContact =
  res
    "UpdateContactResponse"
    "fixture/UpdateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContact)

responseUpdateDevice :: UpdateDeviceResponse -> TestTree
responseUpdateDevice =
  res
    "UpdateDeviceResponse"
    "fixture/UpdateDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevice)

responseUpdateGateway :: UpdateGatewayResponse -> TestTree
responseUpdateGateway =
  res
    "UpdateGatewayResponse"
    "fixture/UpdateGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGateway)

responseUpdateGatewayGroup :: UpdateGatewayGroupResponse -> TestTree
responseUpdateGatewayGroup =
  res
    "UpdateGatewayGroupResponse"
    "fixture/UpdateGatewayGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayGroup)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile =
  res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNetworkProfile)

responseUpdateProfile :: UpdateProfileResponse -> TestTree
responseUpdateProfile =
  res
    "UpdateProfileResponse"
    "fixture/UpdateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProfile)

responseUpdateRoom :: UpdateRoomResponse -> TestTree
responseUpdateRoom =
  res
    "UpdateRoomResponse"
    "fixture/UpdateRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoom)

responseUpdateSkillGroup :: UpdateSkillGroupResponse -> TestTree
responseUpdateSkillGroup =
  res
    "UpdateSkillGroupResponse"
    "fixture/UpdateSkillGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSkillGroup)
