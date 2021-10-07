{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AlexaBusiness
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.AlexaBusiness where

import Data.Proxy
import Network.AWS.AlexaBusiness
import Test.AWS.AlexaBusiness.Internal
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
--         [ requestStartDeviceSync $
--             newStartDeviceSync
--
--         , requestCreateProfile $
--             newCreateProfile
--
--         , requestDeleteBusinessReportSchedule $
--             newDeleteBusinessReportSchedule
--
--         , requestListBusinessReportSchedules $
--             newListBusinessReportSchedules
--
--         , requestUpdateNetworkProfile $
--             newUpdateNetworkProfile
--
--         , requestDeleteDeviceUsageData $
--             newDeleteDeviceUsageData
--
--         , requestDeleteNetworkProfile $
--             newDeleteNetworkProfile
--
--         , requestCreateContact $
--             newCreateContact
--
--         , requestUpdateBusinessReportSchedule $
--             newUpdateBusinessReportSchedule
--
--         , requestAssociateContactWithAddressBook $
--             newAssociateContactWithAddressBook
--
--         , requestListSkillsStoreCategories $
--             newListSkillsStoreCategories
--
--         , requestGetConferenceProvider $
--             newGetConferenceProvider
--
--         , requestGetAddressBook $
--             newGetAddressBook
--
--         , requestCreateBusinessReportSchedule $
--             newCreateBusinessReportSchedule
--
--         , requestGetGatewayGroup $
--             newGetGatewayGroup
--
--         , requestGetRoom $
--             newGetRoom
--
--         , requestCreateNetworkProfile $
--             newCreateNetworkProfile
--
--         , requestSearchProfiles $
--             newSearchProfiles
--
--         , requestDeleteAddressBook $
--             newDeleteAddressBook
--
--         , requestUpdateAddressBook $
--             newUpdateAddressBook
--
--         , requestDeleteRoomSkillParameter $
--             newDeleteRoomSkillParameter
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestListSkillsStoreSkillsByCategory $
--             newListSkillsStoreSkillsByCategory
--
--         , requestPutSkillAuthorization $
--             newPutSkillAuthorization
--
--         , requestDisassociateContactFromAddressBook $
--             newDisassociateContactFromAddressBook
--
--         , requestAssociateDeviceWithNetworkProfile $
--             newAssociateDeviceWithNetworkProfile
--
--         , requestSearchSkillGroups $
--             newSearchSkillGroups
--
--         , requestResolveRoom $
--             newResolveRoom
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestSearchNetworkProfiles $
--             newSearchNetworkProfiles
--
--         , requestPutInvitationConfiguration $
--             newPutInvitationConfiguration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetSkillGroup $
--             newGetSkillGroup
--
--         , requestSendInvitation $
--             newSendInvitation
--
--         , requestListDeviceEvents $
--             newListDeviceEvents
--
--         , requestRegisterAVSDevice $
--             newRegisterAVSDevice
--
--         , requestGetProfile $
--             newGetProfile
--
--         , requestSearchContacts $
--             newSearchContacts
--
--         , requestDisassociateSkillGroupFromRoom $
--             newDisassociateSkillGroupFromRoom
--
--         , requestAssociateSkillWithUsers $
--             newAssociateSkillWithUsers
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestForgetSmartHomeAppliances $
--             newForgetSmartHomeAppliances
--
--         , requestSendAnnouncement $
--             newSendAnnouncement
--
--         , requestAssociateSkillGroupWithRoom $
--             newAssociateSkillGroupWithRoom
--
--         , requestGetInvitationConfiguration $
--             newGetInvitationConfiguration
--
--         , requestDisassociateSkillFromUsers $
--             newDisassociateSkillFromUsers
--
--         , requestUpdateSkillGroup $
--             newUpdateSkillGroup
--
--         , requestDeleteSkillGroup $
--             newDeleteSkillGroup
--
--         , requestSearchUsers $
--             newSearchUsers
--
--         , requestPutConferencePreference $
--             newPutConferencePreference
--
--         , requestUpdateContact $
--             newUpdateContact
--
--         , requestStartSmartHomeApplianceDiscovery $
--             newStartSmartHomeApplianceDiscovery
--
--         , requestDeleteContact $
--             newDeleteContact
--
--         , requestRevokeInvitation $
--             newRevokeInvitation
--
--         , requestAssociateSkillWithSkillGroup $
--             newAssociateSkillWithSkillGroup
--
--         , requestUpdateDevice $
--             newUpdateDevice
--
--         , requestGetRoomSkillParameter $
--             newGetRoomSkillParameter
--
--         , requestUpdateGateway $
--             newUpdateGateway
--
--         , requestAssociateDeviceWithRoom $
--             newAssociateDeviceWithRoom
--
--         , requestListGateways $
--             newListGateways
--
--         , requestDeleteDevice $
--             newDeleteDevice
--
--         , requestUpdateRoom $
--             newUpdateRoom
--
--         , requestListGatewayGroups $
--             newListGatewayGroups
--
--         , requestGetContact $
--             newGetContact
--
--         , requestRejectSkill $
--             newRejectSkill
--
--         , requestListConferenceProviders $
--             newListConferenceProviders
--
--         , requestUpdateConferenceProvider $
--             newUpdateConferenceProvider
--
--         , requestDeleteRoom $
--             newDeleteRoom
--
--         , requestApproveSkill $
--             newApproveSkill
--
--         , requestDeleteGatewayGroup $
--             newDeleteGatewayGroup
--
--         , requestDeleteConferenceProvider $
--             newDeleteConferenceProvider
--
--         , requestGetGateway $
--             newGetGateway
--
--         , requestPutRoomSkillParameter $
--             newPutRoomSkillParameter
--
--         , requestUpdateGatewayGroup $
--             newUpdateGatewayGroup
--
--         , requestListTags $
--             newListTags
--
--         , requestDeleteSkillAuthorization $
--             newDeleteSkillAuthorization
--
--         , requestCreateGatewayGroup $
--             newCreateGatewayGroup
--
--         , requestGetNetworkProfile $
--             newGetNetworkProfile
--
--         , requestDisassociateDeviceFromRoom $
--             newDisassociateDeviceFromRoom
--
--         , requestGetConferencePreference $
--             newGetConferencePreference
--
--         , requestCreateRoom $
--             newCreateRoom
--
--         , requestCreateAddressBook $
--             newCreateAddressBook
--
--         , requestDisassociateSkillFromSkillGroup $
--             newDisassociateSkillFromSkillGroup
--
--         , requestCreateConferenceProvider $
--             newCreateConferenceProvider
--
--         , requestListSkills $
--             newListSkills
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestSearchDevices $
--             newSearchDevices
--
--         , requestSearchRooms $
--             newSearchRooms
--
--         , requestCreateSkillGroup $
--             newCreateSkillGroup
--
--         , requestDeleteProfile $
--             newDeleteProfile
--
--         , requestListSmartHomeAppliances $
--             newListSmartHomeAppliances
--
--         , requestSearchAddressBooks $
--             newSearchAddressBooks
--
--         , requestUpdateProfile $
--             newUpdateProfile
--
--           ]

--     , testGroup "response"
--         [ responseStartDeviceSync $
--             newStartDeviceSyncResponse
--
--         , responseCreateProfile $
--             newCreateProfileResponse
--
--         , responseDeleteBusinessReportSchedule $
--             newDeleteBusinessReportScheduleResponse
--
--         , responseListBusinessReportSchedules $
--             newListBusinessReportSchedulesResponse
--
--         , responseUpdateNetworkProfile $
--             newUpdateNetworkProfileResponse
--
--         , responseDeleteDeviceUsageData $
--             newDeleteDeviceUsageDataResponse
--
--         , responseDeleteNetworkProfile $
--             newDeleteNetworkProfileResponse
--
--         , responseCreateContact $
--             newCreateContactResponse
--
--         , responseUpdateBusinessReportSchedule $
--             newUpdateBusinessReportScheduleResponse
--
--         , responseAssociateContactWithAddressBook $
--             newAssociateContactWithAddressBookResponse
--
--         , responseListSkillsStoreCategories $
--             newListSkillsStoreCategoriesResponse
--
--         , responseGetConferenceProvider $
--             newGetConferenceProviderResponse
--
--         , responseGetAddressBook $
--             newGetAddressBookResponse
--
--         , responseCreateBusinessReportSchedule $
--             newCreateBusinessReportScheduleResponse
--
--         , responseGetGatewayGroup $
--             newGetGatewayGroupResponse
--
--         , responseGetRoom $
--             newGetRoomResponse
--
--         , responseCreateNetworkProfile $
--             newCreateNetworkProfileResponse
--
--         , responseSearchProfiles $
--             newSearchProfilesResponse
--
--         , responseDeleteAddressBook $
--             newDeleteAddressBookResponse
--
--         , responseUpdateAddressBook $
--             newUpdateAddressBookResponse
--
--         , responseDeleteRoomSkillParameter $
--             newDeleteRoomSkillParameterResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseListSkillsStoreSkillsByCategory $
--             newListSkillsStoreSkillsByCategoryResponse
--
--         , responsePutSkillAuthorization $
--             newPutSkillAuthorizationResponse
--
--         , responseDisassociateContactFromAddressBook $
--             newDisassociateContactFromAddressBookResponse
--
--         , responseAssociateDeviceWithNetworkProfile $
--             newAssociateDeviceWithNetworkProfileResponse
--
--         , responseSearchSkillGroups $
--             newSearchSkillGroupsResponse
--
--         , responseResolveRoom $
--             newResolveRoomResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseSearchNetworkProfiles $
--             newSearchNetworkProfilesResponse
--
--         , responsePutInvitationConfiguration $
--             newPutInvitationConfigurationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetSkillGroup $
--             newGetSkillGroupResponse
--
--         , responseSendInvitation $
--             newSendInvitationResponse
--
--         , responseListDeviceEvents $
--             newListDeviceEventsResponse
--
--         , responseRegisterAVSDevice $
--             newRegisterAVSDeviceResponse
--
--         , responseGetProfile $
--             newGetProfileResponse
--
--         , responseSearchContacts $
--             newSearchContactsResponse
--
--         , responseDisassociateSkillGroupFromRoom $
--             newDisassociateSkillGroupFromRoomResponse
--
--         , responseAssociateSkillWithUsers $
--             newAssociateSkillWithUsersResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseForgetSmartHomeAppliances $
--             newForgetSmartHomeAppliancesResponse
--
--         , responseSendAnnouncement $
--             newSendAnnouncementResponse
--
--         , responseAssociateSkillGroupWithRoom $
--             newAssociateSkillGroupWithRoomResponse
--
--         , responseGetInvitationConfiguration $
--             newGetInvitationConfigurationResponse
--
--         , responseDisassociateSkillFromUsers $
--             newDisassociateSkillFromUsersResponse
--
--         , responseUpdateSkillGroup $
--             newUpdateSkillGroupResponse
--
--         , responseDeleteSkillGroup $
--             newDeleteSkillGroupResponse
--
--         , responseSearchUsers $
--             newSearchUsersResponse
--
--         , responsePutConferencePreference $
--             newPutConferencePreferenceResponse
--
--         , responseUpdateContact $
--             newUpdateContactResponse
--
--         , responseStartSmartHomeApplianceDiscovery $
--             newStartSmartHomeApplianceDiscoveryResponse
--
--         , responseDeleteContact $
--             newDeleteContactResponse
--
--         , responseRevokeInvitation $
--             newRevokeInvitationResponse
--
--         , responseAssociateSkillWithSkillGroup $
--             newAssociateSkillWithSkillGroupResponse
--
--         , responseUpdateDevice $
--             newUpdateDeviceResponse
--
--         , responseGetRoomSkillParameter $
--             newGetRoomSkillParameterResponse
--
--         , responseUpdateGateway $
--             newUpdateGatewayResponse
--
--         , responseAssociateDeviceWithRoom $
--             newAssociateDeviceWithRoomResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseDeleteDevice $
--             newDeleteDeviceResponse
--
--         , responseUpdateRoom $
--             newUpdateRoomResponse
--
--         , responseListGatewayGroups $
--             newListGatewayGroupsResponse
--
--         , responseGetContact $
--             newGetContactResponse
--
--         , responseRejectSkill $
--             newRejectSkillResponse
--
--         , responseListConferenceProviders $
--             newListConferenceProvidersResponse
--
--         , responseUpdateConferenceProvider $
--             newUpdateConferenceProviderResponse
--
--         , responseDeleteRoom $
--             newDeleteRoomResponse
--
--         , responseApproveSkill $
--             newApproveSkillResponse
--
--         , responseDeleteGatewayGroup $
--             newDeleteGatewayGroupResponse
--
--         , responseDeleteConferenceProvider $
--             newDeleteConferenceProviderResponse
--
--         , responseGetGateway $
--             newGetGatewayResponse
--
--         , responsePutRoomSkillParameter $
--             newPutRoomSkillParameterResponse
--
--         , responseUpdateGatewayGroup $
--             newUpdateGatewayGroupResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDeleteSkillAuthorization $
--             newDeleteSkillAuthorizationResponse
--
--         , responseCreateGatewayGroup $
--             newCreateGatewayGroupResponse
--
--         , responseGetNetworkProfile $
--             newGetNetworkProfileResponse
--
--         , responseDisassociateDeviceFromRoom $
--             newDisassociateDeviceFromRoomResponse
--
--         , responseGetConferencePreference $
--             newGetConferencePreferenceResponse
--
--         , responseCreateRoom $
--             newCreateRoomResponse
--
--         , responseCreateAddressBook $
--             newCreateAddressBookResponse
--
--         , responseDisassociateSkillFromSkillGroup $
--             newDisassociateSkillFromSkillGroupResponse
--
--         , responseCreateConferenceProvider $
--             newCreateConferenceProviderResponse
--
--         , responseListSkills $
--             newListSkillsResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseSearchDevices $
--             newSearchDevicesResponse
--
--         , responseSearchRooms $
--             newSearchRoomsResponse
--
--         , responseCreateSkillGroup $
--             newCreateSkillGroupResponse
--
--         , responseDeleteProfile $
--             newDeleteProfileResponse
--
--         , responseListSmartHomeAppliances $
--             newListSmartHomeAppliancesResponse
--
--         , responseSearchAddressBooks $
--             newSearchAddressBooksResponse
--
--         , responseUpdateProfile $
--             newUpdateProfileResponse
--
--           ]
--     ]

-- Requests

requestStartDeviceSync :: StartDeviceSync -> TestTree
requestStartDeviceSync =
  req
    "StartDeviceSync"
    "fixture/StartDeviceSync.yaml"

requestCreateProfile :: CreateProfile -> TestTree
requestCreateProfile =
  req
    "CreateProfile"
    "fixture/CreateProfile.yaml"

requestDeleteBusinessReportSchedule :: DeleteBusinessReportSchedule -> TestTree
requestDeleteBusinessReportSchedule =
  req
    "DeleteBusinessReportSchedule"
    "fixture/DeleteBusinessReportSchedule.yaml"

requestListBusinessReportSchedules :: ListBusinessReportSchedules -> TestTree
requestListBusinessReportSchedules =
  req
    "ListBusinessReportSchedules"
    "fixture/ListBusinessReportSchedules.yaml"

requestUpdateNetworkProfile :: UpdateNetworkProfile -> TestTree
requestUpdateNetworkProfile =
  req
    "UpdateNetworkProfile"
    "fixture/UpdateNetworkProfile.yaml"

requestDeleteDeviceUsageData :: DeleteDeviceUsageData -> TestTree
requestDeleteDeviceUsageData =
  req
    "DeleteDeviceUsageData"
    "fixture/DeleteDeviceUsageData.yaml"

requestDeleteNetworkProfile :: DeleteNetworkProfile -> TestTree
requestDeleteNetworkProfile =
  req
    "DeleteNetworkProfile"
    "fixture/DeleteNetworkProfile.yaml"

requestCreateContact :: CreateContact -> TestTree
requestCreateContact =
  req
    "CreateContact"
    "fixture/CreateContact.yaml"

requestUpdateBusinessReportSchedule :: UpdateBusinessReportSchedule -> TestTree
requestUpdateBusinessReportSchedule =
  req
    "UpdateBusinessReportSchedule"
    "fixture/UpdateBusinessReportSchedule.yaml"

requestAssociateContactWithAddressBook :: AssociateContactWithAddressBook -> TestTree
requestAssociateContactWithAddressBook =
  req
    "AssociateContactWithAddressBook"
    "fixture/AssociateContactWithAddressBook.yaml"

requestListSkillsStoreCategories :: ListSkillsStoreCategories -> TestTree
requestListSkillsStoreCategories =
  req
    "ListSkillsStoreCategories"
    "fixture/ListSkillsStoreCategories.yaml"

requestGetConferenceProvider :: GetConferenceProvider -> TestTree
requestGetConferenceProvider =
  req
    "GetConferenceProvider"
    "fixture/GetConferenceProvider.yaml"

requestGetAddressBook :: GetAddressBook -> TestTree
requestGetAddressBook =
  req
    "GetAddressBook"
    "fixture/GetAddressBook.yaml"

requestCreateBusinessReportSchedule :: CreateBusinessReportSchedule -> TestTree
requestCreateBusinessReportSchedule =
  req
    "CreateBusinessReportSchedule"
    "fixture/CreateBusinessReportSchedule.yaml"

requestGetGatewayGroup :: GetGatewayGroup -> TestTree
requestGetGatewayGroup =
  req
    "GetGatewayGroup"
    "fixture/GetGatewayGroup.yaml"

requestGetRoom :: GetRoom -> TestTree
requestGetRoom =
  req
    "GetRoom"
    "fixture/GetRoom.yaml"

requestCreateNetworkProfile :: CreateNetworkProfile -> TestTree
requestCreateNetworkProfile =
  req
    "CreateNetworkProfile"
    "fixture/CreateNetworkProfile.yaml"

requestSearchProfiles :: SearchProfiles -> TestTree
requestSearchProfiles =
  req
    "SearchProfiles"
    "fixture/SearchProfiles.yaml"

requestDeleteAddressBook :: DeleteAddressBook -> TestTree
requestDeleteAddressBook =
  req
    "DeleteAddressBook"
    "fixture/DeleteAddressBook.yaml"

requestUpdateAddressBook :: UpdateAddressBook -> TestTree
requestUpdateAddressBook =
  req
    "UpdateAddressBook"
    "fixture/UpdateAddressBook.yaml"

requestDeleteRoomSkillParameter :: DeleteRoomSkillParameter -> TestTree
requestDeleteRoomSkillParameter =
  req
    "DeleteRoomSkillParameter"
    "fixture/DeleteRoomSkillParameter.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice =
  req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestListSkillsStoreSkillsByCategory :: ListSkillsStoreSkillsByCategory -> TestTree
requestListSkillsStoreSkillsByCategory =
  req
    "ListSkillsStoreSkillsByCategory"
    "fixture/ListSkillsStoreSkillsByCategory.yaml"

requestPutSkillAuthorization :: PutSkillAuthorization -> TestTree
requestPutSkillAuthorization =
  req
    "PutSkillAuthorization"
    "fixture/PutSkillAuthorization.yaml"

requestDisassociateContactFromAddressBook :: DisassociateContactFromAddressBook -> TestTree
requestDisassociateContactFromAddressBook =
  req
    "DisassociateContactFromAddressBook"
    "fixture/DisassociateContactFromAddressBook.yaml"

requestAssociateDeviceWithNetworkProfile :: AssociateDeviceWithNetworkProfile -> TestTree
requestAssociateDeviceWithNetworkProfile =
  req
    "AssociateDeviceWithNetworkProfile"
    "fixture/AssociateDeviceWithNetworkProfile.yaml"

requestSearchSkillGroups :: SearchSkillGroups -> TestTree
requestSearchSkillGroups =
  req
    "SearchSkillGroups"
    "fixture/SearchSkillGroups.yaml"

requestResolveRoom :: ResolveRoom -> TestTree
requestResolveRoom =
  req
    "ResolveRoom"
    "fixture/ResolveRoom.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestSearchNetworkProfiles :: SearchNetworkProfiles -> TestTree
requestSearchNetworkProfiles =
  req
    "SearchNetworkProfiles"
    "fixture/SearchNetworkProfiles.yaml"

requestPutInvitationConfiguration :: PutInvitationConfiguration -> TestTree
requestPutInvitationConfiguration =
  req
    "PutInvitationConfiguration"
    "fixture/PutInvitationConfiguration.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetSkillGroup :: GetSkillGroup -> TestTree
requestGetSkillGroup =
  req
    "GetSkillGroup"
    "fixture/GetSkillGroup.yaml"

requestSendInvitation :: SendInvitation -> TestTree
requestSendInvitation =
  req
    "SendInvitation"
    "fixture/SendInvitation.yaml"

requestListDeviceEvents :: ListDeviceEvents -> TestTree
requestListDeviceEvents =
  req
    "ListDeviceEvents"
    "fixture/ListDeviceEvents.yaml"

requestRegisterAVSDevice :: RegisterAVSDevice -> TestTree
requestRegisterAVSDevice =
  req
    "RegisterAVSDevice"
    "fixture/RegisterAVSDevice.yaml"

requestGetProfile :: GetProfile -> TestTree
requestGetProfile =
  req
    "GetProfile"
    "fixture/GetProfile.yaml"

requestSearchContacts :: SearchContacts -> TestTree
requestSearchContacts =
  req
    "SearchContacts"
    "fixture/SearchContacts.yaml"

requestDisassociateSkillGroupFromRoom :: DisassociateSkillGroupFromRoom -> TestTree
requestDisassociateSkillGroupFromRoom =
  req
    "DisassociateSkillGroupFromRoom"
    "fixture/DisassociateSkillGroupFromRoom.yaml"

requestAssociateSkillWithUsers :: AssociateSkillWithUsers -> TestTree
requestAssociateSkillWithUsers =
  req
    "AssociateSkillWithUsers"
    "fixture/AssociateSkillWithUsers.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestForgetSmartHomeAppliances :: ForgetSmartHomeAppliances -> TestTree
requestForgetSmartHomeAppliances =
  req
    "ForgetSmartHomeAppliances"
    "fixture/ForgetSmartHomeAppliances.yaml"

requestSendAnnouncement :: SendAnnouncement -> TestTree
requestSendAnnouncement =
  req
    "SendAnnouncement"
    "fixture/SendAnnouncement.yaml"

requestAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoom -> TestTree
requestAssociateSkillGroupWithRoom =
  req
    "AssociateSkillGroupWithRoom"
    "fixture/AssociateSkillGroupWithRoom.yaml"

requestGetInvitationConfiguration :: GetInvitationConfiguration -> TestTree
requestGetInvitationConfiguration =
  req
    "GetInvitationConfiguration"
    "fixture/GetInvitationConfiguration.yaml"

requestDisassociateSkillFromUsers :: DisassociateSkillFromUsers -> TestTree
requestDisassociateSkillFromUsers =
  req
    "DisassociateSkillFromUsers"
    "fixture/DisassociateSkillFromUsers.yaml"

requestUpdateSkillGroup :: UpdateSkillGroup -> TestTree
requestUpdateSkillGroup =
  req
    "UpdateSkillGroup"
    "fixture/UpdateSkillGroup.yaml"

requestDeleteSkillGroup :: DeleteSkillGroup -> TestTree
requestDeleteSkillGroup =
  req
    "DeleteSkillGroup"
    "fixture/DeleteSkillGroup.yaml"

requestSearchUsers :: SearchUsers -> TestTree
requestSearchUsers =
  req
    "SearchUsers"
    "fixture/SearchUsers.yaml"

requestPutConferencePreference :: PutConferencePreference -> TestTree
requestPutConferencePreference =
  req
    "PutConferencePreference"
    "fixture/PutConferencePreference.yaml"

requestUpdateContact :: UpdateContact -> TestTree
requestUpdateContact =
  req
    "UpdateContact"
    "fixture/UpdateContact.yaml"

requestStartSmartHomeApplianceDiscovery :: StartSmartHomeApplianceDiscovery -> TestTree
requestStartSmartHomeApplianceDiscovery =
  req
    "StartSmartHomeApplianceDiscovery"
    "fixture/StartSmartHomeApplianceDiscovery.yaml"

requestDeleteContact :: DeleteContact -> TestTree
requestDeleteContact =
  req
    "DeleteContact"
    "fixture/DeleteContact.yaml"

requestRevokeInvitation :: RevokeInvitation -> TestTree
requestRevokeInvitation =
  req
    "RevokeInvitation"
    "fixture/RevokeInvitation.yaml"

requestAssociateSkillWithSkillGroup :: AssociateSkillWithSkillGroup -> TestTree
requestAssociateSkillWithSkillGroup =
  req
    "AssociateSkillWithSkillGroup"
    "fixture/AssociateSkillWithSkillGroup.yaml"

requestUpdateDevice :: UpdateDevice -> TestTree
requestUpdateDevice =
  req
    "UpdateDevice"
    "fixture/UpdateDevice.yaml"

requestGetRoomSkillParameter :: GetRoomSkillParameter -> TestTree
requestGetRoomSkillParameter =
  req
    "GetRoomSkillParameter"
    "fixture/GetRoomSkillParameter.yaml"

requestUpdateGateway :: UpdateGateway -> TestTree
requestUpdateGateway =
  req
    "UpdateGateway"
    "fixture/UpdateGateway.yaml"

requestAssociateDeviceWithRoom :: AssociateDeviceWithRoom -> TestTree
requestAssociateDeviceWithRoom =
  req
    "AssociateDeviceWithRoom"
    "fixture/AssociateDeviceWithRoom.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways =
  req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestDeleteDevice :: DeleteDevice -> TestTree
requestDeleteDevice =
  req
    "DeleteDevice"
    "fixture/DeleteDevice.yaml"

requestUpdateRoom :: UpdateRoom -> TestTree
requestUpdateRoom =
  req
    "UpdateRoom"
    "fixture/UpdateRoom.yaml"

requestListGatewayGroups :: ListGatewayGroups -> TestTree
requestListGatewayGroups =
  req
    "ListGatewayGroups"
    "fixture/ListGatewayGroups.yaml"

requestGetContact :: GetContact -> TestTree
requestGetContact =
  req
    "GetContact"
    "fixture/GetContact.yaml"

requestRejectSkill :: RejectSkill -> TestTree
requestRejectSkill =
  req
    "RejectSkill"
    "fixture/RejectSkill.yaml"

requestListConferenceProviders :: ListConferenceProviders -> TestTree
requestListConferenceProviders =
  req
    "ListConferenceProviders"
    "fixture/ListConferenceProviders.yaml"

requestUpdateConferenceProvider :: UpdateConferenceProvider -> TestTree
requestUpdateConferenceProvider =
  req
    "UpdateConferenceProvider"
    "fixture/UpdateConferenceProvider.yaml"

requestDeleteRoom :: DeleteRoom -> TestTree
requestDeleteRoom =
  req
    "DeleteRoom"
    "fixture/DeleteRoom.yaml"

requestApproveSkill :: ApproveSkill -> TestTree
requestApproveSkill =
  req
    "ApproveSkill"
    "fixture/ApproveSkill.yaml"

requestDeleteGatewayGroup :: DeleteGatewayGroup -> TestTree
requestDeleteGatewayGroup =
  req
    "DeleteGatewayGroup"
    "fixture/DeleteGatewayGroup.yaml"

requestDeleteConferenceProvider :: DeleteConferenceProvider -> TestTree
requestDeleteConferenceProvider =
  req
    "DeleteConferenceProvider"
    "fixture/DeleteConferenceProvider.yaml"

requestGetGateway :: GetGateway -> TestTree
requestGetGateway =
  req
    "GetGateway"
    "fixture/GetGateway.yaml"

requestPutRoomSkillParameter :: PutRoomSkillParameter -> TestTree
requestPutRoomSkillParameter =
  req
    "PutRoomSkillParameter"
    "fixture/PutRoomSkillParameter.yaml"

requestUpdateGatewayGroup :: UpdateGatewayGroup -> TestTree
requestUpdateGatewayGroup =
  req
    "UpdateGatewayGroup"
    "fixture/UpdateGatewayGroup.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDeleteSkillAuthorization :: DeleteSkillAuthorization -> TestTree
requestDeleteSkillAuthorization =
  req
    "DeleteSkillAuthorization"
    "fixture/DeleteSkillAuthorization.yaml"

requestCreateGatewayGroup :: CreateGatewayGroup -> TestTree
requestCreateGatewayGroup =
  req
    "CreateGatewayGroup"
    "fixture/CreateGatewayGroup.yaml"

requestGetNetworkProfile :: GetNetworkProfile -> TestTree
requestGetNetworkProfile =
  req
    "GetNetworkProfile"
    "fixture/GetNetworkProfile.yaml"

requestDisassociateDeviceFromRoom :: DisassociateDeviceFromRoom -> TestTree
requestDisassociateDeviceFromRoom =
  req
    "DisassociateDeviceFromRoom"
    "fixture/DisassociateDeviceFromRoom.yaml"

requestGetConferencePreference :: GetConferencePreference -> TestTree
requestGetConferencePreference =
  req
    "GetConferencePreference"
    "fixture/GetConferencePreference.yaml"

requestCreateRoom :: CreateRoom -> TestTree
requestCreateRoom =
  req
    "CreateRoom"
    "fixture/CreateRoom.yaml"

requestCreateAddressBook :: CreateAddressBook -> TestTree
requestCreateAddressBook =
  req
    "CreateAddressBook"
    "fixture/CreateAddressBook.yaml"

requestDisassociateSkillFromSkillGroup :: DisassociateSkillFromSkillGroup -> TestTree
requestDisassociateSkillFromSkillGroup =
  req
    "DisassociateSkillFromSkillGroup"
    "fixture/DisassociateSkillFromSkillGroup.yaml"

requestCreateConferenceProvider :: CreateConferenceProvider -> TestTree
requestCreateConferenceProvider =
  req
    "CreateConferenceProvider"
    "fixture/CreateConferenceProvider.yaml"

requestListSkills :: ListSkills -> TestTree
requestListSkills =
  req
    "ListSkills"
    "fixture/ListSkills.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestSearchDevices :: SearchDevices -> TestTree
requestSearchDevices =
  req
    "SearchDevices"
    "fixture/SearchDevices.yaml"

requestSearchRooms :: SearchRooms -> TestTree
requestSearchRooms =
  req
    "SearchRooms"
    "fixture/SearchRooms.yaml"

requestCreateSkillGroup :: CreateSkillGroup -> TestTree
requestCreateSkillGroup =
  req
    "CreateSkillGroup"
    "fixture/CreateSkillGroup.yaml"

requestDeleteProfile :: DeleteProfile -> TestTree
requestDeleteProfile =
  req
    "DeleteProfile"
    "fixture/DeleteProfile.yaml"

requestListSmartHomeAppliances :: ListSmartHomeAppliances -> TestTree
requestListSmartHomeAppliances =
  req
    "ListSmartHomeAppliances"
    "fixture/ListSmartHomeAppliances.yaml"

requestSearchAddressBooks :: SearchAddressBooks -> TestTree
requestSearchAddressBooks =
  req
    "SearchAddressBooks"
    "fixture/SearchAddressBooks.yaml"

requestUpdateProfile :: UpdateProfile -> TestTree
requestUpdateProfile =
  req
    "UpdateProfile"
    "fixture/UpdateProfile.yaml"

-- Responses

responseStartDeviceSync :: StartDeviceSyncResponse -> TestTree
responseStartDeviceSync =
  res
    "StartDeviceSyncResponse"
    "fixture/StartDeviceSyncResponse.proto"
    defaultService
    (Proxy :: Proxy StartDeviceSync)

responseCreateProfile :: CreateProfileResponse -> TestTree
responseCreateProfile =
  res
    "CreateProfileResponse"
    "fixture/CreateProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProfile)

responseDeleteBusinessReportSchedule :: DeleteBusinessReportScheduleResponse -> TestTree
responseDeleteBusinessReportSchedule =
  res
    "DeleteBusinessReportScheduleResponse"
    "fixture/DeleteBusinessReportScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBusinessReportSchedule)

responseListBusinessReportSchedules :: ListBusinessReportSchedulesResponse -> TestTree
responseListBusinessReportSchedules =
  res
    "ListBusinessReportSchedulesResponse"
    "fixture/ListBusinessReportSchedulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBusinessReportSchedules)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile =
  res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNetworkProfile)

responseDeleteDeviceUsageData :: DeleteDeviceUsageDataResponse -> TestTree
responseDeleteDeviceUsageData =
  res
    "DeleteDeviceUsageDataResponse"
    "fixture/DeleteDeviceUsageDataResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeviceUsageData)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile =
  res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkProfile)

responseCreateContact :: CreateContactResponse -> TestTree
responseCreateContact =
  res
    "CreateContactResponse"
    "fixture/CreateContactResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContact)

responseUpdateBusinessReportSchedule :: UpdateBusinessReportScheduleResponse -> TestTree
responseUpdateBusinessReportSchedule =
  res
    "UpdateBusinessReportScheduleResponse"
    "fixture/UpdateBusinessReportScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBusinessReportSchedule)

responseAssociateContactWithAddressBook :: AssociateContactWithAddressBookResponse -> TestTree
responseAssociateContactWithAddressBook =
  res
    "AssociateContactWithAddressBookResponse"
    "fixture/AssociateContactWithAddressBookResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateContactWithAddressBook)

responseListSkillsStoreCategories :: ListSkillsStoreCategoriesResponse -> TestTree
responseListSkillsStoreCategories =
  res
    "ListSkillsStoreCategoriesResponse"
    "fixture/ListSkillsStoreCategoriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSkillsStoreCategories)

responseGetConferenceProvider :: GetConferenceProviderResponse -> TestTree
responseGetConferenceProvider =
  res
    "GetConferenceProviderResponse"
    "fixture/GetConferenceProviderResponse.proto"
    defaultService
    (Proxy :: Proxy GetConferenceProvider)

responseGetAddressBook :: GetAddressBookResponse -> TestTree
responseGetAddressBook =
  res
    "GetAddressBookResponse"
    "fixture/GetAddressBookResponse.proto"
    defaultService
    (Proxy :: Proxy GetAddressBook)

responseCreateBusinessReportSchedule :: CreateBusinessReportScheduleResponse -> TestTree
responseCreateBusinessReportSchedule =
  res
    "CreateBusinessReportScheduleResponse"
    "fixture/CreateBusinessReportScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBusinessReportSchedule)

responseGetGatewayGroup :: GetGatewayGroupResponse -> TestTree
responseGetGatewayGroup =
  res
    "GetGatewayGroupResponse"
    "fixture/GetGatewayGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetGatewayGroup)

responseGetRoom :: GetRoomResponse -> TestTree
responseGetRoom =
  res
    "GetRoomResponse"
    "fixture/GetRoomResponse.proto"
    defaultService
    (Proxy :: Proxy GetRoom)

responseCreateNetworkProfile :: CreateNetworkProfileResponse -> TestTree
responseCreateNetworkProfile =
  res
    "CreateNetworkProfileResponse"
    "fixture/CreateNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkProfile)

responseSearchProfiles :: SearchProfilesResponse -> TestTree
responseSearchProfiles =
  res
    "SearchProfilesResponse"
    "fixture/SearchProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchProfiles)

responseDeleteAddressBook :: DeleteAddressBookResponse -> TestTree
responseDeleteAddressBook =
  res
    "DeleteAddressBookResponse"
    "fixture/DeleteAddressBookResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAddressBook)

responseUpdateAddressBook :: UpdateAddressBookResponse -> TestTree
responseUpdateAddressBook =
  res
    "UpdateAddressBookResponse"
    "fixture/UpdateAddressBookResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAddressBook)

responseDeleteRoomSkillParameter :: DeleteRoomSkillParameterResponse -> TestTree
responseDeleteRoomSkillParameter =
  res
    "DeleteRoomSkillParameterResponse"
    "fixture/DeleteRoomSkillParameterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRoomSkillParameter)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevice)

responseListSkillsStoreSkillsByCategory :: ListSkillsStoreSkillsByCategoryResponse -> TestTree
responseListSkillsStoreSkillsByCategory =
  res
    "ListSkillsStoreSkillsByCategoryResponse"
    "fixture/ListSkillsStoreSkillsByCategoryResponse.proto"
    defaultService
    (Proxy :: Proxy ListSkillsStoreSkillsByCategory)

responsePutSkillAuthorization :: PutSkillAuthorizationResponse -> TestTree
responsePutSkillAuthorization =
  res
    "PutSkillAuthorizationResponse"
    "fixture/PutSkillAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy PutSkillAuthorization)

responseDisassociateContactFromAddressBook :: DisassociateContactFromAddressBookResponse -> TestTree
responseDisassociateContactFromAddressBook =
  res
    "DisassociateContactFromAddressBookResponse"
    "fixture/DisassociateContactFromAddressBookResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateContactFromAddressBook)

responseAssociateDeviceWithNetworkProfile :: AssociateDeviceWithNetworkProfileResponse -> TestTree
responseAssociateDeviceWithNetworkProfile =
  res
    "AssociateDeviceWithNetworkProfileResponse"
    "fixture/AssociateDeviceWithNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDeviceWithNetworkProfile)

responseSearchSkillGroups :: SearchSkillGroupsResponse -> TestTree
responseSearchSkillGroups =
  res
    "SearchSkillGroupsResponse"
    "fixture/SearchSkillGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchSkillGroups)

responseResolveRoom :: ResolveRoomResponse -> TestTree
responseResolveRoom =
  res
    "ResolveRoomResponse"
    "fixture/ResolveRoomResponse.proto"
    defaultService
    (Proxy :: Proxy ResolveRoom)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseSearchNetworkProfiles :: SearchNetworkProfilesResponse -> TestTree
responseSearchNetworkProfiles =
  res
    "SearchNetworkProfilesResponse"
    "fixture/SearchNetworkProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchNetworkProfiles)

responsePutInvitationConfiguration :: PutInvitationConfigurationResponse -> TestTree
responsePutInvitationConfiguration =
  res
    "PutInvitationConfigurationResponse"
    "fixture/PutInvitationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutInvitationConfiguration)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetSkillGroup :: GetSkillGroupResponse -> TestTree
responseGetSkillGroup =
  res
    "GetSkillGroupResponse"
    "fixture/GetSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetSkillGroup)

responseSendInvitation :: SendInvitationResponse -> TestTree
responseSendInvitation =
  res
    "SendInvitationResponse"
    "fixture/SendInvitationResponse.proto"
    defaultService
    (Proxy :: Proxy SendInvitation)

responseListDeviceEvents :: ListDeviceEventsResponse -> TestTree
responseListDeviceEvents =
  res
    "ListDeviceEventsResponse"
    "fixture/ListDeviceEventsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeviceEvents)

responseRegisterAVSDevice :: RegisterAVSDeviceResponse -> TestTree
responseRegisterAVSDevice =
  res
    "RegisterAVSDeviceResponse"
    "fixture/RegisterAVSDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterAVSDevice)

responseGetProfile :: GetProfileResponse -> TestTree
responseGetProfile =
  res
    "GetProfileResponse"
    "fixture/GetProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetProfile)

responseSearchContacts :: SearchContactsResponse -> TestTree
responseSearchContacts =
  res
    "SearchContactsResponse"
    "fixture/SearchContactsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchContacts)

responseDisassociateSkillGroupFromRoom :: DisassociateSkillGroupFromRoomResponse -> TestTree
responseDisassociateSkillGroupFromRoom =
  res
    "DisassociateSkillGroupFromRoomResponse"
    "fixture/DisassociateSkillGroupFromRoomResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateSkillGroupFromRoom)

responseAssociateSkillWithUsers :: AssociateSkillWithUsersResponse -> TestTree
responseAssociateSkillWithUsers =
  res
    "AssociateSkillWithUsersResponse"
    "fixture/AssociateSkillWithUsersResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSkillWithUsers)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseForgetSmartHomeAppliances :: ForgetSmartHomeAppliancesResponse -> TestTree
responseForgetSmartHomeAppliances =
  res
    "ForgetSmartHomeAppliancesResponse"
    "fixture/ForgetSmartHomeAppliancesResponse.proto"
    defaultService
    (Proxy :: Proxy ForgetSmartHomeAppliances)

responseSendAnnouncement :: SendAnnouncementResponse -> TestTree
responseSendAnnouncement =
  res
    "SendAnnouncementResponse"
    "fixture/SendAnnouncementResponse.proto"
    defaultService
    (Proxy :: Proxy SendAnnouncement)

responseAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoomResponse -> TestTree
responseAssociateSkillGroupWithRoom =
  res
    "AssociateSkillGroupWithRoomResponse"
    "fixture/AssociateSkillGroupWithRoomResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSkillGroupWithRoom)

responseGetInvitationConfiguration :: GetInvitationConfigurationResponse -> TestTree
responseGetInvitationConfiguration =
  res
    "GetInvitationConfigurationResponse"
    "fixture/GetInvitationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetInvitationConfiguration)

responseDisassociateSkillFromUsers :: DisassociateSkillFromUsersResponse -> TestTree
responseDisassociateSkillFromUsers =
  res
    "DisassociateSkillFromUsersResponse"
    "fixture/DisassociateSkillFromUsersResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateSkillFromUsers)

responseUpdateSkillGroup :: UpdateSkillGroupResponse -> TestTree
responseUpdateSkillGroup =
  res
    "UpdateSkillGroupResponse"
    "fixture/UpdateSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSkillGroup)

responseDeleteSkillGroup :: DeleteSkillGroupResponse -> TestTree
responseDeleteSkillGroup =
  res
    "DeleteSkillGroupResponse"
    "fixture/DeleteSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSkillGroup)

responseSearchUsers :: SearchUsersResponse -> TestTree
responseSearchUsers =
  res
    "SearchUsersResponse"
    "fixture/SearchUsersResponse.proto"
    defaultService
    (Proxy :: Proxy SearchUsers)

responsePutConferencePreference :: PutConferencePreferenceResponse -> TestTree
responsePutConferencePreference =
  res
    "PutConferencePreferenceResponse"
    "fixture/PutConferencePreferenceResponse.proto"
    defaultService
    (Proxy :: Proxy PutConferencePreference)

responseUpdateContact :: UpdateContactResponse -> TestTree
responseUpdateContact =
  res
    "UpdateContactResponse"
    "fixture/UpdateContactResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContact)

responseStartSmartHomeApplianceDiscovery :: StartSmartHomeApplianceDiscoveryResponse -> TestTree
responseStartSmartHomeApplianceDiscovery =
  res
    "StartSmartHomeApplianceDiscoveryResponse"
    "fixture/StartSmartHomeApplianceDiscoveryResponse.proto"
    defaultService
    (Proxy :: Proxy StartSmartHomeApplianceDiscovery)

responseDeleteContact :: DeleteContactResponse -> TestTree
responseDeleteContact =
  res
    "DeleteContactResponse"
    "fixture/DeleteContactResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContact)

responseRevokeInvitation :: RevokeInvitationResponse -> TestTree
responseRevokeInvitation =
  res
    "RevokeInvitationResponse"
    "fixture/RevokeInvitationResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeInvitation)

responseAssociateSkillWithSkillGroup :: AssociateSkillWithSkillGroupResponse -> TestTree
responseAssociateSkillWithSkillGroup =
  res
    "AssociateSkillWithSkillGroupResponse"
    "fixture/AssociateSkillWithSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSkillWithSkillGroup)

responseUpdateDevice :: UpdateDeviceResponse -> TestTree
responseUpdateDevice =
  res
    "UpdateDeviceResponse"
    "fixture/UpdateDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDevice)

responseGetRoomSkillParameter :: GetRoomSkillParameterResponse -> TestTree
responseGetRoomSkillParameter =
  res
    "GetRoomSkillParameterResponse"
    "fixture/GetRoomSkillParameterResponse.proto"
    defaultService
    (Proxy :: Proxy GetRoomSkillParameter)

responseUpdateGateway :: UpdateGatewayResponse -> TestTree
responseUpdateGateway =
  res
    "UpdateGatewayResponse"
    "fixture/UpdateGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGateway)

responseAssociateDeviceWithRoom :: AssociateDeviceWithRoomResponse -> TestTree
responseAssociateDeviceWithRoom =
  res
    "AssociateDeviceWithRoomResponse"
    "fixture/AssociateDeviceWithRoomResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDeviceWithRoom)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy ListGateways)

responseDeleteDevice :: DeleteDeviceResponse -> TestTree
responseDeleteDevice =
  res
    "DeleteDeviceResponse"
    "fixture/DeleteDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDevice)

responseUpdateRoom :: UpdateRoomResponse -> TestTree
responseUpdateRoom =
  res
    "UpdateRoomResponse"
    "fixture/UpdateRoomResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoom)

responseListGatewayGroups :: ListGatewayGroupsResponse -> TestTree
responseListGatewayGroups =
  res
    "ListGatewayGroupsResponse"
    "fixture/ListGatewayGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGatewayGroups)

responseGetContact :: GetContactResponse -> TestTree
responseGetContact =
  res
    "GetContactResponse"
    "fixture/GetContactResponse.proto"
    defaultService
    (Proxy :: Proxy GetContact)

responseRejectSkill :: RejectSkillResponse -> TestTree
responseRejectSkill =
  res
    "RejectSkillResponse"
    "fixture/RejectSkillResponse.proto"
    defaultService
    (Proxy :: Proxy RejectSkill)

responseListConferenceProviders :: ListConferenceProvidersResponse -> TestTree
responseListConferenceProviders =
  res
    "ListConferenceProvidersResponse"
    "fixture/ListConferenceProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy ListConferenceProviders)

responseUpdateConferenceProvider :: UpdateConferenceProviderResponse -> TestTree
responseUpdateConferenceProvider =
  res
    "UpdateConferenceProviderResponse"
    "fixture/UpdateConferenceProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConferenceProvider)

responseDeleteRoom :: DeleteRoomResponse -> TestTree
responseDeleteRoom =
  res
    "DeleteRoomResponse"
    "fixture/DeleteRoomResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRoom)

responseApproveSkill :: ApproveSkillResponse -> TestTree
responseApproveSkill =
  res
    "ApproveSkillResponse"
    "fixture/ApproveSkillResponse.proto"
    defaultService
    (Proxy :: Proxy ApproveSkill)

responseDeleteGatewayGroup :: DeleteGatewayGroupResponse -> TestTree
responseDeleteGatewayGroup =
  res
    "DeleteGatewayGroupResponse"
    "fixture/DeleteGatewayGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGatewayGroup)

responseDeleteConferenceProvider :: DeleteConferenceProviderResponse -> TestTree
responseDeleteConferenceProvider =
  res
    "DeleteConferenceProviderResponse"
    "fixture/DeleteConferenceProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConferenceProvider)

responseGetGateway :: GetGatewayResponse -> TestTree
responseGetGateway =
  res
    "GetGatewayResponse"
    "fixture/GetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy GetGateway)

responsePutRoomSkillParameter :: PutRoomSkillParameterResponse -> TestTree
responsePutRoomSkillParameter =
  res
    "PutRoomSkillParameterResponse"
    "fixture/PutRoomSkillParameterResponse.proto"
    defaultService
    (Proxy :: Proxy PutRoomSkillParameter)

responseUpdateGatewayGroup :: UpdateGatewayGroupResponse -> TestTree
responseUpdateGatewayGroup =
  res
    "UpdateGatewayGroupResponse"
    "fixture/UpdateGatewayGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGatewayGroup)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseDeleteSkillAuthorization :: DeleteSkillAuthorizationResponse -> TestTree
responseDeleteSkillAuthorization =
  res
    "DeleteSkillAuthorizationResponse"
    "fixture/DeleteSkillAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSkillAuthorization)

responseCreateGatewayGroup :: CreateGatewayGroupResponse -> TestTree
responseCreateGatewayGroup =
  res
    "CreateGatewayGroupResponse"
    "fixture/CreateGatewayGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGatewayGroup)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile =
  res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetNetworkProfile)

responseDisassociateDeviceFromRoom :: DisassociateDeviceFromRoomResponse -> TestTree
responseDisassociateDeviceFromRoom =
  res
    "DisassociateDeviceFromRoomResponse"
    "fixture/DisassociateDeviceFromRoomResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateDeviceFromRoom)

responseGetConferencePreference :: GetConferencePreferenceResponse -> TestTree
responseGetConferencePreference =
  res
    "GetConferencePreferenceResponse"
    "fixture/GetConferencePreferenceResponse.proto"
    defaultService
    (Proxy :: Proxy GetConferencePreference)

responseCreateRoom :: CreateRoomResponse -> TestTree
responseCreateRoom =
  res
    "CreateRoomResponse"
    "fixture/CreateRoomResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRoom)

responseCreateAddressBook :: CreateAddressBookResponse -> TestTree
responseCreateAddressBook =
  res
    "CreateAddressBookResponse"
    "fixture/CreateAddressBookResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAddressBook)

responseDisassociateSkillFromSkillGroup :: DisassociateSkillFromSkillGroupResponse -> TestTree
responseDisassociateSkillFromSkillGroup =
  res
    "DisassociateSkillFromSkillGroupResponse"
    "fixture/DisassociateSkillFromSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateSkillFromSkillGroup)

responseCreateConferenceProvider :: CreateConferenceProviderResponse -> TestTree
responseCreateConferenceProvider =
  res
    "CreateConferenceProviderResponse"
    "fixture/CreateConferenceProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConferenceProvider)

responseListSkills :: ListSkillsResponse -> TestTree
responseListSkills =
  res
    "ListSkillsResponse"
    "fixture/ListSkillsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSkills)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseSearchDevices :: SearchDevicesResponse -> TestTree
responseSearchDevices =
  res
    "SearchDevicesResponse"
    "fixture/SearchDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchDevices)

responseSearchRooms :: SearchRoomsResponse -> TestTree
responseSearchRooms =
  res
    "SearchRoomsResponse"
    "fixture/SearchRoomsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchRooms)

responseCreateSkillGroup :: CreateSkillGroupResponse -> TestTree
responseCreateSkillGroup =
  res
    "CreateSkillGroupResponse"
    "fixture/CreateSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSkillGroup)

responseDeleteProfile :: DeleteProfileResponse -> TestTree
responseDeleteProfile =
  res
    "DeleteProfileResponse"
    "fixture/DeleteProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProfile)

responseListSmartHomeAppliances :: ListSmartHomeAppliancesResponse -> TestTree
responseListSmartHomeAppliances =
  res
    "ListSmartHomeAppliancesResponse"
    "fixture/ListSmartHomeAppliancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSmartHomeAppliances)

responseSearchAddressBooks :: SearchAddressBooksResponse -> TestTree
responseSearchAddressBooks =
  res
    "SearchAddressBooksResponse"
    "fixture/SearchAddressBooksResponse.proto"
    defaultService
    (Proxy :: Proxy SearchAddressBooks)

responseUpdateProfile :: UpdateProfileResponse -> TestTree
responseUpdateProfile =
  res
    "UpdateProfileResponse"
    "fixture/UpdateProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProfile)
