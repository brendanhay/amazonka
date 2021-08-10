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
--         , requestCreateContact $
--             newCreateContact
--
--         , requestDeleteBusinessReportSchedule $
--             newDeleteBusinessReportSchedule
--
--         , requestUpdateNetworkProfile $
--             newUpdateNetworkProfile
--
--         , requestListBusinessReportSchedules $
--             newListBusinessReportSchedules
--
--         , requestDeleteNetworkProfile $
--             newDeleteNetworkProfile
--
--         , requestUpdateBusinessReportSchedule $
--             newUpdateBusinessReportSchedule
--
--         , requestDeleteDeviceUsageData $
--             newDeleteDeviceUsageData
--
--         , requestGetConferenceProvider $
--             newGetConferenceProvider
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
--         , requestListSkillsStoreCategories $
--             newListSkillsStoreCategories
--
--         , requestCreateBusinessReportSchedule $
--             newCreateBusinessReportSchedule
--
--         , requestGetAddressBook $
--             newGetAddressBook
--
--         , requestAssociateContactWithAddressBook $
--             newAssociateContactWithAddressBook
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestDeleteRoomSkillParameter $
--             newDeleteRoomSkillParameter
--
--         , requestListSkillsStoreSkillsByCategory $
--             newListSkillsStoreSkillsByCategory
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
--         , requestSearchSkillGroups $
--             newSearchSkillGroups
--
--         , requestResolveRoom $
--             newResolveRoom
--
--         , requestPutSkillAuthorization $
--             newPutSkillAuthorization
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDisassociateContactFromAddressBook $
--             newDisassociateContactFromAddressBook
--
--         , requestAssociateDeviceWithNetworkProfile $
--             newAssociateDeviceWithNetworkProfile
--
--         , requestSearchNetworkProfiles $
--             newSearchNetworkProfiles
--
--         , requestGetSkillGroup $
--             newGetSkillGroup
--
--         , requestPutInvitationConfiguration $
--             newPutInvitationConfiguration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListDeviceEvents $
--             newListDeviceEvents
--
--         , requestSendAnnouncement $
--             newSendAnnouncement
--
--         , requestDisassociateSkillGroupFromRoom $
--             newDisassociateSkillGroupFromRoom
--
--         , requestGetProfile $
--             newGetProfile
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestSearchContacts $
--             newSearchContacts
--
--         , requestRegisterAVSDevice $
--             newRegisterAVSDevice
--
--         , requestSendInvitation $
--             newSendInvitation
--
--         , requestForgetSmartHomeAppliances $
--             newForgetSmartHomeAppliances
--
--         , requestAssociateSkillWithUsers $
--             newAssociateSkillWithUsers
--
--         , requestGetInvitationConfiguration $
--             newGetInvitationConfiguration
--
--         , requestDisassociateSkillFromUsers $
--             newDisassociateSkillFromUsers
--
--         , requestDeleteSkillGroup $
--             newDeleteSkillGroup
--
--         , requestUpdateSkillGroup $
--             newUpdateSkillGroup
--
--         , requestAssociateSkillGroupWithRoom $
--             newAssociateSkillGroupWithRoom
--
--         , requestSearchUsers $
--             newSearchUsers
--
--         , requestPutConferencePreference $
--             newPutConferencePreference
--
--         , requestUpdateGateway $
--             newUpdateGateway
--
--         , requestDeleteDevice $
--             newDeleteDevice
--
--         , requestRevokeInvitation $
--             newRevokeInvitation
--
--         , requestGetRoomSkillParameter $
--             newGetRoomSkillParameter
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
--         , requestUpdateDevice $
--             newUpdateDevice
--
--         , requestAssociateDeviceWithRoom $
--             newAssociateDeviceWithRoom
--
--         , requestAssociateSkillWithSkillGroup $
--             newAssociateSkillWithSkillGroup
--
--         , requestListGateways $
--             newListGateways
--
--         , requestDeleteRoom $
--             newDeleteRoom
--
--         , requestListConferenceProviders $
--             newListConferenceProviders
--
--         , requestDeleteGatewayGroup $
--             newDeleteGatewayGroup
--
--         , requestUpdateRoom $
--             newUpdateRoom
--
--         , requestDeleteConferenceProvider $
--             newDeleteConferenceProvider
--
--         , requestGetGateway $
--             newGetGateway
--
--         , requestUpdateConferenceProvider $
--             newUpdateConferenceProvider
--
--         , requestUpdateGatewayGroup $
--             newUpdateGatewayGroup
--
--         , requestListGatewayGroups $
--             newListGatewayGroups
--
--         , requestApproveSkill $
--             newApproveSkill
--
--         , requestGetContact $
--             newGetContact
--
--         , requestRejectSkill $
--             newRejectSkill
--
--         , requestPutRoomSkillParameter $
--             newPutRoomSkillParameter
--
--         , requestDisassociateDeviceFromRoom $
--             newDisassociateDeviceFromRoom
--
--         , requestCreateAddressBook $
--             newCreateAddressBook
--
--         , requestCreateRoom $
--             newCreateRoom
--
--         , requestCreateConferenceProvider $
--             newCreateConferenceProvider
--
--         , requestGetNetworkProfile $
--             newGetNetworkProfile
--
--         , requestGetConferencePreference $
--             newGetConferencePreference
--
--         , requestDeleteSkillAuthorization $
--             newDeleteSkillAuthorization
--
--         , requestCreateGatewayGroup $
--             newCreateGatewayGroup
--
--         , requestListTags $
--             newListTags
--
--         , requestDisassociateSkillFromSkillGroup $
--             newDisassociateSkillFromSkillGroup
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestListSkills $
--             newListSkills
--
--         , requestSearchDevices $
--             newSearchDevices
--
--         , requestSearchRooms $
--             newSearchRooms
--
--         , requestSearchAddressBooks $
--             newSearchAddressBooks
--
--         , requestListSmartHomeAppliances $
--             newListSmartHomeAppliances
--
--         , requestDeleteProfile $
--             newDeleteProfile
--
--         , requestCreateSkillGroup $
--             newCreateSkillGroup
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
--         , responseCreateContact $
--             newCreateContactResponse
--
--         , responseDeleteBusinessReportSchedule $
--             newDeleteBusinessReportScheduleResponse
--
--         , responseUpdateNetworkProfile $
--             newUpdateNetworkProfileResponse
--
--         , responseListBusinessReportSchedules $
--             newListBusinessReportSchedulesResponse
--
--         , responseDeleteNetworkProfile $
--             newDeleteNetworkProfileResponse
--
--         , responseUpdateBusinessReportSchedule $
--             newUpdateBusinessReportScheduleResponse
--
--         , responseDeleteDeviceUsageData $
--             newDeleteDeviceUsageDataResponse
--
--         , responseGetConferenceProvider $
--             newGetConferenceProviderResponse
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
--         , responseListSkillsStoreCategories $
--             newListSkillsStoreCategoriesResponse
--
--         , responseCreateBusinessReportSchedule $
--             newCreateBusinessReportScheduleResponse
--
--         , responseGetAddressBook $
--             newGetAddressBookResponse
--
--         , responseAssociateContactWithAddressBook $
--             newAssociateContactWithAddressBookResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseDeleteRoomSkillParameter $
--             newDeleteRoomSkillParameterResponse
--
--         , responseListSkillsStoreSkillsByCategory $
--             newListSkillsStoreSkillsByCategoryResponse
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
--         , responseSearchSkillGroups $
--             newSearchSkillGroupsResponse
--
--         , responseResolveRoom $
--             newResolveRoomResponse
--
--         , responsePutSkillAuthorization $
--             newPutSkillAuthorizationResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDisassociateContactFromAddressBook $
--             newDisassociateContactFromAddressBookResponse
--
--         , responseAssociateDeviceWithNetworkProfile $
--             newAssociateDeviceWithNetworkProfileResponse
--
--         , responseSearchNetworkProfiles $
--             newSearchNetworkProfilesResponse
--
--         , responseGetSkillGroup $
--             newGetSkillGroupResponse
--
--         , responsePutInvitationConfiguration $
--             newPutInvitationConfigurationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListDeviceEvents $
--             newListDeviceEventsResponse
--
--         , responseSendAnnouncement $
--             newSendAnnouncementResponse
--
--         , responseDisassociateSkillGroupFromRoom $
--             newDisassociateSkillGroupFromRoomResponse
--
--         , responseGetProfile $
--             newGetProfileResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseSearchContacts $
--             newSearchContactsResponse
--
--         , responseRegisterAVSDevice $
--             newRegisterAVSDeviceResponse
--
--         , responseSendInvitation $
--             newSendInvitationResponse
--
--         , responseForgetSmartHomeAppliances $
--             newForgetSmartHomeAppliancesResponse
--
--         , responseAssociateSkillWithUsers $
--             newAssociateSkillWithUsersResponse
--
--         , responseGetInvitationConfiguration $
--             newGetInvitationConfigurationResponse
--
--         , responseDisassociateSkillFromUsers $
--             newDisassociateSkillFromUsersResponse
--
--         , responseDeleteSkillGroup $
--             newDeleteSkillGroupResponse
--
--         , responseUpdateSkillGroup $
--             newUpdateSkillGroupResponse
--
--         , responseAssociateSkillGroupWithRoom $
--             newAssociateSkillGroupWithRoomResponse
--
--         , responseSearchUsers $
--             newSearchUsersResponse
--
--         , responsePutConferencePreference $
--             newPutConferencePreferenceResponse
--
--         , responseUpdateGateway $
--             newUpdateGatewayResponse
--
--         , responseDeleteDevice $
--             newDeleteDeviceResponse
--
--         , responseRevokeInvitation $
--             newRevokeInvitationResponse
--
--         , responseGetRoomSkillParameter $
--             newGetRoomSkillParameterResponse
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
--         , responseUpdateDevice $
--             newUpdateDeviceResponse
--
--         , responseAssociateDeviceWithRoom $
--             newAssociateDeviceWithRoomResponse
--
--         , responseAssociateSkillWithSkillGroup $
--             newAssociateSkillWithSkillGroupResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseDeleteRoom $
--             newDeleteRoomResponse
--
--         , responseListConferenceProviders $
--             newListConferenceProvidersResponse
--
--         , responseDeleteGatewayGroup $
--             newDeleteGatewayGroupResponse
--
--         , responseUpdateRoom $
--             newUpdateRoomResponse
--
--         , responseDeleteConferenceProvider $
--             newDeleteConferenceProviderResponse
--
--         , responseGetGateway $
--             newGetGatewayResponse
--
--         , responseUpdateConferenceProvider $
--             newUpdateConferenceProviderResponse
--
--         , responseUpdateGatewayGroup $
--             newUpdateGatewayGroupResponse
--
--         , responseListGatewayGroups $
--             newListGatewayGroupsResponse
--
--         , responseApproveSkill $
--             newApproveSkillResponse
--
--         , responseGetContact $
--             newGetContactResponse
--
--         , responseRejectSkill $
--             newRejectSkillResponse
--
--         , responsePutRoomSkillParameter $
--             newPutRoomSkillParameterResponse
--
--         , responseDisassociateDeviceFromRoom $
--             newDisassociateDeviceFromRoomResponse
--
--         , responseCreateAddressBook $
--             newCreateAddressBookResponse
--
--         , responseCreateRoom $
--             newCreateRoomResponse
--
--         , responseCreateConferenceProvider $
--             newCreateConferenceProviderResponse
--
--         , responseGetNetworkProfile $
--             newGetNetworkProfileResponse
--
--         , responseGetConferencePreference $
--             newGetConferencePreferenceResponse
--
--         , responseDeleteSkillAuthorization $
--             newDeleteSkillAuthorizationResponse
--
--         , responseCreateGatewayGroup $
--             newCreateGatewayGroupResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDisassociateSkillFromSkillGroup $
--             newDisassociateSkillFromSkillGroupResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseListSkills $
--             newListSkillsResponse
--
--         , responseSearchDevices $
--             newSearchDevicesResponse
--
--         , responseSearchRooms $
--             newSearchRoomsResponse
--
--         , responseSearchAddressBooks $
--             newSearchAddressBooksResponse
--
--         , responseListSmartHomeAppliances $
--             newListSmartHomeAppliancesResponse
--
--         , responseDeleteProfile $
--             newDeleteProfileResponse
--
--         , responseCreateSkillGroup $
--             newCreateSkillGroupResponse
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

requestCreateContact :: CreateContact -> TestTree
requestCreateContact =
  req
    "CreateContact"
    "fixture/CreateContact.yaml"

requestDeleteBusinessReportSchedule :: DeleteBusinessReportSchedule -> TestTree
requestDeleteBusinessReportSchedule =
  req
    "DeleteBusinessReportSchedule"
    "fixture/DeleteBusinessReportSchedule.yaml"

requestUpdateNetworkProfile :: UpdateNetworkProfile -> TestTree
requestUpdateNetworkProfile =
  req
    "UpdateNetworkProfile"
    "fixture/UpdateNetworkProfile.yaml"

requestListBusinessReportSchedules :: ListBusinessReportSchedules -> TestTree
requestListBusinessReportSchedules =
  req
    "ListBusinessReportSchedules"
    "fixture/ListBusinessReportSchedules.yaml"

requestDeleteNetworkProfile :: DeleteNetworkProfile -> TestTree
requestDeleteNetworkProfile =
  req
    "DeleteNetworkProfile"
    "fixture/DeleteNetworkProfile.yaml"

requestUpdateBusinessReportSchedule :: UpdateBusinessReportSchedule -> TestTree
requestUpdateBusinessReportSchedule =
  req
    "UpdateBusinessReportSchedule"
    "fixture/UpdateBusinessReportSchedule.yaml"

requestDeleteDeviceUsageData :: DeleteDeviceUsageData -> TestTree
requestDeleteDeviceUsageData =
  req
    "DeleteDeviceUsageData"
    "fixture/DeleteDeviceUsageData.yaml"

requestGetConferenceProvider :: GetConferenceProvider -> TestTree
requestGetConferenceProvider =
  req
    "GetConferenceProvider"
    "fixture/GetConferenceProvider.yaml"

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

requestListSkillsStoreCategories :: ListSkillsStoreCategories -> TestTree
requestListSkillsStoreCategories =
  req
    "ListSkillsStoreCategories"
    "fixture/ListSkillsStoreCategories.yaml"

requestCreateBusinessReportSchedule :: CreateBusinessReportSchedule -> TestTree
requestCreateBusinessReportSchedule =
  req
    "CreateBusinessReportSchedule"
    "fixture/CreateBusinessReportSchedule.yaml"

requestGetAddressBook :: GetAddressBook -> TestTree
requestGetAddressBook =
  req
    "GetAddressBook"
    "fixture/GetAddressBook.yaml"

requestAssociateContactWithAddressBook :: AssociateContactWithAddressBook -> TestTree
requestAssociateContactWithAddressBook =
  req
    "AssociateContactWithAddressBook"
    "fixture/AssociateContactWithAddressBook.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice =
  req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestDeleteRoomSkillParameter :: DeleteRoomSkillParameter -> TestTree
requestDeleteRoomSkillParameter =
  req
    "DeleteRoomSkillParameter"
    "fixture/DeleteRoomSkillParameter.yaml"

requestListSkillsStoreSkillsByCategory :: ListSkillsStoreSkillsByCategory -> TestTree
requestListSkillsStoreSkillsByCategory =
  req
    "ListSkillsStoreSkillsByCategory"
    "fixture/ListSkillsStoreSkillsByCategory.yaml"

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

requestPutSkillAuthorization :: PutSkillAuthorization -> TestTree
requestPutSkillAuthorization =
  req
    "PutSkillAuthorization"
    "fixture/PutSkillAuthorization.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestSearchNetworkProfiles :: SearchNetworkProfiles -> TestTree
requestSearchNetworkProfiles =
  req
    "SearchNetworkProfiles"
    "fixture/SearchNetworkProfiles.yaml"

requestGetSkillGroup :: GetSkillGroup -> TestTree
requestGetSkillGroup =
  req
    "GetSkillGroup"
    "fixture/GetSkillGroup.yaml"

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

requestListDeviceEvents :: ListDeviceEvents -> TestTree
requestListDeviceEvents =
  req
    "ListDeviceEvents"
    "fixture/ListDeviceEvents.yaml"

requestSendAnnouncement :: SendAnnouncement -> TestTree
requestSendAnnouncement =
  req
    "SendAnnouncement"
    "fixture/SendAnnouncement.yaml"

requestDisassociateSkillGroupFromRoom :: DisassociateSkillGroupFromRoom -> TestTree
requestDisassociateSkillGroupFromRoom =
  req
    "DisassociateSkillGroupFromRoom"
    "fixture/DisassociateSkillGroupFromRoom.yaml"

requestGetProfile :: GetProfile -> TestTree
requestGetProfile =
  req
    "GetProfile"
    "fixture/GetProfile.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestSearchContacts :: SearchContacts -> TestTree
requestSearchContacts =
  req
    "SearchContacts"
    "fixture/SearchContacts.yaml"

requestRegisterAVSDevice :: RegisterAVSDevice -> TestTree
requestRegisterAVSDevice =
  req
    "RegisterAVSDevice"
    "fixture/RegisterAVSDevice.yaml"

requestSendInvitation :: SendInvitation -> TestTree
requestSendInvitation =
  req
    "SendInvitation"
    "fixture/SendInvitation.yaml"

requestForgetSmartHomeAppliances :: ForgetSmartHomeAppliances -> TestTree
requestForgetSmartHomeAppliances =
  req
    "ForgetSmartHomeAppliances"
    "fixture/ForgetSmartHomeAppliances.yaml"

requestAssociateSkillWithUsers :: AssociateSkillWithUsers -> TestTree
requestAssociateSkillWithUsers =
  req
    "AssociateSkillWithUsers"
    "fixture/AssociateSkillWithUsers.yaml"

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

requestDeleteSkillGroup :: DeleteSkillGroup -> TestTree
requestDeleteSkillGroup =
  req
    "DeleteSkillGroup"
    "fixture/DeleteSkillGroup.yaml"

requestUpdateSkillGroup :: UpdateSkillGroup -> TestTree
requestUpdateSkillGroup =
  req
    "UpdateSkillGroup"
    "fixture/UpdateSkillGroup.yaml"

requestAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoom -> TestTree
requestAssociateSkillGroupWithRoom =
  req
    "AssociateSkillGroupWithRoom"
    "fixture/AssociateSkillGroupWithRoom.yaml"

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

requestUpdateGateway :: UpdateGateway -> TestTree
requestUpdateGateway =
  req
    "UpdateGateway"
    "fixture/UpdateGateway.yaml"

requestDeleteDevice :: DeleteDevice -> TestTree
requestDeleteDevice =
  req
    "DeleteDevice"
    "fixture/DeleteDevice.yaml"

requestRevokeInvitation :: RevokeInvitation -> TestTree
requestRevokeInvitation =
  req
    "RevokeInvitation"
    "fixture/RevokeInvitation.yaml"

requestGetRoomSkillParameter :: GetRoomSkillParameter -> TestTree
requestGetRoomSkillParameter =
  req
    "GetRoomSkillParameter"
    "fixture/GetRoomSkillParameter.yaml"

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

requestUpdateDevice :: UpdateDevice -> TestTree
requestUpdateDevice =
  req
    "UpdateDevice"
    "fixture/UpdateDevice.yaml"

requestAssociateDeviceWithRoom :: AssociateDeviceWithRoom -> TestTree
requestAssociateDeviceWithRoom =
  req
    "AssociateDeviceWithRoom"
    "fixture/AssociateDeviceWithRoom.yaml"

requestAssociateSkillWithSkillGroup :: AssociateSkillWithSkillGroup -> TestTree
requestAssociateSkillWithSkillGroup =
  req
    "AssociateSkillWithSkillGroup"
    "fixture/AssociateSkillWithSkillGroup.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways =
  req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestDeleteRoom :: DeleteRoom -> TestTree
requestDeleteRoom =
  req
    "DeleteRoom"
    "fixture/DeleteRoom.yaml"

requestListConferenceProviders :: ListConferenceProviders -> TestTree
requestListConferenceProviders =
  req
    "ListConferenceProviders"
    "fixture/ListConferenceProviders.yaml"

requestDeleteGatewayGroup :: DeleteGatewayGroup -> TestTree
requestDeleteGatewayGroup =
  req
    "DeleteGatewayGroup"
    "fixture/DeleteGatewayGroup.yaml"

requestUpdateRoom :: UpdateRoom -> TestTree
requestUpdateRoom =
  req
    "UpdateRoom"
    "fixture/UpdateRoom.yaml"

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

requestUpdateConferenceProvider :: UpdateConferenceProvider -> TestTree
requestUpdateConferenceProvider =
  req
    "UpdateConferenceProvider"
    "fixture/UpdateConferenceProvider.yaml"

requestUpdateGatewayGroup :: UpdateGatewayGroup -> TestTree
requestUpdateGatewayGroup =
  req
    "UpdateGatewayGroup"
    "fixture/UpdateGatewayGroup.yaml"

requestListGatewayGroups :: ListGatewayGroups -> TestTree
requestListGatewayGroups =
  req
    "ListGatewayGroups"
    "fixture/ListGatewayGroups.yaml"

requestApproveSkill :: ApproveSkill -> TestTree
requestApproveSkill =
  req
    "ApproveSkill"
    "fixture/ApproveSkill.yaml"

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

requestPutRoomSkillParameter :: PutRoomSkillParameter -> TestTree
requestPutRoomSkillParameter =
  req
    "PutRoomSkillParameter"
    "fixture/PutRoomSkillParameter.yaml"

requestDisassociateDeviceFromRoom :: DisassociateDeviceFromRoom -> TestTree
requestDisassociateDeviceFromRoom =
  req
    "DisassociateDeviceFromRoom"
    "fixture/DisassociateDeviceFromRoom.yaml"

requestCreateAddressBook :: CreateAddressBook -> TestTree
requestCreateAddressBook =
  req
    "CreateAddressBook"
    "fixture/CreateAddressBook.yaml"

requestCreateRoom :: CreateRoom -> TestTree
requestCreateRoom =
  req
    "CreateRoom"
    "fixture/CreateRoom.yaml"

requestCreateConferenceProvider :: CreateConferenceProvider -> TestTree
requestCreateConferenceProvider =
  req
    "CreateConferenceProvider"
    "fixture/CreateConferenceProvider.yaml"

requestGetNetworkProfile :: GetNetworkProfile -> TestTree
requestGetNetworkProfile =
  req
    "GetNetworkProfile"
    "fixture/GetNetworkProfile.yaml"

requestGetConferencePreference :: GetConferencePreference -> TestTree
requestGetConferencePreference =
  req
    "GetConferencePreference"
    "fixture/GetConferencePreference.yaml"

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

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDisassociateSkillFromSkillGroup :: DisassociateSkillFromSkillGroup -> TestTree
requestDisassociateSkillFromSkillGroup =
  req
    "DisassociateSkillFromSkillGroup"
    "fixture/DisassociateSkillFromSkillGroup.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestListSkills :: ListSkills -> TestTree
requestListSkills =
  req
    "ListSkills"
    "fixture/ListSkills.yaml"

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

requestSearchAddressBooks :: SearchAddressBooks -> TestTree
requestSearchAddressBooks =
  req
    "SearchAddressBooks"
    "fixture/SearchAddressBooks.yaml"

requestListSmartHomeAppliances :: ListSmartHomeAppliances -> TestTree
requestListSmartHomeAppliances =
  req
    "ListSmartHomeAppliances"
    "fixture/ListSmartHomeAppliances.yaml"

requestDeleteProfile :: DeleteProfile -> TestTree
requestDeleteProfile =
  req
    "DeleteProfile"
    "fixture/DeleteProfile.yaml"

requestCreateSkillGroup :: CreateSkillGroup -> TestTree
requestCreateSkillGroup =
  req
    "CreateSkillGroup"
    "fixture/CreateSkillGroup.yaml"

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

responseCreateContact :: CreateContactResponse -> TestTree
responseCreateContact =
  res
    "CreateContactResponse"
    "fixture/CreateContactResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContact)

responseDeleteBusinessReportSchedule :: DeleteBusinessReportScheduleResponse -> TestTree
responseDeleteBusinessReportSchedule =
  res
    "DeleteBusinessReportScheduleResponse"
    "fixture/DeleteBusinessReportScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBusinessReportSchedule)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile =
  res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNetworkProfile)

responseListBusinessReportSchedules :: ListBusinessReportSchedulesResponse -> TestTree
responseListBusinessReportSchedules =
  res
    "ListBusinessReportSchedulesResponse"
    "fixture/ListBusinessReportSchedulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBusinessReportSchedules)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile =
  res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkProfile)

responseUpdateBusinessReportSchedule :: UpdateBusinessReportScheduleResponse -> TestTree
responseUpdateBusinessReportSchedule =
  res
    "UpdateBusinessReportScheduleResponse"
    "fixture/UpdateBusinessReportScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBusinessReportSchedule)

responseDeleteDeviceUsageData :: DeleteDeviceUsageDataResponse -> TestTree
responseDeleteDeviceUsageData =
  res
    "DeleteDeviceUsageDataResponse"
    "fixture/DeleteDeviceUsageDataResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeviceUsageData)

responseGetConferenceProvider :: GetConferenceProviderResponse -> TestTree
responseGetConferenceProvider =
  res
    "GetConferenceProviderResponse"
    "fixture/GetConferenceProviderResponse.proto"
    defaultService
    (Proxy :: Proxy GetConferenceProvider)

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

responseListSkillsStoreCategories :: ListSkillsStoreCategoriesResponse -> TestTree
responseListSkillsStoreCategories =
  res
    "ListSkillsStoreCategoriesResponse"
    "fixture/ListSkillsStoreCategoriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSkillsStoreCategories)

responseCreateBusinessReportSchedule :: CreateBusinessReportScheduleResponse -> TestTree
responseCreateBusinessReportSchedule =
  res
    "CreateBusinessReportScheduleResponse"
    "fixture/CreateBusinessReportScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBusinessReportSchedule)

responseGetAddressBook :: GetAddressBookResponse -> TestTree
responseGetAddressBook =
  res
    "GetAddressBookResponse"
    "fixture/GetAddressBookResponse.proto"
    defaultService
    (Proxy :: Proxy GetAddressBook)

responseAssociateContactWithAddressBook :: AssociateContactWithAddressBookResponse -> TestTree
responseAssociateContactWithAddressBook =
  res
    "AssociateContactWithAddressBookResponse"
    "fixture/AssociateContactWithAddressBookResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateContactWithAddressBook)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevice)

responseDeleteRoomSkillParameter :: DeleteRoomSkillParameterResponse -> TestTree
responseDeleteRoomSkillParameter =
  res
    "DeleteRoomSkillParameterResponse"
    "fixture/DeleteRoomSkillParameterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRoomSkillParameter)

responseListSkillsStoreSkillsByCategory :: ListSkillsStoreSkillsByCategoryResponse -> TestTree
responseListSkillsStoreSkillsByCategory =
  res
    "ListSkillsStoreSkillsByCategoryResponse"
    "fixture/ListSkillsStoreSkillsByCategoryResponse.proto"
    defaultService
    (Proxy :: Proxy ListSkillsStoreSkillsByCategory)

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

responsePutSkillAuthorization :: PutSkillAuthorizationResponse -> TestTree
responsePutSkillAuthorization =
  res
    "PutSkillAuthorizationResponse"
    "fixture/PutSkillAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy PutSkillAuthorization)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

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

responseSearchNetworkProfiles :: SearchNetworkProfilesResponse -> TestTree
responseSearchNetworkProfiles =
  res
    "SearchNetworkProfilesResponse"
    "fixture/SearchNetworkProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchNetworkProfiles)

responseGetSkillGroup :: GetSkillGroupResponse -> TestTree
responseGetSkillGroup =
  res
    "GetSkillGroupResponse"
    "fixture/GetSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetSkillGroup)

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

responseListDeviceEvents :: ListDeviceEventsResponse -> TestTree
responseListDeviceEvents =
  res
    "ListDeviceEventsResponse"
    "fixture/ListDeviceEventsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeviceEvents)

responseSendAnnouncement :: SendAnnouncementResponse -> TestTree
responseSendAnnouncement =
  res
    "SendAnnouncementResponse"
    "fixture/SendAnnouncementResponse.proto"
    defaultService
    (Proxy :: Proxy SendAnnouncement)

responseDisassociateSkillGroupFromRoom :: DisassociateSkillGroupFromRoomResponse -> TestTree
responseDisassociateSkillGroupFromRoom =
  res
    "DisassociateSkillGroupFromRoomResponse"
    "fixture/DisassociateSkillGroupFromRoomResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateSkillGroupFromRoom)

responseGetProfile :: GetProfileResponse -> TestTree
responseGetProfile =
  res
    "GetProfileResponse"
    "fixture/GetProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetProfile)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseSearchContacts :: SearchContactsResponse -> TestTree
responseSearchContacts =
  res
    "SearchContactsResponse"
    "fixture/SearchContactsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchContacts)

responseRegisterAVSDevice :: RegisterAVSDeviceResponse -> TestTree
responseRegisterAVSDevice =
  res
    "RegisterAVSDeviceResponse"
    "fixture/RegisterAVSDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterAVSDevice)

responseSendInvitation :: SendInvitationResponse -> TestTree
responseSendInvitation =
  res
    "SendInvitationResponse"
    "fixture/SendInvitationResponse.proto"
    defaultService
    (Proxy :: Proxy SendInvitation)

responseForgetSmartHomeAppliances :: ForgetSmartHomeAppliancesResponse -> TestTree
responseForgetSmartHomeAppliances =
  res
    "ForgetSmartHomeAppliancesResponse"
    "fixture/ForgetSmartHomeAppliancesResponse.proto"
    defaultService
    (Proxy :: Proxy ForgetSmartHomeAppliances)

responseAssociateSkillWithUsers :: AssociateSkillWithUsersResponse -> TestTree
responseAssociateSkillWithUsers =
  res
    "AssociateSkillWithUsersResponse"
    "fixture/AssociateSkillWithUsersResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSkillWithUsers)

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

responseDeleteSkillGroup :: DeleteSkillGroupResponse -> TestTree
responseDeleteSkillGroup =
  res
    "DeleteSkillGroupResponse"
    "fixture/DeleteSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSkillGroup)

responseUpdateSkillGroup :: UpdateSkillGroupResponse -> TestTree
responseUpdateSkillGroup =
  res
    "UpdateSkillGroupResponse"
    "fixture/UpdateSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSkillGroup)

responseAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoomResponse -> TestTree
responseAssociateSkillGroupWithRoom =
  res
    "AssociateSkillGroupWithRoomResponse"
    "fixture/AssociateSkillGroupWithRoomResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSkillGroupWithRoom)

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

responseUpdateGateway :: UpdateGatewayResponse -> TestTree
responseUpdateGateway =
  res
    "UpdateGatewayResponse"
    "fixture/UpdateGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGateway)

responseDeleteDevice :: DeleteDeviceResponse -> TestTree
responseDeleteDevice =
  res
    "DeleteDeviceResponse"
    "fixture/DeleteDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDevice)

responseRevokeInvitation :: RevokeInvitationResponse -> TestTree
responseRevokeInvitation =
  res
    "RevokeInvitationResponse"
    "fixture/RevokeInvitationResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeInvitation)

responseGetRoomSkillParameter :: GetRoomSkillParameterResponse -> TestTree
responseGetRoomSkillParameter =
  res
    "GetRoomSkillParameterResponse"
    "fixture/GetRoomSkillParameterResponse.proto"
    defaultService
    (Proxy :: Proxy GetRoomSkillParameter)

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

responseUpdateDevice :: UpdateDeviceResponse -> TestTree
responseUpdateDevice =
  res
    "UpdateDeviceResponse"
    "fixture/UpdateDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDevice)

responseAssociateDeviceWithRoom :: AssociateDeviceWithRoomResponse -> TestTree
responseAssociateDeviceWithRoom =
  res
    "AssociateDeviceWithRoomResponse"
    "fixture/AssociateDeviceWithRoomResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDeviceWithRoom)

responseAssociateSkillWithSkillGroup :: AssociateSkillWithSkillGroupResponse -> TestTree
responseAssociateSkillWithSkillGroup =
  res
    "AssociateSkillWithSkillGroupResponse"
    "fixture/AssociateSkillWithSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSkillWithSkillGroup)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy ListGateways)

responseDeleteRoom :: DeleteRoomResponse -> TestTree
responseDeleteRoom =
  res
    "DeleteRoomResponse"
    "fixture/DeleteRoomResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRoom)

responseListConferenceProviders :: ListConferenceProvidersResponse -> TestTree
responseListConferenceProviders =
  res
    "ListConferenceProvidersResponse"
    "fixture/ListConferenceProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy ListConferenceProviders)

responseDeleteGatewayGroup :: DeleteGatewayGroupResponse -> TestTree
responseDeleteGatewayGroup =
  res
    "DeleteGatewayGroupResponse"
    "fixture/DeleteGatewayGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGatewayGroup)

responseUpdateRoom :: UpdateRoomResponse -> TestTree
responseUpdateRoom =
  res
    "UpdateRoomResponse"
    "fixture/UpdateRoomResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoom)

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

responseUpdateConferenceProvider :: UpdateConferenceProviderResponse -> TestTree
responseUpdateConferenceProvider =
  res
    "UpdateConferenceProviderResponse"
    "fixture/UpdateConferenceProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConferenceProvider)

responseUpdateGatewayGroup :: UpdateGatewayGroupResponse -> TestTree
responseUpdateGatewayGroup =
  res
    "UpdateGatewayGroupResponse"
    "fixture/UpdateGatewayGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGatewayGroup)

responseListGatewayGroups :: ListGatewayGroupsResponse -> TestTree
responseListGatewayGroups =
  res
    "ListGatewayGroupsResponse"
    "fixture/ListGatewayGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGatewayGroups)

responseApproveSkill :: ApproveSkillResponse -> TestTree
responseApproveSkill =
  res
    "ApproveSkillResponse"
    "fixture/ApproveSkillResponse.proto"
    defaultService
    (Proxy :: Proxy ApproveSkill)

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

responsePutRoomSkillParameter :: PutRoomSkillParameterResponse -> TestTree
responsePutRoomSkillParameter =
  res
    "PutRoomSkillParameterResponse"
    "fixture/PutRoomSkillParameterResponse.proto"
    defaultService
    (Proxy :: Proxy PutRoomSkillParameter)

responseDisassociateDeviceFromRoom :: DisassociateDeviceFromRoomResponse -> TestTree
responseDisassociateDeviceFromRoom =
  res
    "DisassociateDeviceFromRoomResponse"
    "fixture/DisassociateDeviceFromRoomResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateDeviceFromRoom)

responseCreateAddressBook :: CreateAddressBookResponse -> TestTree
responseCreateAddressBook =
  res
    "CreateAddressBookResponse"
    "fixture/CreateAddressBookResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAddressBook)

responseCreateRoom :: CreateRoomResponse -> TestTree
responseCreateRoom =
  res
    "CreateRoomResponse"
    "fixture/CreateRoomResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRoom)

responseCreateConferenceProvider :: CreateConferenceProviderResponse -> TestTree
responseCreateConferenceProvider =
  res
    "CreateConferenceProviderResponse"
    "fixture/CreateConferenceProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConferenceProvider)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile =
  res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetNetworkProfile)

responseGetConferencePreference :: GetConferencePreferenceResponse -> TestTree
responseGetConferencePreference =
  res
    "GetConferencePreferenceResponse"
    "fixture/GetConferencePreferenceResponse.proto"
    defaultService
    (Proxy :: Proxy GetConferencePreference)

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

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseDisassociateSkillFromSkillGroup :: DisassociateSkillFromSkillGroupResponse -> TestTree
responseDisassociateSkillFromSkillGroup =
  res
    "DisassociateSkillFromSkillGroupResponse"
    "fixture/DisassociateSkillFromSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateSkillFromSkillGroup)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseListSkills :: ListSkillsResponse -> TestTree
responseListSkills =
  res
    "ListSkillsResponse"
    "fixture/ListSkillsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSkills)

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

responseSearchAddressBooks :: SearchAddressBooksResponse -> TestTree
responseSearchAddressBooks =
  res
    "SearchAddressBooksResponse"
    "fixture/SearchAddressBooksResponse.proto"
    defaultService
    (Proxy :: Proxy SearchAddressBooks)

responseListSmartHomeAppliances :: ListSmartHomeAppliancesResponse -> TestTree
responseListSmartHomeAppliances =
  res
    "ListSmartHomeAppliancesResponse"
    "fixture/ListSmartHomeAppliancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSmartHomeAppliances)

responseDeleteProfile :: DeleteProfileResponse -> TestTree
responseDeleteProfile =
  res
    "DeleteProfileResponse"
    "fixture/DeleteProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProfile)

responseCreateSkillGroup :: CreateSkillGroupResponse -> TestTree
responseCreateSkillGroup =
  res
    "CreateSkillGroupResponse"
    "fixture/CreateSkillGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSkillGroup)

responseUpdateProfile :: UpdateProfileResponse -> TestTree
responseUpdateProfile =
  res
    "UpdateProfileResponse"
    "fixture/UpdateProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProfile)
