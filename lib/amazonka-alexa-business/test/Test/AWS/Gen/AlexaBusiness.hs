{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AlexaBusiness
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AlexaBusiness where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.AlexaBusiness
import Test.AWS.AlexaBusiness.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestSearchUsers $
--             mkSearchUsers
--
--         , requestPutConferencePreference $
--             mkPutConferencePreference
--
--         , requestUpdateNetworkProfile $
--             mkUpdateNetworkProfile
--
--         , requestDeleteNetworkProfile $
--             mkDeleteNetworkProfile
--
--         , requestUpdateBusinessReportSchedule $
--             mkUpdateBusinessReportSchedule
--
--         , requestDeleteBusinessReportSchedule $
--             mkDeleteBusinessReportSchedule
--
--         , requestAssociateSkillGroupWithRoom $
--             mkAssociateSkillGroupWithRoom
--
--         , requestListSmartHomeAppliances $
--             mkListSmartHomeAppliances
--
--         , requestDeleteProfile $
--             mkDeleteProfile
--
--         , requestUpdateProfile $
--             mkUpdateProfile
--
--         , requestSearchRooms $
--             mkSearchRooms
--
--         , requestAssociateSkillWithUsers $
--             mkAssociateSkillWithUsers
--
--         , requestRegisterAVSDevice $
--             mkRegisterAVSDevice
--
--         , requestForgetSmartHomeAppliances $
--             mkForgetSmartHomeAppliances
--
--         , requestPutInvitationConfiguration $
--             mkPutInvitationConfiguration
--
--         , requestDisassociateContactFromAddressBook $
--             mkDisassociateContactFromAddressBook
--
--         , requestGetNetworkProfile $
--             mkGetNetworkProfile
--
--         , requestGetConferencePreference $
--             mkGetConferencePreference
--
--         , requestDisassociateSkillFromSkillGroup $
--             mkDisassociateSkillFromSkillGroup
--
--         , requestCreateAddressBook $
--             mkCreateAddressBook
--
--         , requestDeleteAddressBook $
--             mkDeleteAddressBook
--
--         , requestUpdateAddressBook $
--             mkUpdateAddressBook
--
--         , requestDeleteGatewayGroup $
--             mkDeleteGatewayGroup
--
--         , requestUpdateGatewayGroup $
--             mkUpdateGatewayGroup
--
--         , requestUpdateRoom $
--             mkUpdateRoom
--
--         , requestDeleteRoom $
--             mkDeleteRoom
--
--         , requestGetDevice $
--             mkGetDevice
--
--         , requestGetGateway $
--             mkGetGateway
--
--         , requestListSkillsStoreSkillsByCategory $
--             mkListSkillsStoreSkillsByCategory
--
--         , requestDeleteConferenceProvider $
--             mkDeleteConferenceProvider
--
--         , requestUpdateConferenceProvider $
--             mkUpdateConferenceProvider
--
--         , requestGetContact $
--             mkGetContact
--
--         , requestApproveSkill $
--             mkApproveSkill
--
--         , requestCreateNetworkProfile $
--             mkCreateNetworkProfile
--
--         , requestAssociateDeviceWithRoom $
--             mkAssociateDeviceWithRoom
--
--         , requestGetRoomSkillParameter $
--             mkGetRoomSkillParameter
--
--         , requestUpdateGateway $
--             mkUpdateGateway
--
--         , requestCreateBusinessReportSchedule $
--             mkCreateBusinessReportSchedule
--
--         , requestDeleteContact $
--             mkDeleteContact
--
--         , requestUpdateContact $
--             mkUpdateContact
--
--         , requestGetAddressBook $
--             mkGetAddressBook
--
--         , requestListBusinessReportSchedules $
--             mkListBusinessReportSchedules
--
--         , requestDeleteDeviceUsageData $
--             mkDeleteDeviceUsageData
--
--         , requestCreateContact $
--             mkCreateContact
--
--         , requestCreateProfile $
--             mkCreateProfile
--
--         , requestDeleteSkillGroup $
--             mkDeleteSkillGroup
--
--         , requestUpdateSkillGroup $
--             mkUpdateSkillGroup
--
--         , requestStartDeviceSync $
--             mkStartDeviceSync
--
--         , requestGetInvitationConfiguration $
--             mkGetInvitationConfiguration
--
--         , requestDisassociateSkillFromUsers $
--             mkDisassociateSkillFromUsers
--
--         , requestSearchAddressBooks $
--             mkSearchAddressBooks
--
--         , requestCreateSkillGroup $
--             mkCreateSkillGroup
--
--         , requestGetProfile $
--             mkGetProfile
--
--         , requestDisassociateSkillGroupFromRoom $
--             mkDisassociateSkillGroupFromRoom
--
--         , requestSendInvitation $
--             mkSendInvitation
--
--         , requestListDeviceEvents $
--             mkListDeviceEvents
--
--         , requestCreateUser $
--             mkCreateUser
--
--         , requestSearchDevices $
--             mkSearchDevices
--
--         , requestSearchContacts $
--             mkSearchContacts
--
--         , requestSendAnnouncement $
--             mkSendAnnouncement
--
--         , requestDeleteUser $
--             mkDeleteUser
--
--         , requestSearchNetworkProfiles $
--             mkSearchNetworkProfiles
--
--         , requestGetSkillGroup $
--             mkGetSkillGroup
--
--         , requestListSkills $
--             mkListSkills
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDisassociateDeviceFromRoom $
--             mkDisassociateDeviceFromRoom
--
--         , requestSearchSkillGroups $
--             mkSearchSkillGroups
--
--         , requestPutSkillAuthorization $
--             mkPutSkillAuthorization
--
--         , requestListTags $
--             mkListTags
--
--         , requestDeleteSkillAuthorization $
--             mkDeleteSkillAuthorization
--
--         , requestAssociateDeviceWithNetworkProfile $
--             mkAssociateDeviceWithNetworkProfile
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestCreateConferenceProvider $
--             mkCreateConferenceProvider
--
--         , requestResolveRoom $
--             mkResolveRoom
--
--         , requestCreateGatewayGroup $
--             mkCreateGatewayGroup
--
--         , requestCreateRoom $
--             mkCreateRoom
--
--         , requestDeleteRoomSkillParameter $
--             mkDeleteRoomSkillParameter
--
--         , requestListGatewayGroups $
--             mkListGatewayGroups
--
--         , requestPutRoomSkillParameter $
--             mkPutRoomSkillParameter
--
--         , requestSearchProfiles $
--             mkSearchProfiles
--
--         , requestRejectSkill $
--             mkRejectSkill
--
--         , requestListConferenceProviders $
--             mkListConferenceProviders
--
--         , requestRevokeInvitation $
--             mkRevokeInvitation
--
--         , requestListGateways $
--             mkListGateways
--
--         , requestDeleteDevice $
--             mkDeleteDevice
--
--         , requestUpdateDevice $
--             mkUpdateDevice
--
--         , requestAssociateSkillWithSkillGroup $
--             mkAssociateSkillWithSkillGroup
--
--         , requestGetConferenceProvider $
--             mkGetConferenceProvider
--
--         , requestGetRoom $
--             mkGetRoom
--
--         , requestGetGatewayGroup $
--             mkGetGatewayGroup
--
--         , requestListSkillsStoreCategories $
--             mkListSkillsStoreCategories
--
--         , requestStartSmartHomeApplianceDiscovery $
--             mkStartSmartHomeApplianceDiscovery
--
--         , requestAssociateContactWithAddressBook $
--             mkAssociateContactWithAddressBook
--
--           ]

--     , testGroup "response"
--         [ responseSearchUsers $
--             mkSearchUsersResponse
--
--         , responsePutConferencePreference $
--             mkPutConferencePreferenceResponse
--
--         , responseUpdateNetworkProfile $
--             mkUpdateNetworkProfileResponse
--
--         , responseDeleteNetworkProfile $
--             mkDeleteNetworkProfileResponse
--
--         , responseUpdateBusinessReportSchedule $
--             mkUpdateBusinessReportScheduleResponse
--
--         , responseDeleteBusinessReportSchedule $
--             mkDeleteBusinessReportScheduleResponse
--
--         , responseAssociateSkillGroupWithRoom $
--             mkAssociateSkillGroupWithRoomResponse
--
--         , responseListSmartHomeAppliances $
--             mkListSmartHomeAppliancesResponse
--
--         , responseDeleteProfile $
--             mkDeleteProfileResponse
--
--         , responseUpdateProfile $
--             mkUpdateProfileResponse
--
--         , responseSearchRooms $
--             mkSearchRoomsResponse
--
--         , responseAssociateSkillWithUsers $
--             mkAssociateSkillWithUsersResponse
--
--         , responseRegisterAVSDevice $
--             mkRegisterAVSDeviceResponse
--
--         , responseForgetSmartHomeAppliances $
--             mkForgetSmartHomeAppliancesResponse
--
--         , responsePutInvitationConfiguration $
--             mkPutInvitationConfigurationResponse
--
--         , responseDisassociateContactFromAddressBook $
--             mkDisassociateContactFromAddressBookResponse
--
--         , responseGetNetworkProfile $
--             mkGetNetworkProfileResponse
--
--         , responseGetConferencePreference $
--             mkGetConferencePreferenceResponse
--
--         , responseDisassociateSkillFromSkillGroup $
--             mkDisassociateSkillFromSkillGroupResponse
--
--         , responseCreateAddressBook $
--             mkCreateAddressBookResponse
--
--         , responseDeleteAddressBook $
--             mkDeleteAddressBookResponse
--
--         , responseUpdateAddressBook $
--             mkUpdateAddressBookResponse
--
--         , responseDeleteGatewayGroup $
--             mkDeleteGatewayGroupResponse
--
--         , responseUpdateGatewayGroup $
--             mkUpdateGatewayGroupResponse
--
--         , responseUpdateRoom $
--             mkUpdateRoomResponse
--
--         , responseDeleteRoom $
--             mkDeleteRoomResponse
--
--         , responseGetDevice $
--             mkGetDeviceResponse
--
--         , responseGetGateway $
--             mkGetGatewayResponse
--
--         , responseListSkillsStoreSkillsByCategory $
--             mkListSkillsStoreSkillsByCategoryResponse
--
--         , responseDeleteConferenceProvider $
--             mkDeleteConferenceProviderResponse
--
--         , responseUpdateConferenceProvider $
--             mkUpdateConferenceProviderResponse
--
--         , responseGetContact $
--             mkGetContactResponse
--
--         , responseApproveSkill $
--             mkApproveSkillResponse
--
--         , responseCreateNetworkProfile $
--             mkCreateNetworkProfileResponse
--
--         , responseAssociateDeviceWithRoom $
--             mkAssociateDeviceWithRoomResponse
--
--         , responseGetRoomSkillParameter $
--             mkGetRoomSkillParameterResponse
--
--         , responseUpdateGateway $
--             mkUpdateGatewayResponse
--
--         , responseCreateBusinessReportSchedule $
--             mkCreateBusinessReportScheduleResponse
--
--         , responseDeleteContact $
--             mkDeleteContactResponse
--
--         , responseUpdateContact $
--             mkUpdateContactResponse
--
--         , responseGetAddressBook $
--             mkGetAddressBookResponse
--
--         , responseListBusinessReportSchedules $
--             mkListBusinessReportSchedulesResponse
--
--         , responseDeleteDeviceUsageData $
--             mkDeleteDeviceUsageDataResponse
--
--         , responseCreateContact $
--             mkCreateContactResponse
--
--         , responseCreateProfile $
--             mkCreateProfileResponse
--
--         , responseDeleteSkillGroup $
--             mkDeleteSkillGroupResponse
--
--         , responseUpdateSkillGroup $
--             mkUpdateSkillGroupResponse
--
--         , responseStartDeviceSync $
--             mkStartDeviceSyncResponse
--
--         , responseGetInvitationConfiguration $
--             mkGetInvitationConfigurationResponse
--
--         , responseDisassociateSkillFromUsers $
--             mkDisassociateSkillFromUsersResponse
--
--         , responseSearchAddressBooks $
--             mkSearchAddressBooksResponse
--
--         , responseCreateSkillGroup $
--             mkCreateSkillGroupResponse
--
--         , responseGetProfile $
--             mkGetProfileResponse
--
--         , responseDisassociateSkillGroupFromRoom $
--             mkDisassociateSkillGroupFromRoomResponse
--
--         , responseSendInvitation $
--             mkSendInvitationResponse
--
--         , responseListDeviceEvents $
--             mkListDeviceEventsResponse
--
--         , responseCreateUser $
--             mkCreateUserResponse
--
--         , responseSearchDevices $
--             mkSearchDevicesResponse
--
--         , responseSearchContacts $
--             mkSearchContactsResponse
--
--         , responseSendAnnouncement $
--             mkSendAnnouncementResponse
--
--         , responseDeleteUser $
--             mkDeleteUserResponse
--
--         , responseSearchNetworkProfiles $
--             mkSearchNetworkProfilesResponse
--
--         , responseGetSkillGroup $
--             mkGetSkillGroupResponse
--
--         , responseListSkills $
--             mkListSkillsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDisassociateDeviceFromRoom $
--             mkDisassociateDeviceFromRoomResponse
--
--         , responseSearchSkillGroups $
--             mkSearchSkillGroupsResponse
--
--         , responsePutSkillAuthorization $
--             mkPutSkillAuthorizationResponse
--
--         , responseListTags $
--             mkListTagsResponse
--
--         , responseDeleteSkillAuthorization $
--             mkDeleteSkillAuthorizationResponse
--
--         , responseAssociateDeviceWithNetworkProfile $
--             mkAssociateDeviceWithNetworkProfileResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseCreateConferenceProvider $
--             mkCreateConferenceProviderResponse
--
--         , responseResolveRoom $
--             mkResolveRoomResponse
--
--         , responseCreateGatewayGroup $
--             mkCreateGatewayGroupResponse
--
--         , responseCreateRoom $
--             mkCreateRoomResponse
--
--         , responseDeleteRoomSkillParameter $
--             mkDeleteRoomSkillParameterResponse
--
--         , responseListGatewayGroups $
--             mkListGatewayGroupsResponse
--
--         , responsePutRoomSkillParameter $
--             mkPutRoomSkillParameterResponse
--
--         , responseSearchProfiles $
--             mkSearchProfilesResponse
--
--         , responseRejectSkill $
--             mkRejectSkillResponse
--
--         , responseListConferenceProviders $
--             mkListConferenceProvidersResponse
--
--         , responseRevokeInvitation $
--             mkRevokeInvitationResponse
--
--         , responseListGateways $
--             mkListGatewaysResponse
--
--         , responseDeleteDevice $
--             mkDeleteDeviceResponse
--
--         , responseUpdateDevice $
--             mkUpdateDeviceResponse
--
--         , responseAssociateSkillWithSkillGroup $
--             mkAssociateSkillWithSkillGroupResponse
--
--         , responseGetConferenceProvider $
--             mkGetConferenceProviderResponse
--
--         , responseGetRoom $
--             mkGetRoomResponse
--
--         , responseGetGatewayGroup $
--             mkGetGatewayGroupResponse
--
--         , responseListSkillsStoreCategories $
--             mkListSkillsStoreCategoriesResponse
--
--         , responseStartSmartHomeApplianceDiscovery $
--             mkStartSmartHomeApplianceDiscoveryResponse
--
--         , responseAssociateContactWithAddressBook $
--             mkAssociateContactWithAddressBookResponse
--
--           ]
--     ]

-- Requests

requestSearchUsers :: SearchUsers -> TestTree
requestSearchUsers = req
    "SearchUsers"
    "fixture/SearchUsers.yaml"

requestPutConferencePreference :: PutConferencePreference -> TestTree
requestPutConferencePreference = req
    "PutConferencePreference"
    "fixture/PutConferencePreference.yaml"

requestUpdateNetworkProfile :: UpdateNetworkProfile -> TestTree
requestUpdateNetworkProfile = req
    "UpdateNetworkProfile"
    "fixture/UpdateNetworkProfile.yaml"

requestDeleteNetworkProfile :: DeleteNetworkProfile -> TestTree
requestDeleteNetworkProfile = req
    "DeleteNetworkProfile"
    "fixture/DeleteNetworkProfile.yaml"

requestUpdateBusinessReportSchedule :: UpdateBusinessReportSchedule -> TestTree
requestUpdateBusinessReportSchedule = req
    "UpdateBusinessReportSchedule"
    "fixture/UpdateBusinessReportSchedule.yaml"

requestDeleteBusinessReportSchedule :: DeleteBusinessReportSchedule -> TestTree
requestDeleteBusinessReportSchedule = req
    "DeleteBusinessReportSchedule"
    "fixture/DeleteBusinessReportSchedule.yaml"

requestAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoom -> TestTree
requestAssociateSkillGroupWithRoom = req
    "AssociateSkillGroupWithRoom"
    "fixture/AssociateSkillGroupWithRoom.yaml"

requestListSmartHomeAppliances :: ListSmartHomeAppliances -> TestTree
requestListSmartHomeAppliances = req
    "ListSmartHomeAppliances"
    "fixture/ListSmartHomeAppliances.yaml"

requestDeleteProfile :: DeleteProfile -> TestTree
requestDeleteProfile = req
    "DeleteProfile"
    "fixture/DeleteProfile.yaml"

requestUpdateProfile :: UpdateProfile -> TestTree
requestUpdateProfile = req
    "UpdateProfile"
    "fixture/UpdateProfile.yaml"

requestSearchRooms :: SearchRooms -> TestTree
requestSearchRooms = req
    "SearchRooms"
    "fixture/SearchRooms.yaml"

requestAssociateSkillWithUsers :: AssociateSkillWithUsers -> TestTree
requestAssociateSkillWithUsers = req
    "AssociateSkillWithUsers"
    "fixture/AssociateSkillWithUsers.yaml"

requestRegisterAVSDevice :: RegisterAVSDevice -> TestTree
requestRegisterAVSDevice = req
    "RegisterAVSDevice"
    "fixture/RegisterAVSDevice.yaml"

requestForgetSmartHomeAppliances :: ForgetSmartHomeAppliances -> TestTree
requestForgetSmartHomeAppliances = req
    "ForgetSmartHomeAppliances"
    "fixture/ForgetSmartHomeAppliances.yaml"

requestPutInvitationConfiguration :: PutInvitationConfiguration -> TestTree
requestPutInvitationConfiguration = req
    "PutInvitationConfiguration"
    "fixture/PutInvitationConfiguration.yaml"

requestDisassociateContactFromAddressBook :: DisassociateContactFromAddressBook -> TestTree
requestDisassociateContactFromAddressBook = req
    "DisassociateContactFromAddressBook"
    "fixture/DisassociateContactFromAddressBook.yaml"

requestGetNetworkProfile :: GetNetworkProfile -> TestTree
requestGetNetworkProfile = req
    "GetNetworkProfile"
    "fixture/GetNetworkProfile.yaml"

requestGetConferencePreference :: GetConferencePreference -> TestTree
requestGetConferencePreference = req
    "GetConferencePreference"
    "fixture/GetConferencePreference.yaml"

requestDisassociateSkillFromSkillGroup :: DisassociateSkillFromSkillGroup -> TestTree
requestDisassociateSkillFromSkillGroup = req
    "DisassociateSkillFromSkillGroup"
    "fixture/DisassociateSkillFromSkillGroup.yaml"

requestCreateAddressBook :: CreateAddressBook -> TestTree
requestCreateAddressBook = req
    "CreateAddressBook"
    "fixture/CreateAddressBook.yaml"

requestDeleteAddressBook :: DeleteAddressBook -> TestTree
requestDeleteAddressBook = req
    "DeleteAddressBook"
    "fixture/DeleteAddressBook.yaml"

requestUpdateAddressBook :: UpdateAddressBook -> TestTree
requestUpdateAddressBook = req
    "UpdateAddressBook"
    "fixture/UpdateAddressBook.yaml"

requestDeleteGatewayGroup :: DeleteGatewayGroup -> TestTree
requestDeleteGatewayGroup = req
    "DeleteGatewayGroup"
    "fixture/DeleteGatewayGroup.yaml"

requestUpdateGatewayGroup :: UpdateGatewayGroup -> TestTree
requestUpdateGatewayGroup = req
    "UpdateGatewayGroup"
    "fixture/UpdateGatewayGroup.yaml"

requestUpdateRoom :: UpdateRoom -> TestTree
requestUpdateRoom = req
    "UpdateRoom"
    "fixture/UpdateRoom.yaml"

requestDeleteRoom :: DeleteRoom -> TestTree
requestDeleteRoom = req
    "DeleteRoom"
    "fixture/DeleteRoom.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice = req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestGetGateway :: GetGateway -> TestTree
requestGetGateway = req
    "GetGateway"
    "fixture/GetGateway.yaml"

requestListSkillsStoreSkillsByCategory :: ListSkillsStoreSkillsByCategory -> TestTree
requestListSkillsStoreSkillsByCategory = req
    "ListSkillsStoreSkillsByCategory"
    "fixture/ListSkillsStoreSkillsByCategory.yaml"

requestDeleteConferenceProvider :: DeleteConferenceProvider -> TestTree
requestDeleteConferenceProvider = req
    "DeleteConferenceProvider"
    "fixture/DeleteConferenceProvider.yaml"

requestUpdateConferenceProvider :: UpdateConferenceProvider -> TestTree
requestUpdateConferenceProvider = req
    "UpdateConferenceProvider"
    "fixture/UpdateConferenceProvider.yaml"

requestGetContact :: GetContact -> TestTree
requestGetContact = req
    "GetContact"
    "fixture/GetContact.yaml"

requestApproveSkill :: ApproveSkill -> TestTree
requestApproveSkill = req
    "ApproveSkill"
    "fixture/ApproveSkill.yaml"

requestCreateNetworkProfile :: CreateNetworkProfile -> TestTree
requestCreateNetworkProfile = req
    "CreateNetworkProfile"
    "fixture/CreateNetworkProfile.yaml"

requestAssociateDeviceWithRoom :: AssociateDeviceWithRoom -> TestTree
requestAssociateDeviceWithRoom = req
    "AssociateDeviceWithRoom"
    "fixture/AssociateDeviceWithRoom.yaml"

requestGetRoomSkillParameter :: GetRoomSkillParameter -> TestTree
requestGetRoomSkillParameter = req
    "GetRoomSkillParameter"
    "fixture/GetRoomSkillParameter.yaml"

requestUpdateGateway :: UpdateGateway -> TestTree
requestUpdateGateway = req
    "UpdateGateway"
    "fixture/UpdateGateway.yaml"

requestCreateBusinessReportSchedule :: CreateBusinessReportSchedule -> TestTree
requestCreateBusinessReportSchedule = req
    "CreateBusinessReportSchedule"
    "fixture/CreateBusinessReportSchedule.yaml"

requestDeleteContact :: DeleteContact -> TestTree
requestDeleteContact = req
    "DeleteContact"
    "fixture/DeleteContact.yaml"

requestUpdateContact :: UpdateContact -> TestTree
requestUpdateContact = req
    "UpdateContact"
    "fixture/UpdateContact.yaml"

requestGetAddressBook :: GetAddressBook -> TestTree
requestGetAddressBook = req
    "GetAddressBook"
    "fixture/GetAddressBook.yaml"

requestListBusinessReportSchedules :: ListBusinessReportSchedules -> TestTree
requestListBusinessReportSchedules = req
    "ListBusinessReportSchedules"
    "fixture/ListBusinessReportSchedules.yaml"

requestDeleteDeviceUsageData :: DeleteDeviceUsageData -> TestTree
requestDeleteDeviceUsageData = req
    "DeleteDeviceUsageData"
    "fixture/DeleteDeviceUsageData.yaml"

requestCreateContact :: CreateContact -> TestTree
requestCreateContact = req
    "CreateContact"
    "fixture/CreateContact.yaml"

requestCreateProfile :: CreateProfile -> TestTree
requestCreateProfile = req
    "CreateProfile"
    "fixture/CreateProfile.yaml"

requestDeleteSkillGroup :: DeleteSkillGroup -> TestTree
requestDeleteSkillGroup = req
    "DeleteSkillGroup"
    "fixture/DeleteSkillGroup.yaml"

requestUpdateSkillGroup :: UpdateSkillGroup -> TestTree
requestUpdateSkillGroup = req
    "UpdateSkillGroup"
    "fixture/UpdateSkillGroup.yaml"

requestStartDeviceSync :: StartDeviceSync -> TestTree
requestStartDeviceSync = req
    "StartDeviceSync"
    "fixture/StartDeviceSync.yaml"

requestGetInvitationConfiguration :: GetInvitationConfiguration -> TestTree
requestGetInvitationConfiguration = req
    "GetInvitationConfiguration"
    "fixture/GetInvitationConfiguration.yaml"

requestDisassociateSkillFromUsers :: DisassociateSkillFromUsers -> TestTree
requestDisassociateSkillFromUsers = req
    "DisassociateSkillFromUsers"
    "fixture/DisassociateSkillFromUsers.yaml"

requestSearchAddressBooks :: SearchAddressBooks -> TestTree
requestSearchAddressBooks = req
    "SearchAddressBooks"
    "fixture/SearchAddressBooks.yaml"

requestCreateSkillGroup :: CreateSkillGroup -> TestTree
requestCreateSkillGroup = req
    "CreateSkillGroup"
    "fixture/CreateSkillGroup.yaml"

requestGetProfile :: GetProfile -> TestTree
requestGetProfile = req
    "GetProfile"
    "fixture/GetProfile.yaml"

requestDisassociateSkillGroupFromRoom :: DisassociateSkillGroupFromRoom -> TestTree
requestDisassociateSkillGroupFromRoom = req
    "DisassociateSkillGroupFromRoom"
    "fixture/DisassociateSkillGroupFromRoom.yaml"

requestSendInvitation :: SendInvitation -> TestTree
requestSendInvitation = req
    "SendInvitation"
    "fixture/SendInvitation.yaml"

requestListDeviceEvents :: ListDeviceEvents -> TestTree
requestListDeviceEvents = req
    "ListDeviceEvents"
    "fixture/ListDeviceEvents.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser = req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestSearchDevices :: SearchDevices -> TestTree
requestSearchDevices = req
    "SearchDevices"
    "fixture/SearchDevices.yaml"

requestSearchContacts :: SearchContacts -> TestTree
requestSearchContacts = req
    "SearchContacts"
    "fixture/SearchContacts.yaml"

requestSendAnnouncement :: SendAnnouncement -> TestTree
requestSendAnnouncement = req
    "SendAnnouncement"
    "fixture/SendAnnouncement.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestSearchNetworkProfiles :: SearchNetworkProfiles -> TestTree
requestSearchNetworkProfiles = req
    "SearchNetworkProfiles"
    "fixture/SearchNetworkProfiles.yaml"

requestGetSkillGroup :: GetSkillGroup -> TestTree
requestGetSkillGroup = req
    "GetSkillGroup"
    "fixture/GetSkillGroup.yaml"

requestListSkills :: ListSkills -> TestTree
requestListSkills = req
    "ListSkills"
    "fixture/ListSkills.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestDisassociateDeviceFromRoom :: DisassociateDeviceFromRoom -> TestTree
requestDisassociateDeviceFromRoom = req
    "DisassociateDeviceFromRoom"
    "fixture/DisassociateDeviceFromRoom.yaml"

requestSearchSkillGroups :: SearchSkillGroups -> TestTree
requestSearchSkillGroups = req
    "SearchSkillGroups"
    "fixture/SearchSkillGroups.yaml"

requestPutSkillAuthorization :: PutSkillAuthorization -> TestTree
requestPutSkillAuthorization = req
    "PutSkillAuthorization"
    "fixture/PutSkillAuthorization.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestDeleteSkillAuthorization :: DeleteSkillAuthorization -> TestTree
requestDeleteSkillAuthorization = req
    "DeleteSkillAuthorization"
    "fixture/DeleteSkillAuthorization.yaml"

requestAssociateDeviceWithNetworkProfile :: AssociateDeviceWithNetworkProfile -> TestTree
requestAssociateDeviceWithNetworkProfile = req
    "AssociateDeviceWithNetworkProfile"
    "fixture/AssociateDeviceWithNetworkProfile.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateConferenceProvider :: CreateConferenceProvider -> TestTree
requestCreateConferenceProvider = req
    "CreateConferenceProvider"
    "fixture/CreateConferenceProvider.yaml"

requestResolveRoom :: ResolveRoom -> TestTree
requestResolveRoom = req
    "ResolveRoom"
    "fixture/ResolveRoom.yaml"

requestCreateGatewayGroup :: CreateGatewayGroup -> TestTree
requestCreateGatewayGroup = req
    "CreateGatewayGroup"
    "fixture/CreateGatewayGroup.yaml"

requestCreateRoom :: CreateRoom -> TestTree
requestCreateRoom = req
    "CreateRoom"
    "fixture/CreateRoom.yaml"

requestDeleteRoomSkillParameter :: DeleteRoomSkillParameter -> TestTree
requestDeleteRoomSkillParameter = req
    "DeleteRoomSkillParameter"
    "fixture/DeleteRoomSkillParameter.yaml"

requestListGatewayGroups :: ListGatewayGroups -> TestTree
requestListGatewayGroups = req
    "ListGatewayGroups"
    "fixture/ListGatewayGroups.yaml"

requestPutRoomSkillParameter :: PutRoomSkillParameter -> TestTree
requestPutRoomSkillParameter = req
    "PutRoomSkillParameter"
    "fixture/PutRoomSkillParameter.yaml"

requestSearchProfiles :: SearchProfiles -> TestTree
requestSearchProfiles = req
    "SearchProfiles"
    "fixture/SearchProfiles.yaml"

requestRejectSkill :: RejectSkill -> TestTree
requestRejectSkill = req
    "RejectSkill"
    "fixture/RejectSkill.yaml"

requestListConferenceProviders :: ListConferenceProviders -> TestTree
requestListConferenceProviders = req
    "ListConferenceProviders"
    "fixture/ListConferenceProviders.yaml"

requestRevokeInvitation :: RevokeInvitation -> TestTree
requestRevokeInvitation = req
    "RevokeInvitation"
    "fixture/RevokeInvitation.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways = req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestDeleteDevice :: DeleteDevice -> TestTree
requestDeleteDevice = req
    "DeleteDevice"
    "fixture/DeleteDevice.yaml"

requestUpdateDevice :: UpdateDevice -> TestTree
requestUpdateDevice = req
    "UpdateDevice"
    "fixture/UpdateDevice.yaml"

requestAssociateSkillWithSkillGroup :: AssociateSkillWithSkillGroup -> TestTree
requestAssociateSkillWithSkillGroup = req
    "AssociateSkillWithSkillGroup"
    "fixture/AssociateSkillWithSkillGroup.yaml"

requestGetConferenceProvider :: GetConferenceProvider -> TestTree
requestGetConferenceProvider = req
    "GetConferenceProvider"
    "fixture/GetConferenceProvider.yaml"

requestGetRoom :: GetRoom -> TestTree
requestGetRoom = req
    "GetRoom"
    "fixture/GetRoom.yaml"

requestGetGatewayGroup :: GetGatewayGroup -> TestTree
requestGetGatewayGroup = req
    "GetGatewayGroup"
    "fixture/GetGatewayGroup.yaml"

requestListSkillsStoreCategories :: ListSkillsStoreCategories -> TestTree
requestListSkillsStoreCategories = req
    "ListSkillsStoreCategories"
    "fixture/ListSkillsStoreCategories.yaml"

requestStartSmartHomeApplianceDiscovery :: StartSmartHomeApplianceDiscovery -> TestTree
requestStartSmartHomeApplianceDiscovery = req
    "StartSmartHomeApplianceDiscovery"
    "fixture/StartSmartHomeApplianceDiscovery.yaml"

requestAssociateContactWithAddressBook :: AssociateContactWithAddressBook -> TestTree
requestAssociateContactWithAddressBook = req
    "AssociateContactWithAddressBook"
    "fixture/AssociateContactWithAddressBook.yaml"

-- Responses

responseSearchUsers :: SearchUsersResponse -> TestTree
responseSearchUsers = res
    "SearchUsersResponse"
    "fixture/SearchUsersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchUsers)

responsePutConferencePreference :: PutConferencePreferenceResponse -> TestTree
responsePutConferencePreference = res
    "PutConferencePreferenceResponse"
    "fixture/PutConferencePreferenceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutConferencePreference)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile = res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateNetworkProfile)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile = res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteNetworkProfile)

responseUpdateBusinessReportSchedule :: UpdateBusinessReportScheduleResponse -> TestTree
responseUpdateBusinessReportSchedule = res
    "UpdateBusinessReportScheduleResponse"
    "fixture/UpdateBusinessReportScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateBusinessReportSchedule)

responseDeleteBusinessReportSchedule :: DeleteBusinessReportScheduleResponse -> TestTree
responseDeleteBusinessReportSchedule = res
    "DeleteBusinessReportScheduleResponse"
    "fixture/DeleteBusinessReportScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBusinessReportSchedule)

responseAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoomResponse -> TestTree
responseAssociateSkillGroupWithRoom = res
    "AssociateSkillGroupWithRoomResponse"
    "fixture/AssociateSkillGroupWithRoomResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateSkillGroupWithRoom)

responseListSmartHomeAppliances :: ListSmartHomeAppliancesResponse -> TestTree
responseListSmartHomeAppliances = res
    "ListSmartHomeAppliancesResponse"
    "fixture/ListSmartHomeAppliancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSmartHomeAppliances)

responseDeleteProfile :: DeleteProfileResponse -> TestTree
responseDeleteProfile = res
    "DeleteProfileResponse"
    "fixture/DeleteProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteProfile)

responseUpdateProfile :: UpdateProfileResponse -> TestTree
responseUpdateProfile = res
    "UpdateProfileResponse"
    "fixture/UpdateProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateProfile)

responseSearchRooms :: SearchRoomsResponse -> TestTree
responseSearchRooms = res
    "SearchRoomsResponse"
    "fixture/SearchRoomsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchRooms)

responseAssociateSkillWithUsers :: AssociateSkillWithUsersResponse -> TestTree
responseAssociateSkillWithUsers = res
    "AssociateSkillWithUsersResponse"
    "fixture/AssociateSkillWithUsersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateSkillWithUsers)

responseRegisterAVSDevice :: RegisterAVSDeviceResponse -> TestTree
responseRegisterAVSDevice = res
    "RegisterAVSDeviceResponse"
    "fixture/RegisterAVSDeviceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterAVSDevice)

responseForgetSmartHomeAppliances :: ForgetSmartHomeAppliancesResponse -> TestTree
responseForgetSmartHomeAppliances = res
    "ForgetSmartHomeAppliancesResponse"
    "fixture/ForgetSmartHomeAppliancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ForgetSmartHomeAppliances)

responsePutInvitationConfiguration :: PutInvitationConfigurationResponse -> TestTree
responsePutInvitationConfiguration = res
    "PutInvitationConfigurationResponse"
    "fixture/PutInvitationConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutInvitationConfiguration)

responseDisassociateContactFromAddressBook :: DisassociateContactFromAddressBookResponse -> TestTree
responseDisassociateContactFromAddressBook = res
    "DisassociateContactFromAddressBookResponse"
    "fixture/DisassociateContactFromAddressBookResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateContactFromAddressBook)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile = res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetNetworkProfile)

responseGetConferencePreference :: GetConferencePreferenceResponse -> TestTree
responseGetConferencePreference = res
    "GetConferencePreferenceResponse"
    "fixture/GetConferencePreferenceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetConferencePreference)

responseDisassociateSkillFromSkillGroup :: DisassociateSkillFromSkillGroupResponse -> TestTree
responseDisassociateSkillFromSkillGroup = res
    "DisassociateSkillFromSkillGroupResponse"
    "fixture/DisassociateSkillFromSkillGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateSkillFromSkillGroup)

responseCreateAddressBook :: CreateAddressBookResponse -> TestTree
responseCreateAddressBook = res
    "CreateAddressBookResponse"
    "fixture/CreateAddressBookResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAddressBook)

responseDeleteAddressBook :: DeleteAddressBookResponse -> TestTree
responseDeleteAddressBook = res
    "DeleteAddressBookResponse"
    "fixture/DeleteAddressBookResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAddressBook)

responseUpdateAddressBook :: UpdateAddressBookResponse -> TestTree
responseUpdateAddressBook = res
    "UpdateAddressBookResponse"
    "fixture/UpdateAddressBookResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAddressBook)

responseDeleteGatewayGroup :: DeleteGatewayGroupResponse -> TestTree
responseDeleteGatewayGroup = res
    "DeleteGatewayGroupResponse"
    "fixture/DeleteGatewayGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGatewayGroup)

responseUpdateGatewayGroup :: UpdateGatewayGroupResponse -> TestTree
responseUpdateGatewayGroup = res
    "UpdateGatewayGroupResponse"
    "fixture/UpdateGatewayGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGatewayGroup)

responseUpdateRoom :: UpdateRoomResponse -> TestTree
responseUpdateRoom = res
    "UpdateRoomResponse"
    "fixture/UpdateRoomResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRoom)

responseDeleteRoom :: DeleteRoomResponse -> TestTree
responseDeleteRoom = res
    "DeleteRoomResponse"
    "fixture/DeleteRoomResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRoom)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice = res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDevice)

responseGetGateway :: GetGatewayResponse -> TestTree
responseGetGateway = res
    "GetGatewayResponse"
    "fixture/GetGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGateway)

responseListSkillsStoreSkillsByCategory :: ListSkillsStoreSkillsByCategoryResponse -> TestTree
responseListSkillsStoreSkillsByCategory = res
    "ListSkillsStoreSkillsByCategoryResponse"
    "fixture/ListSkillsStoreSkillsByCategoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSkillsStoreSkillsByCategory)

responseDeleteConferenceProvider :: DeleteConferenceProviderResponse -> TestTree
responseDeleteConferenceProvider = res
    "DeleteConferenceProviderResponse"
    "fixture/DeleteConferenceProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteConferenceProvider)

responseUpdateConferenceProvider :: UpdateConferenceProviderResponse -> TestTree
responseUpdateConferenceProvider = res
    "UpdateConferenceProviderResponse"
    "fixture/UpdateConferenceProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateConferenceProvider)

responseGetContact :: GetContactResponse -> TestTree
responseGetContact = res
    "GetContactResponse"
    "fixture/GetContactResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContact)

responseApproveSkill :: ApproveSkillResponse -> TestTree
responseApproveSkill = res
    "ApproveSkillResponse"
    "fixture/ApproveSkillResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ApproveSkill)

responseCreateNetworkProfile :: CreateNetworkProfileResponse -> TestTree
responseCreateNetworkProfile = res
    "CreateNetworkProfileResponse"
    "fixture/CreateNetworkProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateNetworkProfile)

responseAssociateDeviceWithRoom :: AssociateDeviceWithRoomResponse -> TestTree
responseAssociateDeviceWithRoom = res
    "AssociateDeviceWithRoomResponse"
    "fixture/AssociateDeviceWithRoomResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateDeviceWithRoom)

responseGetRoomSkillParameter :: GetRoomSkillParameterResponse -> TestTree
responseGetRoomSkillParameter = res
    "GetRoomSkillParameterResponse"
    "fixture/GetRoomSkillParameterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRoomSkillParameter)

responseUpdateGateway :: UpdateGatewayResponse -> TestTree
responseUpdateGateway = res
    "UpdateGatewayResponse"
    "fixture/UpdateGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGateway)

responseCreateBusinessReportSchedule :: CreateBusinessReportScheduleResponse -> TestTree
responseCreateBusinessReportSchedule = res
    "CreateBusinessReportScheduleResponse"
    "fixture/CreateBusinessReportScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBusinessReportSchedule)

responseDeleteContact :: DeleteContactResponse -> TestTree
responseDeleteContact = res
    "DeleteContactResponse"
    "fixture/DeleteContactResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteContact)

responseUpdateContact :: UpdateContactResponse -> TestTree
responseUpdateContact = res
    "UpdateContactResponse"
    "fixture/UpdateContactResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateContact)

responseGetAddressBook :: GetAddressBookResponse -> TestTree
responseGetAddressBook = res
    "GetAddressBookResponse"
    "fixture/GetAddressBookResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAddressBook)

responseListBusinessReportSchedules :: ListBusinessReportSchedulesResponse -> TestTree
responseListBusinessReportSchedules = res
    "ListBusinessReportSchedulesResponse"
    "fixture/ListBusinessReportSchedulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBusinessReportSchedules)

responseDeleteDeviceUsageData :: DeleteDeviceUsageDataResponse -> TestTree
responseDeleteDeviceUsageData = res
    "DeleteDeviceUsageDataResponse"
    "fixture/DeleteDeviceUsageDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDeviceUsageData)

responseCreateContact :: CreateContactResponse -> TestTree
responseCreateContact = res
    "CreateContactResponse"
    "fixture/CreateContactResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateContact)

responseCreateProfile :: CreateProfileResponse -> TestTree
responseCreateProfile = res
    "CreateProfileResponse"
    "fixture/CreateProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateProfile)

responseDeleteSkillGroup :: DeleteSkillGroupResponse -> TestTree
responseDeleteSkillGroup = res
    "DeleteSkillGroupResponse"
    "fixture/DeleteSkillGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSkillGroup)

responseUpdateSkillGroup :: UpdateSkillGroupResponse -> TestTree
responseUpdateSkillGroup = res
    "UpdateSkillGroupResponse"
    "fixture/UpdateSkillGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSkillGroup)

responseStartDeviceSync :: StartDeviceSyncResponse -> TestTree
responseStartDeviceSync = res
    "StartDeviceSyncResponse"
    "fixture/StartDeviceSyncResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartDeviceSync)

responseGetInvitationConfiguration :: GetInvitationConfigurationResponse -> TestTree
responseGetInvitationConfiguration = res
    "GetInvitationConfigurationResponse"
    "fixture/GetInvitationConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInvitationConfiguration)

responseDisassociateSkillFromUsers :: DisassociateSkillFromUsersResponse -> TestTree
responseDisassociateSkillFromUsers = res
    "DisassociateSkillFromUsersResponse"
    "fixture/DisassociateSkillFromUsersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateSkillFromUsers)

responseSearchAddressBooks :: SearchAddressBooksResponse -> TestTree
responseSearchAddressBooks = res
    "SearchAddressBooksResponse"
    "fixture/SearchAddressBooksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchAddressBooks)

responseCreateSkillGroup :: CreateSkillGroupResponse -> TestTree
responseCreateSkillGroup = res
    "CreateSkillGroupResponse"
    "fixture/CreateSkillGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSkillGroup)

responseGetProfile :: GetProfileResponse -> TestTree
responseGetProfile = res
    "GetProfileResponse"
    "fixture/GetProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetProfile)

responseDisassociateSkillGroupFromRoom :: DisassociateSkillGroupFromRoomResponse -> TestTree
responseDisassociateSkillGroupFromRoom = res
    "DisassociateSkillGroupFromRoomResponse"
    "fixture/DisassociateSkillGroupFromRoomResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateSkillGroupFromRoom)

responseSendInvitation :: SendInvitationResponse -> TestTree
responseSendInvitation = res
    "SendInvitationResponse"
    "fixture/SendInvitationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendInvitation)

responseListDeviceEvents :: ListDeviceEventsResponse -> TestTree
responseListDeviceEvents = res
    "ListDeviceEventsResponse"
    "fixture/ListDeviceEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDeviceEvents)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUser)

responseSearchDevices :: SearchDevicesResponse -> TestTree
responseSearchDevices = res
    "SearchDevicesResponse"
    "fixture/SearchDevicesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchDevices)

responseSearchContacts :: SearchContactsResponse -> TestTree
responseSearchContacts = res
    "SearchContactsResponse"
    "fixture/SearchContactsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchContacts)

responseSendAnnouncement :: SendAnnouncementResponse -> TestTree
responseSendAnnouncement = res
    "SendAnnouncementResponse"
    "fixture/SendAnnouncementResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendAnnouncement)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUser)

responseSearchNetworkProfiles :: SearchNetworkProfilesResponse -> TestTree
responseSearchNetworkProfiles = res
    "SearchNetworkProfilesResponse"
    "fixture/SearchNetworkProfilesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchNetworkProfiles)

responseGetSkillGroup :: GetSkillGroupResponse -> TestTree
responseGetSkillGroup = res
    "GetSkillGroupResponse"
    "fixture/GetSkillGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSkillGroup)

responseListSkills :: ListSkillsResponse -> TestTree
responseListSkills = res
    "ListSkillsResponse"
    "fixture/ListSkillsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSkills)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseDisassociateDeviceFromRoom :: DisassociateDeviceFromRoomResponse -> TestTree
responseDisassociateDeviceFromRoom = res
    "DisassociateDeviceFromRoomResponse"
    "fixture/DisassociateDeviceFromRoomResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateDeviceFromRoom)

responseSearchSkillGroups :: SearchSkillGroupsResponse -> TestTree
responseSearchSkillGroups = res
    "SearchSkillGroupsResponse"
    "fixture/SearchSkillGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchSkillGroups)

responsePutSkillAuthorization :: PutSkillAuthorizationResponse -> TestTree
responsePutSkillAuthorization = res
    "PutSkillAuthorizationResponse"
    "fixture/PutSkillAuthorizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutSkillAuthorization)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTags)

responseDeleteSkillAuthorization :: DeleteSkillAuthorizationResponse -> TestTree
responseDeleteSkillAuthorization = res
    "DeleteSkillAuthorizationResponse"
    "fixture/DeleteSkillAuthorizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSkillAuthorization)

responseAssociateDeviceWithNetworkProfile :: AssociateDeviceWithNetworkProfileResponse -> TestTree
responseAssociateDeviceWithNetworkProfile = res
    "AssociateDeviceWithNetworkProfileResponse"
    "fixture/AssociateDeviceWithNetworkProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateDeviceWithNetworkProfile)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseCreateConferenceProvider :: CreateConferenceProviderResponse -> TestTree
responseCreateConferenceProvider = res
    "CreateConferenceProviderResponse"
    "fixture/CreateConferenceProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateConferenceProvider)

responseResolveRoom :: ResolveRoomResponse -> TestTree
responseResolveRoom = res
    "ResolveRoomResponse"
    "fixture/ResolveRoomResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResolveRoom)

responseCreateGatewayGroup :: CreateGatewayGroupResponse -> TestTree
responseCreateGatewayGroup = res
    "CreateGatewayGroupResponse"
    "fixture/CreateGatewayGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGatewayGroup)

responseCreateRoom :: CreateRoomResponse -> TestTree
responseCreateRoom = res
    "CreateRoomResponse"
    "fixture/CreateRoomResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRoom)

responseDeleteRoomSkillParameter :: DeleteRoomSkillParameterResponse -> TestTree
responseDeleteRoomSkillParameter = res
    "DeleteRoomSkillParameterResponse"
    "fixture/DeleteRoomSkillParameterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRoomSkillParameter)

responseListGatewayGroups :: ListGatewayGroupsResponse -> TestTree
responseListGatewayGroups = res
    "ListGatewayGroupsResponse"
    "fixture/ListGatewayGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGatewayGroups)

responsePutRoomSkillParameter :: PutRoomSkillParameterResponse -> TestTree
responsePutRoomSkillParameter = res
    "PutRoomSkillParameterResponse"
    "fixture/PutRoomSkillParameterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutRoomSkillParameter)

responseSearchProfiles :: SearchProfilesResponse -> TestTree
responseSearchProfiles = res
    "SearchProfilesResponse"
    "fixture/SearchProfilesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchProfiles)

responseRejectSkill :: RejectSkillResponse -> TestTree
responseRejectSkill = res
    "RejectSkillResponse"
    "fixture/RejectSkillResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RejectSkill)

responseListConferenceProviders :: ListConferenceProvidersResponse -> TestTree
responseListConferenceProviders = res
    "ListConferenceProvidersResponse"
    "fixture/ListConferenceProvidersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListConferenceProviders)

responseRevokeInvitation :: RevokeInvitationResponse -> TestTree
responseRevokeInvitation = res
    "RevokeInvitationResponse"
    "fixture/RevokeInvitationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RevokeInvitation)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways = res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGateways)

responseDeleteDevice :: DeleteDeviceResponse -> TestTree
responseDeleteDevice = res
    "DeleteDeviceResponse"
    "fixture/DeleteDeviceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDevice)

responseUpdateDevice :: UpdateDeviceResponse -> TestTree
responseUpdateDevice = res
    "UpdateDeviceResponse"
    "fixture/UpdateDeviceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDevice)

responseAssociateSkillWithSkillGroup :: AssociateSkillWithSkillGroupResponse -> TestTree
responseAssociateSkillWithSkillGroup = res
    "AssociateSkillWithSkillGroupResponse"
    "fixture/AssociateSkillWithSkillGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateSkillWithSkillGroup)

responseGetConferenceProvider :: GetConferenceProviderResponse -> TestTree
responseGetConferenceProvider = res
    "GetConferenceProviderResponse"
    "fixture/GetConferenceProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetConferenceProvider)

responseGetRoom :: GetRoomResponse -> TestTree
responseGetRoom = res
    "GetRoomResponse"
    "fixture/GetRoomResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRoom)

responseGetGatewayGroup :: GetGatewayGroupResponse -> TestTree
responseGetGatewayGroup = res
    "GetGatewayGroupResponse"
    "fixture/GetGatewayGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGatewayGroup)

responseListSkillsStoreCategories :: ListSkillsStoreCategoriesResponse -> TestTree
responseListSkillsStoreCategories = res
    "ListSkillsStoreCategoriesResponse"
    "fixture/ListSkillsStoreCategoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSkillsStoreCategories)

responseStartSmartHomeApplianceDiscovery :: StartSmartHomeApplianceDiscoveryResponse -> TestTree
responseStartSmartHomeApplianceDiscovery = res
    "StartSmartHomeApplianceDiscoveryResponse"
    "fixture/StartSmartHomeApplianceDiscoveryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartSmartHomeApplianceDiscovery)

responseAssociateContactWithAddressBook :: AssociateContactWithAddressBookResponse -> TestTree
responseAssociateContactWithAddressBook = res
    "AssociateContactWithAddressBookResponse"
    "fixture/AssociateContactWithAddressBookResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateContactWithAddressBook)
