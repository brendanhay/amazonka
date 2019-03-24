{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AlexaBusiness
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestSearchUsers $
--             searchUsers
--
--         , requestPutConferencePreference $
--             putConferencePreference
--
--         , requestUpdateBusinessReportSchedule $
--             updateBusinessReportSchedule
--
--         , requestDeleteBusinessReportSchedule $
--             deleteBusinessReportSchedule
--
--         , requestAssociateSkillGroupWithRoom $
--             associateSkillGroupWithRoom
--
--         , requestListSmartHomeAppliances $
--             listSmartHomeAppliances
--
--         , requestDeleteProfile $
--             deleteProfile
--
--         , requestUpdateProfile $
--             updateProfile
--
--         , requestSearchRooms $
--             searchRooms
--
--         , requestAssociateSkillWithUsers $
--             associateSkillWithUsers
--
--         , requestRegisterAVSDevice $
--             registerAVSDevice
--
--         , requestForgetSmartHomeAppliances $
--             forgetSmartHomeAppliances
--
--         , requestPutInvitationConfiguration $
--             putInvitationConfiguration
--
--         , requestDisassociateContactFromAddressBook $
--             disassociateContactFromAddressBook
--
--         , requestGetConferencePreference $
--             getConferencePreference
--
--         , requestDisassociateSkillFromSkillGroup $
--             disassociateSkillFromSkillGroup
--
--         , requestCreateAddressBook $
--             createAddressBook
--
--         , requestDeleteAddressBook $
--             deleteAddressBook
--
--         , requestUpdateAddressBook $
--             updateAddressBook
--
--         , requestUpdateRoom $
--             updateRoom
--
--         , requestDeleteRoom $
--             deleteRoom
--
--         , requestGetDevice $
--             getDevice
--
--         , requestListSkillsStoreSkillsByCategory $
--             listSkillsStoreSkillsByCategory
--
--         , requestDeleteConferenceProvider $
--             deleteConferenceProvider
--
--         , requestUpdateConferenceProvider $
--             updateConferenceProvider
--
--         , requestGetContact $
--             getContact
--
--         , requestApproveSkill $
--             approveSkill
--
--         , requestAssociateDeviceWithRoom $
--             associateDeviceWithRoom
--
--         , requestGetRoomSkillParameter $
--             getRoomSkillParameter
--
--         , requestCreateBusinessReportSchedule $
--             createBusinessReportSchedule
--
--         , requestDeleteContact $
--             deleteContact
--
--         , requestUpdateContact $
--             updateContact
--
--         , requestGetAddressBook $
--             getAddressBook
--
--         , requestListBusinessReportSchedules $
--             listBusinessReportSchedules
--
--         , requestCreateContact $
--             createContact
--
--         , requestCreateProfile $
--             createProfile
--
--         , requestDeleteSkillGroup $
--             deleteSkillGroup
--
--         , requestUpdateSkillGroup $
--             updateSkillGroup
--
--         , requestStartDeviceSync $
--             startDeviceSync
--
--         , requestGetInvitationConfiguration $
--             getInvitationConfiguration
--
--         , requestDisassociateSkillFromUsers $
--             disassociateSkillFromUsers
--
--         , requestSearchAddressBooks $
--             searchAddressBooks
--
--         , requestCreateSkillGroup $
--             createSkillGroup
--
--         , requestGetProfile $
--             getProfile
--
--         , requestDisassociateSkillGroupFromRoom $
--             disassociateSkillGroupFromRoom
--
--         , requestSendInvitation $
--             sendInvitation
--
--         , requestListDeviceEvents $
--             listDeviceEvents
--
--         , requestCreateUser $
--             createUser
--
--         , requestSearchDevices $
--             searchDevices
--
--         , requestSearchContacts $
--             searchContacts
--
--         , requestDeleteUser $
--             deleteUser
--
--         , requestGetSkillGroup $
--             getSkillGroup
--
--         , requestListSkills $
--             listSkills
--
--         , requestTagResource $
--             tagResource
--
--         , requestDisassociateDeviceFromRoom $
--             disassociateDeviceFromRoom
--
--         , requestSearchSkillGroups $
--             searchSkillGroups
--
--         , requestPutSkillAuthorization $
--             putSkillAuthorization
--
--         , requestListTags $
--             listTags
--
--         , requestDeleteSkillAuthorization $
--             deleteSkillAuthorization
--
--         , requestUntagResource $
--             untagResource
--
--         , requestCreateConferenceProvider $
--             createConferenceProvider
--
--         , requestResolveRoom $
--             resolveRoom
--
--         , requestCreateRoom $
--             createRoom
--
--         , requestDeleteRoomSkillParameter $
--             deleteRoomSkillParameter
--
--         , requestPutRoomSkillParameter $
--             putRoomSkillParameter
--
--         , requestSearchProfiles $
--             searchProfiles
--
--         , requestRejectSkill $
--             rejectSkill
--
--         , requestListConferenceProviders $
--             listConferenceProviders
--
--         , requestRevokeInvitation $
--             revokeInvitation
--
--         , requestDeleteDevice $
--             deleteDevice
--
--         , requestUpdateDevice $
--             updateDevice
--
--         , requestAssociateSkillWithSkillGroup $
--             associateSkillWithSkillGroup
--
--         , requestGetConferenceProvider $
--             getConferenceProvider
--
--         , requestGetRoom $
--             getRoom
--
--         , requestListSkillsStoreCategories $
--             listSkillsStoreCategories
--
--         , requestStartSmartHomeApplianceDiscovery $
--             startSmartHomeApplianceDiscovery
--
--         , requestAssociateContactWithAddressBook $
--             associateContactWithAddressBook
--
--           ]

--     , testGroup "response"
--         [ responseSearchUsers $
--             searchUsersResponse
--
--         , responsePutConferencePreference $
--             putConferencePreferenceResponse
--
--         , responseUpdateBusinessReportSchedule $
--             updateBusinessReportScheduleResponse
--
--         , responseDeleteBusinessReportSchedule $
--             deleteBusinessReportScheduleResponse
--
--         , responseAssociateSkillGroupWithRoom $
--             associateSkillGroupWithRoomResponse
--
--         , responseListSmartHomeAppliances $
--             listSmartHomeAppliancesResponse
--
--         , responseDeleteProfile $
--             deleteProfileResponse
--
--         , responseUpdateProfile $
--             updateProfileResponse
--
--         , responseSearchRooms $
--             searchRoomsResponse
--
--         , responseAssociateSkillWithUsers $
--             associateSkillWithUsersResponse
--
--         , responseRegisterAVSDevice $
--             registerAVSDeviceResponse
--
--         , responseForgetSmartHomeAppliances $
--             forgetSmartHomeAppliancesResponse
--
--         , responsePutInvitationConfiguration $
--             putInvitationConfigurationResponse
--
--         , responseDisassociateContactFromAddressBook $
--             disassociateContactFromAddressBookResponse
--
--         , responseGetConferencePreference $
--             getConferencePreferenceResponse
--
--         , responseDisassociateSkillFromSkillGroup $
--             disassociateSkillFromSkillGroupResponse
--
--         , responseCreateAddressBook $
--             createAddressBookResponse
--
--         , responseDeleteAddressBook $
--             deleteAddressBookResponse
--
--         , responseUpdateAddressBook $
--             updateAddressBookResponse
--
--         , responseUpdateRoom $
--             updateRoomResponse
--
--         , responseDeleteRoom $
--             deleteRoomResponse
--
--         , responseGetDevice $
--             getDeviceResponse
--
--         , responseListSkillsStoreSkillsByCategory $
--             listSkillsStoreSkillsByCategoryResponse
--
--         , responseDeleteConferenceProvider $
--             deleteConferenceProviderResponse
--
--         , responseUpdateConferenceProvider $
--             updateConferenceProviderResponse
--
--         , responseGetContact $
--             getContactResponse
--
--         , responseApproveSkill $
--             approveSkillResponse
--
--         , responseAssociateDeviceWithRoom $
--             associateDeviceWithRoomResponse
--
--         , responseGetRoomSkillParameter $
--             getRoomSkillParameterResponse
--
--         , responseCreateBusinessReportSchedule $
--             createBusinessReportScheduleResponse
--
--         , responseDeleteContact $
--             deleteContactResponse
--
--         , responseUpdateContact $
--             updateContactResponse
--
--         , responseGetAddressBook $
--             getAddressBookResponse
--
--         , responseListBusinessReportSchedules $
--             listBusinessReportSchedulesResponse
--
--         , responseCreateContact $
--             createContactResponse
--
--         , responseCreateProfile $
--             createProfileResponse
--
--         , responseDeleteSkillGroup $
--             deleteSkillGroupResponse
--
--         , responseUpdateSkillGroup $
--             updateSkillGroupResponse
--
--         , responseStartDeviceSync $
--             startDeviceSyncResponse
--
--         , responseGetInvitationConfiguration $
--             getInvitationConfigurationResponse
--
--         , responseDisassociateSkillFromUsers $
--             disassociateSkillFromUsersResponse
--
--         , responseSearchAddressBooks $
--             searchAddressBooksResponse
--
--         , responseCreateSkillGroup $
--             createSkillGroupResponse
--
--         , responseGetProfile $
--             getProfileResponse
--
--         , responseDisassociateSkillGroupFromRoom $
--             disassociateSkillGroupFromRoomResponse
--
--         , responseSendInvitation $
--             sendInvitationResponse
--
--         , responseListDeviceEvents $
--             listDeviceEventsResponse
--
--         , responseCreateUser $
--             createUserResponse
--
--         , responseSearchDevices $
--             searchDevicesResponse
--
--         , responseSearchContacts $
--             searchContactsResponse
--
--         , responseDeleteUser $
--             deleteUserResponse
--
--         , responseGetSkillGroup $
--             getSkillGroupResponse
--
--         , responseListSkills $
--             listSkillsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseDisassociateDeviceFromRoom $
--             disassociateDeviceFromRoomResponse
--
--         , responseSearchSkillGroups $
--             searchSkillGroupsResponse
--
--         , responsePutSkillAuthorization $
--             putSkillAuthorizationResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseDeleteSkillAuthorization $
--             deleteSkillAuthorizationResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseCreateConferenceProvider $
--             createConferenceProviderResponse
--
--         , responseResolveRoom $
--             resolveRoomResponse
--
--         , responseCreateRoom $
--             createRoomResponse
--
--         , responseDeleteRoomSkillParameter $
--             deleteRoomSkillParameterResponse
--
--         , responsePutRoomSkillParameter $
--             putRoomSkillParameterResponse
--
--         , responseSearchProfiles $
--             searchProfilesResponse
--
--         , responseRejectSkill $
--             rejectSkillResponse
--
--         , responseListConferenceProviders $
--             listConferenceProvidersResponse
--
--         , responseRevokeInvitation $
--             revokeInvitationResponse
--
--         , responseDeleteDevice $
--             deleteDeviceResponse
--
--         , responseUpdateDevice $
--             updateDeviceResponse
--
--         , responseAssociateSkillWithSkillGroup $
--             associateSkillWithSkillGroupResponse
--
--         , responseGetConferenceProvider $
--             getConferenceProviderResponse
--
--         , responseGetRoom $
--             getRoomResponse
--
--         , responseListSkillsStoreCategories $
--             listSkillsStoreCategoriesResponse
--
--         , responseStartSmartHomeApplianceDiscovery $
--             startSmartHomeApplianceDiscoveryResponse
--
--         , responseAssociateContactWithAddressBook $
--             associateContactWithAddressBookResponse
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

requestAssociateDeviceWithRoom :: AssociateDeviceWithRoom -> TestTree
requestAssociateDeviceWithRoom = req
    "AssociateDeviceWithRoom"
    "fixture/AssociateDeviceWithRoom.yaml"

requestGetRoomSkillParameter :: GetRoomSkillParameter -> TestTree
requestGetRoomSkillParameter = req
    "GetRoomSkillParameter"
    "fixture/GetRoomSkillParameter.yaml"

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

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

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

requestCreateRoom :: CreateRoom -> TestTree
requestCreateRoom = req
    "CreateRoom"
    "fixture/CreateRoom.yaml"

requestDeleteRoomSkillParameter :: DeleteRoomSkillParameter -> TestTree
requestDeleteRoomSkillParameter = req
    "DeleteRoomSkillParameter"
    "fixture/DeleteRoomSkillParameter.yaml"

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
    alexaBusiness
    (Proxy :: Proxy SearchUsers)

responsePutConferencePreference :: PutConferencePreferenceResponse -> TestTree
responsePutConferencePreference = res
    "PutConferencePreferenceResponse"
    "fixture/PutConferencePreferenceResponse.proto"
    alexaBusiness
    (Proxy :: Proxy PutConferencePreference)

responseUpdateBusinessReportSchedule :: UpdateBusinessReportScheduleResponse -> TestTree
responseUpdateBusinessReportSchedule = res
    "UpdateBusinessReportScheduleResponse"
    "fixture/UpdateBusinessReportScheduleResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UpdateBusinessReportSchedule)

responseDeleteBusinessReportSchedule :: DeleteBusinessReportScheduleResponse -> TestTree
responseDeleteBusinessReportSchedule = res
    "DeleteBusinessReportScheduleResponse"
    "fixture/DeleteBusinessReportScheduleResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteBusinessReportSchedule)

responseAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoomResponse -> TestTree
responseAssociateSkillGroupWithRoom = res
    "AssociateSkillGroupWithRoomResponse"
    "fixture/AssociateSkillGroupWithRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy AssociateSkillGroupWithRoom)

responseListSmartHomeAppliances :: ListSmartHomeAppliancesResponse -> TestTree
responseListSmartHomeAppliances = res
    "ListSmartHomeAppliancesResponse"
    "fixture/ListSmartHomeAppliancesResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ListSmartHomeAppliances)

responseDeleteProfile :: DeleteProfileResponse -> TestTree
responseDeleteProfile = res
    "DeleteProfileResponse"
    "fixture/DeleteProfileResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteProfile)

responseUpdateProfile :: UpdateProfileResponse -> TestTree
responseUpdateProfile = res
    "UpdateProfileResponse"
    "fixture/UpdateProfileResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UpdateProfile)

responseSearchRooms :: SearchRoomsResponse -> TestTree
responseSearchRooms = res
    "SearchRoomsResponse"
    "fixture/SearchRoomsResponse.proto"
    alexaBusiness
    (Proxy :: Proxy SearchRooms)

responseAssociateSkillWithUsers :: AssociateSkillWithUsersResponse -> TestTree
responseAssociateSkillWithUsers = res
    "AssociateSkillWithUsersResponse"
    "fixture/AssociateSkillWithUsersResponse.proto"
    alexaBusiness
    (Proxy :: Proxy AssociateSkillWithUsers)

responseRegisterAVSDevice :: RegisterAVSDeviceResponse -> TestTree
responseRegisterAVSDevice = res
    "RegisterAVSDeviceResponse"
    "fixture/RegisterAVSDeviceResponse.proto"
    alexaBusiness
    (Proxy :: Proxy RegisterAVSDevice)

responseForgetSmartHomeAppliances :: ForgetSmartHomeAppliancesResponse -> TestTree
responseForgetSmartHomeAppliances = res
    "ForgetSmartHomeAppliancesResponse"
    "fixture/ForgetSmartHomeAppliancesResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ForgetSmartHomeAppliances)

responsePutInvitationConfiguration :: PutInvitationConfigurationResponse -> TestTree
responsePutInvitationConfiguration = res
    "PutInvitationConfigurationResponse"
    "fixture/PutInvitationConfigurationResponse.proto"
    alexaBusiness
    (Proxy :: Proxy PutInvitationConfiguration)

responseDisassociateContactFromAddressBook :: DisassociateContactFromAddressBookResponse -> TestTree
responseDisassociateContactFromAddressBook = res
    "DisassociateContactFromAddressBookResponse"
    "fixture/DisassociateContactFromAddressBookResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DisassociateContactFromAddressBook)

responseGetConferencePreference :: GetConferencePreferenceResponse -> TestTree
responseGetConferencePreference = res
    "GetConferencePreferenceResponse"
    "fixture/GetConferencePreferenceResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetConferencePreference)

responseDisassociateSkillFromSkillGroup :: DisassociateSkillFromSkillGroupResponse -> TestTree
responseDisassociateSkillFromSkillGroup = res
    "DisassociateSkillFromSkillGroupResponse"
    "fixture/DisassociateSkillFromSkillGroupResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DisassociateSkillFromSkillGroup)

responseCreateAddressBook :: CreateAddressBookResponse -> TestTree
responseCreateAddressBook = res
    "CreateAddressBookResponse"
    "fixture/CreateAddressBookResponse.proto"
    alexaBusiness
    (Proxy :: Proxy CreateAddressBook)

responseDeleteAddressBook :: DeleteAddressBookResponse -> TestTree
responseDeleteAddressBook = res
    "DeleteAddressBookResponse"
    "fixture/DeleteAddressBookResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteAddressBook)

responseUpdateAddressBook :: UpdateAddressBookResponse -> TestTree
responseUpdateAddressBook = res
    "UpdateAddressBookResponse"
    "fixture/UpdateAddressBookResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UpdateAddressBook)

responseUpdateRoom :: UpdateRoomResponse -> TestTree
responseUpdateRoom = res
    "UpdateRoomResponse"
    "fixture/UpdateRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UpdateRoom)

responseDeleteRoom :: DeleteRoomResponse -> TestTree
responseDeleteRoom = res
    "DeleteRoomResponse"
    "fixture/DeleteRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteRoom)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice = res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetDevice)

responseListSkillsStoreSkillsByCategory :: ListSkillsStoreSkillsByCategoryResponse -> TestTree
responseListSkillsStoreSkillsByCategory = res
    "ListSkillsStoreSkillsByCategoryResponse"
    "fixture/ListSkillsStoreSkillsByCategoryResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ListSkillsStoreSkillsByCategory)

responseDeleteConferenceProvider :: DeleteConferenceProviderResponse -> TestTree
responseDeleteConferenceProvider = res
    "DeleteConferenceProviderResponse"
    "fixture/DeleteConferenceProviderResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteConferenceProvider)

responseUpdateConferenceProvider :: UpdateConferenceProviderResponse -> TestTree
responseUpdateConferenceProvider = res
    "UpdateConferenceProviderResponse"
    "fixture/UpdateConferenceProviderResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UpdateConferenceProvider)

responseGetContact :: GetContactResponse -> TestTree
responseGetContact = res
    "GetContactResponse"
    "fixture/GetContactResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetContact)

responseApproveSkill :: ApproveSkillResponse -> TestTree
responseApproveSkill = res
    "ApproveSkillResponse"
    "fixture/ApproveSkillResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ApproveSkill)

responseAssociateDeviceWithRoom :: AssociateDeviceWithRoomResponse -> TestTree
responseAssociateDeviceWithRoom = res
    "AssociateDeviceWithRoomResponse"
    "fixture/AssociateDeviceWithRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy AssociateDeviceWithRoom)

responseGetRoomSkillParameter :: GetRoomSkillParameterResponse -> TestTree
responseGetRoomSkillParameter = res
    "GetRoomSkillParameterResponse"
    "fixture/GetRoomSkillParameterResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetRoomSkillParameter)

responseCreateBusinessReportSchedule :: CreateBusinessReportScheduleResponse -> TestTree
responseCreateBusinessReportSchedule = res
    "CreateBusinessReportScheduleResponse"
    "fixture/CreateBusinessReportScheduleResponse.proto"
    alexaBusiness
    (Proxy :: Proxy CreateBusinessReportSchedule)

responseDeleteContact :: DeleteContactResponse -> TestTree
responseDeleteContact = res
    "DeleteContactResponse"
    "fixture/DeleteContactResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteContact)

responseUpdateContact :: UpdateContactResponse -> TestTree
responseUpdateContact = res
    "UpdateContactResponse"
    "fixture/UpdateContactResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UpdateContact)

responseGetAddressBook :: GetAddressBookResponse -> TestTree
responseGetAddressBook = res
    "GetAddressBookResponse"
    "fixture/GetAddressBookResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetAddressBook)

responseListBusinessReportSchedules :: ListBusinessReportSchedulesResponse -> TestTree
responseListBusinessReportSchedules = res
    "ListBusinessReportSchedulesResponse"
    "fixture/ListBusinessReportSchedulesResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ListBusinessReportSchedules)

responseCreateContact :: CreateContactResponse -> TestTree
responseCreateContact = res
    "CreateContactResponse"
    "fixture/CreateContactResponse.proto"
    alexaBusiness
    (Proxy :: Proxy CreateContact)

responseCreateProfile :: CreateProfileResponse -> TestTree
responseCreateProfile = res
    "CreateProfileResponse"
    "fixture/CreateProfileResponse.proto"
    alexaBusiness
    (Proxy :: Proxy CreateProfile)

responseDeleteSkillGroup :: DeleteSkillGroupResponse -> TestTree
responseDeleteSkillGroup = res
    "DeleteSkillGroupResponse"
    "fixture/DeleteSkillGroupResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteSkillGroup)

responseUpdateSkillGroup :: UpdateSkillGroupResponse -> TestTree
responseUpdateSkillGroup = res
    "UpdateSkillGroupResponse"
    "fixture/UpdateSkillGroupResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UpdateSkillGroup)

responseStartDeviceSync :: StartDeviceSyncResponse -> TestTree
responseStartDeviceSync = res
    "StartDeviceSyncResponse"
    "fixture/StartDeviceSyncResponse.proto"
    alexaBusiness
    (Proxy :: Proxy StartDeviceSync)

responseGetInvitationConfiguration :: GetInvitationConfigurationResponse -> TestTree
responseGetInvitationConfiguration = res
    "GetInvitationConfigurationResponse"
    "fixture/GetInvitationConfigurationResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetInvitationConfiguration)

responseDisassociateSkillFromUsers :: DisassociateSkillFromUsersResponse -> TestTree
responseDisassociateSkillFromUsers = res
    "DisassociateSkillFromUsersResponse"
    "fixture/DisassociateSkillFromUsersResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DisassociateSkillFromUsers)

responseSearchAddressBooks :: SearchAddressBooksResponse -> TestTree
responseSearchAddressBooks = res
    "SearchAddressBooksResponse"
    "fixture/SearchAddressBooksResponse.proto"
    alexaBusiness
    (Proxy :: Proxy SearchAddressBooks)

responseCreateSkillGroup :: CreateSkillGroupResponse -> TestTree
responseCreateSkillGroup = res
    "CreateSkillGroupResponse"
    "fixture/CreateSkillGroupResponse.proto"
    alexaBusiness
    (Proxy :: Proxy CreateSkillGroup)

responseGetProfile :: GetProfileResponse -> TestTree
responseGetProfile = res
    "GetProfileResponse"
    "fixture/GetProfileResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetProfile)

responseDisassociateSkillGroupFromRoom :: DisassociateSkillGroupFromRoomResponse -> TestTree
responseDisassociateSkillGroupFromRoom = res
    "DisassociateSkillGroupFromRoomResponse"
    "fixture/DisassociateSkillGroupFromRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DisassociateSkillGroupFromRoom)

responseSendInvitation :: SendInvitationResponse -> TestTree
responseSendInvitation = res
    "SendInvitationResponse"
    "fixture/SendInvitationResponse.proto"
    alexaBusiness
    (Proxy :: Proxy SendInvitation)

responseListDeviceEvents :: ListDeviceEventsResponse -> TestTree
responseListDeviceEvents = res
    "ListDeviceEventsResponse"
    "fixture/ListDeviceEventsResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ListDeviceEvents)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    alexaBusiness
    (Proxy :: Proxy CreateUser)

responseSearchDevices :: SearchDevicesResponse -> TestTree
responseSearchDevices = res
    "SearchDevicesResponse"
    "fixture/SearchDevicesResponse.proto"
    alexaBusiness
    (Proxy :: Proxy SearchDevices)

responseSearchContacts :: SearchContactsResponse -> TestTree
responseSearchContacts = res
    "SearchContactsResponse"
    "fixture/SearchContactsResponse.proto"
    alexaBusiness
    (Proxy :: Proxy SearchContacts)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteUser)

responseGetSkillGroup :: GetSkillGroupResponse -> TestTree
responseGetSkillGroup = res
    "GetSkillGroupResponse"
    "fixture/GetSkillGroupResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetSkillGroup)

responseListSkills :: ListSkillsResponse -> TestTree
responseListSkills = res
    "ListSkillsResponse"
    "fixture/ListSkillsResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ListSkills)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    alexaBusiness
    (Proxy :: Proxy TagResource)

responseDisassociateDeviceFromRoom :: DisassociateDeviceFromRoomResponse -> TestTree
responseDisassociateDeviceFromRoom = res
    "DisassociateDeviceFromRoomResponse"
    "fixture/DisassociateDeviceFromRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DisassociateDeviceFromRoom)

responseSearchSkillGroups :: SearchSkillGroupsResponse -> TestTree
responseSearchSkillGroups = res
    "SearchSkillGroupsResponse"
    "fixture/SearchSkillGroupsResponse.proto"
    alexaBusiness
    (Proxy :: Proxy SearchSkillGroups)

responsePutSkillAuthorization :: PutSkillAuthorizationResponse -> TestTree
responsePutSkillAuthorization = res
    "PutSkillAuthorizationResponse"
    "fixture/PutSkillAuthorizationResponse.proto"
    alexaBusiness
    (Proxy :: Proxy PutSkillAuthorization)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ListTags)

responseDeleteSkillAuthorization :: DeleteSkillAuthorizationResponse -> TestTree
responseDeleteSkillAuthorization = res
    "DeleteSkillAuthorizationResponse"
    "fixture/DeleteSkillAuthorizationResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteSkillAuthorization)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UntagResource)

responseCreateConferenceProvider :: CreateConferenceProviderResponse -> TestTree
responseCreateConferenceProvider = res
    "CreateConferenceProviderResponse"
    "fixture/CreateConferenceProviderResponse.proto"
    alexaBusiness
    (Proxy :: Proxy CreateConferenceProvider)

responseResolveRoom :: ResolveRoomResponse -> TestTree
responseResolveRoom = res
    "ResolveRoomResponse"
    "fixture/ResolveRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ResolveRoom)

responseCreateRoom :: CreateRoomResponse -> TestTree
responseCreateRoom = res
    "CreateRoomResponse"
    "fixture/CreateRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy CreateRoom)

responseDeleteRoomSkillParameter :: DeleteRoomSkillParameterResponse -> TestTree
responseDeleteRoomSkillParameter = res
    "DeleteRoomSkillParameterResponse"
    "fixture/DeleteRoomSkillParameterResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteRoomSkillParameter)

responsePutRoomSkillParameter :: PutRoomSkillParameterResponse -> TestTree
responsePutRoomSkillParameter = res
    "PutRoomSkillParameterResponse"
    "fixture/PutRoomSkillParameterResponse.proto"
    alexaBusiness
    (Proxy :: Proxy PutRoomSkillParameter)

responseSearchProfiles :: SearchProfilesResponse -> TestTree
responseSearchProfiles = res
    "SearchProfilesResponse"
    "fixture/SearchProfilesResponse.proto"
    alexaBusiness
    (Proxy :: Proxy SearchProfiles)

responseRejectSkill :: RejectSkillResponse -> TestTree
responseRejectSkill = res
    "RejectSkillResponse"
    "fixture/RejectSkillResponse.proto"
    alexaBusiness
    (Proxy :: Proxy RejectSkill)

responseListConferenceProviders :: ListConferenceProvidersResponse -> TestTree
responseListConferenceProviders = res
    "ListConferenceProvidersResponse"
    "fixture/ListConferenceProvidersResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ListConferenceProviders)

responseRevokeInvitation :: RevokeInvitationResponse -> TestTree
responseRevokeInvitation = res
    "RevokeInvitationResponse"
    "fixture/RevokeInvitationResponse.proto"
    alexaBusiness
    (Proxy :: Proxy RevokeInvitation)

responseDeleteDevice :: DeleteDeviceResponse -> TestTree
responseDeleteDevice = res
    "DeleteDeviceResponse"
    "fixture/DeleteDeviceResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DeleteDevice)

responseUpdateDevice :: UpdateDeviceResponse -> TestTree
responseUpdateDevice = res
    "UpdateDeviceResponse"
    "fixture/UpdateDeviceResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UpdateDevice)

responseAssociateSkillWithSkillGroup :: AssociateSkillWithSkillGroupResponse -> TestTree
responseAssociateSkillWithSkillGroup = res
    "AssociateSkillWithSkillGroupResponse"
    "fixture/AssociateSkillWithSkillGroupResponse.proto"
    alexaBusiness
    (Proxy :: Proxy AssociateSkillWithSkillGroup)

responseGetConferenceProvider :: GetConferenceProviderResponse -> TestTree
responseGetConferenceProvider = res
    "GetConferenceProviderResponse"
    "fixture/GetConferenceProviderResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetConferenceProvider)

responseGetRoom :: GetRoomResponse -> TestTree
responseGetRoom = res
    "GetRoomResponse"
    "fixture/GetRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetRoom)

responseListSkillsStoreCategories :: ListSkillsStoreCategoriesResponse -> TestTree
responseListSkillsStoreCategories = res
    "ListSkillsStoreCategoriesResponse"
    "fixture/ListSkillsStoreCategoriesResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ListSkillsStoreCategories)

responseStartSmartHomeApplianceDiscovery :: StartSmartHomeApplianceDiscoveryResponse -> TestTree
responseStartSmartHomeApplianceDiscovery = res
    "StartSmartHomeApplianceDiscoveryResponse"
    "fixture/StartSmartHomeApplianceDiscoveryResponse.proto"
    alexaBusiness
    (Proxy :: Proxy StartSmartHomeApplianceDiscovery)

responseAssociateContactWithAddressBook :: AssociateContactWithAddressBookResponse -> TestTree
responseAssociateContactWithAddressBook = res
    "AssociateContactWithAddressBookResponse"
    "fixture/AssociateContactWithAddressBookResponse.proto"
    alexaBusiness
    (Proxy :: Proxy AssociateContactWithAddressBook)
