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
--         , requestAssociateSkillGroupWithRoom $
--             associateSkillGroupWithRoom
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
--         , requestDisassociateContactFromAddressBook $
--             disassociateContactFromAddressBook
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
--         , requestGetContact $
--             getContact
--
--         , requestAssociateDeviceWithRoom $
--             associateDeviceWithRoom
--
--         , requestGetRoomSkillParameter $
--             getRoomSkillParameter
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
--         , requestListTags $
--             listTags
--
--         , requestUntagResource $
--             untagResource
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
--         , requestRevokeInvitation $
--             revokeInvitation
--
--         , requestUpdateDevice $
--             updateDevice
--
--         , requestGetRoom $
--             getRoom
--
--         , requestAssociateContactWithAddressBook $
--             associateContactWithAddressBook
--
--           ]

--     , testGroup "response"
--         [ responseSearchUsers $
--             searchUsersResponse
--
--         , responseAssociateSkillGroupWithRoom $
--             associateSkillGroupWithRoomResponse
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
--         , responseDisassociateContactFromAddressBook $
--             disassociateContactFromAddressBookResponse
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
--         , responseGetContact $
--             getContactResponse
--
--         , responseAssociateDeviceWithRoom $
--             associateDeviceWithRoomResponse
--
--         , responseGetRoomSkillParameter $
--             getRoomSkillParameterResponse
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
--         , responseListTags $
--             listTagsResponse
--
--         , responseUntagResource $
--             untagResourceResponse
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
--         , responseRevokeInvitation $
--             revokeInvitationResponse
--
--         , responseUpdateDevice $
--             updateDeviceResponse
--
--         , responseGetRoom $
--             getRoomResponse
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

requestAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoom -> TestTree
requestAssociateSkillGroupWithRoom = req
    "AssociateSkillGroupWithRoom"
    "fixture/AssociateSkillGroupWithRoom.yaml"

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

requestDisassociateContactFromAddressBook :: DisassociateContactFromAddressBook -> TestTree
requestDisassociateContactFromAddressBook = req
    "DisassociateContactFromAddressBook"
    "fixture/DisassociateContactFromAddressBook.yaml"

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

requestGetContact :: GetContact -> TestTree
requestGetContact = req
    "GetContact"
    "fixture/GetContact.yaml"

requestAssociateDeviceWithRoom :: AssociateDeviceWithRoom -> TestTree
requestAssociateDeviceWithRoom = req
    "AssociateDeviceWithRoom"
    "fixture/AssociateDeviceWithRoom.yaml"

requestGetRoomSkillParameter :: GetRoomSkillParameter -> TestTree
requestGetRoomSkillParameter = req
    "GetRoomSkillParameter"
    "fixture/GetRoomSkillParameter.yaml"

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

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestRevokeInvitation :: RevokeInvitation -> TestTree
requestRevokeInvitation = req
    "RevokeInvitation"
    "fixture/RevokeInvitation.yaml"

requestUpdateDevice :: UpdateDevice -> TestTree
requestUpdateDevice = req
    "UpdateDevice"
    "fixture/UpdateDevice.yaml"

requestGetRoom :: GetRoom -> TestTree
requestGetRoom = req
    "GetRoom"
    "fixture/GetRoom.yaml"

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

responseAssociateSkillGroupWithRoom :: AssociateSkillGroupWithRoomResponse -> TestTree
responseAssociateSkillGroupWithRoom = res
    "AssociateSkillGroupWithRoomResponse"
    "fixture/AssociateSkillGroupWithRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy AssociateSkillGroupWithRoom)

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

responseDisassociateContactFromAddressBook :: DisassociateContactFromAddressBookResponse -> TestTree
responseDisassociateContactFromAddressBook = res
    "DisassociateContactFromAddressBookResponse"
    "fixture/DisassociateContactFromAddressBookResponse.proto"
    alexaBusiness
    (Proxy :: Proxy DisassociateContactFromAddressBook)

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

responseGetContact :: GetContactResponse -> TestTree
responseGetContact = res
    "GetContactResponse"
    "fixture/GetContactResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetContact)

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

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    alexaBusiness
    (Proxy :: Proxy ListTags)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UntagResource)

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

responseRevokeInvitation :: RevokeInvitationResponse -> TestTree
responseRevokeInvitation = res
    "RevokeInvitationResponse"
    "fixture/RevokeInvitationResponse.proto"
    alexaBusiness
    (Proxy :: Proxy RevokeInvitation)

responseUpdateDevice :: UpdateDeviceResponse -> TestTree
responseUpdateDevice = res
    "UpdateDeviceResponse"
    "fixture/UpdateDeviceResponse.proto"
    alexaBusiness
    (Proxy :: Proxy UpdateDevice)

responseGetRoom :: GetRoomResponse -> TestTree
responseGetRoom = res
    "GetRoomResponse"
    "fixture/GetRoomResponse.proto"
    alexaBusiness
    (Proxy :: Proxy GetRoom)

responseAssociateContactWithAddressBook :: AssociateContactWithAddressBookResponse -> TestTree
responseAssociateContactWithAddressBook = res
    "AssociateContactWithAddressBookResponse"
    "fixture/AssociateContactWithAddressBookResponse.proto"
    alexaBusiness
    (Proxy :: Proxy AssociateContactWithAddressBook)
