{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AlexaBusiness
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-11-09@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Alexa for Business helps you use Alexa in your organization. Alexa for
-- Business provides you with the tools to manage Alexa devices, enroll
-- your users, and assign skills, at scale. You can build your own
-- context-aware voice skills using the Alexa Skills Kit and the Alexa for
-- Business API operations. You can also make these available as private
-- skills for your organization. Alexa for Business makes it efficient to
-- voice-enable your products and services, thus providing context-aware
-- voice experiences for your customers. Device makers building with the
-- Alexa Voice Service (AVS) can create fully integrated solutions,
-- register their products with Alexa for Business, and manage them as
-- shared devices in their organization.
module Amazonka.AlexaBusiness
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** DeviceNotRegisteredException
    _DeviceNotRegisteredException,

    -- ** InvalidCertificateAuthorityException
    _InvalidCertificateAuthorityException,

    -- ** InvalidDeviceException
    _InvalidDeviceException,

    -- ** InvalidSecretsManagerResourceException
    _InvalidSecretsManagerResourceException,

    -- ** InvalidServiceLinkedRoleStateException
    _InvalidServiceLinkedRoleStateException,

    -- ** InvalidUserStatusException
    _InvalidUserStatusException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NameInUseException
    _NameInUseException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceAssociatedException
    _ResourceAssociatedException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** SkillNotLinkedException
    _SkillNotLinkedException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ApproveSkill
    ApproveSkill (ApproveSkill'),
    newApproveSkill,
    ApproveSkillResponse (ApproveSkillResponse'),
    newApproveSkillResponse,

    -- ** AssociateContactWithAddressBook
    AssociateContactWithAddressBook (AssociateContactWithAddressBook'),
    newAssociateContactWithAddressBook,
    AssociateContactWithAddressBookResponse (AssociateContactWithAddressBookResponse'),
    newAssociateContactWithAddressBookResponse,

    -- ** AssociateDeviceWithNetworkProfile
    AssociateDeviceWithNetworkProfile (AssociateDeviceWithNetworkProfile'),
    newAssociateDeviceWithNetworkProfile,
    AssociateDeviceWithNetworkProfileResponse (AssociateDeviceWithNetworkProfileResponse'),
    newAssociateDeviceWithNetworkProfileResponse,

    -- ** AssociateDeviceWithRoom
    AssociateDeviceWithRoom (AssociateDeviceWithRoom'),
    newAssociateDeviceWithRoom,
    AssociateDeviceWithRoomResponse (AssociateDeviceWithRoomResponse'),
    newAssociateDeviceWithRoomResponse,

    -- ** AssociateSkillGroupWithRoom
    AssociateSkillGroupWithRoom (AssociateSkillGroupWithRoom'),
    newAssociateSkillGroupWithRoom,
    AssociateSkillGroupWithRoomResponse (AssociateSkillGroupWithRoomResponse'),
    newAssociateSkillGroupWithRoomResponse,

    -- ** AssociateSkillWithSkillGroup
    AssociateSkillWithSkillGroup (AssociateSkillWithSkillGroup'),
    newAssociateSkillWithSkillGroup,
    AssociateSkillWithSkillGroupResponse (AssociateSkillWithSkillGroupResponse'),
    newAssociateSkillWithSkillGroupResponse,

    -- ** AssociateSkillWithUsers
    AssociateSkillWithUsers (AssociateSkillWithUsers'),
    newAssociateSkillWithUsers,
    AssociateSkillWithUsersResponse (AssociateSkillWithUsersResponse'),
    newAssociateSkillWithUsersResponse,

    -- ** CreateAddressBook
    CreateAddressBook (CreateAddressBook'),
    newCreateAddressBook,
    CreateAddressBookResponse (CreateAddressBookResponse'),
    newCreateAddressBookResponse,

    -- ** CreateBusinessReportSchedule
    CreateBusinessReportSchedule (CreateBusinessReportSchedule'),
    newCreateBusinessReportSchedule,
    CreateBusinessReportScheduleResponse (CreateBusinessReportScheduleResponse'),
    newCreateBusinessReportScheduleResponse,

    -- ** CreateConferenceProvider
    CreateConferenceProvider (CreateConferenceProvider'),
    newCreateConferenceProvider,
    CreateConferenceProviderResponse (CreateConferenceProviderResponse'),
    newCreateConferenceProviderResponse,

    -- ** CreateContact
    CreateContact (CreateContact'),
    newCreateContact,
    CreateContactResponse (CreateContactResponse'),
    newCreateContactResponse,

    -- ** CreateGatewayGroup
    CreateGatewayGroup (CreateGatewayGroup'),
    newCreateGatewayGroup,
    CreateGatewayGroupResponse (CreateGatewayGroupResponse'),
    newCreateGatewayGroupResponse,

    -- ** CreateNetworkProfile
    CreateNetworkProfile (CreateNetworkProfile'),
    newCreateNetworkProfile,
    CreateNetworkProfileResponse (CreateNetworkProfileResponse'),
    newCreateNetworkProfileResponse,

    -- ** CreateProfile
    CreateProfile (CreateProfile'),
    newCreateProfile,
    CreateProfileResponse (CreateProfileResponse'),
    newCreateProfileResponse,

    -- ** CreateRoom
    CreateRoom (CreateRoom'),
    newCreateRoom,
    CreateRoomResponse (CreateRoomResponse'),
    newCreateRoomResponse,

    -- ** CreateSkillGroup
    CreateSkillGroup (CreateSkillGroup'),
    newCreateSkillGroup,
    CreateSkillGroupResponse (CreateSkillGroupResponse'),
    newCreateSkillGroupResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** DeleteAddressBook
    DeleteAddressBook (DeleteAddressBook'),
    newDeleteAddressBook,
    DeleteAddressBookResponse (DeleteAddressBookResponse'),
    newDeleteAddressBookResponse,

    -- ** DeleteBusinessReportSchedule
    DeleteBusinessReportSchedule (DeleteBusinessReportSchedule'),
    newDeleteBusinessReportSchedule,
    DeleteBusinessReportScheduleResponse (DeleteBusinessReportScheduleResponse'),
    newDeleteBusinessReportScheduleResponse,

    -- ** DeleteConferenceProvider
    DeleteConferenceProvider (DeleteConferenceProvider'),
    newDeleteConferenceProvider,
    DeleteConferenceProviderResponse (DeleteConferenceProviderResponse'),
    newDeleteConferenceProviderResponse,

    -- ** DeleteContact
    DeleteContact (DeleteContact'),
    newDeleteContact,
    DeleteContactResponse (DeleteContactResponse'),
    newDeleteContactResponse,

    -- ** DeleteDevice
    DeleteDevice (DeleteDevice'),
    newDeleteDevice,
    DeleteDeviceResponse (DeleteDeviceResponse'),
    newDeleteDeviceResponse,

    -- ** DeleteDeviceUsageData
    DeleteDeviceUsageData (DeleteDeviceUsageData'),
    newDeleteDeviceUsageData,
    DeleteDeviceUsageDataResponse (DeleteDeviceUsageDataResponse'),
    newDeleteDeviceUsageDataResponse,

    -- ** DeleteGatewayGroup
    DeleteGatewayGroup (DeleteGatewayGroup'),
    newDeleteGatewayGroup,
    DeleteGatewayGroupResponse (DeleteGatewayGroupResponse'),
    newDeleteGatewayGroupResponse,

    -- ** DeleteNetworkProfile
    DeleteNetworkProfile (DeleteNetworkProfile'),
    newDeleteNetworkProfile,
    DeleteNetworkProfileResponse (DeleteNetworkProfileResponse'),
    newDeleteNetworkProfileResponse,

    -- ** DeleteProfile
    DeleteProfile (DeleteProfile'),
    newDeleteProfile,
    DeleteProfileResponse (DeleteProfileResponse'),
    newDeleteProfileResponse,

    -- ** DeleteRoom
    DeleteRoom (DeleteRoom'),
    newDeleteRoom,
    DeleteRoomResponse (DeleteRoomResponse'),
    newDeleteRoomResponse,

    -- ** DeleteRoomSkillParameter
    DeleteRoomSkillParameter (DeleteRoomSkillParameter'),
    newDeleteRoomSkillParameter,
    DeleteRoomSkillParameterResponse (DeleteRoomSkillParameterResponse'),
    newDeleteRoomSkillParameterResponse,

    -- ** DeleteSkillAuthorization
    DeleteSkillAuthorization (DeleteSkillAuthorization'),
    newDeleteSkillAuthorization,
    DeleteSkillAuthorizationResponse (DeleteSkillAuthorizationResponse'),
    newDeleteSkillAuthorizationResponse,

    -- ** DeleteSkillGroup
    DeleteSkillGroup (DeleteSkillGroup'),
    newDeleteSkillGroup,
    DeleteSkillGroupResponse (DeleteSkillGroupResponse'),
    newDeleteSkillGroupResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DisassociateContactFromAddressBook
    DisassociateContactFromAddressBook (DisassociateContactFromAddressBook'),
    newDisassociateContactFromAddressBook,
    DisassociateContactFromAddressBookResponse (DisassociateContactFromAddressBookResponse'),
    newDisassociateContactFromAddressBookResponse,

    -- ** DisassociateDeviceFromRoom
    DisassociateDeviceFromRoom (DisassociateDeviceFromRoom'),
    newDisassociateDeviceFromRoom,
    DisassociateDeviceFromRoomResponse (DisassociateDeviceFromRoomResponse'),
    newDisassociateDeviceFromRoomResponse,

    -- ** DisassociateSkillFromSkillGroup
    DisassociateSkillFromSkillGroup (DisassociateSkillFromSkillGroup'),
    newDisassociateSkillFromSkillGroup,
    DisassociateSkillFromSkillGroupResponse (DisassociateSkillFromSkillGroupResponse'),
    newDisassociateSkillFromSkillGroupResponse,

    -- ** DisassociateSkillFromUsers
    DisassociateSkillFromUsers (DisassociateSkillFromUsers'),
    newDisassociateSkillFromUsers,
    DisassociateSkillFromUsersResponse (DisassociateSkillFromUsersResponse'),
    newDisassociateSkillFromUsersResponse,

    -- ** DisassociateSkillGroupFromRoom
    DisassociateSkillGroupFromRoom (DisassociateSkillGroupFromRoom'),
    newDisassociateSkillGroupFromRoom,
    DisassociateSkillGroupFromRoomResponse (DisassociateSkillGroupFromRoomResponse'),
    newDisassociateSkillGroupFromRoomResponse,

    -- ** ForgetSmartHomeAppliances
    ForgetSmartHomeAppliances (ForgetSmartHomeAppliances'),
    newForgetSmartHomeAppliances,
    ForgetSmartHomeAppliancesResponse (ForgetSmartHomeAppliancesResponse'),
    newForgetSmartHomeAppliancesResponse,

    -- ** GetAddressBook
    GetAddressBook (GetAddressBook'),
    newGetAddressBook,
    GetAddressBookResponse (GetAddressBookResponse'),
    newGetAddressBookResponse,

    -- ** GetConferencePreference
    GetConferencePreference (GetConferencePreference'),
    newGetConferencePreference,
    GetConferencePreferenceResponse (GetConferencePreferenceResponse'),
    newGetConferencePreferenceResponse,

    -- ** GetConferenceProvider
    GetConferenceProvider (GetConferenceProvider'),
    newGetConferenceProvider,
    GetConferenceProviderResponse (GetConferenceProviderResponse'),
    newGetConferenceProviderResponse,

    -- ** GetContact
    GetContact (GetContact'),
    newGetContact,
    GetContactResponse (GetContactResponse'),
    newGetContactResponse,

    -- ** GetDevice
    GetDevice (GetDevice'),
    newGetDevice,
    GetDeviceResponse (GetDeviceResponse'),
    newGetDeviceResponse,

    -- ** GetGateway
    GetGateway (GetGateway'),
    newGetGateway,
    GetGatewayResponse (GetGatewayResponse'),
    newGetGatewayResponse,

    -- ** GetGatewayGroup
    GetGatewayGroup (GetGatewayGroup'),
    newGetGatewayGroup,
    GetGatewayGroupResponse (GetGatewayGroupResponse'),
    newGetGatewayGroupResponse,

    -- ** GetInvitationConfiguration
    GetInvitationConfiguration (GetInvitationConfiguration'),
    newGetInvitationConfiguration,
    GetInvitationConfigurationResponse (GetInvitationConfigurationResponse'),
    newGetInvitationConfigurationResponse,

    -- ** GetNetworkProfile
    GetNetworkProfile (GetNetworkProfile'),
    newGetNetworkProfile,
    GetNetworkProfileResponse (GetNetworkProfileResponse'),
    newGetNetworkProfileResponse,

    -- ** GetProfile
    GetProfile (GetProfile'),
    newGetProfile,
    GetProfileResponse (GetProfileResponse'),
    newGetProfileResponse,

    -- ** GetRoom
    GetRoom (GetRoom'),
    newGetRoom,
    GetRoomResponse (GetRoomResponse'),
    newGetRoomResponse,

    -- ** GetRoomSkillParameter
    GetRoomSkillParameter (GetRoomSkillParameter'),
    newGetRoomSkillParameter,
    GetRoomSkillParameterResponse (GetRoomSkillParameterResponse'),
    newGetRoomSkillParameterResponse,

    -- ** GetSkillGroup
    GetSkillGroup (GetSkillGroup'),
    newGetSkillGroup,
    GetSkillGroupResponse (GetSkillGroupResponse'),
    newGetSkillGroupResponse,

    -- ** ListBusinessReportSchedules (Paginated)
    ListBusinessReportSchedules (ListBusinessReportSchedules'),
    newListBusinessReportSchedules,
    ListBusinessReportSchedulesResponse (ListBusinessReportSchedulesResponse'),
    newListBusinessReportSchedulesResponse,

    -- ** ListConferenceProviders (Paginated)
    ListConferenceProviders (ListConferenceProviders'),
    newListConferenceProviders,
    ListConferenceProvidersResponse (ListConferenceProvidersResponse'),
    newListConferenceProvidersResponse,

    -- ** ListDeviceEvents (Paginated)
    ListDeviceEvents (ListDeviceEvents'),
    newListDeviceEvents,
    ListDeviceEventsResponse (ListDeviceEventsResponse'),
    newListDeviceEventsResponse,

    -- ** ListGatewayGroups
    ListGatewayGroups (ListGatewayGroups'),
    newListGatewayGroups,
    ListGatewayGroupsResponse (ListGatewayGroupsResponse'),
    newListGatewayGroupsResponse,

    -- ** ListGateways
    ListGateways (ListGateways'),
    newListGateways,
    ListGatewaysResponse (ListGatewaysResponse'),
    newListGatewaysResponse,

    -- ** ListSkills (Paginated)
    ListSkills (ListSkills'),
    newListSkills,
    ListSkillsResponse (ListSkillsResponse'),
    newListSkillsResponse,

    -- ** ListSkillsStoreCategories (Paginated)
    ListSkillsStoreCategories (ListSkillsStoreCategories'),
    newListSkillsStoreCategories,
    ListSkillsStoreCategoriesResponse (ListSkillsStoreCategoriesResponse'),
    newListSkillsStoreCategoriesResponse,

    -- ** ListSkillsStoreSkillsByCategory (Paginated)
    ListSkillsStoreSkillsByCategory (ListSkillsStoreSkillsByCategory'),
    newListSkillsStoreSkillsByCategory,
    ListSkillsStoreSkillsByCategoryResponse (ListSkillsStoreSkillsByCategoryResponse'),
    newListSkillsStoreSkillsByCategoryResponse,

    -- ** ListSmartHomeAppliances (Paginated)
    ListSmartHomeAppliances (ListSmartHomeAppliances'),
    newListSmartHomeAppliances,
    ListSmartHomeAppliancesResponse (ListSmartHomeAppliancesResponse'),
    newListSmartHomeAppliancesResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** PutConferencePreference
    PutConferencePreference (PutConferencePreference'),
    newPutConferencePreference,
    PutConferencePreferenceResponse (PutConferencePreferenceResponse'),
    newPutConferencePreferenceResponse,

    -- ** PutInvitationConfiguration
    PutInvitationConfiguration (PutInvitationConfiguration'),
    newPutInvitationConfiguration,
    PutInvitationConfigurationResponse (PutInvitationConfigurationResponse'),
    newPutInvitationConfigurationResponse,

    -- ** PutRoomSkillParameter
    PutRoomSkillParameter (PutRoomSkillParameter'),
    newPutRoomSkillParameter,
    PutRoomSkillParameterResponse (PutRoomSkillParameterResponse'),
    newPutRoomSkillParameterResponse,

    -- ** PutSkillAuthorization
    PutSkillAuthorization (PutSkillAuthorization'),
    newPutSkillAuthorization,
    PutSkillAuthorizationResponse (PutSkillAuthorizationResponse'),
    newPutSkillAuthorizationResponse,

    -- ** RegisterAVSDevice
    RegisterAVSDevice (RegisterAVSDevice'),
    newRegisterAVSDevice,
    RegisterAVSDeviceResponse (RegisterAVSDeviceResponse'),
    newRegisterAVSDeviceResponse,

    -- ** RejectSkill
    RejectSkill (RejectSkill'),
    newRejectSkill,
    RejectSkillResponse (RejectSkillResponse'),
    newRejectSkillResponse,

    -- ** ResolveRoom
    ResolveRoom (ResolveRoom'),
    newResolveRoom,
    ResolveRoomResponse (ResolveRoomResponse'),
    newResolveRoomResponse,

    -- ** RevokeInvitation
    RevokeInvitation (RevokeInvitation'),
    newRevokeInvitation,
    RevokeInvitationResponse (RevokeInvitationResponse'),
    newRevokeInvitationResponse,

    -- ** SearchAddressBooks
    SearchAddressBooks (SearchAddressBooks'),
    newSearchAddressBooks,
    SearchAddressBooksResponse (SearchAddressBooksResponse'),
    newSearchAddressBooksResponse,

    -- ** SearchContacts
    SearchContacts (SearchContacts'),
    newSearchContacts,
    SearchContactsResponse (SearchContactsResponse'),
    newSearchContactsResponse,

    -- ** SearchDevices (Paginated)
    SearchDevices (SearchDevices'),
    newSearchDevices,
    SearchDevicesResponse (SearchDevicesResponse'),
    newSearchDevicesResponse,

    -- ** SearchNetworkProfiles
    SearchNetworkProfiles (SearchNetworkProfiles'),
    newSearchNetworkProfiles,
    SearchNetworkProfilesResponse (SearchNetworkProfilesResponse'),
    newSearchNetworkProfilesResponse,

    -- ** SearchProfiles (Paginated)
    SearchProfiles (SearchProfiles'),
    newSearchProfiles,
    SearchProfilesResponse (SearchProfilesResponse'),
    newSearchProfilesResponse,

    -- ** SearchRooms (Paginated)
    SearchRooms (SearchRooms'),
    newSearchRooms,
    SearchRoomsResponse (SearchRoomsResponse'),
    newSearchRoomsResponse,

    -- ** SearchSkillGroups (Paginated)
    SearchSkillGroups (SearchSkillGroups'),
    newSearchSkillGroups,
    SearchSkillGroupsResponse (SearchSkillGroupsResponse'),
    newSearchSkillGroupsResponse,

    -- ** SearchUsers (Paginated)
    SearchUsers (SearchUsers'),
    newSearchUsers,
    SearchUsersResponse (SearchUsersResponse'),
    newSearchUsersResponse,

    -- ** SendAnnouncement
    SendAnnouncement (SendAnnouncement'),
    newSendAnnouncement,
    SendAnnouncementResponse (SendAnnouncementResponse'),
    newSendAnnouncementResponse,

    -- ** SendInvitation
    SendInvitation (SendInvitation'),
    newSendInvitation,
    SendInvitationResponse (SendInvitationResponse'),
    newSendInvitationResponse,

    -- ** StartDeviceSync
    StartDeviceSync (StartDeviceSync'),
    newStartDeviceSync,
    StartDeviceSyncResponse (StartDeviceSyncResponse'),
    newStartDeviceSyncResponse,

    -- ** StartSmartHomeApplianceDiscovery
    StartSmartHomeApplianceDiscovery (StartSmartHomeApplianceDiscovery'),
    newStartSmartHomeApplianceDiscovery,
    StartSmartHomeApplianceDiscoveryResponse (StartSmartHomeApplianceDiscoveryResponse'),
    newStartSmartHomeApplianceDiscoveryResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAddressBook
    UpdateAddressBook (UpdateAddressBook'),
    newUpdateAddressBook,
    UpdateAddressBookResponse (UpdateAddressBookResponse'),
    newUpdateAddressBookResponse,

    -- ** UpdateBusinessReportSchedule
    UpdateBusinessReportSchedule (UpdateBusinessReportSchedule'),
    newUpdateBusinessReportSchedule,
    UpdateBusinessReportScheduleResponse (UpdateBusinessReportScheduleResponse'),
    newUpdateBusinessReportScheduleResponse,

    -- ** UpdateConferenceProvider
    UpdateConferenceProvider (UpdateConferenceProvider'),
    newUpdateConferenceProvider,
    UpdateConferenceProviderResponse (UpdateConferenceProviderResponse'),
    newUpdateConferenceProviderResponse,

    -- ** UpdateContact
    UpdateContact (UpdateContact'),
    newUpdateContact,
    UpdateContactResponse (UpdateContactResponse'),
    newUpdateContactResponse,

    -- ** UpdateDevice
    UpdateDevice (UpdateDevice'),
    newUpdateDevice,
    UpdateDeviceResponse (UpdateDeviceResponse'),
    newUpdateDeviceResponse,

    -- ** UpdateGateway
    UpdateGateway (UpdateGateway'),
    newUpdateGateway,
    UpdateGatewayResponse (UpdateGatewayResponse'),
    newUpdateGatewayResponse,

    -- ** UpdateGatewayGroup
    UpdateGatewayGroup (UpdateGatewayGroup'),
    newUpdateGatewayGroup,
    UpdateGatewayGroupResponse (UpdateGatewayGroupResponse'),
    newUpdateGatewayGroupResponse,

    -- ** UpdateNetworkProfile
    UpdateNetworkProfile (UpdateNetworkProfile'),
    newUpdateNetworkProfile,
    UpdateNetworkProfileResponse (UpdateNetworkProfileResponse'),
    newUpdateNetworkProfileResponse,

    -- ** UpdateProfile
    UpdateProfile (UpdateProfile'),
    newUpdateProfile,
    UpdateProfileResponse (UpdateProfileResponse'),
    newUpdateProfileResponse,

    -- ** UpdateRoom
    UpdateRoom (UpdateRoom'),
    newUpdateRoom,
    UpdateRoomResponse (UpdateRoomResponse'),
    newUpdateRoomResponse,

    -- ** UpdateSkillGroup
    UpdateSkillGroup (UpdateSkillGroup'),
    newUpdateSkillGroup,
    UpdateSkillGroupResponse (UpdateSkillGroupResponse'),
    newUpdateSkillGroupResponse,

    -- * Types

    -- ** BusinessReportFailureCode
    BusinessReportFailureCode (..),

    -- ** BusinessReportFormat
    BusinessReportFormat (..),

    -- ** BusinessReportInterval
    BusinessReportInterval (..),

    -- ** BusinessReportStatus
    BusinessReportStatus (..),

    -- ** CommsProtocol
    CommsProtocol (..),

    -- ** ConferenceProviderType
    ConferenceProviderType (..),

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** DeviceEventType
    DeviceEventType (..),

    -- ** DeviceStatus
    DeviceStatus (..),

    -- ** DeviceStatusDetailCode
    DeviceStatusDetailCode (..),

    -- ** DeviceUsageType
    DeviceUsageType (..),

    -- ** DistanceUnit
    DistanceUnit (..),

    -- ** EnablementType
    EnablementType (..),

    -- ** EnablementTypeFilter
    EnablementTypeFilter (..),

    -- ** EndOfMeetingReminderType
    EndOfMeetingReminderType (..),

    -- ** EnrollmentStatus
    EnrollmentStatus (..),

    -- ** Feature
    Feature (..),

    -- ** Locale
    Locale (..),

    -- ** NetworkEapMethod
    NetworkEapMethod (..),

    -- ** NetworkSecurityType
    NetworkSecurityType (..),

    -- ** PhoneNumberType
    PhoneNumberType (..),

    -- ** RequirePin
    RequirePin (..),

    -- ** SipType
    SipType (..),

    -- ** SkillType
    SkillType (..),

    -- ** SkillTypeFilter
    SkillTypeFilter (..),

    -- ** SortValue
    SortValue (..),

    -- ** TemperatureUnit
    TemperatureUnit (..),

    -- ** WakeWord
    WakeWord (..),

    -- ** AddressBook
    AddressBook (AddressBook'),
    newAddressBook,

    -- ** AddressBookData
    AddressBookData (AddressBookData'),
    newAddressBookData,

    -- ** Audio
    Audio (Audio'),
    newAudio,

    -- ** BusinessReport
    BusinessReport (BusinessReport'),
    newBusinessReport,

    -- ** BusinessReportContentRange
    BusinessReportContentRange (BusinessReportContentRange'),
    newBusinessReportContentRange,

    -- ** BusinessReportRecurrence
    BusinessReportRecurrence (BusinessReportRecurrence'),
    newBusinessReportRecurrence,

    -- ** BusinessReportS3Location
    BusinessReportS3Location (BusinessReportS3Location'),
    newBusinessReportS3Location,

    -- ** BusinessReportSchedule
    BusinessReportSchedule (BusinessReportSchedule'),
    newBusinessReportSchedule,

    -- ** Category
    Category (Category'),
    newCategory,

    -- ** ConferencePreference
    ConferencePreference (ConferencePreference'),
    newConferencePreference,

    -- ** ConferenceProvider
    ConferenceProvider (ConferenceProvider'),
    newConferenceProvider,

    -- ** Contact
    Contact (Contact'),
    newContact,

    -- ** ContactData
    ContactData (ContactData'),
    newContactData,

    -- ** Content
    Content (Content'),
    newContent,

    -- ** CreateEndOfMeetingReminder
    CreateEndOfMeetingReminder (CreateEndOfMeetingReminder'),
    newCreateEndOfMeetingReminder,

    -- ** CreateInstantBooking
    CreateInstantBooking (CreateInstantBooking'),
    newCreateInstantBooking,

    -- ** CreateMeetingRoomConfiguration
    CreateMeetingRoomConfiguration (CreateMeetingRoomConfiguration'),
    newCreateMeetingRoomConfiguration,

    -- ** CreateRequireCheckIn
    CreateRequireCheckIn (CreateRequireCheckIn'),
    newCreateRequireCheckIn,

    -- ** DeveloperInfo
    DeveloperInfo (DeveloperInfo'),
    newDeveloperInfo,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** DeviceData
    DeviceData (DeviceData'),
    newDeviceData,

    -- ** DeviceEvent
    DeviceEvent (DeviceEvent'),
    newDeviceEvent,

    -- ** DeviceNetworkProfileInfo
    DeviceNetworkProfileInfo (DeviceNetworkProfileInfo'),
    newDeviceNetworkProfileInfo,

    -- ** DeviceStatusDetail
    DeviceStatusDetail (DeviceStatusDetail'),
    newDeviceStatusDetail,

    -- ** DeviceStatusInfo
    DeviceStatusInfo (DeviceStatusInfo'),
    newDeviceStatusInfo,

    -- ** EndOfMeetingReminder
    EndOfMeetingReminder (EndOfMeetingReminder'),
    newEndOfMeetingReminder,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** Gateway
    Gateway (Gateway'),
    newGateway,

    -- ** GatewayGroup
    GatewayGroup (GatewayGroup'),
    newGatewayGroup,

    -- ** GatewayGroupSummary
    GatewayGroupSummary (GatewayGroupSummary'),
    newGatewayGroupSummary,

    -- ** GatewaySummary
    GatewaySummary (GatewaySummary'),
    newGatewaySummary,

    -- ** IPDialIn
    IPDialIn (IPDialIn'),
    newIPDialIn,

    -- ** InstantBooking
    InstantBooking (InstantBooking'),
    newInstantBooking,

    -- ** MeetingRoomConfiguration
    MeetingRoomConfiguration (MeetingRoomConfiguration'),
    newMeetingRoomConfiguration,

    -- ** MeetingSetting
    MeetingSetting (MeetingSetting'),
    newMeetingSetting,

    -- ** NetworkProfile
    NetworkProfile (NetworkProfile'),
    newNetworkProfile,

    -- ** NetworkProfileData
    NetworkProfileData (NetworkProfileData'),
    newNetworkProfileData,

    -- ** PSTNDialIn
    PSTNDialIn (PSTNDialIn'),
    newPSTNDialIn,

    -- ** PhoneNumber
    PhoneNumber (PhoneNumber'),
    newPhoneNumber,

    -- ** Profile
    Profile (Profile'),
    newProfile,

    -- ** ProfileData
    ProfileData (ProfileData'),
    newProfileData,

    -- ** RequireCheckIn
    RequireCheckIn (RequireCheckIn'),
    newRequireCheckIn,

    -- ** Room
    Room (Room'),
    newRoom,

    -- ** RoomData
    RoomData (RoomData'),
    newRoomData,

    -- ** RoomSkillParameter
    RoomSkillParameter (RoomSkillParameter'),
    newRoomSkillParameter,

    -- ** SipAddress
    SipAddress (SipAddress'),
    newSipAddress,

    -- ** SkillDetails
    SkillDetails (SkillDetails'),
    newSkillDetails,

    -- ** SkillGroup
    SkillGroup (SkillGroup'),
    newSkillGroup,

    -- ** SkillGroupData
    SkillGroupData (SkillGroupData'),
    newSkillGroupData,

    -- ** SkillSummary
    SkillSummary (SkillSummary'),
    newSkillSummary,

    -- ** SkillsStoreSkill
    SkillsStoreSkill (SkillsStoreSkill'),
    newSkillsStoreSkill,

    -- ** SmartHomeAppliance
    SmartHomeAppliance (SmartHomeAppliance'),
    newSmartHomeAppliance,

    -- ** Sort
    Sort (Sort'),
    newSort,

    -- ** Ssml
    Ssml (Ssml'),
    newSsml,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TextMessage
    TextMessage (TextMessage'),
    newTextMessage,

    -- ** UpdateEndOfMeetingReminder
    UpdateEndOfMeetingReminder (UpdateEndOfMeetingReminder'),
    newUpdateEndOfMeetingReminder,

    -- ** UpdateInstantBooking
    UpdateInstantBooking (UpdateInstantBooking'),
    newUpdateInstantBooking,

    -- ** UpdateMeetingRoomConfiguration
    UpdateMeetingRoomConfiguration (UpdateMeetingRoomConfiguration'),
    newUpdateMeetingRoomConfiguration,

    -- ** UpdateRequireCheckIn
    UpdateRequireCheckIn (UpdateRequireCheckIn'),
    newUpdateRequireCheckIn,

    -- ** UserData
    UserData (UserData'),
    newUserData,
  )
where

import Amazonka.AlexaBusiness.ApproveSkill
import Amazonka.AlexaBusiness.AssociateContactWithAddressBook
import Amazonka.AlexaBusiness.AssociateDeviceWithNetworkProfile
import Amazonka.AlexaBusiness.AssociateDeviceWithRoom
import Amazonka.AlexaBusiness.AssociateSkillGroupWithRoom
import Amazonka.AlexaBusiness.AssociateSkillWithSkillGroup
import Amazonka.AlexaBusiness.AssociateSkillWithUsers
import Amazonka.AlexaBusiness.CreateAddressBook
import Amazonka.AlexaBusiness.CreateBusinessReportSchedule
import Amazonka.AlexaBusiness.CreateConferenceProvider
import Amazonka.AlexaBusiness.CreateContact
import Amazonka.AlexaBusiness.CreateGatewayGroup
import Amazonka.AlexaBusiness.CreateNetworkProfile
import Amazonka.AlexaBusiness.CreateProfile
import Amazonka.AlexaBusiness.CreateRoom
import Amazonka.AlexaBusiness.CreateSkillGroup
import Amazonka.AlexaBusiness.CreateUser
import Amazonka.AlexaBusiness.DeleteAddressBook
import Amazonka.AlexaBusiness.DeleteBusinessReportSchedule
import Amazonka.AlexaBusiness.DeleteConferenceProvider
import Amazonka.AlexaBusiness.DeleteContact
import Amazonka.AlexaBusiness.DeleteDevice
import Amazonka.AlexaBusiness.DeleteDeviceUsageData
import Amazonka.AlexaBusiness.DeleteGatewayGroup
import Amazonka.AlexaBusiness.DeleteNetworkProfile
import Amazonka.AlexaBusiness.DeleteProfile
import Amazonka.AlexaBusiness.DeleteRoom
import Amazonka.AlexaBusiness.DeleteRoomSkillParameter
import Amazonka.AlexaBusiness.DeleteSkillAuthorization
import Amazonka.AlexaBusiness.DeleteSkillGroup
import Amazonka.AlexaBusiness.DeleteUser
import Amazonka.AlexaBusiness.DisassociateContactFromAddressBook
import Amazonka.AlexaBusiness.DisassociateDeviceFromRoom
import Amazonka.AlexaBusiness.DisassociateSkillFromSkillGroup
import Amazonka.AlexaBusiness.DisassociateSkillFromUsers
import Amazonka.AlexaBusiness.DisassociateSkillGroupFromRoom
import Amazonka.AlexaBusiness.ForgetSmartHomeAppliances
import Amazonka.AlexaBusiness.GetAddressBook
import Amazonka.AlexaBusiness.GetConferencePreference
import Amazonka.AlexaBusiness.GetConferenceProvider
import Amazonka.AlexaBusiness.GetContact
import Amazonka.AlexaBusiness.GetDevice
import Amazonka.AlexaBusiness.GetGateway
import Amazonka.AlexaBusiness.GetGatewayGroup
import Amazonka.AlexaBusiness.GetInvitationConfiguration
import Amazonka.AlexaBusiness.GetNetworkProfile
import Amazonka.AlexaBusiness.GetProfile
import Amazonka.AlexaBusiness.GetRoom
import Amazonka.AlexaBusiness.GetRoomSkillParameter
import Amazonka.AlexaBusiness.GetSkillGroup
import Amazonka.AlexaBusiness.Lens
import Amazonka.AlexaBusiness.ListBusinessReportSchedules
import Amazonka.AlexaBusiness.ListConferenceProviders
import Amazonka.AlexaBusiness.ListDeviceEvents
import Amazonka.AlexaBusiness.ListGatewayGroups
import Amazonka.AlexaBusiness.ListGateways
import Amazonka.AlexaBusiness.ListSkills
import Amazonka.AlexaBusiness.ListSkillsStoreCategories
import Amazonka.AlexaBusiness.ListSkillsStoreSkillsByCategory
import Amazonka.AlexaBusiness.ListSmartHomeAppliances
import Amazonka.AlexaBusiness.ListTags
import Amazonka.AlexaBusiness.PutConferencePreference
import Amazonka.AlexaBusiness.PutInvitationConfiguration
import Amazonka.AlexaBusiness.PutRoomSkillParameter
import Amazonka.AlexaBusiness.PutSkillAuthorization
import Amazonka.AlexaBusiness.RegisterAVSDevice
import Amazonka.AlexaBusiness.RejectSkill
import Amazonka.AlexaBusiness.ResolveRoom
import Amazonka.AlexaBusiness.RevokeInvitation
import Amazonka.AlexaBusiness.SearchAddressBooks
import Amazonka.AlexaBusiness.SearchContacts
import Amazonka.AlexaBusiness.SearchDevices
import Amazonka.AlexaBusiness.SearchNetworkProfiles
import Amazonka.AlexaBusiness.SearchProfiles
import Amazonka.AlexaBusiness.SearchRooms
import Amazonka.AlexaBusiness.SearchSkillGroups
import Amazonka.AlexaBusiness.SearchUsers
import Amazonka.AlexaBusiness.SendAnnouncement
import Amazonka.AlexaBusiness.SendInvitation
import Amazonka.AlexaBusiness.StartDeviceSync
import Amazonka.AlexaBusiness.StartSmartHomeApplianceDiscovery
import Amazonka.AlexaBusiness.TagResource
import Amazonka.AlexaBusiness.Types
import Amazonka.AlexaBusiness.UntagResource
import Amazonka.AlexaBusiness.UpdateAddressBook
import Amazonka.AlexaBusiness.UpdateBusinessReportSchedule
import Amazonka.AlexaBusiness.UpdateConferenceProvider
import Amazonka.AlexaBusiness.UpdateContact
import Amazonka.AlexaBusiness.UpdateDevice
import Amazonka.AlexaBusiness.UpdateGateway
import Amazonka.AlexaBusiness.UpdateGatewayGroup
import Amazonka.AlexaBusiness.UpdateNetworkProfile
import Amazonka.AlexaBusiness.UpdateProfile
import Amazonka.AlexaBusiness.UpdateRoom
import Amazonka.AlexaBusiness.UpdateSkillGroup
import Amazonka.AlexaBusiness.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AlexaBusiness'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
