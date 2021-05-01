{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
module Network.AWS.AlexaBusiness
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** InvalidServiceLinkedRoleStateException
    _InvalidServiceLinkedRoleStateException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** InvalidUserStatusException
    _InvalidUserStatusException,

    -- ** ResourceAssociatedException
    _ResourceAssociatedException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** DeviceNotRegisteredException
    _DeviceNotRegisteredException,

    -- ** InvalidCertificateAuthorityException
    _InvalidCertificateAuthorityException,

    -- ** NameInUseException
    _NameInUseException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** InvalidDeviceException
    _InvalidDeviceException,

    -- ** SkillNotLinkedException
    _SkillNotLinkedException,

    -- ** InvalidSecretsManagerResourceException
    _InvalidSecretsManagerResourceException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StartDeviceSync
    StartDeviceSync (StartDeviceSync'),
    newStartDeviceSync,
    StartDeviceSyncResponse (StartDeviceSyncResponse'),
    newStartDeviceSyncResponse,

    -- ** CreateProfile
    CreateProfile (CreateProfile'),
    newCreateProfile,
    CreateProfileResponse (CreateProfileResponse'),
    newCreateProfileResponse,

    -- ** CreateContact
    CreateContact (CreateContact'),
    newCreateContact,
    CreateContactResponse (CreateContactResponse'),
    newCreateContactResponse,

    -- ** DeleteBusinessReportSchedule
    DeleteBusinessReportSchedule (DeleteBusinessReportSchedule'),
    newDeleteBusinessReportSchedule,
    DeleteBusinessReportScheduleResponse (DeleteBusinessReportScheduleResponse'),
    newDeleteBusinessReportScheduleResponse,

    -- ** UpdateNetworkProfile
    UpdateNetworkProfile (UpdateNetworkProfile'),
    newUpdateNetworkProfile,
    UpdateNetworkProfileResponse (UpdateNetworkProfileResponse'),
    newUpdateNetworkProfileResponse,

    -- ** ListBusinessReportSchedules (Paginated)
    ListBusinessReportSchedules (ListBusinessReportSchedules'),
    newListBusinessReportSchedules,
    ListBusinessReportSchedulesResponse (ListBusinessReportSchedulesResponse'),
    newListBusinessReportSchedulesResponse,

    -- ** DeleteNetworkProfile
    DeleteNetworkProfile (DeleteNetworkProfile'),
    newDeleteNetworkProfile,
    DeleteNetworkProfileResponse (DeleteNetworkProfileResponse'),
    newDeleteNetworkProfileResponse,

    -- ** UpdateBusinessReportSchedule
    UpdateBusinessReportSchedule (UpdateBusinessReportSchedule'),
    newUpdateBusinessReportSchedule,
    UpdateBusinessReportScheduleResponse (UpdateBusinessReportScheduleResponse'),
    newUpdateBusinessReportScheduleResponse,

    -- ** DeleteDeviceUsageData
    DeleteDeviceUsageData (DeleteDeviceUsageData'),
    newDeleteDeviceUsageData,
    DeleteDeviceUsageDataResponse (DeleteDeviceUsageDataResponse'),
    newDeleteDeviceUsageDataResponse,

    -- ** GetConferenceProvider
    GetConferenceProvider (GetConferenceProvider'),
    newGetConferenceProvider,
    GetConferenceProviderResponse (GetConferenceProviderResponse'),
    newGetConferenceProviderResponse,

    -- ** GetGatewayGroup
    GetGatewayGroup (GetGatewayGroup'),
    newGetGatewayGroup,
    GetGatewayGroupResponse (GetGatewayGroupResponse'),
    newGetGatewayGroupResponse,

    -- ** GetRoom
    GetRoom (GetRoom'),
    newGetRoom,
    GetRoomResponse (GetRoomResponse'),
    newGetRoomResponse,

    -- ** CreateNetworkProfile
    CreateNetworkProfile (CreateNetworkProfile'),
    newCreateNetworkProfile,
    CreateNetworkProfileResponse (CreateNetworkProfileResponse'),
    newCreateNetworkProfileResponse,

    -- ** ListSkillsStoreCategories (Paginated)
    ListSkillsStoreCategories (ListSkillsStoreCategories'),
    newListSkillsStoreCategories,
    ListSkillsStoreCategoriesResponse (ListSkillsStoreCategoriesResponse'),
    newListSkillsStoreCategoriesResponse,

    -- ** CreateBusinessReportSchedule
    CreateBusinessReportSchedule (CreateBusinessReportSchedule'),
    newCreateBusinessReportSchedule,
    CreateBusinessReportScheduleResponse (CreateBusinessReportScheduleResponse'),
    newCreateBusinessReportScheduleResponse,

    -- ** GetAddressBook
    GetAddressBook (GetAddressBook'),
    newGetAddressBook,
    GetAddressBookResponse (GetAddressBookResponse'),
    newGetAddressBookResponse,

    -- ** AssociateContactWithAddressBook
    AssociateContactWithAddressBook (AssociateContactWithAddressBook'),
    newAssociateContactWithAddressBook,
    AssociateContactWithAddressBookResponse (AssociateContactWithAddressBookResponse'),
    newAssociateContactWithAddressBookResponse,

    -- ** GetDevice
    GetDevice (GetDevice'),
    newGetDevice,
    GetDeviceResponse (GetDeviceResponse'),
    newGetDeviceResponse,

    -- ** DeleteRoomSkillParameter
    DeleteRoomSkillParameter (DeleteRoomSkillParameter'),
    newDeleteRoomSkillParameter,
    DeleteRoomSkillParameterResponse (DeleteRoomSkillParameterResponse'),
    newDeleteRoomSkillParameterResponse,

    -- ** ListSkillsStoreSkillsByCategory (Paginated)
    ListSkillsStoreSkillsByCategory (ListSkillsStoreSkillsByCategory'),
    newListSkillsStoreSkillsByCategory,
    ListSkillsStoreSkillsByCategoryResponse (ListSkillsStoreSkillsByCategoryResponse'),
    newListSkillsStoreSkillsByCategoryResponse,

    -- ** SearchProfiles (Paginated)
    SearchProfiles (SearchProfiles'),
    newSearchProfiles,
    SearchProfilesResponse (SearchProfilesResponse'),
    newSearchProfilesResponse,

    -- ** DeleteAddressBook
    DeleteAddressBook (DeleteAddressBook'),
    newDeleteAddressBook,
    DeleteAddressBookResponse (DeleteAddressBookResponse'),
    newDeleteAddressBookResponse,

    -- ** UpdateAddressBook
    UpdateAddressBook (UpdateAddressBook'),
    newUpdateAddressBook,
    UpdateAddressBookResponse (UpdateAddressBookResponse'),
    newUpdateAddressBookResponse,

    -- ** SearchSkillGroups (Paginated)
    SearchSkillGroups (SearchSkillGroups'),
    newSearchSkillGroups,
    SearchSkillGroupsResponse (SearchSkillGroupsResponse'),
    newSearchSkillGroupsResponse,

    -- ** ResolveRoom
    ResolveRoom (ResolveRoom'),
    newResolveRoom,
    ResolveRoomResponse (ResolveRoomResponse'),
    newResolveRoomResponse,

    -- ** PutSkillAuthorization
    PutSkillAuthorization (PutSkillAuthorization'),
    newPutSkillAuthorization,
    PutSkillAuthorizationResponse (PutSkillAuthorizationResponse'),
    newPutSkillAuthorizationResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DisassociateContactFromAddressBook
    DisassociateContactFromAddressBook (DisassociateContactFromAddressBook'),
    newDisassociateContactFromAddressBook,
    DisassociateContactFromAddressBookResponse (DisassociateContactFromAddressBookResponse'),
    newDisassociateContactFromAddressBookResponse,

    -- ** AssociateDeviceWithNetworkProfile
    AssociateDeviceWithNetworkProfile (AssociateDeviceWithNetworkProfile'),
    newAssociateDeviceWithNetworkProfile,
    AssociateDeviceWithNetworkProfileResponse (AssociateDeviceWithNetworkProfileResponse'),
    newAssociateDeviceWithNetworkProfileResponse,

    -- ** SearchNetworkProfiles
    SearchNetworkProfiles (SearchNetworkProfiles'),
    newSearchNetworkProfiles,
    SearchNetworkProfilesResponse (SearchNetworkProfilesResponse'),
    newSearchNetworkProfilesResponse,

    -- ** GetSkillGroup
    GetSkillGroup (GetSkillGroup'),
    newGetSkillGroup,
    GetSkillGroupResponse (GetSkillGroupResponse'),
    newGetSkillGroupResponse,

    -- ** PutInvitationConfiguration
    PutInvitationConfiguration (PutInvitationConfiguration'),
    newPutInvitationConfiguration,
    PutInvitationConfigurationResponse (PutInvitationConfigurationResponse'),
    newPutInvitationConfigurationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListDeviceEvents (Paginated)
    ListDeviceEvents (ListDeviceEvents'),
    newListDeviceEvents,
    ListDeviceEventsResponse (ListDeviceEventsResponse'),
    newListDeviceEventsResponse,

    -- ** SendAnnouncement
    SendAnnouncement (SendAnnouncement'),
    newSendAnnouncement,
    SendAnnouncementResponse (SendAnnouncementResponse'),
    newSendAnnouncementResponse,

    -- ** DisassociateSkillGroupFromRoom
    DisassociateSkillGroupFromRoom (DisassociateSkillGroupFromRoom'),
    newDisassociateSkillGroupFromRoom,
    DisassociateSkillGroupFromRoomResponse (DisassociateSkillGroupFromRoomResponse'),
    newDisassociateSkillGroupFromRoomResponse,

    -- ** GetProfile
    GetProfile (GetProfile'),
    newGetProfile,
    GetProfileResponse (GetProfileResponse'),
    newGetProfileResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** SearchContacts
    SearchContacts (SearchContacts'),
    newSearchContacts,
    SearchContactsResponse (SearchContactsResponse'),
    newSearchContactsResponse,

    -- ** RegisterAVSDevice
    RegisterAVSDevice (RegisterAVSDevice'),
    newRegisterAVSDevice,
    RegisterAVSDeviceResponse (RegisterAVSDeviceResponse'),
    newRegisterAVSDeviceResponse,

    -- ** SendInvitation
    SendInvitation (SendInvitation'),
    newSendInvitation,
    SendInvitationResponse (SendInvitationResponse'),
    newSendInvitationResponse,

    -- ** ForgetSmartHomeAppliances
    ForgetSmartHomeAppliances (ForgetSmartHomeAppliances'),
    newForgetSmartHomeAppliances,
    ForgetSmartHomeAppliancesResponse (ForgetSmartHomeAppliancesResponse'),
    newForgetSmartHomeAppliancesResponse,

    -- ** AssociateSkillWithUsers
    AssociateSkillWithUsers (AssociateSkillWithUsers'),
    newAssociateSkillWithUsers,
    AssociateSkillWithUsersResponse (AssociateSkillWithUsersResponse'),
    newAssociateSkillWithUsersResponse,

    -- ** GetInvitationConfiguration
    GetInvitationConfiguration (GetInvitationConfiguration'),
    newGetInvitationConfiguration,
    GetInvitationConfigurationResponse (GetInvitationConfigurationResponse'),
    newGetInvitationConfigurationResponse,

    -- ** DisassociateSkillFromUsers
    DisassociateSkillFromUsers (DisassociateSkillFromUsers'),
    newDisassociateSkillFromUsers,
    DisassociateSkillFromUsersResponse (DisassociateSkillFromUsersResponse'),
    newDisassociateSkillFromUsersResponse,

    -- ** DeleteSkillGroup
    DeleteSkillGroup (DeleteSkillGroup'),
    newDeleteSkillGroup,
    DeleteSkillGroupResponse (DeleteSkillGroupResponse'),
    newDeleteSkillGroupResponse,

    -- ** UpdateSkillGroup
    UpdateSkillGroup (UpdateSkillGroup'),
    newUpdateSkillGroup,
    UpdateSkillGroupResponse (UpdateSkillGroupResponse'),
    newUpdateSkillGroupResponse,

    -- ** AssociateSkillGroupWithRoom
    AssociateSkillGroupWithRoom (AssociateSkillGroupWithRoom'),
    newAssociateSkillGroupWithRoom,
    AssociateSkillGroupWithRoomResponse (AssociateSkillGroupWithRoomResponse'),
    newAssociateSkillGroupWithRoomResponse,

    -- ** SearchUsers (Paginated)
    SearchUsers (SearchUsers'),
    newSearchUsers,
    SearchUsersResponse (SearchUsersResponse'),
    newSearchUsersResponse,

    -- ** PutConferencePreference
    PutConferencePreference (PutConferencePreference'),
    newPutConferencePreference,
    PutConferencePreferenceResponse (PutConferencePreferenceResponse'),
    newPutConferencePreferenceResponse,

    -- ** UpdateGateway
    UpdateGateway (UpdateGateway'),
    newUpdateGateway,
    UpdateGatewayResponse (UpdateGatewayResponse'),
    newUpdateGatewayResponse,

    -- ** DeleteDevice
    DeleteDevice (DeleteDevice'),
    newDeleteDevice,
    DeleteDeviceResponse (DeleteDeviceResponse'),
    newDeleteDeviceResponse,

    -- ** RevokeInvitation
    RevokeInvitation (RevokeInvitation'),
    newRevokeInvitation,
    RevokeInvitationResponse (RevokeInvitationResponse'),
    newRevokeInvitationResponse,

    -- ** GetRoomSkillParameter
    GetRoomSkillParameter (GetRoomSkillParameter'),
    newGetRoomSkillParameter,
    GetRoomSkillParameterResponse (GetRoomSkillParameterResponse'),
    newGetRoomSkillParameterResponse,

    -- ** UpdateContact
    UpdateContact (UpdateContact'),
    newUpdateContact,
    UpdateContactResponse (UpdateContactResponse'),
    newUpdateContactResponse,

    -- ** StartSmartHomeApplianceDiscovery
    StartSmartHomeApplianceDiscovery (StartSmartHomeApplianceDiscovery'),
    newStartSmartHomeApplianceDiscovery,
    StartSmartHomeApplianceDiscoveryResponse (StartSmartHomeApplianceDiscoveryResponse'),
    newStartSmartHomeApplianceDiscoveryResponse,

    -- ** DeleteContact
    DeleteContact (DeleteContact'),
    newDeleteContact,
    DeleteContactResponse (DeleteContactResponse'),
    newDeleteContactResponse,

    -- ** UpdateDevice
    UpdateDevice (UpdateDevice'),
    newUpdateDevice,
    UpdateDeviceResponse (UpdateDeviceResponse'),
    newUpdateDeviceResponse,

    -- ** AssociateDeviceWithRoom
    AssociateDeviceWithRoom (AssociateDeviceWithRoom'),
    newAssociateDeviceWithRoom,
    AssociateDeviceWithRoomResponse (AssociateDeviceWithRoomResponse'),
    newAssociateDeviceWithRoomResponse,

    -- ** AssociateSkillWithSkillGroup
    AssociateSkillWithSkillGroup (AssociateSkillWithSkillGroup'),
    newAssociateSkillWithSkillGroup,
    AssociateSkillWithSkillGroupResponse (AssociateSkillWithSkillGroupResponse'),
    newAssociateSkillWithSkillGroupResponse,

    -- ** ListGateways
    ListGateways (ListGateways'),
    newListGateways,
    ListGatewaysResponse (ListGatewaysResponse'),
    newListGatewaysResponse,

    -- ** DeleteRoom
    DeleteRoom (DeleteRoom'),
    newDeleteRoom,
    DeleteRoomResponse (DeleteRoomResponse'),
    newDeleteRoomResponse,

    -- ** ListConferenceProviders (Paginated)
    ListConferenceProviders (ListConferenceProviders'),
    newListConferenceProviders,
    ListConferenceProvidersResponse (ListConferenceProvidersResponse'),
    newListConferenceProvidersResponse,

    -- ** DeleteGatewayGroup
    DeleteGatewayGroup (DeleteGatewayGroup'),
    newDeleteGatewayGroup,
    DeleteGatewayGroupResponse (DeleteGatewayGroupResponse'),
    newDeleteGatewayGroupResponse,

    -- ** UpdateRoom
    UpdateRoom (UpdateRoom'),
    newUpdateRoom,
    UpdateRoomResponse (UpdateRoomResponse'),
    newUpdateRoomResponse,

    -- ** DeleteConferenceProvider
    DeleteConferenceProvider (DeleteConferenceProvider'),
    newDeleteConferenceProvider,
    DeleteConferenceProviderResponse (DeleteConferenceProviderResponse'),
    newDeleteConferenceProviderResponse,

    -- ** GetGateway
    GetGateway (GetGateway'),
    newGetGateway,
    GetGatewayResponse (GetGatewayResponse'),
    newGetGatewayResponse,

    -- ** UpdateConferenceProvider
    UpdateConferenceProvider (UpdateConferenceProvider'),
    newUpdateConferenceProvider,
    UpdateConferenceProviderResponse (UpdateConferenceProviderResponse'),
    newUpdateConferenceProviderResponse,

    -- ** UpdateGatewayGroup
    UpdateGatewayGroup (UpdateGatewayGroup'),
    newUpdateGatewayGroup,
    UpdateGatewayGroupResponse (UpdateGatewayGroupResponse'),
    newUpdateGatewayGroupResponse,

    -- ** ListGatewayGroups
    ListGatewayGroups (ListGatewayGroups'),
    newListGatewayGroups,
    ListGatewayGroupsResponse (ListGatewayGroupsResponse'),
    newListGatewayGroupsResponse,

    -- ** ApproveSkill
    ApproveSkill (ApproveSkill'),
    newApproveSkill,
    ApproveSkillResponse (ApproveSkillResponse'),
    newApproveSkillResponse,

    -- ** GetContact
    GetContact (GetContact'),
    newGetContact,
    GetContactResponse (GetContactResponse'),
    newGetContactResponse,

    -- ** RejectSkill
    RejectSkill (RejectSkill'),
    newRejectSkill,
    RejectSkillResponse (RejectSkillResponse'),
    newRejectSkillResponse,

    -- ** PutRoomSkillParameter
    PutRoomSkillParameter (PutRoomSkillParameter'),
    newPutRoomSkillParameter,
    PutRoomSkillParameterResponse (PutRoomSkillParameterResponse'),
    newPutRoomSkillParameterResponse,

    -- ** DisassociateDeviceFromRoom
    DisassociateDeviceFromRoom (DisassociateDeviceFromRoom'),
    newDisassociateDeviceFromRoom,
    DisassociateDeviceFromRoomResponse (DisassociateDeviceFromRoomResponse'),
    newDisassociateDeviceFromRoomResponse,

    -- ** CreateAddressBook
    CreateAddressBook (CreateAddressBook'),
    newCreateAddressBook,
    CreateAddressBookResponse (CreateAddressBookResponse'),
    newCreateAddressBookResponse,

    -- ** CreateRoom
    CreateRoom (CreateRoom'),
    newCreateRoom,
    CreateRoomResponse (CreateRoomResponse'),
    newCreateRoomResponse,

    -- ** CreateConferenceProvider
    CreateConferenceProvider (CreateConferenceProvider'),
    newCreateConferenceProvider,
    CreateConferenceProviderResponse (CreateConferenceProviderResponse'),
    newCreateConferenceProviderResponse,

    -- ** GetNetworkProfile
    GetNetworkProfile (GetNetworkProfile'),
    newGetNetworkProfile,
    GetNetworkProfileResponse (GetNetworkProfileResponse'),
    newGetNetworkProfileResponse,

    -- ** GetConferencePreference
    GetConferencePreference (GetConferencePreference'),
    newGetConferencePreference,
    GetConferencePreferenceResponse (GetConferencePreferenceResponse'),
    newGetConferencePreferenceResponse,

    -- ** DeleteSkillAuthorization
    DeleteSkillAuthorization (DeleteSkillAuthorization'),
    newDeleteSkillAuthorization,
    DeleteSkillAuthorizationResponse (DeleteSkillAuthorizationResponse'),
    newDeleteSkillAuthorizationResponse,

    -- ** CreateGatewayGroup
    CreateGatewayGroup (CreateGatewayGroup'),
    newCreateGatewayGroup,
    CreateGatewayGroupResponse (CreateGatewayGroupResponse'),
    newCreateGatewayGroupResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** DisassociateSkillFromSkillGroup
    DisassociateSkillFromSkillGroup (DisassociateSkillFromSkillGroup'),
    newDisassociateSkillFromSkillGroup,
    DisassociateSkillFromSkillGroupResponse (DisassociateSkillFromSkillGroupResponse'),
    newDisassociateSkillFromSkillGroupResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** ListSkills (Paginated)
    ListSkills (ListSkills'),
    newListSkills,
    ListSkillsResponse (ListSkillsResponse'),
    newListSkillsResponse,

    -- ** SearchDevices (Paginated)
    SearchDevices (SearchDevices'),
    newSearchDevices,
    SearchDevicesResponse (SearchDevicesResponse'),
    newSearchDevicesResponse,

    -- ** SearchRooms (Paginated)
    SearchRooms (SearchRooms'),
    newSearchRooms,
    SearchRoomsResponse (SearchRoomsResponse'),
    newSearchRoomsResponse,

    -- ** SearchAddressBooks
    SearchAddressBooks (SearchAddressBooks'),
    newSearchAddressBooks,
    SearchAddressBooksResponse (SearchAddressBooksResponse'),
    newSearchAddressBooksResponse,

    -- ** ListSmartHomeAppliances (Paginated)
    ListSmartHomeAppliances (ListSmartHomeAppliances'),
    newListSmartHomeAppliances,
    ListSmartHomeAppliancesResponse (ListSmartHomeAppliancesResponse'),
    newListSmartHomeAppliancesResponse,

    -- ** DeleteProfile
    DeleteProfile (DeleteProfile'),
    newDeleteProfile,
    DeleteProfileResponse (DeleteProfileResponse'),
    newDeleteProfileResponse,

    -- ** CreateSkillGroup
    CreateSkillGroup (CreateSkillGroup'),
    newCreateSkillGroup,
    CreateSkillGroupResponse (CreateSkillGroupResponse'),
    newCreateSkillGroupResponse,

    -- ** UpdateProfile
    UpdateProfile (UpdateProfile'),
    newUpdateProfile,
    UpdateProfileResponse (UpdateProfileResponse'),
    newUpdateProfileResponse,

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

import Network.AWS.AlexaBusiness.ApproveSkill
import Network.AWS.AlexaBusiness.AssociateContactWithAddressBook
import Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile
import Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
import Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
import Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
import Network.AWS.AlexaBusiness.AssociateSkillWithUsers
import Network.AWS.AlexaBusiness.CreateAddressBook
import Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
import Network.AWS.AlexaBusiness.CreateConferenceProvider
import Network.AWS.AlexaBusiness.CreateContact
import Network.AWS.AlexaBusiness.CreateGatewayGroup
import Network.AWS.AlexaBusiness.CreateNetworkProfile
import Network.AWS.AlexaBusiness.CreateProfile
import Network.AWS.AlexaBusiness.CreateRoom
import Network.AWS.AlexaBusiness.CreateSkillGroup
import Network.AWS.AlexaBusiness.CreateUser
import Network.AWS.AlexaBusiness.DeleteAddressBook
import Network.AWS.AlexaBusiness.DeleteBusinessReportSchedule
import Network.AWS.AlexaBusiness.DeleteConferenceProvider
import Network.AWS.AlexaBusiness.DeleteContact
import Network.AWS.AlexaBusiness.DeleteDevice
import Network.AWS.AlexaBusiness.DeleteDeviceUsageData
import Network.AWS.AlexaBusiness.DeleteGatewayGroup
import Network.AWS.AlexaBusiness.DeleteNetworkProfile
import Network.AWS.AlexaBusiness.DeleteProfile
import Network.AWS.AlexaBusiness.DeleteRoom
import Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
import Network.AWS.AlexaBusiness.DeleteSkillAuthorization
import Network.AWS.AlexaBusiness.DeleteSkillGroup
import Network.AWS.AlexaBusiness.DeleteUser
import Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
import Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
import Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
import Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
import Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
import Network.AWS.AlexaBusiness.ForgetSmartHomeAppliances
import Network.AWS.AlexaBusiness.GetAddressBook
import Network.AWS.AlexaBusiness.GetConferencePreference
import Network.AWS.AlexaBusiness.GetConferenceProvider
import Network.AWS.AlexaBusiness.GetContact
import Network.AWS.AlexaBusiness.GetDevice
import Network.AWS.AlexaBusiness.GetGateway
import Network.AWS.AlexaBusiness.GetGatewayGroup
import Network.AWS.AlexaBusiness.GetInvitationConfiguration
import Network.AWS.AlexaBusiness.GetNetworkProfile
import Network.AWS.AlexaBusiness.GetProfile
import Network.AWS.AlexaBusiness.GetRoom
import Network.AWS.AlexaBusiness.GetRoomSkillParameter
import Network.AWS.AlexaBusiness.GetSkillGroup
import Network.AWS.AlexaBusiness.Lens
import Network.AWS.AlexaBusiness.ListBusinessReportSchedules
import Network.AWS.AlexaBusiness.ListConferenceProviders
import Network.AWS.AlexaBusiness.ListDeviceEvents
import Network.AWS.AlexaBusiness.ListGatewayGroups
import Network.AWS.AlexaBusiness.ListGateways
import Network.AWS.AlexaBusiness.ListSkills
import Network.AWS.AlexaBusiness.ListSkillsStoreCategories
import Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory
import Network.AWS.AlexaBusiness.ListSmartHomeAppliances
import Network.AWS.AlexaBusiness.ListTags
import Network.AWS.AlexaBusiness.PutConferencePreference
import Network.AWS.AlexaBusiness.PutInvitationConfiguration
import Network.AWS.AlexaBusiness.PutRoomSkillParameter
import Network.AWS.AlexaBusiness.PutSkillAuthorization
import Network.AWS.AlexaBusiness.RegisterAVSDevice
import Network.AWS.AlexaBusiness.RejectSkill
import Network.AWS.AlexaBusiness.ResolveRoom
import Network.AWS.AlexaBusiness.RevokeInvitation
import Network.AWS.AlexaBusiness.SearchAddressBooks
import Network.AWS.AlexaBusiness.SearchContacts
import Network.AWS.AlexaBusiness.SearchDevices
import Network.AWS.AlexaBusiness.SearchNetworkProfiles
import Network.AWS.AlexaBusiness.SearchProfiles
import Network.AWS.AlexaBusiness.SearchRooms
import Network.AWS.AlexaBusiness.SearchSkillGroups
import Network.AWS.AlexaBusiness.SearchUsers
import Network.AWS.AlexaBusiness.SendAnnouncement
import Network.AWS.AlexaBusiness.SendInvitation
import Network.AWS.AlexaBusiness.StartDeviceSync
import Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
import Network.AWS.AlexaBusiness.TagResource
import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.UntagResource
import Network.AWS.AlexaBusiness.UpdateAddressBook
import Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
import Network.AWS.AlexaBusiness.UpdateConferenceProvider
import Network.AWS.AlexaBusiness.UpdateContact
import Network.AWS.AlexaBusiness.UpdateDevice
import Network.AWS.AlexaBusiness.UpdateGateway
import Network.AWS.AlexaBusiness.UpdateGatewayGroup
import Network.AWS.AlexaBusiness.UpdateNetworkProfile
import Network.AWS.AlexaBusiness.UpdateProfile
import Network.AWS.AlexaBusiness.UpdateRoom
import Network.AWS.AlexaBusiness.UpdateSkillGroup
import Network.AWS.AlexaBusiness.Waiters

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
