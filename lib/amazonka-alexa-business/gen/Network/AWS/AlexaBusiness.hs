{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Alexa for Business helps you use Alexa in your organization. Alexa for Business provides you with the tools to manage Alexa devices, enroll your users, and assign skills, at scale. You can build your own context-aware voice skills using the Alexa Skills Kit and the Alexa for Business API operations. You can also make these available as private skills for your organization. Alexa for Business makes it efficient to voice-enable your products and services, thus providing context-aware voice experiences for your customers. Device makers building with the Alexa Voice Service (AVS) can create fully integrated solutions, register their products with Alexa for Business, and manage them as shared devices in their organization.
module Network.AWS.AlexaBusiness
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** SkillNotLinkedException
    _SkillNotLinkedException,

    -- ** InvalidCertificateAuthorityException
    _InvalidCertificateAuthorityException,

    -- ** DeviceNotRegisteredException
    _DeviceNotRegisteredException,

    -- ** ResourceAssociatedException
    _ResourceAssociatedException,

    -- ** InvalidUserStatusException
    _InvalidUserStatusException,

    -- ** InvalidDeviceException
    _InvalidDeviceException,

    -- ** InvalidServiceLinkedRoleStateException
    _InvalidServiceLinkedRoleStateException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** NameInUseException
    _NameInUseException,

    -- ** InvalidSecretsManagerResourceException
    _InvalidSecretsManagerResourceException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** SearchUsers (Paginated)
    module Network.AWS.AlexaBusiness.SearchUsers,

    -- ** PutConferencePreference
    module Network.AWS.AlexaBusiness.PutConferencePreference,

    -- ** UpdateNetworkProfile
    module Network.AWS.AlexaBusiness.UpdateNetworkProfile,

    -- ** DeleteNetworkProfile
    module Network.AWS.AlexaBusiness.DeleteNetworkProfile,

    -- ** UpdateBusinessReportSchedule
    module Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule,

    -- ** DeleteBusinessReportSchedule
    module Network.AWS.AlexaBusiness.DeleteBusinessReportSchedule,

    -- ** AssociateSkillGroupWithRoom
    module Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom,

    -- ** ListSmartHomeAppliances (Paginated)
    module Network.AWS.AlexaBusiness.ListSmartHomeAppliances,

    -- ** DeleteProfile
    module Network.AWS.AlexaBusiness.DeleteProfile,

    -- ** UpdateProfile
    module Network.AWS.AlexaBusiness.UpdateProfile,

    -- ** SearchRooms (Paginated)
    module Network.AWS.AlexaBusiness.SearchRooms,

    -- ** AssociateSkillWithUsers
    module Network.AWS.AlexaBusiness.AssociateSkillWithUsers,

    -- ** RegisterAVSDevice
    module Network.AWS.AlexaBusiness.RegisterAVSDevice,

    -- ** ForgetSmartHomeAppliances
    module Network.AWS.AlexaBusiness.ForgetSmartHomeAppliances,

    -- ** PutInvitationConfiguration
    module Network.AWS.AlexaBusiness.PutInvitationConfiguration,

    -- ** DisassociateContactFromAddressBook
    module Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook,

    -- ** GetNetworkProfile
    module Network.AWS.AlexaBusiness.GetNetworkProfile,

    -- ** GetConferencePreference
    module Network.AWS.AlexaBusiness.GetConferencePreference,

    -- ** DisassociateSkillFromSkillGroup
    module Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup,

    -- ** CreateAddressBook
    module Network.AWS.AlexaBusiness.CreateAddressBook,

    -- ** DeleteAddressBook
    module Network.AWS.AlexaBusiness.DeleteAddressBook,

    -- ** UpdateAddressBook
    module Network.AWS.AlexaBusiness.UpdateAddressBook,

    -- ** DeleteGatewayGroup
    module Network.AWS.AlexaBusiness.DeleteGatewayGroup,

    -- ** UpdateGatewayGroup
    module Network.AWS.AlexaBusiness.UpdateGatewayGroup,

    -- ** UpdateRoom
    module Network.AWS.AlexaBusiness.UpdateRoom,

    -- ** DeleteRoom
    module Network.AWS.AlexaBusiness.DeleteRoom,

    -- ** GetDevice
    module Network.AWS.AlexaBusiness.GetDevice,

    -- ** GetGateway
    module Network.AWS.AlexaBusiness.GetGateway,

    -- ** ListSkillsStoreSkillsByCategory (Paginated)
    module Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory,

    -- ** DeleteConferenceProvider
    module Network.AWS.AlexaBusiness.DeleteConferenceProvider,

    -- ** UpdateConferenceProvider
    module Network.AWS.AlexaBusiness.UpdateConferenceProvider,

    -- ** GetContact
    module Network.AWS.AlexaBusiness.GetContact,

    -- ** ApproveSkill
    module Network.AWS.AlexaBusiness.ApproveSkill,

    -- ** CreateNetworkProfile
    module Network.AWS.AlexaBusiness.CreateNetworkProfile,

    -- ** AssociateDeviceWithRoom
    module Network.AWS.AlexaBusiness.AssociateDeviceWithRoom,

    -- ** GetRoomSkillParameter
    module Network.AWS.AlexaBusiness.GetRoomSkillParameter,

    -- ** UpdateGateway
    module Network.AWS.AlexaBusiness.UpdateGateway,

    -- ** CreateBusinessReportSchedule
    module Network.AWS.AlexaBusiness.CreateBusinessReportSchedule,

    -- ** DeleteContact
    module Network.AWS.AlexaBusiness.DeleteContact,

    -- ** UpdateContact
    module Network.AWS.AlexaBusiness.UpdateContact,

    -- ** GetAddressBook
    module Network.AWS.AlexaBusiness.GetAddressBook,

    -- ** ListBusinessReportSchedules (Paginated)
    module Network.AWS.AlexaBusiness.ListBusinessReportSchedules,

    -- ** DeleteDeviceUsageData
    module Network.AWS.AlexaBusiness.DeleteDeviceUsageData,

    -- ** CreateContact
    module Network.AWS.AlexaBusiness.CreateContact,

    -- ** CreateProfile
    module Network.AWS.AlexaBusiness.CreateProfile,

    -- ** DeleteSkillGroup
    module Network.AWS.AlexaBusiness.DeleteSkillGroup,

    -- ** UpdateSkillGroup
    module Network.AWS.AlexaBusiness.UpdateSkillGroup,

    -- ** StartDeviceSync
    module Network.AWS.AlexaBusiness.StartDeviceSync,

    -- ** GetInvitationConfiguration
    module Network.AWS.AlexaBusiness.GetInvitationConfiguration,

    -- ** DisassociateSkillFromUsers
    module Network.AWS.AlexaBusiness.DisassociateSkillFromUsers,

    -- ** SearchAddressBooks
    module Network.AWS.AlexaBusiness.SearchAddressBooks,

    -- ** CreateSkillGroup
    module Network.AWS.AlexaBusiness.CreateSkillGroup,

    -- ** GetProfile
    module Network.AWS.AlexaBusiness.GetProfile,

    -- ** DisassociateSkillGroupFromRoom
    module Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom,

    -- ** SendInvitation
    module Network.AWS.AlexaBusiness.SendInvitation,

    -- ** ListDeviceEvents (Paginated)
    module Network.AWS.AlexaBusiness.ListDeviceEvents,

    -- ** CreateUser
    module Network.AWS.AlexaBusiness.CreateUser,

    -- ** SearchDevices (Paginated)
    module Network.AWS.AlexaBusiness.SearchDevices,

    -- ** SearchContacts
    module Network.AWS.AlexaBusiness.SearchContacts,

    -- ** SendAnnouncement
    module Network.AWS.AlexaBusiness.SendAnnouncement,

    -- ** DeleteUser
    module Network.AWS.AlexaBusiness.DeleteUser,

    -- ** SearchNetworkProfiles
    module Network.AWS.AlexaBusiness.SearchNetworkProfiles,

    -- ** GetSkillGroup
    module Network.AWS.AlexaBusiness.GetSkillGroup,

    -- ** ListSkills (Paginated)
    module Network.AWS.AlexaBusiness.ListSkills,

    -- ** TagResource
    module Network.AWS.AlexaBusiness.TagResource,

    -- ** DisassociateDeviceFromRoom
    module Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom,

    -- ** SearchSkillGroups (Paginated)
    module Network.AWS.AlexaBusiness.SearchSkillGroups,

    -- ** PutSkillAuthorization
    module Network.AWS.AlexaBusiness.PutSkillAuthorization,

    -- ** ListTags (Paginated)
    module Network.AWS.AlexaBusiness.ListTags,

    -- ** DeleteSkillAuthorization
    module Network.AWS.AlexaBusiness.DeleteSkillAuthorization,

    -- ** AssociateDeviceWithNetworkProfile
    module Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile,

    -- ** UntagResource
    module Network.AWS.AlexaBusiness.UntagResource,

    -- ** CreateConferenceProvider
    module Network.AWS.AlexaBusiness.CreateConferenceProvider,

    -- ** ResolveRoom
    module Network.AWS.AlexaBusiness.ResolveRoom,

    -- ** CreateGatewayGroup
    module Network.AWS.AlexaBusiness.CreateGatewayGroup,

    -- ** CreateRoom
    module Network.AWS.AlexaBusiness.CreateRoom,

    -- ** DeleteRoomSkillParameter
    module Network.AWS.AlexaBusiness.DeleteRoomSkillParameter,

    -- ** ListGatewayGroups
    module Network.AWS.AlexaBusiness.ListGatewayGroups,

    -- ** PutRoomSkillParameter
    module Network.AWS.AlexaBusiness.PutRoomSkillParameter,

    -- ** SearchProfiles (Paginated)
    module Network.AWS.AlexaBusiness.SearchProfiles,

    -- ** RejectSkill
    module Network.AWS.AlexaBusiness.RejectSkill,

    -- ** ListConferenceProviders (Paginated)
    module Network.AWS.AlexaBusiness.ListConferenceProviders,

    -- ** RevokeInvitation
    module Network.AWS.AlexaBusiness.RevokeInvitation,

    -- ** ListGateways
    module Network.AWS.AlexaBusiness.ListGateways,

    -- ** DeleteDevice
    module Network.AWS.AlexaBusiness.DeleteDevice,

    -- ** UpdateDevice
    module Network.AWS.AlexaBusiness.UpdateDevice,

    -- ** AssociateSkillWithSkillGroup
    module Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup,

    -- ** GetConferenceProvider
    module Network.AWS.AlexaBusiness.GetConferenceProvider,

    -- ** GetRoom
    module Network.AWS.AlexaBusiness.GetRoom,

    -- ** GetGatewayGroup
    module Network.AWS.AlexaBusiness.GetGatewayGroup,

    -- ** ListSkillsStoreCategories (Paginated)
    module Network.AWS.AlexaBusiness.ListSkillsStoreCategories,

    -- ** StartSmartHomeApplianceDiscovery
    module Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery,

    -- ** AssociateContactWithAddressBook
    module Network.AWS.AlexaBusiness.AssociateContactWithAddressBook,

    -- * Types

    -- ** DeviceStatus
    DeviceStatus (..),

    -- ** DeviceSerialNumberForAVS
    DeviceSerialNumberForAVS (..),

    -- ** DeviceEvent
    DeviceEvent (..),
    mkDeviceEvent,
    deTimestamp,
    deType,
    deValue,

    -- ** GatewaySummary
    GatewaySummary (..),
    mkGatewaySummary,
    gsArn,
    gsDescription,
    gsGatewayGroupArn,
    gsName,
    gsSoftwareVersion,

    -- ** SipAddress
    SipAddress (..),
    mkSipAddress,
    saUri,
    saType,

    -- ** SkillId
    SkillId (..),

    -- ** RoomSkillParameterValue
    RoomSkillParameterValue (..),

    -- ** Feature
    Feature (..),

    -- ** OrganizationName
    OrganizationName (..),

    -- ** BusinessReportRecurrence
    BusinessReportRecurrence (..),
    mkBusinessReportRecurrence,
    brrStartDate,

    -- ** Email
    Email (..),

    -- ** SkillsStoreSkill
    SkillsStoreSkill (..),
    mkSkillsStoreSkill,
    sssIconUrl,
    sssSampleUtterances,
    sssShortDescription,
    sssSkillDetails,
    sssSkillId,
    sssSkillName,
    sssSupportsLinking,

    -- ** ClientId
    ClientId (..),

    -- ** PhoneNumberType
    PhoneNumberType (..),

    -- ** ConferenceProvider
    ConferenceProvider (..),
    mkConferenceProvider,
    cpArn,
    cpIPDialIn,
    cpMeetingSetting,
    cpName,
    cpPSTNDialIn,
    cpType,

    -- ** MeetingSetting
    MeetingSetting (..),
    mkMeetingSetting,
    msRequirePin,

    -- ** CurrentWiFiPassword
    CurrentWiFiPassword (..),

    -- ** BusinessReportDownloadUrl
    BusinessReportDownloadUrl (..),

    -- ** GatewayGroup
    GatewayGroup (..),
    mkGatewayGroup,
    ggArn,
    ggDescription,
    ggName,

    -- ** Room
    Room (..),
    mkRoom,
    rDescription,
    rProfileArn,
    rProviderCalendarId,
    rRoomArn,
    rRoomName,

    -- ** BusinessReportInterval
    BusinessReportInterval (..),

    -- ** DeviceEventValue
    DeviceEventValue (..),

    -- ** RoomData
    RoomData (..),
    mkRoomData,
    rdDescription,
    rdProfileArn,
    rdProfileName,
    rdProviderCalendarId,
    rdRoomArn,
    rdRoomName,

    -- ** UpdateMeetingRoomConfiguration
    UpdateMeetingRoomConfiguration (..),
    mkUpdateMeetingRoomConfiguration,
    umrcEndOfMeetingReminder,
    umrcInstantBooking,
    umrcRequireCheckIn,
    umrcRoomUtilizationMetricsEnabled,

    -- ** FilterKey
    FilterKey (..),

    -- ** S3KeyPrefix
    S3KeyPrefix (..),

    -- ** BulletPoint
    BulletPoint (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** NetworkProfileName
    NetworkProfileName (..),

    -- ** InstantBooking
    InstantBooking (..),
    mkInstantBooking,
    ibDurationInMinutes,
    ibEnabled,

    -- ** OneClickIdDelay
    OneClickIdDelay (..),

    -- ** DeviceStatusInfo
    DeviceStatusInfo (..),
    mkDeviceStatusInfo,
    dsiConnectionStatus,
    dsiConnectionStatusUpdatedTime,
    dsiDeviceStatusDetails,

    -- ** SipType
    SipType (..),

    -- ** RequirePin
    RequirePin (..),

    -- ** BusinessReportScheduleName
    BusinessReportScheduleName (..),

    -- ** CreateMeetingRoomConfiguration
    CreateMeetingRoomConfiguration (..),
    mkCreateMeetingRoomConfiguration,
    cmrcEndOfMeetingReminder,
    cmrcInstantBooking,
    cmrcRequireCheckIn,
    cmrcRoomUtilizationMetricsEnabled,

    -- ** DistanceUnit
    DistanceUnit (..),

    -- ** SortKey
    SortKey (..),

    -- ** UpdateRequireCheckIn
    UpdateRequireCheckIn (..),
    mkUpdateRequireCheckIn,
    urciEnabled,
    urciReleaseAfterMinutes,

    -- ** BusinessReportS3Location
    BusinessReportS3Location (..),
    mkBusinessReportS3Location,
    brslBucketName,
    brslPath,

    -- ** Arn
    Arn (..),

    -- ** ProductDescription
    ProductDescription (..),

    -- ** ApplianceFriendlyName
    ApplianceFriendlyName (..),

    -- ** ReviewKey
    ReviewKey (..),

    -- ** DeviceUsageType
    DeviceUsageType (..),

    -- ** TextMessage
    TextMessage (..),
    mkTextMessage,
    tmLocale,
    tmValue,

    -- ** SkillSummary
    SkillSummary (..),
    mkSkillSummary,
    ssEnablementType,
    ssSkillId,
    ssSkillName,
    ssSkillType,
    ssSupportsLinking,

    -- ** NetworkEapMethod
    NetworkEapMethod (..),

    -- ** CustomerS3BucketName
    CustomerS3BucketName (..),

    -- ** InvocationPhrase
    InvocationPhrase (..),

    -- ** TrustAnchor
    TrustAnchor (..),

    -- ** AddressBookName
    AddressBookName (..),

    -- ** Contact
    Contact (..),
    mkContact,
    cContactArn,
    cDisplayName,
    cFirstName,
    cLastName,
    cPhoneNumber,
    cPhoneNumbers,
    cSipAddresses,

    -- ** Locale
    Locale (..),

    -- ** CreateRequireCheckIn
    CreateRequireCheckIn (..),
    mkCreateRequireCheckIn,
    crciReleaseAfterMinutes,
    crciEnabled,

    -- ** Category
    Category (..),
    mkCategory,
    cCategoryId,
    cCategoryName,

    -- ** DeveloperInfo
    DeveloperInfo (..),
    mkDeveloperInfo,
    diDeveloperName,
    diEmail,
    diPrivacyPolicy,
    diUrl,

    -- ** SkillGroupDescription
    SkillGroupDescription (..),

    -- ** Device
    Device (..),
    mkDevice,
    dDeviceArn,
    dDeviceName,
    dDeviceSerialNumber,
    dDeviceStatus,
    dDeviceStatusInfo,
    dDeviceType,
    dMacAddress,
    dNetworkProfileInfo,
    dRoomArn,
    dSoftwareVersion,

    -- ** DeviceData
    DeviceData (..),
    mkDeviceData,
    ddCreatedTime,
    ddDeviceArn,
    ddDeviceName,
    ddDeviceSerialNumber,
    ddDeviceStatus,
    ddDeviceStatusInfo,
    ddDeviceType,
    ddMacAddress,
    ddNetworkProfileArn,
    ddNetworkProfileName,
    ddRoomArn,
    ddRoomName,
    ddSoftwareVersion,

    -- ** DeviceStatusDetailCode
    DeviceStatusDetailCode (..),

    -- ** ContactData
    ContactData (..),
    mkContactData,
    cdContactArn,
    cdDisplayName,
    cdFirstName,
    cdLastName,
    cdPhoneNumber,
    cdPhoneNumbers,
    cdSipAddresses,

    -- ** Gateway
    Gateway (..),
    mkGateway,
    gArn,
    gDescription,
    gGatewayGroupArn,
    gName,
    gSoftwareVersion,

    -- ** DeviceLocale
    DeviceLocale (..),

    -- ** OneClickPinDelay
    OneClickPinDelay (..),

    -- ** NetworkSecurityType
    NetworkSecurityType (..),

    -- ** User_LastName
    User_LastName (..),

    -- ** EndUserLicenseAgreement
    EndUserLicenseAgreement (..),

    -- ** SmartHomeAppliance
    SmartHomeAppliance (..),
    mkSmartHomeAppliance,
    shaDescription,
    shaFriendlyName,
    shaManufacturerName,

    -- ** Url
    Url (..),

    -- ** AmazonId
    AmazonId (..),

    -- ** EnablementTypeFilter
    EnablementTypeFilter (..),

    -- ** Value
    Value (..),

    -- ** NetworkProfileDescription
    NetworkProfileDescription (..),

    -- ** Profile
    Profile (..),
    mkProfile,
    pAddress,
    pAddressBookArn,
    pDistanceUnit,
    pIsDefault,
    pLocale,
    pMaxVolumeLimit,
    pMeetingRoomConfiguration,
    pPSTNEnabled,
    pProfileArn,
    pProfileName,
    pSetupModeDisabled,
    pTemperatureUnit,
    pTimezone,
    pWakeWord,

    -- ** AudioLocation
    AudioLocation (..),

    -- ** ProfileData
    ProfileData (..),
    mkProfileData,
    pdAddress,
    pdDistanceUnit,
    pdIsDefault,
    pdLocale,
    pdProfileArn,
    pdProfileName,
    pdTemperatureUnit,
    pdTimezone,
    pdWakeWord,

    -- ** Address
    Address (..),

    -- ** BusinessReportContentRange
    BusinessReportContentRange (..),
    mkBusinessReportContentRange,
    brcrInterval,

    -- ** PSTNDialIn
    PSTNDialIn (..),
    mkPSTNDialIn,
    pstndiCountryCode,
    pstndiPhoneNumber,
    pstndiOneClickIdDelay,
    pstndiOneClickPinDelay,

    -- ** WakeWord
    WakeWord (..),

    -- ** CreateInstantBooking
    CreateInstantBooking (..),
    mkCreateInstantBooking,
    cibDurationInMinutes,
    cibEnabled,

    -- ** MeetingRoomConfiguration
    MeetingRoomConfiguration (..),
    mkMeetingRoomConfiguration,
    mrcEndOfMeetingReminder,
    mrcInstantBooking,
    mrcRequireCheckIn,
    mrcRoomUtilizationMetricsEnabled,

    -- ** TextValue
    TextValue (..),

    -- ** ProviderCalendarId
    ProviderCalendarId (..),

    -- ** EndOfMeetingReminder
    EndOfMeetingReminder (..),
    mkEndOfMeetingReminder,
    eomrEnabled,
    eomrReminderAtMinutes,
    eomrReminderType,

    -- ** MacAddress
    MacAddress (..),

    -- ** NextWiFiPassword
    NextWiFiPassword (..),

    -- ** TagValue
    TagValue (..),

    -- ** Content
    Content (..),
    mkContent,
    cAudioList,
    cSsmlList,
    cTextList,

    -- ** EnrollmentId
    EnrollmentId (..),

    -- ** AddressBookDescription
    AddressBookDescription (..),

    -- ** Ssml
    Ssml (..),
    mkSsml,
    sLocale,
    sValue,

    -- ** CommsProtocol
    CommsProtocol (..),

    -- ** SkillType
    SkillType (..),

    -- ** UserData
    UserData (..),
    mkUserData,
    udEmail,
    udEnrollmentId,
    udEnrollmentStatus,
    udFirstName,
    udLastName,
    udUserArn,

    -- ** UserId
    UserId (..),

    -- ** DeviceEventType
    DeviceEventType (..),

    -- ** NextToken
    NextToken (..),

    -- ** ProfileName
    ProfileName (..),

    -- ** RequireCheckIn
    RequireCheckIn (..),
    mkRequireCheckIn,
    rciEnabled,
    rciReleaseAfterMinutes,

    -- ** ShortDescription
    ShortDescription (..),

    -- ** UserCode
    UserCode (..),

    -- ** BusinessReportStatus
    BusinessReportStatus (..),

    -- ** CategoryName
    CategoryName (..),

    -- ** AddressBookData
    AddressBookData (..),
    mkAddressBookData,
    abdAddressBookArn,
    abdDescription,
    abdName,

    -- ** DeviceName
    DeviceName (..),

    -- ** Key
    Key (..),

    -- ** ConferenceProviderType
    ConferenceProviderType (..),

    -- ** RoomSkillParameter
    RoomSkillParameter (..),
    mkRoomSkillParameter,
    rspParameterKey,
    rspParameterValue,

    -- ** PhoneNumber
    PhoneNumber (..),
    mkPhoneNumber,
    pnNumber,
    pnType,

    -- ** AddressBook
    AddressBook (..),
    mkAddressBook,
    abAddressBookArn,
    abDescription,
    abName,

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** Sort
    Sort (..),
    mkSort,
    sfKey,
    sfValue,

    -- ** TemperatureUnit
    TemperatureUnit (..),

    -- ** BusinessReportSchedule
    BusinessReportSchedule (..),
    mkBusinessReportSchedule,
    brsContentRange,
    brsFormat,
    brsLastBusinessReport,
    brsRecurrence,
    brsS3BucketName,
    brsS3KeyPrefix,
    brsScheduleArn,
    brsScheduleName,

    -- ** ConferencePreference
    ConferencePreference (..),
    mkConferencePreference,
    cpDefaultConferenceProviderArn,

    -- ** NetworkProfileData
    NetworkProfileData (..),
    mkNetworkProfileData,
    npdCertificateAuthorityArn,
    npdDescription,
    npdEapMethod,
    npdNetworkProfileArn,
    npdNetworkProfileName,
    npdSecurityType,
    npdSsid,

    -- ** CountryCode
    CountryCode (..),

    -- ** DeviceStatusDetail
    DeviceStatusDetail (..),
    mkDeviceStatusDetail,
    dsdCode,
    dsdFeature,

    -- ** SoftwareVersion
    SoftwareVersion (..),

    -- ** ReleaseDate
    ReleaseDate (..),

    -- ** NetworkProfile
    NetworkProfile (..),
    mkNetworkProfile,
    npCertificateAuthorityArn,
    npCurrentPassword,
    npDescription,
    npEapMethod,
    npNetworkProfileArn,
    npNetworkProfileName,
    npNextPassword,
    npSecurityType,
    npSsid,
    npTrustAnchors,

    -- ** CreateEndOfMeetingReminder
    CreateEndOfMeetingReminder (..),
    mkCreateEndOfMeetingReminder,
    ceomrReminderAtMinutes,
    ceomrReminderType,
    ceomrEnabled,

    -- ** BusinessReportFailureCode
    BusinessReportFailureCode (..),

    -- ** TagKey
    TagKey (..),

    -- ** DeviceType
    DeviceType (..),

    -- ** EnrollmentStatus
    EnrollmentStatus (..),

    -- ** UpdateEndOfMeetingReminder
    UpdateEndOfMeetingReminder (..),
    mkUpdateEndOfMeetingReminder,
    ueomrEnabled,
    ueomrReminderAtMinutes,
    ueomrReminderType,

    -- ** IconUrl
    IconUrl (..),

    -- ** GatewayGroupName
    GatewayGroupName (..),

    -- ** GenericKeyword
    GenericKeyword (..),

    -- ** DeviceRoomName
    DeviceRoomName (..),

    -- ** RoomName
    RoomName (..),

    -- ** Filter
    Filter (..),
    mkFilter,
    fKey,
    fValues,

    -- ** BusinessReport
    BusinessReport (..),
    mkBusinessReport,
    brDeliveryTime,
    brDownloadUrl,
    brFailureCode,
    brS3Location,
    brStatus,

    -- ** ConferenceProviderName
    ConferenceProviderName (..),

    -- ** DeviceNetworkProfileInfo
    DeviceNetworkProfileInfo (..),
    mkDeviceNetworkProfileInfo,
    dnpiCertificateArn,
    dnpiCertificateExpirationTime,
    dnpiNetworkProfileArn,

    -- ** FilterValue
    FilterValue (..),

    -- ** SkillDetails
    SkillDetails (..),
    mkSkillDetails,
    sdBulletPoints,
    sdDeveloperInfo,
    sdEndUserLicenseAgreement,
    sdGenericKeywords,
    sdInvocationPhrase,
    sdNewInThisVersionBulletPoints,
    sdProductDescription,
    sdReleaseDate,
    sdReviews,
    sdSkillTypes,

    -- ** EnablementType
    EnablementType (..),

    -- ** Timezone
    Timezone (..),

    -- ** RawPhoneNumber
    RawPhoneNumber (..),

    -- ** GatewayGroupSummary
    GatewayGroupSummary (..),
    mkGatewayGroupSummary,
    ggsArn,
    ggsDescription,
    ggsName,

    -- ** Endpoint
    Endpoint (..),

    -- ** BusinessReportFormat
    BusinessReportFormat (..),

    -- ** DeviceSerialNumber
    DeviceSerialNumber (..),

    -- ** OutboundPhoneNumber
    OutboundPhoneNumber (..),

    -- ** Utterance
    Utterance (..),

    -- ** ClientRequestToken
    ClientRequestToken (..),

    -- ** UpdateInstantBooking
    UpdateInstantBooking (..),
    mkUpdateInstantBooking,
    uibDurationInMinutes,
    uibEnabled,

    -- ** SkillGroupData
    SkillGroupData (..),
    mkSkillGroupData,
    sgdDescription,
    sgdSkillGroupArn,
    sgdSkillGroupName,

    -- ** SkillGroup
    SkillGroup (..),
    mkSkillGroup,
    sgDescription,
    sgSkillGroupArn,
    sgSkillGroupName,

    -- ** IPDialIn
    IPDialIn (..),
    mkIPDialIn,
    ipdiEndpoint,
    ipdiCommsProtocol,

    -- ** PrivacyPolicy
    PrivacyPolicy (..),

    -- ** ProductId
    ProductId (..),

    -- ** Audio
    Audio (..),
    mkAudio,
    aLocale,
    aLocation,

    -- ** SkillName
    SkillName (..),

    -- ** SkillStoreType
    SkillStoreType (..),

    -- ** SkillTypeFilter
    SkillTypeFilter (..),

    -- ** SortValue
    SortValue (..),

    -- ** SkillGroupName
    SkillGroupName (..),

    -- ** ReviewValue
    ReviewValue (..),

    -- ** EndOfMeetingReminderType
    EndOfMeetingReminderType (..),

    -- ** DeveloperName
    DeveloperName (..),

    -- ** RoomArn
    RoomArn (..),

    -- ** Description
    Description (..),

    -- ** GatewayGroupArn
    GatewayGroupArn (..),

    -- ** Name
    Name (..),

    -- ** Uri
    Uri (..),

    -- ** StartDate
    StartDate (..),

    -- ** AddressBookArn
    AddressBookArn (..),

    -- ** GatewayArn
    GatewayArn (..),

    -- ** ConferenceProviderArn
    ConferenceProviderArn (..),

    -- ** ContactArn
    ContactArn (..),

    -- ** DeviceArn
    DeviceArn (..),

    -- ** ProfileArn
    ProfileArn (..),

    -- ** ScheduleArn
    ScheduleArn (..),

    -- ** SkillGroupArn
    SkillGroupArn (..),

    -- ** NetworkProfileArn
    NetworkProfileArn (..),

    -- ** BucketName
    BucketName (..),

    -- ** Path
    Path (..),

    -- ** FirstName
    FirstName (..),

    -- ** DisplayName
    DisplayName (..),

    -- ** LastName
    LastName (..),

    -- ** ParameterKey
    ParameterKey (..),

    -- ** Ssid
    Ssid (..),

    -- ** NextPassword
    NextPassword (..),

    -- ** ManufacturerName
    ManufacturerName (..),

    -- ** Number
    Number (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import qualified Network.AWS.Prelude as Lude

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
