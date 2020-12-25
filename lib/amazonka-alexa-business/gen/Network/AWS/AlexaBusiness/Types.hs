-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _SkillNotLinkedException,
    _InvalidCertificateAuthorityException,
    _DeviceNotRegisteredException,
    _ResourceAssociatedException,
    _InvalidUserStatusException,
    _InvalidDeviceException,
    _InvalidServiceLinkedRoleStateException,
    _NotFoundException,
    _NameInUseException,
    _InvalidSecretsManagerResourceException,
    _ConcurrentModificationException,
    _UnauthorizedException,
    _AlreadyExistsException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * DeviceStatus
    DeviceStatus (..),

    -- * DeviceSerialNumberForAVS
    DeviceSerialNumberForAVS (..),

    -- * DeviceEvent
    DeviceEvent (..),
    mkDeviceEvent,
    deTimestamp,
    deType,
    deValue,

    -- * GatewaySummary
    GatewaySummary (..),
    mkGatewaySummary,
    gsArn,
    gsDescription,
    gsGatewayGroupArn,
    gsName,
    gsSoftwareVersion,

    -- * SipAddress
    SipAddress (..),
    mkSipAddress,
    saUri,
    saType,

    -- * SkillId
    SkillId (..),

    -- * RoomSkillParameterValue
    RoomSkillParameterValue (..),

    -- * Feature
    Feature (..),

    -- * OrganizationName
    OrganizationName (..),

    -- * BusinessReportRecurrence
    BusinessReportRecurrence (..),
    mkBusinessReportRecurrence,
    brrStartDate,

    -- * Email
    Email (..),

    -- * SkillsStoreSkill
    SkillsStoreSkill (..),
    mkSkillsStoreSkill,
    sssIconUrl,
    sssSampleUtterances,
    sssShortDescription,
    sssSkillDetails,
    sssSkillId,
    sssSkillName,
    sssSupportsLinking,

    -- * ClientId
    ClientId (..),

    -- * PhoneNumberType
    PhoneNumberType (..),

    -- * ConferenceProvider
    ConferenceProvider (..),
    mkConferenceProvider,
    cpArn,
    cpIPDialIn,
    cpMeetingSetting,
    cpName,
    cpPSTNDialIn,
    cpType,

    -- * MeetingSetting
    MeetingSetting (..),
    mkMeetingSetting,
    msRequirePin,

    -- * CurrentWiFiPassword
    CurrentWiFiPassword (..),

    -- * BusinessReportDownloadUrl
    BusinessReportDownloadUrl (..),

    -- * GatewayGroup
    GatewayGroup (..),
    mkGatewayGroup,
    ggArn,
    ggDescription,
    ggName,

    -- * Room
    Room (..),
    mkRoom,
    rDescription,
    rProfileArn,
    rProviderCalendarId,
    rRoomArn,
    rRoomName,

    -- * BusinessReportInterval
    BusinessReportInterval (..),

    -- * DeviceEventValue
    DeviceEventValue (..),

    -- * RoomData
    RoomData (..),
    mkRoomData,
    rdDescription,
    rdProfileArn,
    rdProfileName,
    rdProviderCalendarId,
    rdRoomArn,
    rdRoomName,

    -- * UpdateMeetingRoomConfiguration
    UpdateMeetingRoomConfiguration (..),
    mkUpdateMeetingRoomConfiguration,
    umrcEndOfMeetingReminder,
    umrcInstantBooking,
    umrcRequireCheckIn,
    umrcRoomUtilizationMetricsEnabled,

    -- * FilterKey
    FilterKey (..),

    -- * S3KeyPrefix
    S3KeyPrefix (..),

    -- * BulletPoint
    BulletPoint (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * NetworkProfileName
    NetworkProfileName (..),

    -- * InstantBooking
    InstantBooking (..),
    mkInstantBooking,
    ibDurationInMinutes,
    ibEnabled,

    -- * OneClickIdDelay
    OneClickIdDelay (..),

    -- * DeviceStatusInfo
    DeviceStatusInfo (..),
    mkDeviceStatusInfo,
    dsiConnectionStatus,
    dsiConnectionStatusUpdatedTime,
    dsiDeviceStatusDetails,

    -- * SipType
    SipType (..),

    -- * RequirePin
    RequirePin (..),

    -- * BusinessReportScheduleName
    BusinessReportScheduleName (..),

    -- * CreateMeetingRoomConfiguration
    CreateMeetingRoomConfiguration (..),
    mkCreateMeetingRoomConfiguration,
    cmrcEndOfMeetingReminder,
    cmrcInstantBooking,
    cmrcRequireCheckIn,
    cmrcRoomUtilizationMetricsEnabled,

    -- * DistanceUnit
    DistanceUnit (..),

    -- * SortKey
    SortKey (..),

    -- * UpdateRequireCheckIn
    UpdateRequireCheckIn (..),
    mkUpdateRequireCheckIn,
    urciEnabled,
    urciReleaseAfterMinutes,

    -- * BusinessReportS3Location
    BusinessReportS3Location (..),
    mkBusinessReportS3Location,
    brslBucketName,
    brslPath,

    -- * Arn
    Arn (..),

    -- * ProductDescription
    ProductDescription (..),

    -- * ApplianceFriendlyName
    ApplianceFriendlyName (..),

    -- * ReviewKey
    ReviewKey (..),

    -- * DeviceUsageType
    DeviceUsageType (..),

    -- * TextMessage
    TextMessage (..),
    mkTextMessage,
    tmLocale,
    tmValue,

    -- * SkillSummary
    SkillSummary (..),
    mkSkillSummary,
    ssEnablementType,
    ssSkillId,
    ssSkillName,
    ssSkillType,
    ssSupportsLinking,

    -- * NetworkEapMethod
    NetworkEapMethod (..),

    -- * CustomerS3BucketName
    CustomerS3BucketName (..),

    -- * InvocationPhrase
    InvocationPhrase (..),

    -- * TrustAnchor
    TrustAnchor (..),

    -- * AddressBookName
    AddressBookName (..),

    -- * Contact
    Contact (..),
    mkContact,
    cContactArn,
    cDisplayName,
    cFirstName,
    cLastName,
    cPhoneNumber,
    cPhoneNumbers,
    cSipAddresses,

    -- * Locale
    Locale (..),

    -- * CreateRequireCheckIn
    CreateRequireCheckIn (..),
    mkCreateRequireCheckIn,
    crciReleaseAfterMinutes,
    crciEnabled,

    -- * Category
    Category (..),
    mkCategory,
    cCategoryId,
    cCategoryName,

    -- * DeveloperInfo
    DeveloperInfo (..),
    mkDeveloperInfo,
    diDeveloperName,
    diEmail,
    diPrivacyPolicy,
    diUrl,

    -- * SkillGroupDescription
    SkillGroupDescription (..),

    -- * Device
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

    -- * DeviceData
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

    -- * DeviceStatusDetailCode
    DeviceStatusDetailCode (..),

    -- * ContactData
    ContactData (..),
    mkContactData,
    cdContactArn,
    cdDisplayName,
    cdFirstName,
    cdLastName,
    cdPhoneNumber,
    cdPhoneNumbers,
    cdSipAddresses,

    -- * Gateway
    Gateway (..),
    mkGateway,
    gArn,
    gDescription,
    gGatewayGroupArn,
    gName,
    gSoftwareVersion,

    -- * DeviceLocale
    DeviceLocale (..),

    -- * OneClickPinDelay
    OneClickPinDelay (..),

    -- * NetworkSecurityType
    NetworkSecurityType (..),

    -- * User_LastName
    User_LastName (..),

    -- * EndUserLicenseAgreement
    EndUserLicenseAgreement (..),

    -- * SmartHomeAppliance
    SmartHomeAppliance (..),
    mkSmartHomeAppliance,
    shaDescription,
    shaFriendlyName,
    shaManufacturerName,

    -- * Url
    Url (..),

    -- * AmazonId
    AmazonId (..),

    -- * EnablementTypeFilter
    EnablementTypeFilter (..),

    -- * Value
    Value (..),

    -- * NetworkProfileDescription
    NetworkProfileDescription (..),

    -- * Profile
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

    -- * AudioLocation
    AudioLocation (..),

    -- * ProfileData
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

    -- * Address
    Address (..),

    -- * BusinessReportContentRange
    BusinessReportContentRange (..),
    mkBusinessReportContentRange,
    brcrInterval,

    -- * PSTNDialIn
    PSTNDialIn (..),
    mkPSTNDialIn,
    pstndiCountryCode,
    pstndiPhoneNumber,
    pstndiOneClickIdDelay,
    pstndiOneClickPinDelay,

    -- * WakeWord
    WakeWord (..),

    -- * CreateInstantBooking
    CreateInstantBooking (..),
    mkCreateInstantBooking,
    cibDurationInMinutes,
    cibEnabled,

    -- * MeetingRoomConfiguration
    MeetingRoomConfiguration (..),
    mkMeetingRoomConfiguration,
    mrcEndOfMeetingReminder,
    mrcInstantBooking,
    mrcRequireCheckIn,
    mrcRoomUtilizationMetricsEnabled,

    -- * TextValue
    TextValue (..),

    -- * ProviderCalendarId
    ProviderCalendarId (..),

    -- * EndOfMeetingReminder
    EndOfMeetingReminder (..),
    mkEndOfMeetingReminder,
    eomrEnabled,
    eomrReminderAtMinutes,
    eomrReminderType,

    -- * MacAddress
    MacAddress (..),

    -- * NextWiFiPassword
    NextWiFiPassword (..),

    -- * TagValue
    TagValue (..),

    -- * Content
    Content (..),
    mkContent,
    cAudioList,
    cSsmlList,
    cTextList,

    -- * EnrollmentId
    EnrollmentId (..),

    -- * AddressBookDescription
    AddressBookDescription (..),

    -- * Ssml
    Ssml (..),
    mkSsml,
    sLocale,
    sValue,

    -- * CommsProtocol
    CommsProtocol (..),

    -- * SkillType
    SkillType (..),

    -- * UserData
    UserData (..),
    mkUserData,
    udEmail,
    udEnrollmentId,
    udEnrollmentStatus,
    udFirstName,
    udLastName,
    udUserArn,

    -- * UserId
    UserId (..),

    -- * DeviceEventType
    DeviceEventType (..),

    -- * NextToken
    NextToken (..),

    -- * ProfileName
    ProfileName (..),

    -- * RequireCheckIn
    RequireCheckIn (..),
    mkRequireCheckIn,
    rciEnabled,
    rciReleaseAfterMinutes,

    -- * ShortDescription
    ShortDescription (..),

    -- * UserCode
    UserCode (..),

    -- * BusinessReportStatus
    BusinessReportStatus (..),

    -- * CategoryName
    CategoryName (..),

    -- * AddressBookData
    AddressBookData (..),
    mkAddressBookData,
    abdAddressBookArn,
    abdDescription,
    abdName,

    -- * DeviceName
    DeviceName (..),

    -- * Key
    Key (..),

    -- * ConferenceProviderType
    ConferenceProviderType (..),

    -- * RoomSkillParameter
    RoomSkillParameter (..),
    mkRoomSkillParameter,
    rspParameterKey,
    rspParameterValue,

    -- * PhoneNumber
    PhoneNumber (..),
    mkPhoneNumber,
    pnNumber,
    pnType,

    -- * AddressBook
    AddressBook (..),
    mkAddressBook,
    abAddressBookArn,
    abDescription,
    abName,

    -- * ConnectionStatus
    ConnectionStatus (..),

    -- * Sort
    Sort (..),
    mkSort,
    sfKey,
    sfValue,

    -- * TemperatureUnit
    TemperatureUnit (..),

    -- * BusinessReportSchedule
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

    -- * ConferencePreference
    ConferencePreference (..),
    mkConferencePreference,
    cpDefaultConferenceProviderArn,

    -- * NetworkProfileData
    NetworkProfileData (..),
    mkNetworkProfileData,
    npdCertificateAuthorityArn,
    npdDescription,
    npdEapMethod,
    npdNetworkProfileArn,
    npdNetworkProfileName,
    npdSecurityType,
    npdSsid,

    -- * CountryCode
    CountryCode (..),

    -- * DeviceStatusDetail
    DeviceStatusDetail (..),
    mkDeviceStatusDetail,
    dsdCode,
    dsdFeature,

    -- * SoftwareVersion
    SoftwareVersion (..),

    -- * ReleaseDate
    ReleaseDate (..),

    -- * NetworkProfile
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

    -- * CreateEndOfMeetingReminder
    CreateEndOfMeetingReminder (..),
    mkCreateEndOfMeetingReminder,
    ceomrReminderAtMinutes,
    ceomrReminderType,
    ceomrEnabled,

    -- * BusinessReportFailureCode
    BusinessReportFailureCode (..),

    -- * TagKey
    TagKey (..),

    -- * DeviceType
    DeviceType (..),

    -- * EnrollmentStatus
    EnrollmentStatus (..),

    -- * UpdateEndOfMeetingReminder
    UpdateEndOfMeetingReminder (..),
    mkUpdateEndOfMeetingReminder,
    ueomrEnabled,
    ueomrReminderAtMinutes,
    ueomrReminderType,

    -- * IconUrl
    IconUrl (..),

    -- * GatewayGroupName
    GatewayGroupName (..),

    -- * GenericKeyword
    GenericKeyword (..),

    -- * DeviceRoomName
    DeviceRoomName (..),

    -- * RoomName
    RoomName (..),

    -- * Filter
    Filter (..),
    mkFilter,
    fKey,
    fValues,

    -- * BusinessReport
    BusinessReport (..),
    mkBusinessReport,
    brDeliveryTime,
    brDownloadUrl,
    brFailureCode,
    brS3Location,
    brStatus,

    -- * ConferenceProviderName
    ConferenceProviderName (..),

    -- * DeviceNetworkProfileInfo
    DeviceNetworkProfileInfo (..),
    mkDeviceNetworkProfileInfo,
    dnpiCertificateArn,
    dnpiCertificateExpirationTime,
    dnpiNetworkProfileArn,

    -- * FilterValue
    FilterValue (..),

    -- * SkillDetails
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

    -- * EnablementType
    EnablementType (..),

    -- * Timezone
    Timezone (..),

    -- * RawPhoneNumber
    RawPhoneNumber (..),

    -- * GatewayGroupSummary
    GatewayGroupSummary (..),
    mkGatewayGroupSummary,
    ggsArn,
    ggsDescription,
    ggsName,

    -- * Endpoint
    Endpoint (..),

    -- * BusinessReportFormat
    BusinessReportFormat (..),

    -- * DeviceSerialNumber
    DeviceSerialNumber (..),

    -- * OutboundPhoneNumber
    OutboundPhoneNumber (..),

    -- * Utterance
    Utterance (..),

    -- * ClientRequestToken
    ClientRequestToken (..),

    -- * UpdateInstantBooking
    UpdateInstantBooking (..),
    mkUpdateInstantBooking,
    uibDurationInMinutes,
    uibEnabled,

    -- * SkillGroupData
    SkillGroupData (..),
    mkSkillGroupData,
    sgdDescription,
    sgdSkillGroupArn,
    sgdSkillGroupName,

    -- * SkillGroup
    SkillGroup (..),
    mkSkillGroup,
    sgDescription,
    sgSkillGroupArn,
    sgSkillGroupName,

    -- * IPDialIn
    IPDialIn (..),
    mkIPDialIn,
    ipdiEndpoint,
    ipdiCommsProtocol,

    -- * PrivacyPolicy
    PrivacyPolicy (..),

    -- * ProductId
    ProductId (..),

    -- * Audio
    Audio (..),
    mkAudio,
    aLocale,
    aLocation,

    -- * SkillName
    SkillName (..),

    -- * SkillStoreType
    SkillStoreType (..),

    -- * SkillTypeFilter
    SkillTypeFilter (..),

    -- * SortValue
    SortValue (..),

    -- * SkillGroupName
    SkillGroupName (..),

    -- * ReviewValue
    ReviewValue (..),

    -- * EndOfMeetingReminderType
    EndOfMeetingReminderType (..),

    -- * DeveloperName
    DeveloperName (..),

    -- * RoomArn
    RoomArn (..),

    -- * Description
    Description (..),

    -- * GatewayGroupArn
    GatewayGroupArn (..),

    -- * Name
    Name (..),

    -- * Uri
    Uri (..),

    -- * StartDate
    StartDate (..),

    -- * AddressBookArn
    AddressBookArn (..),

    -- * GatewayArn
    GatewayArn (..),

    -- * ConferenceProviderArn
    ConferenceProviderArn (..),

    -- * ContactArn
    ContactArn (..),

    -- * DeviceArn
    DeviceArn (..),

    -- * ProfileArn
    ProfileArn (..),

    -- * ScheduleArn
    ScheduleArn (..),

    -- * SkillGroupArn
    SkillGroupArn (..),

    -- * NetworkProfileArn
    NetworkProfileArn (..),

    -- * BucketName
    BucketName (..),

    -- * Path
    Path (..),

    -- * FirstName
    FirstName (..),

    -- * DisplayName
    DisplayName (..),

    -- * LastName
    LastName (..),

    -- * ParameterKey
    ParameterKey (..),

    -- * Ssid
    Ssid (..),

    -- * NextPassword
    NextPassword (..),

    -- * ManufacturerName
    ManufacturerName (..),

    -- * Number
    Number (..),
  )
where

import Network.AWS.AlexaBusiness.Types.Address
import Network.AWS.AlexaBusiness.Types.AddressBook
import Network.AWS.AlexaBusiness.Types.AddressBookArn
import Network.AWS.AlexaBusiness.Types.AddressBookData
import Network.AWS.AlexaBusiness.Types.AddressBookDescription
import Network.AWS.AlexaBusiness.Types.AddressBookName
import Network.AWS.AlexaBusiness.Types.AmazonId
import Network.AWS.AlexaBusiness.Types.ApplianceFriendlyName
import Network.AWS.AlexaBusiness.Types.Arn
import Network.AWS.AlexaBusiness.Types.Audio
import Network.AWS.AlexaBusiness.Types.AudioLocation
import Network.AWS.AlexaBusiness.Types.BucketName
import Network.AWS.AlexaBusiness.Types.BulletPoint
import Network.AWS.AlexaBusiness.Types.BusinessReport
import Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
import Network.AWS.AlexaBusiness.Types.BusinessReportDownloadUrl
import Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
import Network.AWS.AlexaBusiness.Types.BusinessReportFormat
import Network.AWS.AlexaBusiness.Types.BusinessReportInterval
import Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
import Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
import Network.AWS.AlexaBusiness.Types.BusinessReportSchedule
import Network.AWS.AlexaBusiness.Types.BusinessReportScheduleName
import Network.AWS.AlexaBusiness.Types.BusinessReportStatus
import Network.AWS.AlexaBusiness.Types.Category
import Network.AWS.AlexaBusiness.Types.CategoryName
import Network.AWS.AlexaBusiness.Types.ClientId
import Network.AWS.AlexaBusiness.Types.ClientRequestToken
import Network.AWS.AlexaBusiness.Types.CommsProtocol
import Network.AWS.AlexaBusiness.Types.ConferencePreference
import Network.AWS.AlexaBusiness.Types.ConferenceProvider
import Network.AWS.AlexaBusiness.Types.ConferenceProviderArn
import Network.AWS.AlexaBusiness.Types.ConferenceProviderName
import Network.AWS.AlexaBusiness.Types.ConferenceProviderType
import Network.AWS.AlexaBusiness.Types.ConnectionStatus
import Network.AWS.AlexaBusiness.Types.Contact
import Network.AWS.AlexaBusiness.Types.ContactArn
import Network.AWS.AlexaBusiness.Types.ContactData
import Network.AWS.AlexaBusiness.Types.Content
import Network.AWS.AlexaBusiness.Types.CountryCode
import Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.CreateInstantBooking
import Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
import Network.AWS.AlexaBusiness.Types.CurrentWiFiPassword
import Network.AWS.AlexaBusiness.Types.CustomerS3BucketName
import Network.AWS.AlexaBusiness.Types.Description
import Network.AWS.AlexaBusiness.Types.DeveloperInfo
import Network.AWS.AlexaBusiness.Types.DeveloperName
import Network.AWS.AlexaBusiness.Types.Device
import Network.AWS.AlexaBusiness.Types.DeviceArn
import Network.AWS.AlexaBusiness.Types.DeviceData
import Network.AWS.AlexaBusiness.Types.DeviceEvent
import Network.AWS.AlexaBusiness.Types.DeviceEventType
import Network.AWS.AlexaBusiness.Types.DeviceEventValue
import Network.AWS.AlexaBusiness.Types.DeviceLocale
import Network.AWS.AlexaBusiness.Types.DeviceName
import Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Network.AWS.AlexaBusiness.Types.DeviceRoomName
import Network.AWS.AlexaBusiness.Types.DeviceSerialNumber
import Network.AWS.AlexaBusiness.Types.DeviceSerialNumberForAVS
import Network.AWS.AlexaBusiness.Types.DeviceStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
import Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import Network.AWS.AlexaBusiness.Types.DeviceType
import Network.AWS.AlexaBusiness.Types.DeviceUsageType
import Network.AWS.AlexaBusiness.Types.DisplayName
import Network.AWS.AlexaBusiness.Types.DistanceUnit
import Network.AWS.AlexaBusiness.Types.Email
import Network.AWS.AlexaBusiness.Types.EnablementType
import Network.AWS.AlexaBusiness.Types.EnablementTypeFilter
import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import Network.AWS.AlexaBusiness.Types.EndUserLicenseAgreement
import Network.AWS.AlexaBusiness.Types.Endpoint
import Network.AWS.AlexaBusiness.Types.EnrollmentId
import Network.AWS.AlexaBusiness.Types.EnrollmentStatus
import Network.AWS.AlexaBusiness.Types.Feature
import Network.AWS.AlexaBusiness.Types.Filter
import Network.AWS.AlexaBusiness.Types.FilterKey
import Network.AWS.AlexaBusiness.Types.FilterValue
import Network.AWS.AlexaBusiness.Types.FirstName
import Network.AWS.AlexaBusiness.Types.Gateway
import Network.AWS.AlexaBusiness.Types.GatewayArn
import Network.AWS.AlexaBusiness.Types.GatewayGroup
import Network.AWS.AlexaBusiness.Types.GatewayGroupArn
import Network.AWS.AlexaBusiness.Types.GatewayGroupName
import Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
import Network.AWS.AlexaBusiness.Types.GatewaySummary
import Network.AWS.AlexaBusiness.Types.GenericKeyword
import Network.AWS.AlexaBusiness.Types.IPDialIn
import Network.AWS.AlexaBusiness.Types.IconUrl
import Network.AWS.AlexaBusiness.Types.InstantBooking
import Network.AWS.AlexaBusiness.Types.InvocationPhrase
import Network.AWS.AlexaBusiness.Types.Key
import Network.AWS.AlexaBusiness.Types.LastName
import Network.AWS.AlexaBusiness.Types.Locale
import Network.AWS.AlexaBusiness.Types.MacAddress
import Network.AWS.AlexaBusiness.Types.ManufacturerName
import Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.MeetingSetting
import Network.AWS.AlexaBusiness.Types.Name
import Network.AWS.AlexaBusiness.Types.NetworkEapMethod
import Network.AWS.AlexaBusiness.Types.NetworkProfile
import Network.AWS.AlexaBusiness.Types.NetworkProfileArn
import Network.AWS.AlexaBusiness.Types.NetworkProfileData
import Network.AWS.AlexaBusiness.Types.NetworkProfileDescription
import Network.AWS.AlexaBusiness.Types.NetworkProfileName
import Network.AWS.AlexaBusiness.Types.NetworkSecurityType
import Network.AWS.AlexaBusiness.Types.NextPassword
import Network.AWS.AlexaBusiness.Types.NextToken
import Network.AWS.AlexaBusiness.Types.NextWiFiPassword
import Network.AWS.AlexaBusiness.Types.Number
import Network.AWS.AlexaBusiness.Types.OneClickIdDelay
import Network.AWS.AlexaBusiness.Types.OneClickPinDelay
import Network.AWS.AlexaBusiness.Types.OrganizationName
import Network.AWS.AlexaBusiness.Types.OutboundPhoneNumber
import Network.AWS.AlexaBusiness.Types.PSTNDialIn
import Network.AWS.AlexaBusiness.Types.ParameterKey
import Network.AWS.AlexaBusiness.Types.Path
import Network.AWS.AlexaBusiness.Types.PhoneNumber
import Network.AWS.AlexaBusiness.Types.PhoneNumberType
import Network.AWS.AlexaBusiness.Types.PrivacyPolicy
import Network.AWS.AlexaBusiness.Types.ProductDescription
import Network.AWS.AlexaBusiness.Types.ProductId
import Network.AWS.AlexaBusiness.Types.Profile
import Network.AWS.AlexaBusiness.Types.ProfileArn
import Network.AWS.AlexaBusiness.Types.ProfileData
import Network.AWS.AlexaBusiness.Types.ProfileName
import Network.AWS.AlexaBusiness.Types.ProviderCalendarId
import Network.AWS.AlexaBusiness.Types.RawPhoneNumber
import Network.AWS.AlexaBusiness.Types.ReleaseDate
import Network.AWS.AlexaBusiness.Types.RequireCheckIn
import Network.AWS.AlexaBusiness.Types.RequirePin
import Network.AWS.AlexaBusiness.Types.ReviewKey
import Network.AWS.AlexaBusiness.Types.ReviewValue
import Network.AWS.AlexaBusiness.Types.Room
import Network.AWS.AlexaBusiness.Types.RoomArn
import Network.AWS.AlexaBusiness.Types.RoomData
import Network.AWS.AlexaBusiness.Types.RoomName
import Network.AWS.AlexaBusiness.Types.RoomSkillParameter
import Network.AWS.AlexaBusiness.Types.RoomSkillParameterValue
import Network.AWS.AlexaBusiness.Types.S3KeyPrefix
import Network.AWS.AlexaBusiness.Types.ScheduleArn
import Network.AWS.AlexaBusiness.Types.ShortDescription
import Network.AWS.AlexaBusiness.Types.SipAddress
import Network.AWS.AlexaBusiness.Types.SipType
import Network.AWS.AlexaBusiness.Types.SkillDetails
import Network.AWS.AlexaBusiness.Types.SkillGroup
import Network.AWS.AlexaBusiness.Types.SkillGroupArn
import Network.AWS.AlexaBusiness.Types.SkillGroupData
import Network.AWS.AlexaBusiness.Types.SkillGroupDescription
import Network.AWS.AlexaBusiness.Types.SkillGroupName
import Network.AWS.AlexaBusiness.Types.SkillId
import Network.AWS.AlexaBusiness.Types.SkillName
import Network.AWS.AlexaBusiness.Types.SkillStoreType
import Network.AWS.AlexaBusiness.Types.SkillSummary
import Network.AWS.AlexaBusiness.Types.SkillType
import Network.AWS.AlexaBusiness.Types.SkillTypeFilter
import Network.AWS.AlexaBusiness.Types.SkillsStoreSkill
import Network.AWS.AlexaBusiness.Types.SmartHomeAppliance
import Network.AWS.AlexaBusiness.Types.SoftwareVersion
import Network.AWS.AlexaBusiness.Types.Sort
import Network.AWS.AlexaBusiness.Types.SortKey
import Network.AWS.AlexaBusiness.Types.SortValue
import Network.AWS.AlexaBusiness.Types.Ssid
import Network.AWS.AlexaBusiness.Types.Ssml
import Network.AWS.AlexaBusiness.Types.StartDate
import Network.AWS.AlexaBusiness.Types.Tag
import Network.AWS.AlexaBusiness.Types.TagKey
import Network.AWS.AlexaBusiness.Types.TagValue
import Network.AWS.AlexaBusiness.Types.TemperatureUnit
import Network.AWS.AlexaBusiness.Types.TextMessage
import Network.AWS.AlexaBusiness.Types.TextValue
import Network.AWS.AlexaBusiness.Types.Timezone
import Network.AWS.AlexaBusiness.Types.TrustAnchor
import Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
import Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn
import Network.AWS.AlexaBusiness.Types.Uri
import Network.AWS.AlexaBusiness.Types.Url
import Network.AWS.AlexaBusiness.Types.UserCode
import Network.AWS.AlexaBusiness.Types.UserData
import Network.AWS.AlexaBusiness.Types.UserId
import Network.AWS.AlexaBusiness.Types.User_LastName
import Network.AWS.AlexaBusiness.Types.Utterance
import Network.AWS.AlexaBusiness.Types.Value
import Network.AWS.AlexaBusiness.Types.WakeWord
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-09@ of the Amazon Alexa For Business SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "AlexaBusiness",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "a4b",
      Core._svcVersion = "2017-11-09",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "AlexaBusiness",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The skill must be linked to a third-party account.
_SkillNotLinkedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SkillNotLinkedException =
  Core._MatchServiceError mkServiceConfig "SkillNotLinkedException"
{-# DEPRECATED _SkillNotLinkedException "Use generic-lens or generic-optics instead." #-}

-- | The Certificate Authority can't issue or revoke a certificate.
_InvalidCertificateAuthorityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateAuthorityException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidCertificateAuthorityException"
{-# DEPRECATED _InvalidCertificateAuthorityException "Use generic-lens or generic-optics instead." #-}

-- | The request failed because this device is no longer registered and therefore no longer managed by this account.
_DeviceNotRegisteredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeviceNotRegisteredException =
  Core._MatchServiceError
    mkServiceConfig
    "DeviceNotRegisteredException"
{-# DEPRECATED _DeviceNotRegisteredException "Use generic-lens or generic-optics instead." #-}

-- | Another resource is associated with the resource in the request.
_ResourceAssociatedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAssociatedException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceAssociatedException"
{-# DEPRECATED _ResourceAssociatedException "Use generic-lens or generic-optics instead." #-}

-- | The attempt to update a user is invalid due to the user's current status.
_InvalidUserStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUserStatusException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidUserStatusException"
{-# DEPRECATED _InvalidUserStatusException "Use generic-lens or generic-optics instead." #-}

-- | The device is in an invalid state.
_InvalidDeviceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeviceException =
  Core._MatchServiceError mkServiceConfig "InvalidDeviceException"
{-# DEPRECATED _InvalidDeviceException "Use generic-lens or generic-optics instead." #-}

-- | The service linked role is locked for deletion.
_InvalidServiceLinkedRoleStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidServiceLinkedRoleStateException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidServiceLinkedRoleStateException"
{-# DEPRECATED _InvalidServiceLinkedRoleStateException "Use generic-lens or generic-optics instead." #-}

-- | The resource is not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The name sent in the request is already in use.
_NameInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NameInUseException =
  Core._MatchServiceError mkServiceConfig "NameInUseException"
{-# DEPRECATED _NameInUseException "Use generic-lens or generic-optics instead." #-}

-- | A password in SecretsManager is in an invalid state.
_InvalidSecretsManagerResourceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSecretsManagerResourceException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidSecretsManagerResourceException"
{-# DEPRECATED _InvalidSecretsManagerResourceException "Use generic-lens or generic-optics instead." #-}

-- | There is a concurrent modification of resources.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentModificationException"
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead." #-}

-- | The caller has no permissions to operate on the resource involved in the API call.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError mkServiceConfig "UnauthorizedException"
{-# DEPRECATED _UnauthorizedException "Use generic-lens or generic-optics instead." #-}

-- | The resource being created already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError mkServiceConfig "AlreadyExistsException"
{-# DEPRECATED _AlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | You are performing an action that would put you beyond your account's limits.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The resource in the request is already in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead." #-}
