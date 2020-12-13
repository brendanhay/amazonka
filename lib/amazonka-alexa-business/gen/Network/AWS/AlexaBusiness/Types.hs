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
    alexaBusinessService,

    -- * Errors

    -- * BusinessReportFailureCode
    BusinessReportFailureCode (..),

    -- * BusinessReportFormat
    BusinessReportFormat (..),

    -- * BusinessReportInterval
    BusinessReportInterval (..),

    -- * BusinessReportStatus
    BusinessReportStatus (..),

    -- * CommsProtocol
    CommsProtocol (..),

    -- * ConferenceProviderType
    ConferenceProviderType (..),

    -- * ConnectionStatus
    ConnectionStatus (..),

    -- * DeviceEventType
    DeviceEventType (..),

    -- * DeviceStatus
    DeviceStatus (..),

    -- * DeviceStatusDetailCode
    DeviceStatusDetailCode (..),

    -- * DeviceUsageType
    DeviceUsageType (..),

    -- * DistanceUnit
    DistanceUnit (..),

    -- * EnablementType
    EnablementType (..),

    -- * EnablementTypeFilter
    EnablementTypeFilter (..),

    -- * EndOfMeetingReminderType
    EndOfMeetingReminderType (..),

    -- * EnrollmentStatus
    EnrollmentStatus (..),

    -- * Feature
    Feature (..),

    -- * Locale
    Locale (..),

    -- * NetworkEapMethod
    NetworkEapMethod (..),

    -- * NetworkSecurityType
    NetworkSecurityType (..),

    -- * PhoneNumberType
    PhoneNumberType (..),

    -- * RequirePin
    RequirePin (..),

    -- * SipType
    SipType (..),

    -- * SkillType
    SkillType (..),

    -- * SkillTypeFilter
    SkillTypeFilter (..),

    -- * SortValue
    SortValue (..),

    -- * TemperatureUnit
    TemperatureUnit (..),

    -- * WakeWord
    WakeWord (..),

    -- * AddressBook
    AddressBook (..),
    mkAddressBook,
    abAddressBookARN,
    abName,
    abDescription,

    -- * AddressBookData
    AddressBookData (..),
    mkAddressBookData,
    abdAddressBookARN,
    abdName,
    abdDescription,

    -- * Audio
    Audio (..),
    mkAudio,
    aLocation,
    aLocale,

    -- * BusinessReport
    BusinessReport (..),
    mkBusinessReport,
    brStatus,
    brFailureCode,
    brDeliveryTime,
    brDownloadURL,
    brS3Location,

    -- * BusinessReportContentRange
    BusinessReportContentRange (..),
    mkBusinessReportContentRange,
    brcrInterval,

    -- * BusinessReportRecurrence
    BusinessReportRecurrence (..),
    mkBusinessReportRecurrence,
    brrStartDate,

    -- * BusinessReportS3Location
    BusinessReportS3Location (..),
    mkBusinessReportS3Location,
    brslPath,
    brslBucketName,

    -- * BusinessReportSchedule
    BusinessReportSchedule (..),
    mkBusinessReportSchedule,
    brsS3KeyPrefix,
    brsLastBusinessReport,
    brsFormat,
    brsRecurrence,
    brsScheduleName,
    brsScheduleARN,
    brsContentRange,
    brsS3BucketName,

    -- * Category
    Category (..),
    mkCategory,
    cCategoryName,
    cCategoryId,

    -- * ConferencePreference
    ConferencePreference (..),
    mkConferencePreference,
    cpDefaultConferenceProviderARN,

    -- * ConferenceProvider
    ConferenceProvider (..),
    mkConferenceProvider,
    cpMeetingSetting,
    cpARN,
    cpPSTNDialIn,
    cpName,
    cpType,
    cpIPDialIn,

    -- * Contact
    Contact (..),
    mkContact,
    cLastName,
    cContactARN,
    cPhoneNumbers,
    cPhoneNumber,
    cSipAddresses,
    cFirstName,
    cDisplayName,

    -- * ContactData
    ContactData (..),
    mkContactData,
    cdLastName,
    cdContactARN,
    cdPhoneNumbers,
    cdPhoneNumber,
    cdSipAddresses,
    cdFirstName,
    cdDisplayName,

    -- * Content
    Content (..),
    mkContent,
    cAudioList,
    cTextList,
    cSsmlList,

    -- * CreateEndOfMeetingReminder
    CreateEndOfMeetingReminder (..),
    mkCreateEndOfMeetingReminder,
    ceomrEnabled,
    ceomrReminderAtMinutes,
    ceomrReminderType,

    -- * CreateInstantBooking
    CreateInstantBooking (..),
    mkCreateInstantBooking,
    cibEnabled,
    cibDurationInMinutes,

    -- * CreateMeetingRoomConfiguration
    CreateMeetingRoomConfiguration (..),
    mkCreateMeetingRoomConfiguration,
    cmrcInstantBooking,
    cmrcEndOfMeetingReminder,
    cmrcRequireCheckIn,
    cmrcRoomUtilizationMetricsEnabled,

    -- * CreateRequireCheckIn
    CreateRequireCheckIn (..),
    mkCreateRequireCheckIn,
    crciEnabled,
    crciReleaseAfterMinutes,

    -- * DeveloperInfo
    DeveloperInfo (..),
    mkDeveloperInfo,
    diEmail,
    diURL,
    diPrivacyPolicy,
    diDeveloperName,

    -- * Device
    Device (..),
    mkDevice,
    dDeviceStatus,
    dDeviceStatusInfo,
    dDeviceARN,
    dMACAddress,
    dDeviceName,
    dRoomARN,
    dSoftwareVersion,
    dDeviceType,
    dNetworkProfileInfo,
    dDeviceSerialNumber,

    -- * DeviceData
    DeviceData (..),
    mkDeviceData,
    ddDeviceStatus,
    ddNetworkProfileName,
    ddDeviceStatusInfo,
    ddCreatedTime,
    ddDeviceARN,
    ddNetworkProfileARN,
    ddMACAddress,
    ddDeviceName,
    ddRoomARN,
    ddSoftwareVersion,
    ddDeviceType,
    ddRoomName,
    ddDeviceSerialNumber,

    -- * DeviceEvent
    DeviceEvent (..),
    mkDeviceEvent,
    deValue,
    deType,
    deTimestamp,

    -- * DeviceNetworkProfileInfo
    DeviceNetworkProfileInfo (..),
    mkDeviceNetworkProfileInfo,
    dnpiCertificateARN,
    dnpiNetworkProfileARN,
    dnpiCertificateExpirationTime,

    -- * DeviceStatusDetail
    DeviceStatusDetail (..),
    mkDeviceStatusDetail,
    dsdFeature,
    dsdCode,

    -- * DeviceStatusInfo
    DeviceStatusInfo (..),
    mkDeviceStatusInfo,
    dsiConnectionStatusUpdatedTime,
    dsiDeviceStatusDetails,
    dsiConnectionStatus,

    -- * EndOfMeetingReminder
    EndOfMeetingReminder (..),
    mkEndOfMeetingReminder,
    eomrEnabled,
    eomrReminderAtMinutes,
    eomrReminderType,

    -- * Filter
    Filter (..),
    mkFilter,
    fValues,
    fKey,

    -- * Gateway
    Gateway (..),
    mkGateway,
    gARN,
    gName,
    gGatewayGroupARN,
    gSoftwareVersion,
    gDescription,

    -- * GatewayGroup
    GatewayGroup (..),
    mkGatewayGroup,
    ggARN,
    ggName,
    ggDescription,

    -- * GatewayGroupSummary
    GatewayGroupSummary (..),
    mkGatewayGroupSummary,
    ggsARN,
    ggsName,
    ggsDescription,

    -- * GatewaySummary
    GatewaySummary (..),
    mkGatewaySummary,
    gsARN,
    gsName,
    gsGatewayGroupARN,
    gsSoftwareVersion,
    gsDescription,

    -- * IPDialIn
    IPDialIn (..),
    mkIPDialIn,
    idiCommsProtocol,
    idiEndpoint,

    -- * InstantBooking
    InstantBooking (..),
    mkInstantBooking,
    ibEnabled,
    ibDurationInMinutes,

    -- * MeetingRoomConfiguration
    MeetingRoomConfiguration (..),
    mkMeetingRoomConfiguration,
    mrcInstantBooking,
    mrcEndOfMeetingReminder,
    mrcRequireCheckIn,
    mrcRoomUtilizationMetricsEnabled,

    -- * MeetingSetting
    MeetingSetting (..),
    mkMeetingSetting,
    msRequirePin,

    -- * NetworkProfile
    NetworkProfile (..),
    mkNetworkProfile,
    npNetworkProfileName,
    npSsid,
    npNetworkProfileARN,
    npSecurityType,
    npCurrentPassword,
    npNextPassword,
    npEapMethod,
    npDescription,
    npTrustAnchors,
    npCertificateAuthorityARN,

    -- * NetworkProfileData
    NetworkProfileData (..),
    mkNetworkProfileData,
    npdNetworkProfileName,
    npdSsid,
    npdNetworkProfileARN,
    npdSecurityType,
    npdEapMethod,
    npdDescription,
    npdCertificateAuthorityARN,

    -- * PSTNDialIn
    PSTNDialIn (..),
    mkPSTNDialIn,
    pstndiOneClickIdDelay,
    pstndiOneClickPinDelay,
    pstndiPhoneNumber,
    pstndiCountryCode,

    -- * PhoneNumber
    PhoneNumber (..),
    mkPhoneNumber,
    pnType,
    pnNumber,

    -- * Profile
    Profile (..),
    mkProfile,
    pSetupModeDisabled,
    pPSTNEnabled,
    pAddressBookARN,
    pDistanceUnit,
    pLocale,
    pAddress,
    pProfileARN,
    pWakeWord,
    pMeetingRoomConfiguration,
    pProfileName,
    pTemperatureUnit,
    pTimezone,
    pMaxVolumeLimit,
    pIsDefault,

    -- * ProfileData
    ProfileData (..),
    mkProfileData,
    pdDistanceUnit,
    pdLocale,
    pdAddress,
    pdProfileARN,
    pdWakeWord,
    pdProfileName,
    pdTemperatureUnit,
    pdTimezone,
    pdIsDefault,

    -- * RequireCheckIn
    RequireCheckIn (..),
    mkRequireCheckIn,
    rciEnabled,
    rciReleaseAfterMinutes,

    -- * Room
    Room (..),
    mkRoom,
    rProfileARN,
    rProviderCalendarId,
    rRoomARN,
    rRoomName,
    rDescription,

    -- * RoomData
    RoomData (..),
    mkRoomData,
    rdProfileARN,
    rdProviderCalendarId,
    rdProfileName,
    rdRoomARN,
    rdRoomName,
    rdDescription,

    -- * RoomSkillParameter
    RoomSkillParameter (..),
    mkRoomSkillParameter,
    rspParameterValue,
    rspParameterKey,

    -- * SipAddress
    SipAddress (..),
    mkSipAddress,
    saURI,
    saType,

    -- * SkillDetails
    SkillDetails (..),
    mkSkillDetails,
    sdSkillTypes,
    sdProductDescription,
    sdInvocationPhrase,
    sdDeveloperInfo,
    sdEndUserLicenseAgreement,
    sdGenericKeywords,
    sdReviews,
    sdReleaseDate,
    sdNewInThisVersionBulletPoints,
    sdBulletPoints,

    -- * SkillGroup
    SkillGroup (..),
    mkSkillGroup,
    sgSkillGroupARN,
    sgDescription,
    sgSkillGroupName,

    -- * SkillGroupData
    SkillGroupData (..),
    mkSkillGroupData,
    sgdSkillGroupARN,
    sgdDescription,
    sgdSkillGroupName,

    -- * SkillSummary
    SkillSummary (..),
    mkSkillSummary,
    ssSkillId,
    ssSupportsLinking,
    ssSkillType,
    ssEnablementType,
    ssSkillName,

    -- * SkillsStoreSkill
    SkillsStoreSkill (..),
    mkSkillsStoreSkill,
    sssSkillId,
    sssSupportsLinking,
    sssSampleUtterances,
    sssShortDescription,
    sssIconURL,
    sssSkillDetails,
    sssSkillName,

    -- * SmartHomeAppliance
    SmartHomeAppliance (..),
    mkSmartHomeAppliance,
    shaFriendlyName,
    shaManufacturerName,
    shaDescription,

    -- * Sort
    Sort (..),
    mkSort,
    sfValue,
    sfKey,

    -- * Ssml
    Ssml (..),
    mkSsml,
    sLocale,
    sValue,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TextMessage
    TextMessage (..),
    mkTextMessage,
    tmLocale,
    tmValue,

    -- * UpdateEndOfMeetingReminder
    UpdateEndOfMeetingReminder (..),
    mkUpdateEndOfMeetingReminder,
    ueomrEnabled,
    ueomrReminderAtMinutes,
    ueomrReminderType,

    -- * UpdateInstantBooking
    UpdateInstantBooking (..),
    mkUpdateInstantBooking,
    uibEnabled,
    uibDurationInMinutes,

    -- * UpdateMeetingRoomConfiguration
    UpdateMeetingRoomConfiguration (..),
    mkUpdateMeetingRoomConfiguration,
    umrcInstantBooking,
    umrcEndOfMeetingReminder,
    umrcRequireCheckIn,
    umrcRoomUtilizationMetricsEnabled,

    -- * UpdateRequireCheckIn
    UpdateRequireCheckIn (..),
    mkUpdateRequireCheckIn,
    urciEnabled,
    urciReleaseAfterMinutes,

    -- * UserData
    UserData (..),
    mkUserData,
    udEmail,
    udLastName,
    udEnrollmentId,
    udUserARN,
    udFirstName,
    udEnrollmentStatus,
  )
where

import Network.AWS.AlexaBusiness.Types.AddressBook
import Network.AWS.AlexaBusiness.Types.AddressBookData
import Network.AWS.AlexaBusiness.Types.Audio
import Network.AWS.AlexaBusiness.Types.BusinessReport
import Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
import Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
import Network.AWS.AlexaBusiness.Types.BusinessReportFormat
import Network.AWS.AlexaBusiness.Types.BusinessReportInterval
import Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
import Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
import Network.AWS.AlexaBusiness.Types.BusinessReportSchedule
import Network.AWS.AlexaBusiness.Types.BusinessReportStatus
import Network.AWS.AlexaBusiness.Types.Category
import Network.AWS.AlexaBusiness.Types.CommsProtocol
import Network.AWS.AlexaBusiness.Types.ConferencePreference
import Network.AWS.AlexaBusiness.Types.ConferenceProvider
import Network.AWS.AlexaBusiness.Types.ConferenceProviderType
import Network.AWS.AlexaBusiness.Types.ConnectionStatus
import Network.AWS.AlexaBusiness.Types.Contact
import Network.AWS.AlexaBusiness.Types.ContactData
import Network.AWS.AlexaBusiness.Types.Content
import Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.CreateInstantBooking
import Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
import Network.AWS.AlexaBusiness.Types.DeveloperInfo
import Network.AWS.AlexaBusiness.Types.Device
import Network.AWS.AlexaBusiness.Types.DeviceData
import Network.AWS.AlexaBusiness.Types.DeviceEvent
import Network.AWS.AlexaBusiness.Types.DeviceEventType
import Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Network.AWS.AlexaBusiness.Types.DeviceStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
import Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import Network.AWS.AlexaBusiness.Types.DeviceUsageType
import Network.AWS.AlexaBusiness.Types.DistanceUnit
import Network.AWS.AlexaBusiness.Types.EnablementType
import Network.AWS.AlexaBusiness.Types.EnablementTypeFilter
import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import Network.AWS.AlexaBusiness.Types.EnrollmentStatus
import Network.AWS.AlexaBusiness.Types.Feature
import Network.AWS.AlexaBusiness.Types.Filter
import Network.AWS.AlexaBusiness.Types.Gateway
import Network.AWS.AlexaBusiness.Types.GatewayGroup
import Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
import Network.AWS.AlexaBusiness.Types.GatewaySummary
import Network.AWS.AlexaBusiness.Types.IPDialIn
import Network.AWS.AlexaBusiness.Types.InstantBooking
import Network.AWS.AlexaBusiness.Types.Locale
import Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.MeetingSetting
import Network.AWS.AlexaBusiness.Types.NetworkEapMethod
import Network.AWS.AlexaBusiness.Types.NetworkProfile
import Network.AWS.AlexaBusiness.Types.NetworkProfileData
import Network.AWS.AlexaBusiness.Types.NetworkSecurityType
import Network.AWS.AlexaBusiness.Types.PSTNDialIn
import Network.AWS.AlexaBusiness.Types.PhoneNumber
import Network.AWS.AlexaBusiness.Types.PhoneNumberType
import Network.AWS.AlexaBusiness.Types.Profile
import Network.AWS.AlexaBusiness.Types.ProfileData
import Network.AWS.AlexaBusiness.Types.RequireCheckIn
import Network.AWS.AlexaBusiness.Types.RequirePin
import Network.AWS.AlexaBusiness.Types.Room
import Network.AWS.AlexaBusiness.Types.RoomData
import Network.AWS.AlexaBusiness.Types.RoomSkillParameter
import Network.AWS.AlexaBusiness.Types.SipAddress
import Network.AWS.AlexaBusiness.Types.SipType
import Network.AWS.AlexaBusiness.Types.SkillDetails
import Network.AWS.AlexaBusiness.Types.SkillGroup
import Network.AWS.AlexaBusiness.Types.SkillGroupData
import Network.AWS.AlexaBusiness.Types.SkillSummary
import Network.AWS.AlexaBusiness.Types.SkillType
import Network.AWS.AlexaBusiness.Types.SkillTypeFilter
import Network.AWS.AlexaBusiness.Types.SkillsStoreSkill
import Network.AWS.AlexaBusiness.Types.SmartHomeAppliance
import Network.AWS.AlexaBusiness.Types.Sort
import Network.AWS.AlexaBusiness.Types.SortValue
import Network.AWS.AlexaBusiness.Types.Ssml
import Network.AWS.AlexaBusiness.Types.Tag
import Network.AWS.AlexaBusiness.Types.TemperatureUnit
import Network.AWS.AlexaBusiness.Types.TextMessage
import Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
import Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn
import Network.AWS.AlexaBusiness.Types.UserData
import Network.AWS.AlexaBusiness.Types.WakeWord
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-09@ of the Amazon Alexa For Business SDK configuration.
alexaBusinessService :: Lude.Service
alexaBusinessService =
  Lude.Service
    { Lude._svcAbbrev = "AlexaBusiness",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "a4b",
      Lude._svcVersion = "2017-11-09",
      Lude._svcEndpoint = Lude.defaultEndpoint alexaBusinessService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "AlexaBusiness",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
