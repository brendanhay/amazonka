{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types
  ( -- * Service Configuration
    alexaBusiness,

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
    AddressBook,
    addressBook,
    abAddressBookARN,
    abName,
    abDescription,

    -- * AddressBookData
    AddressBookData,
    addressBookData,
    abdAddressBookARN,
    abdName,
    abdDescription,

    -- * Audio
    Audio,
    audio,
    aLocale,
    aLocation,

    -- * BusinessReport
    BusinessReport,
    businessReport,
    brStatus,
    brFailureCode,
    brDeliveryTime,
    brDownloadURL,
    brS3Location,

    -- * BusinessReportContentRange
    BusinessReportContentRange,
    businessReportContentRange,
    brcrInterval,

    -- * BusinessReportRecurrence
    BusinessReportRecurrence,
    businessReportRecurrence,
    brrStartDate,

    -- * BusinessReportS3Location
    BusinessReportS3Location,
    businessReportS3Location,
    brslPath,
    brslBucketName,

    -- * BusinessReportSchedule
    BusinessReportSchedule,
    businessReportSchedule,
    brsS3KeyPrefix,
    brsLastBusinessReport,
    brsFormat,
    brsRecurrence,
    brsScheduleName,
    brsScheduleARN,
    brsContentRange,
    brsS3BucketName,

    -- * Category
    Category,
    category,
    cCategoryName,
    cCategoryId,

    -- * ConferencePreference
    ConferencePreference,
    conferencePreference,
    cpDefaultConferenceProviderARN,

    -- * ConferenceProvider
    ConferenceProvider,
    conferenceProvider,
    cpMeetingSetting,
    cpARN,
    cpPSTNDialIn,
    cpName,
    cpType,
    cpIPDialIn,

    -- * Contact
    Contact,
    contact,
    cLastName,
    cContactARN,
    cPhoneNumbers,
    cPhoneNumber,
    cSipAddresses,
    cFirstName,
    cDisplayName,

    -- * ContactData
    ContactData,
    contactData,
    cdLastName,
    cdContactARN,
    cdPhoneNumbers,
    cdPhoneNumber,
    cdSipAddresses,
    cdFirstName,
    cdDisplayName,

    -- * Content
    Content,
    content,
    cAudioList,
    cTextList,
    cSsmlList,

    -- * CreateEndOfMeetingReminder
    CreateEndOfMeetingReminder,
    createEndOfMeetingReminder,
    ceomrReminderAtMinutes,
    ceomrReminderType,
    ceomrEnabled,

    -- * CreateInstantBooking
    CreateInstantBooking,
    createInstantBooking,
    cibDurationInMinutes,
    cibEnabled,

    -- * CreateMeetingRoomConfiguration
    CreateMeetingRoomConfiguration,
    createMeetingRoomConfiguration,
    cmrcInstantBooking,
    cmrcEndOfMeetingReminder,
    cmrcRequireCheckIn,
    cmrcRoomUtilizationMetricsEnabled,

    -- * CreateRequireCheckIn
    CreateRequireCheckIn,
    createRequireCheckIn,
    crciReleaseAfterMinutes,
    crciEnabled,

    -- * DeveloperInfo
    DeveloperInfo,
    developerInfo,
    diEmail,
    diURL,
    diPrivacyPolicy,
    diDeveloperName,

    -- * Device
    Device,
    device,
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
    DeviceData,
    deviceData,
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
    DeviceEvent,
    deviceEvent,
    deValue,
    deType,
    deTimestamp,

    -- * DeviceNetworkProfileInfo
    DeviceNetworkProfileInfo,
    deviceNetworkProfileInfo,
    dnpiCertificateARN,
    dnpiNetworkProfileARN,
    dnpiCertificateExpirationTime,

    -- * DeviceStatusDetail
    DeviceStatusDetail,
    deviceStatusDetail,
    dsdFeature,
    dsdCode,

    -- * DeviceStatusInfo
    DeviceStatusInfo,
    deviceStatusInfo,
    dsiConnectionStatusUpdatedTime,
    dsiDeviceStatusDetails,
    dsiConnectionStatus,

    -- * EndOfMeetingReminder
    EndOfMeetingReminder,
    endOfMeetingReminder,
    eomrEnabled,
    eomrReminderAtMinutes,
    eomrReminderType,

    -- * Filter
    Filter,
    filter',
    fKey,
    fValues,

    -- * Gateway
    Gateway,
    gateway,
    gARN,
    gName,
    gGatewayGroupARN,
    gSoftwareVersion,
    gDescription,

    -- * GatewayGroup
    GatewayGroup,
    gatewayGroup,
    ggARN,
    ggName,
    ggDescription,

    -- * GatewayGroupSummary
    GatewayGroupSummary,
    gatewayGroupSummary,
    ggsARN,
    ggsName,
    ggsDescription,

    -- * GatewaySummary
    GatewaySummary,
    gatewaySummary,
    gsARN,
    gsName,
    gsGatewayGroupARN,
    gsSoftwareVersion,
    gsDescription,

    -- * IPDialIn
    IPDialIn,
    ipDialIn,
    idiEndpoint,
    idiCommsProtocol,

    -- * InstantBooking
    InstantBooking,
    instantBooking,
    ibEnabled,
    ibDurationInMinutes,

    -- * MeetingRoomConfiguration
    MeetingRoomConfiguration,
    meetingRoomConfiguration,
    mrcInstantBooking,
    mrcEndOfMeetingReminder,
    mrcRequireCheckIn,
    mrcRoomUtilizationMetricsEnabled,

    -- * MeetingSetting
    MeetingSetting,
    meetingSetting,
    msRequirePin,

    -- * NetworkProfile
    NetworkProfile,
    networkProfile,
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
    NetworkProfileData,
    networkProfileData,
    npdNetworkProfileName,
    npdSsid,
    npdNetworkProfileARN,
    npdSecurityType,
    npdEapMethod,
    npdDescription,
    npdCertificateAuthorityARN,

    -- * PSTNDialIn
    PSTNDialIn,
    pSTNDialIn,
    pstndiCountryCode,
    pstndiPhoneNumber,
    pstndiOneClickIdDelay,
    pstndiOneClickPinDelay,

    -- * PhoneNumber
    PhoneNumber,
    phoneNumber,
    pnNumber,
    pnType,

    -- * Profile
    Profile,
    profile,
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
    ProfileData,
    profileData,
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
    RequireCheckIn,
    requireCheckIn,
    rciEnabled,
    rciReleaseAfterMinutes,

    -- * Room
    Room,
    room,
    rProfileARN,
    rProviderCalendarId,
    rRoomARN,
    rRoomName,
    rDescription,

    -- * RoomData
    RoomData,
    roomData,
    rdProfileARN,
    rdProviderCalendarId,
    rdProfileName,
    rdRoomARN,
    rdRoomName,
    rdDescription,

    -- * RoomSkillParameter
    RoomSkillParameter,
    roomSkillParameter,
    rspParameterKey,
    rspParameterValue,

    -- * SipAddress
    SipAddress,
    sipAddress,
    saURI,
    saType,

    -- * SkillDetails
    SkillDetails,
    skillDetails,
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
    SkillGroup,
    skillGroup,
    sgSkillGroupARN,
    sgDescription,
    sgSkillGroupName,

    -- * SkillGroupData
    SkillGroupData,
    skillGroupData,
    sgdSkillGroupARN,
    sgdDescription,
    sgdSkillGroupName,

    -- * SkillSummary
    SkillSummary,
    skillSummary,
    ssSkillId,
    ssSupportsLinking,
    ssSkillType,
    ssEnablementType,
    ssSkillName,

    -- * SkillsStoreSkill
    SkillsStoreSkill,
    skillsStoreSkill,
    sssSkillId,
    sssSupportsLinking,
    sssSampleUtterances,
    sssShortDescription,
    sssIconURL,
    sssSkillDetails,
    sssSkillName,

    -- * SmartHomeAppliance
    SmartHomeAppliance,
    smartHomeAppliance,
    shaFriendlyName,
    shaManufacturerName,
    shaDescription,

    -- * Sort
    Sort,
    sort,
    sKey,
    sValue,

    -- * Ssml
    Ssml,
    ssml,
    ssmLocale,
    ssmValue,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TextMessage
    TextMessage,
    textMessage,
    tmLocale,
    tmValue,

    -- * UpdateEndOfMeetingReminder
    UpdateEndOfMeetingReminder,
    updateEndOfMeetingReminder,
    ueomrEnabled,
    ueomrReminderAtMinutes,
    ueomrReminderType,

    -- * UpdateInstantBooking
    UpdateInstantBooking,
    updateInstantBooking,
    uibEnabled,
    uibDurationInMinutes,

    -- * UpdateMeetingRoomConfiguration
    UpdateMeetingRoomConfiguration,
    updateMeetingRoomConfiguration,
    umrcInstantBooking,
    umrcEndOfMeetingReminder,
    umrcRequireCheckIn,
    umrcRoomUtilizationMetricsEnabled,

    -- * UpdateRequireCheckIn
    UpdateRequireCheckIn,
    updateRequireCheckIn,
    urciEnabled,
    urciReleaseAfterMinutes,

    -- * UserData
    UserData,
    userData,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-11-09@ of the Amazon Alexa For Business SDK configuration.
alexaBusiness :: Service
alexaBusiness =
  Service
    { _svcAbbrev = "AlexaBusiness",
      _svcSigner = v4,
      _svcPrefix = "a4b",
      _svcVersion = "2017-11-09",
      _svcEndpoint = defaultEndpoint alexaBusiness,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "AlexaBusiness",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
