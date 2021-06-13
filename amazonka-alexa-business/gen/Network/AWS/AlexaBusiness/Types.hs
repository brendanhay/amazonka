{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _InvalidServiceLinkedRoleStateException,
    _UnauthorizedException,
    _InvalidUserStatusException,
    _ResourceAssociatedException,
    _ConcurrentModificationException,
    _DeviceNotRegisteredException,
    _InvalidCertificateAuthorityException,
    _NameInUseException,
    _ResourceInUseException,
    _LimitExceededException,
    _AlreadyExistsException,
    _InvalidDeviceException,
    _SkillNotLinkedException,
    _InvalidSecretsManagerResourceException,

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
    newAddressBook,
    addressBook_addressBookArn,
    addressBook_name,
    addressBook_description,

    -- * AddressBookData
    AddressBookData (..),
    newAddressBookData,
    addressBookData_addressBookArn,
    addressBookData_name,
    addressBookData_description,

    -- * Audio
    Audio (..),
    newAudio,
    audio_locale,
    audio_location,

    -- * BusinessReport
    BusinessReport (..),
    newBusinessReport,
    businessReport_downloadUrl,
    businessReport_status,
    businessReport_deliveryTime,
    businessReport_failureCode,
    businessReport_s3Location,

    -- * BusinessReportContentRange
    BusinessReportContentRange (..),
    newBusinessReportContentRange,
    businessReportContentRange_interval,

    -- * BusinessReportRecurrence
    BusinessReportRecurrence (..),
    newBusinessReportRecurrence,
    businessReportRecurrence_startDate,

    -- * BusinessReportS3Location
    BusinessReportS3Location (..),
    newBusinessReportS3Location,
    businessReportS3Location_bucketName,
    businessReportS3Location_path,

    -- * BusinessReportSchedule
    BusinessReportSchedule (..),
    newBusinessReportSchedule,
    businessReportSchedule_contentRange,
    businessReportSchedule_format,
    businessReportSchedule_scheduleArn,
    businessReportSchedule_lastBusinessReport,
    businessReportSchedule_s3KeyPrefix,
    businessReportSchedule_recurrence,
    businessReportSchedule_s3BucketName,
    businessReportSchedule_scheduleName,

    -- * Category
    Category (..),
    newCategory,
    category_categoryId,
    category_categoryName,

    -- * ConferencePreference
    ConferencePreference (..),
    newConferencePreference,
    conferencePreference_defaultConferenceProviderArn,

    -- * ConferenceProvider
    ConferenceProvider (..),
    newConferenceProvider,
    conferenceProvider_meetingSetting,
    conferenceProvider_iPDialIn,
    conferenceProvider_arn,
    conferenceProvider_name,
    conferenceProvider_pSTNDialIn,
    conferenceProvider_type,

    -- * Contact
    Contact (..),
    newContact,
    contact_phoneNumber,
    contact_phoneNumbers,
    contact_displayName,
    contact_contactArn,
    contact_firstName,
    contact_lastName,
    contact_sipAddresses,

    -- * ContactData
    ContactData (..),
    newContactData,
    contactData_phoneNumber,
    contactData_phoneNumbers,
    contactData_displayName,
    contactData_contactArn,
    contactData_firstName,
    contactData_lastName,
    contactData_sipAddresses,

    -- * Content
    Content (..),
    newContent,
    content_textList,
    content_ssmlList,
    content_audioList,

    -- * CreateEndOfMeetingReminder
    CreateEndOfMeetingReminder (..),
    newCreateEndOfMeetingReminder,
    createEndOfMeetingReminder_reminderAtMinutes,
    createEndOfMeetingReminder_reminderType,
    createEndOfMeetingReminder_enabled,

    -- * CreateInstantBooking
    CreateInstantBooking (..),
    newCreateInstantBooking,
    createInstantBooking_durationInMinutes,
    createInstantBooking_enabled,

    -- * CreateMeetingRoomConfiguration
    CreateMeetingRoomConfiguration (..),
    newCreateMeetingRoomConfiguration,
    createMeetingRoomConfiguration_roomUtilizationMetricsEnabled,
    createMeetingRoomConfiguration_endOfMeetingReminder,
    createMeetingRoomConfiguration_instantBooking,
    createMeetingRoomConfiguration_requireCheckIn,

    -- * CreateRequireCheckIn
    CreateRequireCheckIn (..),
    newCreateRequireCheckIn,
    createRequireCheckIn_releaseAfterMinutes,
    createRequireCheckIn_enabled,

    -- * DeveloperInfo
    DeveloperInfo (..),
    newDeveloperInfo,
    developerInfo_developerName,
    developerInfo_email,
    developerInfo_privacyPolicy,
    developerInfo_url,

    -- * Device
    Device (..),
    newDevice,
    device_deviceStatus,
    device_macAddress,
    device_deviceArn,
    device_roomArn,
    device_deviceStatusInfo,
    device_deviceName,
    device_networkProfileInfo,
    device_deviceSerialNumber,
    device_deviceType,
    device_softwareVersion,

    -- * DeviceData
    DeviceData (..),
    newDeviceData,
    deviceData_deviceStatus,
    deviceData_macAddress,
    deviceData_createdTime,
    deviceData_deviceArn,
    deviceData_roomArn,
    deviceData_networkProfileName,
    deviceData_deviceStatusInfo,
    deviceData_deviceName,
    deviceData_deviceSerialNumber,
    deviceData_roomName,
    deviceData_deviceType,
    deviceData_networkProfileArn,
    deviceData_softwareVersion,

    -- * DeviceEvent
    DeviceEvent (..),
    newDeviceEvent,
    deviceEvent_timestamp,
    deviceEvent_value,
    deviceEvent_type,

    -- * DeviceNetworkProfileInfo
    DeviceNetworkProfileInfo (..),
    newDeviceNetworkProfileInfo,
    deviceNetworkProfileInfo_certificateExpirationTime,
    deviceNetworkProfileInfo_certificateArn,
    deviceNetworkProfileInfo_networkProfileArn,

    -- * DeviceStatusDetail
    DeviceStatusDetail (..),
    newDeviceStatusDetail,
    deviceStatusDetail_code,
    deviceStatusDetail_feature,

    -- * DeviceStatusInfo
    DeviceStatusInfo (..),
    newDeviceStatusInfo,
    deviceStatusInfo_deviceStatusDetails,
    deviceStatusInfo_connectionStatusUpdatedTime,
    deviceStatusInfo_connectionStatus,

    -- * EndOfMeetingReminder
    EndOfMeetingReminder (..),
    newEndOfMeetingReminder,
    endOfMeetingReminder_reminderType,
    endOfMeetingReminder_reminderAtMinutes,
    endOfMeetingReminder_enabled,

    -- * Filter
    Filter (..),
    newFilter,
    filter_key,
    filter_values,

    -- * Gateway
    Gateway (..),
    newGateway,
    gateway_arn,
    gateway_gatewayGroupArn,
    gateway_name,
    gateway_description,
    gateway_softwareVersion,

    -- * GatewayGroup
    GatewayGroup (..),
    newGatewayGroup,
    gatewayGroup_arn,
    gatewayGroup_name,
    gatewayGroup_description,

    -- * GatewayGroupSummary
    GatewayGroupSummary (..),
    newGatewayGroupSummary,
    gatewayGroupSummary_arn,
    gatewayGroupSummary_name,
    gatewayGroupSummary_description,

    -- * GatewaySummary
    GatewaySummary (..),
    newGatewaySummary,
    gatewaySummary_arn,
    gatewaySummary_gatewayGroupArn,
    gatewaySummary_name,
    gatewaySummary_description,
    gatewaySummary_softwareVersion,

    -- * IPDialIn
    IPDialIn (..),
    newIPDialIn,
    iPDialIn_endpoint,
    iPDialIn_commsProtocol,

    -- * InstantBooking
    InstantBooking (..),
    newInstantBooking,
    instantBooking_durationInMinutes,
    instantBooking_enabled,

    -- * MeetingRoomConfiguration
    MeetingRoomConfiguration (..),
    newMeetingRoomConfiguration,
    meetingRoomConfiguration_roomUtilizationMetricsEnabled,
    meetingRoomConfiguration_endOfMeetingReminder,
    meetingRoomConfiguration_instantBooking,
    meetingRoomConfiguration_requireCheckIn,

    -- * MeetingSetting
    MeetingSetting (..),
    newMeetingSetting,
    meetingSetting_requirePin,

    -- * NetworkProfile
    NetworkProfile (..),
    newNetworkProfile,
    networkProfile_certificateAuthorityArn,
    networkProfile_trustAnchors,
    networkProfile_currentPassword,
    networkProfile_eapMethod,
    networkProfile_networkProfileName,
    networkProfile_securityType,
    networkProfile_description,
    networkProfile_nextPassword,
    networkProfile_networkProfileArn,
    networkProfile_ssid,

    -- * NetworkProfileData
    NetworkProfileData (..),
    newNetworkProfileData,
    networkProfileData_certificateAuthorityArn,
    networkProfileData_eapMethod,
    networkProfileData_networkProfileName,
    networkProfileData_securityType,
    networkProfileData_description,
    networkProfileData_networkProfileArn,
    networkProfileData_ssid,

    -- * PSTNDialIn
    PSTNDialIn (..),
    newPSTNDialIn,
    pSTNDialIn_countryCode,
    pSTNDialIn_phoneNumber,
    pSTNDialIn_oneClickIdDelay,
    pSTNDialIn_oneClickPinDelay,

    -- * PhoneNumber
    PhoneNumber (..),
    newPhoneNumber,
    phoneNumber_number,
    phoneNumber_type,

    -- * Profile
    Profile (..),
    newProfile,
    profile_profileName,
    profile_isDefault,
    profile_address,
    profile_locale,
    profile_temperatureUnit,
    profile_addressBookArn,
    profile_setupModeDisabled,
    profile_pSTNEnabled,
    profile_maxVolumeLimit,
    profile_meetingRoomConfiguration,
    profile_wakeWord,
    profile_profileArn,
    profile_timezone,
    profile_distanceUnit,

    -- * ProfileData
    ProfileData (..),
    newProfileData,
    profileData_profileName,
    profileData_isDefault,
    profileData_address,
    profileData_locale,
    profileData_temperatureUnit,
    profileData_wakeWord,
    profileData_profileArn,
    profileData_timezone,
    profileData_distanceUnit,

    -- * RequireCheckIn
    RequireCheckIn (..),
    newRequireCheckIn,
    requireCheckIn_releaseAfterMinutes,
    requireCheckIn_enabled,

    -- * Room
    Room (..),
    newRoom,
    room_roomArn,
    room_providerCalendarId,
    room_profileArn,
    room_description,
    room_roomName,

    -- * RoomData
    RoomData (..),
    newRoomData,
    roomData_profileName,
    roomData_roomArn,
    roomData_providerCalendarId,
    roomData_profileArn,
    roomData_description,
    roomData_roomName,

    -- * RoomSkillParameter
    RoomSkillParameter (..),
    newRoomSkillParameter,
    roomSkillParameter_parameterKey,
    roomSkillParameter_parameterValue,

    -- * SipAddress
    SipAddress (..),
    newSipAddress,
    sipAddress_uri,
    sipAddress_type,

    -- * SkillDetails
    SkillDetails (..),
    newSkillDetails,
    skillDetails_newInThisVersionBulletPoints,
    skillDetails_skillTypes,
    skillDetails_reviews,
    skillDetails_bulletPoints,
    skillDetails_genericKeywords,
    skillDetails_endUserLicenseAgreement,
    skillDetails_developerInfo,
    skillDetails_productDescription,
    skillDetails_invocationPhrase,
    skillDetails_releaseDate,

    -- * SkillGroup
    SkillGroup (..),
    newSkillGroup,
    skillGroup_skillGroupName,
    skillGroup_description,
    skillGroup_skillGroupArn,

    -- * SkillGroupData
    SkillGroupData (..),
    newSkillGroupData,
    skillGroupData_skillGroupName,
    skillGroupData_description,
    skillGroupData_skillGroupArn,

    -- * SkillSummary
    SkillSummary (..),
    newSkillSummary,
    skillSummary_skillId,
    skillSummary_supportsLinking,
    skillSummary_skillType,
    skillSummary_skillName,
    skillSummary_enablementType,

    -- * SkillsStoreSkill
    SkillsStoreSkill (..),
    newSkillsStoreSkill,
    skillsStoreSkill_iconUrl,
    skillsStoreSkill_skillId,
    skillsStoreSkill_shortDescription,
    skillsStoreSkill_supportsLinking,
    skillsStoreSkill_skillName,
    skillsStoreSkill_sampleUtterances,
    skillsStoreSkill_skillDetails,

    -- * SmartHomeAppliance
    SmartHomeAppliance (..),
    newSmartHomeAppliance,
    smartHomeAppliance_friendlyName,
    smartHomeAppliance_description,
    smartHomeAppliance_manufacturerName,

    -- * Sort
    Sort (..),
    newSort,
    sort_key,
    sort_value,

    -- * Ssml
    Ssml (..),
    newSsml,
    ssml_locale,
    ssml_value,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TextMessage
    TextMessage (..),
    newTextMessage,
    textMessage_locale,
    textMessage_value,

    -- * UpdateEndOfMeetingReminder
    UpdateEndOfMeetingReminder (..),
    newUpdateEndOfMeetingReminder,
    updateEndOfMeetingReminder_reminderType,
    updateEndOfMeetingReminder_reminderAtMinutes,
    updateEndOfMeetingReminder_enabled,

    -- * UpdateInstantBooking
    UpdateInstantBooking (..),
    newUpdateInstantBooking,
    updateInstantBooking_durationInMinutes,
    updateInstantBooking_enabled,

    -- * UpdateMeetingRoomConfiguration
    UpdateMeetingRoomConfiguration (..),
    newUpdateMeetingRoomConfiguration,
    updateMeetingRoomConfiguration_roomUtilizationMetricsEnabled,
    updateMeetingRoomConfiguration_endOfMeetingReminder,
    updateMeetingRoomConfiguration_instantBooking,
    updateMeetingRoomConfiguration_requireCheckIn,

    -- * UpdateRequireCheckIn
    UpdateRequireCheckIn (..),
    newUpdateRequireCheckIn,
    updateRequireCheckIn_releaseAfterMinutes,
    updateRequireCheckIn_enabled,

    -- * UserData
    UserData (..),
    newUserData,
    userData_userArn,
    userData_enrollmentId,
    userData_email,
    userData_enrollmentStatus,
    userData_firstName,
    userData_lastName,
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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-09@ of the Amazon Alexa For Business SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "AlexaBusiness",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "a4b",
      Core._serviceSigningName = "a4b",
      Core._serviceVersion = "2017-11-09",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "AlexaBusiness",
      Core._serviceRetry = retry
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The resource is not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | The service linked role is locked for deletion.
_InvalidServiceLinkedRoleStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidServiceLinkedRoleStateException =
  Core._MatchServiceError
    defaultService
    "InvalidServiceLinkedRoleStateException"

-- | The caller has no permissions to operate on the resource involved in the
-- API call.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"

-- | The attempt to update a user is invalid due to the user\'s current
-- status.
_InvalidUserStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUserStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidUserStatusException"

-- | Another resource is associated with the resource in the request.
_ResourceAssociatedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAssociatedException =
  Core._MatchServiceError
    defaultService
    "ResourceAssociatedException"

-- | There is a concurrent modification of resources.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The request failed because this device is no longer registered and
-- therefore no longer managed by this account.
_DeviceNotRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeviceNotRegisteredException =
  Core._MatchServiceError
    defaultService
    "DeviceNotRegisteredException"

-- | The Certificate Authority can\'t issue or revoke a certificate.
_InvalidCertificateAuthorityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateAuthorityException =
  Core._MatchServiceError
    defaultService
    "InvalidCertificateAuthorityException"

-- | The name sent in the request is already in use.
_NameInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NameInUseException =
  Core._MatchServiceError
    defaultService
    "NameInUseException"

-- | The resource in the request is already in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | You are performing an action that would put you beyond your account\'s
-- limits.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The resource being created already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | The device is in an invalid state.
_InvalidDeviceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeviceException =
  Core._MatchServiceError
    defaultService
    "InvalidDeviceException"

-- | The skill must be linked to a third-party account.
_SkillNotLinkedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SkillNotLinkedException =
  Core._MatchServiceError
    defaultService
    "SkillNotLinkedException"

-- | A password in SecretsManager is in an invalid state.
_InvalidSecretsManagerResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSecretsManagerResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidSecretsManagerResourceException"
