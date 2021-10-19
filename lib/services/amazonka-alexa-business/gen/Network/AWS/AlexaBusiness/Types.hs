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
    businessReport_status,
    businessReport_failureCode,
    businessReport_deliveryTime,
    businessReport_downloadUrl,
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
    businessReportS3Location_path,
    businessReportS3Location_bucketName,

    -- * BusinessReportSchedule
    BusinessReportSchedule (..),
    newBusinessReportSchedule,
    businessReportSchedule_s3KeyPrefix,
    businessReportSchedule_lastBusinessReport,
    businessReportSchedule_format,
    businessReportSchedule_recurrence,
    businessReportSchedule_scheduleName,
    businessReportSchedule_scheduleArn,
    businessReportSchedule_contentRange,
    businessReportSchedule_s3BucketName,

    -- * Category
    Category (..),
    newCategory,
    category_categoryName,
    category_categoryId,

    -- * ConferencePreference
    ConferencePreference (..),
    newConferencePreference,
    conferencePreference_defaultConferenceProviderArn,

    -- * ConferenceProvider
    ConferenceProvider (..),
    newConferenceProvider,
    conferenceProvider_meetingSetting,
    conferenceProvider_arn,
    conferenceProvider_pSTNDialIn,
    conferenceProvider_name,
    conferenceProvider_type,
    conferenceProvider_iPDialIn,

    -- * Contact
    Contact (..),
    newContact,
    contact_lastName,
    contact_contactArn,
    contact_phoneNumbers,
    contact_phoneNumber,
    contact_sipAddresses,
    contact_firstName,
    contact_displayName,

    -- * ContactData
    ContactData (..),
    newContactData,
    contactData_lastName,
    contactData_contactArn,
    contactData_phoneNumbers,
    contactData_phoneNumber,
    contactData_sipAddresses,
    contactData_firstName,
    contactData_displayName,

    -- * Content
    Content (..),
    newContent,
    content_audioList,
    content_textList,
    content_ssmlList,

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
    createMeetingRoomConfiguration_instantBooking,
    createMeetingRoomConfiguration_endOfMeetingReminder,
    createMeetingRoomConfiguration_requireCheckIn,
    createMeetingRoomConfiguration_roomUtilizationMetricsEnabled,

    -- * CreateRequireCheckIn
    CreateRequireCheckIn (..),
    newCreateRequireCheckIn,
    createRequireCheckIn_releaseAfterMinutes,
    createRequireCheckIn_enabled,

    -- * DeveloperInfo
    DeveloperInfo (..),
    newDeveloperInfo,
    developerInfo_email,
    developerInfo_url,
    developerInfo_privacyPolicy,
    developerInfo_developerName,

    -- * Device
    Device (..),
    newDevice,
    device_deviceStatus,
    device_deviceStatusInfo,
    device_deviceArn,
    device_macAddress,
    device_deviceName,
    device_roomArn,
    device_softwareVersion,
    device_deviceType,
    device_networkProfileInfo,
    device_deviceSerialNumber,

    -- * DeviceData
    DeviceData (..),
    newDeviceData,
    deviceData_deviceStatus,
    deviceData_networkProfileName,
    deviceData_deviceStatusInfo,
    deviceData_createdTime,
    deviceData_deviceArn,
    deviceData_networkProfileArn,
    deviceData_macAddress,
    deviceData_deviceName,
    deviceData_roomArn,
    deviceData_softwareVersion,
    deviceData_deviceType,
    deviceData_roomName,
    deviceData_deviceSerialNumber,

    -- * DeviceEvent
    DeviceEvent (..),
    newDeviceEvent,
    deviceEvent_value,
    deviceEvent_type,
    deviceEvent_timestamp,

    -- * DeviceNetworkProfileInfo
    DeviceNetworkProfileInfo (..),
    newDeviceNetworkProfileInfo,
    deviceNetworkProfileInfo_certificateArn,
    deviceNetworkProfileInfo_networkProfileArn,
    deviceNetworkProfileInfo_certificateExpirationTime,

    -- * DeviceStatusDetail
    DeviceStatusDetail (..),
    newDeviceStatusDetail,
    deviceStatusDetail_feature,
    deviceStatusDetail_code,

    -- * DeviceStatusInfo
    DeviceStatusInfo (..),
    newDeviceStatusInfo,
    deviceStatusInfo_connectionStatusUpdatedTime,
    deviceStatusInfo_deviceStatusDetails,
    deviceStatusInfo_connectionStatus,

    -- * EndOfMeetingReminder
    EndOfMeetingReminder (..),
    newEndOfMeetingReminder,
    endOfMeetingReminder_enabled,
    endOfMeetingReminder_reminderAtMinutes,
    endOfMeetingReminder_reminderType,

    -- * Filter
    Filter (..),
    newFilter,
    filter_key,
    filter_values,

    -- * Gateway
    Gateway (..),
    newGateway,
    gateway_arn,
    gateway_name,
    gateway_gatewayGroupArn,
    gateway_softwareVersion,
    gateway_description,

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
    gatewaySummary_name,
    gatewaySummary_gatewayGroupArn,
    gatewaySummary_softwareVersion,
    gatewaySummary_description,

    -- * IPDialIn
    IPDialIn (..),
    newIPDialIn,
    iPDialIn_endpoint,
    iPDialIn_commsProtocol,

    -- * InstantBooking
    InstantBooking (..),
    newInstantBooking,
    instantBooking_enabled,
    instantBooking_durationInMinutes,

    -- * MeetingRoomConfiguration
    MeetingRoomConfiguration (..),
    newMeetingRoomConfiguration,
    meetingRoomConfiguration_instantBooking,
    meetingRoomConfiguration_endOfMeetingReminder,
    meetingRoomConfiguration_requireCheckIn,
    meetingRoomConfiguration_roomUtilizationMetricsEnabled,

    -- * MeetingSetting
    MeetingSetting (..),
    newMeetingSetting,
    meetingSetting_requirePin,

    -- * NetworkProfile
    NetworkProfile (..),
    newNetworkProfile,
    networkProfile_networkProfileName,
    networkProfile_ssid,
    networkProfile_networkProfileArn,
    networkProfile_securityType,
    networkProfile_currentPassword,
    networkProfile_nextPassword,
    networkProfile_eapMethod,
    networkProfile_description,
    networkProfile_trustAnchors,
    networkProfile_certificateAuthorityArn,

    -- * NetworkProfileData
    NetworkProfileData (..),
    newNetworkProfileData,
    networkProfileData_networkProfileName,
    networkProfileData_ssid,
    networkProfileData_networkProfileArn,
    networkProfileData_securityType,
    networkProfileData_eapMethod,
    networkProfileData_description,
    networkProfileData_certificateAuthorityArn,

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
    profile_setupModeDisabled,
    profile_pSTNEnabled,
    profile_addressBookArn,
    profile_distanceUnit,
    profile_locale,
    profile_address,
    profile_profileArn,
    profile_wakeWord,
    profile_meetingRoomConfiguration,
    profile_profileName,
    profile_temperatureUnit,
    profile_dataRetentionOptIn,
    profile_timezone,
    profile_maxVolumeLimit,
    profile_isDefault,

    -- * ProfileData
    ProfileData (..),
    newProfileData,
    profileData_distanceUnit,
    profileData_locale,
    profileData_address,
    profileData_profileArn,
    profileData_wakeWord,
    profileData_profileName,
    profileData_temperatureUnit,
    profileData_timezone,
    profileData_isDefault,

    -- * RequireCheckIn
    RequireCheckIn (..),
    newRequireCheckIn,
    requireCheckIn_enabled,
    requireCheckIn_releaseAfterMinutes,

    -- * Room
    Room (..),
    newRoom,
    room_profileArn,
    room_providerCalendarId,
    room_roomArn,
    room_roomName,
    room_description,

    -- * RoomData
    RoomData (..),
    newRoomData,
    roomData_profileArn,
    roomData_providerCalendarId,
    roomData_profileName,
    roomData_roomArn,
    roomData_roomName,
    roomData_description,

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
    skillDetails_skillTypes,
    skillDetails_productDescription,
    skillDetails_invocationPhrase,
    skillDetails_developerInfo,
    skillDetails_endUserLicenseAgreement,
    skillDetails_genericKeywords,
    skillDetails_reviews,
    skillDetails_releaseDate,
    skillDetails_newInThisVersionBulletPoints,
    skillDetails_bulletPoints,

    -- * SkillGroup
    SkillGroup (..),
    newSkillGroup,
    skillGroup_skillGroupArn,
    skillGroup_description,
    skillGroup_skillGroupName,

    -- * SkillGroupData
    SkillGroupData (..),
    newSkillGroupData,
    skillGroupData_skillGroupArn,
    skillGroupData_description,
    skillGroupData_skillGroupName,

    -- * SkillSummary
    SkillSummary (..),
    newSkillSummary,
    skillSummary_skillId,
    skillSummary_supportsLinking,
    skillSummary_skillType,
    skillSummary_enablementType,
    skillSummary_skillName,

    -- * SkillsStoreSkill
    SkillsStoreSkill (..),
    newSkillsStoreSkill,
    skillsStoreSkill_skillId,
    skillsStoreSkill_supportsLinking,
    skillsStoreSkill_sampleUtterances,
    skillsStoreSkill_shortDescription,
    skillsStoreSkill_iconUrl,
    skillsStoreSkill_skillDetails,
    skillsStoreSkill_skillName,

    -- * SmartHomeAppliance
    SmartHomeAppliance (..),
    newSmartHomeAppliance,
    smartHomeAppliance_friendlyName,
    smartHomeAppliance_manufacturerName,
    smartHomeAppliance_description,

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
    updateEndOfMeetingReminder_enabled,
    updateEndOfMeetingReminder_reminderAtMinutes,
    updateEndOfMeetingReminder_reminderType,

    -- * UpdateInstantBooking
    UpdateInstantBooking (..),
    newUpdateInstantBooking,
    updateInstantBooking_enabled,
    updateInstantBooking_durationInMinutes,

    -- * UpdateMeetingRoomConfiguration
    UpdateMeetingRoomConfiguration (..),
    newUpdateMeetingRoomConfiguration,
    updateMeetingRoomConfiguration_instantBooking,
    updateMeetingRoomConfiguration_endOfMeetingReminder,
    updateMeetingRoomConfiguration_requireCheckIn,
    updateMeetingRoomConfiguration_roomUtilizationMetricsEnabled,

    -- * UpdateRequireCheckIn
    UpdateRequireCheckIn (..),
    newUpdateRequireCheckIn,
    updateRequireCheckIn_enabled,
    updateRequireCheckIn_releaseAfterMinutes,

    -- * UserData
    UserData (..),
    newUserData,
    userData_email,
    userData_lastName,
    userData_enrollmentId,
    userData_userArn,
    userData_firstName,
    userData_enrollmentStatus,
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The skill must be linked to a third-party account.
_SkillNotLinkedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SkillNotLinkedException =
  Core._MatchServiceError
    defaultService
    "SkillNotLinkedException"

-- | The Certificate Authority can\'t issue or revoke a certificate.
_InvalidCertificateAuthorityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateAuthorityException =
  Core._MatchServiceError
    defaultService
    "InvalidCertificateAuthorityException"

-- | The request failed because this device is no longer registered and
-- therefore no longer managed by this account.
_DeviceNotRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeviceNotRegisteredException =
  Core._MatchServiceError
    defaultService
    "DeviceNotRegisteredException"

-- | Another resource is associated with the resource in the request.
_ResourceAssociatedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAssociatedException =
  Core._MatchServiceError
    defaultService
    "ResourceAssociatedException"

-- | The attempt to update a user is invalid due to the user\'s current
-- status.
_InvalidUserStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUserStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidUserStatusException"

-- | The device is in an invalid state.
_InvalidDeviceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeviceException =
  Core._MatchServiceError
    defaultService
    "InvalidDeviceException"

-- | The service linked role is locked for deletion.
_InvalidServiceLinkedRoleStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidServiceLinkedRoleStateException =
  Core._MatchServiceError
    defaultService
    "InvalidServiceLinkedRoleStateException"

-- | The resource is not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | The name sent in the request is already in use.
_NameInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NameInUseException =
  Core._MatchServiceError
    defaultService
    "NameInUseException"

-- | A password in SecretsManager is in an invalid state.
_InvalidSecretsManagerResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSecretsManagerResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidSecretsManagerResourceException"

-- | There is a concurrent modification of resources.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The caller has no permissions to operate on the resource involved in the
-- API call.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"

-- | The resource being created already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | You are performing an action that would put you beyond your account\'s
-- limits.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The resource in the request is already in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
