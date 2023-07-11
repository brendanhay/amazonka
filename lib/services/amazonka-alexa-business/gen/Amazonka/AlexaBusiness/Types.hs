{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AlexaBusiness.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AlreadyExistsException,
    _ConcurrentModificationException,
    _DeviceNotRegisteredException,
    _InvalidCertificateAuthorityException,
    _InvalidDeviceException,
    _InvalidSecretsManagerResourceException,
    _InvalidServiceLinkedRoleStateException,
    _InvalidUserStatusException,
    _LimitExceededException,
    _NameInUseException,
    _NotFoundException,
    _ResourceAssociatedException,
    _ResourceInUseException,
    _SkillNotLinkedException,
    _UnauthorizedException,

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
    addressBook_description,
    addressBook_name,

    -- * AddressBookData
    AddressBookData (..),
    newAddressBookData,
    addressBookData_addressBookArn,
    addressBookData_description,
    addressBookData_name,

    -- * Audio
    Audio (..),
    newAudio,
    audio_locale,
    audio_location,

    -- * BusinessReport
    BusinessReport (..),
    newBusinessReport,
    businessReport_deliveryTime,
    businessReport_downloadUrl,
    businessReport_failureCode,
    businessReport_s3Location,
    businessReport_status,

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
    businessReportSchedule_lastBusinessReport,
    businessReportSchedule_recurrence,
    businessReportSchedule_s3BucketName,
    businessReportSchedule_s3KeyPrefix,
    businessReportSchedule_scheduleArn,
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
    conferenceProvider_arn,
    conferenceProvider_iPDialIn,
    conferenceProvider_meetingSetting,
    conferenceProvider_name,
    conferenceProvider_pSTNDialIn,
    conferenceProvider_type,

    -- * Contact
    Contact (..),
    newContact,
    contact_contactArn,
    contact_displayName,
    contact_firstName,
    contact_lastName,
    contact_phoneNumber,
    contact_phoneNumbers,
    contact_sipAddresses,

    -- * ContactData
    ContactData (..),
    newContactData,
    contactData_contactArn,
    contactData_displayName,
    contactData_firstName,
    contactData_lastName,
    contactData_phoneNumber,
    contactData_phoneNumbers,
    contactData_sipAddresses,

    -- * Content
    Content (..),
    newContent,
    content_audioList,
    content_ssmlList,
    content_textList,

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
    createMeetingRoomConfiguration_endOfMeetingReminder,
    createMeetingRoomConfiguration_instantBooking,
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
    developerInfo_developerName,
    developerInfo_email,
    developerInfo_privacyPolicy,
    developerInfo_url,

    -- * Device
    Device (..),
    newDevice,
    device_deviceArn,
    device_deviceName,
    device_deviceSerialNumber,
    device_deviceStatus,
    device_deviceStatusInfo,
    device_deviceType,
    device_macAddress,
    device_networkProfileInfo,
    device_roomArn,
    device_softwareVersion,

    -- * DeviceData
    DeviceData (..),
    newDeviceData,
    deviceData_createdTime,
    deviceData_deviceArn,
    deviceData_deviceName,
    deviceData_deviceSerialNumber,
    deviceData_deviceStatus,
    deviceData_deviceStatusInfo,
    deviceData_deviceType,
    deviceData_macAddress,
    deviceData_networkProfileArn,
    deviceData_networkProfileName,
    deviceData_roomArn,
    deviceData_roomName,
    deviceData_softwareVersion,

    -- * DeviceEvent
    DeviceEvent (..),
    newDeviceEvent,
    deviceEvent_timestamp,
    deviceEvent_type,
    deviceEvent_value,

    -- * DeviceNetworkProfileInfo
    DeviceNetworkProfileInfo (..),
    newDeviceNetworkProfileInfo,
    deviceNetworkProfileInfo_certificateArn,
    deviceNetworkProfileInfo_certificateExpirationTime,
    deviceNetworkProfileInfo_networkProfileArn,

    -- * DeviceStatusDetail
    DeviceStatusDetail (..),
    newDeviceStatusDetail,
    deviceStatusDetail_code,
    deviceStatusDetail_feature,

    -- * DeviceStatusInfo
    DeviceStatusInfo (..),
    newDeviceStatusInfo,
    deviceStatusInfo_connectionStatus,
    deviceStatusInfo_connectionStatusUpdatedTime,
    deviceStatusInfo_deviceStatusDetails,

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
    gateway_description,
    gateway_gatewayGroupArn,
    gateway_name,
    gateway_softwareVersion,

    -- * GatewayGroup
    GatewayGroup (..),
    newGatewayGroup,
    gatewayGroup_arn,
    gatewayGroup_description,
    gatewayGroup_name,

    -- * GatewayGroupSummary
    GatewayGroupSummary (..),
    newGatewayGroupSummary,
    gatewayGroupSummary_arn,
    gatewayGroupSummary_description,
    gatewayGroupSummary_name,

    -- * GatewaySummary
    GatewaySummary (..),
    newGatewaySummary,
    gatewaySummary_arn,
    gatewaySummary_description,
    gatewaySummary_gatewayGroupArn,
    gatewaySummary_name,
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
    meetingRoomConfiguration_endOfMeetingReminder,
    meetingRoomConfiguration_instantBooking,
    meetingRoomConfiguration_requireCheckIn,
    meetingRoomConfiguration_roomUtilizationMetricsEnabled,

    -- * MeetingSetting
    MeetingSetting (..),
    newMeetingSetting,
    meetingSetting_requirePin,

    -- * NetworkProfile
    NetworkProfile (..),
    newNetworkProfile,
    networkProfile_certificateAuthorityArn,
    networkProfile_currentPassword,
    networkProfile_description,
    networkProfile_eapMethod,
    networkProfile_networkProfileArn,
    networkProfile_networkProfileName,
    networkProfile_nextPassword,
    networkProfile_securityType,
    networkProfile_ssid,
    networkProfile_trustAnchors,

    -- * NetworkProfileData
    NetworkProfileData (..),
    newNetworkProfileData,
    networkProfileData_certificateAuthorityArn,
    networkProfileData_description,
    networkProfileData_eapMethod,
    networkProfileData_networkProfileArn,
    networkProfileData_networkProfileName,
    networkProfileData_securityType,
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
    profile_address,
    profile_addressBookArn,
    profile_dataRetentionOptIn,
    profile_distanceUnit,
    profile_isDefault,
    profile_locale,
    profile_maxVolumeLimit,
    profile_meetingRoomConfiguration,
    profile_pSTNEnabled,
    profile_profileArn,
    profile_profileName,
    profile_setupModeDisabled,
    profile_temperatureUnit,
    profile_timezone,
    profile_wakeWord,

    -- * ProfileData
    ProfileData (..),
    newProfileData,
    profileData_address,
    profileData_distanceUnit,
    profileData_isDefault,
    profileData_locale,
    profileData_profileArn,
    profileData_profileName,
    profileData_temperatureUnit,
    profileData_timezone,
    profileData_wakeWord,

    -- * RequireCheckIn
    RequireCheckIn (..),
    newRequireCheckIn,
    requireCheckIn_enabled,
    requireCheckIn_releaseAfterMinutes,

    -- * Room
    Room (..),
    newRoom,
    room_description,
    room_profileArn,
    room_providerCalendarId,
    room_roomArn,
    room_roomName,

    -- * RoomData
    RoomData (..),
    newRoomData,
    roomData_description,
    roomData_profileArn,
    roomData_profileName,
    roomData_providerCalendarId,
    roomData_roomArn,
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
    skillDetails_bulletPoints,
    skillDetails_developerInfo,
    skillDetails_endUserLicenseAgreement,
    skillDetails_genericKeywords,
    skillDetails_invocationPhrase,
    skillDetails_newInThisVersionBulletPoints,
    skillDetails_productDescription,
    skillDetails_releaseDate,
    skillDetails_reviews,
    skillDetails_skillTypes,

    -- * SkillGroup
    SkillGroup (..),
    newSkillGroup,
    skillGroup_description,
    skillGroup_skillGroupArn,
    skillGroup_skillGroupName,

    -- * SkillGroupData
    SkillGroupData (..),
    newSkillGroupData,
    skillGroupData_description,
    skillGroupData_skillGroupArn,
    skillGroupData_skillGroupName,

    -- * SkillSummary
    SkillSummary (..),
    newSkillSummary,
    skillSummary_enablementType,
    skillSummary_skillId,
    skillSummary_skillName,
    skillSummary_skillType,
    skillSummary_supportsLinking,

    -- * SkillsStoreSkill
    SkillsStoreSkill (..),
    newSkillsStoreSkill,
    skillsStoreSkill_iconUrl,
    skillsStoreSkill_sampleUtterances,
    skillsStoreSkill_shortDescription,
    skillsStoreSkill_skillDetails,
    skillsStoreSkill_skillId,
    skillsStoreSkill_skillName,
    skillsStoreSkill_supportsLinking,

    -- * SmartHomeAppliance
    SmartHomeAppliance (..),
    newSmartHomeAppliance,
    smartHomeAppliance_description,
    smartHomeAppliance_friendlyName,
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
    updateEndOfMeetingReminder_enabled,
    updateEndOfMeetingReminder_reminderAtMinutes,
    updateEndOfMeetingReminder_reminderType,

    -- * UpdateInstantBooking
    UpdateInstantBooking (..),
    newUpdateInstantBooking,
    updateInstantBooking_durationInMinutes,
    updateInstantBooking_enabled,

    -- * UpdateMeetingRoomConfiguration
    UpdateMeetingRoomConfiguration (..),
    newUpdateMeetingRoomConfiguration,
    updateMeetingRoomConfiguration_endOfMeetingReminder,
    updateMeetingRoomConfiguration_instantBooking,
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
    userData_enrollmentId,
    userData_enrollmentStatus,
    userData_firstName,
    userData_lastName,
    userData_userArn,
  )
where

import Amazonka.AlexaBusiness.Types.AddressBook
import Amazonka.AlexaBusiness.Types.AddressBookData
import Amazonka.AlexaBusiness.Types.Audio
import Amazonka.AlexaBusiness.Types.BusinessReport
import Amazonka.AlexaBusiness.Types.BusinessReportContentRange
import Amazonka.AlexaBusiness.Types.BusinessReportFailureCode
import Amazonka.AlexaBusiness.Types.BusinessReportFormat
import Amazonka.AlexaBusiness.Types.BusinessReportInterval
import Amazonka.AlexaBusiness.Types.BusinessReportRecurrence
import Amazonka.AlexaBusiness.Types.BusinessReportS3Location
import Amazonka.AlexaBusiness.Types.BusinessReportSchedule
import Amazonka.AlexaBusiness.Types.BusinessReportStatus
import Amazonka.AlexaBusiness.Types.Category
import Amazonka.AlexaBusiness.Types.CommsProtocol
import Amazonka.AlexaBusiness.Types.ConferencePreference
import Amazonka.AlexaBusiness.Types.ConferenceProvider
import Amazonka.AlexaBusiness.Types.ConferenceProviderType
import Amazonka.AlexaBusiness.Types.ConnectionStatus
import Amazonka.AlexaBusiness.Types.Contact
import Amazonka.AlexaBusiness.Types.ContactData
import Amazonka.AlexaBusiness.Types.Content
import Amazonka.AlexaBusiness.Types.CreateEndOfMeetingReminder
import Amazonka.AlexaBusiness.Types.CreateInstantBooking
import Amazonka.AlexaBusiness.Types.CreateMeetingRoomConfiguration
import Amazonka.AlexaBusiness.Types.CreateRequireCheckIn
import Amazonka.AlexaBusiness.Types.DeveloperInfo
import Amazonka.AlexaBusiness.Types.Device
import Amazonka.AlexaBusiness.Types.DeviceData
import Amazonka.AlexaBusiness.Types.DeviceEvent
import Amazonka.AlexaBusiness.Types.DeviceEventType
import Amazonka.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Amazonka.AlexaBusiness.Types.DeviceStatus
import Amazonka.AlexaBusiness.Types.DeviceStatusDetail
import Amazonka.AlexaBusiness.Types.DeviceStatusDetailCode
import Amazonka.AlexaBusiness.Types.DeviceStatusInfo
import Amazonka.AlexaBusiness.Types.DeviceUsageType
import Amazonka.AlexaBusiness.Types.DistanceUnit
import Amazonka.AlexaBusiness.Types.EnablementType
import Amazonka.AlexaBusiness.Types.EnablementTypeFilter
import Amazonka.AlexaBusiness.Types.EndOfMeetingReminder
import Amazonka.AlexaBusiness.Types.EndOfMeetingReminderType
import Amazonka.AlexaBusiness.Types.EnrollmentStatus
import Amazonka.AlexaBusiness.Types.Feature
import Amazonka.AlexaBusiness.Types.Filter
import Amazonka.AlexaBusiness.Types.Gateway
import Amazonka.AlexaBusiness.Types.GatewayGroup
import Amazonka.AlexaBusiness.Types.GatewayGroupSummary
import Amazonka.AlexaBusiness.Types.GatewaySummary
import Amazonka.AlexaBusiness.Types.IPDialIn
import Amazonka.AlexaBusiness.Types.InstantBooking
import Amazonka.AlexaBusiness.Types.Locale
import Amazonka.AlexaBusiness.Types.MeetingRoomConfiguration
import Amazonka.AlexaBusiness.Types.MeetingSetting
import Amazonka.AlexaBusiness.Types.NetworkEapMethod
import Amazonka.AlexaBusiness.Types.NetworkProfile
import Amazonka.AlexaBusiness.Types.NetworkProfileData
import Amazonka.AlexaBusiness.Types.NetworkSecurityType
import Amazonka.AlexaBusiness.Types.PSTNDialIn
import Amazonka.AlexaBusiness.Types.PhoneNumber
import Amazonka.AlexaBusiness.Types.PhoneNumberType
import Amazonka.AlexaBusiness.Types.Profile
import Amazonka.AlexaBusiness.Types.ProfileData
import Amazonka.AlexaBusiness.Types.RequireCheckIn
import Amazonka.AlexaBusiness.Types.RequirePin
import Amazonka.AlexaBusiness.Types.Room
import Amazonka.AlexaBusiness.Types.RoomData
import Amazonka.AlexaBusiness.Types.RoomSkillParameter
import Amazonka.AlexaBusiness.Types.SipAddress
import Amazonka.AlexaBusiness.Types.SipType
import Amazonka.AlexaBusiness.Types.SkillDetails
import Amazonka.AlexaBusiness.Types.SkillGroup
import Amazonka.AlexaBusiness.Types.SkillGroupData
import Amazonka.AlexaBusiness.Types.SkillSummary
import Amazonka.AlexaBusiness.Types.SkillType
import Amazonka.AlexaBusiness.Types.SkillTypeFilter
import Amazonka.AlexaBusiness.Types.SkillsStoreSkill
import Amazonka.AlexaBusiness.Types.SmartHomeAppliance
import Amazonka.AlexaBusiness.Types.Sort
import Amazonka.AlexaBusiness.Types.SortValue
import Amazonka.AlexaBusiness.Types.Ssml
import Amazonka.AlexaBusiness.Types.Tag
import Amazonka.AlexaBusiness.Types.TemperatureUnit
import Amazonka.AlexaBusiness.Types.TextMessage
import Amazonka.AlexaBusiness.Types.UpdateEndOfMeetingReminder
import Amazonka.AlexaBusiness.Types.UpdateInstantBooking
import Amazonka.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
import Amazonka.AlexaBusiness.Types.UpdateRequireCheckIn
import Amazonka.AlexaBusiness.Types.UserData
import Amazonka.AlexaBusiness.Types.WakeWord
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-11-09@ of the Amazon Alexa For Business SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AlexaBusiness",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "a4b",
      Core.signingName = "a4b",
      Core.version = "2017-11-09",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AlexaBusiness",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The resource being created already exists.
_AlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | There is a concurrent modification of resources.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The request failed because this device is no longer registered and
-- therefore no longer managed by this account.
_DeviceNotRegisteredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DeviceNotRegisteredException =
  Core._MatchServiceError
    defaultService
    "DeviceNotRegisteredException"

-- | The Certificate Authority can\'t issue or revoke a certificate.
_InvalidCertificateAuthorityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCertificateAuthorityException =
  Core._MatchServiceError
    defaultService
    "InvalidCertificateAuthorityException"

-- | The device is in an invalid state.
_InvalidDeviceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDeviceException =
  Core._MatchServiceError
    defaultService
    "InvalidDeviceException"

-- | A password in SecretsManager is in an invalid state.
_InvalidSecretsManagerResourceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSecretsManagerResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidSecretsManagerResourceException"

-- | The service linked role is locked for deletion.
_InvalidServiceLinkedRoleStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidServiceLinkedRoleStateException =
  Core._MatchServiceError
    defaultService
    "InvalidServiceLinkedRoleStateException"

-- | The attempt to update a user is invalid due to the user\'s current
-- status.
_InvalidUserStatusException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidUserStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidUserStatusException"

-- | You are performing an action that would put you beyond your account\'s
-- limits.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The name sent in the request is already in use.
_NameInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NameInUseException =
  Core._MatchServiceError
    defaultService
    "NameInUseException"

-- | The resource is not found.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | Another resource is associated with the resource in the request.
_ResourceAssociatedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAssociatedException =
  Core._MatchServiceError
    defaultService
    "ResourceAssociatedException"

-- | The resource in the request is already in use.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The skill must be linked to a third-party account.
_SkillNotLinkedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SkillNotLinkedException =
  Core._MatchServiceError
    defaultService
    "SkillNotLinkedException"

-- | The caller has no permissions to operate on the resource involved in the
-- API call.
_UnauthorizedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
