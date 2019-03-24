{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types
    (
    -- * Service Configuration
      alexaBusiness

    -- * Errors
    , _SkillNotLinkedException
    , _InvalidCertificateAuthorityException
    , _DeviceNotRegisteredException
    , _InvalidUserStatusException
    , _InvalidDeviceException
    , _NotFoundException
    , _NameInUseException
    , _ConcurrentModificationException
    , _UnauthorizedException
    , _AlreadyExistsException
    , _LimitExceededException
    , _ResourceInUseException

    -- * BusinessReportFailureCode
    , BusinessReportFailureCode (..)

    -- * BusinessReportFormat
    , BusinessReportFormat (..)

    -- * BusinessReportInterval
    , BusinessReportInterval (..)

    -- * BusinessReportStatus
    , BusinessReportStatus (..)

    -- * CommsProtocol
    , CommsProtocol (..)

    -- * ConferenceProviderType
    , ConferenceProviderType (..)

    -- * ConnectionStatus
    , ConnectionStatus (..)

    -- * DeviceEventType
    , DeviceEventType (..)

    -- * DeviceStatus
    , DeviceStatus (..)

    -- * DeviceStatusDetailCode
    , DeviceStatusDetailCode (..)

    -- * DistanceUnit
    , DistanceUnit (..)

    -- * EnablementType
    , EnablementType (..)

    -- * EnablementTypeFilter
    , EnablementTypeFilter (..)

    -- * EnrollmentStatus
    , EnrollmentStatus (..)

    -- * Feature
    , Feature (..)

    -- * RequirePin
    , RequirePin (..)

    -- * SkillType
    , SkillType (..)

    -- * SkillTypeFilter
    , SkillTypeFilter (..)

    -- * SortValue
    , SortValue (..)

    -- * TemperatureUnit
    , TemperatureUnit (..)

    -- * WakeWord
    , WakeWord (..)

    -- * AddressBook
    , AddressBook
    , addressBook
    , abAddressBookARN
    , abName
    , abDescription

    -- * AddressBookData
    , AddressBookData
    , addressBookData
    , abdAddressBookARN
    , abdName
    , abdDescription

    -- * BusinessReport
    , BusinessReport
    , businessReport
    , brStatus
    , brFailureCode
    , brDeliveryTime
    , brDownloadURL
    , brS3Location

    -- * BusinessReportContentRange
    , BusinessReportContentRange
    , businessReportContentRange
    , brcrInterval

    -- * BusinessReportRecurrence
    , BusinessReportRecurrence
    , businessReportRecurrence
    , brrStartDate

    -- * BusinessReportS3Location
    , BusinessReportS3Location
    , businessReportS3Location
    , brslPath
    , brslBucketName

    -- * BusinessReportSchedule
    , BusinessReportSchedule
    , businessReportSchedule
    , brsS3KeyPrefix
    , brsLastBusinessReport
    , brsFormat
    , brsRecurrence
    , brsScheduleName
    , brsScheduleARN
    , brsContentRange
    , brsS3BucketName

    -- * Category
    , Category
    , category
    , cCategoryName
    , cCategoryId

    -- * ConferencePreference
    , ConferencePreference
    , conferencePreference
    , cpDefaultConferenceProviderARN

    -- * ConferenceProvider
    , ConferenceProvider
    , conferenceProvider
    , cpMeetingSetting
    , cpARN
    , cpPSTNDialIn
    , cpName
    , cpType
    , cpIPDialIn

    -- * Contact
    , Contact
    , contact
    , cLastName
    , cContactARN
    , cPhoneNumber
    , cFirstName
    , cDisplayName

    -- * ContactData
    , ContactData
    , contactData
    , cdLastName
    , cdContactARN
    , cdPhoneNumber
    , cdFirstName
    , cdDisplayName

    -- * DeveloperInfo
    , DeveloperInfo
    , developerInfo
    , diEmail
    , diURL
    , diPrivacyPolicy
    , diDeveloperName

    -- * Device
    , Device
    , device
    , dDeviceStatus
    , dDeviceStatusInfo
    , dDeviceARN
    , dMACAddress
    , dDeviceName
    , dRoomARN
    , dSoftwareVersion
    , dDeviceType
    , dDeviceSerialNumber

    -- * DeviceData
    , DeviceData
    , deviceData
    , ddDeviceStatus
    , ddDeviceStatusInfo
    , ddDeviceARN
    , ddMACAddress
    , ddDeviceName
    , ddRoomARN
    , ddSoftwareVersion
    , ddDeviceType
    , ddRoomName
    , ddDeviceSerialNumber

    -- * DeviceEvent
    , DeviceEvent
    , deviceEvent
    , deValue
    , deType
    , deTimestamp

    -- * DeviceStatusDetail
    , DeviceStatusDetail
    , deviceStatusDetail
    , dsdCode

    -- * DeviceStatusInfo
    , DeviceStatusInfo
    , deviceStatusInfo
    , dsiDeviceStatusDetails
    , dsiConnectionStatus

    -- * Filter
    , Filter
    , filter'
    , fKey
    , fValues

    -- * IPDialIn
    , IPDialIn
    , ipDialIn
    , idiEndpoint
    , idiCommsProtocol

    -- * MeetingSetting
    , MeetingSetting
    , meetingSetting
    , msRequirePin

    -- * PSTNDialIn
    , PSTNDialIn
    , pSTNDialIn
    , pstndiCountryCode
    , pstndiPhoneNumber
    , pstndiOneClickIdDelay
    , pstndiOneClickPinDelay

    -- * Profile
    , Profile
    , profile
    , pSetupModeDisabled
    , pPSTNEnabled
    , pAddressBookARN
    , pDistanceUnit
    , pAddress
    , pProfileARN
    , pWakeWord
    , pProfileName
    , pTemperatureUnit
    , pTimezone
    , pMaxVolumeLimit
    , pIsDefault

    -- * ProfileData
    , ProfileData
    , profileData
    , pdDistanceUnit
    , pdAddress
    , pdProfileARN
    , pdWakeWord
    , pdProfileName
    , pdTemperatureUnit
    , pdTimezone
    , pdIsDefault

    -- * Room
    , Room
    , room
    , rProfileARN
    , rProviderCalendarId
    , rRoomARN
    , rRoomName
    , rDescription

    -- * RoomData
    , RoomData
    , roomData
    , rdProfileARN
    , rdProviderCalendarId
    , rdProfileName
    , rdRoomARN
    , rdRoomName
    , rdDescription

    -- * RoomSkillParameter
    , RoomSkillParameter
    , roomSkillParameter
    , rspParameterKey
    , rspParameterValue

    -- * SkillDetails
    , SkillDetails
    , skillDetails
    , sdSkillTypes
    , sdProductDescription
    , sdInvocationPhrase
    , sdDeveloperInfo
    , sdEndUserLicenseAgreement
    , sdGenericKeywords
    , sdReviews
    , sdReleaseDate
    , sdNewInThisVersionBulletPoints
    , sdBulletPoints

    -- * SkillGroup
    , SkillGroup
    , skillGroup
    , sgSkillGroupARN
    , sgDescription
    , sgSkillGroupName

    -- * SkillGroupData
    , SkillGroupData
    , skillGroupData
    , sgdSkillGroupARN
    , sgdDescription
    , sgdSkillGroupName

    -- * SkillSummary
    , SkillSummary
    , skillSummary
    , ssSkillId
    , ssSupportsLinking
    , ssSkillType
    , ssEnablementType
    , ssSkillName

    -- * SkillsStoreSkill
    , SkillsStoreSkill
    , skillsStoreSkill
    , sssSkillId
    , sssSupportsLinking
    , sssSampleUtterances
    , sssShortDescription
    , sssIconURL
    , sssSkillDetails
    , sssSkillName

    -- * SmartHomeAppliance
    , SmartHomeAppliance
    , smartHomeAppliance
    , shaFriendlyName
    , shaManufacturerName
    , shaDescription

    -- * Sort
    , Sort
    , sort
    , sKey
    , sValue

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * UserData
    , UserData
    , userData
    , udEmail
    , udLastName
    , udEnrollmentId
    , udUserARN
    , udFirstName
    , udEnrollmentStatus
    ) where

import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.AlexaBusiness.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-11-09@ of the Amazon Alexa For Business SDK configuration.
alexaBusiness :: Service
alexaBusiness =
  Service
    { _svcAbbrev = "AlexaBusiness"
    , _svcSigner = v4
    , _svcPrefix = "a4b"
    , _svcVersion = "2017-11-09"
    , _svcEndpoint = defaultEndpoint alexaBusiness
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "AlexaBusiness"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The skill must be linked to a third-party account.
--
--
_SkillNotLinkedException :: AsError a => Getting (First ServiceError) a ServiceError
_SkillNotLinkedException =
  _MatchServiceError alexaBusiness "SkillNotLinkedException"


-- | The Certificate Authority can't issue or revoke a certificate.
--
--
_InvalidCertificateAuthorityException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCertificateAuthorityException =
  _MatchServiceError alexaBusiness "InvalidCertificateAuthorityException"


-- | The request failed because this device is no longer registered and therefore no longer managed by this account.
--
--
_DeviceNotRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeviceNotRegisteredException =
  _MatchServiceError alexaBusiness "DeviceNotRegisteredException"


-- | The attempt to update a user is invalid due to the user's current status.
--
--
_InvalidUserStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidUserStatusException =
  _MatchServiceError alexaBusiness "InvalidUserStatusException"


-- | The device is in an invalid state.
--
--
_InvalidDeviceException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeviceException =
  _MatchServiceError alexaBusiness "InvalidDeviceException"


-- | The resource is not found.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _MatchServiceError alexaBusiness "NotFoundException"


-- | The name sent in the request is already in use.
--
--
_NameInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_NameInUseException = _MatchServiceError alexaBusiness "NameInUseException"


-- | There is a concurrent modification of resources.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
  _MatchServiceError alexaBusiness "ConcurrentModificationException"


-- | The caller has no permissions to operate on the resource involved in the API call.
--
--
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
  _MatchServiceError alexaBusiness "UnauthorizedException"


-- | The resource being created already exists.
--
--
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException =
  _MatchServiceError alexaBusiness "AlreadyExistsException"


-- | You are performing an action that would put you beyond your account's limits.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError alexaBusiness "LimitExceededException"


-- | The resource in the request is already in use.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
  _MatchServiceError alexaBusiness "ResourceInUseException"

