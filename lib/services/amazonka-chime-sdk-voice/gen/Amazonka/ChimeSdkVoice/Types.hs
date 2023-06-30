{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSdkVoice.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _BadRequestException,
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _ResourceLimitExceededException,
    _ServiceFailureException,
    _ServiceUnavailableException,
    _ThrottledClientException,
    _UnauthorizedClientException,

    -- * AlexaSkillStatus
    AlexaSkillStatus (..),

    -- * CallingNameStatus
    CallingNameStatus (..),

    -- * Capability
    Capability (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * GeoMatchLevel
    GeoMatchLevel (..),

    -- * NotificationTarget
    NotificationTarget (..),

    -- * NumberSelectionBehavior
    NumberSelectionBehavior (..),

    -- * OrderedPhoneNumberStatus
    OrderedPhoneNumberStatus (..),

    -- * OriginationRouteProtocol
    OriginationRouteProtocol (..),

    -- * PhoneNumberAssociationName
    PhoneNumberAssociationName (..),

    -- * PhoneNumberOrderStatus
    PhoneNumberOrderStatus (..),

    -- * PhoneNumberOrderType
    PhoneNumberOrderType (..),

    -- * PhoneNumberProductType
    PhoneNumberProductType (..),

    -- * PhoneNumberStatus
    PhoneNumberStatus (..),

    -- * PhoneNumberType
    PhoneNumberType (..),

    -- * ProxySessionStatus
    ProxySessionStatus (..),

    -- * SipRuleTriggerType
    SipRuleTriggerType (..),

    -- * VoiceConnectorAwsRegion
    VoiceConnectorAwsRegion (..),

    -- * Address
    Address (..),
    newAddress,
    address_city,
    address_country,
    address_postDirectional,
    address_postalCode,
    address_postalCodePlus4,
    address_preDirectional,
    address_state,
    address_streetName,
    address_streetNumber,
    address_streetSuffix,

    -- * CandidateAddress
    CandidateAddress (..),
    newCandidateAddress,
    candidateAddress_city,
    candidateAddress_country,
    candidateAddress_postalCode,
    candidateAddress_postalCodePlus4,
    candidateAddress_state,
    candidateAddress_streetInfo,
    candidateAddress_streetNumber,

    -- * Credential
    Credential (..),
    newCredential,
    credential_password,
    credential_username,

    -- * DNISEmergencyCallingConfiguration
    DNISEmergencyCallingConfiguration (..),
    newDNISEmergencyCallingConfiguration,
    dNISEmergencyCallingConfiguration_testPhoneNumber,
    dNISEmergencyCallingConfiguration_emergencyPhoneNumber,
    dNISEmergencyCallingConfiguration_callingCountry,

    -- * EmergencyCallingConfiguration
    EmergencyCallingConfiguration (..),
    newEmergencyCallingConfiguration,
    emergencyCallingConfiguration_dnis,

    -- * GeoMatchParams
    GeoMatchParams (..),
    newGeoMatchParams,
    geoMatchParams_country,
    geoMatchParams_areaCode,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_enableSIPLogs,

    -- * OrderedPhoneNumber
    OrderedPhoneNumber (..),
    newOrderedPhoneNumber,
    orderedPhoneNumber_e164PhoneNumber,
    orderedPhoneNumber_status,

    -- * Origination
    Origination (..),
    newOrigination,
    origination_disabled,
    origination_routes,

    -- * OriginationRoute
    OriginationRoute (..),
    newOriginationRoute,
    originationRoute_host,
    originationRoute_port,
    originationRoute_priority,
    originationRoute_protocol,
    originationRoute_weight,

    -- * Participant
    Participant (..),
    newParticipant,
    participant_phoneNumber,
    participant_proxyPhoneNumber,

    -- * PhoneNumber
    PhoneNumber (..),
    newPhoneNumber,
    phoneNumber_associations,
    phoneNumber_callingName,
    phoneNumber_callingNameStatus,
    phoneNumber_capabilities,
    phoneNumber_country,
    phoneNumber_createdTimestamp,
    phoneNumber_deletionTimestamp,
    phoneNumber_e164PhoneNumber,
    phoneNumber_orderId,
    phoneNumber_phoneNumberId,
    phoneNumber_productType,
    phoneNumber_status,
    phoneNumber_type,
    phoneNumber_updatedTimestamp,

    -- * PhoneNumberAssociation
    PhoneNumberAssociation (..),
    newPhoneNumberAssociation,
    phoneNumberAssociation_associatedTimestamp,
    phoneNumberAssociation_name,
    phoneNumberAssociation_value,

    -- * PhoneNumberCapabilities
    PhoneNumberCapabilities (..),
    newPhoneNumberCapabilities,
    phoneNumberCapabilities_inboundCall,
    phoneNumberCapabilities_inboundMMS,
    phoneNumberCapabilities_inboundSMS,
    phoneNumberCapabilities_outboundCall,
    phoneNumberCapabilities_outboundMMS,
    phoneNumberCapabilities_outboundSMS,

    -- * PhoneNumberCountry
    PhoneNumberCountry (..),
    newPhoneNumberCountry,
    phoneNumberCountry_countryCode,
    phoneNumberCountry_supportedPhoneNumberTypes,

    -- * PhoneNumberError
    PhoneNumberError (..),
    newPhoneNumberError,
    phoneNumberError_errorCode,
    phoneNumberError_errorMessage,
    phoneNumberError_phoneNumberId,

    -- * PhoneNumberOrder
    PhoneNumberOrder (..),
    newPhoneNumberOrder,
    phoneNumberOrder_createdTimestamp,
    phoneNumberOrder_orderType,
    phoneNumberOrder_orderedPhoneNumbers,
    phoneNumberOrder_phoneNumberOrderId,
    phoneNumberOrder_productType,
    phoneNumberOrder_status,
    phoneNumberOrder_updatedTimestamp,

    -- * Proxy
    Proxy (..),
    newProxy,
    proxy_defaultSessionExpiryMinutes,
    proxy_disabled,
    proxy_fallBackPhoneNumber,
    proxy_phoneNumberCountries,

    -- * ProxySession
    ProxySession (..),
    newProxySession,
    proxySession_capabilities,
    proxySession_createdTimestamp,
    proxySession_endedTimestamp,
    proxySession_expiryMinutes,
    proxySession_geoMatchLevel,
    proxySession_geoMatchParams,
    proxySession_name,
    proxySession_numberSelectionBehavior,
    proxySession_participants,
    proxySession_proxySessionId,
    proxySession_status,
    proxySession_updatedTimestamp,
    proxySession_voiceConnectorId,

    -- * SipMediaApplication
    SipMediaApplication (..),
    newSipMediaApplication,
    sipMediaApplication_awsRegion,
    sipMediaApplication_createdTimestamp,
    sipMediaApplication_endpoints,
    sipMediaApplication_name,
    sipMediaApplication_sipMediaApplicationId,
    sipMediaApplication_updatedTimestamp,

    -- * SipMediaApplicationAlexaSkillConfiguration
    SipMediaApplicationAlexaSkillConfiguration (..),
    newSipMediaApplicationAlexaSkillConfiguration,
    sipMediaApplicationAlexaSkillConfiguration_alexaSkillStatus,
    sipMediaApplicationAlexaSkillConfiguration_alexaSkillIds,

    -- * SipMediaApplicationCall
    SipMediaApplicationCall (..),
    newSipMediaApplicationCall,
    sipMediaApplicationCall_transactionId,

    -- * SipMediaApplicationEndpoint
    SipMediaApplicationEndpoint (..),
    newSipMediaApplicationEndpoint,
    sipMediaApplicationEndpoint_lambdaArn,

    -- * SipMediaApplicationLoggingConfiguration
    SipMediaApplicationLoggingConfiguration (..),
    newSipMediaApplicationLoggingConfiguration,
    sipMediaApplicationLoggingConfiguration_enableSipMediaApplicationMessageLogs,

    -- * SipRule
    SipRule (..),
    newSipRule,
    sipRule_createdTimestamp,
    sipRule_disabled,
    sipRule_name,
    sipRule_sipRuleId,
    sipRule_targetApplications,
    sipRule_triggerType,
    sipRule_triggerValue,
    sipRule_updatedTimestamp,

    -- * SipRuleTargetApplication
    SipRuleTargetApplication (..),
    newSipRuleTargetApplication,
    sipRuleTargetApplication_awsRegion,
    sipRuleTargetApplication_priority,
    sipRuleTargetApplication_sipMediaApplicationId,

    -- * StreamingConfiguration
    StreamingConfiguration (..),
    newStreamingConfiguration,
    streamingConfiguration_streamingNotificationTargets,
    streamingConfiguration_dataRetentionInHours,
    streamingConfiguration_disabled,

    -- * StreamingNotificationTarget
    StreamingNotificationTarget (..),
    newStreamingNotificationTarget,
    streamingNotificationTarget_notificationTarget,

    -- * Termination
    Termination (..),
    newTermination,
    termination_callingRegions,
    termination_cidrAllowedList,
    termination_cpsLimit,
    termination_defaultPhoneNumber,
    termination_disabled,

    -- * TerminationHealth
    TerminationHealth (..),
    newTerminationHealth,
    terminationHealth_source,
    terminationHealth_timestamp,

    -- * UpdatePhoneNumberRequestItem
    UpdatePhoneNumberRequestItem (..),
    newUpdatePhoneNumberRequestItem,
    updatePhoneNumberRequestItem_callingName,
    updatePhoneNumberRequestItem_productType,
    updatePhoneNumberRequestItem_phoneNumberId,

    -- * VoiceConnector
    VoiceConnector (..),
    newVoiceConnector,
    voiceConnector_awsRegion,
    voiceConnector_createdTimestamp,
    voiceConnector_name,
    voiceConnector_outboundHostName,
    voiceConnector_requireEncryption,
    voiceConnector_updatedTimestamp,
    voiceConnector_voiceConnectorArn,
    voiceConnector_voiceConnectorId,

    -- * VoiceConnectorGroup
    VoiceConnectorGroup (..),
    newVoiceConnectorGroup,
    voiceConnectorGroup_createdTimestamp,
    voiceConnectorGroup_name,
    voiceConnectorGroup_updatedTimestamp,
    voiceConnectorGroup_voiceConnectorGroupArn,
    voiceConnectorGroup_voiceConnectorGroupId,
    voiceConnectorGroup_voiceConnectorItems,

    -- * VoiceConnectorItem
    VoiceConnectorItem (..),
    newVoiceConnectorItem,
    voiceConnectorItem_voiceConnectorId,
    voiceConnectorItem_priority,

    -- * VoiceConnectorSettings
    VoiceConnectorSettings (..),
    newVoiceConnectorSettings,
    voiceConnectorSettings_cdrBucket,
  )
where

import Amazonka.ChimeSdkVoice.Types.Address
import Amazonka.ChimeSdkVoice.Types.AlexaSkillStatus
import Amazonka.ChimeSdkVoice.Types.CallingNameStatus
import Amazonka.ChimeSdkVoice.Types.CandidateAddress
import Amazonka.ChimeSdkVoice.Types.Capability
import Amazonka.ChimeSdkVoice.Types.Credential
import Amazonka.ChimeSdkVoice.Types.DNISEmergencyCallingConfiguration
import Amazonka.ChimeSdkVoice.Types.EmergencyCallingConfiguration
import Amazonka.ChimeSdkVoice.Types.ErrorCode
import Amazonka.ChimeSdkVoice.Types.GeoMatchLevel
import Amazonka.ChimeSdkVoice.Types.GeoMatchParams
import Amazonka.ChimeSdkVoice.Types.LoggingConfiguration
import Amazonka.ChimeSdkVoice.Types.NotificationTarget
import Amazonka.ChimeSdkVoice.Types.NumberSelectionBehavior
import Amazonka.ChimeSdkVoice.Types.OrderedPhoneNumber
import Amazonka.ChimeSdkVoice.Types.OrderedPhoneNumberStatus
import Amazonka.ChimeSdkVoice.Types.Origination
import Amazonka.ChimeSdkVoice.Types.OriginationRoute
import Amazonka.ChimeSdkVoice.Types.OriginationRouteProtocol
import Amazonka.ChimeSdkVoice.Types.Participant
import Amazonka.ChimeSdkVoice.Types.PhoneNumber
import Amazonka.ChimeSdkVoice.Types.PhoneNumberAssociation
import Amazonka.ChimeSdkVoice.Types.PhoneNumberAssociationName
import Amazonka.ChimeSdkVoice.Types.PhoneNumberCapabilities
import Amazonka.ChimeSdkVoice.Types.PhoneNumberCountry
import Amazonka.ChimeSdkVoice.Types.PhoneNumberError
import Amazonka.ChimeSdkVoice.Types.PhoneNumberOrder
import Amazonka.ChimeSdkVoice.Types.PhoneNumberOrderStatus
import Amazonka.ChimeSdkVoice.Types.PhoneNumberOrderType
import Amazonka.ChimeSdkVoice.Types.PhoneNumberProductType
import Amazonka.ChimeSdkVoice.Types.PhoneNumberStatus
import Amazonka.ChimeSdkVoice.Types.PhoneNumberType
import Amazonka.ChimeSdkVoice.Types.Proxy
import Amazonka.ChimeSdkVoice.Types.ProxySession
import Amazonka.ChimeSdkVoice.Types.ProxySessionStatus
import Amazonka.ChimeSdkVoice.Types.SipMediaApplication
import Amazonka.ChimeSdkVoice.Types.SipMediaApplicationAlexaSkillConfiguration
import Amazonka.ChimeSdkVoice.Types.SipMediaApplicationCall
import Amazonka.ChimeSdkVoice.Types.SipMediaApplicationEndpoint
import Amazonka.ChimeSdkVoice.Types.SipMediaApplicationLoggingConfiguration
import Amazonka.ChimeSdkVoice.Types.SipRule
import Amazonka.ChimeSdkVoice.Types.SipRuleTargetApplication
import Amazonka.ChimeSdkVoice.Types.SipRuleTriggerType
import Amazonka.ChimeSdkVoice.Types.StreamingConfiguration
import Amazonka.ChimeSdkVoice.Types.StreamingNotificationTarget
import Amazonka.ChimeSdkVoice.Types.Termination
import Amazonka.ChimeSdkVoice.Types.TerminationHealth
import Amazonka.ChimeSdkVoice.Types.UpdatePhoneNumberRequestItem
import Amazonka.ChimeSdkVoice.Types.VoiceConnector
import Amazonka.ChimeSdkVoice.Types.VoiceConnectorAwsRegion
import Amazonka.ChimeSdkVoice.Types.VoiceConnectorGroup
import Amazonka.ChimeSdkVoice.Types.VoiceConnectorItem
import Amazonka.ChimeSdkVoice.Types.VoiceConnectorSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-08-03@ of the Amazon Chime SDK Voice SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ChimeSdkVoice",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "voice-chime",
      Core.signingName = "chime",
      Core.version = "2022-08-03",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ChimeSdkVoice",
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

-- | Prism for AccessDeniedException' errors.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Prism for BadRequestException' errors.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | Prism for ConflictException' errors.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Prism for ForbiddenException' errors.
_ForbiddenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Prism for NotFoundException' errors.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Prism for ResourceLimitExceededException' errors.
_ResourceLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | Prism for ServiceFailureException' errors.
_ServiceFailureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceFailureException =
  Core._MatchServiceError
    defaultService
    "ServiceFailureException"
    Prelude.. Core.hasStatus 500

-- | Prism for ServiceUnavailableException' errors.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | Prism for ThrottledClientException' errors.
_ThrottledClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottledClientException =
  Core._MatchServiceError
    defaultService
    "ThrottledClientException"
    Prelude.. Core.hasStatus 429

-- | Prism for UnauthorizedClientException' errors.
_UnauthorizedClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedClientException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedClientException"
    Prelude.. Core.hasStatus 401
