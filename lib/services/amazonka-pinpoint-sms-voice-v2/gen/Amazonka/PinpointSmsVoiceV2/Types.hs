{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PinpointSmsVoiceV2.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

    -- * AccountAttributeName
    AccountAttributeName (..),

    -- * AccountLimitName
    AccountLimitName (..),

    -- * ConfigurationSetFilterName
    ConfigurationSetFilterName (..),

    -- * DestinationCountryParameterKey
    DestinationCountryParameterKey (..),

    -- * EventType
    EventType (..),

    -- * KeywordAction
    KeywordAction (..),

    -- * KeywordFilterName
    KeywordFilterName (..),

    -- * MessageType
    MessageType (..),

    -- * NumberCapability
    NumberCapability (..),

    -- * NumberStatus
    NumberStatus (..),

    -- * NumberType
    NumberType (..),

    -- * OptedOutFilterName
    OptedOutFilterName (..),

    -- * PhoneNumberFilterName
    PhoneNumberFilterName (..),

    -- * PoolFilterName
    PoolFilterName (..),

    -- * PoolOriginationIdentitiesFilterName
    PoolOriginationIdentitiesFilterName (..),

    -- * PoolStatus
    PoolStatus (..),

    -- * RequestableNumberType
    RequestableNumberType (..),

    -- * SenderIdFilterName
    SenderIdFilterName (..),

    -- * SpendLimitName
    SpendLimitName (..),

    -- * VoiceId
    VoiceId (..),

    -- * VoiceMessageBodyTextType
    VoiceMessageBodyTextType (..),

    -- * AccountAttribute
    AccountAttribute (..),
    newAccountAttribute,
    accountAttribute_name,
    accountAttribute_value,

    -- * AccountLimit
    AccountLimit (..),
    newAccountLimit,
    accountLimit_name,
    accountLimit_used,
    accountLimit_max,

    -- * CloudWatchLogsDestination
    CloudWatchLogsDestination (..),
    newCloudWatchLogsDestination,
    cloudWatchLogsDestination_iamRoleArn,
    cloudWatchLogsDestination_logGroupArn,

    -- * ConfigurationSetFilter
    ConfigurationSetFilter (..),
    newConfigurationSetFilter,
    configurationSetFilter_name,
    configurationSetFilter_values,

    -- * ConfigurationSetInformation
    ConfigurationSetInformation (..),
    newConfigurationSetInformation,
    configurationSetInformation_defaultSenderId,
    configurationSetInformation_defaultMessageType,
    configurationSetInformation_configurationSetArn,
    configurationSetInformation_configurationSetName,
    configurationSetInformation_eventDestinations,
    configurationSetInformation_createdTimestamp,

    -- * EventDestination
    EventDestination (..),
    newEventDestination,
    eventDestination_cloudWatchLogsDestination,
    eventDestination_snsDestination,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_eventDestinationName,
    eventDestination_enabled,
    eventDestination_matchingEventTypes,

    -- * KeywordFilter
    KeywordFilter (..),
    newKeywordFilter,
    keywordFilter_name,
    keywordFilter_values,

    -- * KeywordInformation
    KeywordInformation (..),
    newKeywordInformation,
    keywordInformation_keyword,
    keywordInformation_keywordMessage,
    keywordInformation_keywordAction,

    -- * KinesisFirehoseDestination
    KinesisFirehoseDestination (..),
    newKinesisFirehoseDestination,
    kinesisFirehoseDestination_iamRoleArn,
    kinesisFirehoseDestination_deliveryStreamArn,

    -- * OptOutListInformation
    OptOutListInformation (..),
    newOptOutListInformation,
    optOutListInformation_optOutListArn,
    optOutListInformation_optOutListName,
    optOutListInformation_createdTimestamp,

    -- * OptedOutFilter
    OptedOutFilter (..),
    newOptedOutFilter,
    optedOutFilter_name,
    optedOutFilter_values,

    -- * OptedOutNumberInformation
    OptedOutNumberInformation (..),
    newOptedOutNumberInformation,
    optedOutNumberInformation_optedOutNumber,
    optedOutNumberInformation_optedOutTimestamp,
    optedOutNumberInformation_endUserOptedOut,

    -- * OriginationIdentityMetadata
    OriginationIdentityMetadata (..),
    newOriginationIdentityMetadata,
    originationIdentityMetadata_originationIdentityArn,
    originationIdentityMetadata_originationIdentity,
    originationIdentityMetadata_isoCountryCode,
    originationIdentityMetadata_numberCapabilities,

    -- * PhoneNumberFilter
    PhoneNumberFilter (..),
    newPhoneNumberFilter,
    phoneNumberFilter_name,
    phoneNumberFilter_values,

    -- * PhoneNumberInformation
    PhoneNumberInformation (..),
    newPhoneNumberInformation,
    phoneNumberInformation_phoneNumberId,
    phoneNumberInformation_poolId,
    phoneNumberInformation_twoWayChannelArn,
    phoneNumberInformation_phoneNumberArn,
    phoneNumberInformation_phoneNumber,
    phoneNumberInformation_status,
    phoneNumberInformation_isoCountryCode,
    phoneNumberInformation_messageType,
    phoneNumberInformation_numberCapabilities,
    phoneNumberInformation_numberType,
    phoneNumberInformation_monthlyLeasingPrice,
    phoneNumberInformation_twoWayEnabled,
    phoneNumberInformation_selfManagedOptOutsEnabled,
    phoneNumberInformation_optOutListName,
    phoneNumberInformation_deletionProtectionEnabled,
    phoneNumberInformation_createdTimestamp,

    -- * PoolFilter
    PoolFilter (..),
    newPoolFilter,
    poolFilter_name,
    poolFilter_values,

    -- * PoolInformation
    PoolInformation (..),
    newPoolInformation,
    poolInformation_twoWayChannelArn,
    poolInformation_poolArn,
    poolInformation_poolId,
    poolInformation_status,
    poolInformation_messageType,
    poolInformation_twoWayEnabled,
    poolInformation_selfManagedOptOutsEnabled,
    poolInformation_optOutListName,
    poolInformation_sharedRoutesEnabled,
    poolInformation_deletionProtectionEnabled,
    poolInformation_createdTimestamp,

    -- * PoolOriginationIdentitiesFilter
    PoolOriginationIdentitiesFilter (..),
    newPoolOriginationIdentitiesFilter,
    poolOriginationIdentitiesFilter_name,
    poolOriginationIdentitiesFilter_values,

    -- * SenderIdAndCountry
    SenderIdAndCountry (..),
    newSenderIdAndCountry,
    senderIdAndCountry_senderId,
    senderIdAndCountry_isoCountryCode,

    -- * SenderIdFilter
    SenderIdFilter (..),
    newSenderIdFilter,
    senderIdFilter_name,
    senderIdFilter_values,

    -- * SenderIdInformation
    SenderIdInformation (..),
    newSenderIdInformation,
    senderIdInformation_senderIdArn,
    senderIdInformation_senderId,
    senderIdInformation_isoCountryCode,
    senderIdInformation_messageTypes,
    senderIdInformation_monthlyLeasingPrice,

    -- * SnsDestination
    SnsDestination (..),
    newSnsDestination,
    snsDestination_topicArn,

    -- * SpendLimit
    SpendLimit (..),
    newSpendLimit,
    spendLimit_name,
    spendLimit_enforcedLimit,
    spendLimit_maxLimit,
    spendLimit_overridden,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointSmsVoiceV2.Types.AccountAttribute
import Amazonka.PinpointSmsVoiceV2.Types.AccountAttributeName
import Amazonka.PinpointSmsVoiceV2.Types.AccountLimit
import Amazonka.PinpointSmsVoiceV2.Types.AccountLimitName
import Amazonka.PinpointSmsVoiceV2.Types.CloudWatchLogsDestination
import Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetFilter
import Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetFilterName
import Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetInformation
import Amazonka.PinpointSmsVoiceV2.Types.DestinationCountryParameterKey
import Amazonka.PinpointSmsVoiceV2.Types.EventDestination
import Amazonka.PinpointSmsVoiceV2.Types.EventType
import Amazonka.PinpointSmsVoiceV2.Types.KeywordAction
import Amazonka.PinpointSmsVoiceV2.Types.KeywordFilter
import Amazonka.PinpointSmsVoiceV2.Types.KeywordFilterName
import Amazonka.PinpointSmsVoiceV2.Types.KeywordInformation
import Amazonka.PinpointSmsVoiceV2.Types.KinesisFirehoseDestination
import Amazonka.PinpointSmsVoiceV2.Types.MessageType
import Amazonka.PinpointSmsVoiceV2.Types.NumberCapability
import Amazonka.PinpointSmsVoiceV2.Types.NumberStatus
import Amazonka.PinpointSmsVoiceV2.Types.NumberType
import Amazonka.PinpointSmsVoiceV2.Types.OptOutListInformation
import Amazonka.PinpointSmsVoiceV2.Types.OptedOutFilter
import Amazonka.PinpointSmsVoiceV2.Types.OptedOutFilterName
import Amazonka.PinpointSmsVoiceV2.Types.OptedOutNumberInformation
import Amazonka.PinpointSmsVoiceV2.Types.OriginationIdentityMetadata
import Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberFilter
import Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberFilterName
import Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberInformation
import Amazonka.PinpointSmsVoiceV2.Types.PoolFilter
import Amazonka.PinpointSmsVoiceV2.Types.PoolFilterName
import Amazonka.PinpointSmsVoiceV2.Types.PoolInformation
import Amazonka.PinpointSmsVoiceV2.Types.PoolOriginationIdentitiesFilter
import Amazonka.PinpointSmsVoiceV2.Types.PoolOriginationIdentitiesFilterName
import Amazonka.PinpointSmsVoiceV2.Types.PoolStatus
import Amazonka.PinpointSmsVoiceV2.Types.RequestableNumberType
import Amazonka.PinpointSmsVoiceV2.Types.SenderIdAndCountry
import Amazonka.PinpointSmsVoiceV2.Types.SenderIdFilter
import Amazonka.PinpointSmsVoiceV2.Types.SenderIdFilterName
import Amazonka.PinpointSmsVoiceV2.Types.SenderIdInformation
import Amazonka.PinpointSmsVoiceV2.Types.SnsDestination
import Amazonka.PinpointSmsVoiceV2.Types.SpendLimit
import Amazonka.PinpointSmsVoiceV2.Types.SpendLimitName
import Amazonka.PinpointSmsVoiceV2.Types.Tag
import Amazonka.PinpointSmsVoiceV2.Types.VoiceId
import Amazonka.PinpointSmsVoiceV2.Types.VoiceMessageBodyTextType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-03-31@ of the Amazon Pinpoint SMS Voice V2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "PinpointSmsVoiceV2",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "sms-voice",
      Core.signingName = "sms-voice",
      Core.version = "2022-03-31",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "PinpointSmsVoiceV2",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was denied because you don\'t have sufficient permissions to
-- access the resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The API encountered an unexpected error and couldn\'t complete the
-- request. You might be able to successfully issue the request again in
-- the future.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | A requested resource couldn\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Your request has conflicting operations. This can occur if you\'re
-- trying to perform more than one operation on the same resource at the
-- same time or it could be that the requested action isn\'t valid for the
-- current state or configuration of the resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | An error that occurred because too many requests were sent during a
-- certain amount of time.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | A validation exception for a field.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
