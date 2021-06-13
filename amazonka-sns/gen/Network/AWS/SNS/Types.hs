{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _KMSThrottlingException,
    _NotFoundException,
    _KMSInvalidStateException,
    _KMSNotFoundException,
    _ThrottledException,
    _TagLimitExceededException,
    _InternalErrorException,
    _TopicLimitExceededException,
    _KMSOptInRequired,
    _ConcurrentAccessException,
    _TagPolicyException,
    _PlatformApplicationDisabledException,
    _SubscriptionLimitExceededException,
    _StaleTagException,
    _InvalidParameterException,
    _EndpointDisabledException,
    _InvalidParameterValueException,
    _AuthorizationErrorException,
    _FilterPolicyLimitExceededException,
    _KMSAccessDeniedException,
    _ResourceNotFoundException,
    _InvalidSecurityException,
    _KMSDisabledException,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_attributes,
    endpoint_endpointArn,

    -- * MessageAttributeValue
    MessageAttributeValue (..),
    newMessageAttributeValue,
    messageAttributeValue_stringValue,
    messageAttributeValue_binaryValue,
    messageAttributeValue_dataType,

    -- * PlatformApplication
    PlatformApplication (..),
    newPlatformApplication,
    platformApplication_platformApplicationArn,
    platformApplication_attributes,

    -- * Subscription
    Subscription (..),
    newSubscription,
    subscription_topicArn,
    subscription_owner,
    subscription_subscriptionArn,
    subscription_protocol,
    subscription_endpoint,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Topic
    Topic (..),
    newTopic,
    topic_topicArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SNS.Types.Endpoint
import Network.AWS.SNS.Types.MessageAttributeValue
import Network.AWS.SNS.Types.PlatformApplication
import Network.AWS.SNS.Types.Subscription
import Network.AWS.SNS.Types.Tag
import Network.AWS.SNS.Types.Topic
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-03-31@ of the Amazon Simple Notification Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SNS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "sns",
      Core._serviceSigningName = "sns",
      Core._serviceVersion = "2010-03-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "SNS",
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

-- | The request was denied due to request throttling. For more information
-- about throttling, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second Limits>
-- in the /AWS Key Management Service Developer Guide./
_KMSThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSThrottlingException =
  Core._MatchServiceError
    defaultService
    "KMSThrottling"
    Prelude.. Core.hasStatus 400

-- | Indicates that the requested resource does not exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError defaultService "NotFound"
    Prelude.. Core.hasStatus 404

-- | The request was rejected because the state of the specified resource
-- isn\'t valid for this request. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
_KMSInvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidState"
    Prelude.. Core.hasStatus 400

-- | The request was rejected because the specified entity or resource can\'t
-- be found.
_KMSNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSNotFoundException =
  Core._MatchServiceError
    defaultService
    "KMSNotFound"
    Prelude.. Core.hasStatus 400

-- | Indicates that the rate at which requests have been submitted for this
-- action exceeds the limit for your account.
_ThrottledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottledException =
  Core._MatchServiceError defaultService "Throttled"
    Prelude.. Core.hasStatus 429

-- | Can\'t add more than 50 tags to a topic.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagLimitExceeded"
    Prelude.. Core.hasStatus 400

-- | Indicates an internal service error.
_InternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalError"
    Prelude.. Core.hasStatus 500

-- | Indicates that the customer already owns the maximum allowed number of
-- topics.
_TopicLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TopicLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TopicLimitExceeded"
    Prelude.. Core.hasStatus 403

-- | The AWS access key ID needs a subscription for the service.
_KMSOptInRequired :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSOptInRequired =
  Core._MatchServiceError
    defaultService
    "KMSOptInRequired"
    Prelude.. Core.hasStatus 403

-- | Can\'t perform multiple operations on a tag simultaneously. Perform the
-- operations sequentially.
_ConcurrentAccessException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentAccessException =
  Core._MatchServiceError
    defaultService
    "ConcurrentAccess"
    Prelude.. Core.hasStatus 400

-- | The request doesn\'t comply with the IAM tag policy. Correct your
-- request and then retry it.
_TagPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagPolicyException =
  Core._MatchServiceError defaultService "TagPolicy"
    Prelude.. Core.hasStatus 400

-- | Exception error indicating platform application disabled.
_PlatformApplicationDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PlatformApplicationDisabledException =
  Core._MatchServiceError
    defaultService
    "PlatformApplicationDisabled"
    Prelude.. Core.hasStatus 400

-- | Indicates that the customer already owns the maximum allowed number of
-- subscriptions.
_SubscriptionLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "SubscriptionLimitExceeded"
    Prelude.. Core.hasStatus 403

-- | A tag has been added to a resource with the same ARN as a deleted
-- resource. Wait a short while and then retry the operation.
_StaleTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StaleTagException =
  Core._MatchServiceError defaultService "StaleTag"
    Prelude.. Core.hasStatus 400

-- | Indicates that a request parameter does not comply with the associated
-- constraints.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameter"
    Prelude.. Core.hasStatus 400

-- | Exception error indicating endpoint disabled.
_EndpointDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EndpointDisabledException =
  Core._MatchServiceError
    defaultService
    "EndpointDisabled"
    Prelude.. Core.hasStatus 400

-- | Indicates that a request parameter does not comply with the associated
-- constraints.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "ParameterValueInvalid"
    Prelude.. Core.hasStatus 400

-- | Indicates that the user has been denied access to the requested
-- resource.
_AuthorizationErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationErrorException =
  Core._MatchServiceError
    defaultService
    "AuthorizationError"
    Prelude.. Core.hasStatus 403

-- | Indicates that the number of filter polices in your AWS account exceeds
-- the limit. To add more filter polices, submit an SNS Limit Increase case
-- in the AWS Support Center.
_FilterPolicyLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FilterPolicyLimitExceededException =
  Core._MatchServiceError
    defaultService
    "FilterPolicyLimitExceeded"
    Prelude.. Core.hasStatus 403

-- | The ciphertext references a key that doesn\'t exist or that you don\'t
-- have access to.
_KMSAccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "KMSAccessDenied"
    Prelude.. Core.hasStatus 400

-- | Can\'t tag resource. Verify that the topic exists.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Core.hasStatus 404

-- | The credential signature isn\'t valid. You must use an HTTPS endpoint
-- and sign your request using Signature Version 4.
_InvalidSecurityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityException =
  Core._MatchServiceError
    defaultService
    "InvalidSecurity"
    Prelude.. Core.hasStatus 403

-- | The request was rejected because the specified customer master key (CMK)
-- isn\'t enabled.
_KMSDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSDisabledException =
  Core._MatchServiceError
    defaultService
    "KMSDisabled"
    Prelude.. Core.hasStatus 400
