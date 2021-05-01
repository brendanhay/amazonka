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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "SNS",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "sns",
      Prelude._svcSigningName = "sns",
      Prelude._svcVersion = "2010-03-31",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseXMLError "SNS",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was denied due to request throttling. For more information
-- about throttling, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second Limits>
-- in the /AWS Key Management Service Developer Guide./
_KMSThrottlingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSThrottlingException =
  Prelude._MatchServiceError
    defaultService
    "KMSThrottling"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the requested resource does not exist.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFound"
    Prelude.. Prelude.hasStatus 404

-- | The request was rejected because the state of the specified resource
-- isn\'t valid for this request. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
_KMSInvalidStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSInvalidStateException =
  Prelude._MatchServiceError
    defaultService
    "KMSInvalidState"
    Prelude.. Prelude.hasStatus 400

-- | The request was rejected because the specified entity or resource can\'t
-- be found.
_KMSNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "KMSNotFound"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the rate at which requests have been submitted for this
-- action exceeds the limit for your account.
_ThrottledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ThrottledException =
  Prelude._MatchServiceError
    defaultService
    "Throttled"
    Prelude.. Prelude.hasStatus 429

-- | Can\'t add more than 50 tags to a topic.
_TagLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "TagLimitExceeded"
    Prelude.. Prelude.hasStatus 400

-- | Indicates an internal service error.
_InternalErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalError"
    Prelude.. Prelude.hasStatus 500

-- | Indicates that the customer already owns the maximum allowed number of
-- topics.
_TopicLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TopicLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "TopicLimitExceeded"
    Prelude.. Prelude.hasStatus 403

-- | The AWS access key ID needs a subscription for the service.
_KMSOptInRequired :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSOptInRequired =
  Prelude._MatchServiceError
    defaultService
    "KMSOptInRequired"
    Prelude.. Prelude.hasStatus 403

-- | Can\'t perform multiple operations on a tag simultaneously. Perform the
-- operations sequentially.
_ConcurrentAccessException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentAccessException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentAccess"
    Prelude.. Prelude.hasStatus 400

-- | The request doesn\'t comply with the IAM tag policy. Correct your
-- request and then retry it.
_TagPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagPolicyException =
  Prelude._MatchServiceError
    defaultService
    "TagPolicy"
    Prelude.. Prelude.hasStatus 400

-- | Exception error indicating platform application disabled.
_PlatformApplicationDisabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PlatformApplicationDisabledException =
  Prelude._MatchServiceError
    defaultService
    "PlatformApplicationDisabled"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the customer already owns the maximum allowed number of
-- subscriptions.
_SubscriptionLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubscriptionLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "SubscriptionLimitExceeded"
    Prelude.. Prelude.hasStatus 403

-- | A tag has been added to a resource with the same ARN as a deleted
-- resource. Wait a short while and then retry the operation.
_StaleTagException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_StaleTagException =
  Prelude._MatchServiceError
    defaultService
    "StaleTag"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that a request parameter does not comply with the associated
-- constraints.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameter"
    Prelude.. Prelude.hasStatus 400

-- | Exception error indicating endpoint disabled.
_EndpointDisabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EndpointDisabledException =
  Prelude._MatchServiceError
    defaultService
    "EndpointDisabled"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that a request parameter does not comply with the associated
-- constraints.
_InvalidParameterValueException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterValueException =
  Prelude._MatchServiceError
    defaultService
    "ParameterValueInvalid"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the user has been denied access to the requested
-- resource.
_AuthorizationErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AuthorizationErrorException =
  Prelude._MatchServiceError
    defaultService
    "AuthorizationError"
    Prelude.. Prelude.hasStatus 403

-- | Indicates that the number of filter polices in your AWS account exceeds
-- the limit. To add more filter polices, submit an SNS Limit Increase case
-- in the AWS Support Center.
_FilterPolicyLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FilterPolicyLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "FilterPolicyLimitExceeded"
    Prelude.. Prelude.hasStatus 403

-- | The ciphertext references a key that doesn\'t exist or that you don\'t
-- have access to.
_KMSAccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSAccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "KMSAccessDenied"
    Prelude.. Prelude.hasStatus 400

-- | Can\'t tag resource. Verify that the topic exists.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The credential signature isn\'t valid. You must use an HTTPS endpoint
-- and sign your request using Signature Version 4.
_InvalidSecurityException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSecurityException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSecurity"
    Prelude.. Prelude.hasStatus 403

-- | The request was rejected because the specified customer master key (CMK)
-- isn\'t enabled.
_KMSDisabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSDisabledException =
  Prelude._MatchServiceError
    defaultService
    "KMSDisabled"
    Prelude.. Prelude.hasStatus 400
