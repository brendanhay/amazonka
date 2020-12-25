-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _KMSInvalidStateException,
    _EndpointDisabledException,
    _AuthorizationErrorException,
    _KMSThrottlingException,
    _InvalidParameterException,
    _SubscriptionLimitExceededException,
    _PlatformApplicationDisabledException,
    _KMSOptInRequired,
    _InternalErrorException,
    _ThrottledException,
    _KMSNotFoundException,
    _InvalidParameterValueException,
    _NotFoundException,
    _StaleTagException,
    _KMSDisabledException,
    _TagPolicyException,
    _InvalidSecurityException,
    _TopicLimitExceededException,
    _ConcurrentAccessException,
    _TagLimitExceededException,
    _ResourceNotFoundException,
    _FilterPolicyLimitExceededException,
    _KMSAccessDeniedException,

    -- * Subject
    Subject (..),

    -- * Delegate
    Delegate (..),

    -- * AttributeValue
    AttributeValue (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * String
    String (..),

    -- * String
    String (..),

    -- * Token
    Token (..),

    -- * TopicName
    TopicName (..),

    -- * Action
    Action (..),

    -- * Protocol
    Protocol (..),

    -- * Account
    Account (..),

    -- * Topic
    Topic (..),
    mkTopic,
    tTopicArn,

    -- * MessageAttributeValue
    MessageAttributeValue (..),
    mkMessageAttributeValue,
    mavDataType,
    mavBinaryValue,
    mavStringValue,

    -- * TopicARN
    TopicARN (..),

    -- * NextToken
    NextToken (..),

    -- * PhoneNumber
    PhoneNumber (..),

    -- * PlatformApplication
    PlatformApplication (..),
    mkPlatformApplication,
    paAttributes,
    paPlatformApplicationArn,

    -- * TagKey
    TagKey (..),

    -- * Endpoint
    Endpoint (..),

    -- * Subscription
    Subscription (..),
    mkSubscription,
    sEndpoint,
    sOwner,
    sProtocol,
    sSubscriptionArn,
    sTopicArn,

    -- * AttributeName
    AttributeName (..),

    -- * AmazonResourceName
    AmazonResourceName (..),

    -- * Endpoint
    Endpoint (..),
    mkEndpoint,
    eAttributes,
    eEndpointArn,

    -- * NextToken
    NextToken (..),

    -- * Message
    Message (..),

    -- * SubscriptionArn
    SubscriptionArn (..),

    -- * TopicArn
    TopicArn (..),

    -- * Label
    Label (..),

    -- * EndpointArn
    EndpointArn (..),

    -- * Name
    Name (..),

    -- * Platform
    Platform (..),

    -- * PlatformApplicationArn
    PlatformApplicationArn (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * AttributeName
    AttributeName (..),

    -- * ResourceArn
    ResourceArn (..),

    -- * AuthenticateOnUnsubscribe
    AuthenticateOnUnsubscribe (..),

    -- * MessageStructure
    MessageStructure (..),

    -- * MessageId
    MessageId (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.SNS.Types.Account
import Network.AWS.SNS.Types.Action
import Network.AWS.SNS.Types.AmazonResourceName
import Network.AWS.SNS.Types.AttributeName
import Network.AWS.SNS.Types.AttributeValue
import Network.AWS.SNS.Types.AuthenticateOnUnsubscribe
import Network.AWS.SNS.Types.Delegate
import Network.AWS.SNS.Types.Endpoint
import Network.AWS.SNS.Types.EndpointArn
import Network.AWS.SNS.Types.Key
import Network.AWS.SNS.Types.Label
import Network.AWS.SNS.Types.Message
import Network.AWS.SNS.Types.MessageAttributeValue
import Network.AWS.SNS.Types.MessageId
import Network.AWS.SNS.Types.MessageStructure
import Network.AWS.SNS.Types.Name
import Network.AWS.SNS.Types.NextToken
import Network.AWS.SNS.Types.PhoneNumber
import Network.AWS.SNS.Types.Platform
import Network.AWS.SNS.Types.PlatformApplication
import Network.AWS.SNS.Types.PlatformApplicationArn
import Network.AWS.SNS.Types.Protocol
import Network.AWS.SNS.Types.ResourceArn
import Network.AWS.SNS.Types.String
import Network.AWS.SNS.Types.Subject
import Network.AWS.SNS.Types.Subscription
import Network.AWS.SNS.Types.SubscriptionArn
import Network.AWS.SNS.Types.Tag
import Network.AWS.SNS.Types.TagKey
import Network.AWS.SNS.Types.Token
import Network.AWS.SNS.Types.Topic
import Network.AWS.SNS.Types.TopicARN
import Network.AWS.SNS.Types.TopicArn
import Network.AWS.SNS.Types.TopicName
import Network.AWS.SNS.Types.Value
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-03-31@ of the Amazon Simple Notification Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "SNS",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "sns",
      Core._svcVersion = "2010-03-31",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseXMLError "SNS",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request was rejected because the state of the specified resource isn't valid for this request. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
_KMSInvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError mkServiceConfig "KMSInvalidState"
    Core.. Core.hasStatues 400
{-# DEPRECATED _KMSInvalidStateException "Use generic-lens or generic-optics instead." #-}

-- | Exception error indicating endpoint disabled.
_EndpointDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EndpointDisabledException =
  Core._MatchServiceError mkServiceConfig "EndpointDisabled"
    Core.. Core.hasStatues 400
{-# DEPRECATED _EndpointDisabledException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that the user has been denied access to the requested resource.
_AuthorizationErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationErrorException =
  Core._MatchServiceError mkServiceConfig "AuthorizationError"
    Core.. Core.hasStatues 403
{-# DEPRECATED _AuthorizationErrorException "Use generic-lens or generic-optics instead." #-}

-- | The request was denied due to request throttling. For more information about throttling, see <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second Limits> in the /AWS Key Management Service Developer Guide./
_KMSThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSThrottlingException =
  Core._MatchServiceError mkServiceConfig "KMSThrottling"
    Core.. Core.hasStatues 400
{-# DEPRECATED _KMSThrottlingException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that a request parameter does not comply with the associated constraints.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError mkServiceConfig "InvalidParameter"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that the customer already owns the maximum allowed number of subscriptions.
_SubscriptionLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "SubscriptionLimitExceeded"
    Core.. Core.hasStatues 403
{-# DEPRECATED _SubscriptionLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | Exception error indicating platform application disabled.
_PlatformApplicationDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PlatformApplicationDisabledException =
  Core._MatchServiceError
    mkServiceConfig
    "PlatformApplicationDisabled"
    Core.. Core.hasStatues 400
{-# DEPRECATED _PlatformApplicationDisabledException "Use generic-lens or generic-optics instead." #-}

-- | The AWS access key ID needs a subscription for the service.
_KMSOptInRequired :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSOptInRequired =
  Core._MatchServiceError mkServiceConfig "KMSOptInRequired"
    Core.. Core.hasStatues 403
{-# DEPRECATED _KMSOptInRequired "Use generic-lens or generic-optics instead." #-}

-- | Indicates an internal service error.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError mkServiceConfig "InternalError"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalErrorException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that the rate at which requests have been submitted for this action exceeds the limit for your account.
_ThrottledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottledException =
  Core._MatchServiceError mkServiceConfig "Throttled"
    Core.. Core.hasStatues 429
{-# DEPRECATED _ThrottledException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified entity or resource can't be found.
_KMSNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSNotFoundException =
  Core._MatchServiceError mkServiceConfig "KMSNotFound"
    Core.. Core.hasStatues 400
{-# DEPRECATED _KMSNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that a request parameter does not comply with the associated constraints.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError mkServiceConfig "ParameterValueInvalid"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidParameterValueException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that the requested resource does not exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | A tag has been added to a resource with the same ARN as a deleted resource. Wait a short while and then retry the operation.
_StaleTagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StaleTagException =
  Core._MatchServiceError mkServiceConfig "StaleTag"
    Core.. Core.hasStatues 400
{-# DEPRECATED _StaleTagException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified customer master key (CMK) isn't enabled.
_KMSDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSDisabledException =
  Core._MatchServiceError mkServiceConfig "KMSDisabled"
    Core.. Core.hasStatues 400
{-# DEPRECATED _KMSDisabledException "Use generic-lens or generic-optics instead." #-}

-- | The request doesn't comply with the IAM tag policy. Correct your request and then retry it.
_TagPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagPolicyException =
  Core._MatchServiceError mkServiceConfig "TagPolicy"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TagPolicyException "Use generic-lens or generic-optics instead." #-}

-- | The credential signature isn't valid. You must use an HTTPS endpoint and sign your request using Signature Version 4.
_InvalidSecurityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityException =
  Core._MatchServiceError mkServiceConfig "InvalidSecurity"
    Core.. Core.hasStatues 403
{-# DEPRECATED _InvalidSecurityException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that the customer already owns the maximum allowed number of topics.
_TopicLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TopicLimitExceededException =
  Core._MatchServiceError mkServiceConfig "TopicLimitExceeded"
    Core.. Core.hasStatues 403
{-# DEPRECATED _TopicLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | Can't perform multiple operations on a tag simultaneously. Perform the operations sequentially.
_ConcurrentAccessException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentAccessException =
  Core._MatchServiceError mkServiceConfig "ConcurrentAccess"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ConcurrentAccessException "Use generic-lens or generic-optics instead." #-}

-- | Can't add more than 50 tags to a topic.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError mkServiceConfig "TagLimitExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TagLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | Can't tag resource. Verify that the topic exists.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError mkServiceConfig "ResourceNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that the number of filter polices in your AWS account exceeds the limit. To add more filter polices, submit an SNS Limit Increase case in the AWS Support Center.
_FilterPolicyLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FilterPolicyLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "FilterPolicyLimitExceeded"
    Core.. Core.hasStatues 403
{-# DEPRECATED _FilterPolicyLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The ciphertext references a key that doesn't exist or that you don't have access to.
_KMSAccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedException =
  Core._MatchServiceError mkServiceConfig "KMSAccessDenied"
    Core.. Core.hasStatues 400
{-# DEPRECATED _KMSAccessDeniedException "Use generic-lens or generic-optics instead." #-}
