{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SNS.Types
    (
    -- * Service Configuration
      sns

    -- * Errors
    , _EndpointDisabledException
    , _AuthorizationErrorException
    , _InvalidParameterException
    , _SubscriptionLimitExceededException
    , _PlatformApplicationDisabledException
    , _InternalErrorException
    , _ThrottledException
    , _InvalidParameterValueException
    , _NotFoundException
    , _TopicLimitExceededException

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAttributes
    , eEndpointARN

    -- * MessageAttributeValue
    , MessageAttributeValue
    , messageAttributeValue
    , mavBinaryValue
    , mavStringValue
    , mavDataType

    -- * PlatformApplication
    , PlatformApplication
    , platformApplication
    , paPlatformApplicationARN
    , paAttributes

    -- * Subscription
    , Subscription
    , subscription
    , sProtocol
    , sOwner
    , sTopicARN
    , sEndpoint
    , sSubscriptionARN

    -- * Topic
    , Topic
    , topic
    , tTopicARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.SNS.Types.Product
import Network.AWS.SNS.Types.Sum

-- | API version @2010-03-31@ of the Amazon Simple Notification Service SDK configuration.
sns :: Service
sns =
  Service
    { _svcAbbrev = "SNS"
    , _svcSigner = v4
    , _svcPrefix = "sns"
    , _svcVersion = "2010-03-31"
    , _svcEndpoint = defaultEndpoint sns
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "SNS"
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


-- | Exception error indicating endpoint disabled.
--
--
_EndpointDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_EndpointDisabledException =
  _MatchServiceError sns "EndpointDisabled" . hasStatus 400


-- | Indicates that the user has been denied access to the requested resource.
--
--
_AuthorizationErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationErrorException =
  _MatchServiceError sns "AuthorizationError" . hasStatus 403


-- | Indicates that a request parameter does not comply with the associated constraints.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError sns "InvalidParameter" . hasStatus 400


-- | Indicates that the customer already owns the maximum allowed number of subscriptions.
--
--
_SubscriptionLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionLimitExceededException =
  _MatchServiceError sns "SubscriptionLimitExceeded" . hasStatus 403


-- | Exception error indicating platform application disabled.
--
--
_PlatformApplicationDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_PlatformApplicationDisabledException =
  _MatchServiceError sns "PlatformApplicationDisabled" . hasStatus 400


-- | Indicates an internal service error.
--
--
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException = _MatchServiceError sns "InternalError" . hasStatus 500


-- | Indicates that the rate at which requests have been submitted for this action exceeds the limit for your account.
--
--
_ThrottledException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottledException = _MatchServiceError sns "Throttled" . hasStatus 429


-- | Indicates that a request parameter does not comply with the associated constraints.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
  _MatchServiceError sns "ParameterValueInvalid" . hasStatus 400


-- | Indicates that the requested resource does not exist.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _MatchServiceError sns "NotFound" . hasStatus 404


-- | Indicates that the customer already owns the maximum allowed number of topics.
--
--
_TopicLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TopicLimitExceededException =
  _MatchServiceError sns "TopicLimitExceeded" . hasStatus 403

