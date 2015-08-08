{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SNS.Types
    (
    -- * Service Decription
      SNS

    -- * Error Matchers
    , _EndpointDisabledException
    , _AuthorizationErrorException
    , _InvalidParameterException
    , _SubscriptionLimitExceededException
    , _PlatformApplicationDisabledException
    , _InternalErrorException
    , _NotFoundException
    , _InvalidParameterValueException
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

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.SNS.Types.Product
import           Network.AWS.SNS.Types.Sum

-- | Version @2010-03-31@ of the Amazon Simple Notification Service SDK.
data SNS

instance AWSService SNS where
    type Sg SNS = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "SNS"
            , _svcPrefix = "sns"
            , _svcVersion = "2010-03-31"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Exception error indicating endpoint disabled.
_EndpointDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_EndpointDisabledException =
    _ServiceError . hasStatus 400 . hasCode "EndpointDisabled"

-- | Indicates that the user has been denied access to the requested
-- resource.
_AuthorizationErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationErrorException =
    _ServiceError . hasStatus 403 . hasCode "AuthorizationError"

-- | Indicates that a request parameter does not comply with the associated
-- constraints.
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameter"

-- | Indicates that the customer already owns the maximum allowed number of
-- subscriptions.
_SubscriptionLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionLimitExceededException =
    _ServiceError . hasStatus 403 . hasCode "SubscriptionLimitExceeded"

-- | Exception error indicating platform application disabled.
_PlatformApplicationDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_PlatformApplicationDisabledException =
    _ServiceError . hasStatus 400 . hasCode "PlatformApplicationDisabled"

-- | Indicates an internal service error.
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException =
    _ServiceError . hasStatus 500 . hasCode "InternalError"

-- | Indicates that the requested resource does not exist.
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _ServiceError . hasStatus 404 . hasCode "NotFound"

-- | Indicates that a request parameter does not comply with the associated
-- constraints.
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "ParameterValueInvalid"

-- | Indicates that the customer already owns the maximum allowed number of
-- topics.
_TopicLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TopicLimitExceededException =
    _ServiceError . hasStatus 403 . hasCode "TopicLimitExceeded"
