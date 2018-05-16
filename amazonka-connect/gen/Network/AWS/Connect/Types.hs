{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types
    (
    -- * Service Configuration
      connect

    -- * Errors
    , _OutboundContactNotPermittedException
    , _InvalidParameterException
    , _InvalidRequestException
    , _DestinationNotAllowedException
    , _ContactNotFoundException
    , _InternalServiceException
    , _ResourceNotFoundException
    , _LimitExceededException
    ) where

import Network.AWS.Connect.Types.Product
import Network.AWS.Connect.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-08-08@ of the Amazon Connect Service SDK configuration.
connect :: Service
connect =
  Service
    { _svcAbbrev = "Connect"
    , _svcSigner = v4
    , _svcPrefix = "connect"
    , _svcVersion = "2017-08-08"
    , _svcEndpoint = defaultEndpoint connect
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Connect"
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


-- | The contact is not permitted because outbound calling is not enabled for the instance.
--
--
_OutboundContactNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OutboundContactNotPermittedException =
  _MatchServiceError connect "OutboundContactNotPermittedException" .
  hasStatus 403


-- | One or more of the parameters provided to the operation are not valid.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError connect "InvalidParameterException" . hasStatus 400


-- | The request is not valid.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError connect "InvalidRequestException" . hasStatus 400


-- | Outbound calls to the destination number are not allowed for your instance. You can request that the country be included in the allowed countries for your instance by submitting a <https://console.aws.amazon.com/support/v1#/case/create?issueType=service-limit-increase Service Limit Increase> .
--
--
_DestinationNotAllowedException :: AsError a => Getting (First ServiceError) a ServiceError
_DestinationNotAllowedException =
  _MatchServiceError connect "DestinationNotAllowedException" . hasStatus 403


-- | The contact with the specified ID is not active or does not exist.
--
--
_ContactNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ContactNotFoundException =
  _MatchServiceError connect "ContactNotFoundException" . hasStatus 410


-- | Request processing failed due to an error or failure with the service.
--
--
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException =
  _MatchServiceError connect "InternalServiceException" . hasStatus 500


-- | The specified resource was not found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError connect "ResourceNotFoundException" . hasStatus 404


-- | The limit exceeded the maximum allowed active calls in a queue.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError connect "LimitExceededException" . hasStatus 429

