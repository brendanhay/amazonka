{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoMedia.Types
    (
    -- * Service Configuration
      kinesisVideoMedia

    -- * Errors
    , _ConnectionLimitExceededException
    , _InvalidArgumentException
    , _NotAuthorizedException
    , _ClientLimitExceededException
    , _InvalidEndpointException
    , _ResourceNotFoundException

    -- * StartSelectorType
    , StartSelectorType (..)

    -- * StartSelector
    , StartSelector
    , startSelector
    , ssContinuationToken
    , ssAfterFragmentNumber
    , ssStartTimestamp
    , ssStartSelectorType
    ) where

import Network.AWS.KinesisVideoMedia.Types.Product
import Network.AWS.KinesisVideoMedia.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Media SDK configuration.
kinesisVideoMedia :: Service
kinesisVideoMedia =
  Service
    { _svcAbbrev = "KinesisVideoMedia"
    , _svcSigner = v4
    , _svcPrefix = "kinesisvideo"
    , _svcVersion = "2017-09-30"
    , _svcEndpoint = defaultEndpoint kinesisVideoMedia
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "KinesisVideoMedia"
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


-- | Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client connections.
--
--
_ConnectionLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ConnectionLimitExceededException =
  _MatchServiceError kinesisVideoMedia "ConnectionLimitExceededException" .
  hasStatus 400


-- | The value for this input parameter is invalid.
--
--
_InvalidArgumentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgumentException =
  _MatchServiceError kinesisVideoMedia "InvalidArgumentException" .
  hasStatus 400


-- | Status Code: 403, The caller is not authorized to perform an operation on the given stream, or the token has expired.
--
--
_NotAuthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_NotAuthorizedException =
  _MatchServiceError kinesisVideoMedia "NotAuthorizedException" . hasStatus 401


-- | Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client calls. Try making the call later.
--
--
_ClientLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ClientLimitExceededException =
  _MatchServiceError kinesisVideoMedia "ClientLimitExceededException" .
  hasStatus 400


-- | Status Code: 400, Caller used wrong endpoint to write data to a stream. On receiving such an exception, the user must call @GetDataEndpoint@ with @AccessMode@ set to "READ" and use the endpoint Kinesis Video returns in the next @GetMedia@ call.
--
--
_InvalidEndpointException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEndpointException =
  _MatchServiceError kinesisVideoMedia "InvalidEndpointException" .
  hasStatus 400


-- | Status Code: 404, The stream with the given name does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError kinesisVideoMedia "ResourceNotFoundException" .
  hasStatus 404

