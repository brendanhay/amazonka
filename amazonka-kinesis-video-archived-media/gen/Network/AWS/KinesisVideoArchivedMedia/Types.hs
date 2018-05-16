{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types
    (
    -- * Service Configuration
      kinesisVideoArchivedMedia

    -- * Errors
    , _InvalidArgumentException
    , _NotAuthorizedException
    , _ClientLimitExceededException
    , _ResourceNotFoundException

    -- * FragmentSelectorType
    , FragmentSelectorType (..)

    -- * Fragment
    , Fragment
    , fragment
    , fFragmentLengthInMilliseconds
    , fServerTimestamp
    , fFragmentSizeInBytes
    , fFragmentNumber
    , fProducerTimestamp

    -- * FragmentSelector
    , FragmentSelector
    , fragmentSelector
    , fsFragmentSelectorType
    , fsTimestampRange

    -- * TimestampRange
    , TimestampRange
    , timestampRange
    , trStartTimestamp
    , trEndTimestamp
    ) where

import Network.AWS.KinesisVideoArchivedMedia.Types.Product
import Network.AWS.KinesisVideoArchivedMedia.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Archived Media SDK configuration.
kinesisVideoArchivedMedia :: Service
kinesisVideoArchivedMedia =
  Service
    { _svcAbbrev = "KinesisVideoArchivedMedia"
    , _svcSigner = v4
    , _svcPrefix = "kinesisvideo"
    , _svcVersion = "2017-09-30"
    , _svcEndpoint = defaultEndpoint kinesisVideoArchivedMedia
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "KinesisVideoArchivedMedia"
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


-- | A specified parameter exceeds its restrictions, is not supported, or can't be used.
--
--
_InvalidArgumentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgumentException =
  _MatchServiceError kinesisVideoArchivedMedia "InvalidArgumentException" .
  hasStatus 400


-- | Status Code: 403, The caller is not authorized to perform an operation on the given stream, or the token has expired.
--
--
_NotAuthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_NotAuthorizedException =
  _MatchServiceError kinesisVideoArchivedMedia "NotAuthorizedException" .
  hasStatus 401


-- | Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client calls. Try making the call later.
--
--
_ClientLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ClientLimitExceededException =
  _MatchServiceError kinesisVideoArchivedMedia "ClientLimitExceededException" .
  hasStatus 400


-- | Kinesis Video Streams can't find the stream that you specified.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError kinesisVideoArchivedMedia "ResourceNotFoundException" .
  hasStatus 404

