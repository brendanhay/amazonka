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
    , _NoDataRetentionException
    , _InvalidArgumentException
    , _NotAuthorizedException
    , _ClientLimitExceededException
    , _UnsupportedStreamMediaTypeException
    , _InvalidCodecPrivateDataException
    , _MissingCodecPrivateDataException
    , _ResourceNotFoundException

    -- * ContainerFormat
    , ContainerFormat (..)

    -- * DiscontinuityMode
    , DiscontinuityMode (..)

    -- * DisplayFragmentTimestamp
    , DisplayFragmentTimestamp (..)

    -- * FragmentSelectorType
    , FragmentSelectorType (..)

    -- * HLSFragmentSelectorType
    , HLSFragmentSelectorType (..)

    -- * PlaybackMode
    , PlaybackMode (..)

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

    -- * HLSFragmentSelector
    , HLSFragmentSelector
    , hLSFragmentSelector
    , hlsfsFragmentSelectorType
    , hlsfsTimestampRange

    -- * HLSTimestampRange
    , HLSTimestampRange
    , hLSTimestampRange
    , hlstrEndTimestamp
    , hlstrStartTimestamp

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


-- | A @PlaybackMode@ of @ON_DEMAND@ was requested for a stream that does not retain data (that is, has a @DataRetentionInHours@ of 0).
--
--
_NoDataRetentionException :: AsError a => Getting (First ServiceError) a ServiceError
_NoDataRetentionException =
  _MatchServiceError kinesisVideoArchivedMedia "NoDataRetentionException" .
  hasStatus 400


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


-- | The type of the media (for example, h.264 video or ACC audio) could not be determined from the codec IDs of the tracks in the first fragment for a playback session. The codec ID for track 1 should be @V_MPEG/ISO/AVC@ and, optionally, the codec ID for track 2 should be @A_AAC@ .
--
--
_UnsupportedStreamMediaTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedStreamMediaTypeException =
  _MatchServiceError
    kinesisVideoArchivedMedia
    "UnsupportedStreamMediaTypeException" .
  hasStatus 400


-- | The codec private data in at least one of the tracks of the video stream is not valid for this operation.
--
--
_InvalidCodecPrivateDataException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCodecPrivateDataException =
  _MatchServiceError
    kinesisVideoArchivedMedia
    "InvalidCodecPrivateDataException" .
  hasStatus 400


-- | No codec private data was found in at least one of tracks of the video stream.
--
--
_MissingCodecPrivateDataException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingCodecPrivateDataException =
  _MatchServiceError
    kinesisVideoArchivedMedia
    "MissingCodecPrivateDataException" .
  hasStatus 400


-- | @GetMedia@ throws this error when Kinesis Video Streams can't find the stream that you specified.
--
--
-- @GetHLSStreamingSessionURL@ throws this error if a session with a @PlaybackMode@ of @ON_DEMAND@ is requested for a stream that has no fragments within the requested time range, or if a session with a @PlaybackMode@ of @LIVE@ is requested for a stream that has no fragments within the last 30 seconds.
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError kinesisVideoArchivedMedia "ResourceNotFoundException" .
  hasStatus 404

