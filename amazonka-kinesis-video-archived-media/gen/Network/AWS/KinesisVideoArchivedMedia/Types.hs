{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidMediaFrameException,
    _ClientLimitExceededException,
    _InvalidCodecPrivateDataException,
    _NoDataRetentionException,
    _UnsupportedStreamMediaTypeException,
    _ResourceNotFoundException,
    _MissingCodecPrivateDataException,
    _NotAuthorizedException,
    _InvalidArgumentException,

    -- * ClipFragmentSelectorType
    ClipFragmentSelectorType (..),

    -- * ContainerFormat
    ContainerFormat (..),

    -- * DASHDisplayFragmentNumber
    DASHDisplayFragmentNumber (..),

    -- * DASHDisplayFragmentTimestamp
    DASHDisplayFragmentTimestamp (..),

    -- * DASHFragmentSelectorType
    DASHFragmentSelectorType (..),

    -- * DASHPlaybackMode
    DASHPlaybackMode (..),

    -- * FragmentSelectorType
    FragmentSelectorType (..),

    -- * HLSDiscontinuityMode
    HLSDiscontinuityMode (..),

    -- * HLSDisplayFragmentTimestamp
    HLSDisplayFragmentTimestamp (..),

    -- * HLSFragmentSelectorType
    HLSFragmentSelectorType (..),

    -- * HLSPlaybackMode
    HLSPlaybackMode (..),

    -- * ClipFragmentSelector
    ClipFragmentSelector (..),
    newClipFragmentSelector,
    clipFragmentSelector_fragmentSelectorType,
    clipFragmentSelector_timestampRange,

    -- * ClipTimestampRange
    ClipTimestampRange (..),
    newClipTimestampRange,
    clipTimestampRange_startTimestamp,
    clipTimestampRange_endTimestamp,

    -- * DASHFragmentSelector
    DASHFragmentSelector (..),
    newDASHFragmentSelector,
    dASHFragmentSelector_fragmentSelectorType,
    dASHFragmentSelector_timestampRange,

    -- * DASHTimestampRange
    DASHTimestampRange (..),
    newDASHTimestampRange,
    dASHTimestampRange_endTimestamp,
    dASHTimestampRange_startTimestamp,

    -- * Fragment
    Fragment (..),
    newFragment,
    fragment_producerTimestamp,
    fragment_fragmentNumber,
    fragment_fragmentSizeInBytes,
    fragment_serverTimestamp,
    fragment_fragmentLengthInMilliseconds,

    -- * FragmentSelector
    FragmentSelector (..),
    newFragmentSelector,
    fragmentSelector_fragmentSelectorType,
    fragmentSelector_timestampRange,

    -- * HLSFragmentSelector
    HLSFragmentSelector (..),
    newHLSFragmentSelector,
    hLSFragmentSelector_fragmentSelectorType,
    hLSFragmentSelector_timestampRange,

    -- * HLSTimestampRange
    HLSTimestampRange (..),
    newHLSTimestampRange,
    hLSTimestampRange_endTimestamp,
    hLSTimestampRange_startTimestamp,

    -- * TimestampRange
    TimestampRange (..),
    newTimestampRange,
    timestampRange_startTimestamp,
    timestampRange_endTimestamp,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
import Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentTimestamp
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange
import Network.AWS.KinesisVideoArchivedMedia.Types.Fragment
import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSDisplayFragmentTimestamp
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange
import Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Archived Media SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "KinesisVideoArchivedMedia",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "kinesisvideo",
      Prelude._svcVersion = "2017-09-30",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "KinesisVideoArchivedMedia",
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

-- | One or more frames in the requested clip could not be parsed based on
-- the specified codec.
_InvalidMediaFrameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidMediaFrameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidMediaFrameException"
    Prelude.. Prelude.hasStatus 400

-- | Kinesis Video Streams has throttled the request because you have
-- exceeded the limit of allowed client calls. Try making the call later.
_ClientLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClientLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ClientLimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | The codec private data in at least one of the tracks of the video stream
-- is not valid for this operation.
_InvalidCodecPrivateDataException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCodecPrivateDataException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCodecPrivateDataException"
    Prelude.. Prelude.hasStatus 400

-- | A streaming session was requested for a stream that does not retain data
-- (that is, has a @DataRetentionInHours@ of 0).
_NoDataRetentionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoDataRetentionException =
  Prelude._MatchServiceError
    defaultService
    "NoDataRetentionException"
    Prelude.. Prelude.hasStatus 400

-- | The type of the media (for example, h.264 or h.265 video or ACC or G.711
-- audio) could not be determined from the codec IDs of the tracks in the
-- first fragment for a playback session. The codec ID for track 1 should
-- be @V_MPEG\/ISO\/AVC@ and, optionally, the codec ID for track 2 should
-- be @A_AAC@.
_UnsupportedStreamMediaTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedStreamMediaTypeException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedStreamMediaTypeException"
    Prelude.. Prelude.hasStatus 400

-- | @GetMedia@ throws this error when Kinesis Video Streams can\'t find the
-- stream that you specified.
--
-- @GetHLSStreamingSessionURL@ and @GetDASHStreamingSessionURL@ throw this
-- error if a session with a @PlaybackMode@ of @ON_DEMAND@ or
-- @LIVE_REPLAY@is requested for a stream that has no fragments within the
-- requested time range, or if a session with a @PlaybackMode@ of @LIVE@ is
-- requested for a stream that has no fragments within the last 30 seconds.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | No codec private data was found in at least one of tracks of the video
-- stream.
_MissingCodecPrivateDataException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingCodecPrivateDataException =
  Prelude._MatchServiceError
    defaultService
    "MissingCodecPrivateDataException"
    Prelude.. Prelude.hasStatus 400

-- | Status Code: 403, The caller is not authorized to perform an operation
-- on the given stream, or the token has expired.
_NotAuthorizedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotAuthorizedException =
  Prelude._MatchServiceError
    defaultService
    "NotAuthorizedException"
    Prelude.. Prelude.hasStatus 401

-- | A specified parameter exceeds its restrictions, is not supported, or
-- can\'t be used.
_InvalidArgumentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidArgumentException =
  Prelude._MatchServiceError
    defaultService
    "InvalidArgumentException"
    Prelude.. Prelude.hasStatus 400
