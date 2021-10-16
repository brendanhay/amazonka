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
    fragment_serverTimestamp,
    fragment_fragmentNumber,
    fragment_fragmentSizeInBytes,
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

import qualified Network.AWS.Core as Core
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
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "KinesisVideoArchivedMedia",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "kinesisvideo",
      Core._serviceSigningName = "kinesisvideo",
      Core._serviceVersion = "2017-09-30",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "KinesisVideoArchivedMedia",
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

-- | One or more frames in the requested clip could not be parsed based on
-- the specified codec.
_InvalidMediaFrameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMediaFrameException =
  Core._MatchServiceError
    defaultService
    "InvalidMediaFrameException"
    Prelude.. Core.hasStatus 400

-- | Kinesis Video Streams has throttled the request because you have
-- exceeded a limit. Try making the call later. For information about
-- limits, see
-- <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/limits.html Kinesis Video Streams Limits>.
_ClientLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClientLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ClientLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The codec private data in at least one of the tracks of the video stream
-- is not valid for this operation.
_InvalidCodecPrivateDataException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCodecPrivateDataException =
  Core._MatchServiceError
    defaultService
    "InvalidCodecPrivateDataException"
    Prelude.. Core.hasStatus 400

-- | A streaming session was requested for a stream that does not retain data
-- (that is, has a @DataRetentionInHours@ of 0).
_NoDataRetentionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoDataRetentionException =
  Core._MatchServiceError
    defaultService
    "NoDataRetentionException"
    Prelude.. Core.hasStatus 400

-- | The type of the media (for example, h.264 or h.265 video or ACC or G.711
-- audio) could not be determined from the codec IDs of the tracks in the
-- first fragment for a playback session. The codec ID for track 1 should
-- be @V_MPEG\/ISO\/AVC@ and, optionally, the codec ID for track 2 should
-- be @A_AAC@.
_UnsupportedStreamMediaTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedStreamMediaTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedStreamMediaTypeException"
    Prelude.. Core.hasStatus 400

-- | @GetMedia@ throws this error when Kinesis Video Streams can\'t find the
-- stream that you specified.
--
-- @GetHLSStreamingSessionURL@ and @GetDASHStreamingSessionURL@ throw this
-- error if a session with a @PlaybackMode@ of @ON_DEMAND@ or
-- @LIVE_REPLAY@is requested for a stream that has no fragments within the
-- requested time range, or if a session with a @PlaybackMode@ of @LIVE@ is
-- requested for a stream that has no fragments within the last 30 seconds.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | No codec private data was found in at least one of tracks of the video
-- stream.
_MissingCodecPrivateDataException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingCodecPrivateDataException =
  Core._MatchServiceError
    defaultService
    "MissingCodecPrivateDataException"
    Prelude.. Core.hasStatus 400

-- | Status Code: 403, The caller is not authorized to perform an operation
-- on the given stream, or the token has expired.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"
    Prelude.. Core.hasStatus 401

-- | A specified parameter exceeds its restrictions, is not supported, or
-- can\'t be used.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"
    Prelude.. Core.hasStatus 400
