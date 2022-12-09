{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ClientLimitExceededException,
    _InvalidArgumentException,
    _InvalidCodecPrivateDataException,
    _InvalidMediaFrameException,
    _MissingCodecPrivateDataException,
    _NoDataRetentionException,
    _NotAuthorizedException,
    _ResourceNotFoundException,
    _UnsupportedStreamMediaTypeException,

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

    -- * Format
    Format (..),

    -- * FormatConfigKey
    FormatConfigKey (..),

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

    -- * ImageError
    ImageError (..),

    -- * ImageSelectorType
    ImageSelectorType (..),

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
    fragment_fragmentLengthInMilliseconds,
    fragment_fragmentNumber,
    fragment_fragmentSizeInBytes,
    fragment_producerTimestamp,
    fragment_serverTimestamp,

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

    -- * Image
    Image (..),
    newImage,
    image_error,
    image_imageContent,
    image_timeStamp,

    -- * TimestampRange
    TimestampRange (..),
    newTimestampRange,
    timestampRange_startTimestamp,
    timestampRange_endTimestamp,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
import Amazonka.KinesisVideoArchivedMedia.Types.ClipFragmentSelectorType
import Amazonka.KinesisVideoArchivedMedia.Types.ClipTimestampRange
import Amazonka.KinesisVideoArchivedMedia.Types.ContainerFormat
import Amazonka.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber
import Amazonka.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentTimestamp
import Amazonka.KinesisVideoArchivedMedia.Types.DASHFragmentSelector
import Amazonka.KinesisVideoArchivedMedia.Types.DASHFragmentSelectorType
import Amazonka.KinesisVideoArchivedMedia.Types.DASHPlaybackMode
import Amazonka.KinesisVideoArchivedMedia.Types.DASHTimestampRange
import Amazonka.KinesisVideoArchivedMedia.Types.Format
import Amazonka.KinesisVideoArchivedMedia.Types.FormatConfigKey
import Amazonka.KinesisVideoArchivedMedia.Types.Fragment
import Amazonka.KinesisVideoArchivedMedia.Types.FragmentSelector
import Amazonka.KinesisVideoArchivedMedia.Types.FragmentSelectorType
import Amazonka.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode
import Amazonka.KinesisVideoArchivedMedia.Types.HLSDisplayFragmentTimestamp
import Amazonka.KinesisVideoArchivedMedia.Types.HLSFragmentSelector
import Amazonka.KinesisVideoArchivedMedia.Types.HLSFragmentSelectorType
import Amazonka.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
import Amazonka.KinesisVideoArchivedMedia.Types.HLSTimestampRange
import Amazonka.KinesisVideoArchivedMedia.Types.Image
import Amazonka.KinesisVideoArchivedMedia.Types.ImageError
import Amazonka.KinesisVideoArchivedMedia.Types.ImageSelectorType
import Amazonka.KinesisVideoArchivedMedia.Types.TimestampRange
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Archived Media SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "KinesisVideoArchivedMedia",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "kinesisvideo",
      Core.signingName = "kinesisvideo",
      Core.version = "2017-09-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "KinesisVideoArchivedMedia",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

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

-- | A specified parameter exceeds its restrictions, is not supported, or
-- can\'t be used.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"
    Prelude.. Core.hasStatus 400

-- | The codec private data in at least one of the tracks of the video stream
-- is not valid for this operation.
_InvalidCodecPrivateDataException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCodecPrivateDataException =
  Core._MatchServiceError
    defaultService
    "InvalidCodecPrivateDataException"
    Prelude.. Core.hasStatus 400

-- | One or more frames in the requested clip could not be parsed based on
-- the specified codec.
_InvalidMediaFrameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMediaFrameException =
  Core._MatchServiceError
    defaultService
    "InvalidMediaFrameException"
    Prelude.. Core.hasStatus 400

-- | No codec private data was found in at least one of tracks of the video
-- stream.
_MissingCodecPrivateDataException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingCodecPrivateDataException =
  Core._MatchServiceError
    defaultService
    "MissingCodecPrivateDataException"
    Prelude.. Core.hasStatus 400

-- | A streaming session was requested for a stream that does not retain data
-- (that is, has a @DataRetentionInHours@ of 0).
_NoDataRetentionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoDataRetentionException =
  Core._MatchServiceError
    defaultService
    "NoDataRetentionException"
    Prelude.. Core.hasStatus 400

-- | Status Code: 403, The caller is not authorized to perform an operation
-- on the given stream, or the token has expired.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"
    Prelude.. Core.hasStatus 401

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
