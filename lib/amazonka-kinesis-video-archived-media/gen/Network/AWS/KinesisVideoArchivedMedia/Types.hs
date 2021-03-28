-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidMediaFrameException
    , _NoDataRetentionException
    , _InvalidArgumentException
    , _NotAuthorizedException
    , _ClientLimitExceededException
    , _UnsupportedStreamMediaTypeException
    , _InvalidCodecPrivateDataException
    , _MissingCodecPrivateDataException
    , _ResourceNotFoundException

    -- * DASHStreamingSessionURL
    , DASHStreamingSessionURL (..)

    -- * HLSDiscontinuityMode
    , HLSDiscontinuityMode (..)

    -- * DASHDisplayFragmentTimestamp
    , DASHDisplayFragmentTimestamp (..)

    -- * HLSFragmentSelector
    , HLSFragmentSelector (..)
    , mkHLSFragmentSelector
    , hlsfsFragmentSelectorType
    , hlsfsTimestampRange

    -- * FragmentSelector
    , FragmentSelector (..)
    , mkFragmentSelector
    , fsFragmentSelectorType
    , fsTimestampRange

    -- * Fragment
    , Fragment (..)
    , mkFragment
    , fFragmentLengthInMilliseconds
    , fFragmentNumber
    , fFragmentSizeInBytes
    , fProducerTimestamp
    , fServerTimestamp

    -- * HLSDisplayFragmentTimestamp
    , HLSDisplayFragmentTimestamp (..)

    -- * DASHFragmentSelector
    , DASHFragmentSelector (..)
    , mkDASHFragmentSelector
    , dashfsFragmentSelectorType
    , dashfsTimestampRange

    -- * ClipFragmentSelector
    , ClipFragmentSelector (..)
    , mkClipFragmentSelector
    , cfsFragmentSelectorType
    , cfsTimestampRange

    -- * HLSTimestampRange
    , HLSTimestampRange (..)
    , mkHLSTimestampRange
    , hlstrEndTimestamp
    , hlstrStartTimestamp

    -- * FragmentNumberString
    , FragmentNumberString (..)

    -- * HLSStreamingSessionURL
    , HLSStreamingSessionURL (..)

    -- * NextToken
    , NextToken (..)

    -- * ContainerFormat
    , ContainerFormat (..)

    -- * ResourceARN
    , ResourceARN (..)

    -- * DASHTimestampRange
    , DASHTimestampRange (..)
    , mkDASHTimestampRange
    , dashtrEndTimestamp
    , dashtrStartTimestamp

    -- * ClipTimestampRange
    , ClipTimestampRange (..)
    , mkClipTimestampRange
    , ctrStartTimestamp
    , ctrEndTimestamp

    -- * DASHDisplayFragmentNumber
    , DASHDisplayFragmentNumber (..)

    -- * HLSFragmentSelectorType
    , HLSFragmentSelectorType (..)

    -- * DASHPlaybackMode
    , DASHPlaybackMode (..)

    -- * FragmentSelectorType
    , FragmentSelectorType (..)

    -- * DASHFragmentSelectorType
    , DASHFragmentSelectorType (..)

    -- * HLSPlaybackMode
    , HLSPlaybackMode (..)

    -- * StreamName
    , StreamName (..)

    -- * ClipFragmentSelectorType
    , ClipFragmentSelectorType (..)

    -- * ContentType
    , ContentType (..)

    -- * TimestampRange
    , TimestampRange (..)
    , mkTimestampRange
    , trStartTimestamp
    , trEndTimestamp

    -- * FragmentNumber
    , FragmentNumber (..)

    -- * StreamARN
    , StreamARN (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHStreamingSessionURL
  
  
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode
  
  
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentTimestamp
  
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelector
  
  
import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector
  
  
import Network.AWS.KinesisVideoArchivedMedia.Types.Fragment
  
  
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSDisplayFragmentTimestamp
  
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelector
  
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
  
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange
  
import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentNumberString
  
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSStreamingSessionURL
  
import Network.AWS.KinesisVideoArchivedMedia.Types.NextToken
  
import Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat
  
import Network.AWS.KinesisVideoArchivedMedia.Types.ResourceARN
  
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange
  
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
  
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber
  
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelectorType
  
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode
  
  
import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType
  
  
  
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelectorType
  
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
  
import Network.AWS.KinesisVideoArchivedMedia.Types.StreamName
  
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelectorType
  
  
import Network.AWS.KinesisVideoArchivedMedia.Types.ContentType
  
import Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
  
import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentNumber
  
import Network.AWS.KinesisVideoArchivedMedia.Types.StreamARN
  

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Archived Media SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "KinesisVideoArchivedMedia",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "kinesisvideo",
                 Core._svcVersion = "2017-09-30", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "KinesisVideoArchivedMedia",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | One or more frames in the requested clip could not be parsed based on the specified codec.
_InvalidMediaFrameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMediaFrameException
  = Core._MatchServiceError mkServiceConfig
      "InvalidMediaFrameException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidMediaFrameException #-}
{-# DEPRECATED _InvalidMediaFrameException "Use generic-lens or generic-optics instead"  #-}

-- | A streaming session was requested for a stream that does not retain data (that is, has a @DataRetentionInHours@ of 0). 
_NoDataRetentionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoDataRetentionException
  = Core._MatchServiceError mkServiceConfig
      "NoDataRetentionException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _NoDataRetentionException #-}
{-# DEPRECATED _NoDataRetentionException "Use generic-lens or generic-optics instead"  #-}

-- | A specified parameter exceeds its restrictions, is not supported, or can't be used.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException
  = Core._MatchServiceError mkServiceConfig
      "InvalidArgumentException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidArgumentException #-}
{-# DEPRECATED _InvalidArgumentException "Use generic-lens or generic-optics instead"  #-}

-- | Status Code: 403, The caller is not authorized to perform an operation on the given stream, or the token has expired.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException
  = Core._MatchServiceError mkServiceConfig "NotAuthorizedException"
      Core.. Core.hasStatues 401
{-# INLINEABLE _NotAuthorizedException #-}
{-# DEPRECATED _NotAuthorizedException "Use generic-lens or generic-optics instead"  #-}

-- | Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client calls. Try making the call later.
_ClientLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClientLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ClientLimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ClientLimitExceededException #-}
{-# DEPRECATED _ClientLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The type of the media (for example, h.264 or h.265 video or ACC or G.711 audio) could not be determined from the codec IDs of the tracks in the first fragment for a playback session. The codec ID for track 1 should be @V_MPEG/ISO/AVC@ and, optionally, the codec ID for track 2 should be @A_AAC@ .
_UnsupportedStreamMediaTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedStreamMediaTypeException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedStreamMediaTypeException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _UnsupportedStreamMediaTypeException #-}
{-# DEPRECATED _UnsupportedStreamMediaTypeException "Use generic-lens or generic-optics instead"  #-}

-- | The codec private data in at least one of the tracks of the video stream is not valid for this operation.
_InvalidCodecPrivateDataException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCodecPrivateDataException
  = Core._MatchServiceError mkServiceConfig
      "InvalidCodecPrivateDataException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidCodecPrivateDataException #-}
{-# DEPRECATED _InvalidCodecPrivateDataException "Use generic-lens or generic-optics instead"  #-}

-- | No codec private data was found in at least one of tracks of the video stream.
_MissingCodecPrivateDataException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingCodecPrivateDataException
  = Core._MatchServiceError mkServiceConfig
      "MissingCodecPrivateDataException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _MissingCodecPrivateDataException #-}
{-# DEPRECATED _MissingCodecPrivateDataException "Use generic-lens or generic-optics instead"  #-}

-- | @GetMedia@ throws this error when Kinesis Video Streams can't find the stream that you specified.
--
-- @GetHLSStreamingSessionURL@ and @GetDASHStreamingSessionURL@ throw this error if a session with a @PlaybackMode@ of @ON_DEMAND@ or @LIVE_REPLAY@ is requested for a stream that has no fragments within the requested time range, or if a session with a @PlaybackMode@ of @LIVE@ is requested for a stream that has no fragments within the last 30 seconds.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}
