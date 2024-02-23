{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IVS.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ChannelNotBroadcasting,
    _ConflictException,
    _InternalServerException,
    _PendingVerification,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _StreamUnavailable,
    _ThrottlingException,
    _ValidationException,

    -- * ChannelLatencyMode
    ChannelLatencyMode (..),

    -- * ChannelType
    ChannelType (..),

    -- * RecordingConfigurationState
    RecordingConfigurationState (..),

    -- * RecordingMode
    RecordingMode (..),

    -- * StreamHealth
    StreamHealth (..),

    -- * StreamState
    StreamState (..),

    -- * AudioConfiguration
    AudioConfiguration (..),
    newAudioConfiguration,
    audioConfiguration_channels,
    audioConfiguration_codec,
    audioConfiguration_sampleRate,
    audioConfiguration_targetBitrate,

    -- * BatchError
    BatchError (..),
    newBatchError,
    batchError_arn,
    batchError_code,
    batchError_message,

    -- * Channel
    Channel (..),
    newChannel,
    channel_arn,
    channel_authorized,
    channel_ingestEndpoint,
    channel_latencyMode,
    channel_name,
    channel_playbackUrl,
    channel_recordingConfigurationArn,
    channel_tags,
    channel_type,

    -- * ChannelSummary
    ChannelSummary (..),
    newChannelSummary,
    channelSummary_arn,
    channelSummary_authorized,
    channelSummary_latencyMode,
    channelSummary_name,
    channelSummary_recordingConfigurationArn,
    channelSummary_tags,

    -- * DestinationConfiguration
    DestinationConfiguration (..),
    newDestinationConfiguration,
    destinationConfiguration_s3,

    -- * IngestConfiguration
    IngestConfiguration (..),
    newIngestConfiguration,
    ingestConfiguration_audio,
    ingestConfiguration_video,

    -- * PlaybackKeyPair
    PlaybackKeyPair (..),
    newPlaybackKeyPair,
    playbackKeyPair_arn,
    playbackKeyPair_fingerprint,
    playbackKeyPair_name,
    playbackKeyPair_tags,

    -- * PlaybackKeyPairSummary
    PlaybackKeyPairSummary (..),
    newPlaybackKeyPairSummary,
    playbackKeyPairSummary_arn,
    playbackKeyPairSummary_name,
    playbackKeyPairSummary_tags,

    -- * RecordingConfiguration
    RecordingConfiguration (..),
    newRecordingConfiguration,
    recordingConfiguration_name,
    recordingConfiguration_recordingReconnectWindowSeconds,
    recordingConfiguration_tags,
    recordingConfiguration_thumbnailConfiguration,
    recordingConfiguration_arn,
    recordingConfiguration_destinationConfiguration,
    recordingConfiguration_state,

    -- * RecordingConfigurationSummary
    RecordingConfigurationSummary (..),
    newRecordingConfigurationSummary,
    recordingConfigurationSummary_name,
    recordingConfigurationSummary_tags,
    recordingConfigurationSummary_arn,
    recordingConfigurationSummary_destinationConfiguration,
    recordingConfigurationSummary_state,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration (..),
    newS3DestinationConfiguration,
    s3DestinationConfiguration_bucketName,

    -- * Stream
    Stream (..),
    newStream,
    stream_channelArn,
    stream_health,
    stream_playbackUrl,
    stream_startTime,
    stream_state,
    stream_streamId,
    stream_viewerCount,

    -- * StreamEvent
    StreamEvent (..),
    newStreamEvent,
    streamEvent_eventTime,
    streamEvent_name,
    streamEvent_type,

    -- * StreamFilters
    StreamFilters (..),
    newStreamFilters,
    streamFilters_health,

    -- * StreamKey
    StreamKey (..),
    newStreamKey,
    streamKey_arn,
    streamKey_channelArn,
    streamKey_tags,
    streamKey_value,

    -- * StreamKeySummary
    StreamKeySummary (..),
    newStreamKeySummary,
    streamKeySummary_arn,
    streamKeySummary_channelArn,
    streamKeySummary_tags,

    -- * StreamSession
    StreamSession (..),
    newStreamSession,
    streamSession_channel,
    streamSession_endTime,
    streamSession_ingestConfiguration,
    streamSession_recordingConfiguration,
    streamSession_startTime,
    streamSession_streamId,
    streamSession_truncatedEvents,

    -- * StreamSessionSummary
    StreamSessionSummary (..),
    newStreamSessionSummary,
    streamSessionSummary_endTime,
    streamSessionSummary_hasErrorEvent,
    streamSessionSummary_startTime,
    streamSessionSummary_streamId,

    -- * StreamSummary
    StreamSummary (..),
    newStreamSummary,
    streamSummary_channelArn,
    streamSummary_health,
    streamSummary_startTime,
    streamSummary_state,
    streamSummary_streamId,
    streamSummary_viewerCount,

    -- * ThumbnailConfiguration
    ThumbnailConfiguration (..),
    newThumbnailConfiguration,
    thumbnailConfiguration_recordingMode,
    thumbnailConfiguration_targetIntervalSeconds,

    -- * VideoConfiguration
    VideoConfiguration (..),
    newVideoConfiguration,
    videoConfiguration_avcLevel,
    videoConfiguration_avcProfile,
    videoConfiguration_codec,
    videoConfiguration_encoder,
    videoConfiguration_targetBitrate,
    videoConfiguration_targetFramerate,
    videoConfiguration_videoHeight,
    videoConfiguration_videoWidth,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IVS.Types.AudioConfiguration
import Amazonka.IVS.Types.BatchError
import Amazonka.IVS.Types.Channel
import Amazonka.IVS.Types.ChannelLatencyMode
import Amazonka.IVS.Types.ChannelSummary
import Amazonka.IVS.Types.ChannelType
import Amazonka.IVS.Types.DestinationConfiguration
import Amazonka.IVS.Types.IngestConfiguration
import Amazonka.IVS.Types.PlaybackKeyPair
import Amazonka.IVS.Types.PlaybackKeyPairSummary
import Amazonka.IVS.Types.RecordingConfiguration
import Amazonka.IVS.Types.RecordingConfigurationState
import Amazonka.IVS.Types.RecordingConfigurationSummary
import Amazonka.IVS.Types.RecordingMode
import Amazonka.IVS.Types.S3DestinationConfiguration
import Amazonka.IVS.Types.Stream
import Amazonka.IVS.Types.StreamEvent
import Amazonka.IVS.Types.StreamFilters
import Amazonka.IVS.Types.StreamHealth
import Amazonka.IVS.Types.StreamKey
import Amazonka.IVS.Types.StreamKeySummary
import Amazonka.IVS.Types.StreamSession
import Amazonka.IVS.Types.StreamSessionSummary
import Amazonka.IVS.Types.StreamState
import Amazonka.IVS.Types.StreamSummary
import Amazonka.IVS.Types.ThumbnailConfiguration
import Amazonka.IVS.Types.VideoConfiguration
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-07-14@ of the Amazon Interactive Video Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IVS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ivs",
      Core.signingName = "ivs",
      Core.version = "2020-07-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IVS",
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

_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

_ChannelNotBroadcasting :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ChannelNotBroadcasting =
  Core._MatchServiceError
    defaultService
    "ChannelNotBroadcasting"
    Prelude.. Core.hasStatus 404

_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

_PendingVerification :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PendingVerification =
  Core._MatchServiceError
    defaultService
    "PendingVerification"
    Prelude.. Core.hasStatus 403

_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

_StreamUnavailable :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StreamUnavailable =
  Core._MatchServiceError
    defaultService
    "StreamUnavailable"
    Prelude.. Core.hasStatus 503

_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
