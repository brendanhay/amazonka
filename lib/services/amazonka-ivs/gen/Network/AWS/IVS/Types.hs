{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IVS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IVS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _StreamUnavailable,
    _PendingVerification,
    _ChannelNotBroadcasting,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * ChannelLatencyMode
    ChannelLatencyMode (..),

    -- * ChannelType
    ChannelType (..),

    -- * RecordingConfigurationState
    RecordingConfigurationState (..),

    -- * StreamHealth
    StreamHealth (..),

    -- * StreamState
    StreamState (..),

    -- * BatchError
    BatchError (..),
    newBatchError,
    batchError_arn,
    batchError_code,
    batchError_message,

    -- * Channel
    Channel (..),
    newChannel,
    channel_playbackUrl,
    channel_authorized,
    channel_arn,
    channel_latencyMode,
    channel_name,
    channel_recordingConfigurationArn,
    channel_type,
    channel_tags,
    channel_ingestEndpoint,

    -- * ChannelSummary
    ChannelSummary (..),
    newChannelSummary,
    channelSummary_authorized,
    channelSummary_arn,
    channelSummary_latencyMode,
    channelSummary_name,
    channelSummary_recordingConfigurationArn,
    channelSummary_tags,

    -- * DestinationConfiguration
    DestinationConfiguration (..),
    newDestinationConfiguration,
    destinationConfiguration_s3,

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
    recordingConfiguration_tags,
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
    stream_playbackUrl,
    stream_state,
    stream_startTime,
    stream_channelArn,
    stream_viewerCount,
    stream_health,

    -- * StreamKey
    StreamKey (..),
    newStreamKey,
    streamKey_arn,
    streamKey_value,
    streamKey_channelArn,
    streamKey_tags,

    -- * StreamKeySummary
    StreamKeySummary (..),
    newStreamKeySummary,
    streamKeySummary_arn,
    streamKeySummary_channelArn,
    streamKeySummary_tags,

    -- * StreamSummary
    StreamSummary (..),
    newStreamSummary,
    streamSummary_state,
    streamSummary_startTime,
    streamSummary_channelArn,
    streamSummary_viewerCount,
    streamSummary_health,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IVS.Types.BatchError
import Network.AWS.IVS.Types.Channel
import Network.AWS.IVS.Types.ChannelLatencyMode
import Network.AWS.IVS.Types.ChannelSummary
import Network.AWS.IVS.Types.ChannelType
import Network.AWS.IVS.Types.DestinationConfiguration
import Network.AWS.IVS.Types.PlaybackKeyPair
import Network.AWS.IVS.Types.PlaybackKeyPairSummary
import Network.AWS.IVS.Types.RecordingConfiguration
import Network.AWS.IVS.Types.RecordingConfigurationState
import Network.AWS.IVS.Types.RecordingConfigurationSummary
import Network.AWS.IVS.Types.S3DestinationConfiguration
import Network.AWS.IVS.Types.Stream
import Network.AWS.IVS.Types.StreamHealth
import Network.AWS.IVS.Types.StreamKey
import Network.AWS.IVS.Types.StreamKeySummary
import Network.AWS.IVS.Types.StreamState
import Network.AWS.IVS.Types.StreamSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-07-14@ of the Amazon Interactive Video Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "IVS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "ivs",
      Core._serviceSigningName = "ivs",
      Core._serviceVersion = "2020-07-14",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "IVS",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- |
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- |
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- |
_StreamUnavailable :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StreamUnavailable =
  Core._MatchServiceError
    defaultService
    "StreamUnavailable"
    Prelude.. Core.hasStatus 503

-- |
_PendingVerification :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PendingVerification =
  Core._MatchServiceError
    defaultService
    "PendingVerification"
    Prelude.. Core.hasStatus 403

-- |
_ChannelNotBroadcasting :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ChannelNotBroadcasting =
  Core._MatchServiceError
    defaultService
    "ChannelNotBroadcasting"
    Prelude.. Core.hasStatus 404

-- |
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- |
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- |
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- |
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- |
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
