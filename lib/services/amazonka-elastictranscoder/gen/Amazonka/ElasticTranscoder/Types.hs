{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticTranscoder.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _IncompatibleVersionException,
    _InternalServiceException,
    _LimitExceededException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ValidationException,

    -- * Artwork
    Artwork (..),
    newArtwork,
    artwork_albumArtFormat,
    artwork_encryption,
    artwork_inputKey,
    artwork_maxHeight,
    artwork_maxWidth,
    artwork_paddingPolicy,
    artwork_sizingPolicy,

    -- * AudioCodecOptions
    AudioCodecOptions (..),
    newAudioCodecOptions,
    audioCodecOptions_bitDepth,
    audioCodecOptions_bitOrder,
    audioCodecOptions_profile,
    audioCodecOptions_signed,

    -- * AudioParameters
    AudioParameters (..),
    newAudioParameters,
    audioParameters_audioPackingMode,
    audioParameters_bitRate,
    audioParameters_channels,
    audioParameters_codec,
    audioParameters_codecOptions,
    audioParameters_sampleRate,

    -- * CaptionFormat
    CaptionFormat (..),
    newCaptionFormat,
    captionFormat_encryption,
    captionFormat_format,
    captionFormat_pattern,

    -- * CaptionSource
    CaptionSource (..),
    newCaptionSource,
    captionSource_encryption,
    captionSource_key,
    captionSource_label,
    captionSource_language,
    captionSource_timeOffset,

    -- * Captions
    Captions (..),
    newCaptions,
    captions_captionFormats,
    captions_captionSources,
    captions_mergePolicy,

    -- * Clip
    Clip (..),
    newClip,
    clip_timeSpan,

    -- * CreateJobOutput
    CreateJobOutput (..),
    newCreateJobOutput,
    createJobOutput_albumArt,
    createJobOutput_captions,
    createJobOutput_composition,
    createJobOutput_encryption,
    createJobOutput_key,
    createJobOutput_presetId,
    createJobOutput_rotate,
    createJobOutput_segmentDuration,
    createJobOutput_thumbnailEncryption,
    createJobOutput_thumbnailPattern,
    createJobOutput_watermarks,

    -- * CreateJobPlaylist
    CreateJobPlaylist (..),
    newCreateJobPlaylist,
    createJobPlaylist_format,
    createJobPlaylist_hlsContentProtection,
    createJobPlaylist_name,
    createJobPlaylist_outputKeys,
    createJobPlaylist_playReadyDrm,

    -- * DetectedProperties
    DetectedProperties (..),
    newDetectedProperties,
    detectedProperties_durationMillis,
    detectedProperties_fileSize,
    detectedProperties_frameRate,
    detectedProperties_height,
    detectedProperties_width,

    -- * Encryption
    Encryption (..),
    newEncryption,
    encryption_initializationVector,
    encryption_key,
    encryption_keyMd5,
    encryption_mode,

    -- * HlsContentProtection
    HlsContentProtection (..),
    newHlsContentProtection,
    hlsContentProtection_initializationVector,
    hlsContentProtection_key,
    hlsContentProtection_keyMd5,
    hlsContentProtection_keyStoragePolicy,
    hlsContentProtection_licenseAcquisitionUrl,
    hlsContentProtection_method,

    -- * InputCaptions
    InputCaptions (..),
    newInputCaptions,
    inputCaptions_captionSources,
    inputCaptions_mergePolicy,

    -- * Job
    Job (..),
    newJob,
    job_arn,
    job_id,
    job_input,
    job_inputs,
    job_output,
    job_outputKeyPrefix,
    job_outputs,
    job_pipelineId,
    job_playlists,
    job_status,
    job_timing,
    job_userMetadata,

    -- * JobAlbumArt
    JobAlbumArt (..),
    newJobAlbumArt,
    jobAlbumArt_artwork,
    jobAlbumArt_mergePolicy,

    -- * JobInput
    JobInput (..),
    newJobInput,
    jobInput_aspectRatio,
    jobInput_container,
    jobInput_detectedProperties,
    jobInput_encryption,
    jobInput_frameRate,
    jobInput_inputCaptions,
    jobInput_interlaced,
    jobInput_key,
    jobInput_resolution,
    jobInput_timeSpan,

    -- * JobOutput
    JobOutput (..),
    newJobOutput,
    jobOutput_albumArt,
    jobOutput_appliedColorSpaceConversion,
    jobOutput_captions,
    jobOutput_composition,
    jobOutput_duration,
    jobOutput_durationMillis,
    jobOutput_encryption,
    jobOutput_fileSize,
    jobOutput_frameRate,
    jobOutput_height,
    jobOutput_id,
    jobOutput_key,
    jobOutput_presetId,
    jobOutput_rotate,
    jobOutput_segmentDuration,
    jobOutput_status,
    jobOutput_statusDetail,
    jobOutput_thumbnailEncryption,
    jobOutput_thumbnailPattern,
    jobOutput_watermarks,
    jobOutput_width,

    -- * JobWatermark
    JobWatermark (..),
    newJobWatermark,
    jobWatermark_encryption,
    jobWatermark_inputKey,
    jobWatermark_presetWatermarkId,

    -- * Notifications
    Notifications (..),
    newNotifications,
    notifications_completed,
    notifications_error,
    notifications_progressing,
    notifications_warning,

    -- * Permission
    Permission (..),
    newPermission,
    permission_access,
    permission_grantee,
    permission_granteeType,

    -- * Pipeline
    Pipeline (..),
    newPipeline,
    pipeline_arn,
    pipeline_awsKmsKeyArn,
    pipeline_contentConfig,
    pipeline_id,
    pipeline_inputBucket,
    pipeline_name,
    pipeline_notifications,
    pipeline_outputBucket,
    pipeline_role,
    pipeline_status,
    pipeline_thumbnailConfig,

    -- * PipelineOutputConfig
    PipelineOutputConfig (..),
    newPipelineOutputConfig,
    pipelineOutputConfig_bucket,
    pipelineOutputConfig_permissions,
    pipelineOutputConfig_storageClass,

    -- * PlayReadyDrm
    PlayReadyDrm (..),
    newPlayReadyDrm,
    playReadyDrm_format,
    playReadyDrm_initializationVector,
    playReadyDrm_key,
    playReadyDrm_keyId,
    playReadyDrm_keyMd5,
    playReadyDrm_licenseAcquisitionUrl,

    -- * Playlist
    Playlist (..),
    newPlaylist,
    playlist_format,
    playlist_hlsContentProtection,
    playlist_name,
    playlist_outputKeys,
    playlist_playReadyDrm,
    playlist_status,
    playlist_statusDetail,

    -- * Preset
    Preset (..),
    newPreset,
    preset_arn,
    preset_audio,
    preset_container,
    preset_description,
    preset_id,
    preset_name,
    preset_thumbnails,
    preset_type,
    preset_video,

    -- * PresetWatermark
    PresetWatermark (..),
    newPresetWatermark,
    presetWatermark_horizontalAlign,
    presetWatermark_horizontalOffset,
    presetWatermark_id,
    presetWatermark_maxHeight,
    presetWatermark_maxWidth,
    presetWatermark_opacity,
    presetWatermark_sizingPolicy,
    presetWatermark_target,
    presetWatermark_verticalAlign,
    presetWatermark_verticalOffset,

    -- * Thumbnails
    Thumbnails (..),
    newThumbnails,
    thumbnails_aspectRatio,
    thumbnails_format,
    thumbnails_interval,
    thumbnails_maxHeight,
    thumbnails_maxWidth,
    thumbnails_paddingPolicy,
    thumbnails_resolution,
    thumbnails_sizingPolicy,

    -- * TimeSpan
    TimeSpan (..),
    newTimeSpan,
    timeSpan_duration,
    timeSpan_startTime,

    -- * Timing
    Timing (..),
    newTiming,
    timing_finishTimeMillis,
    timing_startTimeMillis,
    timing_submitTimeMillis,

    -- * VideoParameters
    VideoParameters (..),
    newVideoParameters,
    videoParameters_aspectRatio,
    videoParameters_bitRate,
    videoParameters_codec,
    videoParameters_codecOptions,
    videoParameters_displayAspectRatio,
    videoParameters_fixedGOP,
    videoParameters_frameRate,
    videoParameters_keyframesMaxDist,
    videoParameters_maxFrameRate,
    videoParameters_maxHeight,
    videoParameters_maxWidth,
    videoParameters_paddingPolicy,
    videoParameters_resolution,
    videoParameters_sizingPolicy,
    videoParameters_watermarks,

    -- * Warning
    Warning (..),
    newWarning,
    warning_code,
    warning_message,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticTranscoder.Types.Artwork
import Amazonka.ElasticTranscoder.Types.AudioCodecOptions
import Amazonka.ElasticTranscoder.Types.AudioParameters
import Amazonka.ElasticTranscoder.Types.CaptionFormat
import Amazonka.ElasticTranscoder.Types.CaptionSource
import Amazonka.ElasticTranscoder.Types.Captions
import Amazonka.ElasticTranscoder.Types.Clip
import Amazonka.ElasticTranscoder.Types.CreateJobOutput
import Amazonka.ElasticTranscoder.Types.CreateJobPlaylist
import Amazonka.ElasticTranscoder.Types.DetectedProperties
import Amazonka.ElasticTranscoder.Types.Encryption
import Amazonka.ElasticTranscoder.Types.HlsContentProtection
import Amazonka.ElasticTranscoder.Types.InputCaptions
import Amazonka.ElasticTranscoder.Types.Job
import Amazonka.ElasticTranscoder.Types.JobAlbumArt
import Amazonka.ElasticTranscoder.Types.JobInput
import Amazonka.ElasticTranscoder.Types.JobOutput
import Amazonka.ElasticTranscoder.Types.JobWatermark
import Amazonka.ElasticTranscoder.Types.Notifications
import Amazonka.ElasticTranscoder.Types.Permission
import Amazonka.ElasticTranscoder.Types.Pipeline
import Amazonka.ElasticTranscoder.Types.PipelineOutputConfig
import Amazonka.ElasticTranscoder.Types.PlayReadyDrm
import Amazonka.ElasticTranscoder.Types.Playlist
import Amazonka.ElasticTranscoder.Types.Preset
import Amazonka.ElasticTranscoder.Types.PresetWatermark
import Amazonka.ElasticTranscoder.Types.Thumbnails
import Amazonka.ElasticTranscoder.Types.TimeSpan
import Amazonka.ElasticTranscoder.Types.Timing
import Amazonka.ElasticTranscoder.Types.VideoParameters
import Amazonka.ElasticTranscoder.Types.Warning
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2012-09-25@ of the Amazon Elastic Transcoder SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ElasticTranscoder",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "elastictranscoder",
      Core.signingName = "elastictranscoder",
      Core.version = "2012-09-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ElasticTranscoder",
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

-- | General authentication failure. The request was not signed correctly.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Prism for IncompatibleVersionException' errors.
_IncompatibleVersionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IncompatibleVersionException =
  Core._MatchServiceError
    defaultService
    "IncompatibleVersionException"
    Prelude.. Core.hasStatus 400

-- | Elastic Transcoder encountered an unexpected exception while trying to
-- fulfill the request.
_InternalServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | Too many operations for a given AWS account. For example, the number of
-- pipelines exceeds the maximum allowed.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The resource you are attempting to change is in use. For example, you
-- are attempting to delete a pipeline that is currently in use.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409

-- | The requested resource does not exist or is not available. For example,
-- the pipeline to which you\'re trying to add a job doesn\'t exist or is
-- still being created.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | One or more required parameter values were not provided in the request.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
