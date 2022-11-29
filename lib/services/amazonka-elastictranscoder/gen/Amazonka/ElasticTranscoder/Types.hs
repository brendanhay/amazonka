{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticTranscoder.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ResourceNotFoundException,
    _ResourceInUseException,
    _LimitExceededException,
    _IncompatibleVersionException,
    _ValidationException,
    _InternalServiceException,

    -- * Artwork
    Artwork (..),
    newArtwork,
    artwork_albumArtFormat,
    artwork_sizingPolicy,
    artwork_paddingPolicy,
    artwork_inputKey,
    artwork_encryption,
    artwork_maxHeight,
    artwork_maxWidth,

    -- * AudioCodecOptions
    AudioCodecOptions (..),
    newAudioCodecOptions,
    audioCodecOptions_bitDepth,
    audioCodecOptions_profile,
    audioCodecOptions_bitOrder,
    audioCodecOptions_signed,

    -- * AudioParameters
    AudioParameters (..),
    newAudioParameters,
    audioParameters_audioPackingMode,
    audioParameters_codecOptions,
    audioParameters_channels,
    audioParameters_bitRate,
    audioParameters_sampleRate,
    audioParameters_codec,

    -- * CaptionFormat
    CaptionFormat (..),
    newCaptionFormat,
    captionFormat_format,
    captionFormat_pattern,
    captionFormat_encryption,

    -- * CaptionSource
    CaptionSource (..),
    newCaptionSource,
    captionSource_key,
    captionSource_label,
    captionSource_timeOffset,
    captionSource_encryption,
    captionSource_language,

    -- * Captions
    Captions (..),
    newCaptions,
    captions_captionSources,
    captions_mergePolicy,
    captions_captionFormats,

    -- * Clip
    Clip (..),
    newClip,
    clip_timeSpan,

    -- * CreateJobOutput
    CreateJobOutput (..),
    newCreateJobOutput,
    createJobOutput_key,
    createJobOutput_watermarks,
    createJobOutput_composition,
    createJobOutput_thumbnailEncryption,
    createJobOutput_thumbnailPattern,
    createJobOutput_captions,
    createJobOutput_albumArt,
    createJobOutput_presetId,
    createJobOutput_encryption,
    createJobOutput_rotate,
    createJobOutput_segmentDuration,

    -- * CreateJobPlaylist
    CreateJobPlaylist (..),
    newCreateJobPlaylist,
    createJobPlaylist_name,
    createJobPlaylist_hlsContentProtection,
    createJobPlaylist_playReadyDrm,
    createJobPlaylist_format,
    createJobPlaylist_outputKeys,

    -- * DetectedProperties
    DetectedProperties (..),
    newDetectedProperties,
    detectedProperties_fileSize,
    detectedProperties_width,
    detectedProperties_durationMillis,
    detectedProperties_height,
    detectedProperties_frameRate,

    -- * Encryption
    Encryption (..),
    newEncryption,
    encryption_key,
    encryption_initializationVector,
    encryption_mode,
    encryption_keyMd5,

    -- * HlsContentProtection
    HlsContentProtection (..),
    newHlsContentProtection,
    hlsContentProtection_keyStoragePolicy,
    hlsContentProtection_key,
    hlsContentProtection_method,
    hlsContentProtection_licenseAcquisitionUrl,
    hlsContentProtection_initializationVector,
    hlsContentProtection_keyMd5,

    -- * InputCaptions
    InputCaptions (..),
    newInputCaptions,
    inputCaptions_captionSources,
    inputCaptions_mergePolicy,

    -- * Job
    Job (..),
    newJob,
    job_timing,
    job_arn,
    job_status,
    job_id,
    job_input,
    job_pipelineId,
    job_outputs,
    job_output,
    job_playlists,
    job_outputKeyPrefix,
    job_inputs,
    job_userMetadata,

    -- * JobAlbumArt
    JobAlbumArt (..),
    newJobAlbumArt,
    jobAlbumArt_mergePolicy,
    jobAlbumArt_artwork,

    -- * JobInput
    JobInput (..),
    newJobInput,
    jobInput_detectedProperties,
    jobInput_key,
    jobInput_aspectRatio,
    jobInput_interlaced,
    jobInput_timeSpan,
    jobInput_container,
    jobInput_encryption,
    jobInput_resolution,
    jobInput_inputCaptions,
    jobInput_frameRate,

    -- * JobOutput
    JobOutput (..),
    newJobOutput,
    jobOutput_key,
    jobOutput_fileSize,
    jobOutput_watermarks,
    jobOutput_composition,
    jobOutput_thumbnailEncryption,
    jobOutput_thumbnailPattern,
    jobOutput_statusDetail,
    jobOutput_status,
    jobOutput_id,
    jobOutput_captions,
    jobOutput_width,
    jobOutput_appliedColorSpaceConversion,
    jobOutput_duration,
    jobOutput_albumArt,
    jobOutput_presetId,
    jobOutput_encryption,
    jobOutput_durationMillis,
    jobOutput_height,
    jobOutput_rotate,
    jobOutput_segmentDuration,
    jobOutput_frameRate,

    -- * JobWatermark
    JobWatermark (..),
    newJobWatermark,
    jobWatermark_inputKey,
    jobWatermark_encryption,
    jobWatermark_presetWatermarkId,

    -- * Notifications
    Notifications (..),
    newNotifications,
    notifications_warning,
    notifications_completed,
    notifications_error,
    notifications_progressing,

    -- * Permission
    Permission (..),
    newPermission,
    permission_granteeType,
    permission_access,
    permission_grantee,

    -- * Pipeline
    Pipeline (..),
    newPipeline,
    pipeline_notifications,
    pipeline_thumbnailConfig,
    pipeline_name,
    pipeline_inputBucket,
    pipeline_awsKmsKeyArn,
    pipeline_arn,
    pipeline_status,
    pipeline_id,
    pipeline_outputBucket,
    pipeline_role,
    pipeline_contentConfig,

    -- * PipelineOutputConfig
    PipelineOutputConfig (..),
    newPipelineOutputConfig,
    pipelineOutputConfig_bucket,
    pipelineOutputConfig_permissions,
    pipelineOutputConfig_storageClass,

    -- * PlayReadyDrm
    PlayReadyDrm (..),
    newPlayReadyDrm,
    playReadyDrm_key,
    playReadyDrm_licenseAcquisitionUrl,
    playReadyDrm_format,
    playReadyDrm_initializationVector,
    playReadyDrm_keyMd5,
    playReadyDrm_keyId,

    -- * Playlist
    Playlist (..),
    newPlaylist,
    playlist_name,
    playlist_hlsContentProtection,
    playlist_playReadyDrm,
    playlist_format,
    playlist_statusDetail,
    playlist_status,
    playlist_outputKeys,

    -- * Preset
    Preset (..),
    newPreset,
    preset_name,
    preset_type,
    preset_audio,
    preset_arn,
    preset_id,
    preset_description,
    preset_container,
    preset_thumbnails,
    preset_video,

    -- * PresetWatermark
    PresetWatermark (..),
    newPresetWatermark,
    presetWatermark_verticalAlign,
    presetWatermark_sizingPolicy,
    presetWatermark_target,
    presetWatermark_id,
    presetWatermark_horizontalAlign,
    presetWatermark_horizontalOffset,
    presetWatermark_opacity,
    presetWatermark_maxHeight,
    presetWatermark_verticalOffset,
    presetWatermark_maxWidth,

    -- * Thumbnails
    Thumbnails (..),
    newThumbnails,
    thumbnails_sizingPolicy,
    thumbnails_paddingPolicy,
    thumbnails_interval,
    thumbnails_aspectRatio,
    thumbnails_format,
    thumbnails_resolution,
    thumbnails_maxHeight,
    thumbnails_maxWidth,

    -- * TimeSpan
    TimeSpan (..),
    newTimeSpan,
    timeSpan_duration,
    timeSpan_startTime,

    -- * Timing
    Timing (..),
    newTiming,
    timing_submitTimeMillis,
    timing_startTimeMillis,
    timing_finishTimeMillis,

    -- * VideoParameters
    VideoParameters (..),
    newVideoParameters,
    videoParameters_sizingPolicy,
    videoParameters_watermarks,
    videoParameters_codecOptions,
    videoParameters_paddingPolicy,
    videoParameters_keyframesMaxDist,
    videoParameters_bitRate,
    videoParameters_aspectRatio,
    videoParameters_codec,
    videoParameters_fixedGOP,
    videoParameters_resolution,
    videoParameters_maxHeight,
    videoParameters_displayAspectRatio,
    videoParameters_maxWidth,
    videoParameters_maxFrameRate,
    videoParameters_frameRate,

    -- * Warning
    Warning (..),
    newWarning,
    warning_message,
    warning_code,
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | General authentication failure. The request was not signed correctly.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The requested resource does not exist or is not available. For example,
-- the pipeline to which you\'re trying to add a job doesn\'t exist or is
-- still being created.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The resource you are attempting to change is in use. For example, you
-- are attempting to delete a pipeline that is currently in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409

-- | Too many operations for a given AWS account. For example, the number of
-- pipelines exceeds the maximum allowed.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | Prism for IncompatibleVersionException' errors.
_IncompatibleVersionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatibleVersionException =
  Core._MatchServiceError
    defaultService
    "IncompatibleVersionException"
    Prelude.. Core.hasStatus 400

-- | One or more required parameter values were not provided in the request.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Elastic Transcoder encountered an unexpected exception while trying to
-- fulfill the request.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
