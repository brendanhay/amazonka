{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _IncompatibleVersionException,
    _ValidationException,
    _AccessDeniedException,
    _InternalServiceException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * Artwork
    Artwork (..),
    newArtwork,
    artwork_sizingPolicy,
    artwork_albumArtFormat,
    artwork_maxHeight,
    artwork_inputKey,
    artwork_paddingPolicy,
    artwork_encryption,
    artwork_maxWidth,

    -- * AudioCodecOptions
    AudioCodecOptions (..),
    newAudioCodecOptions,
    audioCodecOptions_signed,
    audioCodecOptions_bitDepth,
    audioCodecOptions_profile,
    audioCodecOptions_bitOrder,

    -- * AudioParameters
    AudioParameters (..),
    newAudioParameters,
    audioParameters_channels,
    audioParameters_codec,
    audioParameters_audioPackingMode,
    audioParameters_sampleRate,
    audioParameters_bitRate,
    audioParameters_codecOptions,

    -- * CaptionFormat
    CaptionFormat (..),
    newCaptionFormat,
    captionFormat_pattern,
    captionFormat_format,
    captionFormat_encryption,

    -- * CaptionSource
    CaptionSource (..),
    newCaptionSource,
    captionSource_timeOffset,
    captionSource_encryption,
    captionSource_key,
    captionSource_language,
    captionSource_label,

    -- * Captions
    Captions (..),
    newCaptions,
    captions_mergePolicy,
    captions_captionSources,
    captions_captionFormats,

    -- * Clip
    Clip (..),
    newClip,
    clip_timeSpan,

    -- * CreateJobOutput
    CreateJobOutput (..),
    newCreateJobOutput,
    createJobOutput_thumbnailPattern,
    createJobOutput_captions,
    createJobOutput_presetId,
    createJobOutput_composition,
    createJobOutput_albumArt,
    createJobOutput_watermarks,
    createJobOutput_encryption,
    createJobOutput_key,
    createJobOutput_segmentDuration,
    createJobOutput_thumbnailEncryption,
    createJobOutput_rotate,

    -- * CreateJobPlaylist
    CreateJobPlaylist (..),
    newCreateJobPlaylist,
    createJobPlaylist_playReadyDrm,
    createJobPlaylist_format,
    createJobPlaylist_outputKeys,
    createJobPlaylist_name,
    createJobPlaylist_hlsContentProtection,

    -- * DetectedProperties
    DetectedProperties (..),
    newDetectedProperties,
    detectedProperties_height,
    detectedProperties_frameRate,
    detectedProperties_fileSize,
    detectedProperties_width,
    detectedProperties_durationMillis,

    -- * Encryption
    Encryption (..),
    newEncryption,
    encryption_mode,
    encryption_keyMd5,
    encryption_key,
    encryption_initializationVector,

    -- * HlsContentProtection
    HlsContentProtection (..),
    newHlsContentProtection,
    hlsContentProtection_keyMd5,
    hlsContentProtection_keyStoragePolicy,
    hlsContentProtection_key,
    hlsContentProtection_method,
    hlsContentProtection_initializationVector,
    hlsContentProtection_licenseAcquisitionUrl,

    -- * InputCaptions
    InputCaptions (..),
    newInputCaptions,
    inputCaptions_mergePolicy,
    inputCaptions_captionSources,

    -- * Job
    Job (..),
    newJob,
    job_status,
    job_pipelineId,
    job_arn,
    job_inputs,
    job_input,
    job_userMetadata,
    job_outputs,
    job_output,
    job_id,
    job_playlists,
    job_outputKeyPrefix,
    job_timing,

    -- * JobAlbumArt
    JobAlbumArt (..),
    newJobAlbumArt,
    jobAlbumArt_mergePolicy,
    jobAlbumArt_artwork,

    -- * JobInput
    JobInput (..),
    newJobInput,
    jobInput_frameRate,
    jobInput_resolution,
    jobInput_aspectRatio,
    jobInput_timeSpan,
    jobInput_encryption,
    jobInput_key,
    jobInput_detectedProperties,
    jobInput_container,
    jobInput_interlaced,
    jobInput_inputCaptions,

    -- * JobOutput
    JobOutput (..),
    newJobOutput,
    jobOutput_appliedColorSpaceConversion,
    jobOutput_thumbnailPattern,
    jobOutput_status,
    jobOutput_height,
    jobOutput_frameRate,
    jobOutput_captions,
    jobOutput_presetId,
    jobOutput_composition,
    jobOutput_albumArt,
    jobOutput_fileSize,
    jobOutput_watermarks,
    jobOutput_width,
    jobOutput_encryption,
    jobOutput_key,
    jobOutput_statusDetail,
    jobOutput_id,
    jobOutput_segmentDuration,
    jobOutput_durationMillis,
    jobOutput_thumbnailEncryption,
    jobOutput_duration,
    jobOutput_rotate,

    -- * JobWatermark
    JobWatermark (..),
    newJobWatermark,
    jobWatermark_presetWatermarkId,
    jobWatermark_inputKey,
    jobWatermark_encryption,

    -- * Notifications
    Notifications (..),
    newNotifications,
    notifications_error,
    notifications_warning,
    notifications_progressing,
    notifications_completed,

    -- * Permission
    Permission (..),
    newPermission,
    permission_access,
    permission_granteeType,
    permission_grantee,

    -- * Pipeline
    Pipeline (..),
    newPipeline,
    pipeline_status,
    pipeline_arn,
    pipeline_inputBucket,
    pipeline_contentConfig,
    pipeline_outputBucket,
    pipeline_role,
    pipeline_name,
    pipeline_awsKmsKeyArn,
    pipeline_id,
    pipeline_notifications,
    pipeline_thumbnailConfig,

    -- * PipelineOutputConfig
    PipelineOutputConfig (..),
    newPipelineOutputConfig,
    pipelineOutputConfig_bucket,
    pipelineOutputConfig_storageClass,
    pipelineOutputConfig_permissions,

    -- * PlayReadyDrm
    PlayReadyDrm (..),
    newPlayReadyDrm,
    playReadyDrm_keyId,
    playReadyDrm_format,
    playReadyDrm_keyMd5,
    playReadyDrm_key,
    playReadyDrm_initializationVector,
    playReadyDrm_licenseAcquisitionUrl,

    -- * Playlist
    Playlist (..),
    newPlaylist,
    playlist_status,
    playlist_playReadyDrm,
    playlist_format,
    playlist_outputKeys,
    playlist_name,
    playlist_statusDetail,
    playlist_hlsContentProtection,

    -- * Preset
    Preset (..),
    newPreset,
    preset_arn,
    preset_video,
    preset_thumbnails,
    preset_name,
    preset_container,
    preset_id,
    preset_type,
    preset_description,
    preset_audio,

    -- * PresetWatermark
    PresetWatermark (..),
    newPresetWatermark,
    presetWatermark_verticalAlign,
    presetWatermark_sizingPolicy,
    presetWatermark_horizontalOffset,
    presetWatermark_maxHeight,
    presetWatermark_opacity,
    presetWatermark_verticalOffset,
    presetWatermark_maxWidth,
    presetWatermark_id,
    presetWatermark_horizontalAlign,
    presetWatermark_target,

    -- * Thumbnails
    Thumbnails (..),
    newThumbnails,
    thumbnails_sizingPolicy,
    thumbnails_format,
    thumbnails_maxHeight,
    thumbnails_resolution,
    thumbnails_aspectRatio,
    thumbnails_paddingPolicy,
    thumbnails_interval,
    thumbnails_maxWidth,

    -- * TimeSpan
    TimeSpan (..),
    newTimeSpan,
    timeSpan_startTime,
    timeSpan_duration,

    -- * Timing
    Timing (..),
    newTiming,
    timing_submitTimeMillis,
    timing_finishTimeMillis,
    timing_startTimeMillis,

    -- * VideoParameters
    VideoParameters (..),
    newVideoParameters,
    videoParameters_keyframesMaxDist,
    videoParameters_frameRate,
    videoParameters_sizingPolicy,
    videoParameters_maxFrameRate,
    videoParameters_maxHeight,
    videoParameters_watermarks,
    videoParameters_displayAspectRatio,
    videoParameters_resolution,
    videoParameters_codec,
    videoParameters_aspectRatio,
    videoParameters_paddingPolicy,
    videoParameters_maxWidth,
    videoParameters_bitRate,
    videoParameters_fixedGOP,
    videoParameters_codecOptions,

    -- * Warning
    Warning (..),
    newWarning,
    warning_code,
    warning_message,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types.Artwork
import Network.AWS.ElasticTranscoder.Types.AudioCodecOptions
import Network.AWS.ElasticTranscoder.Types.AudioParameters
import Network.AWS.ElasticTranscoder.Types.CaptionFormat
import Network.AWS.ElasticTranscoder.Types.CaptionSource
import Network.AWS.ElasticTranscoder.Types.Captions
import Network.AWS.ElasticTranscoder.Types.Clip
import Network.AWS.ElasticTranscoder.Types.CreateJobOutput
import Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist
import Network.AWS.ElasticTranscoder.Types.DetectedProperties
import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.ElasticTranscoder.Types.HlsContentProtection
import Network.AWS.ElasticTranscoder.Types.InputCaptions
import Network.AWS.ElasticTranscoder.Types.Job
import Network.AWS.ElasticTranscoder.Types.JobAlbumArt
import Network.AWS.ElasticTranscoder.Types.JobInput
import Network.AWS.ElasticTranscoder.Types.JobOutput
import Network.AWS.ElasticTranscoder.Types.JobWatermark
import Network.AWS.ElasticTranscoder.Types.Notifications
import Network.AWS.ElasticTranscoder.Types.Permission
import Network.AWS.ElasticTranscoder.Types.Pipeline
import Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig
import Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
import Network.AWS.ElasticTranscoder.Types.Playlist
import Network.AWS.ElasticTranscoder.Types.Preset
import Network.AWS.ElasticTranscoder.Types.PresetWatermark
import Network.AWS.ElasticTranscoder.Types.Thumbnails
import Network.AWS.ElasticTranscoder.Types.TimeSpan
import Network.AWS.ElasticTranscoder.Types.Timing
import Network.AWS.ElasticTranscoder.Types.VideoParameters
import Network.AWS.ElasticTranscoder.Types.Warning
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-09-25@ of the Amazon Elastic Transcoder SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ElasticTranscoder",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "elastictranscoder",
      Core._serviceSigningName = "elastictranscoder",
      Core._serviceVersion = "2012-09-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ElasticTranscoder",
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

-- | General authentication failure. The request was not signed correctly.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Elastic Transcoder encountered an unexpected exception while trying to
-- fulfill the request.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The requested resource does not exist or is not available. For example,
-- the pipeline to which you\'re trying to add a job doesn\'t exist or is
-- still being created.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Too many operations for a given AWS account. For example, the number of
-- pipelines exceeds the maximum allowed.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The resource you are attempting to change is in use. For example, you
-- are attempting to delete a pipeline that is currently in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409
