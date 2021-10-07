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
    _InternalServiceException,
    _AccessDeniedException,
    _ValidationException,
    _LimitExceededException,
    _ResourceInUseException,
    _ResourceNotFoundException,

    -- * Artwork
    Artwork (..),
    newArtwork,
    artwork_inputKey,
    artwork_albumArtFormat,
    artwork_sizingPolicy,
    artwork_encryption,
    artwork_paddingPolicy,
    artwork_maxHeight,
    artwork_maxWidth,

    -- * AudioCodecOptions
    AudioCodecOptions (..),
    newAudioCodecOptions,
    audioCodecOptions_bitDepth,
    audioCodecOptions_signed,
    audioCodecOptions_bitOrder,
    audioCodecOptions_profile,

    -- * AudioParameters
    AudioParameters (..),
    newAudioParameters,
    audioParameters_codecOptions,
    audioParameters_codec,
    audioParameters_channels,
    audioParameters_audioPackingMode,
    audioParameters_sampleRate,
    audioParameters_bitRate,

    -- * CaptionFormat
    CaptionFormat (..),
    newCaptionFormat,
    captionFormat_format,
    captionFormat_encryption,
    captionFormat_pattern,

    -- * CaptionSource
    CaptionSource (..),
    newCaptionSource,
    captionSource_key,
    captionSource_timeOffset,
    captionSource_encryption,
    captionSource_label,
    captionSource_language,

    -- * Captions
    Captions (..),
    newCaptions,
    captions_captionSources,
    captions_captionFormats,
    captions_mergePolicy,

    -- * Clip
    Clip (..),
    newClip,
    clip_timeSpan,

    -- * CreateJobOutput
    CreateJobOutput (..),
    newCreateJobOutput,
    createJobOutput_key,
    createJobOutput_thumbnailPattern,
    createJobOutput_thumbnailEncryption,
    createJobOutput_watermarks,
    createJobOutput_albumArt,
    createJobOutput_presetId,
    createJobOutput_encryption,
    createJobOutput_rotate,
    createJobOutput_composition,
    createJobOutput_captions,
    createJobOutput_segmentDuration,

    -- * CreateJobPlaylist
    CreateJobPlaylist (..),
    newCreateJobPlaylist,
    createJobPlaylist_playReadyDrm,
    createJobPlaylist_outputKeys,
    createJobPlaylist_format,
    createJobPlaylist_hlsContentProtection,
    createJobPlaylist_name,

    -- * DetectedProperties
    DetectedProperties (..),
    newDetectedProperties,
    detectedProperties_height,
    detectedProperties_width,
    detectedProperties_fileSize,
    detectedProperties_frameRate,
    detectedProperties_durationMillis,

    -- * Encryption
    Encryption (..),
    newEncryption,
    encryption_key,
    encryption_keyMd5,
    encryption_mode,
    encryption_initializationVector,

    -- * HlsContentProtection
    HlsContentProtection (..),
    newHlsContentProtection,
    hlsContentProtection_key,
    hlsContentProtection_licenseAcquisitionUrl,
    hlsContentProtection_keyMd5,
    hlsContentProtection_method,
    hlsContentProtection_initializationVector,
    hlsContentProtection_keyStoragePolicy,

    -- * InputCaptions
    InputCaptions (..),
    newInputCaptions,
    inputCaptions_captionSources,
    inputCaptions_mergePolicy,

    -- * Job'
    Job' (..),
    newJob',
    job'_pipelineId,
    job'_status,
    job'_outputs,
    job'_input,
    job'_outputKeyPrefix,
    job'_arn,
    job'_id,
    job'_output,
    job'_userMetadata,
    job'_timing,
    job'_playlists,
    job'_inputs,

    -- * JobAlbumArt
    JobAlbumArt (..),
    newJobAlbumArt,
    jobAlbumArt_artwork,
    jobAlbumArt_mergePolicy,

    -- * JobInput
    JobInput (..),
    newJobInput,
    jobInput_key,
    jobInput_container,
    jobInput_timeSpan,
    jobInput_inputCaptions,
    jobInput_encryption,
    jobInput_detectedProperties,
    jobInput_frameRate,
    jobInput_aspectRatio,
    jobInput_resolution,
    jobInput_interlaced,

    -- * JobOutput
    JobOutput (..),
    newJobOutput,
    jobOutput_key,
    jobOutput_height,
    jobOutput_thumbnailPattern,
    jobOutput_status,
    jobOutput_width,
    jobOutput_duration,
    jobOutput_thumbnailEncryption,
    jobOutput_fileSize,
    jobOutput_watermarks,
    jobOutput_albumArt,
    jobOutput_presetId,
    jobOutput_statusDetail,
    jobOutput_id,
    jobOutput_encryption,
    jobOutput_frameRate,
    jobOutput_appliedColorSpaceConversion,
    jobOutput_rotate,
    jobOutput_durationMillis,
    jobOutput_composition,
    jobOutput_captions,
    jobOutput_segmentDuration,

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
    notifications_error,
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
    pipeline_outputBucket,
    pipeline_arn,
    pipeline_id,
    pipeline_name,
    pipeline_role,
    pipeline_thumbnailConfig,
    pipeline_notifications,
    pipeline_contentConfig,
    pipeline_inputBucket,
    pipeline_awsKmsKeyArn,

    -- * PipelineOutputConfig
    PipelineOutputConfig (..),
    newPipelineOutputConfig,
    pipelineOutputConfig_permissions,
    pipelineOutputConfig_storageClass,
    pipelineOutputConfig_bucket,

    -- * PlayReadyDrm
    PlayReadyDrm (..),
    newPlayReadyDrm,
    playReadyDrm_key,
    playReadyDrm_licenseAcquisitionUrl,
    playReadyDrm_keyMd5,
    playReadyDrm_format,
    playReadyDrm_initializationVector,
    playReadyDrm_keyId,

    -- * Playlist
    Playlist (..),
    newPlaylist,
    playlist_status,
    playlist_playReadyDrm,
    playlist_outputKeys,
    playlist_format,
    playlist_statusDetail,
    playlist_hlsContentProtection,
    playlist_name,

    -- * Preset
    Preset (..),
    newPreset,
    preset_container,
    preset_arn,
    preset_id,
    preset_name,
    preset_thumbnails,
    preset_video,
    preset_description,
    preset_audio,
    preset_type,

    -- * PresetWatermark
    PresetWatermark (..),
    newPresetWatermark,
    presetWatermark_horizontalAlign,
    presetWatermark_horizontalOffset,
    presetWatermark_sizingPolicy,
    presetWatermark_id,
    presetWatermark_verticalOffset,
    presetWatermark_verticalAlign,
    presetWatermark_opacity,
    presetWatermark_target,
    presetWatermark_maxHeight,
    presetWatermark_maxWidth,

    -- * Thumbnails
    Thumbnails (..),
    newThumbnails,
    thumbnails_format,
    thumbnails_sizingPolicy,
    thumbnails_interval,
    thumbnails_aspectRatio,
    thumbnails_paddingPolicy,
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
    videoParameters_keyframesMaxDist,
    videoParameters_codecOptions,
    videoParameters_fixedGOP,
    videoParameters_codec,
    videoParameters_maxFrameRate,
    videoParameters_displayAspectRatio,
    videoParameters_watermarks,
    videoParameters_sizingPolicy,
    videoParameters_frameRate,
    videoParameters_aspectRatio,
    videoParameters_paddingPolicy,
    videoParameters_resolution,
    videoParameters_maxHeight,
    videoParameters_bitRate,
    videoParameters_maxWidth,

    -- * Warning
    Warning (..),
    newWarning,
    warning_message,
    warning_code,
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
import Network.AWS.ElasticTranscoder.Types.Job'
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

-- | Prism for IncompatibleVersionException' errors.
_IncompatibleVersionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatibleVersionException =
  Core._MatchServiceError
    defaultService
    "IncompatibleVersionException"
    Prelude.. Core.hasStatus 400

-- | Elastic Transcoder encountered an unexpected exception while trying to
-- fulfill the request.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | General authentication failure. The request was not signed correctly.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | One or more required parameter values were not provided in the request.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

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

-- | The requested resource does not exist or is not available. For example,
-- the pipeline to which you\'re trying to add a job doesn\'t exist or is
-- still being created.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
