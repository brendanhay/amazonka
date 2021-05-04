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
    _ResourceInUseException,
    _LimitExceededException,
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
    audioParameters_sampleRate,
    audioParameters_audioPackingMode,
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
    createJobOutput_presetId,
    createJobOutput_albumArt,
    createJobOutput_encryption,
    createJobOutput_rotate,
    createJobOutput_composition,
    createJobOutput_segmentDuration,
    createJobOutput_captions,

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
    encryption_mode,
    encryption_keyMd5,
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
    job'_inputs,
    job'_playlists,

    -- * JobAlbumArt
    JobAlbumArt (..),
    newJobAlbumArt,
    jobAlbumArt_artwork,
    jobAlbumArt_mergePolicy,

    -- * JobInput
    JobInput (..),
    newJobInput,
    jobInput_container,
    jobInput_key,
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
    jobOutput_height,
    jobOutput_key,
    jobOutput_status,
    jobOutput_thumbnailPattern,
    jobOutput_duration,
    jobOutput_width,
    jobOutput_thumbnailEncryption,
    jobOutput_watermarks,
    jobOutput_fileSize,
    jobOutput_presetId,
    jobOutput_albumArt,
    jobOutput_id,
    jobOutput_statusDetail,
    jobOutput_encryption,
    jobOutput_frameRate,
    jobOutput_appliedColorSpaceConversion,
    jobOutput_rotate,
    jobOutput_durationMillis,
    jobOutput_composition,
    jobOutput_segmentDuration,
    jobOutput_captions,

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
    pipeline_notifications,
    pipeline_thumbnailConfig,
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
    playlist_hlsContentProtection,
    playlist_statusDetail,
    playlist_name,

    -- * Preset
    Preset (..),
    newPreset,
    preset_container,
    preset_arn,
    preset_id,
    preset_thumbnails,
    preset_name,
    preset_video,
    preset_audio,
    preset_description,
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
    videoParameters_watermarks,
    videoParameters_maxFrameRate,
    videoParameters_displayAspectRatio,
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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "ElasticTranscoder",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "elastictranscoder",
      Prelude._svcSigningName = "elastictranscoder",
      Prelude._svcVersion = "2012-09-25",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "ElasticTranscoder",
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

-- | Prism for IncompatibleVersionException' errors.
_IncompatibleVersionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IncompatibleVersionException =
  Prelude._MatchServiceError
    defaultService
    "IncompatibleVersionException"
    Prelude.. Prelude.hasStatus 400

-- | Elastic Transcoder encountered an unexpected exception while trying to
-- fulfill the request.
_InternalServiceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServiceException =
  Prelude._MatchServiceError
    defaultService
    "InternalServiceException"

-- | General authentication failure. The request was not signed correctly.
_AccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Prelude.hasStatus 403

-- | One or more required parameter values were not provided in the request.
_ValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ValidationException =
  Prelude._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Prelude.hasStatus 400

-- | The resource you are attempting to change is in use. For example, you
-- are attempting to delete a pipeline that is currently in use.
_ResourceInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceInUseException =
  Prelude._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Prelude.hasStatus 409

-- | Too many operations for a given AWS account. For example, the number of
-- pipelines exceeds the maximum allowed.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Prelude.hasStatus 429

-- | The requested resource does not exist or is not available. For example,
-- the pipeline to which you\'re trying to add a job doesn\'t exist or is
-- still being created.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404
