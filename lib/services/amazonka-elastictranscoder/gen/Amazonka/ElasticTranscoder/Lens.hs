{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticTranscoder.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Lens
  ( -- * Operations

    -- ** CancelJob
    cancelJob_id,
    cancelJobResponse_httpStatus,

    -- ** CreateJob
    createJob_input,
    createJob_inputs,
    createJob_output,
    createJob_outputKeyPrefix,
    createJob_outputs,
    createJob_playlists,
    createJob_userMetadata,
    createJob_pipelineId,
    createJobResponse_job,
    createJobResponse_httpStatus,

    -- ** CreatePipeline
    createPipeline_awsKmsKeyArn,
    createPipeline_contentConfig,
    createPipeline_notifications,
    createPipeline_outputBucket,
    createPipeline_thumbnailConfig,
    createPipeline_name,
    createPipeline_inputBucket,
    createPipeline_role,
    createPipelineResponse_pipeline,
    createPipelineResponse_warnings,
    createPipelineResponse_httpStatus,

    -- ** CreatePreset
    createPreset_audio,
    createPreset_description,
    createPreset_thumbnails,
    createPreset_video,
    createPreset_name,
    createPreset_container,
    createPresetResponse_preset,
    createPresetResponse_warning,
    createPresetResponse_httpStatus,

    -- ** DeletePipeline
    deletePipeline_id,
    deletePipelineResponse_httpStatus,

    -- ** DeletePreset
    deletePreset_id,
    deletePresetResponse_httpStatus,

    -- ** ListJobsByPipeline
    listJobsByPipeline_ascending,
    listJobsByPipeline_pageToken,
    listJobsByPipeline_pipelineId,
    listJobsByPipelineResponse_jobs,
    listJobsByPipelineResponse_nextPageToken,
    listJobsByPipelineResponse_httpStatus,

    -- ** ListJobsByStatus
    listJobsByStatus_ascending,
    listJobsByStatus_pageToken,
    listJobsByStatus_status,
    listJobsByStatusResponse_jobs,
    listJobsByStatusResponse_nextPageToken,
    listJobsByStatusResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_ascending,
    listPipelines_pageToken,
    listPipelinesResponse_nextPageToken,
    listPipelinesResponse_pipelines,
    listPipelinesResponse_httpStatus,

    -- ** ListPresets
    listPresets_ascending,
    listPresets_pageToken,
    listPresetsResponse_nextPageToken,
    listPresetsResponse_presets,
    listPresetsResponse_httpStatus,

    -- ** ReadJob
    readJob_id,
    readJobResponse_httpStatus,
    readJobResponse_job,

    -- ** ReadPipeline
    readPipeline_id,
    readPipelineResponse_pipeline,
    readPipelineResponse_warnings,
    readPipelineResponse_httpStatus,

    -- ** ReadPreset
    readPreset_id,
    readPresetResponse_preset,
    readPresetResponse_httpStatus,

    -- ** UpdatePipeline
    updatePipeline_awsKmsKeyArn,
    updatePipeline_contentConfig,
    updatePipeline_inputBucket,
    updatePipeline_name,
    updatePipeline_notifications,
    updatePipeline_role,
    updatePipeline_thumbnailConfig,
    updatePipeline_id,
    updatePipelineResponse_pipeline,
    updatePipelineResponse_warnings,
    updatePipelineResponse_httpStatus,

    -- ** UpdatePipelineNotifications
    updatePipelineNotifications_id,
    updatePipelineNotifications_notifications,
    updatePipelineNotificationsResponse_pipeline,
    updatePipelineNotificationsResponse_httpStatus,

    -- ** UpdatePipelineStatus
    updatePipelineStatus_id,
    updatePipelineStatus_status,
    updatePipelineStatusResponse_pipeline,
    updatePipelineStatusResponse_httpStatus,

    -- * Types

    -- ** Artwork
    artwork_albumArtFormat,
    artwork_encryption,
    artwork_inputKey,
    artwork_maxHeight,
    artwork_maxWidth,
    artwork_paddingPolicy,
    artwork_sizingPolicy,

    -- ** AudioCodecOptions
    audioCodecOptions_bitDepth,
    audioCodecOptions_bitOrder,
    audioCodecOptions_profile,
    audioCodecOptions_signed,

    -- ** AudioParameters
    audioParameters_audioPackingMode,
    audioParameters_bitRate,
    audioParameters_channels,
    audioParameters_codec,
    audioParameters_codecOptions,
    audioParameters_sampleRate,

    -- ** CaptionFormat
    captionFormat_encryption,
    captionFormat_format,
    captionFormat_pattern,

    -- ** CaptionSource
    captionSource_encryption,
    captionSource_key,
    captionSource_label,
    captionSource_language,
    captionSource_timeOffset,

    -- ** Captions
    captions_captionFormats,
    captions_captionSources,
    captions_mergePolicy,

    -- ** Clip
    clip_timeSpan,

    -- ** CreateJobOutput
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

    -- ** CreateJobPlaylist
    createJobPlaylist_format,
    createJobPlaylist_hlsContentProtection,
    createJobPlaylist_name,
    createJobPlaylist_outputKeys,
    createJobPlaylist_playReadyDrm,

    -- ** DetectedProperties
    detectedProperties_durationMillis,
    detectedProperties_fileSize,
    detectedProperties_frameRate,
    detectedProperties_height,
    detectedProperties_width,

    -- ** Encryption
    encryption_initializationVector,
    encryption_key,
    encryption_keyMd5,
    encryption_mode,

    -- ** HlsContentProtection
    hlsContentProtection_initializationVector,
    hlsContentProtection_key,
    hlsContentProtection_keyMd5,
    hlsContentProtection_keyStoragePolicy,
    hlsContentProtection_licenseAcquisitionUrl,
    hlsContentProtection_method,

    -- ** InputCaptions
    inputCaptions_captionSources,
    inputCaptions_mergePolicy,

    -- ** Job
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

    -- ** JobAlbumArt
    jobAlbumArt_artwork,
    jobAlbumArt_mergePolicy,

    -- ** JobInput
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

    -- ** JobOutput
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

    -- ** JobWatermark
    jobWatermark_encryption,
    jobWatermark_inputKey,
    jobWatermark_presetWatermarkId,

    -- ** Notifications
    notifications_completed,
    notifications_error,
    notifications_progressing,
    notifications_warning,

    -- ** Permission
    permission_access,
    permission_grantee,
    permission_granteeType,

    -- ** Pipeline
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

    -- ** PipelineOutputConfig
    pipelineOutputConfig_bucket,
    pipelineOutputConfig_permissions,
    pipelineOutputConfig_storageClass,

    -- ** PlayReadyDrm
    playReadyDrm_format,
    playReadyDrm_initializationVector,
    playReadyDrm_key,
    playReadyDrm_keyId,
    playReadyDrm_keyMd5,
    playReadyDrm_licenseAcquisitionUrl,

    -- ** Playlist
    playlist_format,
    playlist_hlsContentProtection,
    playlist_name,
    playlist_outputKeys,
    playlist_playReadyDrm,
    playlist_status,
    playlist_statusDetail,

    -- ** Preset
    preset_arn,
    preset_audio,
    preset_container,
    preset_description,
    preset_id,
    preset_name,
    preset_thumbnails,
    preset_type,
    preset_video,

    -- ** PresetWatermark
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

    -- ** Thumbnails
    thumbnails_aspectRatio,
    thumbnails_format,
    thumbnails_interval,
    thumbnails_maxHeight,
    thumbnails_maxWidth,
    thumbnails_paddingPolicy,
    thumbnails_resolution,
    thumbnails_sizingPolicy,

    -- ** TimeSpan
    timeSpan_duration,
    timeSpan_startTime,

    -- ** Timing
    timing_finishTimeMillis,
    timing_startTimeMillis,
    timing_submitTimeMillis,

    -- ** VideoParameters
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

    -- ** Warning
    warning_code,
    warning_message,
  )
where

import Amazonka.ElasticTranscoder.CancelJob
import Amazonka.ElasticTranscoder.CreateJob
import Amazonka.ElasticTranscoder.CreatePipeline
import Amazonka.ElasticTranscoder.CreatePreset
import Amazonka.ElasticTranscoder.DeletePipeline
import Amazonka.ElasticTranscoder.DeletePreset
import Amazonka.ElasticTranscoder.ListJobsByPipeline
import Amazonka.ElasticTranscoder.ListJobsByStatus
import Amazonka.ElasticTranscoder.ListPipelines
import Amazonka.ElasticTranscoder.ListPresets
import Amazonka.ElasticTranscoder.ReadJob
import Amazonka.ElasticTranscoder.ReadPipeline
import Amazonka.ElasticTranscoder.ReadPreset
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
import Amazonka.ElasticTranscoder.UpdatePipeline
import Amazonka.ElasticTranscoder.UpdatePipelineNotifications
import Amazonka.ElasticTranscoder.UpdatePipelineStatus
