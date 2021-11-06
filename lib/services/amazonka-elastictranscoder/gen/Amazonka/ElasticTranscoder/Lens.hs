{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticTranscoder.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Lens
  ( -- * Operations

    -- ** DeletePreset
    deletePreset_id,
    deletePresetResponse_httpStatus,

    -- ** UpdatePipelineStatus
    updatePipelineStatus_id,
    updatePipelineStatus_status,
    updatePipelineStatusResponse_pipeline,
    updatePipelineStatusResponse_httpStatus,

    -- ** ListJobsByPipeline
    listJobsByPipeline_ascending,
    listJobsByPipeline_pageToken,
    listJobsByPipeline_pipelineId,
    listJobsByPipelineResponse_nextPageToken,
    listJobsByPipelineResponse_jobs,
    listJobsByPipelineResponse_httpStatus,

    -- ** UpdatePipeline
    updatePipeline_inputBucket,
    updatePipeline_contentConfig,
    updatePipeline_role,
    updatePipeline_name,
    updatePipeline_awsKmsKeyArn,
    updatePipeline_notifications,
    updatePipeline_thumbnailConfig,
    updatePipeline_id,
    updatePipelineResponse_warnings,
    updatePipelineResponse_pipeline,
    updatePipelineResponse_httpStatus,

    -- ** DeletePipeline
    deletePipeline_id,
    deletePipelineResponse_httpStatus,

    -- ** CreateJob
    createJob_inputs,
    createJob_input,
    createJob_userMetadata,
    createJob_outputs,
    createJob_output,
    createJob_playlists,
    createJob_outputKeyPrefix,
    createJob_pipelineId,
    createJobResponse_job,
    createJobResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_ascending,
    listPipelines_pageToken,
    listPipelinesResponse_nextPageToken,
    listPipelinesResponse_pipelines,
    listPipelinesResponse_httpStatus,

    -- ** CreatePreset
    createPreset_video,
    createPreset_thumbnails,
    createPreset_description,
    createPreset_audio,
    createPreset_name,
    createPreset_container,
    createPresetResponse_warning,
    createPresetResponse_preset,
    createPresetResponse_httpStatus,

    -- ** ListPresets
    listPresets_ascending,
    listPresets_pageToken,
    listPresetsResponse_nextPageToken,
    listPresetsResponse_presets,
    listPresetsResponse_httpStatus,

    -- ** ReadPreset
    readPreset_id,
    readPresetResponse_preset,
    readPresetResponse_httpStatus,

    -- ** ReadJob
    readJob_id,
    readJobResponse_httpStatus,
    readJobResponse_job,

    -- ** UpdatePipelineNotifications
    updatePipelineNotifications_id,
    updatePipelineNotifications_notifications,
    updatePipelineNotificationsResponse_pipeline,
    updatePipelineNotificationsResponse_httpStatus,

    -- ** ReadPipeline
    readPipeline_id,
    readPipelineResponse_warnings,
    readPipelineResponse_pipeline,
    readPipelineResponse_httpStatus,

    -- ** CreatePipeline
    createPipeline_contentConfig,
    createPipeline_outputBucket,
    createPipeline_awsKmsKeyArn,
    createPipeline_notifications,
    createPipeline_thumbnailConfig,
    createPipeline_name,
    createPipeline_inputBucket,
    createPipeline_role,
    createPipelineResponse_warnings,
    createPipelineResponse_pipeline,
    createPipelineResponse_httpStatus,

    -- ** ListJobsByStatus
    listJobsByStatus_ascending,
    listJobsByStatus_pageToken,
    listJobsByStatus_status,
    listJobsByStatusResponse_nextPageToken,
    listJobsByStatusResponse_jobs,
    listJobsByStatusResponse_httpStatus,

    -- ** CancelJob
    cancelJob_id,
    cancelJobResponse_httpStatus,

    -- * Types

    -- ** Artwork
    artwork_sizingPolicy,
    artwork_albumArtFormat,
    artwork_maxHeight,
    artwork_inputKey,
    artwork_paddingPolicy,
    artwork_encryption,
    artwork_maxWidth,

    -- ** AudioCodecOptions
    audioCodecOptions_signed,
    audioCodecOptions_bitDepth,
    audioCodecOptions_profile,
    audioCodecOptions_bitOrder,

    -- ** AudioParameters
    audioParameters_channels,
    audioParameters_codec,
    audioParameters_audioPackingMode,
    audioParameters_sampleRate,
    audioParameters_bitRate,
    audioParameters_codecOptions,

    -- ** CaptionFormat
    captionFormat_pattern,
    captionFormat_format,
    captionFormat_encryption,

    -- ** CaptionSource
    captionSource_timeOffset,
    captionSource_encryption,
    captionSource_key,
    captionSource_language,
    captionSource_label,

    -- ** Captions
    captions_mergePolicy,
    captions_captionSources,
    captions_captionFormats,

    -- ** Clip
    clip_timeSpan,

    -- ** CreateJobOutput
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

    -- ** CreateJobPlaylist
    createJobPlaylist_playReadyDrm,
    createJobPlaylist_format,
    createJobPlaylist_outputKeys,
    createJobPlaylist_name,
    createJobPlaylist_hlsContentProtection,

    -- ** DetectedProperties
    detectedProperties_height,
    detectedProperties_frameRate,
    detectedProperties_fileSize,
    detectedProperties_width,
    detectedProperties_durationMillis,

    -- ** Encryption
    encryption_mode,
    encryption_keyMd5,
    encryption_key,
    encryption_initializationVector,

    -- ** HlsContentProtection
    hlsContentProtection_keyMd5,
    hlsContentProtection_keyStoragePolicy,
    hlsContentProtection_key,
    hlsContentProtection_method,
    hlsContentProtection_initializationVector,
    hlsContentProtection_licenseAcquisitionUrl,

    -- ** InputCaptions
    inputCaptions_mergePolicy,
    inputCaptions_captionSources,

    -- ** Job
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

    -- ** JobAlbumArt
    jobAlbumArt_mergePolicy,
    jobAlbumArt_artwork,

    -- ** JobInput
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

    -- ** JobOutput
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

    -- ** JobWatermark
    jobWatermark_presetWatermarkId,
    jobWatermark_inputKey,
    jobWatermark_encryption,

    -- ** Notifications
    notifications_error,
    notifications_warning,
    notifications_progressing,
    notifications_completed,

    -- ** Permission
    permission_access,
    permission_granteeType,
    permission_grantee,

    -- ** Pipeline
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

    -- ** PipelineOutputConfig
    pipelineOutputConfig_bucket,
    pipelineOutputConfig_storageClass,
    pipelineOutputConfig_permissions,

    -- ** PlayReadyDrm
    playReadyDrm_keyId,
    playReadyDrm_format,
    playReadyDrm_keyMd5,
    playReadyDrm_key,
    playReadyDrm_initializationVector,
    playReadyDrm_licenseAcquisitionUrl,

    -- ** Playlist
    playlist_status,
    playlist_playReadyDrm,
    playlist_format,
    playlist_outputKeys,
    playlist_name,
    playlist_statusDetail,
    playlist_hlsContentProtection,

    -- ** Preset
    preset_arn,
    preset_video,
    preset_thumbnails,
    preset_name,
    preset_container,
    preset_id,
    preset_type,
    preset_description,
    preset_audio,

    -- ** PresetWatermark
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

    -- ** Thumbnails
    thumbnails_sizingPolicy,
    thumbnails_format,
    thumbnails_maxHeight,
    thumbnails_resolution,
    thumbnails_aspectRatio,
    thumbnails_paddingPolicy,
    thumbnails_interval,
    thumbnails_maxWidth,

    -- ** TimeSpan
    timeSpan_startTime,
    timeSpan_duration,

    -- ** Timing
    timing_submitTimeMillis,
    timing_finishTimeMillis,
    timing_startTimeMillis,

    -- ** VideoParameters
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
