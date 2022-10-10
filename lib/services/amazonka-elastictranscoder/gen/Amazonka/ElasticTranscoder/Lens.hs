{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticTranscoder.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Lens
  ( -- * Operations

    -- ** CancelJob
    cancelJob_id,
    cancelJobResponse_httpStatus,

    -- ** CreateJob
    createJob_input,
    createJob_outputs,
    createJob_output,
    createJob_playlists,
    createJob_outputKeyPrefix,
    createJob_inputs,
    createJob_userMetadata,
    createJob_pipelineId,
    createJobResponse_job,
    createJobResponse_httpStatus,

    -- ** CreatePipeline
    createPipeline_notifications,
    createPipeline_thumbnailConfig,
    createPipeline_awsKmsKeyArn,
    createPipeline_outputBucket,
    createPipeline_contentConfig,
    createPipeline_name,
    createPipeline_inputBucket,
    createPipeline_role,
    createPipelineResponse_warnings,
    createPipelineResponse_pipeline,
    createPipelineResponse_httpStatus,

    -- ** CreatePreset
    createPreset_audio,
    createPreset_description,
    createPreset_thumbnails,
    createPreset_video,
    createPreset_name,
    createPreset_container,
    createPresetResponse_warning,
    createPresetResponse_preset,
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
    listJobsByPipelineResponse_nextPageToken,
    listJobsByPipelineResponse_jobs,
    listJobsByPipelineResponse_httpStatus,

    -- ** ListJobsByStatus
    listJobsByStatus_ascending,
    listJobsByStatus_pageToken,
    listJobsByStatus_status,
    listJobsByStatusResponse_nextPageToken,
    listJobsByStatusResponse_jobs,
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
    listPresetsResponse_presets,
    listPresetsResponse_nextPageToken,
    listPresetsResponse_httpStatus,

    -- ** ReadJob
    readJob_id,
    readJobResponse_httpStatus,
    readJobResponse_job,

    -- ** ReadPipeline
    readPipeline_id,
    readPipelineResponse_warnings,
    readPipelineResponse_pipeline,
    readPipelineResponse_httpStatus,

    -- ** ReadPreset
    readPreset_id,
    readPresetResponse_preset,
    readPresetResponse_httpStatus,

    -- ** UpdatePipeline
    updatePipeline_notifications,
    updatePipeline_thumbnailConfig,
    updatePipeline_name,
    updatePipeline_inputBucket,
    updatePipeline_awsKmsKeyArn,
    updatePipeline_role,
    updatePipeline_contentConfig,
    updatePipeline_id,
    updatePipelineResponse_warnings,
    updatePipelineResponse_pipeline,
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
    artwork_sizingPolicy,
    artwork_paddingPolicy,
    artwork_inputKey,
    artwork_encryption,
    artwork_maxHeight,
    artwork_maxWidth,

    -- ** AudioCodecOptions
    audioCodecOptions_bitDepth,
    audioCodecOptions_profile,
    audioCodecOptions_bitOrder,
    audioCodecOptions_signed,

    -- ** AudioParameters
    audioParameters_audioPackingMode,
    audioParameters_codecOptions,
    audioParameters_channels,
    audioParameters_bitRate,
    audioParameters_sampleRate,
    audioParameters_codec,

    -- ** CaptionFormat
    captionFormat_format,
    captionFormat_pattern,
    captionFormat_encryption,

    -- ** CaptionSource
    captionSource_key,
    captionSource_label,
    captionSource_timeOffset,
    captionSource_encryption,
    captionSource_language,

    -- ** Captions
    captions_captionSources,
    captions_mergePolicy,
    captions_captionFormats,

    -- ** Clip
    clip_timeSpan,

    -- ** CreateJobOutput
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

    -- ** CreateJobPlaylist
    createJobPlaylist_name,
    createJobPlaylist_hlsContentProtection,
    createJobPlaylist_playReadyDrm,
    createJobPlaylist_format,
    createJobPlaylist_outputKeys,

    -- ** DetectedProperties
    detectedProperties_fileSize,
    detectedProperties_width,
    detectedProperties_durationMillis,
    detectedProperties_height,
    detectedProperties_frameRate,

    -- ** Encryption
    encryption_key,
    encryption_initializationVector,
    encryption_mode,
    encryption_keyMd5,

    -- ** HlsContentProtection
    hlsContentProtection_keyStoragePolicy,
    hlsContentProtection_key,
    hlsContentProtection_method,
    hlsContentProtection_licenseAcquisitionUrl,
    hlsContentProtection_initializationVector,
    hlsContentProtection_keyMd5,

    -- ** InputCaptions
    inputCaptions_captionSources,
    inputCaptions_mergePolicy,

    -- ** Job
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

    -- ** JobAlbumArt
    jobAlbumArt_mergePolicy,
    jobAlbumArt_artwork,

    -- ** JobInput
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

    -- ** JobOutput
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

    -- ** JobWatermark
    jobWatermark_inputKey,
    jobWatermark_encryption,
    jobWatermark_presetWatermarkId,

    -- ** Notifications
    notifications_warning,
    notifications_completed,
    notifications_error,
    notifications_progressing,

    -- ** Permission
    permission_granteeType,
    permission_access,
    permission_grantee,

    -- ** Pipeline
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

    -- ** PipelineOutputConfig
    pipelineOutputConfig_bucket,
    pipelineOutputConfig_permissions,
    pipelineOutputConfig_storageClass,

    -- ** PlayReadyDrm
    playReadyDrm_key,
    playReadyDrm_licenseAcquisitionUrl,
    playReadyDrm_format,
    playReadyDrm_initializationVector,
    playReadyDrm_keyMd5,
    playReadyDrm_keyId,

    -- ** Playlist
    playlist_name,
    playlist_hlsContentProtection,
    playlist_playReadyDrm,
    playlist_format,
    playlist_statusDetail,
    playlist_status,
    playlist_outputKeys,

    -- ** Preset
    preset_name,
    preset_type,
    preset_audio,
    preset_arn,
    preset_id,
    preset_description,
    preset_container,
    preset_thumbnails,
    preset_video,

    -- ** PresetWatermark
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

    -- ** Thumbnails
    thumbnails_sizingPolicy,
    thumbnails_paddingPolicy,
    thumbnails_interval,
    thumbnails_aspectRatio,
    thumbnails_format,
    thumbnails_resolution,
    thumbnails_maxHeight,
    thumbnails_maxWidth,

    -- ** TimeSpan
    timeSpan_duration,
    timeSpan_startTime,

    -- ** Timing
    timing_submitTimeMillis,
    timing_startTimeMillis,
    timing_finishTimeMillis,

    -- ** VideoParameters
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

    -- ** Warning
    warning_message,
    warning_code,
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
