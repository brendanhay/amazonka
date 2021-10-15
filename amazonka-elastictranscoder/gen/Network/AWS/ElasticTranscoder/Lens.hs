{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Lens
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

import Network.AWS.ElasticTranscoder.CancelJob
import Network.AWS.ElasticTranscoder.CreateJob
import Network.AWS.ElasticTranscoder.CreatePipeline
import Network.AWS.ElasticTranscoder.CreatePreset
import Network.AWS.ElasticTranscoder.DeletePipeline
import Network.AWS.ElasticTranscoder.DeletePreset
import Network.AWS.ElasticTranscoder.ListJobsByPipeline
import Network.AWS.ElasticTranscoder.ListJobsByStatus
import Network.AWS.ElasticTranscoder.ListPipelines
import Network.AWS.ElasticTranscoder.ListPresets
import Network.AWS.ElasticTranscoder.ReadJob
import Network.AWS.ElasticTranscoder.ReadPipeline
import Network.AWS.ElasticTranscoder.ReadPreset
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
import Network.AWS.ElasticTranscoder.UpdatePipeline
import Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
import Network.AWS.ElasticTranscoder.UpdatePipelineStatus
