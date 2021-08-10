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

    -- ** ListJobsByPipeline
    listJobsByPipeline_ascending,
    listJobsByPipeline_pageToken,
    listJobsByPipeline_pipelineId,
    listJobsByPipelineResponse_nextPageToken,
    listJobsByPipelineResponse_jobs,
    listJobsByPipelineResponse_httpStatus,

    -- ** UpdatePipelineStatus
    updatePipelineStatus_id,
    updatePipelineStatus_status,
    updatePipelineStatusResponse_pipeline,
    updatePipelineStatusResponse_httpStatus,

    -- ** ListPresets
    listPresets_ascending,
    listPresets_pageToken,
    listPresetsResponse_presets,
    listPresetsResponse_nextPageToken,
    listPresetsResponse_httpStatus,

    -- ** DeletePreset
    deletePreset_id,
    deletePresetResponse_httpStatus,

    -- ** CancelJob
    cancelJob_id,
    cancelJobResponse_httpStatus,

    -- ** CreatePreset
    createPreset_thumbnails,
    createPreset_video,
    createPreset_audio,
    createPreset_description,
    createPreset_name,
    createPreset_container,
    createPresetResponse_preset,
    createPresetResponse_warning,
    createPresetResponse_httpStatus,

    -- ** CreatePipeline
    createPipeline_outputBucket,
    createPipeline_notifications,
    createPipeline_thumbnailConfig,
    createPipeline_contentConfig,
    createPipeline_awsKmsKeyArn,
    createPipeline_name,
    createPipeline_inputBucket,
    createPipeline_role,
    createPipelineResponse_warnings,
    createPipelineResponse_pipeline,
    createPipelineResponse_httpStatus,

    -- ** UpdatePipeline
    updatePipeline_name,
    updatePipeline_role,
    updatePipeline_notifications,
    updatePipeline_thumbnailConfig,
    updatePipeline_contentConfig,
    updatePipeline_inputBucket,
    updatePipeline_awsKmsKeyArn,
    updatePipeline_id,
    updatePipelineResponse_warnings,
    updatePipelineResponse_pipeline,
    updatePipelineResponse_httpStatus,

    -- ** DeletePipeline
    deletePipeline_id,
    deletePipelineResponse_httpStatus,

    -- ** ReadPreset
    readPreset_id,
    readPresetResponse_preset,
    readPresetResponse_httpStatus,

    -- ** ListJobsByStatus
    listJobsByStatus_ascending,
    listJobsByStatus_pageToken,
    listJobsByStatus_status,
    listJobsByStatusResponse_nextPageToken,
    listJobsByStatusResponse_jobs,
    listJobsByStatusResponse_httpStatus,

    -- ** CreateJob
    createJob_outputs,
    createJob_input,
    createJob_outputKeyPrefix,
    createJob_output,
    createJob_userMetadata,
    createJob_inputs,
    createJob_playlists,
    createJob_pipelineId,
    createJobResponse_job,
    createJobResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_ascending,
    listPipelines_pageToken,
    listPipelinesResponse_nextPageToken,
    listPipelinesResponse_pipelines,
    listPipelinesResponse_httpStatus,

    -- ** ReadPipeline
    readPipeline_id,
    readPipelineResponse_warnings,
    readPipelineResponse_pipeline,
    readPipelineResponse_httpStatus,

    -- ** UpdatePipelineNotifications
    updatePipelineNotifications_id,
    updatePipelineNotifications_notifications,
    updatePipelineNotificationsResponse_pipeline,
    updatePipelineNotificationsResponse_httpStatus,

    -- ** ReadJob
    readJob_id,
    readJobResponse_httpStatus,
    readJobResponse_job,

    -- * Types

    -- ** Artwork
    artwork_inputKey,
    artwork_albumArtFormat,
    artwork_sizingPolicy,
    artwork_encryption,
    artwork_paddingPolicy,
    artwork_maxHeight,
    artwork_maxWidth,

    -- ** AudioCodecOptions
    audioCodecOptions_bitDepth,
    audioCodecOptions_signed,
    audioCodecOptions_bitOrder,
    audioCodecOptions_profile,

    -- ** AudioParameters
    audioParameters_codecOptions,
    audioParameters_codec,
    audioParameters_channels,
    audioParameters_sampleRate,
    audioParameters_audioPackingMode,
    audioParameters_bitRate,

    -- ** CaptionFormat
    captionFormat_format,
    captionFormat_encryption,
    captionFormat_pattern,

    -- ** CaptionSource
    captionSource_key,
    captionSource_timeOffset,
    captionSource_encryption,
    captionSource_label,
    captionSource_language,

    -- ** Captions
    captions_captionSources,
    captions_captionFormats,
    captions_mergePolicy,

    -- ** Clip
    clip_timeSpan,

    -- ** CreateJobOutput
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

    -- ** CreateJobPlaylist
    createJobPlaylist_playReadyDrm,
    createJobPlaylist_outputKeys,
    createJobPlaylist_format,
    createJobPlaylist_hlsContentProtection,
    createJobPlaylist_name,

    -- ** DetectedProperties
    detectedProperties_height,
    detectedProperties_width,
    detectedProperties_fileSize,
    detectedProperties_frameRate,
    detectedProperties_durationMillis,

    -- ** Encryption
    encryption_key,
    encryption_mode,
    encryption_keyMd5,
    encryption_initializationVector,

    -- ** HlsContentProtection
    hlsContentProtection_key,
    hlsContentProtection_licenseAcquisitionUrl,
    hlsContentProtection_keyMd5,
    hlsContentProtection_method,
    hlsContentProtection_initializationVector,
    hlsContentProtection_keyStoragePolicy,

    -- ** InputCaptions
    inputCaptions_captionSources,
    inputCaptions_mergePolicy,

    -- ** Job'
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

    -- ** JobAlbumArt
    jobAlbumArt_artwork,
    jobAlbumArt_mergePolicy,

    -- ** JobInput
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

    -- ** JobOutput
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

    -- ** JobWatermark
    jobWatermark_inputKey,
    jobWatermark_encryption,
    jobWatermark_presetWatermarkId,

    -- ** Notifications
    notifications_warning,
    notifications_error,
    notifications_progressing,
    notifications_completed,

    -- ** Permission
    permission_access,
    permission_granteeType,
    permission_grantee,

    -- ** Pipeline
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

    -- ** PipelineOutputConfig
    pipelineOutputConfig_permissions,
    pipelineOutputConfig_storageClass,
    pipelineOutputConfig_bucket,

    -- ** PlayReadyDrm
    playReadyDrm_key,
    playReadyDrm_licenseAcquisitionUrl,
    playReadyDrm_keyMd5,
    playReadyDrm_format,
    playReadyDrm_initializationVector,
    playReadyDrm_keyId,

    -- ** Playlist
    playlist_status,
    playlist_playReadyDrm,
    playlist_outputKeys,
    playlist_format,
    playlist_hlsContentProtection,
    playlist_statusDetail,
    playlist_name,

    -- ** Preset
    preset_container,
    preset_arn,
    preset_id,
    preset_thumbnails,
    preset_name,
    preset_video,
    preset_audio,
    preset_description,
    preset_type,

    -- ** PresetWatermark
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

    -- ** Thumbnails
    thumbnails_format,
    thumbnails_sizingPolicy,
    thumbnails_interval,
    thumbnails_aspectRatio,
    thumbnails_paddingPolicy,
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

    -- ** Warning
    warning_message,
    warning_code,
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
import Network.AWS.ElasticTranscoder.UpdatePipeline
import Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
import Network.AWS.ElasticTranscoder.UpdatePipelineStatus
