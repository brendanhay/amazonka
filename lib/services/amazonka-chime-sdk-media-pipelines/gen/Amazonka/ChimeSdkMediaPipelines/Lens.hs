{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSdkMediaPipelines.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Lens
  ( -- * Operations

    -- ** CreateMediaCapturePipeline
    createMediaCapturePipeline_chimeSdkMeetingConfiguration,
    createMediaCapturePipeline_clientRequestToken,
    createMediaCapturePipeline_tags,
    createMediaCapturePipeline_sourceType,
    createMediaCapturePipeline_sourceArn,
    createMediaCapturePipeline_sinkType,
    createMediaCapturePipeline_sinkArn,
    createMediaCapturePipelineResponse_mediaCapturePipeline,
    createMediaCapturePipelineResponse_httpStatus,

    -- ** CreateMediaConcatenationPipeline
    createMediaConcatenationPipeline_clientRequestToken,
    createMediaConcatenationPipeline_tags,
    createMediaConcatenationPipeline_sources,
    createMediaConcatenationPipeline_sinks,
    createMediaConcatenationPipelineResponse_mediaConcatenationPipeline,
    createMediaConcatenationPipelineResponse_httpStatus,

    -- ** CreateMediaLiveConnectorPipeline
    createMediaLiveConnectorPipeline_clientRequestToken,
    createMediaLiveConnectorPipeline_tags,
    createMediaLiveConnectorPipeline_sources,
    createMediaLiveConnectorPipeline_sinks,
    createMediaLiveConnectorPipelineResponse_mediaLiveConnectorPipeline,
    createMediaLiveConnectorPipelineResponse_httpStatus,

    -- ** DeleteMediaCapturePipeline
    deleteMediaCapturePipeline_mediaPipelineId,

    -- ** DeleteMediaPipeline
    deleteMediaPipeline_mediaPipelineId,

    -- ** GetMediaCapturePipeline
    getMediaCapturePipeline_mediaPipelineId,
    getMediaCapturePipelineResponse_mediaCapturePipeline,
    getMediaCapturePipelineResponse_httpStatus,

    -- ** GetMediaPipeline
    getMediaPipeline_mediaPipelineId,
    getMediaPipelineResponse_mediaPipeline,
    getMediaPipelineResponse_httpStatus,

    -- ** ListMediaCapturePipelines
    listMediaCapturePipelines_maxResults,
    listMediaCapturePipelines_nextToken,
    listMediaCapturePipelinesResponse_mediaCapturePipelines,
    listMediaCapturePipelinesResponse_nextToken,
    listMediaCapturePipelinesResponse_httpStatus,

    -- ** ListMediaPipelines
    listMediaPipelines_maxResults,
    listMediaPipelines_nextToken,
    listMediaPipelinesResponse_mediaPipelines,
    listMediaPipelinesResponse_nextToken,
    listMediaPipelinesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** ArtifactsConcatenationConfiguration
    artifactsConcatenationConfiguration_audio,
    artifactsConcatenationConfiguration_video,
    artifactsConcatenationConfiguration_content,
    artifactsConcatenationConfiguration_dataChannel,
    artifactsConcatenationConfiguration_transcriptionMessages,
    artifactsConcatenationConfiguration_meetingEvents,
    artifactsConcatenationConfiguration_compositedVideo,

    -- ** ArtifactsConfiguration
    artifactsConfiguration_compositedVideo,
    artifactsConfiguration_audio,
    artifactsConfiguration_video,
    artifactsConfiguration_content,

    -- ** AudioArtifactsConfiguration
    audioArtifactsConfiguration_muxType,

    -- ** AudioConcatenationConfiguration
    audioConcatenationConfiguration_state,

    -- ** ChimeSdkMeetingConcatenationConfiguration
    chimeSdkMeetingConcatenationConfiguration_artifactsConfiguration,

    -- ** ChimeSdkMeetingConfiguration
    chimeSdkMeetingConfiguration_artifactsConfiguration,
    chimeSdkMeetingConfiguration_sourceConfiguration,

    -- ** ChimeSdkMeetingLiveConnectorConfiguration
    chimeSdkMeetingLiveConnectorConfiguration_compositedVideo,
    chimeSdkMeetingLiveConnectorConfiguration_sourceConfiguration,
    chimeSdkMeetingLiveConnectorConfiguration_arn,
    chimeSdkMeetingLiveConnectorConfiguration_muxType,

    -- ** CompositedVideoArtifactsConfiguration
    compositedVideoArtifactsConfiguration_layout,
    compositedVideoArtifactsConfiguration_resolution,
    compositedVideoArtifactsConfiguration_gridViewConfiguration,

    -- ** CompositedVideoConcatenationConfiguration
    compositedVideoConcatenationConfiguration_state,

    -- ** ConcatenationSink
    concatenationSink_type,
    concatenationSink_s3BucketSinkConfiguration,

    -- ** ConcatenationSource
    concatenationSource_type,
    concatenationSource_mediaCapturePipelineSourceConfiguration,

    -- ** ContentArtifactsConfiguration
    contentArtifactsConfiguration_muxType,
    contentArtifactsConfiguration_state,

    -- ** ContentConcatenationConfiguration
    contentConcatenationConfiguration_state,

    -- ** DataChannelConcatenationConfiguration
    dataChannelConcatenationConfiguration_state,

    -- ** GridViewConfiguration
    gridViewConfiguration_presenterOnlyConfiguration,
    gridViewConfiguration_contentShareLayout,

    -- ** LiveConnectorRTMPConfiguration
    liveConnectorRTMPConfiguration_audioChannels,
    liveConnectorRTMPConfiguration_audioSampleRate,
    liveConnectorRTMPConfiguration_url,

    -- ** LiveConnectorSinkConfiguration
    liveConnectorSinkConfiguration_sinkType,
    liveConnectorSinkConfiguration_rTMPConfiguration,

    -- ** LiveConnectorSourceConfiguration
    liveConnectorSourceConfiguration_sourceType,
    liveConnectorSourceConfiguration_chimeSdkMeetingLiveConnectorConfiguration,

    -- ** MediaCapturePipeline
    mediaCapturePipeline_chimeSdkMeetingConfiguration,
    mediaCapturePipeline_createdTimestamp,
    mediaCapturePipeline_mediaPipelineArn,
    mediaCapturePipeline_mediaPipelineId,
    mediaCapturePipeline_sinkArn,
    mediaCapturePipeline_sinkType,
    mediaCapturePipeline_sourceArn,
    mediaCapturePipeline_sourceType,
    mediaCapturePipeline_status,
    mediaCapturePipeline_updatedTimestamp,

    -- ** MediaCapturePipelineSourceConfiguration
    mediaCapturePipelineSourceConfiguration_mediaPipelineArn,
    mediaCapturePipelineSourceConfiguration_chimeSdkMeetingConfiguration,

    -- ** MediaCapturePipelineSummary
    mediaCapturePipelineSummary_mediaPipelineArn,
    mediaCapturePipelineSummary_mediaPipelineId,

    -- ** MediaConcatenationPipeline
    mediaConcatenationPipeline_createdTimestamp,
    mediaConcatenationPipeline_mediaPipelineArn,
    mediaConcatenationPipeline_mediaPipelineId,
    mediaConcatenationPipeline_sinks,
    mediaConcatenationPipeline_sources,
    mediaConcatenationPipeline_status,
    mediaConcatenationPipeline_updatedTimestamp,

    -- ** MediaLiveConnectorPipeline
    mediaLiveConnectorPipeline_createdTimestamp,
    mediaLiveConnectorPipeline_mediaPipelineArn,
    mediaLiveConnectorPipeline_mediaPipelineId,
    mediaLiveConnectorPipeline_sinks,
    mediaLiveConnectorPipeline_sources,
    mediaLiveConnectorPipeline_status,
    mediaLiveConnectorPipeline_updatedTimestamp,

    -- ** MediaPipeline
    mediaPipeline_mediaCapturePipeline,
    mediaPipeline_mediaConcatenationPipeline,
    mediaPipeline_mediaLiveConnectorPipeline,

    -- ** MediaPipelineSummary
    mediaPipelineSummary_mediaPipelineArn,
    mediaPipelineSummary_mediaPipelineId,

    -- ** MeetingEventsConcatenationConfiguration
    meetingEventsConcatenationConfiguration_state,

    -- ** PresenterOnlyConfiguration
    presenterOnlyConfiguration_presenterPosition,

    -- ** S3BucketSinkConfiguration
    s3BucketSinkConfiguration_destination,

    -- ** SelectedVideoStreams
    selectedVideoStreams_attendeeIds,
    selectedVideoStreams_externalUserIds,

    -- ** SourceConfiguration
    sourceConfiguration_selectedVideoStreams,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TranscriptionMessagesConcatenationConfiguration
    transcriptionMessagesConcatenationConfiguration_state,

    -- ** VideoArtifactsConfiguration
    videoArtifactsConfiguration_muxType,
    videoArtifactsConfiguration_state,

    -- ** VideoConcatenationConfiguration
    videoConcatenationConfiguration_state,
  )
where

import Amazonka.ChimeSdkMediaPipelines.CreateMediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.CreateMediaConcatenationPipeline
import Amazonka.ChimeSdkMediaPipelines.CreateMediaLiveConnectorPipeline
import Amazonka.ChimeSdkMediaPipelines.DeleteMediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.DeleteMediaPipeline
import Amazonka.ChimeSdkMediaPipelines.GetMediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.GetMediaPipeline
import Amazonka.ChimeSdkMediaPipelines.ListMediaCapturePipelines
import Amazonka.ChimeSdkMediaPipelines.ListMediaPipelines
import Amazonka.ChimeSdkMediaPipelines.ListTagsForResource
import Amazonka.ChimeSdkMediaPipelines.TagResource
import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.AudioArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.AudioConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingLiveConnectorConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.CompositedVideoArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.CompositedVideoConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSink
import Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSource
import Amazonka.ChimeSdkMediaPipelines.Types.ContentArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ContentConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.DataChannelConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.GridViewConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorRTMPConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSourceConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipelineSourceConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipelineSummary
import Amazonka.ChimeSdkMediaPipelines.Types.MediaConcatenationPipeline
import Amazonka.ChimeSdkMediaPipelines.Types.MediaLiveConnectorPipeline
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipeline
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineSummary
import Amazonka.ChimeSdkMediaPipelines.Types.MeetingEventsConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.PresenterOnlyConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.S3BucketSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.SelectedVideoStreams
import Amazonka.ChimeSdkMediaPipelines.Types.SourceConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.Tag
import Amazonka.ChimeSdkMediaPipelines.Types.TranscriptionMessagesConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.VideoArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.VideoConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.UntagResource
