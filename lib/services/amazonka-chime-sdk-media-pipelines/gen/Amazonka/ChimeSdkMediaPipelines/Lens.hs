{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSdkMediaPipelines.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Lens
  ( -- * Operations

    -- ** CreateMediaCapturePipeline
    createMediaCapturePipeline_tags,
    createMediaCapturePipeline_clientRequestToken,
    createMediaCapturePipeline_chimeSdkMeetingConfiguration,
    createMediaCapturePipeline_sourceType,
    createMediaCapturePipeline_sourceArn,
    createMediaCapturePipeline_sinkType,
    createMediaCapturePipeline_sinkArn,
    createMediaCapturePipelineResponse_mediaCapturePipeline,
    createMediaCapturePipelineResponse_httpStatus,

    -- ** CreateMediaConcatenationPipeline
    createMediaConcatenationPipeline_tags,
    createMediaConcatenationPipeline_clientRequestToken,
    createMediaConcatenationPipeline_sources,
    createMediaConcatenationPipeline_sinks,
    createMediaConcatenationPipelineResponse_mediaConcatenationPipeline,
    createMediaConcatenationPipelineResponse_httpStatus,

    -- ** CreateMediaLiveConnectorPipeline
    createMediaLiveConnectorPipeline_tags,
    createMediaLiveConnectorPipeline_clientRequestToken,
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
    listMediaCapturePipelines_nextToken,
    listMediaCapturePipelines_maxResults,
    listMediaCapturePipelinesResponse_nextToken,
    listMediaCapturePipelinesResponse_mediaCapturePipelines,
    listMediaCapturePipelinesResponse_httpStatus,

    -- ** ListMediaPipelines
    listMediaPipelines_nextToken,
    listMediaPipelines_maxResults,
    listMediaPipelinesResponse_nextToken,
    listMediaPipelinesResponse_mediaPipelines,
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
    chimeSdkMeetingConfiguration_sourceConfiguration,
    chimeSdkMeetingConfiguration_artifactsConfiguration,

    -- ** ChimeSdkMeetingLiveConnectorConfiguration
    chimeSdkMeetingLiveConnectorConfiguration_sourceConfiguration,
    chimeSdkMeetingLiveConnectorConfiguration_compositedVideo,
    chimeSdkMeetingLiveConnectorConfiguration_arn,
    chimeSdkMeetingLiveConnectorConfiguration_muxType,

    -- ** CompositedVideoArtifactsConfiguration
    compositedVideoArtifactsConfiguration_resolution,
    compositedVideoArtifactsConfiguration_layout,
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
    mediaCapturePipeline_sourceArn,
    mediaCapturePipeline_sinkType,
    mediaCapturePipeline_createdTimestamp,
    mediaCapturePipeline_mediaPipelineArn,
    mediaCapturePipeline_updatedTimestamp,
    mediaCapturePipeline_chimeSdkMeetingConfiguration,
    mediaCapturePipeline_status,
    mediaCapturePipeline_sourceType,
    mediaCapturePipeline_mediaPipelineId,
    mediaCapturePipeline_sinkArn,

    -- ** MediaCapturePipelineSourceConfiguration
    mediaCapturePipelineSourceConfiguration_mediaPipelineArn,
    mediaCapturePipelineSourceConfiguration_chimeSdkMeetingConfiguration,

    -- ** MediaCapturePipelineSummary
    mediaCapturePipelineSummary_mediaPipelineArn,
    mediaCapturePipelineSummary_mediaPipelineId,

    -- ** MediaConcatenationPipeline
    mediaConcatenationPipeline_sources,
    mediaConcatenationPipeline_createdTimestamp,
    mediaConcatenationPipeline_mediaPipelineArn,
    mediaConcatenationPipeline_updatedTimestamp,
    mediaConcatenationPipeline_status,
    mediaConcatenationPipeline_sinks,
    mediaConcatenationPipeline_mediaPipelineId,

    -- ** MediaLiveConnectorPipeline
    mediaLiveConnectorPipeline_sources,
    mediaLiveConnectorPipeline_createdTimestamp,
    mediaLiveConnectorPipeline_mediaPipelineArn,
    mediaLiveConnectorPipeline_updatedTimestamp,
    mediaLiveConnectorPipeline_status,
    mediaLiveConnectorPipeline_sinks,
    mediaLiveConnectorPipeline_mediaPipelineId,

    -- ** MediaPipeline
    mediaPipeline_mediaConcatenationPipeline,
    mediaPipeline_mediaCapturePipeline,
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
