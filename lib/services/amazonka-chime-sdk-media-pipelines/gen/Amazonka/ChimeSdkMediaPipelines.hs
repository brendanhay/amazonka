{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ChimeSdkMediaPipelines
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-07-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amazon Chime SDK media pipeline APIs in this section allow software
-- developers to create Amazon Chime SDK media pipelines that capture,
-- concatenate, or stream your Amazon Chime SDK meetings. For more
-- information about media pipleines, see
-- <http://amazonaws.com/chime/latest/APIReference/API_Operations_Amazon_Chime_SDK_Media_Pipelines.html Amazon Chime SDK media pipelines>.
module Amazonka.ChimeSdkMediaPipelines
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottledClientException
    _ThrottledClientException,

    -- ** UnauthorizedClientException
    _UnauthorizedClientException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateMediaCapturePipeline
    CreateMediaCapturePipeline (CreateMediaCapturePipeline'),
    newCreateMediaCapturePipeline,
    CreateMediaCapturePipelineResponse (CreateMediaCapturePipelineResponse'),
    newCreateMediaCapturePipelineResponse,

    -- ** CreateMediaConcatenationPipeline
    CreateMediaConcatenationPipeline (CreateMediaConcatenationPipeline'),
    newCreateMediaConcatenationPipeline,
    CreateMediaConcatenationPipelineResponse (CreateMediaConcatenationPipelineResponse'),
    newCreateMediaConcatenationPipelineResponse,

    -- ** CreateMediaLiveConnectorPipeline
    CreateMediaLiveConnectorPipeline (CreateMediaLiveConnectorPipeline'),
    newCreateMediaLiveConnectorPipeline,
    CreateMediaLiveConnectorPipelineResponse (CreateMediaLiveConnectorPipelineResponse'),
    newCreateMediaLiveConnectorPipelineResponse,

    -- ** DeleteMediaCapturePipeline
    DeleteMediaCapturePipeline (DeleteMediaCapturePipeline'),
    newDeleteMediaCapturePipeline,
    DeleteMediaCapturePipelineResponse (DeleteMediaCapturePipelineResponse'),
    newDeleteMediaCapturePipelineResponse,

    -- ** DeleteMediaPipeline
    DeleteMediaPipeline (DeleteMediaPipeline'),
    newDeleteMediaPipeline,
    DeleteMediaPipelineResponse (DeleteMediaPipelineResponse'),
    newDeleteMediaPipelineResponse,

    -- ** GetMediaCapturePipeline
    GetMediaCapturePipeline (GetMediaCapturePipeline'),
    newGetMediaCapturePipeline,
    GetMediaCapturePipelineResponse (GetMediaCapturePipelineResponse'),
    newGetMediaCapturePipelineResponse,

    -- ** GetMediaPipeline
    GetMediaPipeline (GetMediaPipeline'),
    newGetMediaPipeline,
    GetMediaPipelineResponse (GetMediaPipelineResponse'),
    newGetMediaPipelineResponse,

    -- ** ListMediaCapturePipelines
    ListMediaCapturePipelines (ListMediaCapturePipelines'),
    newListMediaCapturePipelines,
    ListMediaCapturePipelinesResponse (ListMediaCapturePipelinesResponse'),
    newListMediaCapturePipelinesResponse,

    -- ** ListMediaPipelines
    ListMediaPipelines (ListMediaPipelines'),
    newListMediaPipelines,
    ListMediaPipelinesResponse (ListMediaPipelinesResponse'),
    newListMediaPipelinesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- * Types

    -- ** ArtifactsConcatenationState
    ArtifactsConcatenationState (..),

    -- ** ArtifactsState
    ArtifactsState (..),

    -- ** AudioArtifactsConcatenationState
    AudioArtifactsConcatenationState (..),

    -- ** AudioChannelsOption
    AudioChannelsOption (..),

    -- ** AudioMuxType
    AudioMuxType (..),

    -- ** ConcatenationSinkType
    ConcatenationSinkType (..),

    -- ** ConcatenationSourceType
    ConcatenationSourceType (..),

    -- ** ContentMuxType
    ContentMuxType (..),

    -- ** ContentShareLayoutOption
    ContentShareLayoutOption (..),

    -- ** LayoutOption
    LayoutOption (..),

    -- ** LiveConnectorMuxType
    LiveConnectorMuxType (..),

    -- ** LiveConnectorSinkType
    LiveConnectorSinkType (..),

    -- ** LiveConnectorSourceType
    LiveConnectorSourceType (..),

    -- ** MediaPipelineSinkType
    MediaPipelineSinkType (..),

    -- ** MediaPipelineSourceType
    MediaPipelineSourceType (..),

    -- ** MediaPipelineStatus
    MediaPipelineStatus (..),

    -- ** PresenterPosition
    PresenterPosition (..),

    -- ** ResolutionOption
    ResolutionOption (..),

    -- ** VideoMuxType
    VideoMuxType (..),

    -- ** ArtifactsConcatenationConfiguration
    ArtifactsConcatenationConfiguration (ArtifactsConcatenationConfiguration'),
    newArtifactsConcatenationConfiguration,

    -- ** ArtifactsConfiguration
    ArtifactsConfiguration (ArtifactsConfiguration'),
    newArtifactsConfiguration,

    -- ** AudioArtifactsConfiguration
    AudioArtifactsConfiguration (AudioArtifactsConfiguration'),
    newAudioArtifactsConfiguration,

    -- ** AudioConcatenationConfiguration
    AudioConcatenationConfiguration (AudioConcatenationConfiguration'),
    newAudioConcatenationConfiguration,

    -- ** ChimeSdkMeetingConcatenationConfiguration
    ChimeSdkMeetingConcatenationConfiguration (ChimeSdkMeetingConcatenationConfiguration'),
    newChimeSdkMeetingConcatenationConfiguration,

    -- ** ChimeSdkMeetingConfiguration
    ChimeSdkMeetingConfiguration (ChimeSdkMeetingConfiguration'),
    newChimeSdkMeetingConfiguration,

    -- ** ChimeSdkMeetingLiveConnectorConfiguration
    ChimeSdkMeetingLiveConnectorConfiguration (ChimeSdkMeetingLiveConnectorConfiguration'),
    newChimeSdkMeetingLiveConnectorConfiguration,

    -- ** CompositedVideoArtifactsConfiguration
    CompositedVideoArtifactsConfiguration (CompositedVideoArtifactsConfiguration'),
    newCompositedVideoArtifactsConfiguration,

    -- ** CompositedVideoConcatenationConfiguration
    CompositedVideoConcatenationConfiguration (CompositedVideoConcatenationConfiguration'),
    newCompositedVideoConcatenationConfiguration,

    -- ** ConcatenationSink
    ConcatenationSink (ConcatenationSink'),
    newConcatenationSink,

    -- ** ConcatenationSource
    ConcatenationSource (ConcatenationSource'),
    newConcatenationSource,

    -- ** ContentArtifactsConfiguration
    ContentArtifactsConfiguration (ContentArtifactsConfiguration'),
    newContentArtifactsConfiguration,

    -- ** ContentConcatenationConfiguration
    ContentConcatenationConfiguration (ContentConcatenationConfiguration'),
    newContentConcatenationConfiguration,

    -- ** DataChannelConcatenationConfiguration
    DataChannelConcatenationConfiguration (DataChannelConcatenationConfiguration'),
    newDataChannelConcatenationConfiguration,

    -- ** GridViewConfiguration
    GridViewConfiguration (GridViewConfiguration'),
    newGridViewConfiguration,

    -- ** LiveConnectorRTMPConfiguration
    LiveConnectorRTMPConfiguration (LiveConnectorRTMPConfiguration'),
    newLiveConnectorRTMPConfiguration,

    -- ** LiveConnectorSinkConfiguration
    LiveConnectorSinkConfiguration (LiveConnectorSinkConfiguration'),
    newLiveConnectorSinkConfiguration,

    -- ** LiveConnectorSourceConfiguration
    LiveConnectorSourceConfiguration (LiveConnectorSourceConfiguration'),
    newLiveConnectorSourceConfiguration,

    -- ** MediaCapturePipeline
    MediaCapturePipeline (MediaCapturePipeline'),
    newMediaCapturePipeline,

    -- ** MediaCapturePipelineSourceConfiguration
    MediaCapturePipelineSourceConfiguration (MediaCapturePipelineSourceConfiguration'),
    newMediaCapturePipelineSourceConfiguration,

    -- ** MediaCapturePipelineSummary
    MediaCapturePipelineSummary (MediaCapturePipelineSummary'),
    newMediaCapturePipelineSummary,

    -- ** MediaConcatenationPipeline
    MediaConcatenationPipeline (MediaConcatenationPipeline'),
    newMediaConcatenationPipeline,

    -- ** MediaLiveConnectorPipeline
    MediaLiveConnectorPipeline (MediaLiveConnectorPipeline'),
    newMediaLiveConnectorPipeline,

    -- ** MediaPipeline
    MediaPipeline (MediaPipeline'),
    newMediaPipeline,

    -- ** MediaPipelineSummary
    MediaPipelineSummary (MediaPipelineSummary'),
    newMediaPipelineSummary,

    -- ** MeetingEventsConcatenationConfiguration
    MeetingEventsConcatenationConfiguration (MeetingEventsConcatenationConfiguration'),
    newMeetingEventsConcatenationConfiguration,

    -- ** PresenterOnlyConfiguration
    PresenterOnlyConfiguration (PresenterOnlyConfiguration'),
    newPresenterOnlyConfiguration,

    -- ** S3BucketSinkConfiguration
    S3BucketSinkConfiguration (S3BucketSinkConfiguration'),
    newS3BucketSinkConfiguration,

    -- ** SelectedVideoStreams
    SelectedVideoStreams (SelectedVideoStreams'),
    newSelectedVideoStreams,

    -- ** SourceConfiguration
    SourceConfiguration (SourceConfiguration'),
    newSourceConfiguration,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TranscriptionMessagesConcatenationConfiguration
    TranscriptionMessagesConcatenationConfiguration (TranscriptionMessagesConcatenationConfiguration'),
    newTranscriptionMessagesConcatenationConfiguration,

    -- ** VideoArtifactsConfiguration
    VideoArtifactsConfiguration (VideoArtifactsConfiguration'),
    newVideoArtifactsConfiguration,

    -- ** VideoConcatenationConfiguration
    VideoConcatenationConfiguration (VideoConcatenationConfiguration'),
    newVideoConcatenationConfiguration,
  )
where

import Amazonka.ChimeSdkMediaPipelines.CreateMediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.CreateMediaConcatenationPipeline
import Amazonka.ChimeSdkMediaPipelines.CreateMediaLiveConnectorPipeline
import Amazonka.ChimeSdkMediaPipelines.DeleteMediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.DeleteMediaPipeline
import Amazonka.ChimeSdkMediaPipelines.GetMediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.GetMediaPipeline
import Amazonka.ChimeSdkMediaPipelines.Lens
import Amazonka.ChimeSdkMediaPipelines.ListMediaCapturePipelines
import Amazonka.ChimeSdkMediaPipelines.ListMediaPipelines
import Amazonka.ChimeSdkMediaPipelines.ListTagsForResource
import Amazonka.ChimeSdkMediaPipelines.TagResource
import Amazonka.ChimeSdkMediaPipelines.Types
import Amazonka.ChimeSdkMediaPipelines.UntagResource
import Amazonka.ChimeSdkMediaPipelines.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ChimeSdkMediaPipelines'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
