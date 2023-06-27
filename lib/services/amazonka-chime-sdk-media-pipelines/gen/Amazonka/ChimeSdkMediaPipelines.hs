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
-- information about media pipelines, see
-- <https://docs.aws.amazon.com/chime-sdk/latest/APIReference/API_Operations_Amazon_Chime_SDK_Media_Pipelines.html Amazon Chime SDK media pipelines>.
module Amazonka.ChimeSdkMediaPipelines
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

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

    -- ** CreateMediaInsightsPipeline
    CreateMediaInsightsPipeline (CreateMediaInsightsPipeline'),
    newCreateMediaInsightsPipeline,
    CreateMediaInsightsPipelineResponse (CreateMediaInsightsPipelineResponse'),
    newCreateMediaInsightsPipelineResponse,

    -- ** CreateMediaInsightsPipelineConfiguration
    CreateMediaInsightsPipelineConfiguration (CreateMediaInsightsPipelineConfiguration'),
    newCreateMediaInsightsPipelineConfiguration,
    CreateMediaInsightsPipelineConfigurationResponse (CreateMediaInsightsPipelineConfigurationResponse'),
    newCreateMediaInsightsPipelineConfigurationResponse,

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

    -- ** DeleteMediaInsightsPipelineConfiguration
    DeleteMediaInsightsPipelineConfiguration (DeleteMediaInsightsPipelineConfiguration'),
    newDeleteMediaInsightsPipelineConfiguration,
    DeleteMediaInsightsPipelineConfigurationResponse (DeleteMediaInsightsPipelineConfigurationResponse'),
    newDeleteMediaInsightsPipelineConfigurationResponse,

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

    -- ** GetMediaInsightsPipelineConfiguration
    GetMediaInsightsPipelineConfiguration (GetMediaInsightsPipelineConfiguration'),
    newGetMediaInsightsPipelineConfiguration,
    GetMediaInsightsPipelineConfigurationResponse (GetMediaInsightsPipelineConfigurationResponse'),
    newGetMediaInsightsPipelineConfigurationResponse,

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

    -- ** ListMediaInsightsPipelineConfigurations
    ListMediaInsightsPipelineConfigurations (ListMediaInsightsPipelineConfigurations'),
    newListMediaInsightsPipelineConfigurations,
    ListMediaInsightsPipelineConfigurationsResponse (ListMediaInsightsPipelineConfigurationsResponse'),
    newListMediaInsightsPipelineConfigurationsResponse,

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

    -- ** UpdateMediaInsightsPipelineConfiguration
    UpdateMediaInsightsPipelineConfiguration (UpdateMediaInsightsPipelineConfiguration'),
    newUpdateMediaInsightsPipelineConfiguration,
    UpdateMediaInsightsPipelineConfigurationResponse (UpdateMediaInsightsPipelineConfigurationResponse'),
    newUpdateMediaInsightsPipelineConfigurationResponse,

    -- ** UpdateMediaInsightsPipelineStatus
    UpdateMediaInsightsPipelineStatus (UpdateMediaInsightsPipelineStatus'),
    newUpdateMediaInsightsPipelineStatus,
    UpdateMediaInsightsPipelineStatusResponse (UpdateMediaInsightsPipelineStatusResponse'),
    newUpdateMediaInsightsPipelineStatusResponse,

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

    -- ** CallAnalyticsLanguageCode
    CallAnalyticsLanguageCode (..),

    -- ** ConcatenationSinkType
    ConcatenationSinkType (..),

    -- ** ConcatenationSourceType
    ConcatenationSourceType (..),

    -- ** ContentMuxType
    ContentMuxType (..),

    -- ** ContentRedactionOutput
    ContentRedactionOutput (..),

    -- ** ContentShareLayoutOption
    ContentShareLayoutOption (..),

    -- ** ContentType
    ContentType (..),

    -- ** FragmentSelectorType
    FragmentSelectorType (..),

    -- ** LayoutOption
    LayoutOption (..),

    -- ** LiveConnectorMuxType
    LiveConnectorMuxType (..),

    -- ** LiveConnectorSinkType
    LiveConnectorSinkType (..),

    -- ** LiveConnectorSourceType
    LiveConnectorSourceType (..),

    -- ** MediaEncoding
    MediaEncoding (..),

    -- ** MediaInsightsPipelineConfigurationElementType
    MediaInsightsPipelineConfigurationElementType (..),

    -- ** MediaPipelineSinkType
    MediaPipelineSinkType (..),

    -- ** MediaPipelineSourceType
    MediaPipelineSourceType (..),

    -- ** MediaPipelineStatus
    MediaPipelineStatus (..),

    -- ** MediaPipelineStatusUpdate
    MediaPipelineStatusUpdate (..),

    -- ** PartialResultsStability
    PartialResultsStability (..),

    -- ** ParticipantRole
    ParticipantRole (..),

    -- ** PresenterPosition
    PresenterPosition (..),

    -- ** RealTimeAlertRuleType
    RealTimeAlertRuleType (..),

    -- ** RecordingFileFormat
    RecordingFileFormat (..),

    -- ** ResolutionOption
    ResolutionOption (..),

    -- ** SentimentType
    SentimentType (..),

    -- ** VideoMuxType
    VideoMuxType (..),

    -- ** VocabularyFilterMethod
    VocabularyFilterMethod (..),

    -- ** VoiceAnalyticsConfigurationStatus
    VoiceAnalyticsConfigurationStatus (..),

    -- ** AmazonTranscribeCallAnalyticsProcessorConfiguration
    AmazonTranscribeCallAnalyticsProcessorConfiguration (AmazonTranscribeCallAnalyticsProcessorConfiguration'),
    newAmazonTranscribeCallAnalyticsProcessorConfiguration,

    -- ** AmazonTranscribeProcessorConfiguration
    AmazonTranscribeProcessorConfiguration (AmazonTranscribeProcessorConfiguration'),
    newAmazonTranscribeProcessorConfiguration,

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

    -- ** ChannelDefinition
    ChannelDefinition (ChannelDefinition'),
    newChannelDefinition,

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

    -- ** FragmentSelector
    FragmentSelector (FragmentSelector'),
    newFragmentSelector,

    -- ** GridViewConfiguration
    GridViewConfiguration (GridViewConfiguration'),
    newGridViewConfiguration,

    -- ** IssueDetectionConfiguration
    IssueDetectionConfiguration (IssueDetectionConfiguration'),
    newIssueDetectionConfiguration,

    -- ** KeywordMatchConfiguration
    KeywordMatchConfiguration (KeywordMatchConfiguration'),
    newKeywordMatchConfiguration,

    -- ** KinesisDataStreamSinkConfiguration
    KinesisDataStreamSinkConfiguration (KinesisDataStreamSinkConfiguration'),
    newKinesisDataStreamSinkConfiguration,

    -- ** KinesisVideoStreamRecordingSourceRuntimeConfiguration
    KinesisVideoStreamRecordingSourceRuntimeConfiguration (KinesisVideoStreamRecordingSourceRuntimeConfiguration'),
    newKinesisVideoStreamRecordingSourceRuntimeConfiguration,

    -- ** KinesisVideoStreamSourceRuntimeConfiguration
    KinesisVideoStreamSourceRuntimeConfiguration (KinesisVideoStreamSourceRuntimeConfiguration'),
    newKinesisVideoStreamSourceRuntimeConfiguration,

    -- ** LambdaFunctionSinkConfiguration
    LambdaFunctionSinkConfiguration (LambdaFunctionSinkConfiguration'),
    newLambdaFunctionSinkConfiguration,

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

    -- ** MediaInsightsPipeline
    MediaInsightsPipeline (MediaInsightsPipeline'),
    newMediaInsightsPipeline,

    -- ** MediaInsightsPipelineConfiguration
    MediaInsightsPipelineConfiguration (MediaInsightsPipelineConfiguration'),
    newMediaInsightsPipelineConfiguration,

    -- ** MediaInsightsPipelineConfigurationElement
    MediaInsightsPipelineConfigurationElement (MediaInsightsPipelineConfigurationElement'),
    newMediaInsightsPipelineConfigurationElement,

    -- ** MediaInsightsPipelineConfigurationSummary
    MediaInsightsPipelineConfigurationSummary (MediaInsightsPipelineConfigurationSummary'),
    newMediaInsightsPipelineConfigurationSummary,

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

    -- ** PostCallAnalyticsSettings
    PostCallAnalyticsSettings (PostCallAnalyticsSettings'),
    newPostCallAnalyticsSettings,

    -- ** PresenterOnlyConfiguration
    PresenterOnlyConfiguration (PresenterOnlyConfiguration'),
    newPresenterOnlyConfiguration,

    -- ** RealTimeAlertConfiguration
    RealTimeAlertConfiguration (RealTimeAlertConfiguration'),
    newRealTimeAlertConfiguration,

    -- ** RealTimeAlertRule
    RealTimeAlertRule (RealTimeAlertRule'),
    newRealTimeAlertRule,

    -- ** RecordingStreamConfiguration
    RecordingStreamConfiguration (RecordingStreamConfiguration'),
    newRecordingStreamConfiguration,

    -- ** S3BucketSinkConfiguration
    S3BucketSinkConfiguration (S3BucketSinkConfiguration'),
    newS3BucketSinkConfiguration,

    -- ** S3RecordingSinkConfiguration
    S3RecordingSinkConfiguration (S3RecordingSinkConfiguration'),
    newS3RecordingSinkConfiguration,

    -- ** S3RecordingSinkRuntimeConfiguration
    S3RecordingSinkRuntimeConfiguration (S3RecordingSinkRuntimeConfiguration'),
    newS3RecordingSinkRuntimeConfiguration,

    -- ** SelectedVideoStreams
    SelectedVideoStreams (SelectedVideoStreams'),
    newSelectedVideoStreams,

    -- ** SentimentConfiguration
    SentimentConfiguration (SentimentConfiguration'),
    newSentimentConfiguration,

    -- ** SnsTopicSinkConfiguration
    SnsTopicSinkConfiguration (SnsTopicSinkConfiguration'),
    newSnsTopicSinkConfiguration,

    -- ** SourceConfiguration
    SourceConfiguration (SourceConfiguration'),
    newSourceConfiguration,

    -- ** SqsQueueSinkConfiguration
    SqsQueueSinkConfiguration (SqsQueueSinkConfiguration'),
    newSqsQueueSinkConfiguration,

    -- ** StreamChannelDefinition
    StreamChannelDefinition (StreamChannelDefinition'),
    newStreamChannelDefinition,

    -- ** StreamConfiguration
    StreamConfiguration (StreamConfiguration'),
    newStreamConfiguration,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TimestampRange
    TimestampRange (TimestampRange'),
    newTimestampRange,

    -- ** TranscriptionMessagesConcatenationConfiguration
    TranscriptionMessagesConcatenationConfiguration (TranscriptionMessagesConcatenationConfiguration'),
    newTranscriptionMessagesConcatenationConfiguration,

    -- ** VideoArtifactsConfiguration
    VideoArtifactsConfiguration (VideoArtifactsConfiguration'),
    newVideoArtifactsConfiguration,

    -- ** VideoConcatenationConfiguration
    VideoConcatenationConfiguration (VideoConcatenationConfiguration'),
    newVideoConcatenationConfiguration,

    -- ** VoiceAnalyticsProcessorConfiguration
    VoiceAnalyticsProcessorConfiguration (VoiceAnalyticsProcessorConfiguration'),
    newVoiceAnalyticsProcessorConfiguration,
  )
where

import Amazonka.ChimeSdkMediaPipelines.CreateMediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.CreateMediaConcatenationPipeline
import Amazonka.ChimeSdkMediaPipelines.CreateMediaInsightsPipeline
import Amazonka.ChimeSdkMediaPipelines.CreateMediaInsightsPipelineConfiguration
import Amazonka.ChimeSdkMediaPipelines.CreateMediaLiveConnectorPipeline
import Amazonka.ChimeSdkMediaPipelines.DeleteMediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.DeleteMediaInsightsPipelineConfiguration
import Amazonka.ChimeSdkMediaPipelines.DeleteMediaPipeline
import Amazonka.ChimeSdkMediaPipelines.GetMediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.GetMediaInsightsPipelineConfiguration
import Amazonka.ChimeSdkMediaPipelines.GetMediaPipeline
import Amazonka.ChimeSdkMediaPipelines.Lens
import Amazonka.ChimeSdkMediaPipelines.ListMediaCapturePipelines
import Amazonka.ChimeSdkMediaPipelines.ListMediaInsightsPipelineConfigurations
import Amazonka.ChimeSdkMediaPipelines.ListMediaPipelines
import Amazonka.ChimeSdkMediaPipelines.ListTagsForResource
import Amazonka.ChimeSdkMediaPipelines.TagResource
import Amazonka.ChimeSdkMediaPipelines.Types
import Amazonka.ChimeSdkMediaPipelines.UntagResource
import Amazonka.ChimeSdkMediaPipelines.UpdateMediaInsightsPipelineConfiguration
import Amazonka.ChimeSdkMediaPipelines.UpdateMediaInsightsPipelineStatus
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
