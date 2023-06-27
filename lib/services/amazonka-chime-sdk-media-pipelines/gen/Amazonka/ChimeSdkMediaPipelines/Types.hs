{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _ResourceLimitExceededException,
    _ServiceFailureException,
    _ServiceUnavailableException,
    _ThrottledClientException,
    _UnauthorizedClientException,

    -- * ArtifactsConcatenationState
    ArtifactsConcatenationState (..),

    -- * ArtifactsState
    ArtifactsState (..),

    -- * AudioArtifactsConcatenationState
    AudioArtifactsConcatenationState (..),

    -- * AudioChannelsOption
    AudioChannelsOption (..),

    -- * AudioMuxType
    AudioMuxType (..),

    -- * CallAnalyticsLanguageCode
    CallAnalyticsLanguageCode (..),

    -- * ConcatenationSinkType
    ConcatenationSinkType (..),

    -- * ConcatenationSourceType
    ConcatenationSourceType (..),

    -- * ContentMuxType
    ContentMuxType (..),

    -- * ContentRedactionOutput
    ContentRedactionOutput (..),

    -- * ContentShareLayoutOption
    ContentShareLayoutOption (..),

    -- * ContentType
    ContentType (..),

    -- * FragmentSelectorType
    FragmentSelectorType (..),

    -- * LayoutOption
    LayoutOption (..),

    -- * LiveConnectorMuxType
    LiveConnectorMuxType (..),

    -- * LiveConnectorSinkType
    LiveConnectorSinkType (..),

    -- * LiveConnectorSourceType
    LiveConnectorSourceType (..),

    -- * MediaEncoding
    MediaEncoding (..),

    -- * MediaInsightsPipelineConfigurationElementType
    MediaInsightsPipelineConfigurationElementType (..),

    -- * MediaPipelineSinkType
    MediaPipelineSinkType (..),

    -- * MediaPipelineSourceType
    MediaPipelineSourceType (..),

    -- * MediaPipelineStatus
    MediaPipelineStatus (..),

    -- * MediaPipelineStatusUpdate
    MediaPipelineStatusUpdate (..),

    -- * PartialResultsStability
    PartialResultsStability (..),

    -- * ParticipantRole
    ParticipantRole (..),

    -- * PresenterPosition
    PresenterPosition (..),

    -- * RealTimeAlertRuleType
    RealTimeAlertRuleType (..),

    -- * RecordingFileFormat
    RecordingFileFormat (..),

    -- * ResolutionOption
    ResolutionOption (..),

    -- * SentimentType
    SentimentType (..),

    -- * VideoMuxType
    VideoMuxType (..),

    -- * VocabularyFilterMethod
    VocabularyFilterMethod (..),

    -- * VoiceAnalyticsConfigurationStatus
    VoiceAnalyticsConfigurationStatus (..),

    -- * AmazonTranscribeCallAnalyticsProcessorConfiguration
    AmazonTranscribeCallAnalyticsProcessorConfiguration (..),
    newAmazonTranscribeCallAnalyticsProcessorConfiguration,
    amazonTranscribeCallAnalyticsProcessorConfiguration_callAnalyticsStreamCategories,
    amazonTranscribeCallAnalyticsProcessorConfiguration_contentIdentificationType,
    amazonTranscribeCallAnalyticsProcessorConfiguration_contentRedactionType,
    amazonTranscribeCallAnalyticsProcessorConfiguration_enablePartialResultsStabilization,
    amazonTranscribeCallAnalyticsProcessorConfiguration_filterPartialResults,
    amazonTranscribeCallAnalyticsProcessorConfiguration_languageModelName,
    amazonTranscribeCallAnalyticsProcessorConfiguration_partialResultsStability,
    amazonTranscribeCallAnalyticsProcessorConfiguration_piiEntityTypes,
    amazonTranscribeCallAnalyticsProcessorConfiguration_postCallAnalyticsSettings,
    amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyFilterMethod,
    amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyFilterName,
    amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyName,
    amazonTranscribeCallAnalyticsProcessorConfiguration_languageCode,

    -- * AmazonTranscribeProcessorConfiguration
    AmazonTranscribeProcessorConfiguration (..),
    newAmazonTranscribeProcessorConfiguration,
    amazonTranscribeProcessorConfiguration_contentIdentificationType,
    amazonTranscribeProcessorConfiguration_contentRedactionType,
    amazonTranscribeProcessorConfiguration_enablePartialResultsStabilization,
    amazonTranscribeProcessorConfiguration_filterPartialResults,
    amazonTranscribeProcessorConfiguration_languageModelName,
    amazonTranscribeProcessorConfiguration_partialResultsStability,
    amazonTranscribeProcessorConfiguration_piiEntityTypes,
    amazonTranscribeProcessorConfiguration_showSpeakerLabel,
    amazonTranscribeProcessorConfiguration_vocabularyFilterMethod,
    amazonTranscribeProcessorConfiguration_vocabularyFilterName,
    amazonTranscribeProcessorConfiguration_vocabularyName,
    amazonTranscribeProcessorConfiguration_languageCode,

    -- * ArtifactsConcatenationConfiguration
    ArtifactsConcatenationConfiguration (..),
    newArtifactsConcatenationConfiguration,
    artifactsConcatenationConfiguration_audio,
    artifactsConcatenationConfiguration_video,
    artifactsConcatenationConfiguration_content,
    artifactsConcatenationConfiguration_dataChannel,
    artifactsConcatenationConfiguration_transcriptionMessages,
    artifactsConcatenationConfiguration_meetingEvents,
    artifactsConcatenationConfiguration_compositedVideo,

    -- * ArtifactsConfiguration
    ArtifactsConfiguration (..),
    newArtifactsConfiguration,
    artifactsConfiguration_compositedVideo,
    artifactsConfiguration_audio,
    artifactsConfiguration_video,
    artifactsConfiguration_content,

    -- * AudioArtifactsConfiguration
    AudioArtifactsConfiguration (..),
    newAudioArtifactsConfiguration,
    audioArtifactsConfiguration_muxType,

    -- * AudioConcatenationConfiguration
    AudioConcatenationConfiguration (..),
    newAudioConcatenationConfiguration,
    audioConcatenationConfiguration_state,

    -- * ChannelDefinition
    ChannelDefinition (..),
    newChannelDefinition,
    channelDefinition_participantRole,
    channelDefinition_channelId,

    -- * ChimeSdkMeetingConcatenationConfiguration
    ChimeSdkMeetingConcatenationConfiguration (..),
    newChimeSdkMeetingConcatenationConfiguration,
    chimeSdkMeetingConcatenationConfiguration_artifactsConfiguration,

    -- * ChimeSdkMeetingConfiguration
    ChimeSdkMeetingConfiguration (..),
    newChimeSdkMeetingConfiguration,
    chimeSdkMeetingConfiguration_artifactsConfiguration,
    chimeSdkMeetingConfiguration_sourceConfiguration,

    -- * ChimeSdkMeetingLiveConnectorConfiguration
    ChimeSdkMeetingLiveConnectorConfiguration (..),
    newChimeSdkMeetingLiveConnectorConfiguration,
    chimeSdkMeetingLiveConnectorConfiguration_compositedVideo,
    chimeSdkMeetingLiveConnectorConfiguration_sourceConfiguration,
    chimeSdkMeetingLiveConnectorConfiguration_arn,
    chimeSdkMeetingLiveConnectorConfiguration_muxType,

    -- * CompositedVideoArtifactsConfiguration
    CompositedVideoArtifactsConfiguration (..),
    newCompositedVideoArtifactsConfiguration,
    compositedVideoArtifactsConfiguration_layout,
    compositedVideoArtifactsConfiguration_resolution,
    compositedVideoArtifactsConfiguration_gridViewConfiguration,

    -- * CompositedVideoConcatenationConfiguration
    CompositedVideoConcatenationConfiguration (..),
    newCompositedVideoConcatenationConfiguration,
    compositedVideoConcatenationConfiguration_state,

    -- * ConcatenationSink
    ConcatenationSink (..),
    newConcatenationSink,
    concatenationSink_type,
    concatenationSink_s3BucketSinkConfiguration,

    -- * ConcatenationSource
    ConcatenationSource (..),
    newConcatenationSource,
    concatenationSource_type,
    concatenationSource_mediaCapturePipelineSourceConfiguration,

    -- * ContentArtifactsConfiguration
    ContentArtifactsConfiguration (..),
    newContentArtifactsConfiguration,
    contentArtifactsConfiguration_muxType,
    contentArtifactsConfiguration_state,

    -- * ContentConcatenationConfiguration
    ContentConcatenationConfiguration (..),
    newContentConcatenationConfiguration,
    contentConcatenationConfiguration_state,

    -- * DataChannelConcatenationConfiguration
    DataChannelConcatenationConfiguration (..),
    newDataChannelConcatenationConfiguration,
    dataChannelConcatenationConfiguration_state,

    -- * FragmentSelector
    FragmentSelector (..),
    newFragmentSelector,
    fragmentSelector_fragmentSelectorType,
    fragmentSelector_timestampRange,

    -- * GridViewConfiguration
    GridViewConfiguration (..),
    newGridViewConfiguration,
    gridViewConfiguration_presenterOnlyConfiguration,
    gridViewConfiguration_contentShareLayout,

    -- * IssueDetectionConfiguration
    IssueDetectionConfiguration (..),
    newIssueDetectionConfiguration,
    issueDetectionConfiguration_ruleName,

    -- * KeywordMatchConfiguration
    KeywordMatchConfiguration (..),
    newKeywordMatchConfiguration,
    keywordMatchConfiguration_negate,
    keywordMatchConfiguration_ruleName,
    keywordMatchConfiguration_keywords,

    -- * KinesisDataStreamSinkConfiguration
    KinesisDataStreamSinkConfiguration (..),
    newKinesisDataStreamSinkConfiguration,
    kinesisDataStreamSinkConfiguration_insightsTarget,

    -- * KinesisVideoStreamRecordingSourceRuntimeConfiguration
    KinesisVideoStreamRecordingSourceRuntimeConfiguration (..),
    newKinesisVideoStreamRecordingSourceRuntimeConfiguration,
    kinesisVideoStreamRecordingSourceRuntimeConfiguration_streams,
    kinesisVideoStreamRecordingSourceRuntimeConfiguration_fragmentSelector,

    -- * KinesisVideoStreamSourceRuntimeConfiguration
    KinesisVideoStreamSourceRuntimeConfiguration (..),
    newKinesisVideoStreamSourceRuntimeConfiguration,
    kinesisVideoStreamSourceRuntimeConfiguration_streams,
    kinesisVideoStreamSourceRuntimeConfiguration_mediaEncoding,
    kinesisVideoStreamSourceRuntimeConfiguration_mediaSampleRate,

    -- * LambdaFunctionSinkConfiguration
    LambdaFunctionSinkConfiguration (..),
    newLambdaFunctionSinkConfiguration,
    lambdaFunctionSinkConfiguration_insightsTarget,

    -- * LiveConnectorRTMPConfiguration
    LiveConnectorRTMPConfiguration (..),
    newLiveConnectorRTMPConfiguration,
    liveConnectorRTMPConfiguration_audioChannels,
    liveConnectorRTMPConfiguration_audioSampleRate,
    liveConnectorRTMPConfiguration_url,

    -- * LiveConnectorSinkConfiguration
    LiveConnectorSinkConfiguration (..),
    newLiveConnectorSinkConfiguration,
    liveConnectorSinkConfiguration_sinkType,
    liveConnectorSinkConfiguration_rTMPConfiguration,

    -- * LiveConnectorSourceConfiguration
    LiveConnectorSourceConfiguration (..),
    newLiveConnectorSourceConfiguration,
    liveConnectorSourceConfiguration_sourceType,
    liveConnectorSourceConfiguration_chimeSdkMeetingLiveConnectorConfiguration,

    -- * MediaCapturePipeline
    MediaCapturePipeline (..),
    newMediaCapturePipeline,
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

    -- * MediaCapturePipelineSourceConfiguration
    MediaCapturePipelineSourceConfiguration (..),
    newMediaCapturePipelineSourceConfiguration,
    mediaCapturePipelineSourceConfiguration_mediaPipelineArn,
    mediaCapturePipelineSourceConfiguration_chimeSdkMeetingConfiguration,

    -- * MediaCapturePipelineSummary
    MediaCapturePipelineSummary (..),
    newMediaCapturePipelineSummary,
    mediaCapturePipelineSummary_mediaPipelineArn,
    mediaCapturePipelineSummary_mediaPipelineId,

    -- * MediaConcatenationPipeline
    MediaConcatenationPipeline (..),
    newMediaConcatenationPipeline,
    mediaConcatenationPipeline_createdTimestamp,
    mediaConcatenationPipeline_mediaPipelineArn,
    mediaConcatenationPipeline_mediaPipelineId,
    mediaConcatenationPipeline_sinks,
    mediaConcatenationPipeline_sources,
    mediaConcatenationPipeline_status,
    mediaConcatenationPipeline_updatedTimestamp,

    -- * MediaInsightsPipeline
    MediaInsightsPipeline (..),
    newMediaInsightsPipeline,
    mediaInsightsPipeline_createdTimestamp,
    mediaInsightsPipeline_kinesisVideoStreamRecordingSourceRuntimeConfiguration,
    mediaInsightsPipeline_kinesisVideoStreamSourceRuntimeConfiguration,
    mediaInsightsPipeline_mediaInsightsPipelineConfigurationArn,
    mediaInsightsPipeline_mediaInsightsRuntimeMetadata,
    mediaInsightsPipeline_mediaPipelineArn,
    mediaInsightsPipeline_mediaPipelineId,
    mediaInsightsPipeline_s3RecordingSinkRuntimeConfiguration,
    mediaInsightsPipeline_status,

    -- * MediaInsightsPipelineConfiguration
    MediaInsightsPipelineConfiguration (..),
    newMediaInsightsPipelineConfiguration,
    mediaInsightsPipelineConfiguration_createdTimestamp,
    mediaInsightsPipelineConfiguration_elements,
    mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationArn,
    mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationId,
    mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationName,
    mediaInsightsPipelineConfiguration_realTimeAlertConfiguration,
    mediaInsightsPipelineConfiguration_resourceAccessRoleArn,
    mediaInsightsPipelineConfiguration_updatedTimestamp,

    -- * MediaInsightsPipelineConfigurationElement
    MediaInsightsPipelineConfigurationElement (..),
    newMediaInsightsPipelineConfigurationElement,
    mediaInsightsPipelineConfigurationElement_amazonTranscribeCallAnalyticsProcessorConfiguration,
    mediaInsightsPipelineConfigurationElement_amazonTranscribeProcessorConfiguration,
    mediaInsightsPipelineConfigurationElement_kinesisDataStreamSinkConfiguration,
    mediaInsightsPipelineConfigurationElement_lambdaFunctionSinkConfiguration,
    mediaInsightsPipelineConfigurationElement_s3RecordingSinkConfiguration,
    mediaInsightsPipelineConfigurationElement_snsTopicSinkConfiguration,
    mediaInsightsPipelineConfigurationElement_sqsQueueSinkConfiguration,
    mediaInsightsPipelineConfigurationElement_voiceAnalyticsProcessorConfiguration,
    mediaInsightsPipelineConfigurationElement_type,

    -- * MediaInsightsPipelineConfigurationSummary
    MediaInsightsPipelineConfigurationSummary (..),
    newMediaInsightsPipelineConfigurationSummary,
    mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationArn,
    mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationId,
    mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationName,

    -- * MediaLiveConnectorPipeline
    MediaLiveConnectorPipeline (..),
    newMediaLiveConnectorPipeline,
    mediaLiveConnectorPipeline_createdTimestamp,
    mediaLiveConnectorPipeline_mediaPipelineArn,
    mediaLiveConnectorPipeline_mediaPipelineId,
    mediaLiveConnectorPipeline_sinks,
    mediaLiveConnectorPipeline_sources,
    mediaLiveConnectorPipeline_status,
    mediaLiveConnectorPipeline_updatedTimestamp,

    -- * MediaPipeline
    MediaPipeline (..),
    newMediaPipeline,
    mediaPipeline_mediaCapturePipeline,
    mediaPipeline_mediaConcatenationPipeline,
    mediaPipeline_mediaInsightsPipeline,
    mediaPipeline_mediaLiveConnectorPipeline,

    -- * MediaPipelineSummary
    MediaPipelineSummary (..),
    newMediaPipelineSummary,
    mediaPipelineSummary_mediaPipelineArn,
    mediaPipelineSummary_mediaPipelineId,

    -- * MeetingEventsConcatenationConfiguration
    MeetingEventsConcatenationConfiguration (..),
    newMeetingEventsConcatenationConfiguration,
    meetingEventsConcatenationConfiguration_state,

    -- * PostCallAnalyticsSettings
    PostCallAnalyticsSettings (..),
    newPostCallAnalyticsSettings,
    postCallAnalyticsSettings_contentRedactionOutput,
    postCallAnalyticsSettings_outputEncryptionKMSKeyId,
    postCallAnalyticsSettings_outputLocation,
    postCallAnalyticsSettings_dataAccessRoleArn,

    -- * PresenterOnlyConfiguration
    PresenterOnlyConfiguration (..),
    newPresenterOnlyConfiguration,
    presenterOnlyConfiguration_presenterPosition,

    -- * RealTimeAlertConfiguration
    RealTimeAlertConfiguration (..),
    newRealTimeAlertConfiguration,
    realTimeAlertConfiguration_disabled,
    realTimeAlertConfiguration_rules,

    -- * RealTimeAlertRule
    RealTimeAlertRule (..),
    newRealTimeAlertRule,
    realTimeAlertRule_issueDetectionConfiguration,
    realTimeAlertRule_keywordMatchConfiguration,
    realTimeAlertRule_sentimentConfiguration,
    realTimeAlertRule_type,

    -- * RecordingStreamConfiguration
    RecordingStreamConfiguration (..),
    newRecordingStreamConfiguration,
    recordingStreamConfiguration_streamArn,

    -- * S3BucketSinkConfiguration
    S3BucketSinkConfiguration (..),
    newS3BucketSinkConfiguration,
    s3BucketSinkConfiguration_destination,

    -- * S3RecordingSinkConfiguration
    S3RecordingSinkConfiguration (..),
    newS3RecordingSinkConfiguration,
    s3RecordingSinkConfiguration_destination,
    s3RecordingSinkConfiguration_recordingFileFormat,

    -- * S3RecordingSinkRuntimeConfiguration
    S3RecordingSinkRuntimeConfiguration (..),
    newS3RecordingSinkRuntimeConfiguration,
    s3RecordingSinkRuntimeConfiguration_destination,
    s3RecordingSinkRuntimeConfiguration_recordingFileFormat,

    -- * SelectedVideoStreams
    SelectedVideoStreams (..),
    newSelectedVideoStreams,
    selectedVideoStreams_attendeeIds,
    selectedVideoStreams_externalUserIds,

    -- * SentimentConfiguration
    SentimentConfiguration (..),
    newSentimentConfiguration,
    sentimentConfiguration_ruleName,
    sentimentConfiguration_sentimentType,
    sentimentConfiguration_timePeriod,

    -- * SnsTopicSinkConfiguration
    SnsTopicSinkConfiguration (..),
    newSnsTopicSinkConfiguration,
    snsTopicSinkConfiguration_insightsTarget,

    -- * SourceConfiguration
    SourceConfiguration (..),
    newSourceConfiguration,
    sourceConfiguration_selectedVideoStreams,

    -- * SqsQueueSinkConfiguration
    SqsQueueSinkConfiguration (..),
    newSqsQueueSinkConfiguration,
    sqsQueueSinkConfiguration_insightsTarget,

    -- * StreamChannelDefinition
    StreamChannelDefinition (..),
    newStreamChannelDefinition,
    streamChannelDefinition_channelDefinitions,
    streamChannelDefinition_numberOfChannels,

    -- * StreamConfiguration
    StreamConfiguration (..),
    newStreamConfiguration,
    streamConfiguration_fragmentNumber,
    streamConfiguration_streamArn,
    streamConfiguration_streamChannelDefinition,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TimestampRange
    TimestampRange (..),
    newTimestampRange,
    timestampRange_startTimestamp,
    timestampRange_endTimestamp,

    -- * TranscriptionMessagesConcatenationConfiguration
    TranscriptionMessagesConcatenationConfiguration (..),
    newTranscriptionMessagesConcatenationConfiguration,
    transcriptionMessagesConcatenationConfiguration_state,

    -- * VideoArtifactsConfiguration
    VideoArtifactsConfiguration (..),
    newVideoArtifactsConfiguration,
    videoArtifactsConfiguration_muxType,
    videoArtifactsConfiguration_state,

    -- * VideoConcatenationConfiguration
    VideoConcatenationConfiguration (..),
    newVideoConcatenationConfiguration,
    videoConcatenationConfiguration_state,

    -- * VoiceAnalyticsProcessorConfiguration
    VoiceAnalyticsProcessorConfiguration (..),
    newVoiceAnalyticsProcessorConfiguration,
    voiceAnalyticsProcessorConfiguration_speakerSearchStatus,
    voiceAnalyticsProcessorConfiguration_voiceToneAnalysisStatus,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types.AmazonTranscribeCallAnalyticsProcessorConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.AmazonTranscribeProcessorConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationState
import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsState
import Amazonka.ChimeSdkMediaPipelines.Types.AudioArtifactsConcatenationState
import Amazonka.ChimeSdkMediaPipelines.Types.AudioArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.AudioChannelsOption
import Amazonka.ChimeSdkMediaPipelines.Types.AudioConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.AudioMuxType
import Amazonka.ChimeSdkMediaPipelines.Types.CallAnalyticsLanguageCode
import Amazonka.ChimeSdkMediaPipelines.Types.ChannelDefinition
import Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingLiveConnectorConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.CompositedVideoArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.CompositedVideoConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSink
import Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSinkType
import Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSource
import Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSourceType
import Amazonka.ChimeSdkMediaPipelines.Types.ContentArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ContentConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ContentMuxType
import Amazonka.ChimeSdkMediaPipelines.Types.ContentRedactionOutput
import Amazonka.ChimeSdkMediaPipelines.Types.ContentShareLayoutOption
import Amazonka.ChimeSdkMediaPipelines.Types.ContentType
import Amazonka.ChimeSdkMediaPipelines.Types.DataChannelConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.FragmentSelector
import Amazonka.ChimeSdkMediaPipelines.Types.FragmentSelectorType
import Amazonka.ChimeSdkMediaPipelines.Types.GridViewConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.IssueDetectionConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.KeywordMatchConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.KinesisDataStreamSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.KinesisVideoStreamRecordingSourceRuntimeConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.KinesisVideoStreamSourceRuntimeConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LambdaFunctionSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LayoutOption
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorMuxType
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorRTMPConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSinkType
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSourceConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSourceType
import Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipeline
import Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipelineSourceConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipelineSummary
import Amazonka.ChimeSdkMediaPipelines.Types.MediaConcatenationPipeline
import Amazonka.ChimeSdkMediaPipelines.Types.MediaEncoding
import Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipeline
import Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationElement
import Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationElementType
import Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationSummary
import Amazonka.ChimeSdkMediaPipelines.Types.MediaLiveConnectorPipeline
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipeline
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineSinkType
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineSourceType
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineStatus
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineStatusUpdate
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineSummary
import Amazonka.ChimeSdkMediaPipelines.Types.MeetingEventsConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.PartialResultsStability
import Amazonka.ChimeSdkMediaPipelines.Types.ParticipantRole
import Amazonka.ChimeSdkMediaPipelines.Types.PostCallAnalyticsSettings
import Amazonka.ChimeSdkMediaPipelines.Types.PresenterOnlyConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.PresenterPosition
import Amazonka.ChimeSdkMediaPipelines.Types.RealTimeAlertConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.RealTimeAlertRule
import Amazonka.ChimeSdkMediaPipelines.Types.RealTimeAlertRuleType
import Amazonka.ChimeSdkMediaPipelines.Types.RecordingFileFormat
import Amazonka.ChimeSdkMediaPipelines.Types.RecordingStreamConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ResolutionOption
import Amazonka.ChimeSdkMediaPipelines.Types.S3BucketSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.S3RecordingSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.S3RecordingSinkRuntimeConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.SelectedVideoStreams
import Amazonka.ChimeSdkMediaPipelines.Types.SentimentConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.SentimentType
import Amazonka.ChimeSdkMediaPipelines.Types.SnsTopicSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.SourceConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.SqsQueueSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.StreamChannelDefinition
import Amazonka.ChimeSdkMediaPipelines.Types.StreamConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.Tag
import Amazonka.ChimeSdkMediaPipelines.Types.TimestampRange
import Amazonka.ChimeSdkMediaPipelines.Types.TranscriptionMessagesConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.VideoArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.VideoConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.VideoMuxType
import Amazonka.ChimeSdkMediaPipelines.Types.VocabularyFilterMethod
import Amazonka.ChimeSdkMediaPipelines.Types.VoiceAnalyticsConfigurationStatus
import Amazonka.ChimeSdkMediaPipelines.Types.VoiceAnalyticsProcessorConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-07-15@ of the Amazon Chime SDK Media Pipelines SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "ChimeSdkMediaPipelines",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "media-pipelines-chime",
      Core.signingName = "chime",
      Core.version = "2021-07-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "ChimeSdkMediaPipelines",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The input parameters don\'t match the service\'s restrictions.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The request could not be processed because of conflict in the current
-- state of the resource.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The client is permanently forbidden from making the request.
_ForbiddenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | One or more of the resources in the request does not exist in the
-- system.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request exceeds the resource limit.
_ResourceLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The service encountered an unexpected error.
_ServiceFailureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceFailureException =
  Core._MatchServiceError
    defaultService
    "ServiceFailureException"
    Prelude.. Core.hasStatus 500

-- | The service is currently unavailable.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The client exceeded its request rate limit.
_ThrottledClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottledClientException =
  Core._MatchServiceError
    defaultService
    "ThrottledClientException"
    Prelude.. Core.hasStatus 429

-- | The client is not currently authorized to make the request.
_UnauthorizedClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedClientException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedClientException"
    Prelude.. Core.hasStatus 401
