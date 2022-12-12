{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IVS.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Lens
  ( -- * Operations

    -- ** BatchGetChannel
    batchGetChannel_arns,
    batchGetChannelResponse_channels,
    batchGetChannelResponse_errors,
    batchGetChannelResponse_httpStatus,

    -- ** BatchGetStreamKey
    batchGetStreamKey_arns,
    batchGetStreamKeyResponse_errors,
    batchGetStreamKeyResponse_streamKeys,
    batchGetStreamKeyResponse_httpStatus,

    -- ** CreateChannel
    createChannel_authorized,
    createChannel_latencyMode,
    createChannel_name,
    createChannel_recordingConfigurationArn,
    createChannel_tags,
    createChannel_type,
    createChannelResponse_channel,
    createChannelResponse_streamKey,
    createChannelResponse_httpStatus,

    -- ** CreateRecordingConfiguration
    createRecordingConfiguration_name,
    createRecordingConfiguration_recordingReconnectWindowSeconds,
    createRecordingConfiguration_tags,
    createRecordingConfiguration_thumbnailConfiguration,
    createRecordingConfiguration_destinationConfiguration,
    createRecordingConfigurationResponse_recordingConfiguration,
    createRecordingConfigurationResponse_httpStatus,

    -- ** CreateStreamKey
    createStreamKey_tags,
    createStreamKey_channelArn,
    createStreamKeyResponse_streamKey,
    createStreamKeyResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_arn,

    -- ** DeletePlaybackKeyPair
    deletePlaybackKeyPair_arn,
    deletePlaybackKeyPairResponse_httpStatus,

    -- ** DeleteRecordingConfiguration
    deleteRecordingConfiguration_arn,

    -- ** DeleteStreamKey
    deleteStreamKey_arn,

    -- ** GetChannel
    getChannel_arn,
    getChannelResponse_channel,
    getChannelResponse_httpStatus,

    -- ** GetPlaybackKeyPair
    getPlaybackKeyPair_arn,
    getPlaybackKeyPairResponse_keyPair,
    getPlaybackKeyPairResponse_httpStatus,

    -- ** GetRecordingConfiguration
    getRecordingConfiguration_arn,
    getRecordingConfigurationResponse_recordingConfiguration,
    getRecordingConfigurationResponse_httpStatus,

    -- ** GetStream
    getStream_channelArn,
    getStreamResponse_stream,
    getStreamResponse_httpStatus,

    -- ** GetStreamKey
    getStreamKey_arn,
    getStreamKeyResponse_streamKey,
    getStreamKeyResponse_httpStatus,

    -- ** GetStreamSession
    getStreamSession_streamId,
    getStreamSession_channelArn,
    getStreamSessionResponse_streamSession,
    getStreamSessionResponse_httpStatus,

    -- ** ImportPlaybackKeyPair
    importPlaybackKeyPair_name,
    importPlaybackKeyPair_tags,
    importPlaybackKeyPair_publicKeyMaterial,
    importPlaybackKeyPairResponse_keyPair,
    importPlaybackKeyPairResponse_httpStatus,

    -- ** ListChannels
    listChannels_filterByName,
    listChannels_filterByRecordingConfigurationArn,
    listChannels_maxResults,
    listChannels_nextToken,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,
    listChannelsResponse_channels,

    -- ** ListPlaybackKeyPairs
    listPlaybackKeyPairs_maxResults,
    listPlaybackKeyPairs_nextToken,
    listPlaybackKeyPairsResponse_nextToken,
    listPlaybackKeyPairsResponse_httpStatus,
    listPlaybackKeyPairsResponse_keyPairs,

    -- ** ListRecordingConfigurations
    listRecordingConfigurations_maxResults,
    listRecordingConfigurations_nextToken,
    listRecordingConfigurationsResponse_nextToken,
    listRecordingConfigurationsResponse_httpStatus,
    listRecordingConfigurationsResponse_recordingConfigurations,

    -- ** ListStreamKeys
    listStreamKeys_maxResults,
    listStreamKeys_nextToken,
    listStreamKeys_channelArn,
    listStreamKeysResponse_nextToken,
    listStreamKeysResponse_httpStatus,
    listStreamKeysResponse_streamKeys,

    -- ** ListStreamSessions
    listStreamSessions_maxResults,
    listStreamSessions_nextToken,
    listStreamSessions_channelArn,
    listStreamSessionsResponse_nextToken,
    listStreamSessionsResponse_httpStatus,
    listStreamSessionsResponse_streamSessions,

    -- ** ListStreams
    listStreams_filterBy,
    listStreams_maxResults,
    listStreams_nextToken,
    listStreamsResponse_nextToken,
    listStreamsResponse_httpStatus,
    listStreamsResponse_streams,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** PutMetadata
    putMetadata_channelArn,
    putMetadata_metadata,

    -- ** StopStream
    stopStream_channelArn,
    stopStreamResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateChannel
    updateChannel_authorized,
    updateChannel_latencyMode,
    updateChannel_name,
    updateChannel_recordingConfigurationArn,
    updateChannel_type,
    updateChannel_arn,
    updateChannelResponse_channel,
    updateChannelResponse_httpStatus,

    -- * Types

    -- ** AudioConfiguration
    audioConfiguration_channels,
    audioConfiguration_codec,
    audioConfiguration_sampleRate,
    audioConfiguration_targetBitrate,

    -- ** BatchError
    batchError_arn,
    batchError_code,
    batchError_message,

    -- ** Channel
    channel_arn,
    channel_authorized,
    channel_ingestEndpoint,
    channel_latencyMode,
    channel_name,
    channel_playbackUrl,
    channel_recordingConfigurationArn,
    channel_tags,
    channel_type,

    -- ** ChannelSummary
    channelSummary_arn,
    channelSummary_authorized,
    channelSummary_latencyMode,
    channelSummary_name,
    channelSummary_recordingConfigurationArn,
    channelSummary_tags,

    -- ** DestinationConfiguration
    destinationConfiguration_s3,

    -- ** IngestConfiguration
    ingestConfiguration_audio,
    ingestConfiguration_video,

    -- ** PlaybackKeyPair
    playbackKeyPair_arn,
    playbackKeyPair_fingerprint,
    playbackKeyPair_name,
    playbackKeyPair_tags,

    -- ** PlaybackKeyPairSummary
    playbackKeyPairSummary_arn,
    playbackKeyPairSummary_name,
    playbackKeyPairSummary_tags,

    -- ** RecordingConfiguration
    recordingConfiguration_name,
    recordingConfiguration_recordingReconnectWindowSeconds,
    recordingConfiguration_tags,
    recordingConfiguration_thumbnailConfiguration,
    recordingConfiguration_arn,
    recordingConfiguration_destinationConfiguration,
    recordingConfiguration_state,

    -- ** RecordingConfigurationSummary
    recordingConfigurationSummary_name,
    recordingConfigurationSummary_tags,
    recordingConfigurationSummary_arn,
    recordingConfigurationSummary_destinationConfiguration,
    recordingConfigurationSummary_state,

    -- ** S3DestinationConfiguration
    s3DestinationConfiguration_bucketName,

    -- ** Stream
    stream_channelArn,
    stream_health,
    stream_playbackUrl,
    stream_startTime,
    stream_state,
    stream_streamId,
    stream_viewerCount,

    -- ** StreamEvent
    streamEvent_eventTime,
    streamEvent_name,
    streamEvent_type,

    -- ** StreamFilters
    streamFilters_health,

    -- ** StreamKey
    streamKey_arn,
    streamKey_channelArn,
    streamKey_tags,
    streamKey_value,

    -- ** StreamKeySummary
    streamKeySummary_arn,
    streamKeySummary_channelArn,
    streamKeySummary_tags,

    -- ** StreamSession
    streamSession_channel,
    streamSession_endTime,
    streamSession_ingestConfiguration,
    streamSession_recordingConfiguration,
    streamSession_startTime,
    streamSession_streamId,
    streamSession_truncatedEvents,

    -- ** StreamSessionSummary
    streamSessionSummary_endTime,
    streamSessionSummary_hasErrorEvent,
    streamSessionSummary_startTime,
    streamSessionSummary_streamId,

    -- ** StreamSummary
    streamSummary_channelArn,
    streamSummary_health,
    streamSummary_startTime,
    streamSummary_state,
    streamSummary_streamId,
    streamSummary_viewerCount,

    -- ** ThumbnailConfiguration
    thumbnailConfiguration_recordingMode,
    thumbnailConfiguration_targetIntervalSeconds,

    -- ** VideoConfiguration
    videoConfiguration_avcLevel,
    videoConfiguration_avcProfile,
    videoConfiguration_codec,
    videoConfiguration_encoder,
    videoConfiguration_targetBitrate,
    videoConfiguration_targetFramerate,
    videoConfiguration_videoHeight,
    videoConfiguration_videoWidth,
  )
where

import Amazonka.IVS.BatchGetChannel
import Amazonka.IVS.BatchGetStreamKey
import Amazonka.IVS.CreateChannel
import Amazonka.IVS.CreateRecordingConfiguration
import Amazonka.IVS.CreateStreamKey
import Amazonka.IVS.DeleteChannel
import Amazonka.IVS.DeletePlaybackKeyPair
import Amazonka.IVS.DeleteRecordingConfiguration
import Amazonka.IVS.DeleteStreamKey
import Amazonka.IVS.GetChannel
import Amazonka.IVS.GetPlaybackKeyPair
import Amazonka.IVS.GetRecordingConfiguration
import Amazonka.IVS.GetStream
import Amazonka.IVS.GetStreamKey
import Amazonka.IVS.GetStreamSession
import Amazonka.IVS.ImportPlaybackKeyPair
import Amazonka.IVS.ListChannels
import Amazonka.IVS.ListPlaybackKeyPairs
import Amazonka.IVS.ListRecordingConfigurations
import Amazonka.IVS.ListStreamKeys
import Amazonka.IVS.ListStreamSessions
import Amazonka.IVS.ListStreams
import Amazonka.IVS.ListTagsForResource
import Amazonka.IVS.PutMetadata
import Amazonka.IVS.StopStream
import Amazonka.IVS.TagResource
import Amazonka.IVS.Types.AudioConfiguration
import Amazonka.IVS.Types.BatchError
import Amazonka.IVS.Types.Channel
import Amazonka.IVS.Types.ChannelSummary
import Amazonka.IVS.Types.DestinationConfiguration
import Amazonka.IVS.Types.IngestConfiguration
import Amazonka.IVS.Types.PlaybackKeyPair
import Amazonka.IVS.Types.PlaybackKeyPairSummary
import Amazonka.IVS.Types.RecordingConfiguration
import Amazonka.IVS.Types.RecordingConfigurationSummary
import Amazonka.IVS.Types.S3DestinationConfiguration
import Amazonka.IVS.Types.Stream
import Amazonka.IVS.Types.StreamEvent
import Amazonka.IVS.Types.StreamFilters
import Amazonka.IVS.Types.StreamKey
import Amazonka.IVS.Types.StreamKeySummary
import Amazonka.IVS.Types.StreamSession
import Amazonka.IVS.Types.StreamSessionSummary
import Amazonka.IVS.Types.StreamSummary
import Amazonka.IVS.Types.ThumbnailConfiguration
import Amazonka.IVS.Types.VideoConfiguration
import Amazonka.IVS.UntagResource
import Amazonka.IVS.UpdateChannel
