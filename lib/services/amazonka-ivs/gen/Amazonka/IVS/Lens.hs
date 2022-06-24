{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IVS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    createChannel_tags,
    createChannel_name,
    createChannel_type,
    createChannel_latencyMode,
    createChannel_authorized,
    createChannel_recordingConfigurationArn,
    createChannelResponse_channel,
    createChannelResponse_streamKey,
    createChannelResponse_httpStatus,

    -- ** CreateRecordingConfiguration
    createRecordingConfiguration_tags,
    createRecordingConfiguration_name,
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

    -- ** ImportPlaybackKeyPair
    importPlaybackKeyPair_tags,
    importPlaybackKeyPair_name,
    importPlaybackKeyPair_publicKeyMaterial,
    importPlaybackKeyPairResponse_keyPair,
    importPlaybackKeyPairResponse_httpStatus,

    -- ** ListChannels
    listChannels_nextToken,
    listChannels_filterByName,
    listChannels_maxResults,
    listChannels_filterByRecordingConfigurationArn,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,
    listChannelsResponse_channels,

    -- ** ListPlaybackKeyPairs
    listPlaybackKeyPairs_nextToken,
    listPlaybackKeyPairs_maxResults,
    listPlaybackKeyPairsResponse_nextToken,
    listPlaybackKeyPairsResponse_httpStatus,
    listPlaybackKeyPairsResponse_keyPairs,

    -- ** ListRecordingConfigurations
    listRecordingConfigurations_nextToken,
    listRecordingConfigurations_maxResults,
    listRecordingConfigurationsResponse_nextToken,
    listRecordingConfigurationsResponse_httpStatus,
    listRecordingConfigurationsResponse_recordingConfigurations,

    -- ** ListStreamKeys
    listStreamKeys_nextToken,
    listStreamKeys_maxResults,
    listStreamKeys_channelArn,
    listStreamKeysResponse_nextToken,
    listStreamKeysResponse_httpStatus,
    listStreamKeysResponse_streamKeys,

    -- ** ListStreams
    listStreams_nextToken,
    listStreams_maxResults,
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
    updateChannel_name,
    updateChannel_type,
    updateChannel_latencyMode,
    updateChannel_authorized,
    updateChannel_recordingConfigurationArn,
    updateChannel_arn,
    updateChannelResponse_channel,
    updateChannelResponse_httpStatus,

    -- * Types

    -- ** BatchError
    batchError_message,
    batchError_code,
    batchError_arn,

    -- ** Channel
    channel_tags,
    channel_name,
    channel_type,
    channel_latencyMode,
    channel_arn,
    channel_authorized,
    channel_playbackUrl,
    channel_ingestEndpoint,
    channel_recordingConfigurationArn,

    -- ** ChannelSummary
    channelSummary_tags,
    channelSummary_name,
    channelSummary_latencyMode,
    channelSummary_arn,
    channelSummary_authorized,
    channelSummary_recordingConfigurationArn,

    -- ** DestinationConfiguration
    destinationConfiguration_s3,

    -- ** PlaybackKeyPair
    playbackKeyPair_tags,
    playbackKeyPair_name,
    playbackKeyPair_arn,
    playbackKeyPair_fingerprint,

    -- ** PlaybackKeyPairSummary
    playbackKeyPairSummary_tags,
    playbackKeyPairSummary_name,
    playbackKeyPairSummary_arn,

    -- ** RecordingConfiguration
    recordingConfiguration_tags,
    recordingConfiguration_name,
    recordingConfiguration_arn,
    recordingConfiguration_destinationConfiguration,
    recordingConfiguration_state,

    -- ** RecordingConfigurationSummary
    recordingConfigurationSummary_tags,
    recordingConfigurationSummary_name,
    recordingConfigurationSummary_arn,
    recordingConfigurationSummary_destinationConfiguration,
    recordingConfigurationSummary_state,

    -- ** S3DestinationConfiguration
    s3DestinationConfiguration_bucketName,

    -- ** Stream
    stream_viewerCount,
    stream_channelArn,
    stream_state,
    stream_playbackUrl,
    stream_health,
    stream_startTime,

    -- ** StreamKey
    streamKey_tags,
    streamKey_arn,
    streamKey_channelArn,
    streamKey_value,

    -- ** StreamKeySummary
    streamKeySummary_tags,
    streamKeySummary_arn,
    streamKeySummary_channelArn,

    -- ** StreamSummary
    streamSummary_viewerCount,
    streamSummary_channelArn,
    streamSummary_state,
    streamSummary_health,
    streamSummary_startTime,
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
import Amazonka.IVS.ImportPlaybackKeyPair
import Amazonka.IVS.ListChannels
import Amazonka.IVS.ListPlaybackKeyPairs
import Amazonka.IVS.ListRecordingConfigurations
import Amazonka.IVS.ListStreamKeys
import Amazonka.IVS.ListStreams
import Amazonka.IVS.ListTagsForResource
import Amazonka.IVS.PutMetadata
import Amazonka.IVS.StopStream
import Amazonka.IVS.TagResource
import Amazonka.IVS.Types.BatchError
import Amazonka.IVS.Types.Channel
import Amazonka.IVS.Types.ChannelSummary
import Amazonka.IVS.Types.DestinationConfiguration
import Amazonka.IVS.Types.PlaybackKeyPair
import Amazonka.IVS.Types.PlaybackKeyPairSummary
import Amazonka.IVS.Types.RecordingConfiguration
import Amazonka.IVS.Types.RecordingConfigurationSummary
import Amazonka.IVS.Types.S3DestinationConfiguration
import Amazonka.IVS.Types.Stream
import Amazonka.IVS.Types.StreamKey
import Amazonka.IVS.Types.StreamKeySummary
import Amazonka.IVS.Types.StreamSummary
import Amazonka.IVS.UntagResource
import Amazonka.IVS.UpdateChannel
