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

    -- ** PutMetadata
    putMetadata_channelArn,
    putMetadata_metadata,

    -- ** ListRecordingConfigurations
    listRecordingConfigurations_nextToken,
    listRecordingConfigurations_maxResults,
    listRecordingConfigurationsResponse_nextToken,
    listRecordingConfigurationsResponse_httpStatus,
    listRecordingConfigurationsResponse_recordingConfigurations,

    -- ** CreateStreamKey
    createStreamKey_tags,
    createStreamKey_channelArn,
    createStreamKeyResponse_streamKey,
    createStreamKeyResponse_httpStatus,

    -- ** ImportPlaybackKeyPair
    importPlaybackKeyPair_name,
    importPlaybackKeyPair_tags,
    importPlaybackKeyPair_publicKeyMaterial,
    importPlaybackKeyPairResponse_keyPair,
    importPlaybackKeyPairResponse_httpStatus,

    -- ** ListChannels
    listChannels_filterByName,
    listChannels_nextToken,
    listChannels_filterByRecordingConfigurationArn,
    listChannels_maxResults,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,
    listChannelsResponse_channels,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** DeleteChannel
    deleteChannel_arn,

    -- ** UpdateChannel
    updateChannel_authorized,
    updateChannel_latencyMode,
    updateChannel_name,
    updateChannel_recordingConfigurationArn,
    updateChannel_type,
    updateChannel_arn,
    updateChannelResponse_channel,
    updateChannelResponse_httpStatus,

    -- ** GetStreamKey
    getStreamKey_arn,
    getStreamKeyResponse_streamKey,
    getStreamKeyResponse_httpStatus,

    -- ** DeletePlaybackKeyPair
    deletePlaybackKeyPair_arn,
    deletePlaybackKeyPairResponse_httpStatus,

    -- ** BatchGetStreamKey
    batchGetStreamKey_arns,
    batchGetStreamKeyResponse_streamKeys,
    batchGetStreamKeyResponse_errors,
    batchGetStreamKeyResponse_httpStatus,

    -- ** GetPlaybackKeyPair
    getPlaybackKeyPair_arn,
    getPlaybackKeyPairResponse_keyPair,
    getPlaybackKeyPairResponse_httpStatus,

    -- ** DeleteRecordingConfiguration
    deleteRecordingConfiguration_arn,

    -- ** StopStream
    stopStream_channelArn,
    stopStreamResponse_httpStatus,

    -- ** CreateChannel
    createChannel_authorized,
    createChannel_latencyMode,
    createChannel_name,
    createChannel_recordingConfigurationArn,
    createChannel_type,
    createChannel_tags,
    createChannelResponse_channel,
    createChannelResponse_streamKey,
    createChannelResponse_httpStatus,

    -- ** DeleteStreamKey
    deleteStreamKey_arn,

    -- ** GetStream
    getStream_channelArn,
    getStreamResponse_stream,
    getStreamResponse_httpStatus,

    -- ** ListStreamKeys
    listStreamKeys_nextToken,
    listStreamKeys_maxResults,
    listStreamKeys_channelArn,
    listStreamKeysResponse_nextToken,
    listStreamKeysResponse_httpStatus,
    listStreamKeysResponse_streamKeys,

    -- ** GetChannel
    getChannel_arn,
    getChannelResponse_channel,
    getChannelResponse_httpStatus,

    -- ** ListStreams
    listStreams_nextToken,
    listStreams_maxResults,
    listStreamsResponse_nextToken,
    listStreamsResponse_httpStatus,
    listStreamsResponse_streams,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetRecordingConfiguration
    getRecordingConfiguration_arn,
    getRecordingConfigurationResponse_recordingConfiguration,
    getRecordingConfigurationResponse_httpStatus,

    -- ** ListPlaybackKeyPairs
    listPlaybackKeyPairs_nextToken,
    listPlaybackKeyPairs_maxResults,
    listPlaybackKeyPairsResponse_nextToken,
    listPlaybackKeyPairsResponse_httpStatus,
    listPlaybackKeyPairsResponse_keyPairs,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateRecordingConfiguration
    createRecordingConfiguration_name,
    createRecordingConfiguration_tags,
    createRecordingConfiguration_destinationConfiguration,
    createRecordingConfigurationResponse_recordingConfiguration,
    createRecordingConfigurationResponse_httpStatus,

    -- ** BatchGetChannel
    batchGetChannel_arns,
    batchGetChannelResponse_channels,
    batchGetChannelResponse_errors,
    batchGetChannelResponse_httpStatus,

    -- * Types

    -- ** BatchError
    batchError_arn,
    batchError_code,
    batchError_message,

    -- ** Channel
    channel_playbackUrl,
    channel_authorized,
    channel_arn,
    channel_latencyMode,
    channel_name,
    channel_recordingConfigurationArn,
    channel_type,
    channel_tags,
    channel_ingestEndpoint,

    -- ** ChannelSummary
    channelSummary_authorized,
    channelSummary_arn,
    channelSummary_latencyMode,
    channelSummary_name,
    channelSummary_recordingConfigurationArn,
    channelSummary_tags,

    -- ** DestinationConfiguration
    destinationConfiguration_s3,

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
    recordingConfiguration_tags,
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
    stream_playbackUrl,
    stream_state,
    stream_startTime,
    stream_channelArn,
    stream_viewerCount,
    stream_health,

    -- ** StreamKey
    streamKey_arn,
    streamKey_value,
    streamKey_channelArn,
    streamKey_tags,

    -- ** StreamKeySummary
    streamKeySummary_arn,
    streamKeySummary_channelArn,
    streamKeySummary_tags,

    -- ** StreamSummary
    streamSummary_state,
    streamSummary_startTime,
    streamSummary_channelArn,
    streamSummary_viewerCount,
    streamSummary_health,
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
