{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Lens
  ( -- * Operations

    -- ** ListTagsForStream
    listTagsForStream_nextToken,
    listTagsForStream_streamARN,
    listTagsForStream_streamName,
    listTagsForStreamResponse_nextToken,
    listTagsForStreamResponse_tags,
    listTagsForStreamResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeyList,
    untagResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetDataEndpoint
    getDataEndpoint_streamARN,
    getDataEndpoint_streamName,
    getDataEndpoint_aPIName,
    getDataEndpointResponse_dataEndpoint,
    getDataEndpointResponse_httpStatus,

    -- ** UpdateStream
    updateStream_deviceName,
    updateStream_mediaType,
    updateStream_streamARN,
    updateStream_streamName,
    updateStream_currentVersion,
    updateStreamResponse_httpStatus,

    -- ** DeleteStream
    deleteStream_currentVersion,
    deleteStream_streamARN,
    deleteStreamResponse_httpStatus,

    -- ** UntagStream
    untagStream_streamARN,
    untagStream_streamName,
    untagStream_tagKeyList,
    untagStreamResponse_httpStatus,

    -- ** UpdateDataRetention
    updateDataRetention_streamARN,
    updateDataRetention_streamName,
    updateDataRetention_currentVersion,
    updateDataRetention_operation,
    updateDataRetention_dataRetentionChangeInHours,
    updateDataRetentionResponse_httpStatus,

    -- ** UpdateSignalingChannel
    updateSignalingChannel_singleMasterConfiguration,
    updateSignalingChannel_channelARN,
    updateSignalingChannel_currentVersion,
    updateSignalingChannelResponse_httpStatus,

    -- ** DeleteSignalingChannel
    deleteSignalingChannel_currentVersion,
    deleteSignalingChannel_channelARN,
    deleteSignalingChannelResponse_httpStatus,

    -- ** ListSignalingChannels
    listSignalingChannels_channelNameCondition,
    listSignalingChannels_nextToken,
    listSignalingChannels_maxResults,
    listSignalingChannelsResponse_nextToken,
    listSignalingChannelsResponse_channelInfoList,
    listSignalingChannelsResponse_httpStatus,

    -- ** CreateSignalingChannel
    createSignalingChannel_singleMasterConfiguration,
    createSignalingChannel_channelType,
    createSignalingChannel_tags,
    createSignalingChannel_channelName,
    createSignalingChannelResponse_channelARN,
    createSignalingChannelResponse_httpStatus,

    -- ** DescribeStream
    describeStream_streamARN,
    describeStream_streamName,
    describeStreamResponse_streamInfo,
    describeStreamResponse_httpStatus,

    -- ** TagStream
    tagStream_streamARN,
    tagStream_streamName,
    tagStream_tags,
    tagStreamResponse_httpStatus,

    -- ** GetSignalingChannelEndpoint
    getSignalingChannelEndpoint_singleMasterChannelEndpointConfiguration,
    getSignalingChannelEndpoint_channelARN,
    getSignalingChannelEndpointResponse_resourceEndpointList,
    getSignalingChannelEndpointResponse_httpStatus,

    -- ** DescribeSignalingChannel
    describeSignalingChannel_channelName,
    describeSignalingChannel_channelARN,
    describeSignalingChannelResponse_channelInfo,
    describeSignalingChannelResponse_httpStatus,

    -- ** CreateStream
    createStream_dataRetentionInHours,
    createStream_kmsKeyId,
    createStream_deviceName,
    createStream_mediaType,
    createStream_tags,
    createStream_streamName,
    createStreamResponse_streamARN,
    createStreamResponse_httpStatus,

    -- ** ListStreams
    listStreams_nextToken,
    listStreams_maxResults,
    listStreams_streamNameCondition,
    listStreamsResponse_nextToken,
    listStreamsResponse_streamInfoList,
    listStreamsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** ChannelInfo
    channelInfo_channelName,
    channelInfo_creationTime,
    channelInfo_singleMasterConfiguration,
    channelInfo_channelType,
    channelInfo_version,
    channelInfo_channelStatus,
    channelInfo_channelARN,

    -- ** ChannelNameCondition
    channelNameCondition_comparisonOperator,
    channelNameCondition_comparisonValue,

    -- ** ResourceEndpointListItem
    resourceEndpointListItem_resourceEndpoint,
    resourceEndpointListItem_protocol,

    -- ** SingleMasterChannelEndpointConfiguration
    singleMasterChannelEndpointConfiguration_protocols,
    singleMasterChannelEndpointConfiguration_role,

    -- ** SingleMasterConfiguration
    singleMasterConfiguration_messageTtlSeconds,

    -- ** StreamInfo
    streamInfo_status,
    streamInfo_creationTime,
    streamInfo_dataRetentionInHours,
    streamInfo_version,
    streamInfo_kmsKeyId,
    streamInfo_deviceName,
    streamInfo_mediaType,
    streamInfo_streamARN,
    streamInfo_streamName,

    -- ** StreamNameCondition
    streamNameCondition_comparisonOperator,
    streamNameCondition_comparisonValue,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.KinesisVideo.CreateSignalingChannel
import Network.AWS.KinesisVideo.CreateStream
import Network.AWS.KinesisVideo.DeleteSignalingChannel
import Network.AWS.KinesisVideo.DeleteStream
import Network.AWS.KinesisVideo.DescribeSignalingChannel
import Network.AWS.KinesisVideo.DescribeStream
import Network.AWS.KinesisVideo.GetDataEndpoint
import Network.AWS.KinesisVideo.GetSignalingChannelEndpoint
import Network.AWS.KinesisVideo.ListSignalingChannels
import Network.AWS.KinesisVideo.ListStreams
import Network.AWS.KinesisVideo.ListTagsForResource
import Network.AWS.KinesisVideo.ListTagsForStream
import Network.AWS.KinesisVideo.TagResource
import Network.AWS.KinesisVideo.TagStream
import Network.AWS.KinesisVideo.Types.ChannelInfo
import Network.AWS.KinesisVideo.Types.ChannelNameCondition
import Network.AWS.KinesisVideo.Types.ResourceEndpointListItem
import Network.AWS.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration
import Network.AWS.KinesisVideo.Types.SingleMasterConfiguration
import Network.AWS.KinesisVideo.Types.StreamInfo
import Network.AWS.KinesisVideo.Types.StreamNameCondition
import Network.AWS.KinesisVideo.Types.Tag
import Network.AWS.KinesisVideo.UntagResource
import Network.AWS.KinesisVideo.UntagStream
import Network.AWS.KinesisVideo.UpdateDataRetention
import Network.AWS.KinesisVideo.UpdateSignalingChannel
import Network.AWS.KinesisVideo.UpdateStream
