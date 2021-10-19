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

    -- ** ListSignalingChannels
    listSignalingChannels_channelNameCondition,
    listSignalingChannels_nextToken,
    listSignalingChannels_maxResults,
    listSignalingChannelsResponse_channelInfoList,
    listSignalingChannelsResponse_nextToken,
    listSignalingChannelsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

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

    -- ** GetDataEndpoint
    getDataEndpoint_streamARN,
    getDataEndpoint_streamName,
    getDataEndpoint_aPIName,
    getDataEndpointResponse_dataEndpoint,
    getDataEndpointResponse_httpStatus,

    -- ** GetSignalingChannelEndpoint
    getSignalingChannelEndpoint_singleMasterChannelEndpointConfiguration,
    getSignalingChannelEndpoint_channelARN,
    getSignalingChannelEndpointResponse_resourceEndpointList,
    getSignalingChannelEndpointResponse_httpStatus,

    -- ** ListTagsForStream
    listTagsForStream_streamARN,
    listTagsForStream_nextToken,
    listTagsForStream_streamName,
    listTagsForStreamResponse_nextToken,
    listTagsForStreamResponse_tags,
    listTagsForStreamResponse_httpStatus,

    -- ** DeleteSignalingChannel
    deleteSignalingChannel_currentVersion,
    deleteSignalingChannel_channelARN,
    deleteSignalingChannelResponse_httpStatus,

    -- ** UpdateSignalingChannel
    updateSignalingChannel_singleMasterConfiguration,
    updateSignalingChannel_channelARN,
    updateSignalingChannel_currentVersion,
    updateSignalingChannelResponse_httpStatus,

    -- ** UpdateStream
    updateStream_mediaType,
    updateStream_streamARN,
    updateStream_deviceName,
    updateStream_streamName,
    updateStream_currentVersion,
    updateStreamResponse_httpStatus,

    -- ** DeleteStream
    deleteStream_currentVersion,
    deleteStream_streamARN,
    deleteStreamResponse_httpStatus,

    -- ** ListStreams
    listStreams_nextToken,
    listStreams_streamNameCondition,
    listStreams_maxResults,
    listStreamsResponse_streamInfoList,
    listStreamsResponse_nextToken,
    listStreamsResponse_httpStatus,

    -- ** CreateStream
    createStream_mediaType,
    createStream_dataRetentionInHours,
    createStream_kmsKeyId,
    createStream_deviceName,
    createStream_tags,
    createStream_streamName,
    createStreamResponse_streamARN,
    createStreamResponse_httpStatus,

    -- ** DescribeSignalingChannel
    describeSignalingChannel_channelARN,
    describeSignalingChannel_channelName,
    describeSignalingChannelResponse_channelInfo,
    describeSignalingChannelResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeyList,
    untagResourceResponse_httpStatus,

    -- ** TagStream
    tagStream_streamARN,
    tagStream_streamName,
    tagStream_tags,
    tagStreamResponse_httpStatus,

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

    -- * Types

    -- ** ChannelInfo
    channelInfo_creationTime,
    channelInfo_channelStatus,
    channelInfo_channelARN,
    channelInfo_singleMasterConfiguration,
    channelInfo_channelName,
    channelInfo_version,
    channelInfo_channelType,

    -- ** ChannelNameCondition
    channelNameCondition_comparisonOperator,
    channelNameCondition_comparisonValue,

    -- ** ResourceEndpointListItem
    resourceEndpointListItem_protocol,
    resourceEndpointListItem_resourceEndpoint,

    -- ** SingleMasterChannelEndpointConfiguration
    singleMasterChannelEndpointConfiguration_protocols,
    singleMasterChannelEndpointConfiguration_role,

    -- ** SingleMasterConfiguration
    singleMasterConfiguration_messageTtlSeconds,

    -- ** StreamInfo
    streamInfo_creationTime,
    streamInfo_status,
    streamInfo_mediaType,
    streamInfo_dataRetentionInHours,
    streamInfo_streamARN,
    streamInfo_kmsKeyId,
    streamInfo_deviceName,
    streamInfo_version,
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
