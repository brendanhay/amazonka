{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideo.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Lens
  ( -- * Operations

    -- ** CreateSignalingChannel
    createSignalingChannel_channelType,
    createSignalingChannel_singleMasterConfiguration,
    createSignalingChannel_tags,
    createSignalingChannel_channelName,
    createSignalingChannelResponse_channelARN,
    createSignalingChannelResponse_httpStatus,

    -- ** CreateStream
    createStream_dataRetentionInHours,
    createStream_deviceName,
    createStream_kmsKeyId,
    createStream_mediaType,
    createStream_tags,
    createStream_streamName,
    createStreamResponse_streamARN,
    createStreamResponse_httpStatus,

    -- ** DeleteSignalingChannel
    deleteSignalingChannel_currentVersion,
    deleteSignalingChannel_channelARN,
    deleteSignalingChannelResponse_httpStatus,

    -- ** DeleteStream
    deleteStream_currentVersion,
    deleteStream_streamARN,
    deleteStreamResponse_httpStatus,

    -- ** DescribeImageGenerationConfiguration
    describeImageGenerationConfiguration_streamARN,
    describeImageGenerationConfiguration_streamName,
    describeImageGenerationConfigurationResponse_imageGenerationConfiguration,
    describeImageGenerationConfigurationResponse_httpStatus,

    -- ** DescribeNotificationConfiguration
    describeNotificationConfiguration_streamARN,
    describeNotificationConfiguration_streamName,
    describeNotificationConfigurationResponse_notificationConfiguration,
    describeNotificationConfigurationResponse_httpStatus,

    -- ** DescribeSignalingChannel
    describeSignalingChannel_channelARN,
    describeSignalingChannel_channelName,
    describeSignalingChannelResponse_channelInfo,
    describeSignalingChannelResponse_httpStatus,

    -- ** DescribeStream
    describeStream_streamARN,
    describeStream_streamName,
    describeStreamResponse_streamInfo,
    describeStreamResponse_httpStatus,

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

    -- ** ListSignalingChannels
    listSignalingChannels_channelNameCondition,
    listSignalingChannels_maxResults,
    listSignalingChannels_nextToken,
    listSignalingChannelsResponse_channelInfoList,
    listSignalingChannelsResponse_nextToken,
    listSignalingChannelsResponse_httpStatus,

    -- ** ListStreams
    listStreams_maxResults,
    listStreams_nextToken,
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

    -- ** ListTagsForStream
    listTagsForStream_nextToken,
    listTagsForStream_streamARN,
    listTagsForStream_streamName,
    listTagsForStreamResponse_nextToken,
    listTagsForStreamResponse_tags,
    listTagsForStreamResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TagStream
    tagStream_streamARN,
    tagStream_streamName,
    tagStream_tags,
    tagStreamResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeyList,
    untagResourceResponse_httpStatus,

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

    -- ** UpdateImageGenerationConfiguration
    updateImageGenerationConfiguration_imageGenerationConfiguration,
    updateImageGenerationConfiguration_streamARN,
    updateImageGenerationConfiguration_streamName,
    updateImageGenerationConfigurationResponse_httpStatus,

    -- ** UpdateNotificationConfiguration
    updateNotificationConfiguration_notificationConfiguration,
    updateNotificationConfiguration_streamARN,
    updateNotificationConfiguration_streamName,
    updateNotificationConfigurationResponse_httpStatus,

    -- ** UpdateSignalingChannel
    updateSignalingChannel_singleMasterConfiguration,
    updateSignalingChannel_channelARN,
    updateSignalingChannel_currentVersion,
    updateSignalingChannelResponse_httpStatus,

    -- ** UpdateStream
    updateStream_deviceName,
    updateStream_mediaType,
    updateStream_streamARN,
    updateStream_streamName,
    updateStream_currentVersion,
    updateStreamResponse_httpStatus,

    -- * Types

    -- ** ChannelInfo
    channelInfo_channelARN,
    channelInfo_channelName,
    channelInfo_channelStatus,
    channelInfo_channelType,
    channelInfo_creationTime,
    channelInfo_singleMasterConfiguration,
    channelInfo_version,

    -- ** ChannelNameCondition
    channelNameCondition_comparisonOperator,
    channelNameCondition_comparisonValue,

    -- ** ImageGenerationConfiguration
    imageGenerationConfiguration_formatConfig,
    imageGenerationConfiguration_heightPixels,
    imageGenerationConfiguration_widthPixels,
    imageGenerationConfiguration_status,
    imageGenerationConfiguration_imageSelectorType,
    imageGenerationConfiguration_destinationConfig,
    imageGenerationConfiguration_samplingInterval,
    imageGenerationConfiguration_format,

    -- ** ImageGenerationDestinationConfig
    imageGenerationDestinationConfig_uri,
    imageGenerationDestinationConfig_destinationRegion,

    -- ** NotificationConfiguration
    notificationConfiguration_status,
    notificationConfiguration_destinationConfig,

    -- ** NotificationDestinationConfig
    notificationDestinationConfig_uri,

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
    streamInfo_dataRetentionInHours,
    streamInfo_deviceName,
    streamInfo_kmsKeyId,
    streamInfo_mediaType,
    streamInfo_status,
    streamInfo_streamARN,
    streamInfo_streamName,
    streamInfo_version,

    -- ** StreamNameCondition
    streamNameCondition_comparisonOperator,
    streamNameCondition_comparisonValue,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.KinesisVideo.CreateSignalingChannel
import Amazonka.KinesisVideo.CreateStream
import Amazonka.KinesisVideo.DeleteSignalingChannel
import Amazonka.KinesisVideo.DeleteStream
import Amazonka.KinesisVideo.DescribeImageGenerationConfiguration
import Amazonka.KinesisVideo.DescribeNotificationConfiguration
import Amazonka.KinesisVideo.DescribeSignalingChannel
import Amazonka.KinesisVideo.DescribeStream
import Amazonka.KinesisVideo.GetDataEndpoint
import Amazonka.KinesisVideo.GetSignalingChannelEndpoint
import Amazonka.KinesisVideo.ListSignalingChannels
import Amazonka.KinesisVideo.ListStreams
import Amazonka.KinesisVideo.ListTagsForResource
import Amazonka.KinesisVideo.ListTagsForStream
import Amazonka.KinesisVideo.TagResource
import Amazonka.KinesisVideo.TagStream
import Amazonka.KinesisVideo.Types.ChannelInfo
import Amazonka.KinesisVideo.Types.ChannelNameCondition
import Amazonka.KinesisVideo.Types.ImageGenerationConfiguration
import Amazonka.KinesisVideo.Types.ImageGenerationDestinationConfig
import Amazonka.KinesisVideo.Types.NotificationConfiguration
import Amazonka.KinesisVideo.Types.NotificationDestinationConfig
import Amazonka.KinesisVideo.Types.ResourceEndpointListItem
import Amazonka.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration
import Amazonka.KinesisVideo.Types.SingleMasterConfiguration
import Amazonka.KinesisVideo.Types.StreamInfo
import Amazonka.KinesisVideo.Types.StreamNameCondition
import Amazonka.KinesisVideo.Types.Tag
import Amazonka.KinesisVideo.UntagResource
import Amazonka.KinesisVideo.UntagStream
import Amazonka.KinesisVideo.UpdateDataRetention
import Amazonka.KinesisVideo.UpdateImageGenerationConfiguration
import Amazonka.KinesisVideo.UpdateNotificationConfiguration
import Amazonka.KinesisVideo.UpdateSignalingChannel
import Amazonka.KinesisVideo.UpdateStream
