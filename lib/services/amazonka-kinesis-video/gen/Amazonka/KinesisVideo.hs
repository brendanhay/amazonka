{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.KinesisVideo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-09-30@ of the AWS service descriptions, licensed under Apache 2.0.
module Amazonka.KinesisVideo
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AccountChannelLimitExceededException
    _AccountChannelLimitExceededException,

    -- ** AccountStreamLimitExceededException
    _AccountStreamLimitExceededException,

    -- ** ClientLimitExceededException
    _ClientLimitExceededException,

    -- ** DeviceStreamLimitExceededException
    _DeviceStreamLimitExceededException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** InvalidDeviceException
    _InvalidDeviceException,

    -- ** InvalidResourceFormatException
    _InvalidResourceFormatException,

    -- ** NoDataRetentionException
    _NoDataRetentionException,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** StreamEdgeConfigurationNotFoundException
    _StreamEdgeConfigurationNotFoundException,

    -- ** TagsPerResourceExceededLimitException
    _TagsPerResourceExceededLimitException,

    -- ** VersionMismatchException
    _VersionMismatchException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateSignalingChannel
    CreateSignalingChannel (CreateSignalingChannel'),
    newCreateSignalingChannel,
    CreateSignalingChannelResponse (CreateSignalingChannelResponse'),
    newCreateSignalingChannelResponse,

    -- ** CreateStream
    CreateStream (CreateStream'),
    newCreateStream,
    CreateStreamResponse (CreateStreamResponse'),
    newCreateStreamResponse,

    -- ** DeleteSignalingChannel
    DeleteSignalingChannel (DeleteSignalingChannel'),
    newDeleteSignalingChannel,
    DeleteSignalingChannelResponse (DeleteSignalingChannelResponse'),
    newDeleteSignalingChannelResponse,

    -- ** DeleteStream
    DeleteStream (DeleteStream'),
    newDeleteStream,
    DeleteStreamResponse (DeleteStreamResponse'),
    newDeleteStreamResponse,

    -- ** DescribeEdgeConfiguration
    DescribeEdgeConfiguration (DescribeEdgeConfiguration'),
    newDescribeEdgeConfiguration,
    DescribeEdgeConfigurationResponse (DescribeEdgeConfigurationResponse'),
    newDescribeEdgeConfigurationResponse,

    -- ** DescribeImageGenerationConfiguration
    DescribeImageGenerationConfiguration (DescribeImageGenerationConfiguration'),
    newDescribeImageGenerationConfiguration,
    DescribeImageGenerationConfigurationResponse (DescribeImageGenerationConfigurationResponse'),
    newDescribeImageGenerationConfigurationResponse,

    -- ** DescribeMappedResourceConfiguration (Paginated)
    DescribeMappedResourceConfiguration (DescribeMappedResourceConfiguration'),
    newDescribeMappedResourceConfiguration,
    DescribeMappedResourceConfigurationResponse (DescribeMappedResourceConfigurationResponse'),
    newDescribeMappedResourceConfigurationResponse,

    -- ** DescribeMediaStorageConfiguration
    DescribeMediaStorageConfiguration (DescribeMediaStorageConfiguration'),
    newDescribeMediaStorageConfiguration,
    DescribeMediaStorageConfigurationResponse (DescribeMediaStorageConfigurationResponse'),
    newDescribeMediaStorageConfigurationResponse,

    -- ** DescribeNotificationConfiguration
    DescribeNotificationConfiguration (DescribeNotificationConfiguration'),
    newDescribeNotificationConfiguration,
    DescribeNotificationConfigurationResponse (DescribeNotificationConfigurationResponse'),
    newDescribeNotificationConfigurationResponse,

    -- ** DescribeSignalingChannel
    DescribeSignalingChannel (DescribeSignalingChannel'),
    newDescribeSignalingChannel,
    DescribeSignalingChannelResponse (DescribeSignalingChannelResponse'),
    newDescribeSignalingChannelResponse,

    -- ** DescribeStream
    DescribeStream (DescribeStream'),
    newDescribeStream,
    DescribeStreamResponse (DescribeStreamResponse'),
    newDescribeStreamResponse,

    -- ** GetDataEndpoint
    GetDataEndpoint (GetDataEndpoint'),
    newGetDataEndpoint,
    GetDataEndpointResponse (GetDataEndpointResponse'),
    newGetDataEndpointResponse,

    -- ** GetSignalingChannelEndpoint
    GetSignalingChannelEndpoint (GetSignalingChannelEndpoint'),
    newGetSignalingChannelEndpoint,
    GetSignalingChannelEndpointResponse (GetSignalingChannelEndpointResponse'),
    newGetSignalingChannelEndpointResponse,

    -- ** ListSignalingChannels (Paginated)
    ListSignalingChannels (ListSignalingChannels'),
    newListSignalingChannels,
    ListSignalingChannelsResponse (ListSignalingChannelsResponse'),
    newListSignalingChannelsResponse,

    -- ** ListStreams (Paginated)
    ListStreams (ListStreams'),
    newListStreams,
    ListStreamsResponse (ListStreamsResponse'),
    newListStreamsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTagsForStream
    ListTagsForStream (ListTagsForStream'),
    newListTagsForStream,
    ListTagsForStreamResponse (ListTagsForStreamResponse'),
    newListTagsForStreamResponse,

    -- ** StartEdgeConfigurationUpdate
    StartEdgeConfigurationUpdate (StartEdgeConfigurationUpdate'),
    newStartEdgeConfigurationUpdate,
    StartEdgeConfigurationUpdateResponse (StartEdgeConfigurationUpdateResponse'),
    newStartEdgeConfigurationUpdateResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TagStream
    TagStream (TagStream'),
    newTagStream,
    TagStreamResponse (TagStreamResponse'),
    newTagStreamResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UntagStream
    UntagStream (UntagStream'),
    newUntagStream,
    UntagStreamResponse (UntagStreamResponse'),
    newUntagStreamResponse,

    -- ** UpdateDataRetention
    UpdateDataRetention (UpdateDataRetention'),
    newUpdateDataRetention,
    UpdateDataRetentionResponse (UpdateDataRetentionResponse'),
    newUpdateDataRetentionResponse,

    -- ** UpdateImageGenerationConfiguration
    UpdateImageGenerationConfiguration (UpdateImageGenerationConfiguration'),
    newUpdateImageGenerationConfiguration,
    UpdateImageGenerationConfigurationResponse (UpdateImageGenerationConfigurationResponse'),
    newUpdateImageGenerationConfigurationResponse,

    -- ** UpdateMediaStorageConfiguration
    UpdateMediaStorageConfiguration (UpdateMediaStorageConfiguration'),
    newUpdateMediaStorageConfiguration,
    UpdateMediaStorageConfigurationResponse (UpdateMediaStorageConfigurationResponse'),
    newUpdateMediaStorageConfigurationResponse,

    -- ** UpdateNotificationConfiguration
    UpdateNotificationConfiguration (UpdateNotificationConfiguration'),
    newUpdateNotificationConfiguration,
    UpdateNotificationConfigurationResponse (UpdateNotificationConfigurationResponse'),
    newUpdateNotificationConfigurationResponse,

    -- ** UpdateSignalingChannel
    UpdateSignalingChannel (UpdateSignalingChannel'),
    newUpdateSignalingChannel,
    UpdateSignalingChannelResponse (UpdateSignalingChannelResponse'),
    newUpdateSignalingChannelResponse,

    -- ** UpdateStream
    UpdateStream (UpdateStream'),
    newUpdateStream,
    UpdateStreamResponse (UpdateStreamResponse'),
    newUpdateStreamResponse,

    -- * Types

    -- ** APIName
    APIName (..),

    -- ** ChannelProtocol
    ChannelProtocol (..),

    -- ** ChannelRole
    ChannelRole (..),

    -- ** ChannelType
    ChannelType (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** ConfigurationStatus
    ConfigurationStatus (..),

    -- ** Format
    Format (..),

    -- ** FormatConfigKey
    FormatConfigKey (..),

    -- ** ImageSelectorType
    ImageSelectorType (..),

    -- ** MediaStorageConfigurationStatus
    MediaStorageConfigurationStatus (..),

    -- ** MediaUriType
    MediaUriType (..),

    -- ** StrategyOnFullSize
    StrategyOnFullSize (..),

    -- ** StreamStatus
    StreamStatus (..),

    -- ** SyncStatus
    SyncStatus (..),

    -- ** UpdateDataRetentionOperation
    UpdateDataRetentionOperation (..),

    -- ** ChannelInfo
    ChannelInfo (ChannelInfo'),
    newChannelInfo,

    -- ** ChannelNameCondition
    ChannelNameCondition (ChannelNameCondition'),
    newChannelNameCondition,

    -- ** DeletionConfig
    DeletionConfig (DeletionConfig'),
    newDeletionConfig,

    -- ** EdgeConfig
    EdgeConfig (EdgeConfig'),
    newEdgeConfig,

    -- ** ImageGenerationConfiguration
    ImageGenerationConfiguration (ImageGenerationConfiguration'),
    newImageGenerationConfiguration,

    -- ** ImageGenerationDestinationConfig
    ImageGenerationDestinationConfig (ImageGenerationDestinationConfig'),
    newImageGenerationDestinationConfig,

    -- ** LocalSizeConfig
    LocalSizeConfig (LocalSizeConfig'),
    newLocalSizeConfig,

    -- ** MappedResourceConfigurationListItem
    MappedResourceConfigurationListItem (MappedResourceConfigurationListItem'),
    newMappedResourceConfigurationListItem,

    -- ** MediaSourceConfig
    MediaSourceConfig (MediaSourceConfig'),
    newMediaSourceConfig,

    -- ** MediaStorageConfiguration
    MediaStorageConfiguration (MediaStorageConfiguration'),
    newMediaStorageConfiguration,

    -- ** NotificationConfiguration
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** NotificationDestinationConfig
    NotificationDestinationConfig (NotificationDestinationConfig'),
    newNotificationDestinationConfig,

    -- ** RecorderConfig
    RecorderConfig (RecorderConfig'),
    newRecorderConfig,

    -- ** ResourceEndpointListItem
    ResourceEndpointListItem (ResourceEndpointListItem'),
    newResourceEndpointListItem,

    -- ** ScheduleConfig
    ScheduleConfig (ScheduleConfig'),
    newScheduleConfig,

    -- ** SingleMasterChannelEndpointConfiguration
    SingleMasterChannelEndpointConfiguration (SingleMasterChannelEndpointConfiguration'),
    newSingleMasterChannelEndpointConfiguration,

    -- ** SingleMasterConfiguration
    SingleMasterConfiguration (SingleMasterConfiguration'),
    newSingleMasterConfiguration,

    -- ** StreamInfo
    StreamInfo (StreamInfo'),
    newStreamInfo,

    -- ** StreamNameCondition
    StreamNameCondition (StreamNameCondition'),
    newStreamNameCondition,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UploaderConfig
    UploaderConfig (UploaderConfig'),
    newUploaderConfig,
  )
where

import Amazonka.KinesisVideo.CreateSignalingChannel
import Amazonka.KinesisVideo.CreateStream
import Amazonka.KinesisVideo.DeleteSignalingChannel
import Amazonka.KinesisVideo.DeleteStream
import Amazonka.KinesisVideo.DescribeEdgeConfiguration
import Amazonka.KinesisVideo.DescribeImageGenerationConfiguration
import Amazonka.KinesisVideo.DescribeMappedResourceConfiguration
import Amazonka.KinesisVideo.DescribeMediaStorageConfiguration
import Amazonka.KinesisVideo.DescribeNotificationConfiguration
import Amazonka.KinesisVideo.DescribeSignalingChannel
import Amazonka.KinesisVideo.DescribeStream
import Amazonka.KinesisVideo.GetDataEndpoint
import Amazonka.KinesisVideo.GetSignalingChannelEndpoint
import Amazonka.KinesisVideo.Lens
import Amazonka.KinesisVideo.ListSignalingChannels
import Amazonka.KinesisVideo.ListStreams
import Amazonka.KinesisVideo.ListTagsForResource
import Amazonka.KinesisVideo.ListTagsForStream
import Amazonka.KinesisVideo.StartEdgeConfigurationUpdate
import Amazonka.KinesisVideo.TagResource
import Amazonka.KinesisVideo.TagStream
import Amazonka.KinesisVideo.Types
import Amazonka.KinesisVideo.UntagResource
import Amazonka.KinesisVideo.UntagStream
import Amazonka.KinesisVideo.UpdateDataRetention
import Amazonka.KinesisVideo.UpdateImageGenerationConfiguration
import Amazonka.KinesisVideo.UpdateMediaStorageConfiguration
import Amazonka.KinesisVideo.UpdateNotificationConfiguration
import Amazonka.KinesisVideo.UpdateSignalingChannel
import Amazonka.KinesisVideo.UpdateStream
import Amazonka.KinesisVideo.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'KinesisVideo'.

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
