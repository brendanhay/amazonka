{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.KinesisVideo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-09-30@ of the AWS service descriptions, licensed under Apache 2.0.
module Network.AWS.KinesisVideo
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccountStreamLimitExceededException
    _AccountStreamLimitExceededException,

    -- ** VersionMismatchException
    _VersionMismatchException,

    -- ** AccountChannelLimitExceededException
    _AccountChannelLimitExceededException,

    -- ** ClientLimitExceededException
    _ClientLimitExceededException,

    -- ** TagsPerResourceExceededLimitException
    _TagsPerResourceExceededLimitException,

    -- ** DeviceStreamLimitExceededException
    _DeviceStreamLimitExceededException,

    -- ** InvalidResourceFormatException
    _InvalidResourceFormatException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidDeviceException
    _InvalidDeviceException,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListTagsForStream
    ListTagsForStream (ListTagsForStream'),
    newListTagsForStream,
    ListTagsForStreamResponse (ListTagsForStreamResponse'),
    newListTagsForStreamResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DeleteStream
    DeleteStream (DeleteStream'),
    newDeleteStream,
    DeleteStreamResponse (DeleteStreamResponse'),
    newDeleteStreamResponse,

    -- ** UpdateStream
    UpdateStream (UpdateStream'),
    newUpdateStream,
    UpdateStreamResponse (UpdateStreamResponse'),
    newUpdateStreamResponse,

    -- ** UpdateDataRetention
    UpdateDataRetention (UpdateDataRetention'),
    newUpdateDataRetention,
    UpdateDataRetentionResponse (UpdateDataRetentionResponse'),
    newUpdateDataRetentionResponse,

    -- ** GetDataEndpoint
    GetDataEndpoint (GetDataEndpoint'),
    newGetDataEndpoint,
    GetDataEndpointResponse (GetDataEndpointResponse'),
    newGetDataEndpointResponse,

    -- ** UntagStream
    UntagStream (UntagStream'),
    newUntagStream,
    UntagStreamResponse (UntagStreamResponse'),
    newUntagStreamResponse,

    -- ** DeleteSignalingChannel
    DeleteSignalingChannel (DeleteSignalingChannel'),
    newDeleteSignalingChannel,
    DeleteSignalingChannelResponse (DeleteSignalingChannelResponse'),
    newDeleteSignalingChannelResponse,

    -- ** UpdateSignalingChannel
    UpdateSignalingChannel (UpdateSignalingChannel'),
    newUpdateSignalingChannel,
    UpdateSignalingChannelResponse (UpdateSignalingChannelResponse'),
    newUpdateSignalingChannelResponse,

    -- ** ListSignalingChannels (Paginated)
    ListSignalingChannels (ListSignalingChannels'),
    newListSignalingChannels,
    ListSignalingChannelsResponse (ListSignalingChannelsResponse'),
    newListSignalingChannelsResponse,

    -- ** CreateSignalingChannel
    CreateSignalingChannel (CreateSignalingChannel'),
    newCreateSignalingChannel,
    CreateSignalingChannelResponse (CreateSignalingChannelResponse'),
    newCreateSignalingChannelResponse,

    -- ** DescribeStream
    DescribeStream (DescribeStream'),
    newDescribeStream,
    DescribeStreamResponse (DescribeStreamResponse'),
    newDescribeStreamResponse,

    -- ** TagStream
    TagStream (TagStream'),
    newTagStream,
    TagStreamResponse (TagStreamResponse'),
    newTagStreamResponse,

    -- ** DescribeSignalingChannel
    DescribeSignalingChannel (DescribeSignalingChannel'),
    newDescribeSignalingChannel,
    DescribeSignalingChannelResponse (DescribeSignalingChannelResponse'),
    newDescribeSignalingChannelResponse,

    -- ** GetSignalingChannelEndpoint
    GetSignalingChannelEndpoint (GetSignalingChannelEndpoint'),
    newGetSignalingChannelEndpoint,
    GetSignalingChannelEndpointResponse (GetSignalingChannelEndpointResponse'),
    newGetSignalingChannelEndpointResponse,

    -- ** CreateStream
    CreateStream (CreateStream'),
    newCreateStream,
    CreateStreamResponse (CreateStreamResponse'),
    newCreateStreamResponse,

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

    -- ** StreamStatus
    StreamStatus (..),

    -- ** UpdateDataRetentionOperation
    UpdateDataRetentionOperation (..),

    -- ** ChannelInfo
    ChannelInfo (ChannelInfo'),
    newChannelInfo,

    -- ** ChannelNameCondition
    ChannelNameCondition (ChannelNameCondition'),
    newChannelNameCondition,

    -- ** ResourceEndpointListItem
    ResourceEndpointListItem (ResourceEndpointListItem'),
    newResourceEndpointListItem,

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
import Network.AWS.KinesisVideo.Lens
import Network.AWS.KinesisVideo.ListSignalingChannels
import Network.AWS.KinesisVideo.ListStreams
import Network.AWS.KinesisVideo.ListTagsForResource
import Network.AWS.KinesisVideo.ListTagsForStream
import Network.AWS.KinesisVideo.TagResource
import Network.AWS.KinesisVideo.TagStream
import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.UntagResource
import Network.AWS.KinesisVideo.UntagStream
import Network.AWS.KinesisVideo.UpdateDataRetention
import Network.AWS.KinesisVideo.UpdateSignalingChannel
import Network.AWS.KinesisVideo.UpdateStream
import Network.AWS.KinesisVideo.Waiters

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
