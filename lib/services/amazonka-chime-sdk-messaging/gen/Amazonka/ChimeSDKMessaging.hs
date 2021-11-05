{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ChimeSDKMessaging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-05-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amazon Chime SDK Messaging APIs in this section allow software
-- developers to send and receive messages in custom messaging
-- applications. These APIs depend on the frameworks provided by the Amazon
-- Chime SDK Identity APIs. For more information about the messaging APIs,
-- see
-- <https://docs.aws.amazon.com/chime/latest/APIReference/API_Operations_Amazon_Chime_SDK_Messaging Amazon Chime SDK messaging>
module Amazonka.ChimeSDKMessaging
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ThrottledClientException
    _ThrottledClientException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** UnauthorizedClientException
    _UnauthorizedClientException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeChannelMembership
    DescribeChannelMembership (DescribeChannelMembership'),
    newDescribeChannelMembership,
    DescribeChannelMembershipResponse (DescribeChannelMembershipResponse'),
    newDescribeChannelMembershipResponse,

    -- ** DescribeChannelFlow
    DescribeChannelFlow (DescribeChannelFlow'),
    newDescribeChannelFlow,
    DescribeChannelFlowResponse (DescribeChannelFlowResponse'),
    newDescribeChannelFlowResponse,

    -- ** ListChannels
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** UpdateChannel
    UpdateChannel (UpdateChannel'),
    newUpdateChannel,
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- ** AssociateChannelFlow
    AssociateChannelFlow (AssociateChannelFlow'),
    newAssociateChannelFlow,
    AssociateChannelFlowResponse (AssociateChannelFlowResponse'),
    newAssociateChannelFlowResponse,

    -- ** GetMessagingSessionEndpoint
    GetMessagingSessionEndpoint (GetMessagingSessionEndpoint'),
    newGetMessagingSessionEndpoint,
    GetMessagingSessionEndpointResponse (GetMessagingSessionEndpointResponse'),
    newGetMessagingSessionEndpointResponse,

    -- ** ListChannelsModeratedByAppInstanceUser
    ListChannelsModeratedByAppInstanceUser (ListChannelsModeratedByAppInstanceUser'),
    newListChannelsModeratedByAppInstanceUser,
    ListChannelsModeratedByAppInstanceUserResponse (ListChannelsModeratedByAppInstanceUserResponse'),
    newListChannelsModeratedByAppInstanceUserResponse,

    -- ** RedactChannelMessage
    RedactChannelMessage (RedactChannelMessage'),
    newRedactChannelMessage,
    RedactChannelMessageResponse (RedactChannelMessageResponse'),
    newRedactChannelMessageResponse,

    -- ** ListChannelFlows
    ListChannelFlows (ListChannelFlows'),
    newListChannelFlows,
    ListChannelFlowsResponse (ListChannelFlowsResponse'),
    newListChannelFlowsResponse,

    -- ** DeleteChannelFlow
    DeleteChannelFlow (DeleteChannelFlow'),
    newDeleteChannelFlow,
    DeleteChannelFlowResponse (DeleteChannelFlowResponse'),
    newDeleteChannelFlowResponse,

    -- ** UpdateChannelFlow
    UpdateChannelFlow (UpdateChannelFlow'),
    newUpdateChannelFlow,
    UpdateChannelFlowResponse (UpdateChannelFlowResponse'),
    newUpdateChannelFlowResponse,

    -- ** DeleteChannelMembership
    DeleteChannelMembership (DeleteChannelMembership'),
    newDeleteChannelMembership,
    DeleteChannelMembershipResponse (DeleteChannelMembershipResponse'),
    newDeleteChannelMembershipResponse,

    -- ** ListChannelMemberships
    ListChannelMemberships (ListChannelMemberships'),
    newListChannelMemberships,
    ListChannelMembershipsResponse (ListChannelMembershipsResponse'),
    newListChannelMembershipsResponse,

    -- ** DisassociateChannelFlow
    DisassociateChannelFlow (DisassociateChannelFlow'),
    newDisassociateChannelFlow,
    DisassociateChannelFlowResponse (DisassociateChannelFlowResponse'),
    newDisassociateChannelFlowResponse,

    -- ** GetChannelMessage
    GetChannelMessage (GetChannelMessage'),
    newGetChannelMessage,
    GetChannelMessageResponse (GetChannelMessageResponse'),
    newGetChannelMessageResponse,

    -- ** DescribeChannelMembershipForAppInstanceUser
    DescribeChannelMembershipForAppInstanceUser (DescribeChannelMembershipForAppInstanceUser'),
    newDescribeChannelMembershipForAppInstanceUser,
    DescribeChannelMembershipForAppInstanceUserResponse (DescribeChannelMembershipForAppInstanceUserResponse'),
    newDescribeChannelMembershipForAppInstanceUserResponse,

    -- ** CreateChannelModerator
    CreateChannelModerator (CreateChannelModerator'),
    newCreateChannelModerator,
    CreateChannelModeratorResponse (CreateChannelModeratorResponse'),
    newCreateChannelModeratorResponse,

    -- ** DescribeChannelModeratedByAppInstanceUser
    DescribeChannelModeratedByAppInstanceUser (DescribeChannelModeratedByAppInstanceUser'),
    newDescribeChannelModeratedByAppInstanceUser,
    DescribeChannelModeratedByAppInstanceUserResponse (DescribeChannelModeratedByAppInstanceUserResponse'),
    newDescribeChannelModeratedByAppInstanceUserResponse,

    -- ** SendChannelMessage
    SendChannelMessage (SendChannelMessage'),
    newSendChannelMessage,
    SendChannelMessageResponse (SendChannelMessageResponse'),
    newSendChannelMessageResponse,

    -- ** DeleteChannelBan
    DeleteChannelBan (DeleteChannelBan'),
    newDeleteChannelBan,
    DeleteChannelBanResponse (DeleteChannelBanResponse'),
    newDeleteChannelBanResponse,

    -- ** ListChannelBans
    ListChannelBans (ListChannelBans'),
    newListChannelBans,
    ListChannelBansResponse (ListChannelBansResponse'),
    newListChannelBansResponse,

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** DescribeChannelModerator
    DescribeChannelModerator (DescribeChannelModerator'),
    newDescribeChannelModerator,
    DescribeChannelModeratorResponse (DescribeChannelModeratorResponse'),
    newDescribeChannelModeratorResponse,

    -- ** CreateChannelBan
    CreateChannelBan (CreateChannelBan'),
    newCreateChannelBan,
    CreateChannelBanResponse (CreateChannelBanResponse'),
    newCreateChannelBanResponse,

    -- ** ListChannelMembershipsForAppInstanceUser
    ListChannelMembershipsForAppInstanceUser (ListChannelMembershipsForAppInstanceUser'),
    newListChannelMembershipsForAppInstanceUser,
    ListChannelMembershipsForAppInstanceUserResponse (ListChannelMembershipsForAppInstanceUserResponse'),
    newListChannelMembershipsForAppInstanceUserResponse,

    -- ** UpdateChannelReadMarker
    UpdateChannelReadMarker (UpdateChannelReadMarker'),
    newUpdateChannelReadMarker,
    UpdateChannelReadMarkerResponse (UpdateChannelReadMarkerResponse'),
    newUpdateChannelReadMarkerResponse,

    -- ** GetChannelMessageStatus
    GetChannelMessageStatus (GetChannelMessageStatus'),
    newGetChannelMessageStatus,
    GetChannelMessageStatusResponse (GetChannelMessageStatusResponse'),
    newGetChannelMessageStatusResponse,

    -- ** CreateChannelFlow
    CreateChannelFlow (CreateChannelFlow'),
    newCreateChannelFlow,
    CreateChannelFlowResponse (CreateChannelFlowResponse'),
    newCreateChannelFlowResponse,

    -- ** CreateChannelMembership
    CreateChannelMembership (CreateChannelMembership'),
    newCreateChannelMembership,
    CreateChannelMembershipResponse (CreateChannelMembershipResponse'),
    newCreateChannelMembershipResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ChannelFlowCallback
    ChannelFlowCallback (ChannelFlowCallback'),
    newChannelFlowCallback,
    ChannelFlowCallbackResponse (ChannelFlowCallbackResponse'),
    newChannelFlowCallbackResponse,

    -- ** DeleteChannelModerator
    DeleteChannelModerator (DeleteChannelModerator'),
    newDeleteChannelModerator,
    DeleteChannelModeratorResponse (DeleteChannelModeratorResponse'),
    newDeleteChannelModeratorResponse,

    -- ** DescribeChannelBan
    DescribeChannelBan (DescribeChannelBan'),
    newDescribeChannelBan,
    DescribeChannelBanResponse (DescribeChannelBanResponse'),
    newDescribeChannelBanResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListChannelModerators
    ListChannelModerators (ListChannelModerators'),
    newListChannelModerators,
    ListChannelModeratorsResponse (ListChannelModeratorsResponse'),
    newListChannelModeratorsResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** DeleteChannelMessage
    DeleteChannelMessage (DeleteChannelMessage'),
    newDeleteChannelMessage,
    DeleteChannelMessageResponse (DeleteChannelMessageResponse'),
    newDeleteChannelMessageResponse,

    -- ** UpdateChannelMessage
    UpdateChannelMessage (UpdateChannelMessage'),
    newUpdateChannelMessage,
    UpdateChannelMessageResponse (UpdateChannelMessageResponse'),
    newUpdateChannelMessageResponse,

    -- ** ListChannelMessages
    ListChannelMessages (ListChannelMessages'),
    newListChannelMessages,
    ListChannelMessagesResponse (ListChannelMessagesResponse'),
    newListChannelMessagesResponse,

    -- ** ListChannelsAssociatedWithChannelFlow
    ListChannelsAssociatedWithChannelFlow (ListChannelsAssociatedWithChannelFlow'),
    newListChannelsAssociatedWithChannelFlow,
    ListChannelsAssociatedWithChannelFlowResponse (ListChannelsAssociatedWithChannelFlowResponse'),
    newListChannelsAssociatedWithChannelFlowResponse,

    -- ** BatchCreateChannelMembership
    BatchCreateChannelMembership (BatchCreateChannelMembership'),
    newBatchCreateChannelMembership,
    BatchCreateChannelMembershipResponse (BatchCreateChannelMembershipResponse'),
    newBatchCreateChannelMembershipResponse,

    -- * Types

    -- ** ChannelMembershipType
    ChannelMembershipType (..),

    -- ** ChannelMessagePersistenceType
    ChannelMessagePersistenceType (..),

    -- ** ChannelMessageStatus
    ChannelMessageStatus (..),

    -- ** ChannelMessageType
    ChannelMessageType (..),

    -- ** ChannelMode
    ChannelMode (..),

    -- ** ChannelPrivacy
    ChannelPrivacy (..),

    -- ** ErrorCode
    ErrorCode (..),

    -- ** FallbackAction
    FallbackAction (..),

    -- ** InvocationType
    InvocationType (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** AppInstanceUserMembershipSummary
    AppInstanceUserMembershipSummary (AppInstanceUserMembershipSummary'),
    newAppInstanceUserMembershipSummary,

    -- ** BatchChannelMemberships
    BatchChannelMemberships (BatchChannelMemberships'),
    newBatchChannelMemberships,

    -- ** BatchCreateChannelMembershipError
    BatchCreateChannelMembershipError (BatchCreateChannelMembershipError'),
    newBatchCreateChannelMembershipError,

    -- ** Channel
    Channel (Channel'),
    newChannel,

    -- ** ChannelAssociatedWithFlowSummary
    ChannelAssociatedWithFlowSummary (ChannelAssociatedWithFlowSummary'),
    newChannelAssociatedWithFlowSummary,

    -- ** ChannelBan
    ChannelBan (ChannelBan'),
    newChannelBan,

    -- ** ChannelBanSummary
    ChannelBanSummary (ChannelBanSummary'),
    newChannelBanSummary,

    -- ** ChannelFlow
    ChannelFlow (ChannelFlow'),
    newChannelFlow,

    -- ** ChannelFlowSummary
    ChannelFlowSummary (ChannelFlowSummary'),
    newChannelFlowSummary,

    -- ** ChannelMembership
    ChannelMembership (ChannelMembership'),
    newChannelMembership,

    -- ** ChannelMembershipForAppInstanceUserSummary
    ChannelMembershipForAppInstanceUserSummary (ChannelMembershipForAppInstanceUserSummary'),
    newChannelMembershipForAppInstanceUserSummary,

    -- ** ChannelMembershipSummary
    ChannelMembershipSummary (ChannelMembershipSummary'),
    newChannelMembershipSummary,

    -- ** ChannelMessage
    ChannelMessage (ChannelMessage'),
    newChannelMessage,

    -- ** ChannelMessageCallback
    ChannelMessageCallback (ChannelMessageCallback'),
    newChannelMessageCallback,

    -- ** ChannelMessageStatusStructure
    ChannelMessageStatusStructure (ChannelMessageStatusStructure'),
    newChannelMessageStatusStructure,

    -- ** ChannelMessageSummary
    ChannelMessageSummary (ChannelMessageSummary'),
    newChannelMessageSummary,

    -- ** ChannelModeratedByAppInstanceUserSummary
    ChannelModeratedByAppInstanceUserSummary (ChannelModeratedByAppInstanceUserSummary'),
    newChannelModeratedByAppInstanceUserSummary,

    -- ** ChannelModerator
    ChannelModerator (ChannelModerator'),
    newChannelModerator,

    -- ** ChannelModeratorSummary
    ChannelModeratorSummary (ChannelModeratorSummary'),
    newChannelModeratorSummary,

    -- ** ChannelSummary
    ChannelSummary (ChannelSummary'),
    newChannelSummary,

    -- ** Identity
    Identity (Identity'),
    newIdentity,

    -- ** LambdaConfiguration
    LambdaConfiguration (LambdaConfiguration'),
    newLambdaConfiguration,

    -- ** MessagingSessionEndpoint
    MessagingSessionEndpoint (MessagingSessionEndpoint'),
    newMessagingSessionEndpoint,

    -- ** Processor
    Processor (Processor'),
    newProcessor,

    -- ** ProcessorConfiguration
    ProcessorConfiguration (ProcessorConfiguration'),
    newProcessorConfiguration,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.ChimeSDKMessaging.AssociateChannelFlow
import Amazonka.ChimeSDKMessaging.BatchCreateChannelMembership
import Amazonka.ChimeSDKMessaging.ChannelFlowCallback
import Amazonka.ChimeSDKMessaging.CreateChannel
import Amazonka.ChimeSDKMessaging.CreateChannelBan
import Amazonka.ChimeSDKMessaging.CreateChannelFlow
import Amazonka.ChimeSDKMessaging.CreateChannelMembership
import Amazonka.ChimeSDKMessaging.CreateChannelModerator
import Amazonka.ChimeSDKMessaging.DeleteChannel
import Amazonka.ChimeSDKMessaging.DeleteChannelBan
import Amazonka.ChimeSDKMessaging.DeleteChannelFlow
import Amazonka.ChimeSDKMessaging.DeleteChannelMembership
import Amazonka.ChimeSDKMessaging.DeleteChannelMessage
import Amazonka.ChimeSDKMessaging.DeleteChannelModerator
import Amazonka.ChimeSDKMessaging.DescribeChannel
import Amazonka.ChimeSDKMessaging.DescribeChannelBan
import Amazonka.ChimeSDKMessaging.DescribeChannelFlow
import Amazonka.ChimeSDKMessaging.DescribeChannelMembership
import Amazonka.ChimeSDKMessaging.DescribeChannelMembershipForAppInstanceUser
import Amazonka.ChimeSDKMessaging.DescribeChannelModeratedByAppInstanceUser
import Amazonka.ChimeSDKMessaging.DescribeChannelModerator
import Amazonka.ChimeSDKMessaging.DisassociateChannelFlow
import Amazonka.ChimeSDKMessaging.GetChannelMessage
import Amazonka.ChimeSDKMessaging.GetChannelMessageStatus
import Amazonka.ChimeSDKMessaging.GetMessagingSessionEndpoint
import Amazonka.ChimeSDKMessaging.Lens
import Amazonka.ChimeSDKMessaging.ListChannelBans
import Amazonka.ChimeSDKMessaging.ListChannelFlows
import Amazonka.ChimeSDKMessaging.ListChannelMemberships
import Amazonka.ChimeSDKMessaging.ListChannelMembershipsForAppInstanceUser
import Amazonka.ChimeSDKMessaging.ListChannelMessages
import Amazonka.ChimeSDKMessaging.ListChannelModerators
import Amazonka.ChimeSDKMessaging.ListChannels
import Amazonka.ChimeSDKMessaging.ListChannelsAssociatedWithChannelFlow
import Amazonka.ChimeSDKMessaging.ListChannelsModeratedByAppInstanceUser
import Amazonka.ChimeSDKMessaging.ListTagsForResource
import Amazonka.ChimeSDKMessaging.RedactChannelMessage
import Amazonka.ChimeSDKMessaging.SendChannelMessage
import Amazonka.ChimeSDKMessaging.TagResource
import Amazonka.ChimeSDKMessaging.Types
import Amazonka.ChimeSDKMessaging.UntagResource
import Amazonka.ChimeSDKMessaging.UpdateChannel
import Amazonka.ChimeSDKMessaging.UpdateChannelFlow
import Amazonka.ChimeSDKMessaging.UpdateChannelMessage
import Amazonka.ChimeSDKMessaging.UpdateChannelReadMarker
import Amazonka.ChimeSDKMessaging.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ChimeSDKMessaging'.

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
