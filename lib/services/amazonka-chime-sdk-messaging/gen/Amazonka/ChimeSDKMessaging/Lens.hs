{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSDKMessaging.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Lens
  ( -- * Operations

    -- ** DescribeChannelMembership
    describeChannelMembership_channelArn,
    describeChannelMembership_memberArn,
    describeChannelMembership_chimeBearer,
    describeChannelMembershipResponse_channelMembership,
    describeChannelMembershipResponse_httpStatus,

    -- ** DescribeChannelFlow
    describeChannelFlow_channelFlowArn,
    describeChannelFlowResponse_channelFlow,
    describeChannelFlowResponse_httpStatus,

    -- ** ListChannels
    listChannels_privacy,
    listChannels_nextToken,
    listChannels_maxResults,
    listChannels_appInstanceArn,
    listChannels_chimeBearer,
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_channelArn,
    deleteChannel_chimeBearer,

    -- ** UpdateChannel
    updateChannel_metadata,
    updateChannel_channelArn,
    updateChannel_name,
    updateChannel_mode,
    updateChannel_chimeBearer,
    updateChannelResponse_channelArn,
    updateChannelResponse_httpStatus,

    -- ** AssociateChannelFlow
    associateChannelFlow_channelArn,
    associateChannelFlow_channelFlowArn,
    associateChannelFlow_chimeBearer,

    -- ** GetMessagingSessionEndpoint
    getMessagingSessionEndpointResponse_endpoint,
    getMessagingSessionEndpointResponse_httpStatus,

    -- ** ListChannelsModeratedByAppInstanceUser
    listChannelsModeratedByAppInstanceUser_appInstanceUserArn,
    listChannelsModeratedByAppInstanceUser_nextToken,
    listChannelsModeratedByAppInstanceUser_maxResults,
    listChannelsModeratedByAppInstanceUser_chimeBearer,
    listChannelsModeratedByAppInstanceUserResponse_channels,
    listChannelsModeratedByAppInstanceUserResponse_nextToken,
    listChannelsModeratedByAppInstanceUserResponse_httpStatus,

    -- ** RedactChannelMessage
    redactChannelMessage_channelArn,
    redactChannelMessage_messageId,
    redactChannelMessage_chimeBearer,
    redactChannelMessageResponse_channelArn,
    redactChannelMessageResponse_messageId,
    redactChannelMessageResponse_httpStatus,

    -- ** ListChannelFlows
    listChannelFlows_nextToken,
    listChannelFlows_maxResults,
    listChannelFlows_appInstanceArn,
    listChannelFlowsResponse_channelFlows,
    listChannelFlowsResponse_nextToken,
    listChannelFlowsResponse_httpStatus,

    -- ** DeleteChannelFlow
    deleteChannelFlow_channelFlowArn,

    -- ** UpdateChannelFlow
    updateChannelFlow_channelFlowArn,
    updateChannelFlow_processors,
    updateChannelFlow_name,
    updateChannelFlowResponse_channelFlowArn,
    updateChannelFlowResponse_httpStatus,

    -- ** DeleteChannelMembership
    deleteChannelMembership_channelArn,
    deleteChannelMembership_memberArn,
    deleteChannelMembership_chimeBearer,

    -- ** ListChannelMemberships
    listChannelMemberships_nextToken,
    listChannelMemberships_type,
    listChannelMemberships_maxResults,
    listChannelMemberships_channelArn,
    listChannelMemberships_chimeBearer,
    listChannelMembershipsResponse_channelMemberships,
    listChannelMembershipsResponse_channelArn,
    listChannelMembershipsResponse_nextToken,
    listChannelMembershipsResponse_httpStatus,

    -- ** DisassociateChannelFlow
    disassociateChannelFlow_channelArn,
    disassociateChannelFlow_channelFlowArn,
    disassociateChannelFlow_chimeBearer,

    -- ** GetChannelMessage
    getChannelMessage_channelArn,
    getChannelMessage_messageId,
    getChannelMessage_chimeBearer,
    getChannelMessageResponse_channelMessage,
    getChannelMessageResponse_httpStatus,

    -- ** DescribeChannelMembershipForAppInstanceUser
    describeChannelMembershipForAppInstanceUser_channelArn,
    describeChannelMembershipForAppInstanceUser_appInstanceUserArn,
    describeChannelMembershipForAppInstanceUser_chimeBearer,
    describeChannelMembershipForAppInstanceUserResponse_channelMembership,
    describeChannelMembershipForAppInstanceUserResponse_httpStatus,

    -- ** CreateChannelModerator
    createChannelModerator_channelArn,
    createChannelModerator_channelModeratorArn,
    createChannelModerator_chimeBearer,
    createChannelModeratorResponse_channelModerator,
    createChannelModeratorResponse_channelArn,
    createChannelModeratorResponse_httpStatus,

    -- ** DescribeChannelModeratedByAppInstanceUser
    describeChannelModeratedByAppInstanceUser_channelArn,
    describeChannelModeratedByAppInstanceUser_appInstanceUserArn,
    describeChannelModeratedByAppInstanceUser_chimeBearer,
    describeChannelModeratedByAppInstanceUserResponse_channel,
    describeChannelModeratedByAppInstanceUserResponse_httpStatus,

    -- ** SendChannelMessage
    sendChannelMessage_metadata,
    sendChannelMessage_channelArn,
    sendChannelMessage_content,
    sendChannelMessage_type,
    sendChannelMessage_persistence,
    sendChannelMessage_clientRequestToken,
    sendChannelMessage_chimeBearer,
    sendChannelMessageResponse_status,
    sendChannelMessageResponse_channelArn,
    sendChannelMessageResponse_messageId,
    sendChannelMessageResponse_httpStatus,

    -- ** DeleteChannelBan
    deleteChannelBan_channelArn,
    deleteChannelBan_memberArn,
    deleteChannelBan_chimeBearer,

    -- ** ListChannelBans
    listChannelBans_nextToken,
    listChannelBans_maxResults,
    listChannelBans_channelArn,
    listChannelBans_chimeBearer,
    listChannelBansResponse_channelArn,
    listChannelBansResponse_nextToken,
    listChannelBansResponse_channelBans,
    listChannelBansResponse_httpStatus,

    -- ** CreateChannel
    createChannel_mode,
    createChannel_privacy,
    createChannel_metadata,
    createChannel_tags,
    createChannel_appInstanceArn,
    createChannel_name,
    createChannel_clientRequestToken,
    createChannel_chimeBearer,
    createChannelResponse_channelArn,
    createChannelResponse_httpStatus,

    -- ** DescribeChannelModerator
    describeChannelModerator_channelArn,
    describeChannelModerator_channelModeratorArn,
    describeChannelModerator_chimeBearer,
    describeChannelModeratorResponse_channelModerator,
    describeChannelModeratorResponse_httpStatus,

    -- ** CreateChannelBan
    createChannelBan_channelArn,
    createChannelBan_memberArn,
    createChannelBan_chimeBearer,
    createChannelBanResponse_channelArn,
    createChannelBanResponse_member,
    createChannelBanResponse_httpStatus,

    -- ** ListChannelMembershipsForAppInstanceUser
    listChannelMembershipsForAppInstanceUser_appInstanceUserArn,
    listChannelMembershipsForAppInstanceUser_nextToken,
    listChannelMembershipsForAppInstanceUser_maxResults,
    listChannelMembershipsForAppInstanceUser_chimeBearer,
    listChannelMembershipsForAppInstanceUserResponse_channelMemberships,
    listChannelMembershipsForAppInstanceUserResponse_nextToken,
    listChannelMembershipsForAppInstanceUserResponse_httpStatus,

    -- ** UpdateChannelReadMarker
    updateChannelReadMarker_channelArn,
    updateChannelReadMarker_chimeBearer,
    updateChannelReadMarkerResponse_channelArn,
    updateChannelReadMarkerResponse_httpStatus,

    -- ** GetChannelMessageStatus
    getChannelMessageStatus_channelArn,
    getChannelMessageStatus_messageId,
    getChannelMessageStatus_chimeBearer,
    getChannelMessageStatusResponse_status,
    getChannelMessageStatusResponse_httpStatus,

    -- ** CreateChannelFlow
    createChannelFlow_tags,
    createChannelFlow_appInstanceArn,
    createChannelFlow_processors,
    createChannelFlow_name,
    createChannelFlow_clientRequestToken,
    createChannelFlowResponse_channelFlowArn,
    createChannelFlowResponse_httpStatus,

    -- ** CreateChannelMembership
    createChannelMembership_channelArn,
    createChannelMembership_memberArn,
    createChannelMembership_type,
    createChannelMembership_chimeBearer,
    createChannelMembershipResponse_channelArn,
    createChannelMembershipResponse_member,
    createChannelMembershipResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,

    -- ** ChannelFlowCallback
    channelFlowCallback_deleteResource,
    channelFlowCallback_callbackId,
    channelFlowCallback_channelArn,
    channelFlowCallback_channelMessage,
    channelFlowCallbackResponse_callbackId,
    channelFlowCallbackResponse_channelArn,
    channelFlowCallbackResponse_httpStatus,

    -- ** DeleteChannelModerator
    deleteChannelModerator_channelArn,
    deleteChannelModerator_channelModeratorArn,
    deleteChannelModerator_chimeBearer,

    -- ** DescribeChannelBan
    describeChannelBan_channelArn,
    describeChannelBan_memberArn,
    describeChannelBan_chimeBearer,
    describeChannelBanResponse_channelBan,
    describeChannelBanResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,

    -- ** ListChannelModerators
    listChannelModerators_nextToken,
    listChannelModerators_maxResults,
    listChannelModerators_channelArn,
    listChannelModerators_chimeBearer,
    listChannelModeratorsResponse_channelArn,
    listChannelModeratorsResponse_nextToken,
    listChannelModeratorsResponse_channelModerators,
    listChannelModeratorsResponse_httpStatus,

    -- ** DescribeChannel
    describeChannel_channelArn,
    describeChannel_chimeBearer,
    describeChannelResponse_channel,
    describeChannelResponse_httpStatus,

    -- ** DeleteChannelMessage
    deleteChannelMessage_channelArn,
    deleteChannelMessage_messageId,
    deleteChannelMessage_chimeBearer,

    -- ** UpdateChannelMessage
    updateChannelMessage_content,
    updateChannelMessage_metadata,
    updateChannelMessage_channelArn,
    updateChannelMessage_messageId,
    updateChannelMessage_chimeBearer,
    updateChannelMessageResponse_status,
    updateChannelMessageResponse_channelArn,
    updateChannelMessageResponse_messageId,
    updateChannelMessageResponse_httpStatus,

    -- ** ListChannelMessages
    listChannelMessages_nextToken,
    listChannelMessages_notBefore,
    listChannelMessages_sortOrder,
    listChannelMessages_maxResults,
    listChannelMessages_notAfter,
    listChannelMessages_channelArn,
    listChannelMessages_chimeBearer,
    listChannelMessagesResponse_channelArn,
    listChannelMessagesResponse_nextToken,
    listChannelMessagesResponse_channelMessages,
    listChannelMessagesResponse_httpStatus,

    -- ** ListChannelsAssociatedWithChannelFlow
    listChannelsAssociatedWithChannelFlow_nextToken,
    listChannelsAssociatedWithChannelFlow_maxResults,
    listChannelsAssociatedWithChannelFlow_channelFlowArn,
    listChannelsAssociatedWithChannelFlowResponse_channels,
    listChannelsAssociatedWithChannelFlowResponse_nextToken,
    listChannelsAssociatedWithChannelFlowResponse_httpStatus,

    -- ** BatchCreateChannelMembership
    batchCreateChannelMembership_type,
    batchCreateChannelMembership_channelArn,
    batchCreateChannelMembership_memberArns,
    batchCreateChannelMembership_chimeBearer,
    batchCreateChannelMembershipResponse_errors,
    batchCreateChannelMembershipResponse_batchChannelMemberships,
    batchCreateChannelMembershipResponse_httpStatus,

    -- * Types

    -- ** AppInstanceUserMembershipSummary
    appInstanceUserMembershipSummary_readMarkerTimestamp,
    appInstanceUserMembershipSummary_type,

    -- ** BatchChannelMemberships
    batchChannelMemberships_members,
    batchChannelMemberships_channelArn,
    batchChannelMemberships_type,
    batchChannelMemberships_invitedBy,

    -- ** BatchCreateChannelMembershipError
    batchCreateChannelMembershipError_errorCode,
    batchCreateChannelMembershipError_memberArn,
    batchCreateChannelMembershipError_errorMessage,

    -- ** Channel
    channel_mode,
    channel_createdBy,
    channel_channelArn,
    channel_privacy,
    channel_channelFlowArn,
    channel_lastMessageTimestamp,
    channel_name,
    channel_metadata,
    channel_createdTimestamp,
    channel_lastUpdatedTimestamp,

    -- ** ChannelAssociatedWithFlowSummary
    channelAssociatedWithFlowSummary_mode,
    channelAssociatedWithFlowSummary_channelArn,
    channelAssociatedWithFlowSummary_privacy,
    channelAssociatedWithFlowSummary_name,
    channelAssociatedWithFlowSummary_metadata,

    -- ** ChannelBan
    channelBan_createdBy,
    channelBan_channelArn,
    channelBan_member,
    channelBan_createdTimestamp,

    -- ** ChannelBanSummary
    channelBanSummary_member,

    -- ** ChannelFlow
    channelFlow_processors,
    channelFlow_channelFlowArn,
    channelFlow_name,
    channelFlow_createdTimestamp,
    channelFlow_lastUpdatedTimestamp,

    -- ** ChannelFlowSummary
    channelFlowSummary_processors,
    channelFlowSummary_channelFlowArn,
    channelFlowSummary_name,

    -- ** ChannelMembership
    channelMembership_channelArn,
    channelMembership_member,
    channelMembership_type,
    channelMembership_invitedBy,
    channelMembership_createdTimestamp,
    channelMembership_lastUpdatedTimestamp,

    -- ** ChannelMembershipForAppInstanceUserSummary
    channelMembershipForAppInstanceUserSummary_appInstanceUserMembershipSummary,
    channelMembershipForAppInstanceUserSummary_channelSummary,

    -- ** ChannelMembershipSummary
    channelMembershipSummary_member,

    -- ** ChannelMessage
    channelMessage_status,
    channelMessage_sender,
    channelMessage_channelArn,
    channelMessage_content,
    channelMessage_redacted,
    channelMessage_persistence,
    channelMessage_metadata,
    channelMessage_type,
    channelMessage_createdTimestamp,
    channelMessage_messageId,
    channelMessage_lastUpdatedTimestamp,
    channelMessage_lastEditedTimestamp,

    -- ** ChannelMessageCallback
    channelMessageCallback_content,
    channelMessageCallback_metadata,
    channelMessageCallback_messageId,

    -- ** ChannelMessageStatusStructure
    channelMessageStatusStructure_value,
    channelMessageStatusStructure_detail,

    -- ** ChannelMessageSummary
    channelMessageSummary_status,
    channelMessageSummary_sender,
    channelMessageSummary_content,
    channelMessageSummary_redacted,
    channelMessageSummary_metadata,
    channelMessageSummary_type,
    channelMessageSummary_createdTimestamp,
    channelMessageSummary_messageId,
    channelMessageSummary_lastUpdatedTimestamp,
    channelMessageSummary_lastEditedTimestamp,

    -- ** ChannelModeratedByAppInstanceUserSummary
    channelModeratedByAppInstanceUserSummary_channelSummary,

    -- ** ChannelModerator
    channelModerator_createdBy,
    channelModerator_channelArn,
    channelModerator_createdTimestamp,
    channelModerator_moderator,

    -- ** ChannelModeratorSummary
    channelModeratorSummary_moderator,

    -- ** ChannelSummary
    channelSummary_mode,
    channelSummary_channelArn,
    channelSummary_privacy,
    channelSummary_lastMessageTimestamp,
    channelSummary_name,
    channelSummary_metadata,

    -- ** Identity
    identity_arn,
    identity_name,

    -- ** LambdaConfiguration
    lambdaConfiguration_resourceArn,
    lambdaConfiguration_invocationType,

    -- ** MessagingSessionEndpoint
    messagingSessionEndpoint_url,

    -- ** Processor
    processor_name,
    processor_configuration,
    processor_executionOrder,
    processor_fallbackAction,

    -- ** ProcessorConfiguration
    processorConfiguration_lambda,

    -- ** Tag
    tag_key,
    tag_value,
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
import Amazonka.ChimeSDKMessaging.Types.AppInstanceUserMembershipSummary
import Amazonka.ChimeSDKMessaging.Types.BatchChannelMemberships
import Amazonka.ChimeSDKMessaging.Types.BatchCreateChannelMembershipError
import Amazonka.ChimeSDKMessaging.Types.Channel
import Amazonka.ChimeSDKMessaging.Types.ChannelAssociatedWithFlowSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelBan
import Amazonka.ChimeSDKMessaging.Types.ChannelBanSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelFlow
import Amazonka.ChimeSDKMessaging.Types.ChannelFlowSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelMembership
import Amazonka.ChimeSDKMessaging.Types.ChannelMembershipForAppInstanceUserSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelMembershipSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelMessage
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageCallback
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatusStructure
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelModeratedByAppInstanceUserSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelModerator
import Amazonka.ChimeSDKMessaging.Types.ChannelModeratorSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelSummary
import Amazonka.ChimeSDKMessaging.Types.Identity
import Amazonka.ChimeSDKMessaging.Types.LambdaConfiguration
import Amazonka.ChimeSDKMessaging.Types.MessagingSessionEndpoint
import Amazonka.ChimeSDKMessaging.Types.Processor
import Amazonka.ChimeSDKMessaging.Types.ProcessorConfiguration
import Amazonka.ChimeSDKMessaging.Types.Tag
import Amazonka.ChimeSDKMessaging.UntagResource
import Amazonka.ChimeSDKMessaging.UpdateChannel
import Amazonka.ChimeSDKMessaging.UpdateChannelFlow
import Amazonka.ChimeSDKMessaging.UpdateChannelMessage
import Amazonka.ChimeSDKMessaging.UpdateChannelReadMarker
