{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSDKMessaging.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Lens
  ( -- * Operations

    -- ** AssociateChannelFlow
    associateChannelFlow_channelArn,
    associateChannelFlow_channelFlowArn,
    associateChannelFlow_chimeBearer,

    -- ** BatchCreateChannelMembership
    batchCreateChannelMembership_subChannelId,
    batchCreateChannelMembership_type,
    batchCreateChannelMembership_channelArn,
    batchCreateChannelMembership_memberArns,
    batchCreateChannelMembership_chimeBearer,
    batchCreateChannelMembershipResponse_batchChannelMemberships,
    batchCreateChannelMembershipResponse_errors,
    batchCreateChannelMembershipResponse_httpStatus,

    -- ** ChannelFlowCallback
    channelFlowCallback_deleteResource,
    channelFlowCallback_callbackId,
    channelFlowCallback_channelArn,
    channelFlowCallback_channelMessage,
    channelFlowCallbackResponse_callbackId,
    channelFlowCallbackResponse_channelArn,
    channelFlowCallbackResponse_httpStatus,

    -- ** CreateChannel
    createChannel_channelId,
    createChannel_elasticChannelConfiguration,
    createChannel_memberArns,
    createChannel_metadata,
    createChannel_mode,
    createChannel_moderatorArns,
    createChannel_privacy,
    createChannel_tags,
    createChannel_appInstanceArn,
    createChannel_name,
    createChannel_clientRequestToken,
    createChannel_chimeBearer,
    createChannelResponse_channelArn,
    createChannelResponse_httpStatus,

    -- ** CreateChannelBan
    createChannelBan_channelArn,
    createChannelBan_memberArn,
    createChannelBan_chimeBearer,
    createChannelBanResponse_channelArn,
    createChannelBanResponse_member,
    createChannelBanResponse_httpStatus,

    -- ** CreateChannelFlow
    createChannelFlow_tags,
    createChannelFlow_appInstanceArn,
    createChannelFlow_processors,
    createChannelFlow_name,
    createChannelFlow_clientRequestToken,
    createChannelFlowResponse_channelFlowArn,
    createChannelFlowResponse_httpStatus,

    -- ** CreateChannelMembership
    createChannelMembership_subChannelId,
    createChannelMembership_channelArn,
    createChannelMembership_memberArn,
    createChannelMembership_type,
    createChannelMembership_chimeBearer,
    createChannelMembershipResponse_channelArn,
    createChannelMembershipResponse_member,
    createChannelMembershipResponse_subChannelId,
    createChannelMembershipResponse_httpStatus,

    -- ** CreateChannelModerator
    createChannelModerator_channelArn,
    createChannelModerator_channelModeratorArn,
    createChannelModerator_chimeBearer,
    createChannelModeratorResponse_channelArn,
    createChannelModeratorResponse_channelModerator,
    createChannelModeratorResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_subChannelId,
    deleteChannel_channelArn,
    deleteChannel_chimeBearer,

    -- ** DeleteChannelBan
    deleteChannelBan_channelArn,
    deleteChannelBan_memberArn,
    deleteChannelBan_chimeBearer,

    -- ** DeleteChannelFlow
    deleteChannelFlow_channelFlowArn,

    -- ** DeleteChannelMembership
    deleteChannelMembership_subChannelId,
    deleteChannelMembership_channelArn,
    deleteChannelMembership_memberArn,
    deleteChannelMembership_chimeBearer,

    -- ** DeleteChannelMessage
    deleteChannelMessage_subChannelId,
    deleteChannelMessage_channelArn,
    deleteChannelMessage_messageId,
    deleteChannelMessage_chimeBearer,

    -- ** DeleteChannelModerator
    deleteChannelModerator_channelArn,
    deleteChannelModerator_channelModeratorArn,
    deleteChannelModerator_chimeBearer,

    -- ** DescribeChannel
    describeChannel_channelArn,
    describeChannel_chimeBearer,
    describeChannelResponse_channel,
    describeChannelResponse_httpStatus,

    -- ** DescribeChannelBan
    describeChannelBan_channelArn,
    describeChannelBan_memberArn,
    describeChannelBan_chimeBearer,
    describeChannelBanResponse_channelBan,
    describeChannelBanResponse_httpStatus,

    -- ** DescribeChannelFlow
    describeChannelFlow_channelFlowArn,
    describeChannelFlowResponse_channelFlow,
    describeChannelFlowResponse_httpStatus,

    -- ** DescribeChannelMembership
    describeChannelMembership_subChannelId,
    describeChannelMembership_channelArn,
    describeChannelMembership_memberArn,
    describeChannelMembership_chimeBearer,
    describeChannelMembershipResponse_channelMembership,
    describeChannelMembershipResponse_httpStatus,

    -- ** DescribeChannelMembershipForAppInstanceUser
    describeChannelMembershipForAppInstanceUser_channelArn,
    describeChannelMembershipForAppInstanceUser_appInstanceUserArn,
    describeChannelMembershipForAppInstanceUser_chimeBearer,
    describeChannelMembershipForAppInstanceUserResponse_channelMembership,
    describeChannelMembershipForAppInstanceUserResponse_httpStatus,

    -- ** DescribeChannelModeratedByAppInstanceUser
    describeChannelModeratedByAppInstanceUser_channelArn,
    describeChannelModeratedByAppInstanceUser_appInstanceUserArn,
    describeChannelModeratedByAppInstanceUser_chimeBearer,
    describeChannelModeratedByAppInstanceUserResponse_channel,
    describeChannelModeratedByAppInstanceUserResponse_httpStatus,

    -- ** DescribeChannelModerator
    describeChannelModerator_channelArn,
    describeChannelModerator_channelModeratorArn,
    describeChannelModerator_chimeBearer,
    describeChannelModeratorResponse_channelModerator,
    describeChannelModeratorResponse_httpStatus,

    -- ** DisassociateChannelFlow
    disassociateChannelFlow_channelArn,
    disassociateChannelFlow_channelFlowArn,
    disassociateChannelFlow_chimeBearer,

    -- ** GetChannelMembershipPreferences
    getChannelMembershipPreferences_channelArn,
    getChannelMembershipPreferences_memberArn,
    getChannelMembershipPreferences_chimeBearer,
    getChannelMembershipPreferencesResponse_channelArn,
    getChannelMembershipPreferencesResponse_member,
    getChannelMembershipPreferencesResponse_preferences,
    getChannelMembershipPreferencesResponse_httpStatus,

    -- ** GetChannelMessage
    getChannelMessage_subChannelId,
    getChannelMessage_channelArn,
    getChannelMessage_messageId,
    getChannelMessage_chimeBearer,
    getChannelMessageResponse_channelMessage,
    getChannelMessageResponse_httpStatus,

    -- ** GetChannelMessageStatus
    getChannelMessageStatus_subChannelId,
    getChannelMessageStatus_channelArn,
    getChannelMessageStatus_messageId,
    getChannelMessageStatus_chimeBearer,
    getChannelMessageStatusResponse_status,
    getChannelMessageStatusResponse_httpStatus,

    -- ** GetMessagingSessionEndpoint
    getMessagingSessionEndpointResponse_endpoint,
    getMessagingSessionEndpointResponse_httpStatus,

    -- ** ListChannelBans
    listChannelBans_maxResults,
    listChannelBans_nextToken,
    listChannelBans_channelArn,
    listChannelBans_chimeBearer,
    listChannelBansResponse_channelArn,
    listChannelBansResponse_channelBans,
    listChannelBansResponse_nextToken,
    listChannelBansResponse_httpStatus,

    -- ** ListChannelFlows
    listChannelFlows_maxResults,
    listChannelFlows_nextToken,
    listChannelFlows_appInstanceArn,
    listChannelFlowsResponse_channelFlows,
    listChannelFlowsResponse_nextToken,
    listChannelFlowsResponse_httpStatus,

    -- ** ListChannelMemberships
    listChannelMemberships_maxResults,
    listChannelMemberships_nextToken,
    listChannelMemberships_subChannelId,
    listChannelMemberships_type,
    listChannelMemberships_channelArn,
    listChannelMemberships_chimeBearer,
    listChannelMembershipsResponse_channelArn,
    listChannelMembershipsResponse_channelMemberships,
    listChannelMembershipsResponse_nextToken,
    listChannelMembershipsResponse_httpStatus,

    -- ** ListChannelMembershipsForAppInstanceUser
    listChannelMembershipsForAppInstanceUser_appInstanceUserArn,
    listChannelMembershipsForAppInstanceUser_maxResults,
    listChannelMembershipsForAppInstanceUser_nextToken,
    listChannelMembershipsForAppInstanceUser_chimeBearer,
    listChannelMembershipsForAppInstanceUserResponse_channelMemberships,
    listChannelMembershipsForAppInstanceUserResponse_nextToken,
    listChannelMembershipsForAppInstanceUserResponse_httpStatus,

    -- ** ListChannelMessages
    listChannelMessages_maxResults,
    listChannelMessages_nextToken,
    listChannelMessages_notAfter,
    listChannelMessages_notBefore,
    listChannelMessages_sortOrder,
    listChannelMessages_subChannelId,
    listChannelMessages_channelArn,
    listChannelMessages_chimeBearer,
    listChannelMessagesResponse_channelArn,
    listChannelMessagesResponse_channelMessages,
    listChannelMessagesResponse_nextToken,
    listChannelMessagesResponse_subChannelId,
    listChannelMessagesResponse_httpStatus,

    -- ** ListChannelModerators
    listChannelModerators_maxResults,
    listChannelModerators_nextToken,
    listChannelModerators_channelArn,
    listChannelModerators_chimeBearer,
    listChannelModeratorsResponse_channelArn,
    listChannelModeratorsResponse_channelModerators,
    listChannelModeratorsResponse_nextToken,
    listChannelModeratorsResponse_httpStatus,

    -- ** ListChannels
    listChannels_maxResults,
    listChannels_nextToken,
    listChannels_privacy,
    listChannels_appInstanceArn,
    listChannels_chimeBearer,
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListChannelsAssociatedWithChannelFlow
    listChannelsAssociatedWithChannelFlow_maxResults,
    listChannelsAssociatedWithChannelFlow_nextToken,
    listChannelsAssociatedWithChannelFlow_channelFlowArn,
    listChannelsAssociatedWithChannelFlowResponse_channels,
    listChannelsAssociatedWithChannelFlowResponse_nextToken,
    listChannelsAssociatedWithChannelFlowResponse_httpStatus,

    -- ** ListChannelsModeratedByAppInstanceUser
    listChannelsModeratedByAppInstanceUser_appInstanceUserArn,
    listChannelsModeratedByAppInstanceUser_maxResults,
    listChannelsModeratedByAppInstanceUser_nextToken,
    listChannelsModeratedByAppInstanceUser_chimeBearer,
    listChannelsModeratedByAppInstanceUserResponse_channels,
    listChannelsModeratedByAppInstanceUserResponse_nextToken,
    listChannelsModeratedByAppInstanceUserResponse_httpStatus,

    -- ** ListSubChannels
    listSubChannels_maxResults,
    listSubChannels_nextToken,
    listSubChannels_channelArn,
    listSubChannels_chimeBearer,
    listSubChannelsResponse_channelArn,
    listSubChannelsResponse_nextToken,
    listSubChannelsResponse_subChannels,
    listSubChannelsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutChannelMembershipPreferences
    putChannelMembershipPreferences_channelArn,
    putChannelMembershipPreferences_memberArn,
    putChannelMembershipPreferences_chimeBearer,
    putChannelMembershipPreferences_preferences,
    putChannelMembershipPreferencesResponse_channelArn,
    putChannelMembershipPreferencesResponse_member,
    putChannelMembershipPreferencesResponse_preferences,
    putChannelMembershipPreferencesResponse_httpStatus,

    -- ** RedactChannelMessage
    redactChannelMessage_subChannelId,
    redactChannelMessage_channelArn,
    redactChannelMessage_messageId,
    redactChannelMessage_chimeBearer,
    redactChannelMessageResponse_channelArn,
    redactChannelMessageResponse_messageId,
    redactChannelMessageResponse_subChannelId,
    redactChannelMessageResponse_httpStatus,

    -- ** SearchChannels
    searchChannels_chimeBearer,
    searchChannels_maxResults,
    searchChannels_nextToken,
    searchChannels_fields,
    searchChannelsResponse_channels,
    searchChannelsResponse_nextToken,
    searchChannelsResponse_httpStatus,

    -- ** SendChannelMessage
    sendChannelMessage_messageAttributes,
    sendChannelMessage_metadata,
    sendChannelMessage_pushNotification,
    sendChannelMessage_subChannelId,
    sendChannelMessage_channelArn,
    sendChannelMessage_content,
    sendChannelMessage_type,
    sendChannelMessage_persistence,
    sendChannelMessage_clientRequestToken,
    sendChannelMessage_chimeBearer,
    sendChannelMessageResponse_channelArn,
    sendChannelMessageResponse_messageId,
    sendChannelMessageResponse_status,
    sendChannelMessageResponse_subChannelId,
    sendChannelMessageResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,

    -- ** UpdateChannel
    updateChannel_metadata,
    updateChannel_mode,
    updateChannel_name,
    updateChannel_channelArn,
    updateChannel_chimeBearer,
    updateChannelResponse_channelArn,
    updateChannelResponse_httpStatus,

    -- ** UpdateChannelFlow
    updateChannelFlow_channelFlowArn,
    updateChannelFlow_processors,
    updateChannelFlow_name,
    updateChannelFlowResponse_channelFlowArn,
    updateChannelFlowResponse_httpStatus,

    -- ** UpdateChannelMessage
    updateChannelMessage_content,
    updateChannelMessage_metadata,
    updateChannelMessage_subChannelId,
    updateChannelMessage_channelArn,
    updateChannelMessage_messageId,
    updateChannelMessage_chimeBearer,
    updateChannelMessageResponse_channelArn,
    updateChannelMessageResponse_messageId,
    updateChannelMessageResponse_status,
    updateChannelMessageResponse_subChannelId,
    updateChannelMessageResponse_httpStatus,

    -- ** UpdateChannelReadMarker
    updateChannelReadMarker_subChannelId,
    updateChannelReadMarker_channelArn,
    updateChannelReadMarker_chimeBearer,
    updateChannelReadMarkerResponse_channelArn,
    updateChannelReadMarkerResponse_subChannelId,
    updateChannelReadMarkerResponse_httpStatus,

    -- * Types

    -- ** AppInstanceUserMembershipSummary
    appInstanceUserMembershipSummary_readMarkerTimestamp,
    appInstanceUserMembershipSummary_subChannelId,
    appInstanceUserMembershipSummary_type,

    -- ** BatchChannelMemberships
    batchChannelMemberships_channelArn,
    batchChannelMemberships_invitedBy,
    batchChannelMemberships_members,
    batchChannelMemberships_subChannelId,
    batchChannelMemberships_type,

    -- ** BatchCreateChannelMembershipError
    batchCreateChannelMembershipError_errorCode,
    batchCreateChannelMembershipError_errorMessage,
    batchCreateChannelMembershipError_memberArn,

    -- ** Channel
    channel_channelArn,
    channel_channelFlowArn,
    channel_createdBy,
    channel_createdTimestamp,
    channel_elasticChannelConfiguration,
    channel_lastMessageTimestamp,
    channel_lastUpdatedTimestamp,
    channel_metadata,
    channel_mode,
    channel_name,
    channel_privacy,

    -- ** ChannelAssociatedWithFlowSummary
    channelAssociatedWithFlowSummary_channelArn,
    channelAssociatedWithFlowSummary_metadata,
    channelAssociatedWithFlowSummary_mode,
    channelAssociatedWithFlowSummary_name,
    channelAssociatedWithFlowSummary_privacy,

    -- ** ChannelBan
    channelBan_channelArn,
    channelBan_createdBy,
    channelBan_createdTimestamp,
    channelBan_member,

    -- ** ChannelBanSummary
    channelBanSummary_member,

    -- ** ChannelFlow
    channelFlow_channelFlowArn,
    channelFlow_createdTimestamp,
    channelFlow_lastUpdatedTimestamp,
    channelFlow_name,
    channelFlow_processors,

    -- ** ChannelFlowSummary
    channelFlowSummary_channelFlowArn,
    channelFlowSummary_name,
    channelFlowSummary_processors,

    -- ** ChannelMembership
    channelMembership_channelArn,
    channelMembership_createdTimestamp,
    channelMembership_invitedBy,
    channelMembership_lastUpdatedTimestamp,
    channelMembership_member,
    channelMembership_subChannelId,
    channelMembership_type,

    -- ** ChannelMembershipForAppInstanceUserSummary
    channelMembershipForAppInstanceUserSummary_appInstanceUserMembershipSummary,
    channelMembershipForAppInstanceUserSummary_channelSummary,

    -- ** ChannelMembershipPreferences
    channelMembershipPreferences_pushNotifications,

    -- ** ChannelMembershipSummary
    channelMembershipSummary_member,

    -- ** ChannelMessage
    channelMessage_channelArn,
    channelMessage_content,
    channelMessage_createdTimestamp,
    channelMessage_lastEditedTimestamp,
    channelMessage_lastUpdatedTimestamp,
    channelMessage_messageAttributes,
    channelMessage_messageId,
    channelMessage_metadata,
    channelMessage_persistence,
    channelMessage_redacted,
    channelMessage_sender,
    channelMessage_status,
    channelMessage_subChannelId,
    channelMessage_type,

    -- ** ChannelMessageCallback
    channelMessageCallback_content,
    channelMessageCallback_messageAttributes,
    channelMessageCallback_metadata,
    channelMessageCallback_pushNotification,
    channelMessageCallback_subChannelId,
    channelMessageCallback_messageId,

    -- ** ChannelMessageStatusStructure
    channelMessageStatusStructure_detail,
    channelMessageStatusStructure_value,

    -- ** ChannelMessageSummary
    channelMessageSummary_content,
    channelMessageSummary_createdTimestamp,
    channelMessageSummary_lastEditedTimestamp,
    channelMessageSummary_lastUpdatedTimestamp,
    channelMessageSummary_messageAttributes,
    channelMessageSummary_messageId,
    channelMessageSummary_metadata,
    channelMessageSummary_redacted,
    channelMessageSummary_sender,
    channelMessageSummary_status,
    channelMessageSummary_type,

    -- ** ChannelModeratedByAppInstanceUserSummary
    channelModeratedByAppInstanceUserSummary_channelSummary,

    -- ** ChannelModerator
    channelModerator_channelArn,
    channelModerator_createdBy,
    channelModerator_createdTimestamp,
    channelModerator_moderator,

    -- ** ChannelModeratorSummary
    channelModeratorSummary_moderator,

    -- ** ChannelSummary
    channelSummary_channelArn,
    channelSummary_lastMessageTimestamp,
    channelSummary_metadata,
    channelSummary_mode,
    channelSummary_name,
    channelSummary_privacy,

    -- ** ElasticChannelConfiguration
    elasticChannelConfiguration_maximumSubChannels,
    elasticChannelConfiguration_targetMembershipsPerSubChannel,
    elasticChannelConfiguration_minimumMembershipPercentage,

    -- ** Identity
    identity_arn,
    identity_name,

    -- ** LambdaConfiguration
    lambdaConfiguration_resourceArn,
    lambdaConfiguration_invocationType,

    -- ** MessageAttributeValue
    messageAttributeValue_stringValues,

    -- ** MessagingSessionEndpoint
    messagingSessionEndpoint_url,

    -- ** Processor
    processor_name,
    processor_configuration,
    processor_executionOrder,
    processor_fallbackAction,

    -- ** ProcessorConfiguration
    processorConfiguration_lambda,

    -- ** PushNotificationConfiguration
    pushNotificationConfiguration_body,
    pushNotificationConfiguration_title,
    pushNotificationConfiguration_type,

    -- ** PushNotificationPreferences
    pushNotificationPreferences_filterRule,
    pushNotificationPreferences_allowNotifications,

    -- ** SearchField
    searchField_key,
    searchField_values,
    searchField_operator,

    -- ** SubChannelSummary
    subChannelSummary_membershipCount,
    subChannelSummary_subChannelId,

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
import Amazonka.ChimeSDKMessaging.GetChannelMembershipPreferences
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
import Amazonka.ChimeSDKMessaging.ListSubChannels
import Amazonka.ChimeSDKMessaging.ListTagsForResource
import Amazonka.ChimeSDKMessaging.PutChannelMembershipPreferences
import Amazonka.ChimeSDKMessaging.RedactChannelMessage
import Amazonka.ChimeSDKMessaging.SearchChannels
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
import Amazonka.ChimeSDKMessaging.Types.ChannelMembershipPreferences
import Amazonka.ChimeSDKMessaging.Types.ChannelMembershipSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelMessage
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageCallback
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatusStructure
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelModeratedByAppInstanceUserSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelModerator
import Amazonka.ChimeSDKMessaging.Types.ChannelModeratorSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelSummary
import Amazonka.ChimeSDKMessaging.Types.ElasticChannelConfiguration
import Amazonka.ChimeSDKMessaging.Types.Identity
import Amazonka.ChimeSDKMessaging.Types.LambdaConfiguration
import Amazonka.ChimeSDKMessaging.Types.MessageAttributeValue
import Amazonka.ChimeSDKMessaging.Types.MessagingSessionEndpoint
import Amazonka.ChimeSDKMessaging.Types.Processor
import Amazonka.ChimeSDKMessaging.Types.ProcessorConfiguration
import Amazonka.ChimeSDKMessaging.Types.PushNotificationConfiguration
import Amazonka.ChimeSDKMessaging.Types.PushNotificationPreferences
import Amazonka.ChimeSDKMessaging.Types.SearchField
import Amazonka.ChimeSDKMessaging.Types.SubChannelSummary
import Amazonka.ChimeSDKMessaging.Types.Tag
import Amazonka.ChimeSDKMessaging.UntagResource
import Amazonka.ChimeSDKMessaging.UpdateChannel
import Amazonka.ChimeSDKMessaging.UpdateChannelFlow
import Amazonka.ChimeSDKMessaging.UpdateChannelMessage
import Amazonka.ChimeSDKMessaging.UpdateChannelReadMarker
