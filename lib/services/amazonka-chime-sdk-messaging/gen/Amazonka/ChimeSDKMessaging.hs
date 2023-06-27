{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ChimeSDKMessaging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-05-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amazon Chime SDK messaging APIs in this section allow software
-- developers to send and receive messages in custom messaging
-- applications. These APIs depend on the frameworks provided by the Amazon
-- Chime SDK identity APIs. For more information about the messaging APIs,
-- see
-- <https://docs.aws.amazon.com/chime/latest/APIReference/API_Operations_Amazon_Chime_SDK_Messaging.html Amazon Chime SDK messaging>.
module Amazonka.ChimeSDKMessaging
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottledClientException
    _ThrottledClientException,

    -- ** UnauthorizedClientException
    _UnauthorizedClientException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateChannelFlow
    AssociateChannelFlow (AssociateChannelFlow'),
    newAssociateChannelFlow,
    AssociateChannelFlowResponse (AssociateChannelFlowResponse'),
    newAssociateChannelFlowResponse,

    -- ** BatchCreateChannelMembership
    BatchCreateChannelMembership (BatchCreateChannelMembership'),
    newBatchCreateChannelMembership,
    BatchCreateChannelMembershipResponse (BatchCreateChannelMembershipResponse'),
    newBatchCreateChannelMembershipResponse,

    -- ** ChannelFlowCallback
    ChannelFlowCallback (ChannelFlowCallback'),
    newChannelFlowCallback,
    ChannelFlowCallbackResponse (ChannelFlowCallbackResponse'),
    newChannelFlowCallbackResponse,

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** CreateChannelBan
    CreateChannelBan (CreateChannelBan'),
    newCreateChannelBan,
    CreateChannelBanResponse (CreateChannelBanResponse'),
    newCreateChannelBanResponse,

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

    -- ** CreateChannelModerator
    CreateChannelModerator (CreateChannelModerator'),
    newCreateChannelModerator,
    CreateChannelModeratorResponse (CreateChannelModeratorResponse'),
    newCreateChannelModeratorResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** DeleteChannelBan
    DeleteChannelBan (DeleteChannelBan'),
    newDeleteChannelBan,
    DeleteChannelBanResponse (DeleteChannelBanResponse'),
    newDeleteChannelBanResponse,

    -- ** DeleteChannelFlow
    DeleteChannelFlow (DeleteChannelFlow'),
    newDeleteChannelFlow,
    DeleteChannelFlowResponse (DeleteChannelFlowResponse'),
    newDeleteChannelFlowResponse,

    -- ** DeleteChannelMembership
    DeleteChannelMembership (DeleteChannelMembership'),
    newDeleteChannelMembership,
    DeleteChannelMembershipResponse (DeleteChannelMembershipResponse'),
    newDeleteChannelMembershipResponse,

    -- ** DeleteChannelMessage
    DeleteChannelMessage (DeleteChannelMessage'),
    newDeleteChannelMessage,
    DeleteChannelMessageResponse (DeleteChannelMessageResponse'),
    newDeleteChannelMessageResponse,

    -- ** DeleteChannelModerator
    DeleteChannelModerator (DeleteChannelModerator'),
    newDeleteChannelModerator,
    DeleteChannelModeratorResponse (DeleteChannelModeratorResponse'),
    newDeleteChannelModeratorResponse,

    -- ** DeleteMessagingStreamingConfigurations
    DeleteMessagingStreamingConfigurations (DeleteMessagingStreamingConfigurations'),
    newDeleteMessagingStreamingConfigurations,
    DeleteMessagingStreamingConfigurationsResponse (DeleteMessagingStreamingConfigurationsResponse'),
    newDeleteMessagingStreamingConfigurationsResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** DescribeChannelBan
    DescribeChannelBan (DescribeChannelBan'),
    newDescribeChannelBan,
    DescribeChannelBanResponse (DescribeChannelBanResponse'),
    newDescribeChannelBanResponse,

    -- ** DescribeChannelFlow
    DescribeChannelFlow (DescribeChannelFlow'),
    newDescribeChannelFlow,
    DescribeChannelFlowResponse (DescribeChannelFlowResponse'),
    newDescribeChannelFlowResponse,

    -- ** DescribeChannelMembership
    DescribeChannelMembership (DescribeChannelMembership'),
    newDescribeChannelMembership,
    DescribeChannelMembershipResponse (DescribeChannelMembershipResponse'),
    newDescribeChannelMembershipResponse,

    -- ** DescribeChannelMembershipForAppInstanceUser
    DescribeChannelMembershipForAppInstanceUser (DescribeChannelMembershipForAppInstanceUser'),
    newDescribeChannelMembershipForAppInstanceUser,
    DescribeChannelMembershipForAppInstanceUserResponse (DescribeChannelMembershipForAppInstanceUserResponse'),
    newDescribeChannelMembershipForAppInstanceUserResponse,

    -- ** DescribeChannelModeratedByAppInstanceUser
    DescribeChannelModeratedByAppInstanceUser (DescribeChannelModeratedByAppInstanceUser'),
    newDescribeChannelModeratedByAppInstanceUser,
    DescribeChannelModeratedByAppInstanceUserResponse (DescribeChannelModeratedByAppInstanceUserResponse'),
    newDescribeChannelModeratedByAppInstanceUserResponse,

    -- ** DescribeChannelModerator
    DescribeChannelModerator (DescribeChannelModerator'),
    newDescribeChannelModerator,
    DescribeChannelModeratorResponse (DescribeChannelModeratorResponse'),
    newDescribeChannelModeratorResponse,

    -- ** DisassociateChannelFlow
    DisassociateChannelFlow (DisassociateChannelFlow'),
    newDisassociateChannelFlow,
    DisassociateChannelFlowResponse (DisassociateChannelFlowResponse'),
    newDisassociateChannelFlowResponse,

    -- ** GetChannelMembershipPreferences
    GetChannelMembershipPreferences (GetChannelMembershipPreferences'),
    newGetChannelMembershipPreferences,
    GetChannelMembershipPreferencesResponse (GetChannelMembershipPreferencesResponse'),
    newGetChannelMembershipPreferencesResponse,

    -- ** GetChannelMessage
    GetChannelMessage (GetChannelMessage'),
    newGetChannelMessage,
    GetChannelMessageResponse (GetChannelMessageResponse'),
    newGetChannelMessageResponse,

    -- ** GetChannelMessageStatus
    GetChannelMessageStatus (GetChannelMessageStatus'),
    newGetChannelMessageStatus,
    GetChannelMessageStatusResponse (GetChannelMessageStatusResponse'),
    newGetChannelMessageStatusResponse,

    -- ** GetMessagingSessionEndpoint
    GetMessagingSessionEndpoint (GetMessagingSessionEndpoint'),
    newGetMessagingSessionEndpoint,
    GetMessagingSessionEndpointResponse (GetMessagingSessionEndpointResponse'),
    newGetMessagingSessionEndpointResponse,

    -- ** GetMessagingStreamingConfigurations
    GetMessagingStreamingConfigurations (GetMessagingStreamingConfigurations'),
    newGetMessagingStreamingConfigurations,
    GetMessagingStreamingConfigurationsResponse (GetMessagingStreamingConfigurationsResponse'),
    newGetMessagingStreamingConfigurationsResponse,

    -- ** ListChannelBans
    ListChannelBans (ListChannelBans'),
    newListChannelBans,
    ListChannelBansResponse (ListChannelBansResponse'),
    newListChannelBansResponse,

    -- ** ListChannelFlows
    ListChannelFlows (ListChannelFlows'),
    newListChannelFlows,
    ListChannelFlowsResponse (ListChannelFlowsResponse'),
    newListChannelFlowsResponse,

    -- ** ListChannelMemberships
    ListChannelMemberships (ListChannelMemberships'),
    newListChannelMemberships,
    ListChannelMembershipsResponse (ListChannelMembershipsResponse'),
    newListChannelMembershipsResponse,

    -- ** ListChannelMembershipsForAppInstanceUser
    ListChannelMembershipsForAppInstanceUser (ListChannelMembershipsForAppInstanceUser'),
    newListChannelMembershipsForAppInstanceUser,
    ListChannelMembershipsForAppInstanceUserResponse (ListChannelMembershipsForAppInstanceUserResponse'),
    newListChannelMembershipsForAppInstanceUserResponse,

    -- ** ListChannelMessages
    ListChannelMessages (ListChannelMessages'),
    newListChannelMessages,
    ListChannelMessagesResponse (ListChannelMessagesResponse'),
    newListChannelMessagesResponse,

    -- ** ListChannelModerators
    ListChannelModerators (ListChannelModerators'),
    newListChannelModerators,
    ListChannelModeratorsResponse (ListChannelModeratorsResponse'),
    newListChannelModeratorsResponse,

    -- ** ListChannels
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** ListChannelsAssociatedWithChannelFlow
    ListChannelsAssociatedWithChannelFlow (ListChannelsAssociatedWithChannelFlow'),
    newListChannelsAssociatedWithChannelFlow,
    ListChannelsAssociatedWithChannelFlowResponse (ListChannelsAssociatedWithChannelFlowResponse'),
    newListChannelsAssociatedWithChannelFlowResponse,

    -- ** ListChannelsModeratedByAppInstanceUser
    ListChannelsModeratedByAppInstanceUser (ListChannelsModeratedByAppInstanceUser'),
    newListChannelsModeratedByAppInstanceUser,
    ListChannelsModeratedByAppInstanceUserResponse (ListChannelsModeratedByAppInstanceUserResponse'),
    newListChannelsModeratedByAppInstanceUserResponse,

    -- ** ListSubChannels
    ListSubChannels (ListSubChannels'),
    newListSubChannels,
    ListSubChannelsResponse (ListSubChannelsResponse'),
    newListSubChannelsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutChannelExpirationSettings
    PutChannelExpirationSettings (PutChannelExpirationSettings'),
    newPutChannelExpirationSettings,
    PutChannelExpirationSettingsResponse (PutChannelExpirationSettingsResponse'),
    newPutChannelExpirationSettingsResponse,

    -- ** PutChannelMembershipPreferences
    PutChannelMembershipPreferences (PutChannelMembershipPreferences'),
    newPutChannelMembershipPreferences,
    PutChannelMembershipPreferencesResponse (PutChannelMembershipPreferencesResponse'),
    newPutChannelMembershipPreferencesResponse,

    -- ** PutMessagingStreamingConfigurations
    PutMessagingStreamingConfigurations (PutMessagingStreamingConfigurations'),
    newPutMessagingStreamingConfigurations,
    PutMessagingStreamingConfigurationsResponse (PutMessagingStreamingConfigurationsResponse'),
    newPutMessagingStreamingConfigurationsResponse,

    -- ** RedactChannelMessage
    RedactChannelMessage (RedactChannelMessage'),
    newRedactChannelMessage,
    RedactChannelMessageResponse (RedactChannelMessageResponse'),
    newRedactChannelMessageResponse,

    -- ** SearchChannels
    SearchChannels (SearchChannels'),
    newSearchChannels,
    SearchChannelsResponse (SearchChannelsResponse'),
    newSearchChannelsResponse,

    -- ** SendChannelMessage
    SendChannelMessage (SendChannelMessage'),
    newSendChannelMessage,
    SendChannelMessageResponse (SendChannelMessageResponse'),
    newSendChannelMessageResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateChannel
    UpdateChannel (UpdateChannel'),
    newUpdateChannel,
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- ** UpdateChannelFlow
    UpdateChannelFlow (UpdateChannelFlow'),
    newUpdateChannelFlow,
    UpdateChannelFlowResponse (UpdateChannelFlowResponse'),
    newUpdateChannelFlowResponse,

    -- ** UpdateChannelMessage
    UpdateChannelMessage (UpdateChannelMessage'),
    newUpdateChannelMessage,
    UpdateChannelMessageResponse (UpdateChannelMessageResponse'),
    newUpdateChannelMessageResponse,

    -- ** UpdateChannelReadMarker
    UpdateChannelReadMarker (UpdateChannelReadMarker'),
    newUpdateChannelReadMarker,
    UpdateChannelReadMarkerResponse (UpdateChannelReadMarkerResponse'),
    newUpdateChannelReadMarkerResponse,

    -- * Types

    -- ** AllowNotifications
    AllowNotifications (..),

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

    -- ** ExpirationCriterion
    ExpirationCriterion (..),

    -- ** FallbackAction
    FallbackAction (..),

    -- ** InvocationType
    InvocationType (..),

    -- ** MessagingDataType
    MessagingDataType (..),

    -- ** PushNotificationType
    PushNotificationType (..),

    -- ** SearchFieldKey
    SearchFieldKey (..),

    -- ** SearchFieldOperator
    SearchFieldOperator (..),

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

    -- ** ChannelMembershipPreferences
    ChannelMembershipPreferences (ChannelMembershipPreferences'),
    newChannelMembershipPreferences,

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

    -- ** ElasticChannelConfiguration
    ElasticChannelConfiguration (ElasticChannelConfiguration'),
    newElasticChannelConfiguration,

    -- ** ExpirationSettings
    ExpirationSettings (ExpirationSettings'),
    newExpirationSettings,

    -- ** Identity
    Identity (Identity'),
    newIdentity,

    -- ** LambdaConfiguration
    LambdaConfiguration (LambdaConfiguration'),
    newLambdaConfiguration,

    -- ** MessageAttributeValue
    MessageAttributeValue (MessageAttributeValue'),
    newMessageAttributeValue,

    -- ** MessagingSessionEndpoint
    MessagingSessionEndpoint (MessagingSessionEndpoint'),
    newMessagingSessionEndpoint,

    -- ** Processor
    Processor (Processor'),
    newProcessor,

    -- ** ProcessorConfiguration
    ProcessorConfiguration (ProcessorConfiguration'),
    newProcessorConfiguration,

    -- ** PushNotificationConfiguration
    PushNotificationConfiguration (PushNotificationConfiguration'),
    newPushNotificationConfiguration,

    -- ** PushNotificationPreferences
    PushNotificationPreferences (PushNotificationPreferences'),
    newPushNotificationPreferences,

    -- ** SearchField
    SearchField (SearchField'),
    newSearchField,

    -- ** StreamingConfiguration
    StreamingConfiguration (StreamingConfiguration'),
    newStreamingConfiguration,

    -- ** SubChannelSummary
    SubChannelSummary (SubChannelSummary'),
    newSubChannelSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Target
    Target (Target'),
    newTarget,
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
import Amazonka.ChimeSDKMessaging.DeleteMessagingStreamingConfigurations
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
import Amazonka.ChimeSDKMessaging.GetMessagingStreamingConfigurations
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
import Amazonka.ChimeSDKMessaging.ListSubChannels
import Amazonka.ChimeSDKMessaging.ListTagsForResource
import Amazonka.ChimeSDKMessaging.PutChannelExpirationSettings
import Amazonka.ChimeSDKMessaging.PutChannelMembershipPreferences
import Amazonka.ChimeSDKMessaging.PutMessagingStreamingConfigurations
import Amazonka.ChimeSDKMessaging.RedactChannelMessage
import Amazonka.ChimeSDKMessaging.SearchChannels
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
