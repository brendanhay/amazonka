{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IVSChat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-07-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- __Introduction__
--
-- The Amazon IVS Chat control-plane API enables you to create and manage
-- Amazon IVS Chat resources. You also need to integrate with the
-- <https://docs.aws.amazon.com/ivs/latest/chatmsgapireference/chat-messaging-api.html Amazon IVS Chat Messaging API>,
-- to enable users to interact with chat rooms in real time.
--
-- The API is an AWS regional service. For a list of supported regions and
-- Amazon IVS Chat HTTPS service endpoints, see the Amazon IVS Chat
-- information on the
-- <https://docs.aws.amazon.com/general/latest/gr/ivs.html Amazon IVS page>
-- in the /AWS General Reference/.
--
-- __Notes on terminology:__
--
-- -   You create service applications using the Amazon IVS Chat API. We
--     refer to these as /applications/.
--
-- -   You create front-end client applications (browser and Android\/iOS
--     apps) using the Amazon IVS Chat Messaging API. We refer to these as
--     /clients/.
--
-- __Resources__
--
-- The following resources are part of Amazon IVS Chat:
--
-- -   __LoggingConfiguration__ — A configuration that allows customers to
--     store and record sent messages in a chat room. See the Logging
--     Configuration endpoints for more information.
--
-- -   __Room__ — The central Amazon IVS Chat resource through which
--     clients connect to and exchange chat messages. See the Room
--     endpoints for more information.
--
-- __Tagging__
--
-- A /tag/ is a metadata label that you assign to an AWS resource. A tag
-- comprises a /key/ and a /value/, both set by you. For example, you might
-- set a tag as @topic:nature@ to label a particular video category. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS Chat has no
-- service-specific constraints beyond what is documented there.
--
-- Tags can help you identify and organize your AWS resources. For example,
-- you can use the same tag for different resources to indicate that they
-- are related. You can also use tags to manage access (see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Access Tags>).
--
-- The Amazon IVS Chat API has these tag-related endpoints: TagResource,
-- UntagResource, and ListTagsForResource. The following resource supports
-- tagging: Room.
--
-- At most 50 tags can be applied to a resource.
--
-- __API Access Security__
--
-- Your Amazon IVS Chat applications (service applications and clients)
-- must be authenticated and authorized to access Amazon IVS Chat
-- resources. Note the differences between these concepts:
--
-- -   /Authentication/ is about verifying identity. Requests to the Amazon
--     IVS Chat API must be signed to verify your identity.
--
-- -   /Authorization/ is about granting permissions. Your IAM roles need
--     to have permissions for Amazon IVS Chat API requests.
--
-- Users (viewers) connect to a room using secure access tokens that you
-- create using the CreateChatToken endpoint through the AWS SDK. You call
-- CreateChatToken for every user’s chat session, passing identity and
-- authorization information about the user.
--
-- __Signing API Requests__
--
-- HTTP API requests must be signed with an AWS SigV4 signature using your
-- AWS security credentials. The AWS Command Line Interface (CLI) and the
-- AWS SDKs take care of signing the underlying API calls for you. However,
-- if your application calls the Amazon IVS Chat HTTP API directly, it’s
-- your responsibility to sign the requests.
--
-- You generate a signature using valid AWS credentials for an IAM role
-- that has permission to perform the requested action. For example,
-- DeleteMessage requests must be made using an IAM role that has the
-- @ivschat:DeleteMessage@ permission.
--
-- For more information:
--
-- -   Authentication and generating signatures — See
--     <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (Amazon Web Services Signature Version 4)>
--     in the /Amazon Web Services General Reference/.
--
-- -   Managing Amazon IVS permissions — See
--     <https://docs.aws.amazon.com/ivs/latest/userguide/security-iam.html Identity and Access Management>
--     on the Security page of the /Amazon IVS User Guide/.
--
-- __Amazon Resource Names (ARNs)__
--
-- ARNs uniquely identify AWS resources. An ARN is required when you need
-- to specify a resource unambiguously across all of AWS, such as in IAM
-- policies and API calls. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names>
-- in the /AWS General Reference/.
--
-- __Messaging Endpoints__
--
-- -   DeleteMessage — Sends an event to a specific room which directs
--     clients to delete a specific message; that is, unrender it from view
--     and delete it from the client’s chat history. This event’s
--     @EventName@ is @aws:DELETE_MESSAGE@. This replicates the
--     <https://docs.aws.amazon.com/ivs/latest/chatmsgapireference/actions-deletemessage-publish.html DeleteMessage>
--     WebSocket operation in the Amazon IVS Chat Messaging API.
--
-- -   DisconnectUser — Disconnects all connections using a specified user
--     ID from a room. This replicates the
--     <https://docs.aws.amazon.com/ivs/latest/chatmsgapireference/actions-disconnectuser-publish.html DisconnectUser>
--     WebSocket operation in the Amazon IVS Chat Messaging API.
--
-- -   SendEvent — Sends an event to a room. Use this within your
--     application’s business logic to send events to clients of a room;
--     e.g., to notify clients to change the way the chat UI is rendered.
--
-- __Chat Token Endpoint__
--
-- -   CreateChatToken — Creates an encrypted token that is used by a chat
--     participant to establish an individual WebSocket chat connection to
--     a room. When the token is used to connect to chat, the connection is
--     valid for the session duration specified in the request. The token
--     becomes invalid at the token-expiration timestamp included in the
--     response.
--
-- __Room Endpoints__
--
-- -   CreateRoom — Creates a room that allows clients to connect and pass
--     messages.
--
-- -   DeleteRoom — Deletes the specified room.
--
-- -   GetRoom — Gets the specified room.
--
-- -   ListRooms — Gets summary information about all your rooms in the AWS
--     region where the API request is processed.
--
-- -   UpdateRoom — Updates a room’s configuration.
--
-- __Logging Configuration Endpoints__
--
-- -   CreateLoggingConfiguration — Creates a logging configuration that
--     allows clients to store and record sent messages.
--
-- -   DeleteLoggingConfiguration — Deletes the specified logging
--     configuration.
--
-- -   GetLoggingConfiguration — Gets the specified logging configuration.
--
-- -   ListLoggingConfigurations — Gets summary information about all your
--     logging configurations in the AWS region where the API request is
--     processed.
--
-- -   UpdateLoggingConfiguration — Updates a specified logging
--     configuration.
--
-- __Tags Endpoints__
--
-- -   ListTagsForResource — Gets information about AWS tags for the
--     specified ARN.
--
-- -   TagResource — Adds or updates tags for the AWS resource with the
--     specified ARN.
--
-- -   UntagResource — Removes tags from the resource with the specified
--     ARN.
--
-- All the above are HTTP operations. There is a separate /messaging/ API
-- for managing Chat resources; see the
-- <https://docs.aws.amazon.com/ivs/latest/chatmsgapireference/chat-messaging-api.html Amazon IVS Chat Messaging API Reference>.
module Amazonka.IVSChat
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** PendingVerification
    _PendingVerification,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateChatToken
    CreateChatToken (CreateChatToken'),
    newCreateChatToken,
    CreateChatTokenResponse (CreateChatTokenResponse'),
    newCreateChatTokenResponse,

    -- ** CreateLoggingConfiguration
    CreateLoggingConfiguration (CreateLoggingConfiguration'),
    newCreateLoggingConfiguration,
    CreateLoggingConfigurationResponse (CreateLoggingConfigurationResponse'),
    newCreateLoggingConfigurationResponse,

    -- ** CreateRoom
    CreateRoom (CreateRoom'),
    newCreateRoom,
    CreateRoomResponse (CreateRoomResponse'),
    newCreateRoomResponse,

    -- ** DeleteLoggingConfiguration
    DeleteLoggingConfiguration (DeleteLoggingConfiguration'),
    newDeleteLoggingConfiguration,
    DeleteLoggingConfigurationResponse (DeleteLoggingConfigurationResponse'),
    newDeleteLoggingConfigurationResponse,

    -- ** DeleteMessage
    DeleteMessage (DeleteMessage'),
    newDeleteMessage,
    DeleteMessageResponse (DeleteMessageResponse'),
    newDeleteMessageResponse,

    -- ** DeleteRoom
    DeleteRoom (DeleteRoom'),
    newDeleteRoom,
    DeleteRoomResponse (DeleteRoomResponse'),
    newDeleteRoomResponse,

    -- ** DisconnectUser
    DisconnectUser (DisconnectUser'),
    newDisconnectUser,
    DisconnectUserResponse (DisconnectUserResponse'),
    newDisconnectUserResponse,

    -- ** GetLoggingConfiguration
    GetLoggingConfiguration (GetLoggingConfiguration'),
    newGetLoggingConfiguration,
    GetLoggingConfigurationResponse (GetLoggingConfigurationResponse'),
    newGetLoggingConfigurationResponse,

    -- ** GetRoom
    GetRoom (GetRoom'),
    newGetRoom,
    GetRoomResponse (GetRoomResponse'),
    newGetRoomResponse,

    -- ** ListLoggingConfigurations
    ListLoggingConfigurations (ListLoggingConfigurations'),
    newListLoggingConfigurations,
    ListLoggingConfigurationsResponse (ListLoggingConfigurationsResponse'),
    newListLoggingConfigurationsResponse,

    -- ** ListRooms
    ListRooms (ListRooms'),
    newListRooms,
    ListRoomsResponse (ListRoomsResponse'),
    newListRoomsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** SendEvent
    SendEvent (SendEvent'),
    newSendEvent,
    SendEventResponse (SendEventResponse'),
    newSendEventResponse,

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

    -- ** UpdateLoggingConfiguration
    UpdateLoggingConfiguration (UpdateLoggingConfiguration'),
    newUpdateLoggingConfiguration,
    UpdateLoggingConfigurationResponse (UpdateLoggingConfigurationResponse'),
    newUpdateLoggingConfigurationResponse,

    -- ** UpdateRoom
    UpdateRoom (UpdateRoom'),
    newUpdateRoom,
    UpdateRoomResponse (UpdateRoomResponse'),
    newUpdateRoomResponse,

    -- * Types

    -- ** ChatTokenCapability
    ChatTokenCapability (..),

    -- ** CreateLoggingConfigurationState
    CreateLoggingConfigurationState (..),

    -- ** FallbackResult
    FallbackResult (..),

    -- ** LoggingConfigurationState
    LoggingConfigurationState (..),

    -- ** UpdateLoggingConfigurationState
    UpdateLoggingConfigurationState (..),

    -- ** CloudWatchLogsDestinationConfiguration
    CloudWatchLogsDestinationConfiguration (CloudWatchLogsDestinationConfiguration'),
    newCloudWatchLogsDestinationConfiguration,

    -- ** DestinationConfiguration
    DestinationConfiguration (DestinationConfiguration'),
    newDestinationConfiguration,

    -- ** FirehoseDestinationConfiguration
    FirehoseDestinationConfiguration (FirehoseDestinationConfiguration'),
    newFirehoseDestinationConfiguration,

    -- ** LoggingConfigurationSummary
    LoggingConfigurationSummary (LoggingConfigurationSummary'),
    newLoggingConfigurationSummary,

    -- ** MessageReviewHandler
    MessageReviewHandler (MessageReviewHandler'),
    newMessageReviewHandler,

    -- ** RoomSummary
    RoomSummary (RoomSummary'),
    newRoomSummary,

    -- ** S3DestinationConfiguration
    S3DestinationConfiguration (S3DestinationConfiguration'),
    newS3DestinationConfiguration,
  )
where

import Amazonka.IVSChat.CreateChatToken
import Amazonka.IVSChat.CreateLoggingConfiguration
import Amazonka.IVSChat.CreateRoom
import Amazonka.IVSChat.DeleteLoggingConfiguration
import Amazonka.IVSChat.DeleteMessage
import Amazonka.IVSChat.DeleteRoom
import Amazonka.IVSChat.DisconnectUser
import Amazonka.IVSChat.GetLoggingConfiguration
import Amazonka.IVSChat.GetRoom
import Amazonka.IVSChat.Lens
import Amazonka.IVSChat.ListLoggingConfigurations
import Amazonka.IVSChat.ListRooms
import Amazonka.IVSChat.ListTagsForResource
import Amazonka.IVSChat.SendEvent
import Amazonka.IVSChat.TagResource
import Amazonka.IVSChat.Types
import Amazonka.IVSChat.UntagResource
import Amazonka.IVSChat.UpdateLoggingConfiguration
import Amazonka.IVSChat.UpdateRoom
import Amazonka.IVSChat.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IVSChat'.

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
