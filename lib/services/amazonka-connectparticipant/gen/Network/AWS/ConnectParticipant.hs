{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.ConnectParticipant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-09-07@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Connect is a cloud-based contact center solution that makes it
-- easy to set up and manage a customer contact center and provide reliable
-- customer engagement at any scale.
--
-- Amazon Connect enables customer contacts through voice or chat.
--
-- The APIs described here are used by chat participants, such as agents
-- and customers.
module Network.AWS.ConnectParticipant
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** SendMessage
    SendMessage (SendMessage'),
    newSendMessage,
    SendMessageResponse (SendMessageResponse'),
    newSendMessageResponse,

    -- ** DisconnectParticipant
    DisconnectParticipant (DisconnectParticipant'),
    newDisconnectParticipant,
    DisconnectParticipantResponse (DisconnectParticipantResponse'),
    newDisconnectParticipantResponse,

    -- ** CompleteAttachmentUpload
    CompleteAttachmentUpload (CompleteAttachmentUpload'),
    newCompleteAttachmentUpload,
    CompleteAttachmentUploadResponse (CompleteAttachmentUploadResponse'),
    newCompleteAttachmentUploadResponse,

    -- ** GetAttachment
    GetAttachment (GetAttachment'),
    newGetAttachment,
    GetAttachmentResponse (GetAttachmentResponse'),
    newGetAttachmentResponse,

    -- ** StartAttachmentUpload
    StartAttachmentUpload (StartAttachmentUpload'),
    newStartAttachmentUpload,
    StartAttachmentUploadResponse (StartAttachmentUploadResponse'),
    newStartAttachmentUploadResponse,

    -- ** CreateParticipantConnection
    CreateParticipantConnection (CreateParticipantConnection'),
    newCreateParticipantConnection,
    CreateParticipantConnectionResponse (CreateParticipantConnectionResponse'),
    newCreateParticipantConnectionResponse,

    -- ** GetTranscript
    GetTranscript (GetTranscript'),
    newGetTranscript,
    GetTranscriptResponse (GetTranscriptResponse'),
    newGetTranscriptResponse,

    -- ** SendEvent
    SendEvent (SendEvent'),
    newSendEvent,
    SendEventResponse (SendEventResponse'),
    newSendEventResponse,

    -- * Types

    -- ** ArtifactStatus
    ArtifactStatus (..),

    -- ** ChatItemType
    ChatItemType (..),

    -- ** ConnectionType
    ConnectionType (..),

    -- ** ParticipantRole
    ParticipantRole (..),

    -- ** ScanDirection
    ScanDirection (..),

    -- ** SortKey
    SortKey (..),

    -- ** AttachmentItem
    AttachmentItem (AttachmentItem'),
    newAttachmentItem,

    -- ** ConnectionCredentials
    ConnectionCredentials (ConnectionCredentials'),
    newConnectionCredentials,

    -- ** Item
    Item (Item'),
    newItem,

    -- ** StartPosition
    StartPosition (StartPosition'),
    newStartPosition,

    -- ** UploadMetadata
    UploadMetadata (UploadMetadata'),
    newUploadMetadata,

    -- ** Websocket
    Websocket (Websocket'),
    newWebsocket,
  )
where

import Network.AWS.ConnectParticipant.CompleteAttachmentUpload
import Network.AWS.ConnectParticipant.CreateParticipantConnection
import Network.AWS.ConnectParticipant.DisconnectParticipant
import Network.AWS.ConnectParticipant.GetAttachment
import Network.AWS.ConnectParticipant.GetTranscript
import Network.AWS.ConnectParticipant.Lens
import Network.AWS.ConnectParticipant.SendEvent
import Network.AWS.ConnectParticipant.SendMessage
import Network.AWS.ConnectParticipant.StartAttachmentUpload
import Network.AWS.ConnectParticipant.Types
import Network.AWS.ConnectParticipant.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ConnectParticipant'.

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
