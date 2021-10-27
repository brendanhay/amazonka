{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ConnectParticipant.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ConnectParticipant.Lens
  ( -- * Operations

    -- ** SendMessage
    sendMessage_clientToken,
    sendMessage_contentType,
    sendMessage_content,
    sendMessage_connectionToken,
    sendMessageResponse_absoluteTime,
    sendMessageResponse_id,
    sendMessageResponse_httpStatus,

    -- ** DisconnectParticipant
    disconnectParticipant_clientToken,
    disconnectParticipant_connectionToken,
    disconnectParticipantResponse_httpStatus,

    -- ** CompleteAttachmentUpload
    completeAttachmentUpload_attachmentIds,
    completeAttachmentUpload_clientToken,
    completeAttachmentUpload_connectionToken,
    completeAttachmentUploadResponse_httpStatus,

    -- ** GetAttachment
    getAttachment_attachmentId,
    getAttachment_connectionToken,
    getAttachmentResponse_urlExpiry,
    getAttachmentResponse_url,
    getAttachmentResponse_httpStatus,

    -- ** StartAttachmentUpload
    startAttachmentUpload_contentType,
    startAttachmentUpload_attachmentSizeInBytes,
    startAttachmentUpload_attachmentName,
    startAttachmentUpload_clientToken,
    startAttachmentUpload_connectionToken,
    startAttachmentUploadResponse_attachmentId,
    startAttachmentUploadResponse_uploadMetadata,
    startAttachmentUploadResponse_httpStatus,

    -- ** CreateParticipantConnection
    createParticipantConnection_type,
    createParticipantConnection_participantToken,
    createParticipantConnectionResponse_connectionCredentials,
    createParticipantConnectionResponse_websocket,
    createParticipantConnectionResponse_httpStatus,

    -- ** GetTranscript
    getTranscript_scanDirection,
    getTranscript_nextToken,
    getTranscript_sortOrder,
    getTranscript_contactId,
    getTranscript_maxResults,
    getTranscript_startPosition,
    getTranscript_connectionToken,
    getTranscriptResponse_nextToken,
    getTranscriptResponse_initialContactId,
    getTranscriptResponse_transcript,
    getTranscriptResponse_httpStatus,

    -- ** SendEvent
    sendEvent_clientToken,
    sendEvent_content,
    sendEvent_contentType,
    sendEvent_connectionToken,
    sendEventResponse_absoluteTime,
    sendEventResponse_id,
    sendEventResponse_httpStatus,

    -- * Types

    -- ** AttachmentItem
    attachmentItem_status,
    attachmentItem_attachmentName,
    attachmentItem_attachmentId,
    attachmentItem_contentType,

    -- ** ConnectionCredentials
    connectionCredentials_expiry,
    connectionCredentials_connectionToken,

    -- ** Item
    item_participantId,
    item_absoluteTime,
    item_attachments,
    item_participantRole,
    item_content,
    item_id,
    item_displayName,
    item_type,
    item_contentType,

    -- ** StartPosition
    startPosition_absoluteTime,
    startPosition_id,
    startPosition_mostRecent,

    -- ** UploadMetadata
    uploadMetadata_urlExpiry,
    uploadMetadata_headersToInclude,
    uploadMetadata_url,

    -- ** Websocket
    websocket_url,
    websocket_connectionExpiry,
  )
where

import Network.AWS.ConnectParticipant.CompleteAttachmentUpload
import Network.AWS.ConnectParticipant.CreateParticipantConnection
import Network.AWS.ConnectParticipant.DisconnectParticipant
import Network.AWS.ConnectParticipant.GetAttachment
import Network.AWS.ConnectParticipant.GetTranscript
import Network.AWS.ConnectParticipant.SendEvent
import Network.AWS.ConnectParticipant.SendMessage
import Network.AWS.ConnectParticipant.StartAttachmentUpload
import Network.AWS.ConnectParticipant.Types.AttachmentItem
import Network.AWS.ConnectParticipant.Types.ConnectionCredentials
import Network.AWS.ConnectParticipant.Types.Item
import Network.AWS.ConnectParticipant.Types.StartPosition
import Network.AWS.ConnectParticipant.Types.UploadMetadata
import Network.AWS.ConnectParticipant.Types.Websocket
