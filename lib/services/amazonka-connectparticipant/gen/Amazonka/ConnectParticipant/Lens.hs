{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectParticipant.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Lens
  ( -- * Operations

    -- ** CompleteAttachmentUpload
    completeAttachmentUpload_attachmentIds,
    completeAttachmentUpload_clientToken,
    completeAttachmentUpload_connectionToken,
    completeAttachmentUploadResponse_httpStatus,

    -- ** CreateParticipantConnection
    createParticipantConnection_connectParticipant,
    createParticipantConnection_type,
    createParticipantConnection_participantToken,
    createParticipantConnectionResponse_connectionCredentials,
    createParticipantConnectionResponse_websocket,
    createParticipantConnectionResponse_httpStatus,

    -- ** DisconnectParticipant
    disconnectParticipant_clientToken,
    disconnectParticipant_connectionToken,
    disconnectParticipantResponse_httpStatus,

    -- ** GetAttachment
    getAttachment_attachmentId,
    getAttachment_connectionToken,
    getAttachmentResponse_url,
    getAttachmentResponse_urlExpiry,
    getAttachmentResponse_httpStatus,

    -- ** GetTranscript
    getTranscript_contactId,
    getTranscript_maxResults,
    getTranscript_nextToken,
    getTranscript_scanDirection,
    getTranscript_sortOrder,
    getTranscript_startPosition,
    getTranscript_connectionToken,
    getTranscriptResponse_initialContactId,
    getTranscriptResponse_nextToken,
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

    -- ** SendMessage
    sendMessage_clientToken,
    sendMessage_contentType,
    sendMessage_content,
    sendMessage_connectionToken,
    sendMessageResponse_absoluteTime,
    sendMessageResponse_id,
    sendMessageResponse_httpStatus,

    -- ** StartAttachmentUpload
    startAttachmentUpload_contentType,
    startAttachmentUpload_attachmentSizeInBytes,
    startAttachmentUpload_attachmentName,
    startAttachmentUpload_clientToken,
    startAttachmentUpload_connectionToken,
    startAttachmentUploadResponse_attachmentId,
    startAttachmentUploadResponse_uploadMetadata,
    startAttachmentUploadResponse_httpStatus,

    -- * Types

    -- ** AttachmentItem
    attachmentItem_attachmentId,
    attachmentItem_attachmentName,
    attachmentItem_contentType,
    attachmentItem_status,

    -- ** ConnectionCredentials
    connectionCredentials_connectionToken,
    connectionCredentials_expiry,

    -- ** Item
    item_absoluteTime,
    item_attachments,
    item_content,
    item_contentType,
    item_displayName,
    item_id,
    item_messageMetadata,
    item_participantId,
    item_participantRole,
    item_type,

    -- ** MessageMetadata
    messageMetadata_messageId,
    messageMetadata_receipts,

    -- ** Receipt
    receipt_deliveredTimestamp,
    receipt_readTimestamp,
    receipt_recipientParticipantId,

    -- ** StartPosition
    startPosition_absoluteTime,
    startPosition_id,
    startPosition_mostRecent,

    -- ** UploadMetadata
    uploadMetadata_headersToInclude,
    uploadMetadata_url,
    uploadMetadata_urlExpiry,

    -- ** Websocket
    websocket_connectionExpiry,
    websocket_url,
  )
where

import Amazonka.ConnectParticipant.CompleteAttachmentUpload
import Amazonka.ConnectParticipant.CreateParticipantConnection
import Amazonka.ConnectParticipant.DisconnectParticipant
import Amazonka.ConnectParticipant.GetAttachment
import Amazonka.ConnectParticipant.GetTranscript
import Amazonka.ConnectParticipant.SendEvent
import Amazonka.ConnectParticipant.SendMessage
import Amazonka.ConnectParticipant.StartAttachmentUpload
import Amazonka.ConnectParticipant.Types.AttachmentItem
import Amazonka.ConnectParticipant.Types.ConnectionCredentials
import Amazonka.ConnectParticipant.Types.Item
import Amazonka.ConnectParticipant.Types.MessageMetadata
import Amazonka.ConnectParticipant.Types.Receipt
import Amazonka.ConnectParticipant.Types.StartPosition
import Amazonka.ConnectParticipant.Types.UploadMetadata
import Amazonka.ConnectParticipant.Types.Websocket
