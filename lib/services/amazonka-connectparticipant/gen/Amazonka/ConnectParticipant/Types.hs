{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectParticipant.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * ArtifactStatus
    ArtifactStatus (..),

    -- * ChatItemType
    ChatItemType (..),

    -- * ConnectionType
    ConnectionType (..),

    -- * ParticipantRole
    ParticipantRole (..),

    -- * ScanDirection
    ScanDirection (..),

    -- * SortKey
    SortKey (..),

    -- * AttachmentItem
    AttachmentItem (..),
    newAttachmentItem,
    attachmentItem_attachmentId,
    attachmentItem_attachmentName,
    attachmentItem_contentType,
    attachmentItem_status,

    -- * ConnectionCredentials
    ConnectionCredentials (..),
    newConnectionCredentials,
    connectionCredentials_connectionToken,
    connectionCredentials_expiry,

    -- * Item
    Item (..),
    newItem,
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

    -- * MessageMetadata
    MessageMetadata (..),
    newMessageMetadata,
    messageMetadata_messageId,
    messageMetadata_receipts,

    -- * Receipt
    Receipt (..),
    newReceipt,
    receipt_deliveredTimestamp,
    receipt_readTimestamp,
    receipt_recipientParticipantId,

    -- * StartPosition
    StartPosition (..),
    newStartPosition,
    startPosition_absoluteTime,
    startPosition_id,
    startPosition_mostRecent,

    -- * UploadMetadata
    UploadMetadata (..),
    newUploadMetadata,
    uploadMetadata_headersToInclude,
    uploadMetadata_url,
    uploadMetadata_urlExpiry,

    -- * Websocket
    Websocket (..),
    newWebsocket,
    websocket_connectionExpiry,
    websocket_url,
  )
where

import Amazonka.ConnectParticipant.Types.ArtifactStatus
import Amazonka.ConnectParticipant.Types.AttachmentItem
import Amazonka.ConnectParticipant.Types.ChatItemType
import Amazonka.ConnectParticipant.Types.ConnectionCredentials
import Amazonka.ConnectParticipant.Types.ConnectionType
import Amazonka.ConnectParticipant.Types.Item
import Amazonka.ConnectParticipant.Types.MessageMetadata
import Amazonka.ConnectParticipant.Types.ParticipantRole
import Amazonka.ConnectParticipant.Types.Receipt
import Amazonka.ConnectParticipant.Types.ScanDirection
import Amazonka.ConnectParticipant.Types.SortKey
import Amazonka.ConnectParticipant.Types.StartPosition
import Amazonka.ConnectParticipant.Types.UploadMetadata
import Amazonka.ConnectParticipant.Types.Websocket
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-09-07@ of the Amazon Connect Participant Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ConnectParticipant",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "participant.connect",
      Core.signingName = "execute-api",
      Core.version = "2018-09-07",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "ConnectParticipant",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An attachment with that identifier is already being uploaded.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | This exception occurs when there is an internal failure in the Amazon
-- Connect service.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The number of attachments per contact exceeds the quota.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by Amazon Connect.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
