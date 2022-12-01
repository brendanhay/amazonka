{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SQS.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SQS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _QueueDoesNotExist,
    _InvalidIdFormat,
    _BatchRequestTooLong,
    _ReceiptHandleIsInvalid,
    _MessageNotInflight,
    _TooManyEntriesInBatchRequest,
    _PurgeQueueInProgress,
    _EmptyBatchRequest,
    _InvalidBatchEntryId,
    _InvalidMessageContents,
    _InvalidAttributeName,
    _QueueDeletedRecently,
    _QueueNameExists,
    _UnsupportedOperation,
    _OverLimit,
    _BatchEntryIdsNotDistinct,

    -- * MessageAttribute
    MessageAttribute (..),

    -- * MessageSystemAttributeNameForSends
    MessageSystemAttributeNameForSends (..),

    -- * QueueAttributeName
    QueueAttributeName (..),

    -- * BatchResultErrorEntry
    BatchResultErrorEntry (..),
    newBatchResultErrorEntry,
    batchResultErrorEntry_message,
    batchResultErrorEntry_id,
    batchResultErrorEntry_senderFault,
    batchResultErrorEntry_code,

    -- * ChangeMessageVisibilityBatchRequestEntry
    ChangeMessageVisibilityBatchRequestEntry (..),
    newChangeMessageVisibilityBatchRequestEntry,
    changeMessageVisibilityBatchRequestEntry_visibilityTimeout,
    changeMessageVisibilityBatchRequestEntry_id,
    changeMessageVisibilityBatchRequestEntry_receiptHandle,

    -- * ChangeMessageVisibilityBatchResultEntry
    ChangeMessageVisibilityBatchResultEntry (..),
    newChangeMessageVisibilityBatchResultEntry,
    changeMessageVisibilityBatchResultEntry_id,

    -- * DeleteMessageBatchRequestEntry
    DeleteMessageBatchRequestEntry (..),
    newDeleteMessageBatchRequestEntry,
    deleteMessageBatchRequestEntry_id,
    deleteMessageBatchRequestEntry_receiptHandle,

    -- * DeleteMessageBatchResultEntry
    DeleteMessageBatchResultEntry (..),
    newDeleteMessageBatchResultEntry,
    deleteMessageBatchResultEntry_id,

    -- * Message
    Message (..),
    newMessage,
    message_receiptHandle,
    message_body,
    message_messageId,
    message_messageAttributes,
    message_attributes,
    message_mD5OfBody,
    message_mD5OfMessageAttributes,

    -- * MessageAttributeValue
    MessageAttributeValue (..),
    newMessageAttributeValue,
    messageAttributeValue_stringListValues,
    messageAttributeValue_binaryValue,
    messageAttributeValue_binaryListValues,
    messageAttributeValue_stringValue,
    messageAttributeValue_dataType,

    -- * MessageSystemAttributeValue
    MessageSystemAttributeValue (..),
    newMessageSystemAttributeValue,
    messageSystemAttributeValue_stringListValues,
    messageSystemAttributeValue_binaryValue,
    messageSystemAttributeValue_binaryListValues,
    messageSystemAttributeValue_stringValue,
    messageSystemAttributeValue_dataType,

    -- * SendMessageBatchRequestEntry
    SendMessageBatchRequestEntry (..),
    newSendMessageBatchRequestEntry,
    sendMessageBatchRequestEntry_messageDeduplicationId,
    sendMessageBatchRequestEntry_messageGroupId,
    sendMessageBatchRequestEntry_delaySeconds,
    sendMessageBatchRequestEntry_messageAttributes,
    sendMessageBatchRequestEntry_messageSystemAttributes,
    sendMessageBatchRequestEntry_id,
    sendMessageBatchRequestEntry_messageBody,

    -- * SendMessageBatchResultEntry
    SendMessageBatchResultEntry (..),
    newSendMessageBatchResultEntry,
    sendMessageBatchResultEntry_mD5OfMessageSystemAttributes,
    sendMessageBatchResultEntry_sequenceNumber,
    sendMessageBatchResultEntry_mD5OfMessageAttributes,
    sendMessageBatchResultEntry_id,
    sendMessageBatchResultEntry_messageId,
    sendMessageBatchResultEntry_mD5OfMessageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SQS.Types.BatchResultErrorEntry
import Amazonka.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
import Amazonka.SQS.Types.ChangeMessageVisibilityBatchResultEntry
import Amazonka.SQS.Types.DeleteMessageBatchRequestEntry
import Amazonka.SQS.Types.DeleteMessageBatchResultEntry
import Amazonka.SQS.Types.Message
import Amazonka.SQS.Types.MessageAttribute
import Amazonka.SQS.Types.MessageAttributeValue
import Amazonka.SQS.Types.MessageSystemAttributeNameForSends
import Amazonka.SQS.Types.MessageSystemAttributeValue
import Amazonka.SQS.Types.QueueAttributeName
import Amazonka.SQS.Types.SendMessageBatchRequestEntry
import Amazonka.SQS.Types.SendMessageBatchResultEntry
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2012-11-05@ of the Amazon Simple Queue Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SQS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "sqs",
      Core.signingName = "sqs",
      Core.version = "2012-11-05",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "SQS",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "RequestThrottled"
              Prelude.. Core.hasStatus 403
          )
          e =
        Prelude.Just "request_limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified queue doesn\'t exist.
_QueueDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_QueueDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.NonExistentQueue"
    Prelude.. Core.hasStatus 400

-- | The specified receipt handle isn\'t valid for the current version.
_InvalidIdFormat :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidIdFormat =
  Core._MatchServiceError
    defaultService
    "InvalidIdFormat"

-- | The length of all the messages put together is more than the limit.
_BatchRequestTooLong :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchRequestTooLong =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.BatchRequestTooLong"
    Prelude.. Core.hasStatus 400

-- | The specified receipt handle isn\'t valid.
_ReceiptHandleIsInvalid :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReceiptHandleIsInvalid =
  Core._MatchServiceError
    defaultService
    "ReceiptHandleIsInvalid"

-- | The specified message isn\'t in flight.
_MessageNotInflight :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MessageNotInflight =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.MessageNotInflight"
    Prelude.. Core.hasStatus 400

-- | The batch request contains more entries than permissible.
_TooManyEntriesInBatchRequest :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyEntriesInBatchRequest =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.TooManyEntriesInBatchRequest"
    Prelude.. Core.hasStatus 400

-- | Indicates that the specified queue previously received a @PurgeQueue@
-- request within the last 60 seconds (the time it can take to delete the
-- messages in the queue).
_PurgeQueueInProgress :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PurgeQueueInProgress =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.PurgeQueueInProgress"
    Prelude.. Core.hasStatus 403

-- | The batch request doesn\'t contain any entries.
_EmptyBatchRequest :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EmptyBatchRequest =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.EmptyBatchRequest"
    Prelude.. Core.hasStatus 400

-- | The @Id@ of a batch entry in a batch request doesn\'t abide by the
-- specification.
_InvalidBatchEntryId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidBatchEntryId =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.InvalidBatchEntryId"
    Prelude.. Core.hasStatus 400

-- | The message contains characters outside the allowed set.
_InvalidMessageContents :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMessageContents =
  Core._MatchServiceError
    defaultService
    "InvalidMessageContents"

-- | The specified attribute doesn\'t exist.
_InvalidAttributeName :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAttributeName =
  Core._MatchServiceError
    defaultService
    "InvalidAttributeName"

-- | You must wait 60 seconds after deleting a queue before you can create
-- another queue with the same name.
_QueueDeletedRecently :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_QueueDeletedRecently =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.QueueDeletedRecently"
    Prelude.. Core.hasStatus 400

-- | A queue with this name already exists. Amazon SQS returns this error
-- only if the request includes attributes whose values differ from those
-- of the existing queue.
_QueueNameExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_QueueNameExists =
  Core._MatchServiceError
    defaultService
    "QueueAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Error code 400. Unsupported operation.
_UnsupportedOperation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperation =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.UnsupportedOperation"
    Prelude.. Core.hasStatus 400

-- | The specified action violates a limit. For example, @ReceiveMessage@
-- returns this error if the maximum number of inflight messages is reached
-- and @AddPermission@ returns this error if the maximum number of
-- permissions for the queue is reached.
_OverLimit :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OverLimit =
  Core._MatchServiceError defaultService "OverLimit"
    Prelude.. Core.hasStatus 403

-- | Two or more batch entries in the request have the same @Id@.
_BatchEntryIdsNotDistinct :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchEntryIdsNotDistinct =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.BatchEntryIdsNotDistinct"
    Prelude.. Core.hasStatus 400
