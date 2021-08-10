{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidIdFormat,
    _TooManyEntriesInBatchRequest,
    _QueueNameExists,
    _EmptyBatchRequest,
    _InvalidMessageContents,
    _UnsupportedOperation,
    _ReceiptHandleIsInvalid,
    _InvalidAttributeName,
    _QueueDeletedRecently,
    _InvalidBatchEntryId,
    _BatchEntryIdsNotDistinct,
    _MessageNotInflight,
    _PurgeQueueInProgress,
    _BatchRequestTooLong,
    _OverLimit,
    _QueueDoesNotExist,

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
    message_body,
    message_mD5OfBody,
    message_attributes,
    message_messageAttributes,
    message_mD5OfMessageAttributes,
    message_receiptHandle,
    message_messageId,

    -- * MessageAttributeValue
    MessageAttributeValue (..),
    newMessageAttributeValue,
    messageAttributeValue_stringListValues,
    messageAttributeValue_stringValue,
    messageAttributeValue_binaryListValues,
    messageAttributeValue_binaryValue,
    messageAttributeValue_dataType,

    -- * MessageSystemAttributeValue
    MessageSystemAttributeValue (..),
    newMessageSystemAttributeValue,
    messageSystemAttributeValue_stringListValues,
    messageSystemAttributeValue_stringValue,
    messageSystemAttributeValue_binaryListValues,
    messageSystemAttributeValue_binaryValue,
    messageSystemAttributeValue_dataType,

    -- * SendMessageBatchRequestEntry
    SendMessageBatchRequestEntry (..),
    newSendMessageBatchRequestEntry,
    sendMessageBatchRequestEntry_messageDeduplicationId,
    sendMessageBatchRequestEntry_messageAttributes,
    sendMessageBatchRequestEntry_messageSystemAttributes,
    sendMessageBatchRequestEntry_messageGroupId,
    sendMessageBatchRequestEntry_delaySeconds,
    sendMessageBatchRequestEntry_id,
    sendMessageBatchRequestEntry_messageBody,

    -- * SendMessageBatchResultEntry
    SendMessageBatchResultEntry (..),
    newSendMessageBatchResultEntry,
    sendMessageBatchResultEntry_sequenceNumber,
    sendMessageBatchResultEntry_mD5OfMessageSystemAttributes,
    sendMessageBatchResultEntry_mD5OfMessageAttributes,
    sendMessageBatchResultEntry_id,
    sendMessageBatchResultEntry_messageId,
    sendMessageBatchResultEntry_mD5OfMessageBody,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SQS.Types.BatchResultErrorEntry
import Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
import Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry
import Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry
import Network.AWS.SQS.Types.DeleteMessageBatchResultEntry
import Network.AWS.SQS.Types.Message
import Network.AWS.SQS.Types.MessageAttribute
import Network.AWS.SQS.Types.MessageAttributeValue
import Network.AWS.SQS.Types.MessageSystemAttributeNameForSends
import Network.AWS.SQS.Types.MessageSystemAttributeValue
import Network.AWS.SQS.Types.QueueAttributeName
import Network.AWS.SQS.Types.SendMessageBatchRequestEntry
import Network.AWS.SQS.Types.SendMessageBatchResultEntry
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-11-05@ of the Amazon Simple Queue Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SQS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "sqs",
      Core._serviceSigningName = "sqs",
      Core._serviceVersion = "2012-11-05",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "SQS",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "RequestThrottled"
              Prelude.. Core.hasStatus 403
          )
          e =
        Prelude.Just "request_limit_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified receipt handle isn\'t valid for the current version.
_InvalidIdFormat :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidIdFormat =
  Core._MatchServiceError
    defaultService
    "InvalidIdFormat"

-- | The batch request contains more entries than permissible.
_TooManyEntriesInBatchRequest :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyEntriesInBatchRequest =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.TooManyEntriesInBatchRequest"
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

-- | The batch request doesn\'t contain any entries.
_EmptyBatchRequest :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EmptyBatchRequest =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.EmptyBatchRequest"
    Prelude.. Core.hasStatus 400

-- | The message contains characters outside the allowed set.
_InvalidMessageContents :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMessageContents =
  Core._MatchServiceError
    defaultService
    "InvalidMessageContents"

-- | Error code 400. Unsupported operation.
_UnsupportedOperation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperation =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.UnsupportedOperation"
    Prelude.. Core.hasStatus 400

-- | The specified receipt handle isn\'t valid.
_ReceiptHandleIsInvalid :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReceiptHandleIsInvalid =
  Core._MatchServiceError
    defaultService
    "ReceiptHandleIsInvalid"

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

-- | The @Id@ of a batch entry in a batch request doesn\'t abide by the
-- specification.
_InvalidBatchEntryId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidBatchEntryId =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.InvalidBatchEntryId"
    Prelude.. Core.hasStatus 400

-- | Two or more batch entries in the request have the same @Id@.
_BatchEntryIdsNotDistinct :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchEntryIdsNotDistinct =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.BatchEntryIdsNotDistinct"
    Prelude.. Core.hasStatus 400

-- | The specified message isn\'t in flight.
_MessageNotInflight :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MessageNotInflight =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.MessageNotInflight"
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

-- | The length of all the messages put together is more than the limit.
_BatchRequestTooLong :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchRequestTooLong =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.BatchRequestTooLong"
    Prelude.. Core.hasStatus 400

-- | The specified action violates a limit. For example, @ReceiveMessage@
-- returns this error if the maximum number of inflight messages is reached
-- and @AddPermission@ returns this error if the maximum number of
-- permissions for the queue is reached.
_OverLimit :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OverLimit =
  Core._MatchServiceError defaultService "OverLimit"
    Prelude.. Core.hasStatus 403

-- | The specified queue doesn\'t exist.
_QueueDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_QueueDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.NonExistentQueue"
    Prelude.. Core.hasStatus 400
