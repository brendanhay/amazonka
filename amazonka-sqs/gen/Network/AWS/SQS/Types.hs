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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "SQS",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "sqs",
      Prelude._svcVersion = "2012-11-05",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseXMLError "SQS",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has
          ( Prelude.hasCode "RequestThrottled"
              Prelude.. Prelude.hasStatus 403
          )
          e =
        Prelude.Just "request_limit_exceeded"
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified receipt handle isn\'t valid for the current version.
_InvalidIdFormat :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidIdFormat =
  Prelude._MatchServiceError
    defaultService
    "InvalidIdFormat"

-- | The batch request contains more entries than permissible.
_TooManyEntriesInBatchRequest :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyEntriesInBatchRequest =
  Prelude._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.TooManyEntriesInBatchRequest"
    Prelude.. Prelude.hasStatus 400

-- | A queue with this name already exists. Amazon SQS returns this error
-- only if the request includes attributes whose values differ from those
-- of the existing queue.
_QueueNameExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_QueueNameExists =
  Prelude._MatchServiceError
    defaultService
    "QueueAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | The batch request doesn\'t contain any entries.
_EmptyBatchRequest :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EmptyBatchRequest =
  Prelude._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.EmptyBatchRequest"
    Prelude.. Prelude.hasStatus 400

-- | The message contains characters outside the allowed set.
_InvalidMessageContents :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidMessageContents =
  Prelude._MatchServiceError
    defaultService
    "InvalidMessageContents"

-- | Error code 400. Unsupported operation.
_UnsupportedOperation :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedOperation =
  Prelude._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.UnsupportedOperation"
    Prelude.. Prelude.hasStatus 400

-- | The specified receipt handle isn\'t valid.
_ReceiptHandleIsInvalid :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReceiptHandleIsInvalid =
  Prelude._MatchServiceError
    defaultService
    "ReceiptHandleIsInvalid"

-- | The specified attribute doesn\'t exist.
_InvalidAttributeName :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAttributeName =
  Prelude._MatchServiceError
    defaultService
    "InvalidAttributeName"

-- | You must wait 60 seconds after deleting a queue before you can create
-- another queue with the same name.
_QueueDeletedRecently :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_QueueDeletedRecently =
  Prelude._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.QueueDeletedRecently"
    Prelude.. Prelude.hasStatus 400

-- | The @Id@ of a batch entry in a batch request doesn\'t abide by the
-- specification.
_InvalidBatchEntryId :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidBatchEntryId =
  Prelude._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.InvalidBatchEntryId"
    Prelude.. Prelude.hasStatus 400

-- | Two or more batch entries in the request have the same @Id@.
_BatchEntryIdsNotDistinct :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BatchEntryIdsNotDistinct =
  Prelude._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.BatchEntryIdsNotDistinct"
    Prelude.. Prelude.hasStatus 400

-- | The specified message isn\'t in flight.
_MessageNotInflight :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MessageNotInflight =
  Prelude._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.MessageNotInflight"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the specified queue previously received a @PurgeQueue@
-- request within the last 60 seconds (the time it can take to delete the
-- messages in the queue).
_PurgeQueueInProgress :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PurgeQueueInProgress =
  Prelude._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.PurgeQueueInProgress"
    Prelude.. Prelude.hasStatus 403

-- | The length of all the messages put together is more than the limit.
_BatchRequestTooLong :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BatchRequestTooLong =
  Prelude._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.BatchRequestTooLong"
    Prelude.. Prelude.hasStatus 400

-- | The specified action violates a limit. For example, @ReceiveMessage@
-- returns this error if the maximum number of inflight messages is reached
-- and @AddPermission@ returns this error if the maximum number of
-- permissions for the queue is reached.
_OverLimit :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OverLimit =
  Prelude._MatchServiceError
    defaultService
    "OverLimit"
    Prelude.. Prelude.hasStatus 403

-- | The specified queue doesn\'t exist.
_QueueDoesNotExist :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_QueueDoesNotExist =
  Prelude._MatchServiceError
    defaultService
    "AWS.SimpleQueueService.NonExistentQueue"
    Prelude.. Prelude.hasStatus 400
