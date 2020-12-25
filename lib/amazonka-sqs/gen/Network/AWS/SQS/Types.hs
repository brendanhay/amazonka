-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidBatchEntryId,
    _TooManyEntriesInBatchRequest,
    _QueueDeletedRecently,
    _QueueDoesNotExist,
    _InvalidAttributeName,
    _UnsupportedOperation,
    _InvalidMessageContents,
    _BatchRequestTooLong,
    _OverLimit,
    _QueueNameExists,
    _PurgeQueueInProgress,
    _InvalidIdFormat,
    _ReceiptHandleIsInvalid,
    _EmptyBatchRequest,
    _BatchEntryIdsNotDistinct,
    _MessageNotInflight,

    -- * MessageAttribute
    MessageAttribute (..),

    -- * QueueAttributeName
    QueueAttributeName (..),

    -- * String
    String (..),

    -- * Token
    Token (..),

    -- * DeleteMessageBatchRequestEntry
    DeleteMessageBatchRequestEntry (..),
    mkDeleteMessageBatchRequestEntry,
    dmbreId,
    dmbreReceiptHandle,

    -- * TagValue
    TagValue (..),

    -- * MessageSystemAttributeNameForSends
    MessageSystemAttributeNameForSends (..),

    -- * MessageAttributeValue
    MessageAttributeValue (..),
    mkMessageAttributeValue,
    mavDataType,
    mavBinaryListValues,
    mavBinaryValue,
    mavStringListValues,
    mavStringValue,

    -- * ChangeMessageVisibilityBatchResultEntry
    ChangeMessageVisibilityBatchResultEntry (..),
    mkChangeMessageVisibilityBatchResultEntry,
    cmvbreId,

    -- * ChangeMessageVisibilityBatchRequestEntry
    ChangeMessageVisibilityBatchRequestEntry (..),
    mkChangeMessageVisibilityBatchRequestEntry,
    cId,
    cReceiptHandle,
    cVisibilityTimeout,

    -- * TagKey
    TagKey (..),

    -- * MessageAttributeName
    MessageAttributeName (..),

    -- * DeleteMessageBatchResultEntry
    DeleteMessageBatchResultEntry (..),
    mkDeleteMessageBatchResultEntry,
    dId,

    -- * Message
    Message (..),
    mkMessage,
    mAttributes,
    mBody,
    mMD5OfBody,
    mMD5OfMessageAttributes,
    mMessageAttributes,
    mMessageId,
    mReceiptHandle,

    -- * SendMessageBatchRequestEntry
    SendMessageBatchRequestEntry (..),
    mkSendMessageBatchRequestEntry,
    sId,
    sMessageBody,
    sDelaySeconds,
    sMessageAttributes,
    sMessageDeduplicationId,
    sMessageGroupId,
    sMessageSystemAttributes,

    -- * SendMessageBatchResultEntry
    SendMessageBatchResultEntry (..),
    mkSendMessageBatchResultEntry,
    smbreId,
    smbreMessageId,
    smbreMD5OfMessageBody,
    smbreMD5OfMessageAttributes,
    smbreMD5OfMessageSystemAttributes,
    smbreSequenceNumber,

    -- * BatchResultErrorEntry
    BatchResultErrorEntry (..),
    mkBatchResultErrorEntry,
    breeId,
    breeSenderFault,
    breeCode,
    breeMessage,

    -- * MessageSystemAttributeValue
    MessageSystemAttributeValue (..),
    mkMessageSystemAttributeValue,
    msavDataType,
    msavBinaryListValues,
    msavBinaryValue,
    msavStringListValues,
    msavStringValue,

    -- * QueueUrl
    QueueUrl (..),

    -- * Label
    Label (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.SQS.Types.BatchResultErrorEntry
import Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
import Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry
import Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry
import Network.AWS.SQS.Types.DeleteMessageBatchResultEntry
import Network.AWS.SQS.Types.Label
import Network.AWS.SQS.Types.Message
import Network.AWS.SQS.Types.MessageAttribute
import Network.AWS.SQS.Types.MessageAttributeName
import Network.AWS.SQS.Types.MessageAttributeValue
import Network.AWS.SQS.Types.MessageSystemAttributeNameForSends
import Network.AWS.SQS.Types.MessageSystemAttributeValue
import Network.AWS.SQS.Types.QueueAttributeName
import Network.AWS.SQS.Types.QueueUrl
import Network.AWS.SQS.Types.SendMessageBatchRequestEntry
import Network.AWS.SQS.Types.SendMessageBatchResultEntry
import Network.AWS.SQS.Types.String
import Network.AWS.SQS.Types.TagKey
import Network.AWS.SQS.Types.TagValue
import Network.AWS.SQS.Types.Token
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-11-05@ of the Amazon Simple Queue Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "SQS",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "sqs",
      Core._svcVersion = "2012-11-05",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseXMLError "SQS",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Lens.has
          (Core.hasCode "RequestThrottled" Core.. Core.hasStatus 403)
          e =
        Core.Just "request_limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The @Id@ of a batch entry in a batch request doesn't abide by the specification.
_InvalidBatchEntryId :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidBatchEntryId =
  Core._MatchServiceError
    mkServiceConfig
    "AWS.SimpleQueueService.InvalidBatchEntryId"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidBatchEntryId "Use generic-lens or generic-optics instead." #-}

-- | The batch request contains more entries than permissible.
_TooManyEntriesInBatchRequest :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyEntriesInBatchRequest =
  Core._MatchServiceError
    mkServiceConfig
    "AWS.SimpleQueueService.TooManyEntriesInBatchRequest"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TooManyEntriesInBatchRequest "Use generic-lens or generic-optics instead." #-}

-- | You must wait 60 seconds after deleting a queue before you can create another queue with the same name.
_QueueDeletedRecently :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_QueueDeletedRecently =
  Core._MatchServiceError
    mkServiceConfig
    "AWS.SimpleQueueService.QueueDeletedRecently"
    Core.. Core.hasStatues 400
{-# DEPRECATED _QueueDeletedRecently "Use generic-lens or generic-optics instead." #-}

-- | The specified queue doesn't exist.
_QueueDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_QueueDoesNotExist =
  Core._MatchServiceError
    mkServiceConfig
    "AWS.SimpleQueueService.NonExistentQueue"
    Core.. Core.hasStatues 400
{-# DEPRECATED _QueueDoesNotExist "Use generic-lens or generic-optics instead." #-}

-- | The specified attribute doesn't exist.
_InvalidAttributeName :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAttributeName =
  Core._MatchServiceError mkServiceConfig "InvalidAttributeName"
{-# DEPRECATED _InvalidAttributeName "Use generic-lens or generic-optics instead." #-}

-- | Error code 400. Unsupported operation.
_UnsupportedOperation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperation =
  Core._MatchServiceError
    mkServiceConfig
    "AWS.SimpleQueueService.UnsupportedOperation"
    Core.. Core.hasStatues 400
{-# DEPRECATED _UnsupportedOperation "Use generic-lens or generic-optics instead." #-}

-- | The message contains characters outside the allowed set.
_InvalidMessageContents :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMessageContents =
  Core._MatchServiceError mkServiceConfig "InvalidMessageContents"
{-# DEPRECATED _InvalidMessageContents "Use generic-lens or generic-optics instead." #-}

-- | The length of all the messages put together is more than the limit.
_BatchRequestTooLong :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BatchRequestTooLong =
  Core._MatchServiceError
    mkServiceConfig
    "AWS.SimpleQueueService.BatchRequestTooLong"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BatchRequestTooLong "Use generic-lens or generic-optics instead." #-}

-- | The specified action violates a limit. For example, @ReceiveMessage@ returns this error if the maximum number of inflight messages is reached and @AddPermission@ returns this error if the maximum number of permissions for the queue is reached.
_OverLimit :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OverLimit =
  Core._MatchServiceError mkServiceConfig "OverLimit"
    Core.. Core.hasStatues 403
{-# DEPRECATED _OverLimit "Use generic-lens or generic-optics instead." #-}

-- | A queue with this name already exists. Amazon SQS returns this error only if the request includes attributes whose values differ from those of the existing queue.
_QueueNameExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_QueueNameExists =
  Core._MatchServiceError mkServiceConfig "QueueAlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _QueueNameExists "Use generic-lens or generic-optics instead." #-}

-- | Indicates that the specified queue previously received a @PurgeQueue@ request within the last 60 seconds (the time it can take to delete the messages in the queue).
_PurgeQueueInProgress :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PurgeQueueInProgress =
  Core._MatchServiceError
    mkServiceConfig
    "AWS.SimpleQueueService.PurgeQueueInProgress"
    Core.. Core.hasStatues 403
{-# DEPRECATED _PurgeQueueInProgress "Use generic-lens or generic-optics instead." #-}

-- | The specified receipt handle isn't valid for the current version.
_InvalidIdFormat :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidIdFormat =
  Core._MatchServiceError mkServiceConfig "InvalidIdFormat"
{-# DEPRECATED _InvalidIdFormat "Use generic-lens or generic-optics instead." #-}

-- | The specified receipt handle isn't valid.
_ReceiptHandleIsInvalid :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReceiptHandleIsInvalid =
  Core._MatchServiceError mkServiceConfig "ReceiptHandleIsInvalid"
{-# DEPRECATED _ReceiptHandleIsInvalid "Use generic-lens or generic-optics instead." #-}

-- | The batch request doesn't contain any entries.
_EmptyBatchRequest :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EmptyBatchRequest =
  Core._MatchServiceError
    mkServiceConfig
    "AWS.SimpleQueueService.EmptyBatchRequest"
    Core.. Core.hasStatues 400
{-# DEPRECATED _EmptyBatchRequest "Use generic-lens or generic-optics instead." #-}

-- | Two or more batch entries in the request have the same @Id@ .
_BatchEntryIdsNotDistinct :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BatchEntryIdsNotDistinct =
  Core._MatchServiceError
    mkServiceConfig
    "AWS.SimpleQueueService.BatchEntryIdsNotDistinct"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BatchEntryIdsNotDistinct "Use generic-lens or generic-optics instead." #-}

-- | The specified message isn't in flight.
_MessageNotInflight :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MessageNotInflight =
  Core._MatchServiceError
    mkServiceConfig
    "AWS.SimpleQueueService.MessageNotInflight"
    Core.. Core.hasStatues 400
{-# DEPRECATED _MessageNotInflight "Use generic-lens or generic-optics instead." #-}
