{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types
    (
    -- * Service Configuration
      sQS

    -- * Errors
    , _InvalidBatchEntryId
    , _TooManyEntriesInBatchRequest
    , _QueueDeletedRecently
    , _QueueDoesNotExist
    , _InvalidAttributeName
    , _UnsupportedOperation
    , _InvalidMessageContents
    , _BatchRequestTooLong
    , _OverLimit
    , _QueueNameExists
    , _PurgeQueueInProgress
    , _InvalidIdFormat
    , _ReceiptHandleIsInvalid
    , _EmptyBatchRequest
    , _BatchEntryIdsNotDistinct
    , _MessageNotInflight

    -- * MessageAttribute
    , MessageAttribute (..)

    -- * QueueAttributeName
    , QueueAttributeName (..)

    -- * BatchResultErrorEntry
    , BatchResultErrorEntry
    , batchResultErrorEntry
    , breeMessage
    , breeId
    , breeSenderFault
    , breeCode

    -- * ChangeMessageVisibilityBatchRequestEntry
    , ChangeMessageVisibilityBatchRequestEntry
    , changeMessageVisibilityBatchRequestEntry
    , cVisibilityTimeout
    , cId
    , cReceiptHandle

    -- * ChangeMessageVisibilityBatchResultEntry
    , ChangeMessageVisibilityBatchResultEntry
    , changeMessageVisibilityBatchResultEntry
    , cmvbreId

    -- * DeleteMessageBatchRequestEntry
    , DeleteMessageBatchRequestEntry
    , deleteMessageBatchRequestEntry
    , dmbreId
    , dmbreReceiptHandle

    -- * DeleteMessageBatchResultEntry
    , DeleteMessageBatchResultEntry
    , deleteMessageBatchResultEntry
    , dId

    -- * Message
    , Message
    , message
    , mMessageAttributes
    , mMD5OfBody
    , mBody
    , mAttributes
    , mReceiptHandle
    , mMessageId
    , mMD5OfMessageAttributes

    -- * MessageAttributeValue
    , MessageAttributeValue
    , messageAttributeValue
    , mavBinaryValue
    , mavStringListValues
    , mavStringValue
    , mavBinaryListValues
    , mavDataType

    -- * SendMessageBatchRequestEntry
    , SendMessageBatchRequestEntry
    , sendMessageBatchRequestEntry
    , sMessageAttributes
    , sDelaySeconds
    , sId
    , sMessageBody

    -- * SendMessageBatchResultEntry
    , SendMessageBatchResultEntry
    , sendMessageBatchResultEntry
    , smbreMD5OfMessageAttributes
    , smbreId
    , smbreMessageId
    , smbreMD5OfMessageBody
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.SQS.Types.Product
import           Network.AWS.SQS.Types.Sum

-- | API version '2012-11-05' of the Amazon Simple Queue Service SDK configuration.
sQS :: Service
sQS =
    Service
    { _svcAbbrev = "SQS"
    , _svcSigner = v4
    , _svcPrefix = "sqs"
    , _svcVersion = "2012-11-05"
    , _svcEndpoint = defaultEndpoint sQS
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "RequestThrottled" . hasStatus 403) e =
          Just "request_limit_exceeded"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The 'Id' of a batch entry in a batch request does not abide by the
-- specification.
_InvalidBatchEntryId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBatchEntryId =
    _ServiceError .
    hasStatus 400 . hasCode "AWS.SimpleQueueService.InvalidBatchEntryId"

-- | Batch request contains more number of entries than permissible.
_TooManyEntriesInBatchRequest :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyEntriesInBatchRequest =
    _ServiceError .
    hasStatus 400 .
    hasCode "AWS.SimpleQueueService.TooManyEntriesInBatchRequest"

-- | You must wait 60 seconds after deleting a queue before you can create
-- another with the same name.
_QueueDeletedRecently :: AsError a => Getting (First ServiceError) a ServiceError
_QueueDeletedRecently =
    _ServiceError .
    hasStatus 400 . hasCode "AWS.SimpleQueueService.QueueDeletedRecently"

-- | The queue referred to does not exist.
_QueueDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_QueueDoesNotExist =
    _ServiceError .
    hasStatus 400 . hasCode "AWS.SimpleQueueService.NonExistentQueue"

-- | The attribute referred to does not exist.
_InvalidAttributeName :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAttributeName = _ServiceError . hasCode "InvalidAttributeName"

-- | Error code 400. Unsupported operation.
_UnsupportedOperation :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperation =
    _ServiceError .
    hasStatus 400 . hasCode "AWS.SimpleQueueService.UnsupportedOperation"

-- | The message contains characters outside the allowed set.
_InvalidMessageContents :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMessageContents = _ServiceError . hasCode "InvalidMessageContents"

-- | The length of all the messages put together is more than the limit.
_BatchRequestTooLong :: AsError a => Getting (First ServiceError) a ServiceError
_BatchRequestTooLong =
    _ServiceError .
    hasStatus 400 . hasCode "AWS.SimpleQueueService.BatchRequestTooLong"

-- | The action that you requested would violate a limit. For example,
-- ReceiveMessage returns this error if the maximum number of messages
-- inflight has already been reached. AddPermission returns this error if
-- the maximum number of permissions for the queue has already been
-- reached.
_OverLimit :: AsError a => Getting (First ServiceError) a ServiceError
_OverLimit = _ServiceError . hasStatus 403 . hasCode "OverLimit"

-- | A queue already exists with this name. Amazon SQS returns this error
-- only if the request includes attributes whose values differ from those
-- of the existing queue.
_QueueNameExists :: AsError a => Getting (First ServiceError) a ServiceError
_QueueNameExists = _ServiceError . hasStatus 400 . hasCode "QueueAlreadyExists"

-- | Indicates that the specified queue previously received a 'PurgeQueue'
-- request within the last 60 seconds, the time it can take to delete the
-- messages in the queue.
_PurgeQueueInProgress :: AsError a => Getting (First ServiceError) a ServiceError
_PurgeQueueInProgress =
    _ServiceError .
    hasStatus 403 . hasCode "AWS.SimpleQueueService.PurgeQueueInProgress"

-- | The receipt handle is not valid for the current version.
_InvalidIdFormat :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIdFormat = _ServiceError . hasCode "InvalidIdFormat"

-- | The receipt handle provided is not valid.
_ReceiptHandleIsInvalid :: AsError a => Getting (First ServiceError) a ServiceError
_ReceiptHandleIsInvalid = _ServiceError . hasCode "ReceiptHandleIsInvalid"

-- | Batch request does not contain an entry.
_EmptyBatchRequest :: AsError a => Getting (First ServiceError) a ServiceError
_EmptyBatchRequest =
    _ServiceError .
    hasStatus 400 . hasCode "AWS.SimpleQueueService.EmptyBatchRequest"

-- | Two or more batch entries have the same 'Id' in the request.
_BatchEntryIdsNotDistinct :: AsError a => Getting (First ServiceError) a ServiceError
_BatchEntryIdsNotDistinct =
    _ServiceError .
    hasStatus 400 . hasCode "AWS.SimpleQueueService.BatchEntryIdsNotDistinct"

-- | The message referred to is not in flight.
_MessageNotInflight :: AsError a => Getting (First ServiceError) a ServiceError
_MessageNotInflight =
    _ServiceError .
    hasStatus 400 . hasCode "AWS.SimpleQueueService.MessageNotInflight"
