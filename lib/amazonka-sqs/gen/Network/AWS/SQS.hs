{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Welcome to the /Amazon Simple Queue Service API Reference/ .
--
-- Amazon Simple Queue Service (Amazon SQS) is a reliable, highly-scalable hosted queue for storing messages as they travel between applications or microservices. Amazon SQS moves data between distributed application components and helps you decouple these components.
-- For information on the permissions you need to use this API, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-authentication-and-access-control.html Identity and access management> in the /Amazon Simple Queue Service Developer Guide./
-- You can use <http://aws.amazon.com/tools/#sdk AWS SDKs> to access Amazon SQS using your favorite programming language. The SDKs perform tasks such as the following automatically:
--
--     * Cryptographically sign your service requests
--
--
--     * Retry requests
--
--
--     * Handle error responses
--
--
-- __Additional Information__
--
--     * <http://aws.amazon.com/sqs/ Amazon SQS Product Page>
--
--
--     * /Amazon Simple Queue Service Developer Guide/
--
--     * <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html Making API Requests>
--
--
--     * <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
--
--
--     * <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Amazon SQS Dead-Letter Queues>
--
--
--
--
--     * <http://docs.aws.amazon.com/cli/latest/reference/sqs/index.html Amazon SQS in the /AWS CLI Command Reference/ >
--
--
--     * /Amazon Web Services General Reference/
--
--     * <https://docs.aws.amazon.com/general/latest/gr/rande.html#sqs_region Regions and Endpoints>
module Network.AWS.SQS
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidBatchEntryId
    _InvalidBatchEntryId,

    -- ** TooManyEntriesInBatchRequest
    _TooManyEntriesInBatchRequest,

    -- ** QueueDeletedRecently
    _QueueDeletedRecently,

    -- ** QueueDoesNotExist
    _QueueDoesNotExist,

    -- ** InvalidAttributeName
    _InvalidAttributeName,

    -- ** UnsupportedOperation
    _UnsupportedOperation,

    -- ** InvalidMessageContents
    _InvalidMessageContents,

    -- ** BatchRequestTooLong
    _BatchRequestTooLong,

    -- ** OverLimit
    _OverLimit,

    -- ** QueueNameExists
    _QueueNameExists,

    -- ** PurgeQueueInProgress
    _PurgeQueueInProgress,

    -- ** InvalidIdFormat
    _InvalidIdFormat,

    -- ** ReceiptHandleIsInvalid
    _ReceiptHandleIsInvalid,

    -- ** EmptyBatchRequest
    _EmptyBatchRequest,

    -- ** BatchEntryIdsNotDistinct
    _BatchEntryIdsNotDistinct,

    -- ** MessageNotInflight
    _MessageNotInflight,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetQueueUrl
    module Network.AWS.SQS.GetQueueUrl,

    -- ** PurgeQueue
    module Network.AWS.SQS.PurgeQueue,

    -- ** ChangeMessageVisibilityBatch
    module Network.AWS.SQS.ChangeMessageVisibilityBatch,

    -- ** SendMessage
    module Network.AWS.SQS.SendMessage,

    -- ** RemovePermission
    module Network.AWS.SQS.RemovePermission,

    -- ** GetQueueAttributes
    module Network.AWS.SQS.GetQueueAttributes,

    -- ** ListQueues (Paginated)
    module Network.AWS.SQS.ListQueues,

    -- ** ReceiveMessage
    module Network.AWS.SQS.ReceiveMessage,

    -- ** DeleteQueue
    module Network.AWS.SQS.DeleteQueue,

    -- ** TagQueue
    module Network.AWS.SQS.TagQueue,

    -- ** DeleteMessageBatch
    module Network.AWS.SQS.DeleteMessageBatch,

    -- ** SetQueueAttributes
    module Network.AWS.SQS.SetQueueAttributes,

    -- ** ListDeadLetterSourceQueues (Paginated)
    module Network.AWS.SQS.ListDeadLetterSourceQueues,

    -- ** AddPermission
    module Network.AWS.SQS.AddPermission,

    -- ** DeleteMessage
    module Network.AWS.SQS.DeleteMessage,

    -- ** ListQueueTags
    module Network.AWS.SQS.ListQueueTags,

    -- ** CreateQueue
    module Network.AWS.SQS.CreateQueue,

    -- ** UntagQueue
    module Network.AWS.SQS.UntagQueue,

    -- ** SendMessageBatch
    module Network.AWS.SQS.SendMessageBatch,

    -- ** ChangeMessageVisibility
    module Network.AWS.SQS.ChangeMessageVisibility,

    -- * Types

    -- ** MessageAttribute
    MessageAttribute (..),

    -- ** QueueAttributeName
    QueueAttributeName (..),

    -- ** String
    String (..),

    -- ** Token
    Token (..),

    -- ** DeleteMessageBatchRequestEntry
    DeleteMessageBatchRequestEntry (..),
    mkDeleteMessageBatchRequestEntry,
    dmbreId,
    dmbreReceiptHandle,

    -- ** TagValue
    TagValue (..),

    -- ** MessageSystemAttributeNameForSends
    MessageSystemAttributeNameForSends (..),

    -- ** MessageAttributeValue
    MessageAttributeValue (..),
    mkMessageAttributeValue,
    mavDataType,
    mavBinaryListValues,
    mavBinaryValue,
    mavStringListValues,
    mavStringValue,

    -- ** ChangeMessageVisibilityBatchResultEntry
    ChangeMessageVisibilityBatchResultEntry (..),
    mkChangeMessageVisibilityBatchResultEntry,
    cmvbreId,

    -- ** ChangeMessageVisibilityBatchRequestEntry
    ChangeMessageVisibilityBatchRequestEntry (..),
    mkChangeMessageVisibilityBatchRequestEntry,
    cId,
    cReceiptHandle,
    cVisibilityTimeout,

    -- ** TagKey
    TagKey (..),

    -- ** MessageAttributeName
    MessageAttributeName (..),

    -- ** DeleteMessageBatchResultEntry
    DeleteMessageBatchResultEntry (..),
    mkDeleteMessageBatchResultEntry,
    dId,

    -- ** Message
    Message (..),
    mkMessage,
    mAttributes,
    mBody,
    mMD5OfBody,
    mMD5OfMessageAttributes,
    mMessageAttributes,
    mMessageId,
    mReceiptHandle,

    -- ** SendMessageBatchRequestEntry
    SendMessageBatchRequestEntry (..),
    mkSendMessageBatchRequestEntry,
    sId,
    sMessageBody,
    sDelaySeconds,
    sMessageAttributes,
    sMessageDeduplicationId,
    sMessageGroupId,
    sMessageSystemAttributes,

    -- ** SendMessageBatchResultEntry
    SendMessageBatchResultEntry (..),
    mkSendMessageBatchResultEntry,
    smbreId,
    smbreMessageId,
    smbreMD5OfMessageBody,
    smbreMD5OfMessageAttributes,
    smbreMD5OfMessageSystemAttributes,
    smbreSequenceNumber,

    -- ** BatchResultErrorEntry
    BatchResultErrorEntry (..),
    mkBatchResultErrorEntry,
    breeId,
    breeSenderFault,
    breeCode,
    breeMessage,

    -- ** MessageSystemAttributeValue
    MessageSystemAttributeValue (..),
    mkMessageSystemAttributeValue,
    msavDataType,
    msavBinaryListValues,
    msavBinaryValue,
    msavStringListValues,
    msavStringValue,

    -- ** QueueUrl
    QueueUrl (..),

    -- ** Label
    Label (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.SQS.AddPermission
import Network.AWS.SQS.ChangeMessageVisibility
import Network.AWS.SQS.ChangeMessageVisibilityBatch
import Network.AWS.SQS.CreateQueue
import Network.AWS.SQS.DeleteMessage
import Network.AWS.SQS.DeleteMessageBatch
import Network.AWS.SQS.DeleteQueue
import Network.AWS.SQS.GetQueueAttributes
import Network.AWS.SQS.GetQueueUrl
import Network.AWS.SQS.ListDeadLetterSourceQueues
import Network.AWS.SQS.ListQueueTags
import Network.AWS.SQS.ListQueues
import Network.AWS.SQS.PurgeQueue
import Network.AWS.SQS.ReceiveMessage
import Network.AWS.SQS.RemovePermission
import Network.AWS.SQS.SendMessage
import Network.AWS.SQS.SendMessageBatch
import Network.AWS.SQS.SetQueueAttributes
import Network.AWS.SQS.TagQueue
import Network.AWS.SQS.Types
import Network.AWS.SQS.UntagQueue
import Network.AWS.SQS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SQS'.

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
