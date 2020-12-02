{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Welcome to the /Amazon Simple Queue Service API Reference/ .
--
--
-- Amazon Simple Queue Service (Amazon SQS) is a reliable, highly-scalable hosted queue for storing messages as they travel between applications or microservices. Amazon SQS moves data between distributed application components and helps you decouple these components.
--
-- You can use <http://aws.amazon.com/tools/#sdk AWS SDKs> to access Amazon SQS using your favorite programming language. The SDKs perform tasks such as the following automatically:
--
--     * Cryptographically sign your service requests
--
--     * Retry requests
--
--     * Handle error responses
--
--
--
-- __Additional Information__
--
--     * <http://aws.amazon.com/sqs/ Amazon SQS Product Page>
--
--     * /Amazon Simple Queue Service Developer Guide/
--
--     * <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/MakingRequestsArticle.html Making API Requests>
--
--     * <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html Using Amazon SQS Message Attributes>
--
--     * <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues>
--
--
--
--     * /Amazon Web Services General Reference/
--
--     * <http://docs.aws.amazon.com/general/latest/gr/rande.html#sqs_region Regions and Endpoints>
--
--
--
--
--
module Network.AWS.SQS
    (
    -- * Service Configuration
      sqs

    -- * Errors
    -- $errors

    -- ** InvalidBatchEntryId
    , _InvalidBatchEntryId

    -- ** TooManyEntriesInBatchRequest
    , _TooManyEntriesInBatchRequest

    -- ** QueueDeletedRecently
    , _QueueDeletedRecently

    -- ** QueueDoesNotExist
    , _QueueDoesNotExist

    -- ** InvalidAttributeName
    , _InvalidAttributeName

    -- ** UnsupportedOperation
    , _UnsupportedOperation

    -- ** InvalidMessageContents
    , _InvalidMessageContents

    -- ** BatchRequestTooLong
    , _BatchRequestTooLong

    -- ** OverLimit
    , _OverLimit

    -- ** QueueNameExists
    , _QueueNameExists

    -- ** PurgeQueueInProgress
    , _PurgeQueueInProgress

    -- ** InvalidIdFormat
    , _InvalidIdFormat

    -- ** ReceiptHandleIsInvalid
    , _ReceiptHandleIsInvalid

    -- ** EmptyBatchRequest
    , _EmptyBatchRequest

    -- ** BatchEntryIdsNotDistinct
    , _BatchEntryIdsNotDistinct

    -- ** MessageNotInflight
    , _MessageNotInflight

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetQueueURL
    , module Network.AWS.SQS.GetQueueURL

    -- ** PurgeQueue
    , module Network.AWS.SQS.PurgeQueue

    -- ** ChangeMessageVisibilityBatch
    , module Network.AWS.SQS.ChangeMessageVisibilityBatch

    -- ** SendMessage
    , module Network.AWS.SQS.SendMessage

    -- ** RemovePermission
    , module Network.AWS.SQS.RemovePermission

    -- ** GetQueueAttributes
    , module Network.AWS.SQS.GetQueueAttributes

    -- ** ListQueues
    , module Network.AWS.SQS.ListQueues

    -- ** ReceiveMessage
    , module Network.AWS.SQS.ReceiveMessage

    -- ** DeleteQueue
    , module Network.AWS.SQS.DeleteQueue

    -- ** TagQueue
    , module Network.AWS.SQS.TagQueue

    -- ** DeleteMessageBatch
    , module Network.AWS.SQS.DeleteMessageBatch

    -- ** SetQueueAttributes
    , module Network.AWS.SQS.SetQueueAttributes

    -- ** ListDeadLetterSourceQueues
    , module Network.AWS.SQS.ListDeadLetterSourceQueues

    -- ** AddPermission
    , module Network.AWS.SQS.AddPermission

    -- ** DeleteMessage
    , module Network.AWS.SQS.DeleteMessage

    -- ** ListQueueTags
    , module Network.AWS.SQS.ListQueueTags

    -- ** CreateQueue
    , module Network.AWS.SQS.CreateQueue

    -- ** UntagQueue
    , module Network.AWS.SQS.UntagQueue

    -- ** SendMessageBatch
    , module Network.AWS.SQS.SendMessageBatch

    -- ** ChangeMessageVisibility
    , module Network.AWS.SQS.ChangeMessageVisibility

    -- * Types

    -- ** MessageAttribute
    , MessageAttribute (..)

    -- ** QueueAttributeName
    , QueueAttributeName (..)

    -- ** BatchResultErrorEntry
    , BatchResultErrorEntry
    , batchResultErrorEntry
    , breeMessage
    , breeId
    , breeSenderFault
    , breeCode

    -- ** ChangeMessageVisibilityBatchRequestEntry
    , ChangeMessageVisibilityBatchRequestEntry
    , changeMessageVisibilityBatchRequestEntry
    , cVisibilityTimeout
    , cId
    , cReceiptHandle

    -- ** ChangeMessageVisibilityBatchResultEntry
    , ChangeMessageVisibilityBatchResultEntry
    , changeMessageVisibilityBatchResultEntry
    , cmvbreId

    -- ** DeleteMessageBatchRequestEntry
    , DeleteMessageBatchRequestEntry
    , deleteMessageBatchRequestEntry
    , dmbreId
    , dmbreReceiptHandle

    -- ** DeleteMessageBatchResultEntry
    , DeleteMessageBatchResultEntry
    , deleteMessageBatchResultEntry
    , dId

    -- ** Message
    , Message
    , message
    , mMessageAttributes
    , mMD5OfBody
    , mBody
    , mAttributes
    , mReceiptHandle
    , mMessageId
    , mMD5OfMessageAttributes

    -- ** MessageAttributeValue
    , MessageAttributeValue
    , messageAttributeValue
    , mavBinaryValue
    , mavStringListValues
    , mavStringValue
    , mavBinaryListValues
    , mavDataType

    -- ** SendMessageBatchRequestEntry
    , SendMessageBatchRequestEntry
    , sendMessageBatchRequestEntry
    , sMessageAttributes
    , sDelaySeconds
    , sMessageDeduplicationId
    , sMessageGroupId
    , sId
    , sMessageBody

    -- ** SendMessageBatchResultEntry
    , SendMessageBatchResultEntry
    , sendMessageBatchResultEntry
    , smbreSequenceNumber
    , smbreMD5OfMessageAttributes
    , smbreId
    , smbreMessageId
    , smbreMD5OfMessageBody
    ) where

import Network.AWS.SQS.AddPermission
import Network.AWS.SQS.ChangeMessageVisibility
import Network.AWS.SQS.ChangeMessageVisibilityBatch
import Network.AWS.SQS.CreateQueue
import Network.AWS.SQS.DeleteMessage
import Network.AWS.SQS.DeleteMessageBatch
import Network.AWS.SQS.DeleteQueue
import Network.AWS.SQS.GetQueueAttributes
import Network.AWS.SQS.GetQueueURL
import Network.AWS.SQS.ListDeadLetterSourceQueues
import Network.AWS.SQS.ListQueues
import Network.AWS.SQS.ListQueueTags
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

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SQS'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
