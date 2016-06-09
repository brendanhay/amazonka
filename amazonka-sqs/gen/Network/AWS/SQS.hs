{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Welcome to the /Amazon Simple Queue Service API Reference/. This section describes who should read this guide, how the guide is organized, and other resources related to the Amazon Simple Queue Service (Amazon SQS).
--
-- Amazon SQS offers reliable and scalable hosted queues for storing messages as they travel between computers. By using Amazon SQS, you can move data between distributed components of your applications that perform different tasks without losing messages or requiring each component to be always available.
--
-- Helpful Links:
--
-- -   <http://queue.amazonaws.com/doc/2012-11-05/QueueService.wsdl Current WSDL (2012-11-05)>
-- -   <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/MakingRequestsArticle.html Making API Requests>
-- -   <http://aws.amazon.com/sqs/ Amazon SQS product page>
-- -   <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html Using Amazon SQS Message Attributes>
-- -   <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSDeadLetterQueue.html Using Amazon SQS Dead Letter Queues>
-- -   <http://docs.aws.amazon.com/general/latest/gr/rande.html#sqs_region Regions and Endpoints>
--
-- We also provide SDKs that enable you to access Amazon SQS from your preferred programming language. The SDKs contain functionality that automatically takes care of tasks such as:
--
-- -   Cryptographically signing your service requests
-- -   Retrying requests
-- -   Handling error responses
--
-- For a list of available SDKs, go to <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
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

    -- ** CreateQueue
    , module Network.AWS.SQS.CreateQueue

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
    , sId
    , sMessageBody

    -- ** SendMessageBatchResultEntry
    , SendMessageBatchResultEntry
    , sendMessageBatchResultEntry
    , smbreMD5OfMessageAttributes
    , smbreId
    , smbreMessageId
    , smbreMD5OfMessageBody
    ) where

import           Network.AWS.SQS.AddPermission
import           Network.AWS.SQS.ChangeMessageVisibility
import           Network.AWS.SQS.ChangeMessageVisibilityBatch
import           Network.AWS.SQS.CreateQueue
import           Network.AWS.SQS.DeleteMessage
import           Network.AWS.SQS.DeleteMessageBatch
import           Network.AWS.SQS.DeleteQueue
import           Network.AWS.SQS.GetQueueAttributes
import           Network.AWS.SQS.GetQueueURL
import           Network.AWS.SQS.ListDeadLetterSourceQueues
import           Network.AWS.SQS.ListQueues
import           Network.AWS.SQS.PurgeQueue
import           Network.AWS.SQS.ReceiveMessage
import           Network.AWS.SQS.RemovePermission
import           Network.AWS.SQS.SendMessage
import           Network.AWS.SQS.SendMessageBatch
import           Network.AWS.SQS.SetQueueAttributes
import           Network.AWS.SQS.Types
import           Network.AWS.SQS.Waiters

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
