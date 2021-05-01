{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Welcome to the /Amazon Simple Queue Service API Reference/.
--
-- Amazon Simple Queue Service (Amazon SQS) is a reliable, highly-scalable
-- hosted queue for storing messages as they travel between applications or
-- microservices. Amazon SQS moves data between distributed application
-- components and helps you decouple these components.
--
-- For information on the permissions you need to use this API, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-authentication-and-access-control.html Identity and access management>
-- in the /Amazon Simple Queue Service Developer Guide./
--
-- You can use <http://aws.amazon.com/tools/#sdk AWS SDKs> to access Amazon
-- SQS using your favorite programming language. The SDKs perform tasks
-- such as the following automatically:
--
-- -   Cryptographically sign your service requests
--
-- -   Retry requests
--
-- -   Handle error responses
--
-- __Additional Information__
--
-- -   <http://aws.amazon.com/sqs/ Amazon SQS Product Page>
--
-- -   /Amazon Simple Queue Service Developer Guide/
--
--     -   <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html Making API Requests>
--
--     -   <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
--
--     -   <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Amazon SQS Dead-Letter Queues>
--
-- -   <http://docs.aws.amazon.com/cli/latest/reference/sqs/index.html Amazon SQS in the AWS CLI Command Reference>
--
-- -   /Amazon Web Services General Reference/
--
--     -   <https://docs.aws.amazon.com/general/latest/gr/rande.html#sqs_region Regions and Endpoints>
module Network.AWS.SQS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidIdFormat
    _InvalidIdFormat,

    -- ** TooManyEntriesInBatchRequest
    _TooManyEntriesInBatchRequest,

    -- ** QueueNameExists
    _QueueNameExists,

    -- ** EmptyBatchRequest
    _EmptyBatchRequest,

    -- ** InvalidMessageContents
    _InvalidMessageContents,

    -- ** UnsupportedOperation
    _UnsupportedOperation,

    -- ** ReceiptHandleIsInvalid
    _ReceiptHandleIsInvalid,

    -- ** InvalidAttributeName
    _InvalidAttributeName,

    -- ** QueueDeletedRecently
    _QueueDeletedRecently,

    -- ** InvalidBatchEntryId
    _InvalidBatchEntryId,

    -- ** BatchEntryIdsNotDistinct
    _BatchEntryIdsNotDistinct,

    -- ** MessageNotInflight
    _MessageNotInflight,

    -- ** PurgeQueueInProgress
    _PurgeQueueInProgress,

    -- ** BatchRequestTooLong
    _BatchRequestTooLong,

    -- ** OverLimit
    _OverLimit,

    -- ** QueueDoesNotExist
    _QueueDoesNotExist,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ChangeMessageVisibilityBatch
    ChangeMessageVisibilityBatch (ChangeMessageVisibilityBatch'),
    newChangeMessageVisibilityBatch,
    ChangeMessageVisibilityBatchResponse (ChangeMessageVisibilityBatchResponse'),
    newChangeMessageVisibilityBatchResponse,

    -- ** PurgeQueue
    PurgeQueue (PurgeQueue'),
    newPurgeQueue,
    PurgeQueueResponse (PurgeQueueResponse'),
    newPurgeQueueResponse,

    -- ** ChangeMessageVisibility
    ChangeMessageVisibility (ChangeMessageVisibility'),
    newChangeMessageVisibility,
    ChangeMessageVisibilityResponse (ChangeMessageVisibilityResponse'),
    newChangeMessageVisibilityResponse,

    -- ** TagQueue
    TagQueue (TagQueue'),
    newTagQueue,
    TagQueueResponse (TagQueueResponse'),
    newTagQueueResponse,

    -- ** ListQueues (Paginated)
    ListQueues (ListQueues'),
    newListQueues,
    ListQueuesResponse (ListQueuesResponse'),
    newListQueuesResponse,

    -- ** ReceiveMessage
    ReceiveMessage (ReceiveMessage'),
    newReceiveMessage,
    ReceiveMessageResponse (ReceiveMessageResponse'),
    newReceiveMessageResponse,

    -- ** GetQueueAttributes
    GetQueueAttributes (GetQueueAttributes'),
    newGetQueueAttributes,
    GetQueueAttributesResponse (GetQueueAttributesResponse'),
    newGetQueueAttributesResponse,

    -- ** DeleteMessage
    DeleteMessage (DeleteMessage'),
    newDeleteMessage,
    DeleteMessageResponse (DeleteMessageResponse'),
    newDeleteMessageResponse,

    -- ** AddPermission
    AddPermission (AddPermission'),
    newAddPermission,
    AddPermissionResponse (AddPermissionResponse'),
    newAddPermissionResponse,

    -- ** ListQueueTags
    ListQueueTags (ListQueueTags'),
    newListQueueTags,
    ListQueueTagsResponse (ListQueueTagsResponse'),
    newListQueueTagsResponse,

    -- ** SendMessage
    SendMessage (SendMessage'),
    newSendMessage,
    SendMessageResponse (SendMessageResponse'),
    newSendMessageResponse,

    -- ** ListDeadLetterSourceQueues (Paginated)
    ListDeadLetterSourceQueues (ListDeadLetterSourceQueues'),
    newListDeadLetterSourceQueues,
    ListDeadLetterSourceQueuesResponse (ListDeadLetterSourceQueuesResponse'),
    newListDeadLetterSourceQueuesResponse,

    -- ** GetQueueUrl
    GetQueueUrl (GetQueueUrl'),
    newGetQueueUrl,
    GetQueueUrlResponse (GetQueueUrlResponse'),
    newGetQueueUrlResponse,

    -- ** SetQueueAttributes
    SetQueueAttributes (SetQueueAttributes'),
    newSetQueueAttributes,
    SetQueueAttributesResponse (SetQueueAttributesResponse'),
    newSetQueueAttributesResponse,

    -- ** DeleteMessageBatch
    DeleteMessageBatch (DeleteMessageBatch'),
    newDeleteMessageBatch,
    DeleteMessageBatchResponse (DeleteMessageBatchResponse'),
    newDeleteMessageBatchResponse,

    -- ** SendMessageBatch
    SendMessageBatch (SendMessageBatch'),
    newSendMessageBatch,
    SendMessageBatchResponse (SendMessageBatchResponse'),
    newSendMessageBatchResponse,

    -- ** UntagQueue
    UntagQueue (UntagQueue'),
    newUntagQueue,
    UntagQueueResponse (UntagQueueResponse'),
    newUntagQueueResponse,

    -- ** DeleteQueue
    DeleteQueue (DeleteQueue'),
    newDeleteQueue,
    DeleteQueueResponse (DeleteQueueResponse'),
    newDeleteQueueResponse,

    -- ** CreateQueue
    CreateQueue (CreateQueue'),
    newCreateQueue,
    CreateQueueResponse (CreateQueueResponse'),
    newCreateQueueResponse,

    -- ** RemovePermission
    RemovePermission (RemovePermission'),
    newRemovePermission,
    RemovePermissionResponse (RemovePermissionResponse'),
    newRemovePermissionResponse,

    -- * Types

    -- ** MessageAttribute
    MessageAttribute (..),

    -- ** MessageSystemAttributeNameForSends
    MessageSystemAttributeNameForSends (..),

    -- ** QueueAttributeName
    QueueAttributeName (..),

    -- ** BatchResultErrorEntry
    BatchResultErrorEntry (BatchResultErrorEntry'),
    newBatchResultErrorEntry,

    -- ** ChangeMessageVisibilityBatchRequestEntry
    ChangeMessageVisibilityBatchRequestEntry (ChangeMessageVisibilityBatchRequestEntry'),
    newChangeMessageVisibilityBatchRequestEntry,

    -- ** ChangeMessageVisibilityBatchResultEntry
    ChangeMessageVisibilityBatchResultEntry (ChangeMessageVisibilityBatchResultEntry'),
    newChangeMessageVisibilityBatchResultEntry,

    -- ** DeleteMessageBatchRequestEntry
    DeleteMessageBatchRequestEntry (DeleteMessageBatchRequestEntry'),
    newDeleteMessageBatchRequestEntry,

    -- ** DeleteMessageBatchResultEntry
    DeleteMessageBatchResultEntry (DeleteMessageBatchResultEntry'),
    newDeleteMessageBatchResultEntry,

    -- ** Message
    Message (Message'),
    newMessage,

    -- ** MessageAttributeValue
    MessageAttributeValue (MessageAttributeValue'),
    newMessageAttributeValue,

    -- ** MessageSystemAttributeValue
    MessageSystemAttributeValue (MessageSystemAttributeValue'),
    newMessageSystemAttributeValue,

    -- ** SendMessageBatchRequestEntry
    SendMessageBatchRequestEntry (SendMessageBatchRequestEntry'),
    newSendMessageBatchRequestEntry,

    -- ** SendMessageBatchResultEntry
    SendMessageBatchResultEntry (SendMessageBatchResultEntry'),
    newSendMessageBatchResultEntry,
  )
where

import Network.AWS.SQS.AddPermission
import Network.AWS.SQS.ChangeMessageVisibility
import Network.AWS.SQS.ChangeMessageVisibilityBatch
import Network.AWS.SQS.CreateQueue
import Network.AWS.SQS.DeleteMessage
import Network.AWS.SQS.DeleteMessageBatch
import Network.AWS.SQS.DeleteQueue
import Network.AWS.SQS.GetQueueAttributes
import Network.AWS.SQS.GetQueueUrl
import Network.AWS.SQS.Lens
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
