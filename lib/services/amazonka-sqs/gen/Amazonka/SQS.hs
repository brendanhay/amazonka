{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SQS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2012-11-05@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the /Amazon SQS API Reference/.
--
-- Amazon SQS is a reliable, highly-scalable hosted queue for storing
-- messages as they travel between applications or microservices. Amazon
-- SQS moves data between distributed application components and helps you
-- decouple these components.
--
-- For information on the permissions you need to use this API, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-authentication-and-access-control.html Identity and access management>
-- in the /Amazon SQS Developer Guide./
--
-- You can use <http://aws.amazon.com/tools/#sdk Amazon Web Services SDKs>
-- to access Amazon SQS using your favorite programming language. The SDKs
-- perform tasks such as the following automatically:
--
-- -   Cryptographically sign your service requests
--
-- -   Retry requests
--
-- -   Handle error responses
--
-- __Additional information__
--
-- -   <http://aws.amazon.com/sqs/ Amazon SQS Product Page>
--
-- -   /Amazon SQS Developer Guide/
--
--     -   <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html Making API Requests>
--
--     -   <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
--
--     -   <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Amazon SQS Dead-Letter Queues>
--
-- -   <http://docs.aws.amazon.com/cli/latest/reference/sqs/index.html Amazon SQS in the Command Line Interface>
--
-- -   /Amazon Web Services General Reference/
--
--     -   <https://docs.aws.amazon.com/general/latest/gr/rande.html#sqs_region Regions and Endpoints>
module Amazonka.SQS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BatchEntryIdsNotDistinct
    _BatchEntryIdsNotDistinct,

    -- ** BatchRequestTooLong
    _BatchRequestTooLong,

    -- ** EmptyBatchRequest
    _EmptyBatchRequest,

    -- ** InvalidAttributeName
    _InvalidAttributeName,

    -- ** InvalidBatchEntryId
    _InvalidBatchEntryId,

    -- ** InvalidIdFormat
    _InvalidIdFormat,

    -- ** InvalidMessageContents
    _InvalidMessageContents,

    -- ** MessageNotInflight
    _MessageNotInflight,

    -- ** OverLimit
    _OverLimit,

    -- ** PurgeQueueInProgress
    _PurgeQueueInProgress,

    -- ** QueueDeletedRecently
    _QueueDeletedRecently,

    -- ** QueueDoesNotExist
    _QueueDoesNotExist,

    -- ** QueueNameExists
    _QueueNameExists,

    -- ** ReceiptHandleIsInvalid
    _ReceiptHandleIsInvalid,

    -- ** TooManyEntriesInBatchRequest
    _TooManyEntriesInBatchRequest,

    -- ** UnsupportedOperation
    _UnsupportedOperation,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddPermission
    AddPermission (AddPermission'),
    newAddPermission,
    AddPermissionResponse (AddPermissionResponse'),
    newAddPermissionResponse,

    -- ** ChangeMessageVisibility
    ChangeMessageVisibility (ChangeMessageVisibility'),
    newChangeMessageVisibility,
    ChangeMessageVisibilityResponse (ChangeMessageVisibilityResponse'),
    newChangeMessageVisibilityResponse,

    -- ** ChangeMessageVisibilityBatch
    ChangeMessageVisibilityBatch (ChangeMessageVisibilityBatch'),
    newChangeMessageVisibilityBatch,
    ChangeMessageVisibilityBatchResponse (ChangeMessageVisibilityBatchResponse'),
    newChangeMessageVisibilityBatchResponse,

    -- ** CreateQueue
    CreateQueue (CreateQueue'),
    newCreateQueue,
    CreateQueueResponse (CreateQueueResponse'),
    newCreateQueueResponse,

    -- ** DeleteMessage
    DeleteMessage (DeleteMessage'),
    newDeleteMessage,
    DeleteMessageResponse (DeleteMessageResponse'),
    newDeleteMessageResponse,

    -- ** DeleteMessageBatch
    DeleteMessageBatch (DeleteMessageBatch'),
    newDeleteMessageBatch,
    DeleteMessageBatchResponse (DeleteMessageBatchResponse'),
    newDeleteMessageBatchResponse,

    -- ** DeleteQueue
    DeleteQueue (DeleteQueue'),
    newDeleteQueue,
    DeleteQueueResponse (DeleteQueueResponse'),
    newDeleteQueueResponse,

    -- ** GetQueueAttributes
    GetQueueAttributes (GetQueueAttributes'),
    newGetQueueAttributes,
    GetQueueAttributesResponse (GetQueueAttributesResponse'),
    newGetQueueAttributesResponse,

    -- ** GetQueueUrl
    GetQueueUrl (GetQueueUrl'),
    newGetQueueUrl,
    GetQueueUrlResponse (GetQueueUrlResponse'),
    newGetQueueUrlResponse,

    -- ** ListDeadLetterSourceQueues (Paginated)
    ListDeadLetterSourceQueues (ListDeadLetterSourceQueues'),
    newListDeadLetterSourceQueues,
    ListDeadLetterSourceQueuesResponse (ListDeadLetterSourceQueuesResponse'),
    newListDeadLetterSourceQueuesResponse,

    -- ** ListQueueTags
    ListQueueTags (ListQueueTags'),
    newListQueueTags,
    ListQueueTagsResponse (ListQueueTagsResponse'),
    newListQueueTagsResponse,

    -- ** ListQueues (Paginated)
    ListQueues (ListQueues'),
    newListQueues,
    ListQueuesResponse (ListQueuesResponse'),
    newListQueuesResponse,

    -- ** PurgeQueue
    PurgeQueue (PurgeQueue'),
    newPurgeQueue,
    PurgeQueueResponse (PurgeQueueResponse'),
    newPurgeQueueResponse,

    -- ** ReceiveMessage
    ReceiveMessage (ReceiveMessage'),
    newReceiveMessage,
    ReceiveMessageResponse (ReceiveMessageResponse'),
    newReceiveMessageResponse,

    -- ** RemovePermission
    RemovePermission (RemovePermission'),
    newRemovePermission,
    RemovePermissionResponse (RemovePermissionResponse'),
    newRemovePermissionResponse,

    -- ** SendMessage
    SendMessage (SendMessage'),
    newSendMessage,
    SendMessageResponse (SendMessageResponse'),
    newSendMessageResponse,

    -- ** SendMessageBatch
    SendMessageBatch (SendMessageBatch'),
    newSendMessageBatch,
    SendMessageBatchResponse (SendMessageBatchResponse'),
    newSendMessageBatchResponse,

    -- ** SetQueueAttributes
    SetQueueAttributes (SetQueueAttributes'),
    newSetQueueAttributes,
    SetQueueAttributesResponse (SetQueueAttributesResponse'),
    newSetQueueAttributesResponse,

    -- ** TagQueue
    TagQueue (TagQueue'),
    newTagQueue,
    TagQueueResponse (TagQueueResponse'),
    newTagQueueResponse,

    -- ** UntagQueue
    UntagQueue (UntagQueue'),
    newUntagQueue,
    UntagQueueResponse (UntagQueueResponse'),
    newUntagQueueResponse,

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

import Amazonka.SQS.AddPermission
import Amazonka.SQS.ChangeMessageVisibility
import Amazonka.SQS.ChangeMessageVisibilityBatch
import Amazonka.SQS.CreateQueue
import Amazonka.SQS.DeleteMessage
import Amazonka.SQS.DeleteMessageBatch
import Amazonka.SQS.DeleteQueue
import Amazonka.SQS.GetQueueAttributes
import Amazonka.SQS.GetQueueUrl
import Amazonka.SQS.Lens
import Amazonka.SQS.ListDeadLetterSourceQueues
import Amazonka.SQS.ListQueueTags
import Amazonka.SQS.ListQueues
import Amazonka.SQS.PurgeQueue
import Amazonka.SQS.ReceiveMessage
import Amazonka.SQS.RemovePermission
import Amazonka.SQS.SendMessage
import Amazonka.SQS.SendMessageBatch
import Amazonka.SQS.SetQueueAttributes
import Amazonka.SQS.TagQueue
import Amazonka.SQS.Types
import Amazonka.SQS.UntagQueue
import Amazonka.SQS.Waiters

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
