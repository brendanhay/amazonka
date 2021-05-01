{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Lens
  ( -- * Operations

    -- ** ChangeMessageVisibilityBatch
    changeMessageVisibilityBatch_queueUrl,
    changeMessageVisibilityBatch_entries,
    changeMessageVisibilityBatchResponse_httpStatus,
    changeMessageVisibilityBatchResponse_successful,
    changeMessageVisibilityBatchResponse_failed,

    -- ** PurgeQueue
    purgeQueue_queueUrl,

    -- ** ChangeMessageVisibility
    changeMessageVisibility_queueUrl,
    changeMessageVisibility_receiptHandle,
    changeMessageVisibility_visibilityTimeout,

    -- ** TagQueue
    tagQueue_queueUrl,
    tagQueue_tags,

    -- ** ListQueues
    listQueues_nextToken,
    listQueues_maxResults,
    listQueues_queueNamePrefix,
    listQueuesResponse_nextToken,
    listQueuesResponse_queueUrls,
    listQueuesResponse_httpStatus,

    -- ** ReceiveMessage
    receiveMessage_visibilityTimeout,
    receiveMessage_maxNumberOfMessages,
    receiveMessage_messageAttributeNames,
    receiveMessage_attributeNames,
    receiveMessage_waitTimeSeconds,
    receiveMessage_receiveRequestAttemptId,
    receiveMessage_queueUrl,
    receiveMessageResponse_messages,
    receiveMessageResponse_httpStatus,

    -- ** GetQueueAttributes
    getQueueAttributes_attributeNames,
    getQueueAttributes_queueUrl,
    getQueueAttributesResponse_attributes,
    getQueueAttributesResponse_httpStatus,

    -- ** DeleteMessage
    deleteMessage_queueUrl,
    deleteMessage_receiptHandle,

    -- ** AddPermission
    addPermission_queueUrl,
    addPermission_label,
    addPermission_aWSAccountIds,
    addPermission_actions,

    -- ** ListQueueTags
    listQueueTags_queueUrl,
    listQueueTagsResponse_tags,
    listQueueTagsResponse_httpStatus,

    -- ** SendMessage
    sendMessage_messageDeduplicationId,
    sendMessage_messageAttributes,
    sendMessage_messageSystemAttributes,
    sendMessage_messageGroupId,
    sendMessage_delaySeconds,
    sendMessage_queueUrl,
    sendMessage_messageBody,
    sendMessageResponse_sequenceNumber,
    sendMessageResponse_mD5OfMessageBody,
    sendMessageResponse_mD5OfMessageSystemAttributes,
    sendMessageResponse_mD5OfMessageAttributes,
    sendMessageResponse_messageId,
    sendMessageResponse_httpStatus,

    -- ** ListDeadLetterSourceQueues
    listDeadLetterSourceQueues_nextToken,
    listDeadLetterSourceQueues_maxResults,
    listDeadLetterSourceQueues_queueUrl,
    listDeadLetterSourceQueuesResponse_nextToken,
    listDeadLetterSourceQueuesResponse_httpStatus,
    listDeadLetterSourceQueuesResponse_queueUrls,

    -- ** GetQueueUrl
    getQueueUrl_queueOwnerAWSAccountId,
    getQueueUrl_queueName,
    getQueueUrlResponse_httpStatus,
    getQueueUrlResponse_queueUrl,

    -- ** SetQueueAttributes
    setQueueAttributes_queueUrl,
    setQueueAttributes_attributes,

    -- ** DeleteMessageBatch
    deleteMessageBatch_queueUrl,
    deleteMessageBatch_entries,
    deleteMessageBatchResponse_httpStatus,
    deleteMessageBatchResponse_successful,
    deleteMessageBatchResponse_failed,

    -- ** SendMessageBatch
    sendMessageBatch_queueUrl,
    sendMessageBatch_entries,
    sendMessageBatchResponse_httpStatus,
    sendMessageBatchResponse_successful,
    sendMessageBatchResponse_failed,

    -- ** UntagQueue
    untagQueue_queueUrl,
    untagQueue_tagKeys,

    -- ** DeleteQueue
    deleteQueue_queueUrl,

    -- ** CreateQueue
    createQueue_attributes,
    createQueue_tags,
    createQueue_queueName,
    createQueueResponse_queueUrl,
    createQueueResponse_httpStatus,

    -- ** RemovePermission
    removePermission_queueUrl,
    removePermission_label,

    -- * Types

    -- ** BatchResultErrorEntry
    batchResultErrorEntry_message,
    batchResultErrorEntry_id,
    batchResultErrorEntry_senderFault,
    batchResultErrorEntry_code,

    -- ** ChangeMessageVisibilityBatchRequestEntry
    changeMessageVisibilityBatchRequestEntry_visibilityTimeout,
    changeMessageVisibilityBatchRequestEntry_id,
    changeMessageVisibilityBatchRequestEntry_receiptHandle,

    -- ** ChangeMessageVisibilityBatchResultEntry
    changeMessageVisibilityBatchResultEntry_id,

    -- ** DeleteMessageBatchRequestEntry
    deleteMessageBatchRequestEntry_id,
    deleteMessageBatchRequestEntry_receiptHandle,

    -- ** DeleteMessageBatchResultEntry
    deleteMessageBatchResultEntry_id,

    -- ** Message
    message_body,
    message_mD5OfBody,
    message_attributes,
    message_messageAttributes,
    message_mD5OfMessageAttributes,
    message_receiptHandle,
    message_messageId,

    -- ** MessageAttributeValue
    messageAttributeValue_stringListValues,
    messageAttributeValue_stringValue,
    messageAttributeValue_binaryListValues,
    messageAttributeValue_binaryValue,
    messageAttributeValue_dataType,

    -- ** MessageSystemAttributeValue
    messageSystemAttributeValue_stringListValues,
    messageSystemAttributeValue_stringValue,
    messageSystemAttributeValue_binaryListValues,
    messageSystemAttributeValue_binaryValue,
    messageSystemAttributeValue_dataType,

    -- ** SendMessageBatchRequestEntry
    sendMessageBatchRequestEntry_messageDeduplicationId,
    sendMessageBatchRequestEntry_messageAttributes,
    sendMessageBatchRequestEntry_messageSystemAttributes,
    sendMessageBatchRequestEntry_messageGroupId,
    sendMessageBatchRequestEntry_delaySeconds,
    sendMessageBatchRequestEntry_id,
    sendMessageBatchRequestEntry_messageBody,

    -- ** SendMessageBatchResultEntry
    sendMessageBatchResultEntry_sequenceNumber,
    sendMessageBatchResultEntry_mD5OfMessageSystemAttributes,
    sendMessageBatchResultEntry_mD5OfMessageAttributes,
    sendMessageBatchResultEntry_id,
    sendMessageBatchResultEntry_messageId,
    sendMessageBatchResultEntry_mD5OfMessageBody,
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
import Network.AWS.SQS.Types.BatchResultErrorEntry
import Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
import Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry
import Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry
import Network.AWS.SQS.Types.DeleteMessageBatchResultEntry
import Network.AWS.SQS.Types.Message
import Network.AWS.SQS.Types.MessageAttributeValue
import Network.AWS.SQS.Types.MessageSystemAttributeValue
import Network.AWS.SQS.Types.SendMessageBatchRequestEntry
import Network.AWS.SQS.Types.SendMessageBatchResultEntry
import Network.AWS.SQS.UntagQueue
