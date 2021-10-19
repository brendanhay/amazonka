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

    -- ** GetQueueUrl
    getQueueUrl_queueOwnerAWSAccountId,
    getQueueUrl_queueName,
    getQueueUrlResponse_httpStatus,
    getQueueUrlResponse_queueUrl,

    -- ** PurgeQueue
    purgeQueue_queueUrl,

    -- ** ChangeMessageVisibilityBatch
    changeMessageVisibilityBatch_queueUrl,
    changeMessageVisibilityBatch_entries,
    changeMessageVisibilityBatchResponse_httpStatus,
    changeMessageVisibilityBatchResponse_successful,
    changeMessageVisibilityBatchResponse_failed,

    -- ** SendMessage
    sendMessage_messageAttributes,
    sendMessage_delaySeconds,
    sendMessage_messageSystemAttributes,
    sendMessage_messageDeduplicationId,
    sendMessage_messageGroupId,
    sendMessage_queueUrl,
    sendMessage_messageBody,
    sendMessageResponse_sequenceNumber,
    sendMessageResponse_mD5OfMessageSystemAttributes,
    sendMessageResponse_messageId,
    sendMessageResponse_mD5OfMessageBody,
    sendMessageResponse_mD5OfMessageAttributes,
    sendMessageResponse_httpStatus,

    -- ** RemovePermission
    removePermission_queueUrl,
    removePermission_label,

    -- ** GetQueueAttributes
    getQueueAttributes_attributeNames,
    getQueueAttributes_queueUrl,
    getQueueAttributesResponse_attributes,
    getQueueAttributesResponse_httpStatus,

    -- ** ListQueues
    listQueues_queueNamePrefix,
    listQueues_nextToken,
    listQueues_maxResults,
    listQueuesResponse_queueUrls,
    listQueuesResponse_nextToken,
    listQueuesResponse_httpStatus,

    -- ** ReceiveMessage
    receiveMessage_receiveRequestAttemptId,
    receiveMessage_visibilityTimeout,
    receiveMessage_messageAttributeNames,
    receiveMessage_waitTimeSeconds,
    receiveMessage_attributeNames,
    receiveMessage_maxNumberOfMessages,
    receiveMessage_queueUrl,
    receiveMessageResponse_messages,
    receiveMessageResponse_httpStatus,

    -- ** DeleteQueue
    deleteQueue_queueUrl,

    -- ** TagQueue
    tagQueue_queueUrl,
    tagQueue_tags,

    -- ** DeleteMessageBatch
    deleteMessageBatch_queueUrl,
    deleteMessageBatch_entries,
    deleteMessageBatchResponse_httpStatus,
    deleteMessageBatchResponse_successful,
    deleteMessageBatchResponse_failed,

    -- ** SetQueueAttributes
    setQueueAttributes_queueUrl,
    setQueueAttributes_attributes,

    -- ** ListDeadLetterSourceQueues
    listDeadLetterSourceQueues_nextToken,
    listDeadLetterSourceQueues_maxResults,
    listDeadLetterSourceQueues_queueUrl,
    listDeadLetterSourceQueuesResponse_nextToken,
    listDeadLetterSourceQueuesResponse_httpStatus,
    listDeadLetterSourceQueuesResponse_queueUrls,

    -- ** AddPermission
    addPermission_queueUrl,
    addPermission_label,
    addPermission_aWSAccountIds,
    addPermission_actions,

    -- ** DeleteMessage
    deleteMessage_queueUrl,
    deleteMessage_receiptHandle,

    -- ** ListQueueTags
    listQueueTags_queueUrl,
    listQueueTagsResponse_tags,
    listQueueTagsResponse_httpStatus,

    -- ** CreateQueue
    createQueue_attributes,
    createQueue_tags,
    createQueue_queueName,
    createQueueResponse_queueUrl,
    createQueueResponse_httpStatus,

    -- ** UntagQueue
    untagQueue_queueUrl,
    untagQueue_tagKeys,

    -- ** SendMessageBatch
    sendMessageBatch_queueUrl,
    sendMessageBatch_entries,
    sendMessageBatchResponse_httpStatus,
    sendMessageBatchResponse_successful,
    sendMessageBatchResponse_failed,

    -- ** ChangeMessageVisibility
    changeMessageVisibility_queueUrl,
    changeMessageVisibility_receiptHandle,
    changeMessageVisibility_visibilityTimeout,

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
    message_messageAttributes,
    message_mD5OfBody,
    message_body,
    message_attributes,
    message_receiptHandle,
    message_messageId,
    message_mD5OfMessageAttributes,

    -- ** MessageAttributeValue
    messageAttributeValue_binaryValue,
    messageAttributeValue_stringListValues,
    messageAttributeValue_stringValue,
    messageAttributeValue_binaryListValues,
    messageAttributeValue_dataType,

    -- ** MessageSystemAttributeValue
    messageSystemAttributeValue_binaryValue,
    messageSystemAttributeValue_stringListValues,
    messageSystemAttributeValue_stringValue,
    messageSystemAttributeValue_binaryListValues,
    messageSystemAttributeValue_dataType,

    -- ** SendMessageBatchRequestEntry
    sendMessageBatchRequestEntry_messageAttributes,
    sendMessageBatchRequestEntry_delaySeconds,
    sendMessageBatchRequestEntry_messageSystemAttributes,
    sendMessageBatchRequestEntry_messageDeduplicationId,
    sendMessageBatchRequestEntry_messageGroupId,
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
