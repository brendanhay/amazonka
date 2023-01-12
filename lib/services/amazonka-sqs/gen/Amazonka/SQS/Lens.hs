{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SQS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SQS.Lens
  ( -- * Operations

    -- ** AddPermission
    addPermission_queueUrl,
    addPermission_label,
    addPermission_aWSAccountIds,
    addPermission_actions,

    -- ** ChangeMessageVisibility
    changeMessageVisibility_queueUrl,
    changeMessageVisibility_receiptHandle,
    changeMessageVisibility_visibilityTimeout,

    -- ** ChangeMessageVisibilityBatch
    changeMessageVisibilityBatch_queueUrl,
    changeMessageVisibilityBatch_entries,
    changeMessageVisibilityBatchResponse_httpStatus,
    changeMessageVisibilityBatchResponse_successful,
    changeMessageVisibilityBatchResponse_failed,

    -- ** CreateQueue
    createQueue_attributes,
    createQueue_tags,
    createQueue_queueName,
    createQueueResponse_queueUrl,
    createQueueResponse_httpStatus,

    -- ** DeleteMessage
    deleteMessage_queueUrl,
    deleteMessage_receiptHandle,

    -- ** DeleteMessageBatch
    deleteMessageBatch_queueUrl,
    deleteMessageBatch_entries,
    deleteMessageBatchResponse_httpStatus,
    deleteMessageBatchResponse_successful,
    deleteMessageBatchResponse_failed,

    -- ** DeleteQueue
    deleteQueue_queueUrl,

    -- ** GetQueueAttributes
    getQueueAttributes_attributeNames,
    getQueueAttributes_queueUrl,
    getQueueAttributesResponse_attributes,
    getQueueAttributesResponse_httpStatus,

    -- ** GetQueueUrl
    getQueueUrl_queueOwnerAWSAccountId,
    getQueueUrl_queueName,
    getQueueUrlResponse_httpStatus,
    getQueueUrlResponse_queueUrl,

    -- ** ListDeadLetterSourceQueues
    listDeadLetterSourceQueues_maxResults,
    listDeadLetterSourceQueues_nextToken,
    listDeadLetterSourceQueues_queueUrl,
    listDeadLetterSourceQueuesResponse_nextToken,
    listDeadLetterSourceQueuesResponse_httpStatus,
    listDeadLetterSourceQueuesResponse_queueUrls,

    -- ** ListQueueTags
    listQueueTags_queueUrl,
    listQueueTagsResponse_tags,
    listQueueTagsResponse_httpStatus,

    -- ** ListQueues
    listQueues_maxResults,
    listQueues_nextToken,
    listQueues_queueNamePrefix,
    listQueuesResponse_nextToken,
    listQueuesResponse_queueUrls,
    listQueuesResponse_httpStatus,

    -- ** PurgeQueue
    purgeQueue_queueUrl,

    -- ** ReceiveMessage
    receiveMessage_attributeNames,
    receiveMessage_maxNumberOfMessages,
    receiveMessage_messageAttributeNames,
    receiveMessage_receiveRequestAttemptId,
    receiveMessage_visibilityTimeout,
    receiveMessage_waitTimeSeconds,
    receiveMessage_queueUrl,
    receiveMessageResponse_messages,
    receiveMessageResponse_httpStatus,

    -- ** RemovePermission
    removePermission_queueUrl,
    removePermission_label,

    -- ** SendMessage
    sendMessage_delaySeconds,
    sendMessage_messageAttributes,
    sendMessage_messageDeduplicationId,
    sendMessage_messageGroupId,
    sendMessage_messageSystemAttributes,
    sendMessage_queueUrl,
    sendMessage_messageBody,
    sendMessageResponse_mD5OfMessageAttributes,
    sendMessageResponse_mD5OfMessageBody,
    sendMessageResponse_mD5OfMessageSystemAttributes,
    sendMessageResponse_messageId,
    sendMessageResponse_sequenceNumber,
    sendMessageResponse_httpStatus,

    -- ** SendMessageBatch
    sendMessageBatch_queueUrl,
    sendMessageBatch_entries,
    sendMessageBatchResponse_httpStatus,
    sendMessageBatchResponse_successful,
    sendMessageBatchResponse_failed,

    -- ** SetQueueAttributes
    setQueueAttributes_queueUrl,
    setQueueAttributes_attributes,

    -- ** TagQueue
    tagQueue_queueUrl,
    tagQueue_tags,

    -- ** UntagQueue
    untagQueue_queueUrl,
    untagQueue_tagKeys,

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
    message_attributes,
    message_body,
    message_mD5OfBody,
    message_mD5OfMessageAttributes,
    message_messageAttributes,
    message_messageId,
    message_receiptHandle,

    -- ** MessageAttributeValue
    messageAttributeValue_binaryListValues,
    messageAttributeValue_binaryValue,
    messageAttributeValue_stringListValues,
    messageAttributeValue_stringValue,
    messageAttributeValue_dataType,

    -- ** MessageSystemAttributeValue
    messageSystemAttributeValue_binaryListValues,
    messageSystemAttributeValue_binaryValue,
    messageSystemAttributeValue_stringListValues,
    messageSystemAttributeValue_stringValue,
    messageSystemAttributeValue_dataType,

    -- ** SendMessageBatchRequestEntry
    sendMessageBatchRequestEntry_delaySeconds,
    sendMessageBatchRequestEntry_messageAttributes,
    sendMessageBatchRequestEntry_messageDeduplicationId,
    sendMessageBatchRequestEntry_messageGroupId,
    sendMessageBatchRequestEntry_messageSystemAttributes,
    sendMessageBatchRequestEntry_id,
    sendMessageBatchRequestEntry_messageBody,

    -- ** SendMessageBatchResultEntry
    sendMessageBatchResultEntry_mD5OfMessageAttributes,
    sendMessageBatchResultEntry_mD5OfMessageSystemAttributes,
    sendMessageBatchResultEntry_sequenceNumber,
    sendMessageBatchResultEntry_id,
    sendMessageBatchResultEntry_messageId,
    sendMessageBatchResultEntry_mD5OfMessageBody,
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
import Amazonka.SQS.Types.BatchResultErrorEntry
import Amazonka.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
import Amazonka.SQS.Types.ChangeMessageVisibilityBatchResultEntry
import Amazonka.SQS.Types.DeleteMessageBatchRequestEntry
import Amazonka.SQS.Types.DeleteMessageBatchResultEntry
import Amazonka.SQS.Types.Message
import Amazonka.SQS.Types.MessageAttributeValue
import Amazonka.SQS.Types.MessageSystemAttributeValue
import Amazonka.SQS.Types.SendMessageBatchRequestEntry
import Amazonka.SQS.Types.SendMessageBatchResultEntry
import Amazonka.SQS.UntagQueue
