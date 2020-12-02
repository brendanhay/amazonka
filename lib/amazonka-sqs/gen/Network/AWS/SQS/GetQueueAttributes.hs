{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.GetQueueAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets attributes for the specified queue.
module Network.AWS.SQS.GetQueueAttributes
  ( -- * Creating a Request
    getQueueAttributes,
    GetQueueAttributes,

    -- * Request Lenses
    gqaAttributeNames,
    gqaQueueURL,

    -- * Destructuring the Response
    getQueueAttributesResponse,
    GetQueueAttributesResponse,

    -- * Response Lenses
    gqarsAttributes,
    gqarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- |
--
--
--
-- /See:/ 'getQueueAttributes' smart constructor.
data GetQueueAttributes = GetQueueAttributes'
  { _gqaAttributeNames ::
      !(Maybe [QueueAttributeName]),
    _gqaQueueURL :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetQueueAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqaAttributeNames' - A list of attributes for which to retrieve information. The following attributes are supported: /Important:/ The @ApproximateNumberOfMessagesDelayed@ , @ApproximateNumberOfMessagesNotVisible@ , and @ApproximateNumberOfMessagesVisible@ metrics may not achieve consistency until at least 1 minute after the producers stop sending messages. This period is required for the queue metadata to reach eventual consistency.      * @All@ – Returns all values.      * @ApproximateNumberOfMessages@ – Returns the approximate number of messages available for retrieval from the queue.     * @ApproximateNumberOfMessagesDelayed@ – Returns the approximate number of messages in the queue that are delayed and not available for reading immediately. This can happen when the queue is configured as a delay queue or when a message has been sent with a delay parameter.     * @ApproximateNumberOfMessagesNotVisible@ – Returns the approximate number of messages that are in flight. Messages are considered to be /in flight/ if they have been sent to a client but have not yet been deleted or have not yet reached the end of their visibility window.      * @CreatedTimestamp@ – Returns the time when the queue was created in seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time> ).     * @DelaySeconds@ – Returns the default delay on the queue in seconds.     * @LastModifiedTimestamp@ – Returns the time when the queue was last changed in seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time> ).     * @MaximumMessageSize@ – Returns the limit of how many bytes a message can contain before Amazon SQS rejects it.     * @MessageRetentionPeriod@ – Returns the length of time, in seconds, for which Amazon SQS retains a message.     * @Policy@ – Returns the policy of the queue.     * @QueueArn@ – Returns the Amazon resource name (ARN) of the queue.     * @ReceiveMessageWaitTimeSeconds@ – Returns the length of time, in seconds, for which the @ReceiveMessage@ action waits for a message to arrive.      * @RedrivePolicy@ – The string that includes the parameters for the dead-letter queue functionality of the source queue as a JSON object. For more information about the redrive policy and dead-letter queues, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .     * @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the dead-letter queue to which Amazon SQS moves messages after the value of @maxReceiveCount@ is exceeded.     * @maxReceiveCount@ – The number of times a message is delivered to the source queue before being moved to the dead-letter queue. When the @ReceiveCount@ for a message exceeds the @maxReceiveCount@ for a queue, Amazon SQS moves the message to the dead-letter-queue.     * @VisibilityTimeout@ – Returns the visibility timeout for the queue. For more information about the visibility timeout, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .  The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ – Returns the ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> .      * @KmsDataKeyReusePeriodSeconds@ – Returns the length of time, in seconds, for which Amazon SQS can reuse a data key to encrypt or decrypt messages before calling AWS KMS again. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> .  The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :     * @FifoQueue@ – Returns whether the queue is FIFO. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon Simple Queue Service Developer Guide/ .     * @ContentBasedDeduplication@ – Returns whether content-based deduplication is enabled for the queue. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- * 'gqaQueueURL' - The URL of the Amazon SQS queue whose attribute information is retrieved. Queue URLs and names are case-sensitive.
getQueueAttributes ::
  -- | 'gqaQueueURL'
  Text ->
  GetQueueAttributes
getQueueAttributes pQueueURL_ =
  GetQueueAttributes'
    { _gqaAttributeNames = Nothing,
      _gqaQueueURL = pQueueURL_
    }

-- | A list of attributes for which to retrieve information. The following attributes are supported: /Important:/ The @ApproximateNumberOfMessagesDelayed@ , @ApproximateNumberOfMessagesNotVisible@ , and @ApproximateNumberOfMessagesVisible@ metrics may not achieve consistency until at least 1 minute after the producers stop sending messages. This period is required for the queue metadata to reach eventual consistency.      * @All@ – Returns all values.      * @ApproximateNumberOfMessages@ – Returns the approximate number of messages available for retrieval from the queue.     * @ApproximateNumberOfMessagesDelayed@ – Returns the approximate number of messages in the queue that are delayed and not available for reading immediately. This can happen when the queue is configured as a delay queue or when a message has been sent with a delay parameter.     * @ApproximateNumberOfMessagesNotVisible@ – Returns the approximate number of messages that are in flight. Messages are considered to be /in flight/ if they have been sent to a client but have not yet been deleted or have not yet reached the end of their visibility window.      * @CreatedTimestamp@ – Returns the time when the queue was created in seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time> ).     * @DelaySeconds@ – Returns the default delay on the queue in seconds.     * @LastModifiedTimestamp@ – Returns the time when the queue was last changed in seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time> ).     * @MaximumMessageSize@ – Returns the limit of how many bytes a message can contain before Amazon SQS rejects it.     * @MessageRetentionPeriod@ – Returns the length of time, in seconds, for which Amazon SQS retains a message.     * @Policy@ – Returns the policy of the queue.     * @QueueArn@ – Returns the Amazon resource name (ARN) of the queue.     * @ReceiveMessageWaitTimeSeconds@ – Returns the length of time, in seconds, for which the @ReceiveMessage@ action waits for a message to arrive.      * @RedrivePolicy@ – The string that includes the parameters for the dead-letter queue functionality of the source queue as a JSON object. For more information about the redrive policy and dead-letter queues, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .     * @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the dead-letter queue to which Amazon SQS moves messages after the value of @maxReceiveCount@ is exceeded.     * @maxReceiveCount@ – The number of times a message is delivered to the source queue before being moved to the dead-letter queue. When the @ReceiveCount@ for a message exceeds the @maxReceiveCount@ for a queue, Amazon SQS moves the message to the dead-letter-queue.     * @VisibilityTimeout@ – Returns the visibility timeout for the queue. For more information about the visibility timeout, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .  The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ – Returns the ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> .      * @KmsDataKeyReusePeriodSeconds@ – Returns the length of time, in seconds, for which Amazon SQS can reuse a data key to encrypt or decrypt messages before calling AWS KMS again. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> .  The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :     * @FifoQueue@ – Returns whether the queue is FIFO. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon Simple Queue Service Developer Guide/ .     * @ContentBasedDeduplication@ – Returns whether content-based deduplication is enabled for the queue. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .
gqaAttributeNames :: Lens' GetQueueAttributes [QueueAttributeName]
gqaAttributeNames = lens _gqaAttributeNames (\s a -> s {_gqaAttributeNames = a}) . _Default . _Coerce

-- | The URL of the Amazon SQS queue whose attribute information is retrieved. Queue URLs and names are case-sensitive.
gqaQueueURL :: Lens' GetQueueAttributes Text
gqaQueueURL = lens _gqaQueueURL (\s a -> s {_gqaQueueURL = a})

instance AWSRequest GetQueueAttributes where
  type Rs GetQueueAttributes = GetQueueAttributesResponse
  request = postQuery sqs
  response =
    receiveXMLWrapper
      "GetQueueAttributesResult"
      ( \s h x ->
          GetQueueAttributesResponse'
            <$> (may (parseXMLMap "Attribute" "Name" "Value") x)
            <*> (pure (fromEnum s))
      )

instance Hashable GetQueueAttributes

instance NFData GetQueueAttributes

instance ToHeaders GetQueueAttributes where
  toHeaders = const mempty

instance ToPath GetQueueAttributes where
  toPath = const "/"

instance ToQuery GetQueueAttributes where
  toQuery GetQueueAttributes' {..} =
    mconcat
      [ "Action" =: ("GetQueueAttributes" :: ByteString),
        "Version" =: ("2012-11-05" :: ByteString),
        toQuery (toQueryList "AttributeName" <$> _gqaAttributeNames),
        "QueueUrl" =: _gqaQueueURL
      ]

-- | A list of returned queue attributes.
--
--
--
-- /See:/ 'getQueueAttributesResponse' smart constructor.
data GetQueueAttributesResponse = GetQueueAttributesResponse'
  { _gqarsAttributes ::
      !( Maybe
           (Map QueueAttributeName (Text))
       ),
    _gqarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetQueueAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqarsAttributes' - A map of attributes to their respective values.
--
-- * 'gqarsResponseStatus' - -- | The response status code.
getQueueAttributesResponse ::
  -- | 'gqarsResponseStatus'
  Int ->
  GetQueueAttributesResponse
getQueueAttributesResponse pResponseStatus_ =
  GetQueueAttributesResponse'
    { _gqarsAttributes = Nothing,
      _gqarsResponseStatus = pResponseStatus_
    }

-- | A map of attributes to their respective values.
gqarsAttributes :: Lens' GetQueueAttributesResponse (HashMap QueueAttributeName (Text))
gqarsAttributes = lens _gqarsAttributes (\s a -> s {_gqarsAttributes = a}) . _Default . _Map

-- | -- | The response status code.
gqarsResponseStatus :: Lens' GetQueueAttributesResponse Int
gqarsResponseStatus = lens _gqarsResponseStatus (\s a -> s {_gqarsResponseStatus = a})

instance NFData GetQueueAttributesResponse
