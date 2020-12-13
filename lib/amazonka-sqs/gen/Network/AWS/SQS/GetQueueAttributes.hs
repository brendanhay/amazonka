{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    GetQueueAttributes (..),
    mkGetQueueAttributes,

    -- ** Request lenses
    gqaQueueURL,
    gqaAttributeNames,

    -- * Destructuring the response
    GetQueueAttributesResponse (..),
    mkGetQueueAttributesResponse,

    -- ** Response lenses
    gqarsAttributes,
    gqarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkGetQueueAttributes' smart constructor.
data GetQueueAttributes = GetQueueAttributes'
  { -- | The URL of the Amazon SQS queue whose attribute information is retrieved.
    --
    -- Queue URLs and names are case-sensitive.
    queueURL :: Lude.Text,
    -- | A list of attributes for which to retrieve information.
    --
    -- The following attributes are supported:
    -- /Important:/ The @ApproximateNumberOfMessagesDelayed@ , @ApproximateNumberOfMessagesNotVisible@ , and @ApproximateNumberOfMessagesVisible@ metrics may not achieve consistency until at least 1 minute after the producers stop sending messages. This period is required for the queue metadata to reach eventual consistency.
    --
    --     * @All@ – Returns all values.
    --
    --
    --     * @ApproximateNumberOfMessages@ – Returns the approximate number of messages available for retrieval from the queue.
    --
    --
    --     * @ApproximateNumberOfMessagesDelayed@ – Returns the approximate number of messages in the queue that are delayed and not available for reading immediately. This can happen when the queue is configured as a delay queue or when a message has been sent with a delay parameter.
    --
    --
    --     * @ApproximateNumberOfMessagesNotVisible@ – Returns the approximate number of messages that are in flight. Messages are considered to be /in flight/ if they have been sent to a client but have not yet been deleted or have not yet reached the end of their visibility window.
    --
    --
    --     * @CreatedTimestamp@ – Returns the time when the queue was created in seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time> ).
    --
    --
    --     * @DelaySeconds@ – Returns the default delay on the queue in seconds.
    --
    --
    --     * @LastModifiedTimestamp@ – Returns the time when the queue was last changed in seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time> ).
    --
    --
    --     * @MaximumMessageSize@ – Returns the limit of how many bytes a message can contain before Amazon SQS rejects it.
    --
    --
    --     * @MessageRetentionPeriod@ – Returns the length of time, in seconds, for which Amazon SQS retains a message.
    --
    --
    --     * @Policy@ – Returns the policy of the queue.
    --
    --
    --     * @QueueArn@ – Returns the Amazon resource name (ARN) of the queue.
    --
    --
    --     * @ReceiveMessageWaitTimeSeconds@ – Returns the length of time, in seconds, for which the @ReceiveMessage@ action waits for a message to arrive.
    --
    --
    --     * @RedrivePolicy@ – The string that includes the parameters for the dead-letter queue functionality of the source queue as a JSON object. For more information about the redrive policy and dead-letter queues, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .
    --
    --     * @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the dead-letter queue to which Amazon SQS moves messages after the value of @maxReceiveCount@ is exceeded.
    --
    --
    --     * @maxReceiveCount@ – The number of times a message is delivered to the source queue before being moved to the dead-letter queue. When the @ReceiveCount@ for a message exceeds the @maxReceiveCount@ for a queue, Amazon SQS moves the message to the dead-letter-queue.
    --
    --
    --
    --
    --     * @VisibilityTimeout@ – Returns the visibility timeout for the queue. For more information about the visibility timeout, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
    --
    --
    -- The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :
    --
    --     * @KmsMasterKeyId@ – Returns the ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> .
    --
    --
    --     * @KmsDataKeyReusePeriodSeconds@ – Returns the length of time, in seconds, for which Amazon SQS can reuse a data key to encrypt or decrypt messages before calling AWS KMS again. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> .
    --
    --
    -- The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :
    --
    --     * @FifoQueue@ – Returns whether the queue is FIFO. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon Simple Queue Service Developer Guide/ .
    --
    --
    --     * @ContentBasedDeduplication@ – Returns whether content-based deduplication is enabled for the queue. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .
    attributeNames :: Lude.Maybe [QueueAttributeName]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueueAttributes' with the minimum fields required to make a request.
--
-- * 'queueURL' - The URL of the Amazon SQS queue whose attribute information is retrieved.
--
-- Queue URLs and names are case-sensitive.
-- * 'attributeNames' - A list of attributes for which to retrieve information.
--
-- The following attributes are supported:
-- /Important:/ The @ApproximateNumberOfMessagesDelayed@ , @ApproximateNumberOfMessagesNotVisible@ , and @ApproximateNumberOfMessagesVisible@ metrics may not achieve consistency until at least 1 minute after the producers stop sending messages. This period is required for the queue metadata to reach eventual consistency.
--
--     * @All@ – Returns all values.
--
--
--     * @ApproximateNumberOfMessages@ – Returns the approximate number of messages available for retrieval from the queue.
--
--
--     * @ApproximateNumberOfMessagesDelayed@ – Returns the approximate number of messages in the queue that are delayed and not available for reading immediately. This can happen when the queue is configured as a delay queue or when a message has been sent with a delay parameter.
--
--
--     * @ApproximateNumberOfMessagesNotVisible@ – Returns the approximate number of messages that are in flight. Messages are considered to be /in flight/ if they have been sent to a client but have not yet been deleted or have not yet reached the end of their visibility window.
--
--
--     * @CreatedTimestamp@ – Returns the time when the queue was created in seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time> ).
--
--
--     * @DelaySeconds@ – Returns the default delay on the queue in seconds.
--
--
--     * @LastModifiedTimestamp@ – Returns the time when the queue was last changed in seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time> ).
--
--
--     * @MaximumMessageSize@ – Returns the limit of how many bytes a message can contain before Amazon SQS rejects it.
--
--
--     * @MessageRetentionPeriod@ – Returns the length of time, in seconds, for which Amazon SQS retains a message.
--
--
--     * @Policy@ – Returns the policy of the queue.
--
--
--     * @QueueArn@ – Returns the Amazon resource name (ARN) of the queue.
--
--
--     * @ReceiveMessageWaitTimeSeconds@ – Returns the length of time, in seconds, for which the @ReceiveMessage@ action waits for a message to arrive.
--
--
--     * @RedrivePolicy@ – The string that includes the parameters for the dead-letter queue functionality of the source queue as a JSON object. For more information about the redrive policy and dead-letter queues, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
--     * @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the dead-letter queue to which Amazon SQS moves messages after the value of @maxReceiveCount@ is exceeded.
--
--
--     * @maxReceiveCount@ – The number of times a message is delivered to the source queue before being moved to the dead-letter queue. When the @ReceiveCount@ for a message exceeds the @maxReceiveCount@ for a queue, Amazon SQS moves the message to the dead-letter-queue.
--
--
--
--
--     * @VisibilityTimeout@ – Returns the visibility timeout for the queue. For more information about the visibility timeout, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
-- The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :
--
--     * @KmsMasterKeyId@ – Returns the ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> .
--
--
--     * @KmsDataKeyReusePeriodSeconds@ – Returns the length of time, in seconds, for which Amazon SQS can reuse a data key to encrypt or decrypt messages before calling AWS KMS again. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> .
--
--
-- The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :
--
--     * @FifoQueue@ – Returns whether the queue is FIFO. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
--     * @ContentBasedDeduplication@ – Returns whether content-based deduplication is enabled for the queue. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .
mkGetQueueAttributes ::
  -- | 'queueURL'
  Lude.Text ->
  GetQueueAttributes
mkGetQueueAttributes pQueueURL_ =
  GetQueueAttributes'
    { queueURL = pQueueURL_,
      attributeNames = Lude.Nothing
    }

-- | The URL of the Amazon SQS queue whose attribute information is retrieved.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqaQueueURL :: Lens.Lens' GetQueueAttributes Lude.Text
gqaQueueURL = Lens.lens (queueURL :: GetQueueAttributes -> Lude.Text) (\s a -> s {queueURL = a} :: GetQueueAttributes)
{-# DEPRECATED gqaQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

-- | A list of attributes for which to retrieve information.
--
-- The following attributes are supported:
-- /Important:/ The @ApproximateNumberOfMessagesDelayed@ , @ApproximateNumberOfMessagesNotVisible@ , and @ApproximateNumberOfMessagesVisible@ metrics may not achieve consistency until at least 1 minute after the producers stop sending messages. This period is required for the queue metadata to reach eventual consistency.
--
--     * @All@ – Returns all values.
--
--
--     * @ApproximateNumberOfMessages@ – Returns the approximate number of messages available for retrieval from the queue.
--
--
--     * @ApproximateNumberOfMessagesDelayed@ – Returns the approximate number of messages in the queue that are delayed and not available for reading immediately. This can happen when the queue is configured as a delay queue or when a message has been sent with a delay parameter.
--
--
--     * @ApproximateNumberOfMessagesNotVisible@ – Returns the approximate number of messages that are in flight. Messages are considered to be /in flight/ if they have been sent to a client but have not yet been deleted or have not yet reached the end of their visibility window.
--
--
--     * @CreatedTimestamp@ – Returns the time when the queue was created in seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time> ).
--
--
--     * @DelaySeconds@ – Returns the default delay on the queue in seconds.
--
--
--     * @LastModifiedTimestamp@ – Returns the time when the queue was last changed in seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time> ).
--
--
--     * @MaximumMessageSize@ – Returns the limit of how many bytes a message can contain before Amazon SQS rejects it.
--
--
--     * @MessageRetentionPeriod@ – Returns the length of time, in seconds, for which Amazon SQS retains a message.
--
--
--     * @Policy@ – Returns the policy of the queue.
--
--
--     * @QueueArn@ – Returns the Amazon resource name (ARN) of the queue.
--
--
--     * @ReceiveMessageWaitTimeSeconds@ – Returns the length of time, in seconds, for which the @ReceiveMessage@ action waits for a message to arrive.
--
--
--     * @RedrivePolicy@ – The string that includes the parameters for the dead-letter queue functionality of the source queue as a JSON object. For more information about the redrive policy and dead-letter queues, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
--     * @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the dead-letter queue to which Amazon SQS moves messages after the value of @maxReceiveCount@ is exceeded.
--
--
--     * @maxReceiveCount@ – The number of times a message is delivered to the source queue before being moved to the dead-letter queue. When the @ReceiveCount@ for a message exceeds the @maxReceiveCount@ for a queue, Amazon SQS moves the message to the dead-letter-queue.
--
--
--
--
--     * @VisibilityTimeout@ – Returns the visibility timeout for the queue. For more information about the visibility timeout, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
-- The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :
--
--     * @KmsMasterKeyId@ – Returns the ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> .
--
--
--     * @KmsDataKeyReusePeriodSeconds@ – Returns the length of time, in seconds, for which Amazon SQS can reuse a data key to encrypt or decrypt messages before calling AWS KMS again. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> .
--
--
-- The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :
--
--     * @FifoQueue@ – Returns whether the queue is FIFO. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
--     * @ContentBasedDeduplication@ – Returns whether content-based deduplication is enabled for the queue. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqaAttributeNames :: Lens.Lens' GetQueueAttributes (Lude.Maybe [QueueAttributeName])
gqaAttributeNames = Lens.lens (attributeNames :: GetQueueAttributes -> Lude.Maybe [QueueAttributeName]) (\s a -> s {attributeNames = a} :: GetQueueAttributes)
{-# DEPRECATED gqaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

instance Lude.AWSRequest GetQueueAttributes where
  type Rs GetQueueAttributes = GetQueueAttributesResponse
  request = Req.postQuery sqsService
  response =
    Res.receiveXMLWrapper
      "GetQueueAttributesResult"
      ( \s h x ->
          GetQueueAttributesResponse'
            Lude.<$> (Lude.may (Lude.parseXMLMap "Attribute" "Name" "Value") x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetQueueAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetQueueAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetQueueAttributes where
  toQuery GetQueueAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetQueueAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "QueueUrl" Lude.=: queueURL,
        Lude.toQuery
          (Lude.toQueryList "AttributeName" Lude.<$> attributeNames)
      ]

-- | A list of returned queue attributes.
--
-- /See:/ 'mkGetQueueAttributesResponse' smart constructor.
data GetQueueAttributesResponse = GetQueueAttributesResponse'
  { -- | A map of attributes to their respective values.
    attributes :: Lude.Maybe (Lude.HashMap QueueAttributeName (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueueAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - A map of attributes to their respective values.
-- * 'responseStatus' - The response status code.
mkGetQueueAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetQueueAttributesResponse
mkGetQueueAttributesResponse pResponseStatus_ =
  GetQueueAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A map of attributes to their respective values.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqarsAttributes :: Lens.Lens' GetQueueAttributesResponse (Lude.Maybe (Lude.HashMap QueueAttributeName (Lude.Text)))
gqarsAttributes = Lens.lens (attributes :: GetQueueAttributesResponse -> Lude.Maybe (Lude.HashMap QueueAttributeName (Lude.Text))) (\s a -> s {attributes = a} :: GetQueueAttributesResponse)
{-# DEPRECATED gqarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqarsResponseStatus :: Lens.Lens' GetQueueAttributesResponse Lude.Int
gqarsResponseStatus = Lens.lens (responseStatus :: GetQueueAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetQueueAttributesResponse)
{-# DEPRECATED gqarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
