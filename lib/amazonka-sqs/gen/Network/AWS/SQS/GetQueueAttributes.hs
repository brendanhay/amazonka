{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetQueueAttributes (..)
    , mkGetQueueAttributes
    -- ** Request lenses
    , gqaQueueUrl
    , gqaAttributeNames

    -- * Destructuring the response
    , GetQueueAttributesResponse (..)
    , mkGetQueueAttributesResponse
    -- ** Response lenses
    , gqarrsAttributes
    , gqarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | 
--
-- /See:/ 'mkGetQueueAttributes' smart constructor.
data GetQueueAttributes = GetQueueAttributes'
  { queueUrl :: Core.Text
    -- ^ The URL of the Amazon SQS queue whose attribute information is retrieved.
--
-- Queue URLs and names are case-sensitive.
  , attributeNames :: Core.Maybe [Types.QueueAttributeName]
    -- ^ A list of attributes for which to retrieve information.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetQueueAttributes' value with any optional fields omitted.
mkGetQueueAttributes
    :: Core.Text -- ^ 'queueUrl'
    -> GetQueueAttributes
mkGetQueueAttributes queueUrl
  = GetQueueAttributes'{queueUrl, attributeNames = Core.Nothing}

-- | The URL of the Amazon SQS queue whose attribute information is retrieved.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqaQueueUrl :: Lens.Lens' GetQueueAttributes Core.Text
gqaQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE gqaQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

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
gqaAttributeNames :: Lens.Lens' GetQueueAttributes (Core.Maybe [Types.QueueAttributeName])
gqaAttributeNames = Lens.field @"attributeNames"
{-# INLINEABLE gqaAttributeNames #-}
{-# DEPRECATED attributeNames "Use generic-lens or generic-optics with 'attributeNames' instead"  #-}

instance Core.ToQuery GetQueueAttributes where
        toQuery GetQueueAttributes{..}
          = Core.toQueryPair "Action" ("GetQueueAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueUrl" queueUrl
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AttributeName")
                attributeNames

instance Core.ToHeaders GetQueueAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetQueueAttributes where
        type Rs GetQueueAttributes = GetQueueAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "GetQueueAttributesResult"
              (\ s h x ->
                 GetQueueAttributesResponse' Core.<$>
                   (x Core..@? "Attribute") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A list of returned queue attributes.
--
-- /See:/ 'mkGetQueueAttributesResponse' smart constructor.
data GetQueueAttributesResponse = GetQueueAttributesResponse'
  { attributes :: Core.Maybe (Core.HashMap Types.QueueAttributeName Core.Text)
    -- ^ A map of attributes to their respective values.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetQueueAttributesResponse' value with any optional fields omitted.
mkGetQueueAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetQueueAttributesResponse
mkGetQueueAttributesResponse responseStatus
  = GetQueueAttributesResponse'{attributes = Core.Nothing,
                                responseStatus}

-- | A map of attributes to their respective values.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqarrsAttributes :: Lens.Lens' GetQueueAttributesResponse (Core.Maybe (Core.HashMap Types.QueueAttributeName Core.Text))
gqarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE gqarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqarrsResponseStatus :: Lens.Lens' GetQueueAttributesResponse Core.Int
gqarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gqarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
