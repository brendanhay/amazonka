{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.CreateQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new standard or FIFO queue. You can pass one or more attributes in the request. Keep the following in mind:
--
--
--     * If you don't specify the @FifoQueue@ attribute, Amazon SQS creates a standard queue.
--
--
--     * If you don't provide a value for an attribute, the queue is created with the default value for the attribute.
--
--
--     * If you delete a queue, you must wait at least 60 seconds before creating a queue with the same name.
--
--
-- To successfully create a new queue, you must provide a queue name that adheres to the <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html limits related to queues> and is unique within the scope of your queues.
-- To get the queue URL, use the @'GetQueueUrl' @ action. @'GetQueueUrl' @ requires only the @QueueName@ parameter. be aware of existing queue names:
--
--     * If you provide the name of an existing queue along with the exact names and values of all the queue's attributes, @CreateQueue@ returns the queue URL for the existing queue.
--
--
--     * If the queue name, attribute names, or attribute values don't match an existing queue, @CreateQueue@ returns an error.
--
--
-- Some actions take lists of parameters. These lists are specified using the @param.n@ notation. Values of @n@ are integers starting from 1. For example, a parameter list with two elements looks like this:
-- @&AttributeName.1=first@ 
-- @&AttributeName.2=second@ 
module Network.AWS.SQS.CreateQueue
    (
    -- * Creating a request
      CreateQueue (..)
    , mkCreateQueue
    -- ** Request lenses
    , cqQueueName
    , cqAttributes
    , cqTags

    -- * Destructuring the response
    , CreateQueueResponse (..)
    , mkCreateQueueResponse
    -- ** Response lenses
    , cqrrsQueueUrl
    , cqrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | 
--
-- /See:/ 'mkCreateQueue' smart constructor.
data CreateQueue = CreateQueue'
  { queueName :: Core.Text
    -- ^ The name of the new queue. The following limits apply to this name:
--
--
--     * A queue name can have up to 80 characters.
--
--
--     * Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
--
--
--     * A FIFO queue name must end with the @.fifo@ suffix.
--
--
-- Queue URLs and names are case-sensitive.
  , attributes :: Core.Maybe (Core.HashMap Types.QueueAttributeName Core.Text)
    -- ^ A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that the @CreateQueue@ action uses:
--
--     * @DelaySeconds@ – The length of time, in seconds, for which the delivery of all messages in the queue is delayed. Valid values: An integer from 0 to 900 seconds (15 minutes). Default: 0. 
--
--
--     * @MaximumMessageSize@ – The limit of how many bytes a message can contain before Amazon SQS rejects it. Valid values: An integer from 1,024 bytes (1 KiB) to 262,144 bytes (256 KiB). Default: 262,144 (256 KiB). 
--
--
--     * @MessageRetentionPeriod@ – The length of time, in seconds, for which Amazon SQS retains a message. Valid values: An integer from 60 seconds (1 minute) to 1,209,600 seconds (14 days). Default: 345,600 (4 days). 
--
--
--     * @Policy@ – The queue's policy. A valid AWS policy. For more information about policy structure, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html Overview of AWS IAM Policies> in the /Amazon IAM User Guide/ . 
--
--
--     * @ReceiveMessageWaitTimeSeconds@ – The length of time, in seconds, for which a @'ReceiveMessage' @ action waits for a message to arrive. Valid values: An integer from 0 to 20 (seconds). Default: 0. 
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
--     * @VisibilityTimeout@ – The visibility timeout for the queue, in seconds. Valid values: An integer from 0 to 43,200 (12 hours). Default: 30. For more information about the visibility timeout, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
-- The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :
--
--     * @KmsMasterKeyId@ – The ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> . While the alias of the AWS-managed CMK for Amazon SQS is always @alias/aws/sqs@ , the alias of a custom CMK can, for example, be @alias//MyAlias/ @ . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ . 
--
--
--     * @KmsDataKeyReusePeriodSeconds@ – The length of time, in seconds, for which Amazon SQS can reuse a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-keys data key> to encrypt or decrypt messages before calling AWS KMS again. An integer representing seconds, between 60 seconds (1 minute) and 86,400 seconds (24 hours). Default: 300 (5 minutes). A shorter time period provides better security but results in more calls to KMS which might incur charges after Free Tier. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> . 
--
--
-- The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :
--
--     * @FifoQueue@ – Designates a queue as FIFO. Valid values: @true@ , @false@ . If you don't specify the @FifoQueue@ attribute, Amazon SQS creates a standard queue. You can provide this attribute only during queue creation. You can't change it for an existing queue. When you set this attribute, you must also provide the @MessageGroupId@ for your messages explicitly.
-- For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
--     * @ContentBasedDeduplication@ – Enables content-based deduplication. Valid values: @true@ , @false@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ . 
--
--     * Every message must have a unique @MessageDeduplicationId@ ,
--
--     * You may provide a @MessageDeduplicationId@ explicitly.
--
--
--     * If you aren't able to provide a @MessageDeduplicationId@ and you enable @ContentBasedDeduplication@ for your queue, Amazon SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message). 
--
--
--     * If you don't provide a @MessageDeduplicationId@ and the queue doesn't have @ContentBasedDeduplication@ set, the action fails with an error.
--
--
--     * If the queue has @ContentBasedDeduplication@ set, your @MessageDeduplicationId@ overrides the generated one.
--
--
--
--
--     * When @ContentBasedDeduplication@ is in effect, messages with identical content sent within the deduplication interval are treated as duplicates and only one copy of the message is delivered.
--
--
--     * If you send one message with @ContentBasedDeduplication@ enabled and then another message with a @MessageDeduplicationId@ that is the same as the one generated for the first @MessageDeduplicationId@ , the two messages are treated as duplicates and only one copy of the message is delivered. 
--
--
--
--
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ Add cost allocation tags to the specified Amazon SQS queue. For an overview, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- When you use queue tags, keep the following guidelines in mind:
--
--     * Adding more than 50 tags to a queue isn't recommended.
--
--
--     * Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.
--
--
--     * Tags are case-sensitive.
--
--
--     * A new tag with a key identical to that of an existing tag overwrites the existing tag.
--
--
-- For a full list of tag restrictions, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-limits.html#limits-queues Limits Related to Queues> in the /Amazon Simple Queue Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateQueue' value with any optional fields omitted.
mkCreateQueue
    :: Core.Text -- ^ 'queueName'
    -> CreateQueue
mkCreateQueue queueName
  = CreateQueue'{queueName, attributes = Core.Nothing,
                 tags = Core.Nothing}

-- | The name of the new queue. The following limits apply to this name:
--
--
--     * A queue name can have up to 80 characters.
--
--
--     * Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
--
--
--     * A FIFO queue name must end with the @.fifo@ suffix.
--
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqQueueName :: Lens.Lens' CreateQueue Core.Text
cqQueueName = Lens.field @"queueName"
{-# INLINEABLE cqQueueName #-}
{-# DEPRECATED queueName "Use generic-lens or generic-optics with 'queueName' instead"  #-}

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that the @CreateQueue@ action uses:
--
--     * @DelaySeconds@ – The length of time, in seconds, for which the delivery of all messages in the queue is delayed. Valid values: An integer from 0 to 900 seconds (15 minutes). Default: 0. 
--
--
--     * @MaximumMessageSize@ – The limit of how many bytes a message can contain before Amazon SQS rejects it. Valid values: An integer from 1,024 bytes (1 KiB) to 262,144 bytes (256 KiB). Default: 262,144 (256 KiB). 
--
--
--     * @MessageRetentionPeriod@ – The length of time, in seconds, for which Amazon SQS retains a message. Valid values: An integer from 60 seconds (1 minute) to 1,209,600 seconds (14 days). Default: 345,600 (4 days). 
--
--
--     * @Policy@ – The queue's policy. A valid AWS policy. For more information about policy structure, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html Overview of AWS IAM Policies> in the /Amazon IAM User Guide/ . 
--
--
--     * @ReceiveMessageWaitTimeSeconds@ – The length of time, in seconds, for which a @'ReceiveMessage' @ action waits for a message to arrive. Valid values: An integer from 0 to 20 (seconds). Default: 0. 
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
--     * @VisibilityTimeout@ – The visibility timeout for the queue, in seconds. Valid values: An integer from 0 to 43,200 (12 hours). Default: 30. For more information about the visibility timeout, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
-- The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :
--
--     * @KmsMasterKeyId@ – The ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> . While the alias of the AWS-managed CMK for Amazon SQS is always @alias/aws/sqs@ , the alias of a custom CMK can, for example, be @alias//MyAlias/ @ . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ . 
--
--
--     * @KmsDataKeyReusePeriodSeconds@ – The length of time, in seconds, for which Amazon SQS can reuse a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-keys data key> to encrypt or decrypt messages before calling AWS KMS again. An integer representing seconds, between 60 seconds (1 minute) and 86,400 seconds (24 hours). Default: 300 (5 minutes). A shorter time period provides better security but results in more calls to KMS which might incur charges after Free Tier. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> . 
--
--
-- The following attributes apply only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :
--
--     * @FifoQueue@ – Designates a queue as FIFO. Valid values: @true@ , @false@ . If you don't specify the @FifoQueue@ attribute, Amazon SQS creates a standard queue. You can provide this attribute only during queue creation. You can't change it for an existing queue. When you set this attribute, you must also provide the @MessageGroupId@ for your messages explicitly.
-- For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
--     * @ContentBasedDeduplication@ – Enables content-based deduplication. Valid values: @true@ , @false@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ . 
--
--     * Every message must have a unique @MessageDeduplicationId@ ,
--
--     * You may provide a @MessageDeduplicationId@ explicitly.
--
--
--     * If you aren't able to provide a @MessageDeduplicationId@ and you enable @ContentBasedDeduplication@ for your queue, Amazon SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message). 
--
--
--     * If you don't provide a @MessageDeduplicationId@ and the queue doesn't have @ContentBasedDeduplication@ set, the action fails with an error.
--
--
--     * If the queue has @ContentBasedDeduplication@ set, your @MessageDeduplicationId@ overrides the generated one.
--
--
--
--
--     * When @ContentBasedDeduplication@ is in effect, messages with identical content sent within the deduplication interval are treated as duplicates and only one copy of the message is delivered.
--
--
--     * If you send one message with @ContentBasedDeduplication@ enabled and then another message with a @MessageDeduplicationId@ that is the same as the one generated for the first @MessageDeduplicationId@ , the two messages are treated as duplicates and only one copy of the message is delivered. 
--
--
--
--
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqAttributes :: Lens.Lens' CreateQueue (Core.Maybe (Core.HashMap Types.QueueAttributeName Core.Text))
cqAttributes = Lens.field @"attributes"
{-# INLINEABLE cqAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | Add cost allocation tags to the specified Amazon SQS queue. For an overview, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- When you use queue tags, keep the following guidelines in mind:
--
--     * Adding more than 50 tags to a queue isn't recommended.
--
--
--     * Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.
--
--
--     * Tags are case-sensitive.
--
--
--     * A new tag with a key identical to that of an existing tag overwrites the existing tag.
--
--
-- For a full list of tag restrictions, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-limits.html#limits-queues Limits Related to Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqTags :: Lens.Lens' CreateQueue (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cqTags = Lens.field @"tags"
{-# INLINEABLE cqTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateQueue where
        toQuery CreateQueue{..}
          = Core.toQueryPair "Action" ("CreateQueue" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueName" queueName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryMap "Attribute" "Name" "Value")
                attributes
              Core.<>
              Core.maybe Core.mempty (Core.toQueryMap "Tag" "Key" "Value") tags

instance Core.ToHeaders CreateQueue where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateQueue where
        type Rs CreateQueue = CreateQueueResponse
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
          = Response.receiveXMLWrapper "CreateQueueResult"
              (\ s h x ->
                 CreateQueueResponse' Core.<$>
                   (x Core..@? "QueueUrl") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Returns the @QueueUrl@ attribute of the created queue.
--
-- /See:/ 'mkCreateQueueResponse' smart constructor.
data CreateQueueResponse = CreateQueueResponse'
  { queueUrl :: Core.Maybe Core.Text
    -- ^ The URL of the created Amazon SQS queue.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateQueueResponse' value with any optional fields omitted.
mkCreateQueueResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateQueueResponse
mkCreateQueueResponse responseStatus
  = CreateQueueResponse'{queueUrl = Core.Nothing, responseStatus}

-- | The URL of the created Amazon SQS queue.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqrrsQueueUrl :: Lens.Lens' CreateQueueResponse (Core.Maybe Core.Text)
cqrrsQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE cqrrsQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqrrsResponseStatus :: Lens.Lens' CreateQueueResponse Core.Int
cqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
