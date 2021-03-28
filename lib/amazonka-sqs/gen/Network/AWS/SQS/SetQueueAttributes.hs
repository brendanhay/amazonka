{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.SetQueueAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the value of one or more queue attributes. When you change a queue's attributes, the change can take up to 60 seconds for most of the attributes to propagate throughout the Amazon SQS system. Changes made to the @MessageRetentionPeriod@ attribute can take up to 15 minutes.
module Network.AWS.SQS.SetQueueAttributes
    (
    -- * Creating a request
      SetQueueAttributes (..)
    , mkSetQueueAttributes
    -- ** Request lenses
    , sqaQueueUrl
    , sqaAttributes

    -- * Destructuring the response
    , SetQueueAttributesResponse (..)
    , mkSetQueueAttributesResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | 
--
-- /See:/ 'mkSetQueueAttributes' smart constructor.
data SetQueueAttributes = SetQueueAttributes'
  { queueUrl :: Core.Text
    -- ^ The URL of the Amazon SQS queue whose attributes are set.
--
-- Queue URLs and names are case-sensitive.
  , attributes :: Core.HashMap Types.QueueAttributeName Core.Text
    -- ^ A map of attributes to set.
--
-- The following lists the names, descriptions, and values of the special request parameters that the @SetQueueAttributes@ action uses:
--
--     * @DelaySeconds@ – The length of time, in seconds, for which the delivery of all messages in the queue is delayed. Valid values: An integer from 0 to 900 (15 minutes). Default: 0. 
--
--
--     * @MaximumMessageSize@ – The limit of how many bytes a message can contain before Amazon SQS rejects it. Valid values: An integer from 1,024 bytes (1 KiB) up to 262,144 bytes (256 KiB). Default: 262,144 (256 KiB). 
--
--
--     * @MessageRetentionPeriod@ – The length of time, in seconds, for which Amazon SQS retains a message. Valid values: An integer representing seconds, from 60 (1 minute) to 1,209,600 (14 days). Default: 345,600 (4 days). 
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
-- The following attribute applies only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :
--
--     * @ContentBasedDeduplication@ – Enables content-based deduplication. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ . 
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetQueueAttributes' value with any optional fields omitted.
mkSetQueueAttributes
    :: Core.Text -- ^ 'queueUrl'
    -> SetQueueAttributes
mkSetQueueAttributes queueUrl
  = SetQueueAttributes'{queueUrl, attributes = Core.mempty}

-- | The URL of the Amazon SQS queue whose attributes are set.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqaQueueUrl :: Lens.Lens' SetQueueAttributes Core.Text
sqaQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE sqaQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

-- | A map of attributes to set.
--
-- The following lists the names, descriptions, and values of the special request parameters that the @SetQueueAttributes@ action uses:
--
--     * @DelaySeconds@ – The length of time, in seconds, for which the delivery of all messages in the queue is delayed. Valid values: An integer from 0 to 900 (15 minutes). Default: 0. 
--
--
--     * @MaximumMessageSize@ – The limit of how many bytes a message can contain before Amazon SQS rejects it. Valid values: An integer from 1,024 bytes (1 KiB) up to 262,144 bytes (256 KiB). Default: 262,144 (256 KiB). 
--
--
--     * @MessageRetentionPeriod@ – The length of time, in seconds, for which Amazon SQS retains a message. Valid values: An integer representing seconds, from 60 (1 minute) to 1,209,600 (14 days). Default: 345,600 (4 days). 
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
-- The following attribute applies only to <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :
--
--     * @ContentBasedDeduplication@ – Enables content-based deduplication. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ . 
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
sqaAttributes :: Lens.Lens' SetQueueAttributes (Core.HashMap Types.QueueAttributeName Core.Text)
sqaAttributes = Lens.field @"attributes"
{-# INLINEABLE sqaAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.ToQuery SetQueueAttributes where
        toQuery SetQueueAttributes{..}
          = Core.toQueryPair "Action" ("SetQueueAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueUrl" queueUrl
              Core.<> Core.toQueryMap "Attribute" "Name" "Value" attributes

instance Core.ToHeaders SetQueueAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetQueueAttributes where
        type Rs SetQueueAttributes = SetQueueAttributesResponse
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
        parseResponse = Response.receiveNull SetQueueAttributesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetQueueAttributesResponse' smart constructor.
data SetQueueAttributesResponse = SetQueueAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetQueueAttributesResponse' value with any optional fields omitted.
mkSetQueueAttributesResponse
    :: SetQueueAttributesResponse
mkSetQueueAttributesResponse = SetQueueAttributesResponse'
