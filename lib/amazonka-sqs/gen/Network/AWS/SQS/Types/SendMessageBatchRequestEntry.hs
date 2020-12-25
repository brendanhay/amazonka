{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.SendMessageBatchRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.SendMessageBatchRequestEntry
  ( SendMessageBatchRequestEntry (..),

    -- * Smart constructor
    mkSendMessageBatchRequestEntry,

    -- * Lenses
    sId,
    sMessageBody,
    sDelaySeconds,
    sMessageAttributes,
    sMessageDeduplicationId,
    sMessageGroupId,
    sMessageSystemAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SQS.Types.MessageAttributeValue as Types
import qualified Network.AWS.SQS.Types.MessageSystemAttributeNameForSends as Types
import qualified Network.AWS.SQS.Types.MessageSystemAttributeValue as Types
import qualified Network.AWS.SQS.Types.String as Types

-- | Contains the details of a single Amazon SQS message along with an @Id@ .
--
-- /See:/ 'mkSendMessageBatchRequestEntry' smart constructor.
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry'
  { -- | An identifier for a message in this batch used to communicate the result.
    id :: Types.String,
    -- | The body of the message.
    messageBody :: Types.String,
    -- | The length of time, in seconds, for which a specific message is delayed. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue is applied.
    delaySeconds :: Core.Maybe Core.Int,
    -- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
    messageAttributes :: Core.Maybe (Core.HashMap Types.String Types.MessageAttributeValue),
    -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The token used for deduplication of messages within a 5-minute minimum deduplication interval. If a message with a particular @MessageDeduplicationId@ is sent successfully, subsequent messages with the same @MessageDeduplicationId@ are accepted successfully but aren't delivered. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .
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
    -- The length of @MessageDeduplicationId@ is 128 characters. @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ).
    -- For best practices of using @MessageDeduplicationId@ , see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property> in the /Amazon Simple Queue Service Developer Guide/ .
    messageDeduplicationId :: Core.Maybe Types.String,
    -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). To interleave multiple ordered streams within a single queue, use @MessageGroupId@ values (for example, session data for multiple users). In this scenario, multiple consumers can process the queue, but the session data of each user is processed in a FIFO fashion.
    --
    --     * You must associate a non-empty @MessageGroupId@ with a message. If you don't provide a @MessageGroupId@ , the action fails.
    --
    --
    --     * @ReceiveMessage@ might return messages with multiple @MessageGroupId@ values. For each @MessageGroupId@ , the messages are sorted by time sent. The caller can't specify a @MessageGroupId@ .
    --
    --
    -- The length of @MessageGroupId@ is 128 characters. Valid values: alphanumeric characters and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ .
    -- For best practices of using @MessageGroupId@ , see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagegroupid-property.html Using the MessageGroupId Property> in the /Amazon Simple Queue Service Developer Guide/ .
    -- /Important:/ @MessageGroupId@ is required for FIFO queues. You can't use it for Standard queues.
    messageGroupId :: Core.Maybe Types.String,
    -- | The message system attribute to send Each message system attribute consists of a @Name@ , @Type@ , and @Value@ .
    --
    -- /Important:/
    --     * Currently, the only supported message system attribute is @AWSTraceHeader@ . Its type must be @String@ and its value must be a correctly formatted AWS X-Ray trace header string.
    --
    --
    --     * The size of a message system attribute doesn't count towards the total size of a message.
    messageSystemAttributes :: Core.Maybe (Core.HashMap Types.MessageSystemAttributeNameForSends Types.MessageSystemAttributeValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendMessageBatchRequestEntry' value with any optional fields omitted.
mkSendMessageBatchRequestEntry ::
  -- | 'id'
  Types.String ->
  -- | 'messageBody'
  Types.String ->
  SendMessageBatchRequestEntry
mkSendMessageBatchRequestEntry id messageBody =
  SendMessageBatchRequestEntry'
    { id,
      messageBody,
      delaySeconds = Core.Nothing,
      messageAttributes = Core.Nothing,
      messageDeduplicationId = Core.Nothing,
      messageGroupId = Core.Nothing,
      messageSystemAttributes = Core.Nothing
    }

-- | An identifier for a message in this batch used to communicate the result.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' SendMessageBatchRequestEntry Types.String
sId = Lens.field @"id"
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The body of the message.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessageBody :: Lens.Lens' SendMessageBatchRequestEntry Types.String
sMessageBody = Lens.field @"messageBody"
{-# DEPRECATED sMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The length of time, in seconds, for which a specific message is delayed. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue is applied.
--
-- /Note:/ Consider using 'delaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDelaySeconds :: Lens.Lens' SendMessageBatchRequestEntry (Core.Maybe Core.Int)
sDelaySeconds = Lens.field @"delaySeconds"
{-# DEPRECATED sDelaySeconds "Use generic-lens or generic-optics with 'delaySeconds' instead." #-}

-- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'messageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessageAttributes :: Lens.Lens' SendMessageBatchRequestEntry (Core.Maybe (Core.HashMap Types.String Types.MessageAttributeValue))
sMessageAttributes = Lens.field @"messageAttributes"
{-# DEPRECATED sMessageAttributes "Use generic-lens or generic-optics with 'messageAttributes' instead." #-}

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of messages within a 5-minute minimum deduplication interval. If a message with a particular @MessageDeduplicationId@ is sent successfully, subsequent messages with the same @MessageDeduplicationId@ are accepted successfully but aren't delivered. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .
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
-- The length of @MessageDeduplicationId@ is 128 characters. @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ).
-- For best practices of using @MessageDeduplicationId@ , see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'messageDeduplicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessageDeduplicationId :: Lens.Lens' SendMessageBatchRequestEntry (Core.Maybe Types.String)
sMessageDeduplicationId = Lens.field @"messageDeduplicationId"
{-# DEPRECATED sMessageDeduplicationId "Use generic-lens or generic-optics with 'messageDeduplicationId' instead." #-}

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). To interleave multiple ordered streams within a single queue, use @MessageGroupId@ values (for example, session data for multiple users). In this scenario, multiple consumers can process the queue, but the session data of each user is processed in a FIFO fashion.
--
--     * You must associate a non-empty @MessageGroupId@ with a message. If you don't provide a @MessageGroupId@ , the action fails.
--
--
--     * @ReceiveMessage@ might return messages with multiple @MessageGroupId@ values. For each @MessageGroupId@ , the messages are sorted by time sent. The caller can't specify a @MessageGroupId@ .
--
--
-- The length of @MessageGroupId@ is 128 characters. Valid values: alphanumeric characters and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ .
-- For best practices of using @MessageGroupId@ , see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagegroupid-property.html Using the MessageGroupId Property> in the /Amazon Simple Queue Service Developer Guide/ .
-- /Important:/ @MessageGroupId@ is required for FIFO queues. You can't use it for Standard queues.
--
-- /Note:/ Consider using 'messageGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessageGroupId :: Lens.Lens' SendMessageBatchRequestEntry (Core.Maybe Types.String)
sMessageGroupId = Lens.field @"messageGroupId"
{-# DEPRECATED sMessageGroupId "Use generic-lens or generic-optics with 'messageGroupId' instead." #-}

-- | The message system attribute to send Each message system attribute consists of a @Name@ , @Type@ , and @Value@ .
--
-- /Important:/
--     * Currently, the only supported message system attribute is @AWSTraceHeader@ . Its type must be @String@ and its value must be a correctly formatted AWS X-Ray trace header string.
--
--
--     * The size of a message system attribute doesn't count towards the total size of a message.
--
--
--
-- /Note:/ Consider using 'messageSystemAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessageSystemAttributes :: Lens.Lens' SendMessageBatchRequestEntry (Core.Maybe (Core.HashMap Types.MessageSystemAttributeNameForSends Types.MessageSystemAttributeValue))
sMessageSystemAttributes = Lens.field @"messageSystemAttributes"
{-# DEPRECATED sMessageSystemAttributes "Use generic-lens or generic-optics with 'messageSystemAttributes' instead." #-}
