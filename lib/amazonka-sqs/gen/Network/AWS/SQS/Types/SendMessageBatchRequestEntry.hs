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
    sMessageAttributes,
    sDelaySeconds,
    sMessageSystemAttributes,
    sMessageDeduplicationId,
    sMessageGroupId,
    sId,
    sMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SQS.Types.MessageAttributeValue
import Network.AWS.SQS.Types.MessageSystemAttributeNameForSends
import Network.AWS.SQS.Types.MessageSystemAttributeValue

-- | Contains the details of a single Amazon SQS message along with an @Id@ .
--
-- /See:/ 'mkSendMessageBatchRequestEntry' smart constructor.
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry'
  { messageAttributes ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (MessageAttributeValue)
        ),
    delaySeconds ::
      Lude.Maybe Lude.Int,
    messageSystemAttributes ::
      Lude.Maybe
        ( Lude.HashMap
            MessageSystemAttributeNameForSends
            (MessageSystemAttributeValue)
        ),
    messageDeduplicationId ::
      Lude.Maybe Lude.Text,
    messageGroupId ::
      Lude.Maybe Lude.Text,
    id :: Lude.Text,
    messageBody :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendMessageBatchRequestEntry' with the minimum fields required to make a request.
--
-- * 'delaySeconds' - The length of time, in seconds, for which a specific message is delayed. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue is applied.
-- * 'id' - An identifier for a message in this batch used to communicate the result.
-- * 'messageAttributes' - Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
-- * 'messageBody' - The body of the message.
-- * 'messageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) queues.
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
-- * 'messageGroupId' - This parameter applies only to FIFO (first-in-first-out) queues.
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
-- * 'messageSystemAttributes' - The message system attribute to send Each message system attribute consists of a @Name@ , @Type@ , and @Value@ .
--
-- /Important:/
--     * Currently, the only supported message system attribute is @AWSTraceHeader@ . Its type must be @String@ and its value must be a correctly formatted AWS X-Ray trace header string.
--
--
--     * The size of a message system attribute doesn't count towards the total size of a message.
mkSendMessageBatchRequestEntry ::
  -- | 'id'
  Lude.Text ->
  -- | 'messageBody'
  Lude.Text ->
  SendMessageBatchRequestEntry
mkSendMessageBatchRequestEntry pId_ pMessageBody_ =
  SendMessageBatchRequestEntry'
    { messageAttributes = Lude.Nothing,
      delaySeconds = Lude.Nothing,
      messageSystemAttributes = Lude.Nothing,
      messageDeduplicationId = Lude.Nothing,
      messageGroupId = Lude.Nothing,
      id = pId_,
      messageBody = pMessageBody_
    }

-- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'messageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessageAttributes :: Lens.Lens' SendMessageBatchRequestEntry (Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue)))
sMessageAttributes = Lens.lens (messageAttributes :: SendMessageBatchRequestEntry -> Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue))) (\s a -> s {messageAttributes = a} :: SendMessageBatchRequestEntry)
{-# DEPRECATED sMessageAttributes "Use generic-lens or generic-optics with 'messageAttributes' instead." #-}

-- | The length of time, in seconds, for which a specific message is delayed. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue is applied.
--
-- /Note:/ Consider using 'delaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDelaySeconds :: Lens.Lens' SendMessageBatchRequestEntry (Lude.Maybe Lude.Int)
sDelaySeconds = Lens.lens (delaySeconds :: SendMessageBatchRequestEntry -> Lude.Maybe Lude.Int) (\s a -> s {delaySeconds = a} :: SendMessageBatchRequestEntry)
{-# DEPRECATED sDelaySeconds "Use generic-lens or generic-optics with 'delaySeconds' instead." #-}

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
sMessageSystemAttributes :: Lens.Lens' SendMessageBatchRequestEntry (Lude.Maybe (Lude.HashMap MessageSystemAttributeNameForSends (MessageSystemAttributeValue)))
sMessageSystemAttributes = Lens.lens (messageSystemAttributes :: SendMessageBatchRequestEntry -> Lude.Maybe (Lude.HashMap MessageSystemAttributeNameForSends (MessageSystemAttributeValue))) (\s a -> s {messageSystemAttributes = a} :: SendMessageBatchRequestEntry)
{-# DEPRECATED sMessageSystemAttributes "Use generic-lens or generic-optics with 'messageSystemAttributes' instead." #-}

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
sMessageDeduplicationId :: Lens.Lens' SendMessageBatchRequestEntry (Lude.Maybe Lude.Text)
sMessageDeduplicationId = Lens.lens (messageDeduplicationId :: SendMessageBatchRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {messageDeduplicationId = a} :: SendMessageBatchRequestEntry)
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
sMessageGroupId :: Lens.Lens' SendMessageBatchRequestEntry (Lude.Maybe Lude.Text)
sMessageGroupId = Lens.lens (messageGroupId :: SendMessageBatchRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {messageGroupId = a} :: SendMessageBatchRequestEntry)
{-# DEPRECATED sMessageGroupId "Use generic-lens or generic-optics with 'messageGroupId' instead." #-}

-- | An identifier for a message in this batch used to communicate the result.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' SendMessageBatchRequestEntry Lude.Text
sId = Lens.lens (id :: SendMessageBatchRequestEntry -> Lude.Text) (\s a -> s {id = a} :: SendMessageBatchRequestEntry)
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The body of the message.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessageBody :: Lens.Lens' SendMessageBatchRequestEntry Lude.Text
sMessageBody = Lens.lens (messageBody :: SendMessageBatchRequestEntry -> Lude.Text) (\s a -> s {messageBody = a} :: SendMessageBatchRequestEntry)
{-# DEPRECATED sMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

instance Lude.ToQuery SendMessageBatchRequestEntry where
  toQuery SendMessageBatchRequestEntry' {..} =
    Lude.mconcat
      [ Lude.toQuery
          ( Lude.toQueryMap "MessageAttribute" "Name" "Value"
              Lude.<$> messageAttributes
          ),
        "DelaySeconds" Lude.=: delaySeconds,
        Lude.toQuery
          ( Lude.toQueryMap "MessageSystemAttribute" "Name" "Value"
              Lude.<$> messageSystemAttributes
          ),
        "MessageDeduplicationId" Lude.=: messageDeduplicationId,
        "MessageGroupId" Lude.=: messageGroupId,
        "Id" Lude.=: id,
        "MessageBody" Lude.=: messageBody
      ]
