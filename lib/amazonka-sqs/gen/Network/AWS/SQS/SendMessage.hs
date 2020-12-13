{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.SendMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delivers a message to the specified queue.
--
-- /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@
-- Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
module Network.AWS.SQS.SendMessage
  ( -- * Creating a request
    SendMessage (..),
    mkSendMessage,

    -- ** Request lenses
    smMessageAttributes,
    smDelaySeconds,
    smMessageSystemAttributes,
    smQueueURL,
    smMessageDeduplicationId,
    smMessageBody,
    smMessageGroupId,

    -- * Destructuring the response
    SendMessageResponse (..),
    mkSendMessageResponse,

    -- ** Response lenses
    smrsSequenceNumber,
    smrsMD5OfMessageSystemAttributes,
    smrsMessageId,
    smrsMD5OfMessageBody,
    smrsMD5OfMessageAttributes,
    smrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkSendMessage' smart constructor.
data SendMessage = SendMessage'
  { -- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
    messageAttributes :: Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue)),
    -- | The length of time, in seconds, for which to delay a specific message. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue applies.
    delaySeconds :: Lude.Maybe Lude.Int,
    -- | The message system attribute to send. Each message system attribute consists of a @Name@ , @Type@ , and @Value@ .
    --
    -- /Important:/
    --     * Currently, the only supported message system attribute is @AWSTraceHeader@ . Its type must be @String@ and its value must be a correctly formatted AWS X-Ray trace header string.
    --
    --
    --     * The size of a message system attribute doesn't count towards the total size of a message.
    messageSystemAttributes :: Lude.Maybe (Lude.HashMap MessageSystemAttributeNameForSends (MessageSystemAttributeValue)),
    -- | The URL of the Amazon SQS queue to which a message is sent.
    --
    -- Queue URLs and names are case-sensitive.
    queueURL :: Lude.Text,
    -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any messages sent with the same @MessageDeduplicationId@ are accepted successfully but aren't delivered during the 5-minute deduplication interval. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .
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
    -- The maximum length of @MessageDeduplicationId@ is 128 characters. @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ).
    -- For best practices of using @MessageDeduplicationId@ , see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property> in the /Amazon Simple Queue Service Developer Guide/ .
    messageDeduplicationId :: Lude.Maybe Lude.Text,
    -- | The message to send. The minimum size is one character. The maximum size is 256 KB.
    --
    -- /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:
    -- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@
    -- Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
    messageBody :: Lude.Text,
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
    messageGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendMessage' with the minimum fields required to make a request.
--
-- * 'messageAttributes' - Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
-- * 'delaySeconds' - The length of time, in seconds, for which to delay a specific message. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue applies.
-- * 'messageSystemAttributes' - The message system attribute to send. Each message system attribute consists of a @Name@ , @Type@ , and @Value@ .
--
-- /Important:/
--     * Currently, the only supported message system attribute is @AWSTraceHeader@ . Its type must be @String@ and its value must be a correctly formatted AWS X-Ray trace header string.
--
--
--     * The size of a message system attribute doesn't count towards the total size of a message.
--
--
-- * 'queueURL' - The URL of the Amazon SQS queue to which a message is sent.
--
-- Queue URLs and names are case-sensitive.
-- * 'messageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any messages sent with the same @MessageDeduplicationId@ are accepted successfully but aren't delivered during the 5-minute deduplication interval. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .
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
-- The maximum length of @MessageDeduplicationId@ is 128 characters. @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ).
-- For best practices of using @MessageDeduplicationId@ , see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property> in the /Amazon Simple Queue Service Developer Guide/ .
-- * 'messageBody' - The message to send. The minimum size is one character. The maximum size is 256 KB.
--
-- /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@
-- Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
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
mkSendMessage ::
  -- | 'queueURL'
  Lude.Text ->
  -- | 'messageBody'
  Lude.Text ->
  SendMessage
mkSendMessage pQueueURL_ pMessageBody_ =
  SendMessage'
    { messageAttributes = Lude.Nothing,
      delaySeconds = Lude.Nothing,
      messageSystemAttributes = Lude.Nothing,
      queueURL = pQueueURL_,
      messageDeduplicationId = Lude.Nothing,
      messageBody = pMessageBody_,
      messageGroupId = Lude.Nothing
    }

-- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'messageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smMessageAttributes :: Lens.Lens' SendMessage (Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue)))
smMessageAttributes = Lens.lens (messageAttributes :: SendMessage -> Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue))) (\s a -> s {messageAttributes = a} :: SendMessage)
{-# DEPRECATED smMessageAttributes "Use generic-lens or generic-optics with 'messageAttributes' instead." #-}

-- | The length of time, in seconds, for which to delay a specific message. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue applies.
--
-- /Note:/ Consider using 'delaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smDelaySeconds :: Lens.Lens' SendMessage (Lude.Maybe Lude.Int)
smDelaySeconds = Lens.lens (delaySeconds :: SendMessage -> Lude.Maybe Lude.Int) (\s a -> s {delaySeconds = a} :: SendMessage)
{-# DEPRECATED smDelaySeconds "Use generic-lens or generic-optics with 'delaySeconds' instead." #-}

-- | The message system attribute to send. Each message system attribute consists of a @Name@ , @Type@ , and @Value@ .
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
smMessageSystemAttributes :: Lens.Lens' SendMessage (Lude.Maybe (Lude.HashMap MessageSystemAttributeNameForSends (MessageSystemAttributeValue)))
smMessageSystemAttributes = Lens.lens (messageSystemAttributes :: SendMessage -> Lude.Maybe (Lude.HashMap MessageSystemAttributeNameForSends (MessageSystemAttributeValue))) (\s a -> s {messageSystemAttributes = a} :: SendMessage)
{-# DEPRECATED smMessageSystemAttributes "Use generic-lens or generic-optics with 'messageSystemAttributes' instead." #-}

-- | The URL of the Amazon SQS queue to which a message is sent.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smQueueURL :: Lens.Lens' SendMessage Lude.Text
smQueueURL = Lens.lens (queueURL :: SendMessage -> Lude.Text) (\s a -> s {queueURL = a} :: SendMessage)
{-# DEPRECATED smQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any messages sent with the same @MessageDeduplicationId@ are accepted successfully but aren't delivered during the 5-minute deduplication interval. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .
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
-- The maximum length of @MessageDeduplicationId@ is 128 characters. @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ).
-- For best practices of using @MessageDeduplicationId@ , see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'messageDeduplicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smMessageDeduplicationId :: Lens.Lens' SendMessage (Lude.Maybe Lude.Text)
smMessageDeduplicationId = Lens.lens (messageDeduplicationId :: SendMessage -> Lude.Maybe Lude.Text) (\s a -> s {messageDeduplicationId = a} :: SendMessage)
{-# DEPRECATED smMessageDeduplicationId "Use generic-lens or generic-optics with 'messageDeduplicationId' instead." #-}

-- | The message to send. The minimum size is one character. The maximum size is 256 KB.
--
-- /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@
-- Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smMessageBody :: Lens.Lens' SendMessage Lude.Text
smMessageBody = Lens.lens (messageBody :: SendMessage -> Lude.Text) (\s a -> s {messageBody = a} :: SendMessage)
{-# DEPRECATED smMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

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
smMessageGroupId :: Lens.Lens' SendMessage (Lude.Maybe Lude.Text)
smMessageGroupId = Lens.lens (messageGroupId :: SendMessage -> Lude.Maybe Lude.Text) (\s a -> s {messageGroupId = a} :: SendMessage)
{-# DEPRECATED smMessageGroupId "Use generic-lens or generic-optics with 'messageGroupId' instead." #-}

instance Lude.AWSRequest SendMessage where
  type Rs SendMessage = SendMessageResponse
  request = Req.postQuery sqsService
  response =
    Res.receiveXMLWrapper
      "SendMessageResult"
      ( \s h x ->
          SendMessageResponse'
            Lude.<$> (x Lude..@? "SequenceNumber")
            Lude.<*> (x Lude..@? "MD5OfMessageSystemAttributes")
            Lude.<*> (x Lude..@? "MessageId")
            Lude.<*> (x Lude..@? "MD5OfMessageBody")
            Lude.<*> (x Lude..@? "MD5OfMessageAttributes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendMessage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SendMessage where
  toPath = Lude.const "/"

instance Lude.ToQuery SendMessage where
  toQuery SendMessage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SendMessage" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryMap "MessageAttribute" "Name" "Value"
              Lude.<$> messageAttributes
          ),
        "DelaySeconds" Lude.=: delaySeconds,
        Lude.toQuery
          ( Lude.toQueryMap "MessageSystemAttribute" "Name" "Value"
              Lude.<$> messageSystemAttributes
          ),
        "QueueUrl" Lude.=: queueURL,
        "MessageDeduplicationId" Lude.=: messageDeduplicationId,
        "MessageBody" Lude.=: messageBody,
        "MessageGroupId" Lude.=: messageGroupId
      ]

-- | The @MD5OfMessageBody@ and @MessageId@ elements.
--
-- /See:/ 'mkSendMessageResponse' smart constructor.
data SendMessageResponse = SendMessageResponse'
  { -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The large, non-consecutive number that Amazon SQS assigns to each message.
    -- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
    sequenceNumber :: Lude.Maybe Lude.Text,
    -- | An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest.
    md5OfMessageSystemAttributes :: Lude.Maybe Lude.Text,
    -- | An attribute containing the @MessageId@ of the message sent to the queue. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
    messageId :: Lude.Maybe Lude.Text,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
    md5OfMessageBody :: Lude.Maybe Lude.Text,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
    md5OfMessageAttributes :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendMessageResponse' with the minimum fields required to make a request.
--
-- * 'sequenceNumber' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each message.
-- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
-- * 'md5OfMessageSystemAttributes' - An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest.
-- * 'messageId' - An attribute containing the @MessageId@ of the message sent to the queue. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
-- * 'md5OfMessageBody' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
-- * 'md5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
-- * 'responseStatus' - The response status code.
mkSendMessageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendMessageResponse
mkSendMessageResponse pResponseStatus_ =
  SendMessageResponse'
    { sequenceNumber = Lude.Nothing,
      md5OfMessageSystemAttributes = Lude.Nothing,
      messageId = Lude.Nothing,
      md5OfMessageBody = Lude.Nothing,
      md5OfMessageAttributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each message.
-- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsSequenceNumber :: Lens.Lens' SendMessageResponse (Lude.Maybe Lude.Text)
smrsSequenceNumber = Lens.lens (sequenceNumber :: SendMessageResponse -> Lude.Maybe Lude.Text) (\s a -> s {sequenceNumber = a} :: SendMessageResponse)
{-# DEPRECATED smrsSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest.
--
-- /Note:/ Consider using 'md5OfMessageSystemAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsMD5OfMessageSystemAttributes :: Lens.Lens' SendMessageResponse (Lude.Maybe Lude.Text)
smrsMD5OfMessageSystemAttributes = Lens.lens (md5OfMessageSystemAttributes :: SendMessageResponse -> Lude.Maybe Lude.Text) (\s a -> s {md5OfMessageSystemAttributes = a} :: SendMessageResponse)
{-# DEPRECATED smrsMD5OfMessageSystemAttributes "Use generic-lens or generic-optics with 'md5OfMessageSystemAttributes' instead." #-}

-- | An attribute containing the @MessageId@ of the message sent to the queue. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsMessageId :: Lens.Lens' SendMessageResponse (Lude.Maybe Lude.Text)
smrsMessageId = Lens.lens (messageId :: SendMessageResponse -> Lude.Maybe Lude.Text) (\s a -> s {messageId = a} :: SendMessageResponse)
{-# DEPRECATED smrsMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'md5OfMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsMD5OfMessageBody :: Lens.Lens' SendMessageResponse (Lude.Maybe Lude.Text)
smrsMD5OfMessageBody = Lens.lens (md5OfMessageBody :: SendMessageResponse -> Lude.Maybe Lude.Text) (\s a -> s {md5OfMessageBody = a} :: SendMessageResponse)
{-# DEPRECATED smrsMD5OfMessageBody "Use generic-lens or generic-optics with 'md5OfMessageBody' instead." #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'md5OfMessageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsMD5OfMessageAttributes :: Lens.Lens' SendMessageResponse (Lude.Maybe Lude.Text)
smrsMD5OfMessageAttributes = Lens.lens (md5OfMessageAttributes :: SendMessageResponse -> Lude.Maybe Lude.Text) (\s a -> s {md5OfMessageAttributes = a} :: SendMessageResponse)
{-# DEPRECATED smrsMD5OfMessageAttributes "Use generic-lens or generic-optics with 'md5OfMessageAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsResponseStatus :: Lens.Lens' SendMessageResponse Lude.Int
smrsResponseStatus = Lens.lens (responseStatus :: SendMessageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendMessageResponse)
{-# DEPRECATED smrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
