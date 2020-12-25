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
    smQueueUrl,
    smMessageBody,
    smDelaySeconds,
    smMessageAttributes,
    smMessageDeduplicationId,
    smMessageGroupId,
    smMessageSystemAttributes,

    -- * Destructuring the response
    SendMessageResponse (..),
    mkSendMessageResponse,

    -- ** Response lenses
    smrrsMD5OfMessageAttributes,
    smrrsMD5OfMessageBody,
    smrrsMD5OfMessageSystemAttributes,
    smrrsMessageId,
    smrrsSequenceNumber,
    smrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- |
--
-- /See:/ 'mkSendMessage' smart constructor.
data SendMessage = SendMessage'
  { -- | The URL of the Amazon SQS queue to which a message is sent.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Types.String,
    -- | The message to send. The minimum size is one character. The maximum size is 256 KB.
    --
    -- /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:
    -- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@
    -- Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
    messageBody :: Types.String,
    -- | The length of time, in seconds, for which to delay a specific message. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue applies.
    delaySeconds :: Core.Maybe Core.Int,
    -- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
    messageAttributes :: Core.Maybe (Core.HashMap Types.String Types.MessageAttributeValue),
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
    -- | The message system attribute to send. Each message system attribute consists of a @Name@ , @Type@ , and @Value@ .
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

-- | Creates a 'SendMessage' value with any optional fields omitted.
mkSendMessage ::
  -- | 'queueUrl'
  Types.String ->
  -- | 'messageBody'
  Types.String ->
  SendMessage
mkSendMessage queueUrl messageBody =
  SendMessage'
    { queueUrl,
      messageBody,
      delaySeconds = Core.Nothing,
      messageAttributes = Core.Nothing,
      messageDeduplicationId = Core.Nothing,
      messageGroupId = Core.Nothing,
      messageSystemAttributes = Core.Nothing
    }

-- | The URL of the Amazon SQS queue to which a message is sent.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smQueueUrl :: Lens.Lens' SendMessage Types.String
smQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED smQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

-- | The message to send. The minimum size is one character. The maximum size is 256 KB.
--
-- /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@
-- Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smMessageBody :: Lens.Lens' SendMessage Types.String
smMessageBody = Lens.field @"messageBody"
{-# DEPRECATED smMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The length of time, in seconds, for which to delay a specific message. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue applies.
--
-- /Note:/ Consider using 'delaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smDelaySeconds :: Lens.Lens' SendMessage (Core.Maybe Core.Int)
smDelaySeconds = Lens.field @"delaySeconds"
{-# DEPRECATED smDelaySeconds "Use generic-lens or generic-optics with 'delaySeconds' instead." #-}

-- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'messageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smMessageAttributes :: Lens.Lens' SendMessage (Core.Maybe (Core.HashMap Types.String Types.MessageAttributeValue))
smMessageAttributes = Lens.field @"messageAttributes"
{-# DEPRECATED smMessageAttributes "Use generic-lens or generic-optics with 'messageAttributes' instead." #-}

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
smMessageDeduplicationId :: Lens.Lens' SendMessage (Core.Maybe Types.String)
smMessageDeduplicationId = Lens.field @"messageDeduplicationId"
{-# DEPRECATED smMessageDeduplicationId "Use generic-lens or generic-optics with 'messageDeduplicationId' instead." #-}

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
smMessageGroupId :: Lens.Lens' SendMessage (Core.Maybe Types.String)
smMessageGroupId = Lens.field @"messageGroupId"
{-# DEPRECATED smMessageGroupId "Use generic-lens or generic-optics with 'messageGroupId' instead." #-}

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
smMessageSystemAttributes :: Lens.Lens' SendMessage (Core.Maybe (Core.HashMap Types.MessageSystemAttributeNameForSends Types.MessageSystemAttributeValue))
smMessageSystemAttributes = Lens.field @"messageSystemAttributes"
{-# DEPRECATED smMessageSystemAttributes "Use generic-lens or generic-optics with 'messageSystemAttributes' instead." #-}

instance Core.AWSRequest SendMessage where
  type Rs SendMessage = SendMessageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SendMessage")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
                Core.<> (Core.toQueryValue "MessageBody" messageBody)
                Core.<> (Core.toQueryValue "DelaySeconds" Core.<$> delaySeconds)
                Core.<> ( Core.toQueryMap "MessageAttribute" "Name" "Value"
                            Core.<$> messageAttributes
                        )
                Core.<> ( Core.toQueryValue "MessageDeduplicationId"
                            Core.<$> messageDeduplicationId
                        )
                Core.<> (Core.toQueryValue "MessageGroupId" Core.<$> messageGroupId)
                Core.<> ( Core.toQueryMap "MessageSystemAttribute" "Name" "Value"
                            Core.<$> messageSystemAttributes
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "SendMessageResult"
      ( \s h x ->
          SendMessageResponse'
            Core.<$> (x Core..@? "MD5OfMessageAttributes")
            Core.<*> (x Core..@? "MD5OfMessageBody")
            Core.<*> (x Core..@? "MD5OfMessageSystemAttributes")
            Core.<*> (x Core..@? "MessageId")
            Core.<*> (x Core..@? "SequenceNumber")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The @MD5OfMessageBody@ and @MessageId@ elements.
--
-- /See:/ 'mkSendMessageResponse' smart constructor.
data SendMessageResponse = SendMessageResponse'
  { -- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
    mD5OfMessageAttributes :: Core.Maybe Types.String,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
    mD5OfMessageBody :: Core.Maybe Types.String,
    -- | An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest.
    mD5OfMessageSystemAttributes :: Core.Maybe Types.String,
    -- | An attribute containing the @MessageId@ of the message sent to the queue. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
    messageId :: Core.Maybe Types.String,
    -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The large, non-consecutive number that Amazon SQS assigns to each message.
    -- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
    sequenceNumber :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendMessageResponse' value with any optional fields omitted.
mkSendMessageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SendMessageResponse
mkSendMessageResponse responseStatus =
  SendMessageResponse'
    { mD5OfMessageAttributes = Core.Nothing,
      mD5OfMessageBody = Core.Nothing,
      mD5OfMessageSystemAttributes = Core.Nothing,
      messageId = Core.Nothing,
      sequenceNumber = Core.Nothing,
      responseStatus
    }

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'mD5OfMessageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsMD5OfMessageAttributes :: Lens.Lens' SendMessageResponse (Core.Maybe Types.String)
smrrsMD5OfMessageAttributes = Lens.field @"mD5OfMessageAttributes"
{-# DEPRECATED smrrsMD5OfMessageAttributes "Use generic-lens or generic-optics with 'mD5OfMessageAttributes' instead." #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'mD5OfMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsMD5OfMessageBody :: Lens.Lens' SendMessageResponse (Core.Maybe Types.String)
smrrsMD5OfMessageBody = Lens.field @"mD5OfMessageBody"
{-# DEPRECATED smrrsMD5OfMessageBody "Use generic-lens or generic-optics with 'mD5OfMessageBody' instead." #-}

-- | An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest.
--
-- /Note:/ Consider using 'mD5OfMessageSystemAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsMD5OfMessageSystemAttributes :: Lens.Lens' SendMessageResponse (Core.Maybe Types.String)
smrrsMD5OfMessageSystemAttributes = Lens.field @"mD5OfMessageSystemAttributes"
{-# DEPRECATED smrrsMD5OfMessageSystemAttributes "Use generic-lens or generic-optics with 'mD5OfMessageSystemAttributes' instead." #-}

-- | An attribute containing the @MessageId@ of the message sent to the queue. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsMessageId :: Lens.Lens' SendMessageResponse (Core.Maybe Types.String)
smrrsMessageId = Lens.field @"messageId"
{-# DEPRECATED smrrsMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each message.
-- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsSequenceNumber :: Lens.Lens' SendMessageResponse (Core.Maybe Types.String)
smrrsSequenceNumber = Lens.field @"sequenceNumber"
{-# DEPRECATED smrrsSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsResponseStatus :: Lens.Lens' SendMessageResponse Core.Int
smrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED smrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
