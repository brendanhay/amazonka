-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.Message
  ( Message (..),

    -- * Smart constructor
    mkMessage,

    -- * Lenses
    mMessageAttributes,
    mMD5OfBody,
    mBody,
    mAttributes,
    mReceiptHandle,
    mMessageId,
    mMD5OfMessageAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SQS.Types.MessageAttribute
import Network.AWS.SQS.Types.MessageAttributeValue

-- | An Amazon SQS message.
--
-- /See:/ 'mkMessage' smart constructor.
data Message = Message'
  { messageAttributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue)),
    md5OfBody :: Lude.Maybe Lude.Text,
    body :: Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe (Lude.HashMap MessageAttribute (Lude.Text)),
    receiptHandle :: Lude.Maybe Lude.Text,
    messageId :: Lude.Maybe Lude.Text,
    md5OfMessageAttributes :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- * 'attributes' - A map of the attributes requested in @'ReceiveMessage' @ to their respective values. Supported attributes:
--
--
--     * @ApproximateReceiveCount@
--
--
--     * @ApproximateFirstReceiveTimestamp@
--
--
--     * @MessageDeduplicationId@
--
--
--     * @MessageGroupId@
--
--
--     * @SenderId@
--
--
--     * @SentTimestamp@
--
--
--     * @SequenceNumber@
--
--
-- @ApproximateFirstReceiveTimestamp@ and @SentTimestamp@ are each returned as an integer representing the <http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds.
-- * 'body' - The message's contents (not URL-encoded).
-- * 'md5OfBody' - An MD5 digest of the non-URL-encoded message body string.
-- * 'md5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
-- * 'messageAttributes' - Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
-- * 'messageId' - A unique identifier for the message. A @MessageId@ is considered unique across all AWS accounts for an extended period of time.
-- * 'receiptHandle' - An identifier associated with the act of receiving the message. A new receipt handle is returned every time you receive a message. When deleting a message, you provide the last received receipt handle to delete the message.
mkMessage ::
  Message
mkMessage =
  Message'
    { messageAttributes = Lude.Nothing,
      md5OfBody = Lude.Nothing,
      body = Lude.Nothing,
      attributes = Lude.Nothing,
      receiptHandle = Lude.Nothing,
      messageId = Lude.Nothing,
      md5OfMessageAttributes = Lude.Nothing
    }

-- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'messageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMessageAttributes :: Lens.Lens' Message (Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue)))
mMessageAttributes = Lens.lens (messageAttributes :: Message -> Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue))) (\s a -> s {messageAttributes = a} :: Message)
{-# DEPRECATED mMessageAttributes "Use generic-lens or generic-optics with 'messageAttributes' instead." #-}

-- | An MD5 digest of the non-URL-encoded message body string.
--
-- /Note:/ Consider using 'md5OfBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMD5OfBody :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mMD5OfBody = Lens.lens (md5OfBody :: Message -> Lude.Maybe Lude.Text) (\s a -> s {md5OfBody = a} :: Message)
{-# DEPRECATED mMD5OfBody "Use generic-lens or generic-optics with 'md5OfBody' instead." #-}

-- | The message's contents (not URL-encoded).
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBody :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mBody = Lens.lens (body :: Message -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: Message)
{-# DEPRECATED mBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | A map of the attributes requested in @'ReceiveMessage' @ to their respective values. Supported attributes:
--
--
--     * @ApproximateReceiveCount@
--
--
--     * @ApproximateFirstReceiveTimestamp@
--
--
--     * @MessageDeduplicationId@
--
--
--     * @MessageGroupId@
--
--
--     * @SenderId@
--
--
--     * @SentTimestamp@
--
--
--     * @SequenceNumber@
--
--
-- @ApproximateFirstReceiveTimestamp@ and @SentTimestamp@ are each returned as an integer representing the <http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAttributes :: Lens.Lens' Message (Lude.Maybe (Lude.HashMap MessageAttribute (Lude.Text)))
mAttributes = Lens.lens (attributes :: Message -> Lude.Maybe (Lude.HashMap MessageAttribute (Lude.Text))) (\s a -> s {attributes = a} :: Message)
{-# DEPRECATED mAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | An identifier associated with the act of receiving the message. A new receipt handle is returned every time you receive a message. When deleting a message, you provide the last received receipt handle to delete the message.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mReceiptHandle :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mReceiptHandle = Lens.lens (receiptHandle :: Message -> Lude.Maybe Lude.Text) (\s a -> s {receiptHandle = a} :: Message)
{-# DEPRECATED mReceiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead." #-}

-- | A unique identifier for the message. A @MessageId@ is considered unique across all AWS accounts for an extended period of time.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMessageId :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mMessageId = Lens.lens (messageId :: Message -> Lude.Maybe Lude.Text) (\s a -> s {messageId = a} :: Message)
{-# DEPRECATED mMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'md5OfMessageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMD5OfMessageAttributes :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mMD5OfMessageAttributes = Lens.lens (md5OfMessageAttributes :: Message -> Lude.Maybe Lude.Text) (\s a -> s {md5OfMessageAttributes = a} :: Message)
{-# DEPRECATED mMD5OfMessageAttributes "Use generic-lens or generic-optics with 'md5OfMessageAttributes' instead." #-}

instance Lude.FromXML Message where
  parseXML x =
    Message'
      Lude.<$> (Lude.may (Lude.parseXMLMap "MessageAttribute" "Name" "Value") x)
      Lude.<*> (x Lude..@? "MD5OfBody")
      Lude.<*> (x Lude..@? "Body")
      Lude.<*> (Lude.may (Lude.parseXMLMap "Attribute" "Name" "Value") x)
      Lude.<*> (x Lude..@? "ReceiptHandle")
      Lude.<*> (x Lude..@? "MessageId")
      Lude.<*> (x Lude..@? "MD5OfMessageAttributes")
