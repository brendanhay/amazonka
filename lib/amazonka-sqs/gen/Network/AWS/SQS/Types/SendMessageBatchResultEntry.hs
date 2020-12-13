{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.SendMessageBatchResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.SendMessageBatchResultEntry
  ( SendMessageBatchResultEntry (..),

    -- * Smart constructor
    mkSendMessageBatchResultEntry,

    -- * Lenses
    smbreSequenceNumber,
    smbreMD5OfMessageSystemAttributes,
    smbreId,
    smbreMessageId,
    smbreMD5OfMessageBody,
    smbreMD5OfMessageAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Encloses a @MessageId@ for a successfully-enqueued message in a @'SendMessageBatch' .@
--
-- /See:/ 'mkSendMessageBatchResultEntry' smart constructor.
data SendMessageBatchResultEntry = SendMessageBatchResultEntry'
  { -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The large, non-consecutive number that Amazon SQS assigns to each message.
    -- The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
    sequenceNumber :: Lude.Maybe Lude.Text,
    -- | An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
    md5OfMessageSystemAttributes :: Lude.Maybe Lude.Text,
    -- | An identifier for the message in this batch.
    id :: Lude.Text,
    -- | An identifier for the message.
    messageId :: Lude.Text,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
    md5OfMessageBody :: Lude.Text,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
    md5OfMessageAttributes :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendMessageBatchResultEntry' with the minimum fields required to make a request.
--
-- * 'sequenceNumber' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each message.
-- The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
-- * 'md5OfMessageSystemAttributes' - An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
-- * 'id' - An identifier for the message in this batch.
-- * 'messageId' - An identifier for the message.
-- * 'md5OfMessageBody' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
-- * 'md5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
mkSendMessageBatchResultEntry ::
  -- | 'id'
  Lude.Text ->
  -- | 'messageId'
  Lude.Text ->
  -- | 'md5OfMessageBody'
  Lude.Text ->
  SendMessageBatchResultEntry
mkSendMessageBatchResultEntry pId_ pMessageId_ pMD5OfMessageBody_ =
  SendMessageBatchResultEntry'
    { sequenceNumber = Lude.Nothing,
      md5OfMessageSystemAttributes = Lude.Nothing,
      id = pId_,
      messageId = pMessageId_,
      md5OfMessageBody = pMD5OfMessageBody_,
      md5OfMessageAttributes = Lude.Nothing
    }

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each message.
-- The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreSequenceNumber :: Lens.Lens' SendMessageBatchResultEntry (Lude.Maybe Lude.Text)
smbreSequenceNumber = Lens.lens (sequenceNumber :: SendMessageBatchResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {sequenceNumber = a} :: SendMessageBatchResultEntry)
{-# DEPRECATED smbreSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'md5OfMessageSystemAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreMD5OfMessageSystemAttributes :: Lens.Lens' SendMessageBatchResultEntry (Lude.Maybe Lude.Text)
smbreMD5OfMessageSystemAttributes = Lens.lens (md5OfMessageSystemAttributes :: SendMessageBatchResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {md5OfMessageSystemAttributes = a} :: SendMessageBatchResultEntry)
{-# DEPRECATED smbreMD5OfMessageSystemAttributes "Use generic-lens or generic-optics with 'md5OfMessageSystemAttributes' instead." #-}

-- | An identifier for the message in this batch.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreId :: Lens.Lens' SendMessageBatchResultEntry Lude.Text
smbreId = Lens.lens (id :: SendMessageBatchResultEntry -> Lude.Text) (\s a -> s {id = a} :: SendMessageBatchResultEntry)
{-# DEPRECATED smbreId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | An identifier for the message.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreMessageId :: Lens.Lens' SendMessageBatchResultEntry Lude.Text
smbreMessageId = Lens.lens (messageId :: SendMessageBatchResultEntry -> Lude.Text) (\s a -> s {messageId = a} :: SendMessageBatchResultEntry)
{-# DEPRECATED smbreMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'md5OfMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreMD5OfMessageBody :: Lens.Lens' SendMessageBatchResultEntry Lude.Text
smbreMD5OfMessageBody = Lens.lens (md5OfMessageBody :: SendMessageBatchResultEntry -> Lude.Text) (\s a -> s {md5OfMessageBody = a} :: SendMessageBatchResultEntry)
{-# DEPRECATED smbreMD5OfMessageBody "Use generic-lens or generic-optics with 'md5OfMessageBody' instead." #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'md5OfMessageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreMD5OfMessageAttributes :: Lens.Lens' SendMessageBatchResultEntry (Lude.Maybe Lude.Text)
smbreMD5OfMessageAttributes = Lens.lens (md5OfMessageAttributes :: SendMessageBatchResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {md5OfMessageAttributes = a} :: SendMessageBatchResultEntry)
{-# DEPRECATED smbreMD5OfMessageAttributes "Use generic-lens or generic-optics with 'md5OfMessageAttributes' instead." #-}

instance Lude.FromXML SendMessageBatchResultEntry where
  parseXML x =
    SendMessageBatchResultEntry'
      Lude.<$> (x Lude..@? "SequenceNumber")
      Lude.<*> (x Lude..@? "MD5OfMessageSystemAttributes")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "MessageId")
      Lude.<*> (x Lude..@ "MD5OfMessageBody")
      Lude.<*> (x Lude..@? "MD5OfMessageAttributes")
