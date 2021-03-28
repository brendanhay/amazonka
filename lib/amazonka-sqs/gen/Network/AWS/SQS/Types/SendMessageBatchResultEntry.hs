{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.SendMessageBatchResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.SendMessageBatchResultEntry
  ( SendMessageBatchResultEntry (..)
  -- * Smart constructor
  , mkSendMessageBatchResultEntry
  -- * Lenses
  , smbreId
  , smbreMessageId
  , smbreMD5OfMessageBody
  , smbreMD5OfMessageAttributes
  , smbreMD5OfMessageSystemAttributes
  , smbreSequenceNumber
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Encloses a @MessageId@ for a successfully-enqueued message in a @'SendMessageBatch' .@ 
--
-- /See:/ 'mkSendMessageBatchResultEntry' smart constructor.
data SendMessageBatchResultEntry = SendMessageBatchResultEntry'
  { id :: Core.Text
    -- ^ An identifier for the message in this batch.
  , messageId :: Core.Text
    -- ^ An identifier for the message.
  , mD5OfMessageBody :: Core.Text
    -- ^ An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
  , mD5OfMessageAttributes :: Core.Maybe Core.Text
    -- ^ An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
  , mD5OfMessageSystemAttributes :: Core.Maybe Core.Text
    -- ^ An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
  , sequenceNumber :: Core.Maybe Core.Text
    -- ^ This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each message.
-- The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendMessageBatchResultEntry' value with any optional fields omitted.
mkSendMessageBatchResultEntry
    :: Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'messageId'
    -> Core.Text -- ^ 'mD5OfMessageBody'
    -> SendMessageBatchResultEntry
mkSendMessageBatchResultEntry id messageId mD5OfMessageBody
  = SendMessageBatchResultEntry'{id, messageId, mD5OfMessageBody,
                                 mD5OfMessageAttributes = Core.Nothing,
                                 mD5OfMessageSystemAttributes = Core.Nothing,
                                 sequenceNumber = Core.Nothing}

-- | An identifier for the message in this batch.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreId :: Lens.Lens' SendMessageBatchResultEntry Core.Text
smbreId = Lens.field @"id"
{-# INLINEABLE smbreId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | An identifier for the message.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreMessageId :: Lens.Lens' SendMessageBatchResultEntry Core.Text
smbreMessageId = Lens.field @"messageId"
{-# INLINEABLE smbreMessageId #-}
{-# DEPRECATED messageId "Use generic-lens or generic-optics with 'messageId' instead"  #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'mD5OfMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreMD5OfMessageBody :: Lens.Lens' SendMessageBatchResultEntry Core.Text
smbreMD5OfMessageBody = Lens.field @"mD5OfMessageBody"
{-# INLINEABLE smbreMD5OfMessageBody #-}
{-# DEPRECATED mD5OfMessageBody "Use generic-lens or generic-optics with 'mD5OfMessageBody' instead"  #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'mD5OfMessageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreMD5OfMessageAttributes :: Lens.Lens' SendMessageBatchResultEntry (Core.Maybe Core.Text)
smbreMD5OfMessageAttributes = Lens.field @"mD5OfMessageAttributes"
{-# INLINEABLE smbreMD5OfMessageAttributes #-}
{-# DEPRECATED mD5OfMessageAttributes "Use generic-lens or generic-optics with 'mD5OfMessageAttributes' instead"  #-}

-- | An MD5 digest of the non-URL-encoded message system attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'mD5OfMessageSystemAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreMD5OfMessageSystemAttributes :: Lens.Lens' SendMessageBatchResultEntry (Core.Maybe Core.Text)
smbreMD5OfMessageSystemAttributes = Lens.field @"mD5OfMessageSystemAttributes"
{-# INLINEABLE smbreMD5OfMessageSystemAttributes #-}
{-# DEPRECATED mD5OfMessageSystemAttributes "Use generic-lens or generic-optics with 'mD5OfMessageSystemAttributes' instead"  #-}

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each message.
-- The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbreSequenceNumber :: Lens.Lens' SendMessageBatchResultEntry (Core.Maybe Core.Text)
smbreSequenceNumber = Lens.field @"sequenceNumber"
{-# INLINEABLE smbreSequenceNumber #-}
{-# DEPRECATED sequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead"  #-}

instance Core.FromXML SendMessageBatchResultEntry where
        parseXML x
          = SendMessageBatchResultEntry' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "MessageId" Core.<*>
                x Core..@ "MD5OfMessageBody"
                Core.<*> x Core..@? "MD5OfMessageAttributes"
                Core.<*> x Core..@? "MD5OfMessageSystemAttributes"
                Core.<*> x Core..@? "SequenceNumber"
