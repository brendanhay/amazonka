{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.Message
  ( Message (..)
  -- * Smart constructor
  , mkMessage
  -- * Lenses
  , mAttributes
  , mBody
  , mMD5OfBody
  , mMD5OfMessageAttributes
  , mMessageAttributes
  , mMessageId
  , mReceiptHandle
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SQS.Types.MessageAttribute as Types
import qualified Network.AWS.SQS.Types.MessageAttributeValue as Types

-- | An Amazon SQS message.
--
-- /See:/ 'mkMessage' smart constructor.
data Message = Message'
  { attributes :: Core.Maybe (Core.HashMap Types.MessageAttribute Core.Text)
    -- ^ A map of the attributes requested in @'ReceiveMessage' @ to their respective values. Supported attributes:
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
  , body :: Core.Maybe Core.Text
    -- ^ The message's contents (not URL-encoded).
  , mD5OfBody :: Core.Maybe Core.Text
    -- ^ An MD5 digest of the non-URL-encoded message body string.
  , mD5OfMessageAttributes :: Core.Maybe Core.Text
    -- ^ An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
  , messageAttributes :: Core.Maybe (Core.HashMap Core.Text Types.MessageAttributeValue)
    -- ^ Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
  , messageId :: Core.Maybe Core.Text
    -- ^ A unique identifier for the message. A @MessageId@ is considered unique across all AWS accounts for an extended period of time.
  , receiptHandle :: Core.Maybe Core.Text
    -- ^ An identifier associated with the act of receiving the message. A new receipt handle is returned every time you receive a message. When deleting a message, you provide the last received receipt handle to delete the message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Message' value with any optional fields omitted.
mkMessage
    :: Message
mkMessage
  = Message'{attributes = Core.Nothing, body = Core.Nothing,
             mD5OfBody = Core.Nothing, mD5OfMessageAttributes = Core.Nothing,
             messageAttributes = Core.Nothing, messageId = Core.Nothing,
             receiptHandle = Core.Nothing}

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
mAttributes :: Lens.Lens' Message (Core.Maybe (Core.HashMap Types.MessageAttribute Core.Text))
mAttributes = Lens.field @"attributes"
{-# INLINEABLE mAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The message's contents (not URL-encoded).
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBody :: Lens.Lens' Message (Core.Maybe Core.Text)
mBody = Lens.field @"body"
{-# INLINEABLE mBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | An MD5 digest of the non-URL-encoded message body string.
--
-- /Note:/ Consider using 'mD5OfBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMD5OfBody :: Lens.Lens' Message (Core.Maybe Core.Text)
mMD5OfBody = Lens.field @"mD5OfBody"
{-# INLINEABLE mMD5OfBody #-}
{-# DEPRECATED mD5OfBody "Use generic-lens or generic-optics with 'mD5OfBody' instead"  #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- /Note:/ Consider using 'mD5OfMessageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMD5OfMessageAttributes :: Lens.Lens' Message (Core.Maybe Core.Text)
mMD5OfMessageAttributes = Lens.field @"mD5OfMessageAttributes"
{-# INLINEABLE mMD5OfMessageAttributes #-}
{-# DEPRECATED mD5OfMessageAttributes "Use generic-lens or generic-optics with 'mD5OfMessageAttributes' instead"  #-}

-- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'messageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMessageAttributes :: Lens.Lens' Message (Core.Maybe (Core.HashMap Core.Text Types.MessageAttributeValue))
mMessageAttributes = Lens.field @"messageAttributes"
{-# INLINEABLE mMessageAttributes #-}
{-# DEPRECATED messageAttributes "Use generic-lens or generic-optics with 'messageAttributes' instead"  #-}

-- | A unique identifier for the message. A @MessageId@ is considered unique across all AWS accounts for an extended period of time.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMessageId :: Lens.Lens' Message (Core.Maybe Core.Text)
mMessageId = Lens.field @"messageId"
{-# INLINEABLE mMessageId #-}
{-# DEPRECATED messageId "Use generic-lens or generic-optics with 'messageId' instead"  #-}

-- | An identifier associated with the act of receiving the message. A new receipt handle is returned every time you receive a message. When deleting a message, you provide the last received receipt handle to delete the message.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mReceiptHandle :: Lens.Lens' Message (Core.Maybe Core.Text)
mReceiptHandle = Lens.field @"receiptHandle"
{-# INLINEABLE mReceiptHandle #-}
{-# DEPRECATED receiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead"  #-}

instance Core.FromXML Message where
        parseXML x
          = Message' Core.<$>
              (x Core..@? "Attribute") Core.<*> x Core..@? "Body" Core.<*>
                x Core..@? "MD5OfBody"
                Core.<*> x Core..@? "MD5OfMessageAttributes"
                Core.<*> x Core..@? "MessageAttribute"
                Core.<*> x Core..@? "MessageId"
                Core.<*> x Core..@? "ReceiptHandle"
