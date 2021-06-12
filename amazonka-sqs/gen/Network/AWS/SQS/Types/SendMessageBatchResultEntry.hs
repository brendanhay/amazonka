{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.SendMessageBatchResultEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.SendMessageBatchResultEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Encloses a @MessageId@ for a successfully-enqueued message in a
-- @ SendMessageBatch.@
--
-- /See:/ 'newSendMessageBatchResultEntry' smart constructor.
data SendMessageBatchResultEntry = SendMessageBatchResultEntry'
  { -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The large, non-consecutive number that Amazon SQS assigns to each
    -- message.
    --
    -- The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@
    -- continues to increase for a particular @MessageGroupId@.
    sequenceNumber :: Core.Maybe Core.Text,
    -- | An MD5 digest of the non-URL-encoded message system attribute string.
    -- You can use this attribute to verify that Amazon SQS received the
    -- message correctly. Amazon SQS URL-decodes the message before creating
    -- the MD5 digest. For information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageSystemAttributes :: Core.Maybe Core.Text,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can
    -- use this attribute to verify that Amazon SQS received the message
    -- correctly. Amazon SQS URL-decodes the message before creating the MD5
    -- digest. For information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageAttributes :: Core.Maybe Core.Text,
    -- | An identifier for the message in this batch.
    id :: Core.Text,
    -- | An identifier for the message.
    messageId :: Core.Text,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can
    -- use this attribute to verify that Amazon SQS received the message
    -- correctly. Amazon SQS URL-decodes the message before creating the MD5
    -- digest. For information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageBody :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SendMessageBatchResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceNumber', 'sendMessageBatchResultEntry_sequenceNumber' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each
-- message.
--
-- The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@
-- continues to increase for a particular @MessageGroupId@.
--
-- 'mD5OfMessageSystemAttributes', 'sendMessageBatchResultEntry_mD5OfMessageSystemAttributes' - An MD5 digest of the non-URL-encoded message system attribute string.
-- You can use this attribute to verify that Amazon SQS received the
-- message correctly. Amazon SQS URL-decodes the message before creating
-- the MD5 digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
--
-- 'mD5OfMessageAttributes', 'sendMessageBatchResultEntry_mD5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
--
-- 'id', 'sendMessageBatchResultEntry_id' - An identifier for the message in this batch.
--
-- 'messageId', 'sendMessageBatchResultEntry_messageId' - An identifier for the message.
--
-- 'mD5OfMessageBody', 'sendMessageBatchResultEntry_mD5OfMessageBody' - An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
newSendMessageBatchResultEntry ::
  -- | 'id'
  Core.Text ->
  -- | 'messageId'
  Core.Text ->
  -- | 'mD5OfMessageBody'
  Core.Text ->
  SendMessageBatchResultEntry
newSendMessageBatchResultEntry
  pId_
  pMessageId_
  pMD5OfMessageBody_ =
    SendMessageBatchResultEntry'
      { sequenceNumber =
          Core.Nothing,
        mD5OfMessageSystemAttributes = Core.Nothing,
        mD5OfMessageAttributes = Core.Nothing,
        id = pId_,
        messageId = pMessageId_,
        mD5OfMessageBody = pMD5OfMessageBody_
      }

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each
-- message.
--
-- The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@
-- continues to increase for a particular @MessageGroupId@.
sendMessageBatchResultEntry_sequenceNumber :: Lens.Lens' SendMessageBatchResultEntry (Core.Maybe Core.Text)
sendMessageBatchResultEntry_sequenceNumber = Lens.lens (\SendMessageBatchResultEntry' {sequenceNumber} -> sequenceNumber) (\s@SendMessageBatchResultEntry' {} a -> s {sequenceNumber = a} :: SendMessageBatchResultEntry)

-- | An MD5 digest of the non-URL-encoded message system attribute string.
-- You can use this attribute to verify that Amazon SQS received the
-- message correctly. Amazon SQS URL-decodes the message before creating
-- the MD5 digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
sendMessageBatchResultEntry_mD5OfMessageSystemAttributes :: Lens.Lens' SendMessageBatchResultEntry (Core.Maybe Core.Text)
sendMessageBatchResultEntry_mD5OfMessageSystemAttributes = Lens.lens (\SendMessageBatchResultEntry' {mD5OfMessageSystemAttributes} -> mD5OfMessageSystemAttributes) (\s@SendMessageBatchResultEntry' {} a -> s {mD5OfMessageSystemAttributes = a} :: SendMessageBatchResultEntry)

-- | An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
sendMessageBatchResultEntry_mD5OfMessageAttributes :: Lens.Lens' SendMessageBatchResultEntry (Core.Maybe Core.Text)
sendMessageBatchResultEntry_mD5OfMessageAttributes = Lens.lens (\SendMessageBatchResultEntry' {mD5OfMessageAttributes} -> mD5OfMessageAttributes) (\s@SendMessageBatchResultEntry' {} a -> s {mD5OfMessageAttributes = a} :: SendMessageBatchResultEntry)

-- | An identifier for the message in this batch.
sendMessageBatchResultEntry_id :: Lens.Lens' SendMessageBatchResultEntry Core.Text
sendMessageBatchResultEntry_id = Lens.lens (\SendMessageBatchResultEntry' {id} -> id) (\s@SendMessageBatchResultEntry' {} a -> s {id = a} :: SendMessageBatchResultEntry)

-- | An identifier for the message.
sendMessageBatchResultEntry_messageId :: Lens.Lens' SendMessageBatchResultEntry Core.Text
sendMessageBatchResultEntry_messageId = Lens.lens (\SendMessageBatchResultEntry' {messageId} -> messageId) (\s@SendMessageBatchResultEntry' {} a -> s {messageId = a} :: SendMessageBatchResultEntry)

-- | An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
sendMessageBatchResultEntry_mD5OfMessageBody :: Lens.Lens' SendMessageBatchResultEntry Core.Text
sendMessageBatchResultEntry_mD5OfMessageBody = Lens.lens (\SendMessageBatchResultEntry' {mD5OfMessageBody} -> mD5OfMessageBody) (\s@SendMessageBatchResultEntry' {} a -> s {mD5OfMessageBody = a} :: SendMessageBatchResultEntry)

instance Core.FromXML SendMessageBatchResultEntry where
  parseXML x =
    SendMessageBatchResultEntry'
      Core.<$> (x Core..@? "SequenceNumber")
      Core.<*> (x Core..@? "MD5OfMessageSystemAttributes")
      Core.<*> (x Core..@? "MD5OfMessageAttributes")
      Core.<*> (x Core..@ "Id")
      Core.<*> (x Core..@ "MessageId")
      Core.<*> (x Core..@ "MD5OfMessageBody")

instance Core.Hashable SendMessageBatchResultEntry

instance Core.NFData SendMessageBatchResultEntry
