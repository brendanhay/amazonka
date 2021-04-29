{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    sequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | An MD5 digest of the non-URL-encoded message system attribute string.
    -- You can use this attribute to verify that Amazon SQS received the
    -- message correctly. Amazon SQS URL-decodes the message before creating
    -- the MD5 digest. For information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageSystemAttributes :: Prelude.Maybe Prelude.Text,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can
    -- use this attribute to verify that Amazon SQS received the message
    -- correctly. Amazon SQS URL-decodes the message before creating the MD5
    -- digest. For information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageAttributes :: Prelude.Maybe Prelude.Text,
    -- | An identifier for the message in this batch.
    id :: Prelude.Text,
    -- | An identifier for the message.
    messageId :: Prelude.Text,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can
    -- use this attribute to verify that Amazon SQS received the message
    -- correctly. Amazon SQS URL-decodes the message before creating the MD5
    -- digest. For information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'messageId'
  Prelude.Text ->
  -- | 'mD5OfMessageBody'
  Prelude.Text ->
  SendMessageBatchResultEntry
newSendMessageBatchResultEntry
  pId_
  pMessageId_
  pMD5OfMessageBody_ =
    SendMessageBatchResultEntry'
      { sequenceNumber =
          Prelude.Nothing,
        mD5OfMessageSystemAttributes = Prelude.Nothing,
        mD5OfMessageAttributes = Prelude.Nothing,
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
sendMessageBatchResultEntry_sequenceNumber :: Lens.Lens' SendMessageBatchResultEntry (Prelude.Maybe Prelude.Text)
sendMessageBatchResultEntry_sequenceNumber = Lens.lens (\SendMessageBatchResultEntry' {sequenceNumber} -> sequenceNumber) (\s@SendMessageBatchResultEntry' {} a -> s {sequenceNumber = a} :: SendMessageBatchResultEntry)

-- | An MD5 digest of the non-URL-encoded message system attribute string.
-- You can use this attribute to verify that Amazon SQS received the
-- message correctly. Amazon SQS URL-decodes the message before creating
-- the MD5 digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
sendMessageBatchResultEntry_mD5OfMessageSystemAttributes :: Lens.Lens' SendMessageBatchResultEntry (Prelude.Maybe Prelude.Text)
sendMessageBatchResultEntry_mD5OfMessageSystemAttributes = Lens.lens (\SendMessageBatchResultEntry' {mD5OfMessageSystemAttributes} -> mD5OfMessageSystemAttributes) (\s@SendMessageBatchResultEntry' {} a -> s {mD5OfMessageSystemAttributes = a} :: SendMessageBatchResultEntry)

-- | An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
sendMessageBatchResultEntry_mD5OfMessageAttributes :: Lens.Lens' SendMessageBatchResultEntry (Prelude.Maybe Prelude.Text)
sendMessageBatchResultEntry_mD5OfMessageAttributes = Lens.lens (\SendMessageBatchResultEntry' {mD5OfMessageAttributes} -> mD5OfMessageAttributes) (\s@SendMessageBatchResultEntry' {} a -> s {mD5OfMessageAttributes = a} :: SendMessageBatchResultEntry)

-- | An identifier for the message in this batch.
sendMessageBatchResultEntry_id :: Lens.Lens' SendMessageBatchResultEntry Prelude.Text
sendMessageBatchResultEntry_id = Lens.lens (\SendMessageBatchResultEntry' {id} -> id) (\s@SendMessageBatchResultEntry' {} a -> s {id = a} :: SendMessageBatchResultEntry)

-- | An identifier for the message.
sendMessageBatchResultEntry_messageId :: Lens.Lens' SendMessageBatchResultEntry Prelude.Text
sendMessageBatchResultEntry_messageId = Lens.lens (\SendMessageBatchResultEntry' {messageId} -> messageId) (\s@SendMessageBatchResultEntry' {} a -> s {messageId = a} :: SendMessageBatchResultEntry)

-- | An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
sendMessageBatchResultEntry_mD5OfMessageBody :: Lens.Lens' SendMessageBatchResultEntry Prelude.Text
sendMessageBatchResultEntry_mD5OfMessageBody = Lens.lens (\SendMessageBatchResultEntry' {mD5OfMessageBody} -> mD5OfMessageBody) (\s@SendMessageBatchResultEntry' {} a -> s {mD5OfMessageBody = a} :: SendMessageBatchResultEntry)

instance Prelude.FromXML SendMessageBatchResultEntry where
  parseXML x =
    SendMessageBatchResultEntry'
      Prelude.<$> (x Prelude..@? "SequenceNumber")
      Prelude.<*> (x Prelude..@? "MD5OfMessageSystemAttributes")
      Prelude.<*> (x Prelude..@? "MD5OfMessageAttributes")
      Prelude.<*> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "MessageId")
      Prelude.<*> (x Prelude..@ "MD5OfMessageBody")

instance Prelude.Hashable SendMessageBatchResultEntry

instance Prelude.NFData SendMessageBatchResultEntry
