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
-- Module      : Network.AWS.SQS.Types.Message
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.Message where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SQS.Types.MessageAttribute
import Network.AWS.SQS.Types.MessageAttributeValue

-- | An Amazon SQS message.
--
-- /See:/ 'newMessage' smart constructor.
data Message = Message'
  { -- | The message\'s contents (not URL-encoded).
    body :: Core.Maybe Core.Text,
    -- | An MD5 digest of the non-URL-encoded message body string.
    mD5OfBody :: Core.Maybe Core.Text,
    -- | A map of the attributes requested in @ ReceiveMessage @ to their
    -- respective values. Supported attributes:
    --
    -- -   @ApproximateReceiveCount@
    --
    -- -   @ApproximateFirstReceiveTimestamp@
    --
    -- -   @MessageDeduplicationId@
    --
    -- -   @MessageGroupId@
    --
    -- -   @SenderId@
    --
    -- -   @SentTimestamp@
    --
    -- -   @SequenceNumber@
    --
    -- @ApproximateFirstReceiveTimestamp@ and @SentTimestamp@ are each returned
    -- as an integer representing the
    -- <http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds.
    attributes :: Core.Maybe (Core.HashMap MessageAttribute Core.Text),
    -- | Each message attribute consists of a @Name@, @Type@, and @Value@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
    -- in the /Amazon Simple Queue Service Developer Guide/.
    messageAttributes :: Core.Maybe (Core.HashMap Core.Text MessageAttributeValue),
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can
    -- use this attribute to verify that Amazon SQS received the message
    -- correctly. Amazon SQS URL-decodes the message before creating the MD5
    -- digest. For information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageAttributes :: Core.Maybe Core.Text,
    -- | An identifier associated with the act of receiving the message. A new
    -- receipt handle is returned every time you receive a message. When
    -- deleting a message, you provide the last received receipt handle to
    -- delete the message.
    receiptHandle :: Core.Maybe Core.Text,
    -- | A unique identifier for the message. A @MessageId@is considered unique
    -- across all AWS accounts for an extended period of time.
    messageId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Message' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'message_body' - The message\'s contents (not URL-encoded).
--
-- 'mD5OfBody', 'message_mD5OfBody' - An MD5 digest of the non-URL-encoded message body string.
--
-- 'attributes', 'message_attributes' - A map of the attributes requested in @ ReceiveMessage @ to their
-- respective values. Supported attributes:
--
-- -   @ApproximateReceiveCount@
--
-- -   @ApproximateFirstReceiveTimestamp@
--
-- -   @MessageDeduplicationId@
--
-- -   @MessageGroupId@
--
-- -   @SenderId@
--
-- -   @SentTimestamp@
--
-- -   @SequenceNumber@
--
-- @ApproximateFirstReceiveTimestamp@ and @SentTimestamp@ are each returned
-- as an integer representing the
-- <http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds.
--
-- 'messageAttributes', 'message_messageAttributes' - Each message attribute consists of a @Name@, @Type@, and @Value@. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- 'mD5OfMessageAttributes', 'message_mD5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
--
-- 'receiptHandle', 'message_receiptHandle' - An identifier associated with the act of receiving the message. A new
-- receipt handle is returned every time you receive a message. When
-- deleting a message, you provide the last received receipt handle to
-- delete the message.
--
-- 'messageId', 'message_messageId' - A unique identifier for the message. A @MessageId@is considered unique
-- across all AWS accounts for an extended period of time.
newMessage ::
  Message
newMessage =
  Message'
    { body = Core.Nothing,
      mD5OfBody = Core.Nothing,
      attributes = Core.Nothing,
      messageAttributes = Core.Nothing,
      mD5OfMessageAttributes = Core.Nothing,
      receiptHandle = Core.Nothing,
      messageId = Core.Nothing
    }

-- | The message\'s contents (not URL-encoded).
message_body :: Lens.Lens' Message (Core.Maybe Core.Text)
message_body = Lens.lens (\Message' {body} -> body) (\s@Message' {} a -> s {body = a} :: Message)

-- | An MD5 digest of the non-URL-encoded message body string.
message_mD5OfBody :: Lens.Lens' Message (Core.Maybe Core.Text)
message_mD5OfBody = Lens.lens (\Message' {mD5OfBody} -> mD5OfBody) (\s@Message' {} a -> s {mD5OfBody = a} :: Message)

-- | A map of the attributes requested in @ ReceiveMessage @ to their
-- respective values. Supported attributes:
--
-- -   @ApproximateReceiveCount@
--
-- -   @ApproximateFirstReceiveTimestamp@
--
-- -   @MessageDeduplicationId@
--
-- -   @MessageGroupId@
--
-- -   @SenderId@
--
-- -   @SentTimestamp@
--
-- -   @SequenceNumber@
--
-- @ApproximateFirstReceiveTimestamp@ and @SentTimestamp@ are each returned
-- as an integer representing the
-- <http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds.
message_attributes :: Lens.Lens' Message (Core.Maybe (Core.HashMap MessageAttribute Core.Text))
message_attributes = Lens.lens (\Message' {attributes} -> attributes) (\s@Message' {} a -> s {attributes = a} :: Message) Core.. Lens.mapping Lens._Coerce

-- | Each message attribute consists of a @Name@, @Type@, and @Value@. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon Simple Queue Service Developer Guide/.
message_messageAttributes :: Lens.Lens' Message (Core.Maybe (Core.HashMap Core.Text MessageAttributeValue))
message_messageAttributes = Lens.lens (\Message' {messageAttributes} -> messageAttributes) (\s@Message' {} a -> s {messageAttributes = a} :: Message) Core.. Lens.mapping Lens._Coerce

-- | An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
message_mD5OfMessageAttributes :: Lens.Lens' Message (Core.Maybe Core.Text)
message_mD5OfMessageAttributes = Lens.lens (\Message' {mD5OfMessageAttributes} -> mD5OfMessageAttributes) (\s@Message' {} a -> s {mD5OfMessageAttributes = a} :: Message)

-- | An identifier associated with the act of receiving the message. A new
-- receipt handle is returned every time you receive a message. When
-- deleting a message, you provide the last received receipt handle to
-- delete the message.
message_receiptHandle :: Lens.Lens' Message (Core.Maybe Core.Text)
message_receiptHandle = Lens.lens (\Message' {receiptHandle} -> receiptHandle) (\s@Message' {} a -> s {receiptHandle = a} :: Message)

-- | A unique identifier for the message. A @MessageId@is considered unique
-- across all AWS accounts for an extended period of time.
message_messageId :: Lens.Lens' Message (Core.Maybe Core.Text)
message_messageId = Lens.lens (\Message' {messageId} -> messageId) (\s@Message' {} a -> s {messageId = a} :: Message)

instance Core.FromXML Message where
  parseXML x =
    Message'
      Core.<$> (x Core..@? "Body")
      Core.<*> (x Core..@? "MD5OfBody")
      Core.<*> ( Core.may
                   (Core.parseXMLMap "Attribute" "Name" "Value")
                   x
               )
      Core.<*> ( Core.may
                   (Core.parseXMLMap "MessageAttribute" "Name" "Value")
                   x
               )
      Core.<*> (x Core..@? "MD5OfMessageAttributes")
      Core.<*> (x Core..@? "ReceiptHandle")
      Core.<*> (x Core..@? "MessageId")

instance Core.Hashable Message

instance Core.NFData Message
