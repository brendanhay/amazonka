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
-- Module      : Network.AWS.SQS.Types.SendMessageBatchRequestEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.SendMessageBatchRequestEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SQS.Types.MessageAttributeValue
import Network.AWS.SQS.Types.MessageSystemAttributeNameForSends
import Network.AWS.SQS.Types.MessageSystemAttributeValue

-- | Contains the details of a single Amazon SQS message along with an @Id@.
--
-- /See:/ 'newSendMessageBatchRequestEntry' smart constructor.
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry'
  { -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The token used for deduplication of messages within a 5-minute minimum
    -- deduplication interval. If a message with a particular
    -- @MessageDeduplicationId@ is sent successfully, subsequent messages with
    -- the same @MessageDeduplicationId@ are accepted successfully but aren\'t
    -- delivered. For more information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing>
    -- in the /Amazon Simple Queue Service Developer Guide/.
    --
    -- -   Every message must have a unique @MessageDeduplicationId@,
    --
    --     -   You may provide a @MessageDeduplicationId@ explicitly.
    --
    --     -   If you aren\'t able to provide a @MessageDeduplicationId@ and
    --         you enable @ContentBasedDeduplication@ for your queue, Amazon
    --         SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@
    --         using the body of the message (but not the attributes of the
    --         message).
    --
    --     -   If you don\'t provide a @MessageDeduplicationId@ and the queue
    --         doesn\'t have @ContentBasedDeduplication@ set, the action fails
    --         with an error.
    --
    --     -   If the queue has @ContentBasedDeduplication@ set, your
    --         @MessageDeduplicationId@ overrides the generated one.
    --
    -- -   When @ContentBasedDeduplication@ is in effect, messages with
    --     identical content sent within the deduplication interval are treated
    --     as duplicates and only one copy of the message is delivered.
    --
    -- -   If you send one message with @ContentBasedDeduplication@ enabled and
    --     then another message with a @MessageDeduplicationId@ that is the
    --     same as the one generated for the first @MessageDeduplicationId@,
    --     the two messages are treated as duplicates and only one copy of the
    --     message is delivered.
    --
    -- The @MessageDeduplicationId@ is available to the consumer of the message
    -- (this can be useful for troubleshooting delivery issues).
    --
    -- If a message is sent successfully but the acknowledgement is lost and
    -- the message is resent with the same @MessageDeduplicationId@ after the
    -- deduplication interval, Amazon SQS can\'t detect duplicate messages.
    --
    -- Amazon SQS continues to keep track of the message deduplication ID even
    -- after the message is received and deleted.
    --
    -- The length of @MessageDeduplicationId@ is 128 characters.
    -- @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@,
    -- @A-Z@, @0-9@) and punctuation
    -- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
    --
    -- For best practices of using @MessageDeduplicationId@, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property>
    -- in the /Amazon Simple Queue Service Developer Guide/.
    messageDeduplicationId :: Prelude.Maybe Prelude.Text,
    -- | Each message attribute consists of a @Name@, @Type@, and @Value@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
    -- in the /Amazon Simple Queue Service Developer Guide/.
    messageAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue),
    -- | The message system attribute to send Each message system attribute
    -- consists of a @Name@, @Type@, and @Value@.
    --
    -- -   Currently, the only supported message system attribute is
    --     @AWSTraceHeader@. Its type must be @String@ and its value must be a
    --     correctly formatted AWS X-Ray trace header string.
    --
    -- -   The size of a message system attribute doesn\'t count towards the
    --     total size of a message.
    messageSystemAttributes :: Prelude.Maybe (Prelude.HashMap MessageSystemAttributeNameForSends MessageSystemAttributeValue),
    -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The tag that specifies that a message belongs to a specific message
    -- group. Messages that belong to the same message group are processed in a
    -- FIFO manner (however, messages in different message groups might be
    -- processed out of order). To interleave multiple ordered streams within a
    -- single queue, use @MessageGroupId@ values (for example, session data for
    -- multiple users). In this scenario, multiple consumers can process the
    -- queue, but the session data of each user is processed in a FIFO fashion.
    --
    -- -   You must associate a non-empty @MessageGroupId@ with a message. If
    --     you don\'t provide a @MessageGroupId@, the action fails.
    --
    -- -   @ReceiveMessage@ might return messages with multiple
    --     @MessageGroupId@ values. For each @MessageGroupId@, the messages are
    --     sorted by time sent. The caller can\'t specify a @MessageGroupId@.
    --
    -- The length of @MessageGroupId@ is 128 characters. Valid values:
    -- alphanumeric characters and punctuation
    -- @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
    --
    -- For best practices of using @MessageGroupId@, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagegroupid-property.html Using the MessageGroupId Property>
    -- in the /Amazon Simple Queue Service Developer Guide/.
    --
    -- @MessageGroupId@ is required for FIFO queues. You can\'t use it for
    -- Standard queues.
    messageGroupId :: Prelude.Maybe Prelude.Text,
    -- | The length of time, in seconds, for which a specific message is delayed.
    -- Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive
    -- @DelaySeconds@ value become available for processing after the delay
    -- period is finished. If you don\'t specify a value, the default value for
    -- the queue is applied.
    --
    -- When you set @FifoQueue@, you can\'t set @DelaySeconds@ per message. You
    -- can set this parameter only on a queue level.
    delaySeconds :: Prelude.Maybe Prelude.Int,
    -- | An identifier for a message in this batch used to communicate the
    -- result.
    --
    -- The @Id@s of a batch request need to be unique within a request.
    --
    -- This identifier can have up to 80 characters. The following characters
    -- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
    id :: Prelude.Text,
    -- | The body of the message.
    messageBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendMessageBatchRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageDeduplicationId', 'sendMessageBatchRequestEntry_messageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of messages within a 5-minute minimum
-- deduplication interval. If a message with a particular
-- @MessageDeduplicationId@ is sent successfully, subsequent messages with
-- the same @MessageDeduplicationId@ are accepted successfully but aren\'t
-- delivered. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- -   Every message must have a unique @MessageDeduplicationId@,
--
--     -   You may provide a @MessageDeduplicationId@ explicitly.
--
--     -   If you aren\'t able to provide a @MessageDeduplicationId@ and
--         you enable @ContentBasedDeduplication@ for your queue, Amazon
--         SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@
--         using the body of the message (but not the attributes of the
--         message).
--
--     -   If you don\'t provide a @MessageDeduplicationId@ and the queue
--         doesn\'t have @ContentBasedDeduplication@ set, the action fails
--         with an error.
--
--     -   If the queue has @ContentBasedDeduplication@ set, your
--         @MessageDeduplicationId@ overrides the generated one.
--
-- -   When @ContentBasedDeduplication@ is in effect, messages with
--     identical content sent within the deduplication interval are treated
--     as duplicates and only one copy of the message is delivered.
--
-- -   If you send one message with @ContentBasedDeduplication@ enabled and
--     then another message with a @MessageDeduplicationId@ that is the
--     same as the one generated for the first @MessageDeduplicationId@,
--     the two messages are treated as duplicates and only one copy of the
--     message is delivered.
--
-- The @MessageDeduplicationId@ is available to the consumer of the message
-- (this can be useful for troubleshooting delivery issues).
--
-- If a message is sent successfully but the acknowledgement is lost and
-- the message is resent with the same @MessageDeduplicationId@ after the
-- deduplication interval, Amazon SQS can\'t detect duplicate messages.
--
-- Amazon SQS continues to keep track of the message deduplication ID even
-- after the message is received and deleted.
--
-- The length of @MessageDeduplicationId@ is 128 characters.
-- @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@,
-- @A-Z@, @0-9@) and punctuation
-- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
--
-- For best practices of using @MessageDeduplicationId@, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- 'messageAttributes', 'sendMessageBatchRequestEntry_messageAttributes' - Each message attribute consists of a @Name@, @Type@, and @Value@. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- 'messageSystemAttributes', 'sendMessageBatchRequestEntry_messageSystemAttributes' - The message system attribute to send Each message system attribute
-- consists of a @Name@, @Type@, and @Value@.
--
-- -   Currently, the only supported message system attribute is
--     @AWSTraceHeader@. Its type must be @String@ and its value must be a
--     correctly formatted AWS X-Ray trace header string.
--
-- -   The size of a message system attribute doesn\'t count towards the
--     total size of a message.
--
-- 'messageGroupId', 'sendMessageBatchRequestEntry_messageGroupId' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The tag that specifies that a message belongs to a specific message
-- group. Messages that belong to the same message group are processed in a
-- FIFO manner (however, messages in different message groups might be
-- processed out of order). To interleave multiple ordered streams within a
-- single queue, use @MessageGroupId@ values (for example, session data for
-- multiple users). In this scenario, multiple consumers can process the
-- queue, but the session data of each user is processed in a FIFO fashion.
--
-- -   You must associate a non-empty @MessageGroupId@ with a message. If
--     you don\'t provide a @MessageGroupId@, the action fails.
--
-- -   @ReceiveMessage@ might return messages with multiple
--     @MessageGroupId@ values. For each @MessageGroupId@, the messages are
--     sorted by time sent. The caller can\'t specify a @MessageGroupId@.
--
-- The length of @MessageGroupId@ is 128 characters. Valid values:
-- alphanumeric characters and punctuation
-- @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
--
-- For best practices of using @MessageGroupId@, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagegroupid-property.html Using the MessageGroupId Property>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- @MessageGroupId@ is required for FIFO queues. You can\'t use it for
-- Standard queues.
--
-- 'delaySeconds', 'sendMessageBatchRequestEntry_delaySeconds' - The length of time, in seconds, for which a specific message is delayed.
-- Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive
-- @DelaySeconds@ value become available for processing after the delay
-- period is finished. If you don\'t specify a value, the default value for
-- the queue is applied.
--
-- When you set @FifoQueue@, you can\'t set @DelaySeconds@ per message. You
-- can set this parameter only on a queue level.
--
-- 'id', 'sendMessageBatchRequestEntry_id' - An identifier for a message in this batch used to communicate the
-- result.
--
-- The @Id@s of a batch request need to be unique within a request.
--
-- This identifier can have up to 80 characters. The following characters
-- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
--
-- 'messageBody', 'sendMessageBatchRequestEntry_messageBody' - The body of the message.
newSendMessageBatchRequestEntry ::
  -- | 'id'
  Prelude.Text ->
  -- | 'messageBody'
  Prelude.Text ->
  SendMessageBatchRequestEntry
newSendMessageBatchRequestEntry pId_ pMessageBody_ =
  SendMessageBatchRequestEntry'
    { messageDeduplicationId =
        Prelude.Nothing,
      messageAttributes = Prelude.Nothing,
      messageSystemAttributes = Prelude.Nothing,
      messageGroupId = Prelude.Nothing,
      delaySeconds = Prelude.Nothing,
      id = pId_,
      messageBody = pMessageBody_
    }

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of messages within a 5-minute minimum
-- deduplication interval. If a message with a particular
-- @MessageDeduplicationId@ is sent successfully, subsequent messages with
-- the same @MessageDeduplicationId@ are accepted successfully but aren\'t
-- delivered. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- -   Every message must have a unique @MessageDeduplicationId@,
--
--     -   You may provide a @MessageDeduplicationId@ explicitly.
--
--     -   If you aren\'t able to provide a @MessageDeduplicationId@ and
--         you enable @ContentBasedDeduplication@ for your queue, Amazon
--         SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@
--         using the body of the message (but not the attributes of the
--         message).
--
--     -   If you don\'t provide a @MessageDeduplicationId@ and the queue
--         doesn\'t have @ContentBasedDeduplication@ set, the action fails
--         with an error.
--
--     -   If the queue has @ContentBasedDeduplication@ set, your
--         @MessageDeduplicationId@ overrides the generated one.
--
-- -   When @ContentBasedDeduplication@ is in effect, messages with
--     identical content sent within the deduplication interval are treated
--     as duplicates and only one copy of the message is delivered.
--
-- -   If you send one message with @ContentBasedDeduplication@ enabled and
--     then another message with a @MessageDeduplicationId@ that is the
--     same as the one generated for the first @MessageDeduplicationId@,
--     the two messages are treated as duplicates and only one copy of the
--     message is delivered.
--
-- The @MessageDeduplicationId@ is available to the consumer of the message
-- (this can be useful for troubleshooting delivery issues).
--
-- If a message is sent successfully but the acknowledgement is lost and
-- the message is resent with the same @MessageDeduplicationId@ after the
-- deduplication interval, Amazon SQS can\'t detect duplicate messages.
--
-- Amazon SQS continues to keep track of the message deduplication ID even
-- after the message is received and deleted.
--
-- The length of @MessageDeduplicationId@ is 128 characters.
-- @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@,
-- @A-Z@, @0-9@) and punctuation
-- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
--
-- For best practices of using @MessageDeduplicationId@, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property>
-- in the /Amazon Simple Queue Service Developer Guide/.
sendMessageBatchRequestEntry_messageDeduplicationId :: Lens.Lens' SendMessageBatchRequestEntry (Prelude.Maybe Prelude.Text)
sendMessageBatchRequestEntry_messageDeduplicationId = Lens.lens (\SendMessageBatchRequestEntry' {messageDeduplicationId} -> messageDeduplicationId) (\s@SendMessageBatchRequestEntry' {} a -> s {messageDeduplicationId = a} :: SendMessageBatchRequestEntry)

-- | Each message attribute consists of a @Name@, @Type@, and @Value@. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon Simple Queue Service Developer Guide/.
sendMessageBatchRequestEntry_messageAttributes :: Lens.Lens' SendMessageBatchRequestEntry (Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue))
sendMessageBatchRequestEntry_messageAttributes = Lens.lens (\SendMessageBatchRequestEntry' {messageAttributes} -> messageAttributes) (\s@SendMessageBatchRequestEntry' {} a -> s {messageAttributes = a} :: SendMessageBatchRequestEntry) Prelude.. Lens.mapping Lens._Coerce

-- | The message system attribute to send Each message system attribute
-- consists of a @Name@, @Type@, and @Value@.
--
-- -   Currently, the only supported message system attribute is
--     @AWSTraceHeader@. Its type must be @String@ and its value must be a
--     correctly formatted AWS X-Ray trace header string.
--
-- -   The size of a message system attribute doesn\'t count towards the
--     total size of a message.
sendMessageBatchRequestEntry_messageSystemAttributes :: Lens.Lens' SendMessageBatchRequestEntry (Prelude.Maybe (Prelude.HashMap MessageSystemAttributeNameForSends MessageSystemAttributeValue))
sendMessageBatchRequestEntry_messageSystemAttributes = Lens.lens (\SendMessageBatchRequestEntry' {messageSystemAttributes} -> messageSystemAttributes) (\s@SendMessageBatchRequestEntry' {} a -> s {messageSystemAttributes = a} :: SendMessageBatchRequestEntry) Prelude.. Lens.mapping Lens._Coerce

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The tag that specifies that a message belongs to a specific message
-- group. Messages that belong to the same message group are processed in a
-- FIFO manner (however, messages in different message groups might be
-- processed out of order). To interleave multiple ordered streams within a
-- single queue, use @MessageGroupId@ values (for example, session data for
-- multiple users). In this scenario, multiple consumers can process the
-- queue, but the session data of each user is processed in a FIFO fashion.
--
-- -   You must associate a non-empty @MessageGroupId@ with a message. If
--     you don\'t provide a @MessageGroupId@, the action fails.
--
-- -   @ReceiveMessage@ might return messages with multiple
--     @MessageGroupId@ values. For each @MessageGroupId@, the messages are
--     sorted by time sent. The caller can\'t specify a @MessageGroupId@.
--
-- The length of @MessageGroupId@ is 128 characters. Valid values:
-- alphanumeric characters and punctuation
-- @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
--
-- For best practices of using @MessageGroupId@, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagegroupid-property.html Using the MessageGroupId Property>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- @MessageGroupId@ is required for FIFO queues. You can\'t use it for
-- Standard queues.
sendMessageBatchRequestEntry_messageGroupId :: Lens.Lens' SendMessageBatchRequestEntry (Prelude.Maybe Prelude.Text)
sendMessageBatchRequestEntry_messageGroupId = Lens.lens (\SendMessageBatchRequestEntry' {messageGroupId} -> messageGroupId) (\s@SendMessageBatchRequestEntry' {} a -> s {messageGroupId = a} :: SendMessageBatchRequestEntry)

-- | The length of time, in seconds, for which a specific message is delayed.
-- Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive
-- @DelaySeconds@ value become available for processing after the delay
-- period is finished. If you don\'t specify a value, the default value for
-- the queue is applied.
--
-- When you set @FifoQueue@, you can\'t set @DelaySeconds@ per message. You
-- can set this parameter only on a queue level.
sendMessageBatchRequestEntry_delaySeconds :: Lens.Lens' SendMessageBatchRequestEntry (Prelude.Maybe Prelude.Int)
sendMessageBatchRequestEntry_delaySeconds = Lens.lens (\SendMessageBatchRequestEntry' {delaySeconds} -> delaySeconds) (\s@SendMessageBatchRequestEntry' {} a -> s {delaySeconds = a} :: SendMessageBatchRequestEntry)

-- | An identifier for a message in this batch used to communicate the
-- result.
--
-- The @Id@s of a batch request need to be unique within a request.
--
-- This identifier can have up to 80 characters. The following characters
-- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
sendMessageBatchRequestEntry_id :: Lens.Lens' SendMessageBatchRequestEntry Prelude.Text
sendMessageBatchRequestEntry_id = Lens.lens (\SendMessageBatchRequestEntry' {id} -> id) (\s@SendMessageBatchRequestEntry' {} a -> s {id = a} :: SendMessageBatchRequestEntry)

-- | The body of the message.
sendMessageBatchRequestEntry_messageBody :: Lens.Lens' SendMessageBatchRequestEntry Prelude.Text
sendMessageBatchRequestEntry_messageBody = Lens.lens (\SendMessageBatchRequestEntry' {messageBody} -> messageBody) (\s@SendMessageBatchRequestEntry' {} a -> s {messageBody = a} :: SendMessageBatchRequestEntry)

instance
  Prelude.Hashable
    SendMessageBatchRequestEntry

instance Prelude.NFData SendMessageBatchRequestEntry

instance Core.ToQuery SendMessageBatchRequestEntry where
  toQuery SendMessageBatchRequestEntry' {..} =
    Prelude.mconcat
      [ "MessageDeduplicationId"
          Core.=: messageDeduplicationId,
        Core.toQuery
          ( Core.toQueryMap "MessageAttribute" "Name" "Value"
              Prelude.<$> messageAttributes
          ),
        Core.toQuery
          ( Core.toQueryMap
              "MessageSystemAttribute"
              "Name"
              "Value"
              Prelude.<$> messageSystemAttributes
          ),
        "MessageGroupId" Core.=: messageGroupId,
        "DelaySeconds" Core.=: delaySeconds,
        "Id" Core.=: id,
        "MessageBody" Core.=: messageBody
      ]
