{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SQS.SendMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delivers a message to the specified queue.
--
-- A message can include only XML, JSON, and unformatted text. The
-- following Unicode characters are allowed:
--
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ |
-- @#x10000@ to @#x10FFFF@
--
-- Any characters not included in this list will be rejected. For more
-- information, see the
-- <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters>.
module Amazonka.SQS.SendMessage
  ( -- * Creating a Request
    SendMessage (..),
    newSendMessage,

    -- * Request Lenses
    sendMessage_delaySeconds,
    sendMessage_messageAttributes,
    sendMessage_messageDeduplicationId,
    sendMessage_messageGroupId,
    sendMessage_messageSystemAttributes,
    sendMessage_queueUrl,
    sendMessage_messageBody,

    -- * Destructuring the Response
    SendMessageResponse (..),
    newSendMessageResponse,

    -- * Response Lenses
    sendMessageResponse_mD5OfMessageAttributes,
    sendMessageResponse_mD5OfMessageBody,
    sendMessageResponse_mD5OfMessageSystemAttributes,
    sendMessageResponse_messageId,
    sendMessageResponse_sequenceNumber,
    sendMessageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

-- |
--
-- /See:/ 'newSendMessage' smart constructor.
data SendMessage = SendMessage'
  { -- | The length of time, in seconds, for which to delay a specific message.
    -- Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive
    -- @DelaySeconds@ value become available for processing after the delay
    -- period is finished. If you don\'t specify a value, the default value for
    -- the queue applies.
    --
    -- When you set @FifoQueue@, you can\'t set @DelaySeconds@ per message. You
    -- can set this parameter only on a queue level.
    delaySeconds :: Prelude.Maybe Prelude.Int,
    -- | Each message attribute consists of a @Name@, @Type@, and @Value@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS message attributes>
    -- in the /Amazon SQS Developer Guide/.
    messageAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue),
    -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The token used for deduplication of sent messages. If a message with a
    -- particular @MessageDeduplicationId@ is sent successfully, any messages
    -- sent with the same @MessageDeduplicationId@ are accepted successfully
    -- but aren\'t delivered during the 5-minute deduplication interval. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-exactly-once-processing.html Exactly-once processing>
    -- in the /Amazon SQS Developer Guide/.
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
    -- The maximum length of @MessageDeduplicationId@ is 128 characters.
    -- @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@,
    -- @A-Z@, @0-9@) and punctuation
    -- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
    --
    -- For best practices of using @MessageDeduplicationId@, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property>
    -- in the /Amazon SQS Developer Guide/.
    messageDeduplicationId :: Prelude.Maybe Prelude.Text,
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
    -- in the /Amazon SQS Developer Guide/.
    --
    -- @MessageGroupId@ is required for FIFO queues. You can\'t use it for
    -- Standard queues.
    messageGroupId :: Prelude.Maybe Prelude.Text,
    -- | The message system attribute to send. Each message system attribute
    -- consists of a @Name@, @Type@, and @Value@.
    --
    -- -   Currently, the only supported message system attribute is
    --     @AWSTraceHeader@. Its type must be @String@ and its value must be a
    --     correctly formatted X-Ray trace header string.
    --
    -- -   The size of a message system attribute doesn\'t count towards the
    --     total size of a message.
    messageSystemAttributes :: Prelude.Maybe (Prelude.HashMap MessageSystemAttributeNameForSends MessageSystemAttributeValue),
    -- | The URL of the Amazon SQS queue to which a message is sent.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text,
    -- | The message to send. The minimum size is one character. The maximum size
    -- is 256 KiB.
    --
    -- A message can include only XML, JSON, and unformatted text. The
    -- following Unicode characters are allowed:
    --
    -- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ |
    -- @#x10000@ to @#x10FFFF@
    --
    -- Any characters not included in this list will be rejected. For more
    -- information, see the
    -- <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters>.
    messageBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delaySeconds', 'sendMessage_delaySeconds' - The length of time, in seconds, for which to delay a specific message.
-- Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive
-- @DelaySeconds@ value become available for processing after the delay
-- period is finished. If you don\'t specify a value, the default value for
-- the queue applies.
--
-- When you set @FifoQueue@, you can\'t set @DelaySeconds@ per message. You
-- can set this parameter only on a queue level.
--
-- 'messageAttributes', 'sendMessage_messageAttributes' - Each message attribute consists of a @Name@, @Type@, and @Value@. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS message attributes>
-- in the /Amazon SQS Developer Guide/.
--
-- 'messageDeduplicationId', 'sendMessage_messageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of sent messages. If a message with a
-- particular @MessageDeduplicationId@ is sent successfully, any messages
-- sent with the same @MessageDeduplicationId@ are accepted successfully
-- but aren\'t delivered during the 5-minute deduplication interval. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-exactly-once-processing.html Exactly-once processing>
-- in the /Amazon SQS Developer Guide/.
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
-- The maximum length of @MessageDeduplicationId@ is 128 characters.
-- @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@,
-- @A-Z@, @0-9@) and punctuation
-- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
--
-- For best practices of using @MessageDeduplicationId@, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property>
-- in the /Amazon SQS Developer Guide/.
--
-- 'messageGroupId', 'sendMessage_messageGroupId' - This parameter applies only to FIFO (first-in-first-out) queues.
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
-- in the /Amazon SQS Developer Guide/.
--
-- @MessageGroupId@ is required for FIFO queues. You can\'t use it for
-- Standard queues.
--
-- 'messageSystemAttributes', 'sendMessage_messageSystemAttributes' - The message system attribute to send. Each message system attribute
-- consists of a @Name@, @Type@, and @Value@.
--
-- -   Currently, the only supported message system attribute is
--     @AWSTraceHeader@. Its type must be @String@ and its value must be a
--     correctly formatted X-Ray trace header string.
--
-- -   The size of a message system attribute doesn\'t count towards the
--     total size of a message.
--
-- 'queueUrl', 'sendMessage_queueUrl' - The URL of the Amazon SQS queue to which a message is sent.
--
-- Queue URLs and names are case-sensitive.
--
-- 'messageBody', 'sendMessage_messageBody' - The message to send. The minimum size is one character. The maximum size
-- is 256 KiB.
--
-- A message can include only XML, JSON, and unformatted text. The
-- following Unicode characters are allowed:
--
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ |
-- @#x10000@ to @#x10FFFF@
--
-- Any characters not included in this list will be rejected. For more
-- information, see the
-- <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters>.
newSendMessage ::
  -- | 'queueUrl'
  Prelude.Text ->
  -- | 'messageBody'
  Prelude.Text ->
  SendMessage
newSendMessage pQueueUrl_ pMessageBody_ =
  SendMessage'
    { delaySeconds = Prelude.Nothing,
      messageAttributes = Prelude.Nothing,
      messageDeduplicationId = Prelude.Nothing,
      messageGroupId = Prelude.Nothing,
      messageSystemAttributes = Prelude.Nothing,
      queueUrl = pQueueUrl_,
      messageBody = pMessageBody_
    }

-- | The length of time, in seconds, for which to delay a specific message.
-- Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive
-- @DelaySeconds@ value become available for processing after the delay
-- period is finished. If you don\'t specify a value, the default value for
-- the queue applies.
--
-- When you set @FifoQueue@, you can\'t set @DelaySeconds@ per message. You
-- can set this parameter only on a queue level.
sendMessage_delaySeconds :: Lens.Lens' SendMessage (Prelude.Maybe Prelude.Int)
sendMessage_delaySeconds = Lens.lens (\SendMessage' {delaySeconds} -> delaySeconds) (\s@SendMessage' {} a -> s {delaySeconds = a} :: SendMessage)

-- | Each message attribute consists of a @Name@, @Type@, and @Value@. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS message attributes>
-- in the /Amazon SQS Developer Guide/.
sendMessage_messageAttributes :: Lens.Lens' SendMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue))
sendMessage_messageAttributes = Lens.lens (\SendMessage' {messageAttributes} -> messageAttributes) (\s@SendMessage' {} a -> s {messageAttributes = a} :: SendMessage) Prelude.. Lens.mapping Lens.coerced

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of sent messages. If a message with a
-- particular @MessageDeduplicationId@ is sent successfully, any messages
-- sent with the same @MessageDeduplicationId@ are accepted successfully
-- but aren\'t delivered during the 5-minute deduplication interval. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-exactly-once-processing.html Exactly-once processing>
-- in the /Amazon SQS Developer Guide/.
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
-- The maximum length of @MessageDeduplicationId@ is 128 characters.
-- @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@,
-- @A-Z@, @0-9@) and punctuation
-- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
--
-- For best practices of using @MessageDeduplicationId@, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property>
-- in the /Amazon SQS Developer Guide/.
sendMessage_messageDeduplicationId :: Lens.Lens' SendMessage (Prelude.Maybe Prelude.Text)
sendMessage_messageDeduplicationId = Lens.lens (\SendMessage' {messageDeduplicationId} -> messageDeduplicationId) (\s@SendMessage' {} a -> s {messageDeduplicationId = a} :: SendMessage)

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
-- in the /Amazon SQS Developer Guide/.
--
-- @MessageGroupId@ is required for FIFO queues. You can\'t use it for
-- Standard queues.
sendMessage_messageGroupId :: Lens.Lens' SendMessage (Prelude.Maybe Prelude.Text)
sendMessage_messageGroupId = Lens.lens (\SendMessage' {messageGroupId} -> messageGroupId) (\s@SendMessage' {} a -> s {messageGroupId = a} :: SendMessage)

-- | The message system attribute to send. Each message system attribute
-- consists of a @Name@, @Type@, and @Value@.
--
-- -   Currently, the only supported message system attribute is
--     @AWSTraceHeader@. Its type must be @String@ and its value must be a
--     correctly formatted X-Ray trace header string.
--
-- -   The size of a message system attribute doesn\'t count towards the
--     total size of a message.
sendMessage_messageSystemAttributes :: Lens.Lens' SendMessage (Prelude.Maybe (Prelude.HashMap MessageSystemAttributeNameForSends MessageSystemAttributeValue))
sendMessage_messageSystemAttributes = Lens.lens (\SendMessage' {messageSystemAttributes} -> messageSystemAttributes) (\s@SendMessage' {} a -> s {messageSystemAttributes = a} :: SendMessage) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the Amazon SQS queue to which a message is sent.
--
-- Queue URLs and names are case-sensitive.
sendMessage_queueUrl :: Lens.Lens' SendMessage Prelude.Text
sendMessage_queueUrl = Lens.lens (\SendMessage' {queueUrl} -> queueUrl) (\s@SendMessage' {} a -> s {queueUrl = a} :: SendMessage)

-- | The message to send. The minimum size is one character. The maximum size
-- is 256 KiB.
--
-- A message can include only XML, JSON, and unformatted text. The
-- following Unicode characters are allowed:
--
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ |
-- @#x10000@ to @#x10FFFF@
--
-- Any characters not included in this list will be rejected. For more
-- information, see the
-- <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters>.
sendMessage_messageBody :: Lens.Lens' SendMessage Prelude.Text
sendMessage_messageBody = Lens.lens (\SendMessage' {messageBody} -> messageBody) (\s@SendMessage' {} a -> s {messageBody = a} :: SendMessage)

instance Core.AWSRequest SendMessage where
  type AWSResponse SendMessage = SendMessageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SendMessageResult"
      ( \s h x ->
          SendMessageResponse'
            Prelude.<$> (x Data..@? "MD5OfMessageAttributes")
            Prelude.<*> (x Data..@? "MD5OfMessageBody")
            Prelude.<*> (x Data..@? "MD5OfMessageSystemAttributes")
            Prelude.<*> (x Data..@? "MessageId")
            Prelude.<*> (x Data..@? "SequenceNumber")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendMessage where
  hashWithSalt _salt SendMessage' {..} =
    _salt
      `Prelude.hashWithSalt` delaySeconds
      `Prelude.hashWithSalt` messageAttributes
      `Prelude.hashWithSalt` messageDeduplicationId
      `Prelude.hashWithSalt` messageGroupId
      `Prelude.hashWithSalt` messageSystemAttributes
      `Prelude.hashWithSalt` queueUrl
      `Prelude.hashWithSalt` messageBody

instance Prelude.NFData SendMessage where
  rnf SendMessage' {..} =
    Prelude.rnf delaySeconds
      `Prelude.seq` Prelude.rnf messageAttributes
      `Prelude.seq` Prelude.rnf messageDeduplicationId
      `Prelude.seq` Prelude.rnf messageGroupId
      `Prelude.seq` Prelude.rnf messageSystemAttributes
      `Prelude.seq` Prelude.rnf queueUrl
      `Prelude.seq` Prelude.rnf messageBody

instance Data.ToHeaders SendMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SendMessage where
  toPath = Prelude.const "/"

instance Data.ToQuery SendMessage where
  toQuery SendMessage' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SendMessage" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        "DelaySeconds" Data.=: delaySeconds,
        Data.toQuery
          ( Data.toQueryMap "MessageAttribute" "Name" "Value"
              Prelude.<$> messageAttributes
          ),
        "MessageDeduplicationId"
          Data.=: messageDeduplicationId,
        "MessageGroupId" Data.=: messageGroupId,
        Data.toQuery
          ( Data.toQueryMap
              "MessageSystemAttribute"
              "Name"
              "Value"
              Prelude.<$> messageSystemAttributes
          ),
        "QueueUrl" Data.=: queueUrl,
        "MessageBody" Data.=: messageBody
      ]

-- | The @MD5OfMessageBody@ and @MessageId@ elements.
--
-- /See:/ 'newSendMessageResponse' smart constructor.
data SendMessageResponse = SendMessageResponse'
  { -- | An MD5 digest of the non-URL-encoded message attribute string. You can
    -- use this attribute to verify that Amazon SQS received the message
    -- correctly. Amazon SQS URL-decodes the message before creating the MD5
    -- digest. For information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageAttributes :: Prelude.Maybe Prelude.Text,
    -- | An MD5 digest of the non-URL-encoded message body string. You can use
    -- this attribute to verify that Amazon SQS received the message correctly.
    -- Amazon SQS URL-decodes the message before creating the MD5 digest. For
    -- information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageBody :: Prelude.Maybe Prelude.Text,
    -- | An MD5 digest of the non-URL-encoded message system attribute string.
    -- You can use this attribute to verify that Amazon SQS received the
    -- message correctly. Amazon SQS URL-decodes the message before creating
    -- the MD5 digest.
    mD5OfMessageSystemAttributes :: Prelude.Maybe Prelude.Text,
    -- | An attribute containing the @MessageId@ of the message sent to the
    -- queue. For more information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers>
    -- in the /Amazon SQS Developer Guide/.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The large, non-consecutive number that Amazon SQS assigns to each
    -- message.
    --
    -- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues
    -- to increase for a particular @MessageGroupId@.
    sequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mD5OfMessageAttributes', 'sendMessageResponse_mD5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
--
-- 'mD5OfMessageBody', 'sendMessageResponse_mD5OfMessageBody' - An MD5 digest of the non-URL-encoded message body string. You can use
-- this attribute to verify that Amazon SQS received the message correctly.
-- Amazon SQS URL-decodes the message before creating the MD5 digest. For
-- information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
--
-- 'mD5OfMessageSystemAttributes', 'sendMessageResponse_mD5OfMessageSystemAttributes' - An MD5 digest of the non-URL-encoded message system attribute string.
-- You can use this attribute to verify that Amazon SQS received the
-- message correctly. Amazon SQS URL-decodes the message before creating
-- the MD5 digest.
--
-- 'messageId', 'sendMessageResponse_messageId' - An attribute containing the @MessageId@ of the message sent to the
-- queue. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers>
-- in the /Amazon SQS Developer Guide/.
--
-- 'sequenceNumber', 'sendMessageResponse_sequenceNumber' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each
-- message.
--
-- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues
-- to increase for a particular @MessageGroupId@.
--
-- 'httpStatus', 'sendMessageResponse_httpStatus' - The response's http status code.
newSendMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendMessageResponse
newSendMessageResponse pHttpStatus_ =
  SendMessageResponse'
    { mD5OfMessageAttributes =
        Prelude.Nothing,
      mD5OfMessageBody = Prelude.Nothing,
      mD5OfMessageSystemAttributes = Prelude.Nothing,
      messageId = Prelude.Nothing,
      sequenceNumber = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
sendMessageResponse_mD5OfMessageAttributes :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_mD5OfMessageAttributes = Lens.lens (\SendMessageResponse' {mD5OfMessageAttributes} -> mD5OfMessageAttributes) (\s@SendMessageResponse' {} a -> s {mD5OfMessageAttributes = a} :: SendMessageResponse)

-- | An MD5 digest of the non-URL-encoded message body string. You can use
-- this attribute to verify that Amazon SQS received the message correctly.
-- Amazon SQS URL-decodes the message before creating the MD5 digest. For
-- information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
sendMessageResponse_mD5OfMessageBody :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_mD5OfMessageBody = Lens.lens (\SendMessageResponse' {mD5OfMessageBody} -> mD5OfMessageBody) (\s@SendMessageResponse' {} a -> s {mD5OfMessageBody = a} :: SendMessageResponse)

-- | An MD5 digest of the non-URL-encoded message system attribute string.
-- You can use this attribute to verify that Amazon SQS received the
-- message correctly. Amazon SQS URL-decodes the message before creating
-- the MD5 digest.
sendMessageResponse_mD5OfMessageSystemAttributes :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_mD5OfMessageSystemAttributes = Lens.lens (\SendMessageResponse' {mD5OfMessageSystemAttributes} -> mD5OfMessageSystemAttributes) (\s@SendMessageResponse' {} a -> s {mD5OfMessageSystemAttributes = a} :: SendMessageResponse)

-- | An attribute containing the @MessageId@ of the message sent to the
-- queue. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers>
-- in the /Amazon SQS Developer Guide/.
sendMessageResponse_messageId :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_messageId = Lens.lens (\SendMessageResponse' {messageId} -> messageId) (\s@SendMessageResponse' {} a -> s {messageId = a} :: SendMessageResponse)

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each
-- message.
--
-- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues
-- to increase for a particular @MessageGroupId@.
sendMessageResponse_sequenceNumber :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_sequenceNumber = Lens.lens (\SendMessageResponse' {sequenceNumber} -> sequenceNumber) (\s@SendMessageResponse' {} a -> s {sequenceNumber = a} :: SendMessageResponse)

-- | The response's http status code.
sendMessageResponse_httpStatus :: Lens.Lens' SendMessageResponse Prelude.Int
sendMessageResponse_httpStatus = Lens.lens (\SendMessageResponse' {httpStatus} -> httpStatus) (\s@SendMessageResponse' {} a -> s {httpStatus = a} :: SendMessageResponse)

instance Prelude.NFData SendMessageResponse where
  rnf SendMessageResponse' {..} =
    Prelude.rnf mD5OfMessageAttributes
      `Prelude.seq` Prelude.rnf mD5OfMessageBody
      `Prelude.seq` Prelude.rnf mD5OfMessageSystemAttributes
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf sequenceNumber
      `Prelude.seq` Prelude.rnf httpStatus
