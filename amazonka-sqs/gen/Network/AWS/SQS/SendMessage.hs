{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SQS.SendMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SQS.SendMessage
  ( -- * Creating a Request
    SendMessage (..),
    newSendMessage,

    -- * Request Lenses
    sendMessage_messageDeduplicationId,
    sendMessage_messageAttributes,
    sendMessage_messageSystemAttributes,
    sendMessage_messageGroupId,
    sendMessage_delaySeconds,
    sendMessage_queueUrl,
    sendMessage_messageBody,

    -- * Destructuring the Response
    SendMessageResponse (..),
    newSendMessageResponse,

    -- * Response Lenses
    sendMessageResponse_sequenceNumber,
    sendMessageResponse_mD5OfMessageBody,
    sendMessageResponse_mD5OfMessageSystemAttributes,
    sendMessageResponse_mD5OfMessageAttributes,
    sendMessageResponse_messageId,
    sendMessageResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'newSendMessage' smart constructor.
data SendMessage = SendMessage'
  { -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The token used for deduplication of sent messages. If a message with a
    -- particular @MessageDeduplicationId@ is sent successfully, any messages
    -- sent with the same @MessageDeduplicationId@ are accepted successfully
    -- but aren\'t delivered during the 5-minute deduplication interval. For
    -- more information, see
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
    -- The maximum length of @MessageDeduplicationId@ is 128 characters.
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
    -- | The message system attribute to send. Each message system attribute
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
    -- | The length of time, in seconds, for which to delay a specific message.
    -- Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive
    -- @DelaySeconds@ value become available for processing after the delay
    -- period is finished. If you don\'t specify a value, the default value for
    -- the queue applies.
    --
    -- When you set @FifoQueue@, you can\'t set @DelaySeconds@ per message. You
    -- can set this parameter only on a queue level.
    delaySeconds :: Prelude.Maybe Prelude.Int,
    -- | The URL of the Amazon SQS queue to which a message is sent.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text,
    -- | The message to send. The minimum size is one character. The maximum size
    -- is 256 KB.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageDeduplicationId', 'sendMessage_messageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of sent messages. If a message with a
-- particular @MessageDeduplicationId@ is sent successfully, any messages
-- sent with the same @MessageDeduplicationId@ are accepted successfully
-- but aren\'t delivered during the 5-minute deduplication interval. For
-- more information, see
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
-- The maximum length of @MessageDeduplicationId@ is 128 characters.
-- @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@,
-- @A-Z@, @0-9@) and punctuation
-- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
--
-- For best practices of using @MessageDeduplicationId@, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- 'messageAttributes', 'sendMessage_messageAttributes' - Each message attribute consists of a @Name@, @Type@, and @Value@. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- 'messageSystemAttributes', 'sendMessage_messageSystemAttributes' - The message system attribute to send. Each message system attribute
-- consists of a @Name@, @Type@, and @Value@.
--
-- -   Currently, the only supported message system attribute is
--     @AWSTraceHeader@. Its type must be @String@ and its value must be a
--     correctly formatted AWS X-Ray trace header string.
--
-- -   The size of a message system attribute doesn\'t count towards the
--     total size of a message.
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
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- @MessageGroupId@ is required for FIFO queues. You can\'t use it for
-- Standard queues.
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
-- 'queueUrl', 'sendMessage_queueUrl' - The URL of the Amazon SQS queue to which a message is sent.
--
-- Queue URLs and names are case-sensitive.
--
-- 'messageBody', 'sendMessage_messageBody' - The message to send. The minimum size is one character. The maximum size
-- is 256 KB.
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
    { messageDeduplicationId =
        Prelude.Nothing,
      messageAttributes = Prelude.Nothing,
      messageSystemAttributes = Prelude.Nothing,
      messageGroupId = Prelude.Nothing,
      delaySeconds = Prelude.Nothing,
      queueUrl = pQueueUrl_,
      messageBody = pMessageBody_
    }

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of sent messages. If a message with a
-- particular @MessageDeduplicationId@ is sent successfully, any messages
-- sent with the same @MessageDeduplicationId@ are accepted successfully
-- but aren\'t delivered during the 5-minute deduplication interval. For
-- more information, see
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
-- The maximum length of @MessageDeduplicationId@ is 128 characters.
-- @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@,
-- @A-Z@, @0-9@) and punctuation
-- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
--
-- For best practices of using @MessageDeduplicationId@, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the MessageDeduplicationId Property>
-- in the /Amazon Simple Queue Service Developer Guide/.
sendMessage_messageDeduplicationId :: Lens.Lens' SendMessage (Prelude.Maybe Prelude.Text)
sendMessage_messageDeduplicationId = Lens.lens (\SendMessage' {messageDeduplicationId} -> messageDeduplicationId) (\s@SendMessage' {} a -> s {messageDeduplicationId = a} :: SendMessage)

-- | Each message attribute consists of a @Name@, @Type@, and @Value@. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-metadata.html#sqs-message-attributes Amazon SQS Message Attributes>
-- in the /Amazon Simple Queue Service Developer Guide/.
sendMessage_messageAttributes :: Lens.Lens' SendMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue))
sendMessage_messageAttributes = Lens.lens (\SendMessage' {messageAttributes} -> messageAttributes) (\s@SendMessage' {} a -> s {messageAttributes = a} :: SendMessage) Prelude.. Lens.mapping Prelude._Coerce

-- | The message system attribute to send. Each message system attribute
-- consists of a @Name@, @Type@, and @Value@.
--
-- -   Currently, the only supported message system attribute is
--     @AWSTraceHeader@. Its type must be @String@ and its value must be a
--     correctly formatted AWS X-Ray trace header string.
--
-- -   The size of a message system attribute doesn\'t count towards the
--     total size of a message.
sendMessage_messageSystemAttributes :: Lens.Lens' SendMessage (Prelude.Maybe (Prelude.HashMap MessageSystemAttributeNameForSends MessageSystemAttributeValue))
sendMessage_messageSystemAttributes = Lens.lens (\SendMessage' {messageSystemAttributes} -> messageSystemAttributes) (\s@SendMessage' {} a -> s {messageSystemAttributes = a} :: SendMessage) Prelude.. Lens.mapping Prelude._Coerce

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
sendMessage_messageGroupId :: Lens.Lens' SendMessage (Prelude.Maybe Prelude.Text)
sendMessage_messageGroupId = Lens.lens (\SendMessage' {messageGroupId} -> messageGroupId) (\s@SendMessage' {} a -> s {messageGroupId = a} :: SendMessage)

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

-- | The URL of the Amazon SQS queue to which a message is sent.
--
-- Queue URLs and names are case-sensitive.
sendMessage_queueUrl :: Lens.Lens' SendMessage Prelude.Text
sendMessage_queueUrl = Lens.lens (\SendMessage' {queueUrl} -> queueUrl) (\s@SendMessage' {} a -> s {queueUrl = a} :: SendMessage)

-- | The message to send. The minimum size is one character. The maximum size
-- is 256 KB.
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

instance Prelude.AWSRequest SendMessage where
  type Rs SendMessage = SendMessageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SendMessageResult"
      ( \s h x ->
          SendMessageResponse'
            Prelude.<$> (x Prelude..@? "SequenceNumber")
            Prelude.<*> (x Prelude..@? "MD5OfMessageBody")
            Prelude.<*> (x Prelude..@? "MD5OfMessageSystemAttributes")
            Prelude.<*> (x Prelude..@? "MD5OfMessageAttributes")
            Prelude.<*> (x Prelude..@? "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendMessage

instance Prelude.NFData SendMessage

instance Prelude.ToHeaders SendMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SendMessage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SendMessage where
  toQuery SendMessage' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SendMessage" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-11-05" :: Prelude.ByteString),
        "MessageDeduplicationId"
          Prelude.=: messageDeduplicationId,
        Prelude.toQuery
          ( Prelude.toQueryMap "MessageAttribute" "Name" "Value"
              Prelude.<$> messageAttributes
          ),
        Prelude.toQuery
          ( Prelude.toQueryMap
              "MessageSystemAttribute"
              "Name"
              "Value"
              Prelude.<$> messageSystemAttributes
          ),
        "MessageGroupId" Prelude.=: messageGroupId,
        "DelaySeconds" Prelude.=: delaySeconds,
        "QueueUrl" Prelude.=: queueUrl,
        "MessageBody" Prelude.=: messageBody
      ]

-- | The @MD5OfMessageBody@ and @MessageId@ elements.
--
-- /See:/ 'newSendMessageResponse' smart constructor.
data SendMessageResponse = SendMessageResponse'
  { -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The large, non-consecutive number that Amazon SQS assigns to each
    -- message.
    --
    -- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues
    -- to increase for a particular @MessageGroupId@.
    sequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can
    -- use this attribute to verify that Amazon SQS received the message
    -- correctly. Amazon SQS URL-decodes the message before creating the MD5
    -- digest. For information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageBody :: Prelude.Maybe Prelude.Text,
    -- | An MD5 digest of the non-URL-encoded message system attribute string.
    -- You can use this attribute to verify that Amazon SQS received the
    -- message correctly. Amazon SQS URL-decodes the message before creating
    -- the MD5 digest.
    mD5OfMessageSystemAttributes :: Prelude.Maybe Prelude.Text,
    -- | An MD5 digest of the non-URL-encoded message attribute string. You can
    -- use this attribute to verify that Amazon SQS received the message
    -- correctly. Amazon SQS URL-decodes the message before creating the MD5
    -- digest. For information about MD5, see
    -- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
    mD5OfMessageAttributes :: Prelude.Maybe Prelude.Text,
    -- | An attribute containing the @MessageId@ of the message sent to the
    -- queue. For more information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers>
    -- in the /Amazon Simple Queue Service Developer Guide/.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceNumber', 'sendMessageResponse_sequenceNumber' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each
-- message.
--
-- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues
-- to increase for a particular @MessageGroupId@.
--
-- 'mD5OfMessageBody', 'sendMessageResponse_mD5OfMessageBody' - An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
--
-- 'mD5OfMessageSystemAttributes', 'sendMessageResponse_mD5OfMessageSystemAttributes' - An MD5 digest of the non-URL-encoded message system attribute string.
-- You can use this attribute to verify that Amazon SQS received the
-- message correctly. Amazon SQS URL-decodes the message before creating
-- the MD5 digest.
--
-- 'mD5OfMessageAttributes', 'sendMessageResponse_mD5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
--
-- 'messageId', 'sendMessageResponse_messageId' - An attribute containing the @MessageId@ of the message sent to the
-- queue. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- 'httpStatus', 'sendMessageResponse_httpStatus' - The response's http status code.
newSendMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendMessageResponse
newSendMessageResponse pHttpStatus_ =
  SendMessageResponse'
    { sequenceNumber =
        Prelude.Nothing,
      mD5OfMessageBody = Prelude.Nothing,
      mD5OfMessageSystemAttributes = Prelude.Nothing,
      mD5OfMessageAttributes = Prelude.Nothing,
      messageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The large, non-consecutive number that Amazon SQS assigns to each
-- message.
--
-- The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues
-- to increase for a particular @MessageGroupId@.
sendMessageResponse_sequenceNumber :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_sequenceNumber = Lens.lens (\SendMessageResponse' {sequenceNumber} -> sequenceNumber) (\s@SendMessageResponse' {} a -> s {sequenceNumber = a} :: SendMessageResponse)

-- | An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
sendMessageResponse_mD5OfMessageBody :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_mD5OfMessageBody = Lens.lens (\SendMessageResponse' {mD5OfMessageBody} -> mD5OfMessageBody) (\s@SendMessageResponse' {} a -> s {mD5OfMessageBody = a} :: SendMessageResponse)

-- | An MD5 digest of the non-URL-encoded message system attribute string.
-- You can use this attribute to verify that Amazon SQS received the
-- message correctly. Amazon SQS URL-decodes the message before creating
-- the MD5 digest.
sendMessageResponse_mD5OfMessageSystemAttributes :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_mD5OfMessageSystemAttributes = Lens.lens (\SendMessageResponse' {mD5OfMessageSystemAttributes} -> mD5OfMessageSystemAttributes) (\s@SendMessageResponse' {} a -> s {mD5OfMessageSystemAttributes = a} :: SendMessageResponse)

-- | An MD5 digest of the non-URL-encoded message attribute string. You can
-- use this attribute to verify that Amazon SQS received the message
-- correctly. Amazon SQS URL-decodes the message before creating the MD5
-- digest. For information about MD5, see
-- <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
sendMessageResponse_mD5OfMessageAttributes :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_mD5OfMessageAttributes = Lens.lens (\SendMessageResponse' {mD5OfMessageAttributes} -> mD5OfMessageAttributes) (\s@SendMessageResponse' {} a -> s {mD5OfMessageAttributes = a} :: SendMessageResponse)

-- | An attribute containing the @MessageId@ of the message sent to the
-- queue. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers>
-- in the /Amazon Simple Queue Service Developer Guide/.
sendMessageResponse_messageId :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_messageId = Lens.lens (\SendMessageResponse' {messageId} -> messageId) (\s@SendMessageResponse' {} a -> s {messageId = a} :: SendMessageResponse)

-- | The response's http status code.
sendMessageResponse_httpStatus :: Lens.Lens' SendMessageResponse Prelude.Int
sendMessageResponse_httpStatus = Lens.lens (\SendMessageResponse' {httpStatus} -> httpStatus) (\s@SendMessageResponse' {} a -> s {httpStatus = a} :: SendMessageResponse)

instance Prelude.NFData SendMessageResponse
