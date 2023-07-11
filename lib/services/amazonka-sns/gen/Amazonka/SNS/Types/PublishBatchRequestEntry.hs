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
-- Module      : Amazonka.SNS.Types.PublishBatchRequestEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.PublishBatchRequestEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SNS.Types.MessageAttributeValue

-- | Contains the details of a single Amazon SNS message along with an @Id@
-- that identifies a message within the batch.
--
-- /See:/ 'newPublishBatchRequestEntry' smart constructor.
data PublishBatchRequestEntry = PublishBatchRequestEntry'
  { -- | Each message attribute consists of a @Name@, @Type@, and @Value@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/sns/latest/dg/sns-message-attributes.html Amazon SNS message attributes>
    -- in the Amazon SNS Developer Guide.
    messageAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue),
    -- | This parameter applies only to FIFO (first-in-first-out) topics.
    --
    -- The token used for deduplication of messages within a 5-minute minimum
    -- deduplication interval. If a message with a particular
    -- @MessageDeduplicationId@ is sent successfully, subsequent messages with
    -- the same @MessageDeduplicationId@ are accepted successfully but aren\'t
    -- delivered.
    --
    -- -   Every message must have a unique @MessageDeduplicationId@.
    --
    --     -   You may provide a @MessageDeduplicationId@ explicitly.
    --
    --     -   If you aren\'t able to provide a @MessageDeduplicationId@ and
    --         you enable @ContentBasedDeduplication@ for your topic, Amazon
    --         SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@
    --         using the body of the message (but not the attributes of the
    --         message).
    --
    --     -   If you don\'t provide a @MessageDeduplicationId@ and the topic
    --         doesn\'t have @ContentBasedDeduplication@ set, the action fails
    --         with an error.
    --
    --     -   If the topic has a @ContentBasedDeduplication@ set, your
    --         @MessageDeduplicationId@ overrides the generated one.
    --
    -- -   When @ContentBasedDeduplication@ is in effect, messages with
    --     identical content sent within the deduplication interval are treated
    --     as duplicates and only one copy of the message is delivered.
    --
    -- -   If you send one message with @ContentBasedDeduplication@ enabled,
    --     and then another message with a @MessageDeduplicationId@ that is the
    --     same as the one generated for the first @MessageDeduplicationId@,
    --     the two messages are treated as duplicates and only one copy of the
    --     message is delivered.
    --
    -- The @MessageDeduplicationId@ is available to the consumer of the message
    -- (this can be useful for troubleshooting delivery issues).
    --
    -- If a message is sent successfully but the acknowledgement is lost and
    -- the message is resent with the same @MessageDeduplicationId@ after the
    -- deduplication interval, Amazon SNS can\'t detect duplicate messages.
    --
    -- Amazon SNS continues to keep track of the message deduplication ID even
    -- after the message is received and deleted.
    --
    -- The length of @MessageDeduplicationId@ is 128 characters.
    --
    -- @MessageDeduplicationId@ can contain alphanumeric characters
    -- @(a-z, A-Z, 0-9)@ and punctuation
    -- @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
    messageDeduplicationId :: Prelude.Maybe Prelude.Text,
    -- | This parameter applies only to FIFO (first-in-first-out) topics.
    --
    -- The tag that specifies that a message belongs to a specific message
    -- group. Messages that belong to the same message group are processed in a
    -- FIFO manner (however, messages in different message groups might be
    -- processed out of order). To interleave multiple ordered streams within a
    -- single topic, use @MessageGroupId@ values (for example, session data for
    -- multiple users). In this scenario, multiple consumers can process the
    -- topic, but the session data of each user is processed in a FIFO fashion.
    --
    -- You must associate a non-empty @MessageGroupId@ with a message. If you
    -- don\'t provide a @MessageGroupId@, the action fails.
    --
    -- The length of @MessageGroupId@ is 128 characters.
    --
    -- @MessageGroupId@ can contain alphanumeric characters @(a-z, A-Z, 0-9)@
    -- and punctuation @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
    --
    -- @MessageGroupId@ is required for FIFO topics. You can\'t use it for
    -- standard topics.
    messageGroupId :: Prelude.Maybe Prelude.Text,
    -- | Set @MessageStructure@ to @json@ if you want to send a different message
    -- for each protocol. For example, using one publish action, you can send a
    -- short message to your SMS subscribers and a longer message to your email
    -- subscribers. If you set @MessageStructure@ to @json@, the value of the
    -- @Message@ parameter must:
    --
    -- -   be a syntactically valid JSON object; and
    --
    -- -   contain at least a top-level JSON key of \"default\" with a value
    --     that is a string.
    --
    -- You can define other top-level keys that define the message you want to
    -- send to a specific transport protocol (e.g. http).
    messageStructure :: Prelude.Maybe Prelude.Text,
    -- | The subject of the batch message.
    subject :: Prelude.Maybe Prelude.Text,
    -- | An identifier for the message in this batch.
    --
    -- The @Ids@ of a batch request must be unique within a request.
    --
    -- This identifier can have up to 80 characters. The following characters
    -- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
    id :: Prelude.Text,
    -- | The body of the message.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishBatchRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageAttributes', 'publishBatchRequestEntry_messageAttributes' - Each message attribute consists of a @Name@, @Type@, and @Value@. For
-- more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-message-attributes.html Amazon SNS message attributes>
-- in the Amazon SNS Developer Guide.
--
-- 'messageDeduplicationId', 'publishBatchRequestEntry_messageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) topics.
--
-- The token used for deduplication of messages within a 5-minute minimum
-- deduplication interval. If a message with a particular
-- @MessageDeduplicationId@ is sent successfully, subsequent messages with
-- the same @MessageDeduplicationId@ are accepted successfully but aren\'t
-- delivered.
--
-- -   Every message must have a unique @MessageDeduplicationId@.
--
--     -   You may provide a @MessageDeduplicationId@ explicitly.
--
--     -   If you aren\'t able to provide a @MessageDeduplicationId@ and
--         you enable @ContentBasedDeduplication@ for your topic, Amazon
--         SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@
--         using the body of the message (but not the attributes of the
--         message).
--
--     -   If you don\'t provide a @MessageDeduplicationId@ and the topic
--         doesn\'t have @ContentBasedDeduplication@ set, the action fails
--         with an error.
--
--     -   If the topic has a @ContentBasedDeduplication@ set, your
--         @MessageDeduplicationId@ overrides the generated one.
--
-- -   When @ContentBasedDeduplication@ is in effect, messages with
--     identical content sent within the deduplication interval are treated
--     as duplicates and only one copy of the message is delivered.
--
-- -   If you send one message with @ContentBasedDeduplication@ enabled,
--     and then another message with a @MessageDeduplicationId@ that is the
--     same as the one generated for the first @MessageDeduplicationId@,
--     the two messages are treated as duplicates and only one copy of the
--     message is delivered.
--
-- The @MessageDeduplicationId@ is available to the consumer of the message
-- (this can be useful for troubleshooting delivery issues).
--
-- If a message is sent successfully but the acknowledgement is lost and
-- the message is resent with the same @MessageDeduplicationId@ after the
-- deduplication interval, Amazon SNS can\'t detect duplicate messages.
--
-- Amazon SNS continues to keep track of the message deduplication ID even
-- after the message is received and deleted.
--
-- The length of @MessageDeduplicationId@ is 128 characters.
--
-- @MessageDeduplicationId@ can contain alphanumeric characters
-- @(a-z, A-Z, 0-9)@ and punctuation
-- @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
--
-- 'messageGroupId', 'publishBatchRequestEntry_messageGroupId' - This parameter applies only to FIFO (first-in-first-out) topics.
--
-- The tag that specifies that a message belongs to a specific message
-- group. Messages that belong to the same message group are processed in a
-- FIFO manner (however, messages in different message groups might be
-- processed out of order). To interleave multiple ordered streams within a
-- single topic, use @MessageGroupId@ values (for example, session data for
-- multiple users). In this scenario, multiple consumers can process the
-- topic, but the session data of each user is processed in a FIFO fashion.
--
-- You must associate a non-empty @MessageGroupId@ with a message. If you
-- don\'t provide a @MessageGroupId@, the action fails.
--
-- The length of @MessageGroupId@ is 128 characters.
--
-- @MessageGroupId@ can contain alphanumeric characters @(a-z, A-Z, 0-9)@
-- and punctuation @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
--
-- @MessageGroupId@ is required for FIFO topics. You can\'t use it for
-- standard topics.
--
-- 'messageStructure', 'publishBatchRequestEntry_messageStructure' - Set @MessageStructure@ to @json@ if you want to send a different message
-- for each protocol. For example, using one publish action, you can send a
-- short message to your SMS subscribers and a longer message to your email
-- subscribers. If you set @MessageStructure@ to @json@, the value of the
-- @Message@ parameter must:
--
-- -   be a syntactically valid JSON object; and
--
-- -   contain at least a top-level JSON key of \"default\" with a value
--     that is a string.
--
-- You can define other top-level keys that define the message you want to
-- send to a specific transport protocol (e.g. http).
--
-- 'subject', 'publishBatchRequestEntry_subject' - The subject of the batch message.
--
-- 'id', 'publishBatchRequestEntry_id' - An identifier for the message in this batch.
--
-- The @Ids@ of a batch request must be unique within a request.
--
-- This identifier can have up to 80 characters. The following characters
-- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
--
-- 'message', 'publishBatchRequestEntry_message' - The body of the message.
newPublishBatchRequestEntry ::
  -- | 'id'
  Prelude.Text ->
  -- | 'message'
  Prelude.Text ->
  PublishBatchRequestEntry
newPublishBatchRequestEntry pId_ pMessage_ =
  PublishBatchRequestEntry'
    { messageAttributes =
        Prelude.Nothing,
      messageDeduplicationId = Prelude.Nothing,
      messageGroupId = Prelude.Nothing,
      messageStructure = Prelude.Nothing,
      subject = Prelude.Nothing,
      id = pId_,
      message = pMessage_
    }

-- | Each message attribute consists of a @Name@, @Type@, and @Value@. For
-- more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-message-attributes.html Amazon SNS message attributes>
-- in the Amazon SNS Developer Guide.
publishBatchRequestEntry_messageAttributes :: Lens.Lens' PublishBatchRequestEntry (Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue))
publishBatchRequestEntry_messageAttributes = Lens.lens (\PublishBatchRequestEntry' {messageAttributes} -> messageAttributes) (\s@PublishBatchRequestEntry' {} a -> s {messageAttributes = a} :: PublishBatchRequestEntry) Prelude.. Lens.mapping Lens.coerced

-- | This parameter applies only to FIFO (first-in-first-out) topics.
--
-- The token used for deduplication of messages within a 5-minute minimum
-- deduplication interval. If a message with a particular
-- @MessageDeduplicationId@ is sent successfully, subsequent messages with
-- the same @MessageDeduplicationId@ are accepted successfully but aren\'t
-- delivered.
--
-- -   Every message must have a unique @MessageDeduplicationId@.
--
--     -   You may provide a @MessageDeduplicationId@ explicitly.
--
--     -   If you aren\'t able to provide a @MessageDeduplicationId@ and
--         you enable @ContentBasedDeduplication@ for your topic, Amazon
--         SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@
--         using the body of the message (but not the attributes of the
--         message).
--
--     -   If you don\'t provide a @MessageDeduplicationId@ and the topic
--         doesn\'t have @ContentBasedDeduplication@ set, the action fails
--         with an error.
--
--     -   If the topic has a @ContentBasedDeduplication@ set, your
--         @MessageDeduplicationId@ overrides the generated one.
--
-- -   When @ContentBasedDeduplication@ is in effect, messages with
--     identical content sent within the deduplication interval are treated
--     as duplicates and only one copy of the message is delivered.
--
-- -   If you send one message with @ContentBasedDeduplication@ enabled,
--     and then another message with a @MessageDeduplicationId@ that is the
--     same as the one generated for the first @MessageDeduplicationId@,
--     the two messages are treated as duplicates and only one copy of the
--     message is delivered.
--
-- The @MessageDeduplicationId@ is available to the consumer of the message
-- (this can be useful for troubleshooting delivery issues).
--
-- If a message is sent successfully but the acknowledgement is lost and
-- the message is resent with the same @MessageDeduplicationId@ after the
-- deduplication interval, Amazon SNS can\'t detect duplicate messages.
--
-- Amazon SNS continues to keep track of the message deduplication ID even
-- after the message is received and deleted.
--
-- The length of @MessageDeduplicationId@ is 128 characters.
--
-- @MessageDeduplicationId@ can contain alphanumeric characters
-- @(a-z, A-Z, 0-9)@ and punctuation
-- @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
publishBatchRequestEntry_messageDeduplicationId :: Lens.Lens' PublishBatchRequestEntry (Prelude.Maybe Prelude.Text)
publishBatchRequestEntry_messageDeduplicationId = Lens.lens (\PublishBatchRequestEntry' {messageDeduplicationId} -> messageDeduplicationId) (\s@PublishBatchRequestEntry' {} a -> s {messageDeduplicationId = a} :: PublishBatchRequestEntry)

-- | This parameter applies only to FIFO (first-in-first-out) topics.
--
-- The tag that specifies that a message belongs to a specific message
-- group. Messages that belong to the same message group are processed in a
-- FIFO manner (however, messages in different message groups might be
-- processed out of order). To interleave multiple ordered streams within a
-- single topic, use @MessageGroupId@ values (for example, session data for
-- multiple users). In this scenario, multiple consumers can process the
-- topic, but the session data of each user is processed in a FIFO fashion.
--
-- You must associate a non-empty @MessageGroupId@ with a message. If you
-- don\'t provide a @MessageGroupId@, the action fails.
--
-- The length of @MessageGroupId@ is 128 characters.
--
-- @MessageGroupId@ can contain alphanumeric characters @(a-z, A-Z, 0-9)@
-- and punctuation @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
--
-- @MessageGroupId@ is required for FIFO topics. You can\'t use it for
-- standard topics.
publishBatchRequestEntry_messageGroupId :: Lens.Lens' PublishBatchRequestEntry (Prelude.Maybe Prelude.Text)
publishBatchRequestEntry_messageGroupId = Lens.lens (\PublishBatchRequestEntry' {messageGroupId} -> messageGroupId) (\s@PublishBatchRequestEntry' {} a -> s {messageGroupId = a} :: PublishBatchRequestEntry)

-- | Set @MessageStructure@ to @json@ if you want to send a different message
-- for each protocol. For example, using one publish action, you can send a
-- short message to your SMS subscribers and a longer message to your email
-- subscribers. If you set @MessageStructure@ to @json@, the value of the
-- @Message@ parameter must:
--
-- -   be a syntactically valid JSON object; and
--
-- -   contain at least a top-level JSON key of \"default\" with a value
--     that is a string.
--
-- You can define other top-level keys that define the message you want to
-- send to a specific transport protocol (e.g. http).
publishBatchRequestEntry_messageStructure :: Lens.Lens' PublishBatchRequestEntry (Prelude.Maybe Prelude.Text)
publishBatchRequestEntry_messageStructure = Lens.lens (\PublishBatchRequestEntry' {messageStructure} -> messageStructure) (\s@PublishBatchRequestEntry' {} a -> s {messageStructure = a} :: PublishBatchRequestEntry)

-- | The subject of the batch message.
publishBatchRequestEntry_subject :: Lens.Lens' PublishBatchRequestEntry (Prelude.Maybe Prelude.Text)
publishBatchRequestEntry_subject = Lens.lens (\PublishBatchRequestEntry' {subject} -> subject) (\s@PublishBatchRequestEntry' {} a -> s {subject = a} :: PublishBatchRequestEntry)

-- | An identifier for the message in this batch.
--
-- The @Ids@ of a batch request must be unique within a request.
--
-- This identifier can have up to 80 characters. The following characters
-- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
publishBatchRequestEntry_id :: Lens.Lens' PublishBatchRequestEntry Prelude.Text
publishBatchRequestEntry_id = Lens.lens (\PublishBatchRequestEntry' {id} -> id) (\s@PublishBatchRequestEntry' {} a -> s {id = a} :: PublishBatchRequestEntry)

-- | The body of the message.
publishBatchRequestEntry_message :: Lens.Lens' PublishBatchRequestEntry Prelude.Text
publishBatchRequestEntry_message = Lens.lens (\PublishBatchRequestEntry' {message} -> message) (\s@PublishBatchRequestEntry' {} a -> s {message = a} :: PublishBatchRequestEntry)

instance Prelude.Hashable PublishBatchRequestEntry where
  hashWithSalt _salt PublishBatchRequestEntry' {..} =
    _salt
      `Prelude.hashWithSalt` messageAttributes
      `Prelude.hashWithSalt` messageDeduplicationId
      `Prelude.hashWithSalt` messageGroupId
      `Prelude.hashWithSalt` messageStructure
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` message

instance Prelude.NFData PublishBatchRequestEntry where
  rnf PublishBatchRequestEntry' {..} =
    Prelude.rnf messageAttributes
      `Prelude.seq` Prelude.rnf messageDeduplicationId
      `Prelude.seq` Prelude.rnf messageGroupId
      `Prelude.seq` Prelude.rnf messageStructure
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf message

instance Data.ToQuery PublishBatchRequestEntry where
  toQuery PublishBatchRequestEntry' {..} =
    Prelude.mconcat
      [ "MessageAttributes"
          Data.=: Data.toQuery
            ( Data.toQueryMap "entry" "Name" "Value"
                Prelude.<$> messageAttributes
            ),
        "MessageDeduplicationId"
          Data.=: messageDeduplicationId,
        "MessageGroupId" Data.=: messageGroupId,
        "MessageStructure" Data.=: messageStructure,
        "Subject" Data.=: subject,
        "Id" Data.=: id,
        "Message" Data.=: message
      ]
