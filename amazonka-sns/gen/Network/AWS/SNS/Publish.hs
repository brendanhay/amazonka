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
-- Module      : Network.AWS.SNS.Publish
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a message to an Amazon SNS topic, a text message (SMS message)
-- directly to a phone number, or a message to a mobile platform endpoint
-- (when you specify the @TargetArn@).
--
-- If you send a message to a topic, Amazon SNS delivers the message to
-- each endpoint that is subscribed to the topic. The format of the message
-- depends on the notification protocol for each subscribed endpoint.
--
-- When a @messageId@ is returned, the message has been saved and Amazon
-- SNS will attempt to deliver it shortly.
--
-- To use the @Publish@ action for sending a message to a mobile endpoint,
-- such as an app on a Kindle device or mobile phone, you must specify the
-- EndpointArn for the TargetArn parameter. The EndpointArn is returned
-- when making a call with the @CreatePlatformEndpoint@ action.
--
-- For more information about formatting messages, see
-- <https://docs.aws.amazon.com/sns/latest/dg/mobile-push-send-custommessage.html Send Custom Platform-Specific Payloads in Messages to Mobile Devices>.
--
-- You can publish messages only to topics and endpoints in the same AWS
-- Region.
module Network.AWS.SNS.Publish
  ( -- * Creating a Request
    Publish (..),
    newPublish,

    -- * Request Lenses
    publish_phoneNumber,
    publish_messageStructure,
    publish_messageDeduplicationId,
    publish_messageAttributes,
    publish_targetArn,
    publish_subject,
    publish_topicArn,
    publish_messageGroupId,
    publish_message,

    -- * Destructuring the Response
    PublishResponse (..),
    newPublishResponse,

    -- * Response Lenses
    publishResponse_sequenceNumber,
    publishResponse_messageId,
    publishResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for Publish action.
--
-- /See:/ 'newPublish' smart constructor.
data Publish = Publish'
  { -- | The phone number to which you want to deliver an SMS message. Use E.164
    -- format.
    --
    -- If you don\'t specify a value for the @PhoneNumber@ parameter, you must
    -- specify a value for the @TargetArn@ or @TopicArn@ parameters.
    phoneNumber :: Prelude.Maybe Prelude.Text,
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
    -- send to a specific transport protocol (e.g., \"http\").
    --
    -- Valid value: @json@
    messageStructure :: Prelude.Maybe Prelude.Text,
    -- | This parameter applies only to FIFO (first-in-first-out) topics. The
    -- @MessageDeduplicationId@ can contain up to 128 alphanumeric characters
    -- (a-z, A-Z, 0-9) and punctuation
    -- @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
    --
    -- Every message must have a unique @MessageDeduplicationId@, which is a
    -- token used for deduplication of sent messages. If a message with a
    -- particular @MessageDeduplicationId@ is sent successfully, any message
    -- sent with the same @MessageDeduplicationId@ during the 5-minute
    -- deduplication interval is treated as a duplicate.
    --
    -- If the topic has @ContentBasedDeduplication@ set, the system generates a
    -- @MessageDeduplicationId@ based on the contents of the message. Your
    -- @MessageDeduplicationId@ overrides the generated one.
    messageDeduplicationId :: Prelude.Maybe Prelude.Text,
    -- | Message attributes for Publish action.
    messageAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue),
    -- | If you don\'t specify a value for the @TargetArn@ parameter, you must
    -- specify a value for the @PhoneNumber@ or @TopicArn@ parameters.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | Optional parameter to be used as the \"Subject\" line when the message
    -- is delivered to email endpoints. This field will also be included, if
    -- present, in the standard JSON messages delivered to other endpoints.
    --
    -- Constraints: Subjects must be ASCII text that begins with a letter,
    -- number, or punctuation mark; must not include line breaks or control
    -- characters; and must be less than 100 characters long.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The topic you want to publish to.
    --
    -- If you don\'t specify a value for the @TopicArn@ parameter, you must
    -- specify a value for the @PhoneNumber@ or @TargetArn@ parameters.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | This parameter applies only to FIFO (first-in-first-out) topics. The
    -- @MessageGroupId@ can contain up to 128 alphanumeric characters (a-z,
    -- A-Z, 0-9) and punctuation @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
    --
    -- The @MessageGroupId@ is a tag that specifies that a message belongs to a
    -- specific message group. Messages that belong to the same message group
    -- are processed in a FIFO manner (however, messages in different message
    -- groups might be processed out of order). Every message must include a
    -- @MessageGroupId@.
    messageGroupId :: Prelude.Maybe Prelude.Text,
    -- | The message you want to send.
    --
    -- If you are publishing to a topic and you want to send the same message
    -- to all transport protocols, include the text of the message as a String
    -- value. If you want to send different messages for each transport
    -- protocol, set the value of the @MessageStructure@ parameter to @json@
    -- and use a JSON object for the @Message@ parameter.
    --
    -- Constraints:
    --
    -- -   With the exception of SMS, messages must be UTF-8 encoded strings
    --     and at most 256 KB in size (262,144 bytes, not 262,144 characters).
    --
    -- -   For SMS, each message can contain up to 140 characters. This
    --     character limit depends on the encoding schema. For example, an SMS
    --     message can contain 160 GSM characters, 140 ASCII characters, or 70
    --     UCS-2 characters.
    --
    --     If you publish a message that exceeds this size limit, Amazon SNS
    --     sends the message as multiple messages, each fitting within the size
    --     limit. Messages aren\'t truncated mid-word but are cut off at
    --     whole-word boundaries.
    --
    --     The total size limit for a single SMS @Publish@ action is 1,600
    --     characters.
    --
    -- JSON-specific constraints:
    --
    -- -   Keys in the JSON object that correspond to supported transport
    --     protocols must have simple JSON string values.
    --
    -- -   The values will be parsed (unescaped) before they are used in
    --     outgoing messages.
    --
    -- -   Outbound notifications are JSON encoded (meaning that the characters
    --     will be reescaped for sending).
    --
    -- -   Values have a minimum length of 0 (the empty string, \"\", is
    --     allowed).
    --
    -- -   Values have a maximum length bounded by the overall message size
    --     (so, including multiple protocols may limit message sizes).
    --
    -- -   Non-string values will cause the key to be ignored.
    --
    -- -   Keys that do not correspond to supported transport protocols are
    --     ignored.
    --
    -- -   Duplicate keys are not allowed.
    --
    -- -   Failure to parse or validate any key or value in the message will
    --     cause the @Publish@ call to return an error (no partial delivery).
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Publish' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'publish_phoneNumber' - The phone number to which you want to deliver an SMS message. Use E.164
-- format.
--
-- If you don\'t specify a value for the @PhoneNumber@ parameter, you must
-- specify a value for the @TargetArn@ or @TopicArn@ parameters.
--
-- 'messageStructure', 'publish_messageStructure' - Set @MessageStructure@ to @json@ if you want to send a different message
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
-- send to a specific transport protocol (e.g., \"http\").
--
-- Valid value: @json@
--
-- 'messageDeduplicationId', 'publish_messageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) topics. The
-- @MessageDeduplicationId@ can contain up to 128 alphanumeric characters
-- (a-z, A-Z, 0-9) and punctuation
-- @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
--
-- Every message must have a unique @MessageDeduplicationId@, which is a
-- token used for deduplication of sent messages. If a message with a
-- particular @MessageDeduplicationId@ is sent successfully, any message
-- sent with the same @MessageDeduplicationId@ during the 5-minute
-- deduplication interval is treated as a duplicate.
--
-- If the topic has @ContentBasedDeduplication@ set, the system generates a
-- @MessageDeduplicationId@ based on the contents of the message. Your
-- @MessageDeduplicationId@ overrides the generated one.
--
-- 'messageAttributes', 'publish_messageAttributes' - Message attributes for Publish action.
--
-- 'targetArn', 'publish_targetArn' - If you don\'t specify a value for the @TargetArn@ parameter, you must
-- specify a value for the @PhoneNumber@ or @TopicArn@ parameters.
--
-- 'subject', 'publish_subject' - Optional parameter to be used as the \"Subject\" line when the message
-- is delivered to email endpoints. This field will also be included, if
-- present, in the standard JSON messages delivered to other endpoints.
--
-- Constraints: Subjects must be ASCII text that begins with a letter,
-- number, or punctuation mark; must not include line breaks or control
-- characters; and must be less than 100 characters long.
--
-- 'topicArn', 'publish_topicArn' - The topic you want to publish to.
--
-- If you don\'t specify a value for the @TopicArn@ parameter, you must
-- specify a value for the @PhoneNumber@ or @TargetArn@ parameters.
--
-- 'messageGroupId', 'publish_messageGroupId' - This parameter applies only to FIFO (first-in-first-out) topics. The
-- @MessageGroupId@ can contain up to 128 alphanumeric characters (a-z,
-- A-Z, 0-9) and punctuation @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
--
-- The @MessageGroupId@ is a tag that specifies that a message belongs to a
-- specific message group. Messages that belong to the same message group
-- are processed in a FIFO manner (however, messages in different message
-- groups might be processed out of order). Every message must include a
-- @MessageGroupId@.
--
-- 'message', 'publish_message' - The message you want to send.
--
-- If you are publishing to a topic and you want to send the same message
-- to all transport protocols, include the text of the message as a String
-- value. If you want to send different messages for each transport
-- protocol, set the value of the @MessageStructure@ parameter to @json@
-- and use a JSON object for the @Message@ parameter.
--
-- Constraints:
--
-- -   With the exception of SMS, messages must be UTF-8 encoded strings
--     and at most 256 KB in size (262,144 bytes, not 262,144 characters).
--
-- -   For SMS, each message can contain up to 140 characters. This
--     character limit depends on the encoding schema. For example, an SMS
--     message can contain 160 GSM characters, 140 ASCII characters, or 70
--     UCS-2 characters.
--
--     If you publish a message that exceeds this size limit, Amazon SNS
--     sends the message as multiple messages, each fitting within the size
--     limit. Messages aren\'t truncated mid-word but are cut off at
--     whole-word boundaries.
--
--     The total size limit for a single SMS @Publish@ action is 1,600
--     characters.
--
-- JSON-specific constraints:
--
-- -   Keys in the JSON object that correspond to supported transport
--     protocols must have simple JSON string values.
--
-- -   The values will be parsed (unescaped) before they are used in
--     outgoing messages.
--
-- -   Outbound notifications are JSON encoded (meaning that the characters
--     will be reescaped for sending).
--
-- -   Values have a minimum length of 0 (the empty string, \"\", is
--     allowed).
--
-- -   Values have a maximum length bounded by the overall message size
--     (so, including multiple protocols may limit message sizes).
--
-- -   Non-string values will cause the key to be ignored.
--
-- -   Keys that do not correspond to supported transport protocols are
--     ignored.
--
-- -   Duplicate keys are not allowed.
--
-- -   Failure to parse or validate any key or value in the message will
--     cause the @Publish@ call to return an error (no partial delivery).
newPublish ::
  -- | 'message'
  Prelude.Text ->
  Publish
newPublish pMessage_ =
  Publish'
    { phoneNumber = Prelude.Nothing,
      messageStructure = Prelude.Nothing,
      messageDeduplicationId = Prelude.Nothing,
      messageAttributes = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      subject = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      messageGroupId = Prelude.Nothing,
      message = pMessage_
    }

-- | The phone number to which you want to deliver an SMS message. Use E.164
-- format.
--
-- If you don\'t specify a value for the @PhoneNumber@ parameter, you must
-- specify a value for the @TargetArn@ or @TopicArn@ parameters.
publish_phoneNumber :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_phoneNumber = Lens.lens (\Publish' {phoneNumber} -> phoneNumber) (\s@Publish' {} a -> s {phoneNumber = a} :: Publish)

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
-- send to a specific transport protocol (e.g., \"http\").
--
-- Valid value: @json@
publish_messageStructure :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_messageStructure = Lens.lens (\Publish' {messageStructure} -> messageStructure) (\s@Publish' {} a -> s {messageStructure = a} :: Publish)

-- | This parameter applies only to FIFO (first-in-first-out) topics. The
-- @MessageDeduplicationId@ can contain up to 128 alphanumeric characters
-- (a-z, A-Z, 0-9) and punctuation
-- @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
--
-- Every message must have a unique @MessageDeduplicationId@, which is a
-- token used for deduplication of sent messages. If a message with a
-- particular @MessageDeduplicationId@ is sent successfully, any message
-- sent with the same @MessageDeduplicationId@ during the 5-minute
-- deduplication interval is treated as a duplicate.
--
-- If the topic has @ContentBasedDeduplication@ set, the system generates a
-- @MessageDeduplicationId@ based on the contents of the message. Your
-- @MessageDeduplicationId@ overrides the generated one.
publish_messageDeduplicationId :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_messageDeduplicationId = Lens.lens (\Publish' {messageDeduplicationId} -> messageDeduplicationId) (\s@Publish' {} a -> s {messageDeduplicationId = a} :: Publish)

-- | Message attributes for Publish action.
publish_messageAttributes :: Lens.Lens' Publish (Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue))
publish_messageAttributes = Lens.lens (\Publish' {messageAttributes} -> messageAttributes) (\s@Publish' {} a -> s {messageAttributes = a} :: Publish) Prelude.. Lens.mapping Prelude._Coerce

-- | If you don\'t specify a value for the @TargetArn@ parameter, you must
-- specify a value for the @PhoneNumber@ or @TopicArn@ parameters.
publish_targetArn :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_targetArn = Lens.lens (\Publish' {targetArn} -> targetArn) (\s@Publish' {} a -> s {targetArn = a} :: Publish)

-- | Optional parameter to be used as the \"Subject\" line when the message
-- is delivered to email endpoints. This field will also be included, if
-- present, in the standard JSON messages delivered to other endpoints.
--
-- Constraints: Subjects must be ASCII text that begins with a letter,
-- number, or punctuation mark; must not include line breaks or control
-- characters; and must be less than 100 characters long.
publish_subject :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_subject = Lens.lens (\Publish' {subject} -> subject) (\s@Publish' {} a -> s {subject = a} :: Publish)

-- | The topic you want to publish to.
--
-- If you don\'t specify a value for the @TopicArn@ parameter, you must
-- specify a value for the @PhoneNumber@ or @TargetArn@ parameters.
publish_topicArn :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_topicArn = Lens.lens (\Publish' {topicArn} -> topicArn) (\s@Publish' {} a -> s {topicArn = a} :: Publish)

-- | This parameter applies only to FIFO (first-in-first-out) topics. The
-- @MessageGroupId@ can contain up to 128 alphanumeric characters (a-z,
-- A-Z, 0-9) and punctuation @(!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~)@.
--
-- The @MessageGroupId@ is a tag that specifies that a message belongs to a
-- specific message group. Messages that belong to the same message group
-- are processed in a FIFO manner (however, messages in different message
-- groups might be processed out of order). Every message must include a
-- @MessageGroupId@.
publish_messageGroupId :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_messageGroupId = Lens.lens (\Publish' {messageGroupId} -> messageGroupId) (\s@Publish' {} a -> s {messageGroupId = a} :: Publish)

-- | The message you want to send.
--
-- If you are publishing to a topic and you want to send the same message
-- to all transport protocols, include the text of the message as a String
-- value. If you want to send different messages for each transport
-- protocol, set the value of the @MessageStructure@ parameter to @json@
-- and use a JSON object for the @Message@ parameter.
--
-- Constraints:
--
-- -   With the exception of SMS, messages must be UTF-8 encoded strings
--     and at most 256 KB in size (262,144 bytes, not 262,144 characters).
--
-- -   For SMS, each message can contain up to 140 characters. This
--     character limit depends on the encoding schema. For example, an SMS
--     message can contain 160 GSM characters, 140 ASCII characters, or 70
--     UCS-2 characters.
--
--     If you publish a message that exceeds this size limit, Amazon SNS
--     sends the message as multiple messages, each fitting within the size
--     limit. Messages aren\'t truncated mid-word but are cut off at
--     whole-word boundaries.
--
--     The total size limit for a single SMS @Publish@ action is 1,600
--     characters.
--
-- JSON-specific constraints:
--
-- -   Keys in the JSON object that correspond to supported transport
--     protocols must have simple JSON string values.
--
-- -   The values will be parsed (unescaped) before they are used in
--     outgoing messages.
--
-- -   Outbound notifications are JSON encoded (meaning that the characters
--     will be reescaped for sending).
--
-- -   Values have a minimum length of 0 (the empty string, \"\", is
--     allowed).
--
-- -   Values have a maximum length bounded by the overall message size
--     (so, including multiple protocols may limit message sizes).
--
-- -   Non-string values will cause the key to be ignored.
--
-- -   Keys that do not correspond to supported transport protocols are
--     ignored.
--
-- -   Duplicate keys are not allowed.
--
-- -   Failure to parse or validate any key or value in the message will
--     cause the @Publish@ call to return an error (no partial delivery).
publish_message :: Lens.Lens' Publish Prelude.Text
publish_message = Lens.lens (\Publish' {message} -> message) (\s@Publish' {} a -> s {message = a} :: Publish)

instance Prelude.AWSRequest Publish where
  type Rs Publish = PublishResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "PublishResult"
      ( \s h x ->
          PublishResponse'
            Prelude.<$> (x Prelude..@? "SequenceNumber")
            Prelude.<*> (x Prelude..@? "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Publish

instance Prelude.NFData Publish

instance Prelude.ToHeaders Publish where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath Publish where
  toPath = Prelude.const "/"

instance Prelude.ToQuery Publish where
  toQuery Publish' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("Publish" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "PhoneNumber" Prelude.=: phoneNumber,
        "MessageStructure" Prelude.=: messageStructure,
        "MessageDeduplicationId"
          Prelude.=: messageDeduplicationId,
        "MessageAttributes"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryMap "entry" "Name" "Value"
                Prelude.<$> messageAttributes
            ),
        "TargetArn" Prelude.=: targetArn,
        "Subject" Prelude.=: subject,
        "TopicArn" Prelude.=: topicArn,
        "MessageGroupId" Prelude.=: messageGroupId,
        "Message" Prelude.=: message
      ]

-- | Response for Publish action.
--
-- /See:/ 'newPublishResponse' smart constructor.
data PublishResponse = PublishResponse'
  { -- | This response element applies only to FIFO (first-in-first-out) topics.
    --
    -- The sequence number is a large, non-consecutive number that Amazon SNS
    -- assigns to each message. The length of @SequenceNumber@ is 128 bits.
    -- @SequenceNumber@ continues to increase for each @MessageGroupId@.
    sequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier assigned to the published message.
    --
    -- Length Constraint: Maximum 100 characters
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PublishResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceNumber', 'publishResponse_sequenceNumber' - This response element applies only to FIFO (first-in-first-out) topics.
--
-- The sequence number is a large, non-consecutive number that Amazon SNS
-- assigns to each message. The length of @SequenceNumber@ is 128 bits.
-- @SequenceNumber@ continues to increase for each @MessageGroupId@.
--
-- 'messageId', 'publishResponse_messageId' - Unique identifier assigned to the published message.
--
-- Length Constraint: Maximum 100 characters
--
-- 'httpStatus', 'publishResponse_httpStatus' - The response's http status code.
newPublishResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PublishResponse
newPublishResponse pHttpStatus_ =
  PublishResponse'
    { sequenceNumber = Prelude.Nothing,
      messageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This response element applies only to FIFO (first-in-first-out) topics.
--
-- The sequence number is a large, non-consecutive number that Amazon SNS
-- assigns to each message. The length of @SequenceNumber@ is 128 bits.
-- @SequenceNumber@ continues to increase for each @MessageGroupId@.
publishResponse_sequenceNumber :: Lens.Lens' PublishResponse (Prelude.Maybe Prelude.Text)
publishResponse_sequenceNumber = Lens.lens (\PublishResponse' {sequenceNumber} -> sequenceNumber) (\s@PublishResponse' {} a -> s {sequenceNumber = a} :: PublishResponse)

-- | Unique identifier assigned to the published message.
--
-- Length Constraint: Maximum 100 characters
publishResponse_messageId :: Lens.Lens' PublishResponse (Prelude.Maybe Prelude.Text)
publishResponse_messageId = Lens.lens (\PublishResponse' {messageId} -> messageId) (\s@PublishResponse' {} a -> s {messageId = a} :: PublishResponse)

-- | The response's http status code.
publishResponse_httpStatus :: Lens.Lens' PublishResponse Prelude.Int
publishResponse_httpStatus = Lens.lens (\PublishResponse' {httpStatus} -> httpStatus) (\s@PublishResponse' {} a -> s {httpStatus = a} :: PublishResponse)

instance Prelude.NFData PublishResponse
