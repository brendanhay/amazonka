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
-- Module      : Network.AWS.PinpointEmail.SendEmail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an email message. You can use the Amazon Pinpoint Email API to
-- send two types of messages:
--
-- -   __Simple__ – A standard email message. When you create this type of
--     message, you specify the sender, the recipient, and the message
--     body, and Amazon Pinpoint assembles the message for you.
--
-- -   __Raw__ – A raw, MIME-formatted email message. When you send this
--     type of email, you have to specify all of the message headers, as
--     well as the message body. You can use this message type to send
--     messages that contain attachments. The message that you specify has
--     to be a valid MIME message.
module Network.AWS.PinpointEmail.SendEmail
  ( -- * Creating a Request
    SendEmail (..),
    newSendEmail,

    -- * Request Lenses
    sendEmail_fromEmailAddress,
    sendEmail_configurationSetName,
    sendEmail_emailTags,
    sendEmail_feedbackForwardingEmailAddress,
    sendEmail_replyToAddresses,
    sendEmail_destination,
    sendEmail_content,

    -- * Destructuring the Response
    SendEmailResponse (..),
    newSendEmailResponse,

    -- * Response Lenses
    sendEmailResponse_messageId,
    sendEmailResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.PinpointEmail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to send an email message.
--
-- /See:/ 'newSendEmail' smart constructor.
data SendEmail = SendEmail'
  { -- | The email address that you want to use as the \"From\" address for the
    -- email. The address that you specify has to be verified.
    fromEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set that you want to use when sending the
    -- email.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags, in the form of name\/value pairs, to apply to an email
    -- that you send using the @SendEmail@ operation. Tags correspond to
    -- characteristics of the email that you define, so that you can publish
    -- email sending events.
    emailTags :: Prelude.Maybe [MessageTag],
    -- | The address that Amazon Pinpoint should send bounce and complaint
    -- notifications to.
    feedbackForwardingEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The \"Reply-to\" email addresses for the message. When the recipient
    -- replies to the message, each Reply-to address receives the reply.
    replyToAddresses :: Prelude.Maybe [Prelude.Text],
    -- | An object that contains the recipients of the email message.
    destination :: Destination,
    -- | An object that contains the body of the message. You can send either a
    -- Simple message or a Raw message.
    content :: EmailContent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendEmail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromEmailAddress', 'sendEmail_fromEmailAddress' - The email address that you want to use as the \"From\" address for the
-- email. The address that you specify has to be verified.
--
-- 'configurationSetName', 'sendEmail_configurationSetName' - The name of the configuration set that you want to use when sending the
-- email.
--
-- 'emailTags', 'sendEmail_emailTags' - A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using the @SendEmail@ operation. Tags correspond to
-- characteristics of the email that you define, so that you can publish
-- email sending events.
--
-- 'feedbackForwardingEmailAddress', 'sendEmail_feedbackForwardingEmailAddress' - The address that Amazon Pinpoint should send bounce and complaint
-- notifications to.
--
-- 'replyToAddresses', 'sendEmail_replyToAddresses' - The \"Reply-to\" email addresses for the message. When the recipient
-- replies to the message, each Reply-to address receives the reply.
--
-- 'destination', 'sendEmail_destination' - An object that contains the recipients of the email message.
--
-- 'content', 'sendEmail_content' - An object that contains the body of the message. You can send either a
-- Simple message or a Raw message.
newSendEmail ::
  -- | 'destination'
  Destination ->
  -- | 'content'
  EmailContent ->
  SendEmail
newSendEmail pDestination_ pContent_ =
  SendEmail'
    { fromEmailAddress = Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      emailTags = Prelude.Nothing,
      feedbackForwardingEmailAddress = Prelude.Nothing,
      replyToAddresses = Prelude.Nothing,
      destination = pDestination_,
      content = pContent_
    }

-- | The email address that you want to use as the \"From\" address for the
-- email. The address that you specify has to be verified.
sendEmail_fromEmailAddress :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_fromEmailAddress = Lens.lens (\SendEmail' {fromEmailAddress} -> fromEmailAddress) (\s@SendEmail' {} a -> s {fromEmailAddress = a} :: SendEmail)

-- | The name of the configuration set that you want to use when sending the
-- email.
sendEmail_configurationSetName :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_configurationSetName = Lens.lens (\SendEmail' {configurationSetName} -> configurationSetName) (\s@SendEmail' {} a -> s {configurationSetName = a} :: SendEmail)

-- | A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using the @SendEmail@ operation. Tags correspond to
-- characteristics of the email that you define, so that you can publish
-- email sending events.
sendEmail_emailTags :: Lens.Lens' SendEmail (Prelude.Maybe [MessageTag])
sendEmail_emailTags = Lens.lens (\SendEmail' {emailTags} -> emailTags) (\s@SendEmail' {} a -> s {emailTags = a} :: SendEmail) Prelude.. Lens.mapping Lens.coerced

-- | The address that Amazon Pinpoint should send bounce and complaint
-- notifications to.
sendEmail_feedbackForwardingEmailAddress :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_feedbackForwardingEmailAddress = Lens.lens (\SendEmail' {feedbackForwardingEmailAddress} -> feedbackForwardingEmailAddress) (\s@SendEmail' {} a -> s {feedbackForwardingEmailAddress = a} :: SendEmail)

-- | The \"Reply-to\" email addresses for the message. When the recipient
-- replies to the message, each Reply-to address receives the reply.
sendEmail_replyToAddresses :: Lens.Lens' SendEmail (Prelude.Maybe [Prelude.Text])
sendEmail_replyToAddresses = Lens.lens (\SendEmail' {replyToAddresses} -> replyToAddresses) (\s@SendEmail' {} a -> s {replyToAddresses = a} :: SendEmail) Prelude.. Lens.mapping Lens.coerced

-- | An object that contains the recipients of the email message.
sendEmail_destination :: Lens.Lens' SendEmail Destination
sendEmail_destination = Lens.lens (\SendEmail' {destination} -> destination) (\s@SendEmail' {} a -> s {destination = a} :: SendEmail)

-- | An object that contains the body of the message. You can send either a
-- Simple message or a Raw message.
sendEmail_content :: Lens.Lens' SendEmail EmailContent
sendEmail_content = Lens.lens (\SendEmail' {content} -> content) (\s@SendEmail' {} a -> s {content = a} :: SendEmail)

instance Core.AWSRequest SendEmail where
  type AWSResponse SendEmail = SendEmailResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SendEmailResponse'
            Prelude.<$> (x Core..?> "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendEmail

instance Prelude.NFData SendEmail

instance Core.ToHeaders SendEmail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SendEmail where
  toJSON SendEmail' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FromEmailAddress" Core..=)
              Prelude.<$> fromEmailAddress,
            ("ConfigurationSetName" Core..=)
              Prelude.<$> configurationSetName,
            ("EmailTags" Core..=) Prelude.<$> emailTags,
            ("FeedbackForwardingEmailAddress" Core..=)
              Prelude.<$> feedbackForwardingEmailAddress,
            ("ReplyToAddresses" Core..=)
              Prelude.<$> replyToAddresses,
            Prelude.Just ("Destination" Core..= destination),
            Prelude.Just ("Content" Core..= content)
          ]
      )

instance Core.ToPath SendEmail where
  toPath = Prelude.const "/v1/email/outbound-emails"

instance Core.ToQuery SendEmail where
  toQuery = Prelude.const Prelude.mempty

-- | A unique message ID that you receive when Amazon Pinpoint accepts an
-- email for sending.
--
-- /See:/ 'newSendEmailResponse' smart constructor.
data SendEmailResponse = SendEmailResponse'
  { -- | A unique identifier for the message that is generated when Amazon
    -- Pinpoint accepts the message.
    --
    -- It is possible for Amazon Pinpoint to accept a message without sending
    -- it. This can happen when the message you\'re trying to send has an
    -- attachment doesn\'t pass a virus check, or when you send a templated
    -- email that contains invalid personalization content, for example.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendEmailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'sendEmailResponse_messageId' - A unique identifier for the message that is generated when Amazon
-- Pinpoint accepts the message.
--
-- It is possible for Amazon Pinpoint to accept a message without sending
-- it. This can happen when the message you\'re trying to send has an
-- attachment doesn\'t pass a virus check, or when you send a templated
-- email that contains invalid personalization content, for example.
--
-- 'httpStatus', 'sendEmailResponse_httpStatus' - The response's http status code.
newSendEmailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendEmailResponse
newSendEmailResponse pHttpStatus_ =
  SendEmailResponse'
    { messageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the message that is generated when Amazon
-- Pinpoint accepts the message.
--
-- It is possible for Amazon Pinpoint to accept a message without sending
-- it. This can happen when the message you\'re trying to send has an
-- attachment doesn\'t pass a virus check, or when you send a templated
-- email that contains invalid personalization content, for example.
sendEmailResponse_messageId :: Lens.Lens' SendEmailResponse (Prelude.Maybe Prelude.Text)
sendEmailResponse_messageId = Lens.lens (\SendEmailResponse' {messageId} -> messageId) (\s@SendEmailResponse' {} a -> s {messageId = a} :: SendEmailResponse)

-- | The response's http status code.
sendEmailResponse_httpStatus :: Lens.Lens' SendEmailResponse Prelude.Int
sendEmailResponse_httpStatus = Lens.lens (\SendEmailResponse' {httpStatus} -> httpStatus) (\s@SendEmailResponse' {} a -> s {httpStatus = a} :: SendEmailResponse)

instance Prelude.NFData SendEmailResponse
