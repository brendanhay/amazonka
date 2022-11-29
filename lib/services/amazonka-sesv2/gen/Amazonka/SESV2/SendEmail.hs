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
-- Module      : Amazonka.SESV2.SendEmail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an email message. You can use the Amazon SES API v2 to send the
-- following types of messages:
--
-- -   __Simple__ – A standard email message. When you create this type of
--     message, you specify the sender, the recipient, and the message
--     body, and Amazon SES assembles the message for you.
--
-- -   __Raw__ – A raw, MIME-formatted email message. When you send this
--     type of email, you have to specify all of the message headers, as
--     well as the message body. You can use this message type to send
--     messages that contain attachments. The message that you specify has
--     to be a valid MIME message.
--
-- -   __Templated__ – A message that contains personalization tags. When
--     you send this type of email, Amazon SES API v2 automatically
--     replaces the tags with values that you specify.
module Amazonka.SESV2.SendEmail
  ( -- * Creating a Request
    SendEmail (..),
    newSendEmail,

    -- * Request Lenses
    sendEmail_destination,
    sendEmail_feedbackForwardingEmailAddressIdentityArn,
    sendEmail_replyToAddresses,
    sendEmail_emailTags,
    sendEmail_fromEmailAddress,
    sendEmail_configurationSetName,
    sendEmail_fromEmailAddressIdentityArn,
    sendEmail_feedbackForwardingEmailAddress,
    sendEmail_listManagementOptions,
    sendEmail_content,

    -- * Destructuring the Response
    SendEmailResponse (..),
    newSendEmailResponse,

    -- * Response Lenses
    sendEmailResponse_messageId,
    sendEmailResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Represents a request to send a single formatted email using Amazon SES.
-- For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-formatted.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSendEmail' smart constructor.
data SendEmail = SendEmail'
  { -- | An object that contains the recipients of the email message.
    destination :: Prelude.Maybe Destination,
    -- | This parameter is used only for sending authorization. It is the ARN of
    -- the identity that is associated with the sending authorization policy
    -- that permits you to use the email address specified in the
    -- @FeedbackForwardingEmailAddress@ parameter.
    --
    -- For example, if the owner of example.com (which has ARN
    -- arn:aws:ses:us-east-1:123456789012:identity\/example.com) attaches a
    -- policy to it that authorizes you to use feedback\@example.com, then you
    -- would specify the @FeedbackForwardingEmailAddressIdentityArn@ to be
    -- arn:aws:ses:us-east-1:123456789012:identity\/example.com, and the
    -- @FeedbackForwardingEmailAddress@ to be feedback\@example.com.
    --
    -- For more information about sending authorization, see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
    feedbackForwardingEmailAddressIdentityArn :: Prelude.Maybe Prelude.Text,
    -- | The \"Reply-to\" email addresses for the message. When the recipient
    -- replies to the message, each Reply-to address receives the reply.
    replyToAddresses :: Prelude.Maybe [Prelude.Text],
    -- | A list of tags, in the form of name\/value pairs, to apply to an email
    -- that you send using the @SendEmail@ operation. Tags correspond to
    -- characteristics of the email that you define, so that you can publish
    -- email sending events.
    emailTags :: Prelude.Maybe [MessageTag],
    -- | The email address to use as the \"From\" address for the email. The
    -- address that you specify has to be verified.
    fromEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set to use when sending the email.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | This parameter is used only for sending authorization. It is the ARN of
    -- the identity that is associated with the sending authorization policy
    -- that permits you to use the email address specified in the
    -- @FromEmailAddress@ parameter.
    --
    -- For example, if the owner of example.com (which has ARN
    -- arn:aws:ses:us-east-1:123456789012:identity\/example.com) attaches a
    -- policy to it that authorizes you to use sender\@example.com, then you
    -- would specify the @FromEmailAddressIdentityArn@ to be
    -- arn:aws:ses:us-east-1:123456789012:identity\/example.com, and the
    -- @FromEmailAddress@ to be sender\@example.com.
    --
    -- For more information about sending authorization, see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
    --
    -- For Raw emails, the @FromEmailAddressIdentityArn@ value overrides the
    -- X-SES-SOURCE-ARN and X-SES-FROM-ARN headers specified in raw email
    -- message content.
    fromEmailAddressIdentityArn :: Prelude.Maybe Prelude.Text,
    -- | The address that you want bounce and complaint notifications to be sent
    -- to.
    feedbackForwardingEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | An object used to specify a list or topic to which an email belongs,
    -- which will be used when a contact chooses to unsubscribe.
    listManagementOptions :: Prelude.Maybe ListManagementOptions,
    -- | An object that contains the body of the message. You can send either a
    -- Simple message Raw message or a template Message.
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
-- 'destination', 'sendEmail_destination' - An object that contains the recipients of the email message.
--
-- 'feedbackForwardingEmailAddressIdentityArn', 'sendEmail_feedbackForwardingEmailAddressIdentityArn' - This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to use the email address specified in the
-- @FeedbackForwardingEmailAddress@ parameter.
--
-- For example, if the owner of example.com (which has ARN
-- arn:aws:ses:us-east-1:123456789012:identity\/example.com) attaches a
-- policy to it that authorizes you to use feedback\@example.com, then you
-- would specify the @FeedbackForwardingEmailAddressIdentityArn@ to be
-- arn:aws:ses:us-east-1:123456789012:identity\/example.com, and the
-- @FeedbackForwardingEmailAddress@ to be feedback\@example.com.
--
-- For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- 'replyToAddresses', 'sendEmail_replyToAddresses' - The \"Reply-to\" email addresses for the message. When the recipient
-- replies to the message, each Reply-to address receives the reply.
--
-- 'emailTags', 'sendEmail_emailTags' - A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using the @SendEmail@ operation. Tags correspond to
-- characteristics of the email that you define, so that you can publish
-- email sending events.
--
-- 'fromEmailAddress', 'sendEmail_fromEmailAddress' - The email address to use as the \"From\" address for the email. The
-- address that you specify has to be verified.
--
-- 'configurationSetName', 'sendEmail_configurationSetName' - The name of the configuration set to use when sending the email.
--
-- 'fromEmailAddressIdentityArn', 'sendEmail_fromEmailAddressIdentityArn' - This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to use the email address specified in the
-- @FromEmailAddress@ parameter.
--
-- For example, if the owner of example.com (which has ARN
-- arn:aws:ses:us-east-1:123456789012:identity\/example.com) attaches a
-- policy to it that authorizes you to use sender\@example.com, then you
-- would specify the @FromEmailAddressIdentityArn@ to be
-- arn:aws:ses:us-east-1:123456789012:identity\/example.com, and the
-- @FromEmailAddress@ to be sender\@example.com.
--
-- For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- For Raw emails, the @FromEmailAddressIdentityArn@ value overrides the
-- X-SES-SOURCE-ARN and X-SES-FROM-ARN headers specified in raw email
-- message content.
--
-- 'feedbackForwardingEmailAddress', 'sendEmail_feedbackForwardingEmailAddress' - The address that you want bounce and complaint notifications to be sent
-- to.
--
-- 'listManagementOptions', 'sendEmail_listManagementOptions' - An object used to specify a list or topic to which an email belongs,
-- which will be used when a contact chooses to unsubscribe.
--
-- 'content', 'sendEmail_content' - An object that contains the body of the message. You can send either a
-- Simple message Raw message or a template Message.
newSendEmail ::
  -- | 'content'
  EmailContent ->
  SendEmail
newSendEmail pContent_ =
  SendEmail'
    { destination = Prelude.Nothing,
      feedbackForwardingEmailAddressIdentityArn =
        Prelude.Nothing,
      replyToAddresses = Prelude.Nothing,
      emailTags = Prelude.Nothing,
      fromEmailAddress = Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      fromEmailAddressIdentityArn = Prelude.Nothing,
      feedbackForwardingEmailAddress = Prelude.Nothing,
      listManagementOptions = Prelude.Nothing,
      content = pContent_
    }

-- | An object that contains the recipients of the email message.
sendEmail_destination :: Lens.Lens' SendEmail (Prelude.Maybe Destination)
sendEmail_destination = Lens.lens (\SendEmail' {destination} -> destination) (\s@SendEmail' {} a -> s {destination = a} :: SendEmail)

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to use the email address specified in the
-- @FeedbackForwardingEmailAddress@ parameter.
--
-- For example, if the owner of example.com (which has ARN
-- arn:aws:ses:us-east-1:123456789012:identity\/example.com) attaches a
-- policy to it that authorizes you to use feedback\@example.com, then you
-- would specify the @FeedbackForwardingEmailAddressIdentityArn@ to be
-- arn:aws:ses:us-east-1:123456789012:identity\/example.com, and the
-- @FeedbackForwardingEmailAddress@ to be feedback\@example.com.
--
-- For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
sendEmail_feedbackForwardingEmailAddressIdentityArn :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_feedbackForwardingEmailAddressIdentityArn = Lens.lens (\SendEmail' {feedbackForwardingEmailAddressIdentityArn} -> feedbackForwardingEmailAddressIdentityArn) (\s@SendEmail' {} a -> s {feedbackForwardingEmailAddressIdentityArn = a} :: SendEmail)

-- | The \"Reply-to\" email addresses for the message. When the recipient
-- replies to the message, each Reply-to address receives the reply.
sendEmail_replyToAddresses :: Lens.Lens' SendEmail (Prelude.Maybe [Prelude.Text])
sendEmail_replyToAddresses = Lens.lens (\SendEmail' {replyToAddresses} -> replyToAddresses) (\s@SendEmail' {} a -> s {replyToAddresses = a} :: SendEmail) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using the @SendEmail@ operation. Tags correspond to
-- characteristics of the email that you define, so that you can publish
-- email sending events.
sendEmail_emailTags :: Lens.Lens' SendEmail (Prelude.Maybe [MessageTag])
sendEmail_emailTags = Lens.lens (\SendEmail' {emailTags} -> emailTags) (\s@SendEmail' {} a -> s {emailTags = a} :: SendEmail) Prelude.. Lens.mapping Lens.coerced

-- | The email address to use as the \"From\" address for the email. The
-- address that you specify has to be verified.
sendEmail_fromEmailAddress :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_fromEmailAddress = Lens.lens (\SendEmail' {fromEmailAddress} -> fromEmailAddress) (\s@SendEmail' {} a -> s {fromEmailAddress = a} :: SendEmail)

-- | The name of the configuration set to use when sending the email.
sendEmail_configurationSetName :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_configurationSetName = Lens.lens (\SendEmail' {configurationSetName} -> configurationSetName) (\s@SendEmail' {} a -> s {configurationSetName = a} :: SendEmail)

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to use the email address specified in the
-- @FromEmailAddress@ parameter.
--
-- For example, if the owner of example.com (which has ARN
-- arn:aws:ses:us-east-1:123456789012:identity\/example.com) attaches a
-- policy to it that authorizes you to use sender\@example.com, then you
-- would specify the @FromEmailAddressIdentityArn@ to be
-- arn:aws:ses:us-east-1:123456789012:identity\/example.com, and the
-- @FromEmailAddress@ to be sender\@example.com.
--
-- For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- For Raw emails, the @FromEmailAddressIdentityArn@ value overrides the
-- X-SES-SOURCE-ARN and X-SES-FROM-ARN headers specified in raw email
-- message content.
sendEmail_fromEmailAddressIdentityArn :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_fromEmailAddressIdentityArn = Lens.lens (\SendEmail' {fromEmailAddressIdentityArn} -> fromEmailAddressIdentityArn) (\s@SendEmail' {} a -> s {fromEmailAddressIdentityArn = a} :: SendEmail)

-- | The address that you want bounce and complaint notifications to be sent
-- to.
sendEmail_feedbackForwardingEmailAddress :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_feedbackForwardingEmailAddress = Lens.lens (\SendEmail' {feedbackForwardingEmailAddress} -> feedbackForwardingEmailAddress) (\s@SendEmail' {} a -> s {feedbackForwardingEmailAddress = a} :: SendEmail)

-- | An object used to specify a list or topic to which an email belongs,
-- which will be used when a contact chooses to unsubscribe.
sendEmail_listManagementOptions :: Lens.Lens' SendEmail (Prelude.Maybe ListManagementOptions)
sendEmail_listManagementOptions = Lens.lens (\SendEmail' {listManagementOptions} -> listManagementOptions) (\s@SendEmail' {} a -> s {listManagementOptions = a} :: SendEmail)

-- | An object that contains the body of the message. You can send either a
-- Simple message Raw message or a template Message.
sendEmail_content :: Lens.Lens' SendEmail EmailContent
sendEmail_content = Lens.lens (\SendEmail' {content} -> content) (\s@SendEmail' {} a -> s {content = a} :: SendEmail)

instance Core.AWSRequest SendEmail where
  type AWSResponse SendEmail = SendEmailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendEmailResponse'
            Prelude.<$> (x Core..?> "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendEmail where
  hashWithSalt _salt SendEmail' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` feedbackForwardingEmailAddressIdentityArn
      `Prelude.hashWithSalt` replyToAddresses
      `Prelude.hashWithSalt` emailTags
      `Prelude.hashWithSalt` fromEmailAddress
      `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` fromEmailAddressIdentityArn
      `Prelude.hashWithSalt` feedbackForwardingEmailAddress
      `Prelude.hashWithSalt` listManagementOptions
      `Prelude.hashWithSalt` content

instance Prelude.NFData SendEmail where
  rnf SendEmail' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf feedbackForwardingEmailAddressIdentityArn
      `Prelude.seq` Prelude.rnf replyToAddresses
      `Prelude.seq` Prelude.rnf emailTags
      `Prelude.seq` Prelude.rnf fromEmailAddress
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf fromEmailAddressIdentityArn
      `Prelude.seq` Prelude.rnf feedbackForwardingEmailAddress
      `Prelude.seq` Prelude.rnf listManagementOptions
      `Prelude.seq` Prelude.rnf content

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
          [ ("Destination" Core..=) Prelude.<$> destination,
            ("FeedbackForwardingEmailAddressIdentityArn" Core..=)
              Prelude.<$> feedbackForwardingEmailAddressIdentityArn,
            ("ReplyToAddresses" Core..=)
              Prelude.<$> replyToAddresses,
            ("EmailTags" Core..=) Prelude.<$> emailTags,
            ("FromEmailAddress" Core..=)
              Prelude.<$> fromEmailAddress,
            ("ConfigurationSetName" Core..=)
              Prelude.<$> configurationSetName,
            ("FromEmailAddressIdentityArn" Core..=)
              Prelude.<$> fromEmailAddressIdentityArn,
            ("FeedbackForwardingEmailAddress" Core..=)
              Prelude.<$> feedbackForwardingEmailAddress,
            ("ListManagementOptions" Core..=)
              Prelude.<$> listManagementOptions,
            Prelude.Just ("Content" Core..= content)
          ]
      )

instance Core.ToPath SendEmail where
  toPath = Prelude.const "/v2/email/outbound-emails"

instance Core.ToQuery SendEmail where
  toQuery = Prelude.const Prelude.mempty

-- | A unique message ID that you receive when an email is accepted for
-- sending.
--
-- /See:/ 'newSendEmailResponse' smart constructor.
data SendEmailResponse = SendEmailResponse'
  { -- | A unique identifier for the message that is generated when the message
    -- is accepted.
    --
    -- It\'s possible for Amazon SES to accept a message without sending it.
    -- This can happen when the message that you\'re trying to send has an
    -- attachment contains a virus, or when you send a templated email that
    -- contains invalid personalization content, for example.
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
-- 'messageId', 'sendEmailResponse_messageId' - A unique identifier for the message that is generated when the message
-- is accepted.
--
-- It\'s possible for Amazon SES to accept a message without sending it.
-- This can happen when the message that you\'re trying to send has an
-- attachment contains a virus, or when you send a templated email that
-- contains invalid personalization content, for example.
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

-- | A unique identifier for the message that is generated when the message
-- is accepted.
--
-- It\'s possible for Amazon SES to accept a message without sending it.
-- This can happen when the message that you\'re trying to send has an
-- attachment contains a virus, or when you send a templated email that
-- contains invalid personalization content, for example.
sendEmailResponse_messageId :: Lens.Lens' SendEmailResponse (Prelude.Maybe Prelude.Text)
sendEmailResponse_messageId = Lens.lens (\SendEmailResponse' {messageId} -> messageId) (\s@SendEmailResponse' {} a -> s {messageId = a} :: SendEmailResponse)

-- | The response's http status code.
sendEmailResponse_httpStatus :: Lens.Lens' SendEmailResponse Prelude.Int
sendEmailResponse_httpStatus = Lens.lens (\SendEmailResponse' {httpStatus} -> httpStatus) (\s@SendEmailResponse' {} a -> s {httpStatus = a} :: SendEmailResponse)

instance Prelude.NFData SendEmailResponse where
  rnf SendEmailResponse' {..} =
    Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf httpStatus
