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
-- Module      : Network.AWS.SES.SendEmail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message and immediately queues it for sending. In
-- order to send email using the @SendEmail@ operation, your message must
-- meet the following requirements:
--
-- -   The message must be sent from a verified email address or domain. If
--     you attempt to send email using a non-verified address or domain,
--     the operation will result in an \"Email address not verified\"
--     error.
--
-- -   If your account is still in the Amazon SES sandbox, you may only
--     send to verified addresses or domains, or to email addresses
--     associated with the Amazon SES Mailbox Simulator. For more
--     information, see
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Verifying Email Addresses and Domains>
--     in the /Amazon SES Developer Guide./
--
-- -   The maximum message size is 10 MB.
--
-- -   The message must include at least one recipient email address. The
--     recipient address can be a To: address, a CC: address, or a BCC:
--     address. If a recipient email address is invalid (that is, it is not
--     in the format /UserName\@[SubDomain.]Domain.TopLevelDomain/), the
--     entire message will be rejected, even if the message contains other
--     recipients that are valid.
--
-- -   The message may not include more than 50 recipients, across the To:,
--     CC: and BCC: fields. If you need to send an email message to a
--     larger audience, you can divide your recipient list into groups of
--     50 or fewer, and then call the @SendEmail@ operation several times
--     to send the message to each group.
--
-- For every message that you send, the total number of recipients
-- (including each recipient in the To:, CC: and BCC: fields) is counted
-- against the maximum number of emails you can send in a 24-hour period
-- (your /sending quota/). For more information about sending quotas in
-- Amazon SES, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html Managing Your Amazon SES Sending Limits>
-- in the /Amazon SES Developer Guide./
module Network.AWS.SES.SendEmail
  ( -- * Creating a Request
    SendEmail (..),
    newSendEmail,

    -- * Request Lenses
    sendEmail_returnPath,
    sendEmail_returnPathArn,
    sendEmail_replyToAddresses,
    sendEmail_tags,
    sendEmail_sourceArn,
    sendEmail_configurationSetName,
    sendEmail_source,
    sendEmail_destination,
    sendEmail_message,

    -- * Destructuring the Response
    SendEmailResponse (..),
    newSendEmailResponse,

    -- * Response Lenses
    sendEmailResponse_httpStatus,
    sendEmailResponse_messageId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to send a single formatted email using Amazon SES.
-- For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-formatted.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSendEmail' smart constructor.
data SendEmail = SendEmail'
  { -- | The email address that bounces and complaints will be forwarded to when
    -- feedback forwarding is enabled. If the message cannot be delivered to
    -- the recipient, then an error message will be returned from the
    -- recipient\'s ISP; this message will then be forwarded to the email
    -- address specified by the @ReturnPath@ parameter. The @ReturnPath@
    -- parameter is never overwritten. This email address must be either
    -- individually verified with Amazon SES, or from a domain that has been
    -- verified with Amazon SES.
    returnPath :: Prelude.Maybe Prelude.Text,
    -- | This parameter is used only for sending authorization. It is the ARN of
    -- the identity that is associated with the sending authorization policy
    -- that permits you to use the email address specified in the @ReturnPath@
    -- parameter.
    --
    -- For example, if the owner of @example.com@ (which has ARN
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@) attaches a
    -- policy to it that authorizes you to use @feedback\@example.com@, then
    -- you would specify the @ReturnPathArn@ to be
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@, and the
    -- @ReturnPath@ to be @feedback\@example.com@.
    --
    -- For more information about sending authorization, see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
    returnPathArn :: Prelude.Maybe Prelude.Text,
    -- | The reply-to email address(es) for the message. If the recipient replies
    -- to the message, each reply-to address will receive the reply.
    replyToAddresses :: Prelude.Maybe [Prelude.Text],
    -- | A list of tags, in the form of name\/value pairs, to apply to an email
    -- that you send using @SendEmail@. Tags correspond to characteristics of
    -- the email that you define, so that you can publish email sending events.
    tags :: Prelude.Maybe [MessageTag],
    -- | This parameter is used only for sending authorization. It is the ARN of
    -- the identity that is associated with the sending authorization policy
    -- that permits you to send for the email address specified in the @Source@
    -- parameter.
    --
    -- For example, if the owner of @example.com@ (which has ARN
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@) attaches a
    -- policy to it that authorizes you to send from @user\@example.com@, then
    -- you would specify the @SourceArn@ to be
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@, and the
    -- @Source@ to be @user\@example.com@.
    --
    -- For more information about sending authorization, see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set to use when you send an email using
    -- @SendEmail@.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The email address that is sending the email. This email address must be
    -- either individually verified with Amazon SES, or from a domain that has
    -- been verified with Amazon SES. For information about verifying
    -- identities, see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide>.
    --
    -- If you are sending on behalf of another user and have been permitted to
    -- do so by a sending authorization policy, then you must also specify the
    -- @SourceArn@ parameter. For more information about sending authorization,
    -- see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
    --
    -- Amazon SES does not support the SMTPUTF8 extension, as described in
    -- <https://tools.ietf.org/html/rfc6531 RFC6531>. For this reason, the
    -- /local part/ of a source email address (the part of the email address
    -- that precedes the \@ sign) may only contain
    -- <https://en.wikipedia.org/wiki/Email_address#Local-part 7-bit ASCII characters>.
    -- If the /domain part/ of an address (the part after the \@ sign) contains
    -- non-ASCII characters, they must be encoded using Punycode, as described
    -- in <https://tools.ietf.org/html/rfc3492.html RFC3492>. The sender name
    -- (also known as the /friendly name/) may contain non-ASCII characters.
    -- These characters must be encoded using MIME encoded-word syntax, as
    -- described in <https://tools.ietf.org/html/rfc2047 RFC 2047>. MIME
    -- encoded-word syntax uses the following form:
    -- @=?charset?encoding?encoded-text?=@.
    source :: Prelude.Text,
    -- | The destination for this email, composed of To:, CC:, and BCC: fields.
    destination :: Destination,
    -- | The message to be sent.
    message :: Message
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendEmail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnPath', 'sendEmail_returnPath' - The email address that bounces and complaints will be forwarded to when
-- feedback forwarding is enabled. If the message cannot be delivered to
-- the recipient, then an error message will be returned from the
-- recipient\'s ISP; this message will then be forwarded to the email
-- address specified by the @ReturnPath@ parameter. The @ReturnPath@
-- parameter is never overwritten. This email address must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
--
-- 'returnPathArn', 'sendEmail_returnPathArn' - This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to use the email address specified in the @ReturnPath@
-- parameter.
--
-- For example, if the owner of @example.com@ (which has ARN
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@) attaches a
-- policy to it that authorizes you to use @feedback\@example.com@, then
-- you would specify the @ReturnPathArn@ to be
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@, and the
-- @ReturnPath@ to be @feedback\@example.com@.
--
-- For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- 'replyToAddresses', 'sendEmail_replyToAddresses' - The reply-to email address(es) for the message. If the recipient replies
-- to the message, each reply-to address will receive the reply.
--
-- 'tags', 'sendEmail_tags' - A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using @SendEmail@. Tags correspond to characteristics of
-- the email that you define, so that you can publish email sending events.
--
-- 'sourceArn', 'sendEmail_sourceArn' - This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to send for the email address specified in the @Source@
-- parameter.
--
-- For example, if the owner of @example.com@ (which has ARN
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@) attaches a
-- policy to it that authorizes you to send from @user\@example.com@, then
-- you would specify the @SourceArn@ to be
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@, and the
-- @Source@ to be @user\@example.com@.
--
-- For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- 'configurationSetName', 'sendEmail_configurationSetName' - The name of the configuration set to use when you send an email using
-- @SendEmail@.
--
-- 'source', 'sendEmail_source' - The email address that is sending the email. This email address must be
-- either individually verified with Amazon SES, or from a domain that has
-- been verified with Amazon SES. For information about verifying
-- identities, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide>.
--
-- If you are sending on behalf of another user and have been permitted to
-- do so by a sending authorization policy, then you must also specify the
-- @SourceArn@ parameter. For more information about sending authorization,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- Amazon SES does not support the SMTPUTF8 extension, as described in
-- <https://tools.ietf.org/html/rfc6531 RFC6531>. For this reason, the
-- /local part/ of a source email address (the part of the email address
-- that precedes the \@ sign) may only contain
-- <https://en.wikipedia.org/wiki/Email_address#Local-part 7-bit ASCII characters>.
-- If the /domain part/ of an address (the part after the \@ sign) contains
-- non-ASCII characters, they must be encoded using Punycode, as described
-- in <https://tools.ietf.org/html/rfc3492.html RFC3492>. The sender name
-- (also known as the /friendly name/) may contain non-ASCII characters.
-- These characters must be encoded using MIME encoded-word syntax, as
-- described in <https://tools.ietf.org/html/rfc2047 RFC 2047>. MIME
-- encoded-word syntax uses the following form:
-- @=?charset?encoding?encoded-text?=@.
--
-- 'destination', 'sendEmail_destination' - The destination for this email, composed of To:, CC:, and BCC: fields.
--
-- 'message', 'sendEmail_message' - The message to be sent.
newSendEmail ::
  -- | 'source'
  Prelude.Text ->
  -- | 'destination'
  Destination ->
  -- | 'message'
  Message ->
  SendEmail
newSendEmail pSource_ pDestination_ pMessage_ =
  SendEmail'
    { returnPath = Prelude.Nothing,
      returnPathArn = Prelude.Nothing,
      replyToAddresses = Prelude.Nothing,
      tags = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      source = pSource_,
      destination = pDestination_,
      message = pMessage_
    }

-- | The email address that bounces and complaints will be forwarded to when
-- feedback forwarding is enabled. If the message cannot be delivered to
-- the recipient, then an error message will be returned from the
-- recipient\'s ISP; this message will then be forwarded to the email
-- address specified by the @ReturnPath@ parameter. The @ReturnPath@
-- parameter is never overwritten. This email address must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
sendEmail_returnPath :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_returnPath = Lens.lens (\SendEmail' {returnPath} -> returnPath) (\s@SendEmail' {} a -> s {returnPath = a} :: SendEmail)

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to use the email address specified in the @ReturnPath@
-- parameter.
--
-- For example, if the owner of @example.com@ (which has ARN
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@) attaches a
-- policy to it that authorizes you to use @feedback\@example.com@, then
-- you would specify the @ReturnPathArn@ to be
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@, and the
-- @ReturnPath@ to be @feedback\@example.com@.
--
-- For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
sendEmail_returnPathArn :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_returnPathArn = Lens.lens (\SendEmail' {returnPathArn} -> returnPathArn) (\s@SendEmail' {} a -> s {returnPathArn = a} :: SendEmail)

-- | The reply-to email address(es) for the message. If the recipient replies
-- to the message, each reply-to address will receive the reply.
sendEmail_replyToAddresses :: Lens.Lens' SendEmail (Prelude.Maybe [Prelude.Text])
sendEmail_replyToAddresses = Lens.lens (\SendEmail' {replyToAddresses} -> replyToAddresses) (\s@SendEmail' {} a -> s {replyToAddresses = a} :: SendEmail) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using @SendEmail@. Tags correspond to characteristics of
-- the email that you define, so that you can publish email sending events.
sendEmail_tags :: Lens.Lens' SendEmail (Prelude.Maybe [MessageTag])
sendEmail_tags = Lens.lens (\SendEmail' {tags} -> tags) (\s@SendEmail' {} a -> s {tags = a} :: SendEmail) Prelude.. Lens.mapping Prelude._Coerce

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to send for the email address specified in the @Source@
-- parameter.
--
-- For example, if the owner of @example.com@ (which has ARN
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@) attaches a
-- policy to it that authorizes you to send from @user\@example.com@, then
-- you would specify the @SourceArn@ to be
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@, and the
-- @Source@ to be @user\@example.com@.
--
-- For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
sendEmail_sourceArn :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_sourceArn = Lens.lens (\SendEmail' {sourceArn} -> sourceArn) (\s@SendEmail' {} a -> s {sourceArn = a} :: SendEmail)

-- | The name of the configuration set to use when you send an email using
-- @SendEmail@.
sendEmail_configurationSetName :: Lens.Lens' SendEmail (Prelude.Maybe Prelude.Text)
sendEmail_configurationSetName = Lens.lens (\SendEmail' {configurationSetName} -> configurationSetName) (\s@SendEmail' {} a -> s {configurationSetName = a} :: SendEmail)

-- | The email address that is sending the email. This email address must be
-- either individually verified with Amazon SES, or from a domain that has
-- been verified with Amazon SES. For information about verifying
-- identities, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide>.
--
-- If you are sending on behalf of another user and have been permitted to
-- do so by a sending authorization policy, then you must also specify the
-- @SourceArn@ parameter. For more information about sending authorization,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- Amazon SES does not support the SMTPUTF8 extension, as described in
-- <https://tools.ietf.org/html/rfc6531 RFC6531>. For this reason, the
-- /local part/ of a source email address (the part of the email address
-- that precedes the \@ sign) may only contain
-- <https://en.wikipedia.org/wiki/Email_address#Local-part 7-bit ASCII characters>.
-- If the /domain part/ of an address (the part after the \@ sign) contains
-- non-ASCII characters, they must be encoded using Punycode, as described
-- in <https://tools.ietf.org/html/rfc3492.html RFC3492>. The sender name
-- (also known as the /friendly name/) may contain non-ASCII characters.
-- These characters must be encoded using MIME encoded-word syntax, as
-- described in <https://tools.ietf.org/html/rfc2047 RFC 2047>. MIME
-- encoded-word syntax uses the following form:
-- @=?charset?encoding?encoded-text?=@.
sendEmail_source :: Lens.Lens' SendEmail Prelude.Text
sendEmail_source = Lens.lens (\SendEmail' {source} -> source) (\s@SendEmail' {} a -> s {source = a} :: SendEmail)

-- | The destination for this email, composed of To:, CC:, and BCC: fields.
sendEmail_destination :: Lens.Lens' SendEmail Destination
sendEmail_destination = Lens.lens (\SendEmail' {destination} -> destination) (\s@SendEmail' {} a -> s {destination = a} :: SendEmail)

-- | The message to be sent.
sendEmail_message :: Lens.Lens' SendEmail Message
sendEmail_message = Lens.lens (\SendEmail' {message} -> message) (\s@SendEmail' {} a -> s {message = a} :: SendEmail)

instance Prelude.AWSRequest SendEmail where
  type Rs SendEmail = SendEmailResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SendEmailResult"
      ( \s h x ->
          SendEmailResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "MessageId")
      )

instance Prelude.Hashable SendEmail

instance Prelude.NFData SendEmail

instance Prelude.ToHeaders SendEmail where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SendEmail where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SendEmail where
  toQuery SendEmail' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SendEmail" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "ReturnPath" Prelude.=: returnPath,
        "ReturnPathArn" Prelude.=: returnPathArn,
        "ReplyToAddresses"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> replyToAddresses
            ),
        "Tags"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> tags),
        "SourceArn" Prelude.=: sourceArn,
        "ConfigurationSetName"
          Prelude.=: configurationSetName,
        "Source" Prelude.=: source,
        "Destination" Prelude.=: destination,
        "Message" Prelude.=: message
      ]

-- | Represents a unique message ID.
--
-- /See:/ 'newSendEmailResponse' smart constructor.
data SendEmailResponse = SendEmailResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique message identifier returned from the @SendEmail@ action.
    messageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendEmailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendEmailResponse_httpStatus' - The response's http status code.
--
-- 'messageId', 'sendEmailResponse_messageId' - The unique message identifier returned from the @SendEmail@ action.
newSendEmailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageId'
  Prelude.Text ->
  SendEmailResponse
newSendEmailResponse pHttpStatus_ pMessageId_ =
  SendEmailResponse'
    { httpStatus = pHttpStatus_,
      messageId = pMessageId_
    }

-- | The response's http status code.
sendEmailResponse_httpStatus :: Lens.Lens' SendEmailResponse Prelude.Int
sendEmailResponse_httpStatus = Lens.lens (\SendEmailResponse' {httpStatus} -> httpStatus) (\s@SendEmailResponse' {} a -> s {httpStatus = a} :: SendEmailResponse)

-- | The unique message identifier returned from the @SendEmail@ action.
sendEmailResponse_messageId :: Lens.Lens' SendEmailResponse Prelude.Text
sendEmailResponse_messageId = Lens.lens (\SendEmailResponse' {messageId} -> messageId) (\s@SendEmailResponse' {} a -> s {messageId = a} :: SendEmailResponse)

instance Prelude.NFData SendEmailResponse
