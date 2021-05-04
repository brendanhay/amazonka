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
-- Module      : Network.AWS.SES.SendTemplatedEmail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message using an email template and immediately queues
-- it for sending.
--
-- In order to send email using the @SendTemplatedEmail@ operation, your
-- call to the API must meet the following requirements:
--
-- -   The call must refer to an existing email template. You can create
--     email templates using the CreateTemplate operation.
--
-- -   The message must be sent from a verified email address or domain.
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
-- -   Calls to the @SendTemplatedEmail@ operation may only include one
--     @Destination@ parameter. A destination is a set of recipients who
--     will receive the same version of the email. The @Destination@
--     parameter can include up to 50 recipients, across the To:, CC: and
--     BCC: fields.
--
-- -   The @Destination@ parameter must include at least one recipient
--     email address. The recipient address can be a To: address, a CC:
--     address, or a BCC: address. If a recipient email address is invalid
--     (that is, it is not in the format
--     /UserName\@[SubDomain.]Domain.TopLevelDomain/), the entire message
--     will be rejected, even if the message contains other recipients that
--     are valid.
--
-- If your call to the @SendTemplatedEmail@ operation includes all of the
-- required parameters, Amazon SES accepts it and returns a Message ID.
-- However, if Amazon SES can\'t render the email because the template
-- contains errors, it doesn\'t send the email. Additionally, because it
-- already accepted the message, Amazon SES doesn\'t return a message
-- stating that it was unable to send the email.
--
-- For these reasons, we highly recommend that you set up Amazon SES to
-- send you notifications when Rendering Failure events occur. For more
-- information, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Sending Personalized Email Using the Amazon SES API>
-- in the /Amazon Simple Email Service Developer Guide/.
module Network.AWS.SES.SendTemplatedEmail
  ( -- * Creating a Request
    SendTemplatedEmail (..),
    newSendTemplatedEmail,

    -- * Request Lenses
    sendTemplatedEmail_returnPath,
    sendTemplatedEmail_returnPathArn,
    sendTemplatedEmail_replyToAddresses,
    sendTemplatedEmail_tags,
    sendTemplatedEmail_sourceArn,
    sendTemplatedEmail_templateArn,
    sendTemplatedEmail_configurationSetName,
    sendTemplatedEmail_source,
    sendTemplatedEmail_destination,
    sendTemplatedEmail_template,
    sendTemplatedEmail_templateData,

    -- * Destructuring the Response
    SendTemplatedEmailResponse (..),
    newSendTemplatedEmailResponse,

    -- * Response Lenses
    sendTemplatedEmailResponse_httpStatus,
    sendTemplatedEmailResponse_messageId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to send a templated email using Amazon SES. For
-- more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSendTemplatedEmail' smart constructor.
data SendTemplatedEmail = SendTemplatedEmail'
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
    -- that you send using @SendTemplatedEmail@. Tags correspond to
    -- characteristics of the email that you define, so that you can publish
    -- email sending events.
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
    -- | The ARN of the template to use when sending this email.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set to use when you send an email using
    -- @SendTemplatedEmail@.
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
    -- described in<https://tools.ietf.org/html/rfc2047 RFC 2047>. MIME
    -- encoded-word syntax uses the following form:
    -- @=?charset?encoding?encoded-text?=@.
    source :: Prelude.Text,
    -- | The destination for this email, composed of To:, CC:, and BCC: fields. A
    -- Destination can include up to 50 recipients across these three fields.
    destination :: Destination,
    -- | The template to use when sending this email.
    template :: Prelude.Text,
    -- | A list of replacement values to apply to the template. This parameter is
    -- a JSON object, typically consisting of key-value pairs in which the keys
    -- correspond to replacement tags in the email template.
    templateData :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendTemplatedEmail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnPath', 'sendTemplatedEmail_returnPath' - The email address that bounces and complaints will be forwarded to when
-- feedback forwarding is enabled. If the message cannot be delivered to
-- the recipient, then an error message will be returned from the
-- recipient\'s ISP; this message will then be forwarded to the email
-- address specified by the @ReturnPath@ parameter. The @ReturnPath@
-- parameter is never overwritten. This email address must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
--
-- 'returnPathArn', 'sendTemplatedEmail_returnPathArn' - This parameter is used only for sending authorization. It is the ARN of
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
-- 'replyToAddresses', 'sendTemplatedEmail_replyToAddresses' - The reply-to email address(es) for the message. If the recipient replies
-- to the message, each reply-to address will receive the reply.
--
-- 'tags', 'sendTemplatedEmail_tags' - A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using @SendTemplatedEmail@. Tags correspond to
-- characteristics of the email that you define, so that you can publish
-- email sending events.
--
-- 'sourceArn', 'sendTemplatedEmail_sourceArn' - This parameter is used only for sending authorization. It is the ARN of
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
-- 'templateArn', 'sendTemplatedEmail_templateArn' - The ARN of the template to use when sending this email.
--
-- 'configurationSetName', 'sendTemplatedEmail_configurationSetName' - The name of the configuration set to use when you send an email using
-- @SendTemplatedEmail@.
--
-- 'source', 'sendTemplatedEmail_source' - The email address that is sending the email. This email address must be
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
-- described in<https://tools.ietf.org/html/rfc2047 RFC 2047>. MIME
-- encoded-word syntax uses the following form:
-- @=?charset?encoding?encoded-text?=@.
--
-- 'destination', 'sendTemplatedEmail_destination' - The destination for this email, composed of To:, CC:, and BCC: fields. A
-- Destination can include up to 50 recipients across these three fields.
--
-- 'template', 'sendTemplatedEmail_template' - The template to use when sending this email.
--
-- 'templateData', 'sendTemplatedEmail_templateData' - A list of replacement values to apply to the template. This parameter is
-- a JSON object, typically consisting of key-value pairs in which the keys
-- correspond to replacement tags in the email template.
newSendTemplatedEmail ::
  -- | 'source'
  Prelude.Text ->
  -- | 'destination'
  Destination ->
  -- | 'template'
  Prelude.Text ->
  -- | 'templateData'
  Prelude.Text ->
  SendTemplatedEmail
newSendTemplatedEmail
  pSource_
  pDestination_
  pTemplate_
  pTemplateData_ =
    SendTemplatedEmail'
      { returnPath = Prelude.Nothing,
        returnPathArn = Prelude.Nothing,
        replyToAddresses = Prelude.Nothing,
        tags = Prelude.Nothing,
        sourceArn = Prelude.Nothing,
        templateArn = Prelude.Nothing,
        configurationSetName = Prelude.Nothing,
        source = pSource_,
        destination = pDestination_,
        template = pTemplate_,
        templateData = pTemplateData_
      }

-- | The email address that bounces and complaints will be forwarded to when
-- feedback forwarding is enabled. If the message cannot be delivered to
-- the recipient, then an error message will be returned from the
-- recipient\'s ISP; this message will then be forwarded to the email
-- address specified by the @ReturnPath@ parameter. The @ReturnPath@
-- parameter is never overwritten. This email address must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
sendTemplatedEmail_returnPath :: Lens.Lens' SendTemplatedEmail (Prelude.Maybe Prelude.Text)
sendTemplatedEmail_returnPath = Lens.lens (\SendTemplatedEmail' {returnPath} -> returnPath) (\s@SendTemplatedEmail' {} a -> s {returnPath = a} :: SendTemplatedEmail)

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
sendTemplatedEmail_returnPathArn :: Lens.Lens' SendTemplatedEmail (Prelude.Maybe Prelude.Text)
sendTemplatedEmail_returnPathArn = Lens.lens (\SendTemplatedEmail' {returnPathArn} -> returnPathArn) (\s@SendTemplatedEmail' {} a -> s {returnPathArn = a} :: SendTemplatedEmail)

-- | The reply-to email address(es) for the message. If the recipient replies
-- to the message, each reply-to address will receive the reply.
sendTemplatedEmail_replyToAddresses :: Lens.Lens' SendTemplatedEmail (Prelude.Maybe [Prelude.Text])
sendTemplatedEmail_replyToAddresses = Lens.lens (\SendTemplatedEmail' {replyToAddresses} -> replyToAddresses) (\s@SendTemplatedEmail' {} a -> s {replyToAddresses = a} :: SendTemplatedEmail) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using @SendTemplatedEmail@. Tags correspond to
-- characteristics of the email that you define, so that you can publish
-- email sending events.
sendTemplatedEmail_tags :: Lens.Lens' SendTemplatedEmail (Prelude.Maybe [MessageTag])
sendTemplatedEmail_tags = Lens.lens (\SendTemplatedEmail' {tags} -> tags) (\s@SendTemplatedEmail' {} a -> s {tags = a} :: SendTemplatedEmail) Prelude.. Lens.mapping Prelude._Coerce

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
sendTemplatedEmail_sourceArn :: Lens.Lens' SendTemplatedEmail (Prelude.Maybe Prelude.Text)
sendTemplatedEmail_sourceArn = Lens.lens (\SendTemplatedEmail' {sourceArn} -> sourceArn) (\s@SendTemplatedEmail' {} a -> s {sourceArn = a} :: SendTemplatedEmail)

-- | The ARN of the template to use when sending this email.
sendTemplatedEmail_templateArn :: Lens.Lens' SendTemplatedEmail (Prelude.Maybe Prelude.Text)
sendTemplatedEmail_templateArn = Lens.lens (\SendTemplatedEmail' {templateArn} -> templateArn) (\s@SendTemplatedEmail' {} a -> s {templateArn = a} :: SendTemplatedEmail)

-- | The name of the configuration set to use when you send an email using
-- @SendTemplatedEmail@.
sendTemplatedEmail_configurationSetName :: Lens.Lens' SendTemplatedEmail (Prelude.Maybe Prelude.Text)
sendTemplatedEmail_configurationSetName = Lens.lens (\SendTemplatedEmail' {configurationSetName} -> configurationSetName) (\s@SendTemplatedEmail' {} a -> s {configurationSetName = a} :: SendTemplatedEmail)

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
-- described in<https://tools.ietf.org/html/rfc2047 RFC 2047>. MIME
-- encoded-word syntax uses the following form:
-- @=?charset?encoding?encoded-text?=@.
sendTemplatedEmail_source :: Lens.Lens' SendTemplatedEmail Prelude.Text
sendTemplatedEmail_source = Lens.lens (\SendTemplatedEmail' {source} -> source) (\s@SendTemplatedEmail' {} a -> s {source = a} :: SendTemplatedEmail)

-- | The destination for this email, composed of To:, CC:, and BCC: fields. A
-- Destination can include up to 50 recipients across these three fields.
sendTemplatedEmail_destination :: Lens.Lens' SendTemplatedEmail Destination
sendTemplatedEmail_destination = Lens.lens (\SendTemplatedEmail' {destination} -> destination) (\s@SendTemplatedEmail' {} a -> s {destination = a} :: SendTemplatedEmail)

-- | The template to use when sending this email.
sendTemplatedEmail_template :: Lens.Lens' SendTemplatedEmail Prelude.Text
sendTemplatedEmail_template = Lens.lens (\SendTemplatedEmail' {template} -> template) (\s@SendTemplatedEmail' {} a -> s {template = a} :: SendTemplatedEmail)

-- | A list of replacement values to apply to the template. This parameter is
-- a JSON object, typically consisting of key-value pairs in which the keys
-- correspond to replacement tags in the email template.
sendTemplatedEmail_templateData :: Lens.Lens' SendTemplatedEmail Prelude.Text
sendTemplatedEmail_templateData = Lens.lens (\SendTemplatedEmail' {templateData} -> templateData) (\s@SendTemplatedEmail' {} a -> s {templateData = a} :: SendTemplatedEmail)

instance Prelude.AWSRequest SendTemplatedEmail where
  type
    Rs SendTemplatedEmail =
      SendTemplatedEmailResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SendTemplatedEmailResult"
      ( \s h x ->
          SendTemplatedEmailResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "MessageId")
      )

instance Prelude.Hashable SendTemplatedEmail

instance Prelude.NFData SendTemplatedEmail

instance Prelude.ToHeaders SendTemplatedEmail where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SendTemplatedEmail where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SendTemplatedEmail where
  toQuery SendTemplatedEmail' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SendTemplatedEmail" :: Prelude.ByteString),
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
        "TemplateArn" Prelude.=: templateArn,
        "ConfigurationSetName"
          Prelude.=: configurationSetName,
        "Source" Prelude.=: source,
        "Destination" Prelude.=: destination,
        "Template" Prelude.=: template,
        "TemplateData" Prelude.=: templateData
      ]

-- | /See:/ 'newSendTemplatedEmailResponse' smart constructor.
data SendTemplatedEmailResponse = SendTemplatedEmailResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique message identifier returned from the @SendTemplatedEmail@
    -- action.
    messageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendTemplatedEmailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendTemplatedEmailResponse_httpStatus' - The response's http status code.
--
-- 'messageId', 'sendTemplatedEmailResponse_messageId' - The unique message identifier returned from the @SendTemplatedEmail@
-- action.
newSendTemplatedEmailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageId'
  Prelude.Text ->
  SendTemplatedEmailResponse
newSendTemplatedEmailResponse
  pHttpStatus_
  pMessageId_ =
    SendTemplatedEmailResponse'
      { httpStatus =
          pHttpStatus_,
        messageId = pMessageId_
      }

-- | The response's http status code.
sendTemplatedEmailResponse_httpStatus :: Lens.Lens' SendTemplatedEmailResponse Prelude.Int
sendTemplatedEmailResponse_httpStatus = Lens.lens (\SendTemplatedEmailResponse' {httpStatus} -> httpStatus) (\s@SendTemplatedEmailResponse' {} a -> s {httpStatus = a} :: SendTemplatedEmailResponse)

-- | The unique message identifier returned from the @SendTemplatedEmail@
-- action.
sendTemplatedEmailResponse_messageId :: Lens.Lens' SendTemplatedEmailResponse Prelude.Text
sendTemplatedEmailResponse_messageId = Lens.lens (\SendTemplatedEmailResponse' {messageId} -> messageId) (\s@SendTemplatedEmailResponse' {} a -> s {messageId = a} :: SendTemplatedEmailResponse)

instance Prelude.NFData SendTemplatedEmailResponse
