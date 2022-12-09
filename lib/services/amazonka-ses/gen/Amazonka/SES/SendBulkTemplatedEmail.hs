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
-- Module      : Amazonka.SES.SendBulkTemplatedEmail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message to multiple destinations. The message body is
-- created using an email template.
--
-- In order to send email using the @SendBulkTemplatedEmail@ operation,
-- your call to the API must meet the following requirements:
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
-- -   Each @Destination@ parameter must include at least one recipient
--     email address. The recipient address can be a To: address, a CC:
--     address, or a BCC: address. If a recipient email address is invalid
--     (that is, it is not in the format
--     /UserName\@[SubDomain.]Domain.TopLevelDomain/), the entire message
--     will be rejected, even if the message contains other recipients that
--     are valid.
--
-- -   The message may not include more than 50 recipients, across the To:,
--     CC: and BCC: fields. If you need to send an email message to a
--     larger audience, you can divide your recipient list into groups of
--     50 or fewer, and then call the @SendBulkTemplatedEmail@ operation
--     several times to send the message to each group.
--
-- -   The number of destinations you can contact in a single call to the
--     API may be limited by your account\'s maximum sending rate.
module Amazonka.SES.SendBulkTemplatedEmail
  ( -- * Creating a Request
    SendBulkTemplatedEmail (..),
    newSendBulkTemplatedEmail,

    -- * Request Lenses
    sendBulkTemplatedEmail_configurationSetName,
    sendBulkTemplatedEmail_defaultTags,
    sendBulkTemplatedEmail_defaultTemplateData,
    sendBulkTemplatedEmail_replyToAddresses,
    sendBulkTemplatedEmail_returnPath,
    sendBulkTemplatedEmail_returnPathArn,
    sendBulkTemplatedEmail_sourceArn,
    sendBulkTemplatedEmail_templateArn,
    sendBulkTemplatedEmail_source,
    sendBulkTemplatedEmail_template,
    sendBulkTemplatedEmail_destinations,

    -- * Destructuring the Response
    SendBulkTemplatedEmailResponse (..),
    newSendBulkTemplatedEmailResponse,

    -- * Response Lenses
    sendBulkTemplatedEmailResponse_httpStatus,
    sendBulkTemplatedEmailResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to send a templated email to multiple destinations
-- using Amazon SES. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSendBulkTemplatedEmail' smart constructor.
data SendBulkTemplatedEmail = SendBulkTemplatedEmail'
  { -- | The name of the configuration set to use when you send an email using
    -- @SendBulkTemplatedEmail@.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags, in the form of name\/value pairs, to apply to an email
    -- that you send to a destination using @SendBulkTemplatedEmail@.
    defaultTags :: Prelude.Maybe [MessageTag],
    -- | A list of replacement values to apply to the template when replacement
    -- data is not specified in a Destination object. These values act as a
    -- default or fallback option when no other data is available.
    --
    -- The template data is a JSON object, typically consisting of key-value
    -- pairs in which the keys correspond to replacement tags in the email
    -- template.
    defaultTemplateData :: Prelude.Maybe Prelude.Text,
    -- | The reply-to email address(es) for the message. If the recipient replies
    -- to the message, each reply-to address will receive the reply.
    replyToAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The email address that bounces and complaints will be forwarded to when
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
    -- | The template to use when sending this email.
    template :: Prelude.Text,
    -- | One or more @Destination@ objects. All of the recipients in a
    -- @Destination@ will receive the same version of the email. You can
    -- specify up to 50 @Destination@ objects within a @Destinations@ array.
    destinations :: [BulkEmailDestination]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendBulkTemplatedEmail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'sendBulkTemplatedEmail_configurationSetName' - The name of the configuration set to use when you send an email using
-- @SendBulkTemplatedEmail@.
--
-- 'defaultTags', 'sendBulkTemplatedEmail_defaultTags' - A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send to a destination using @SendBulkTemplatedEmail@.
--
-- 'defaultTemplateData', 'sendBulkTemplatedEmail_defaultTemplateData' - A list of replacement values to apply to the template when replacement
-- data is not specified in a Destination object. These values act as a
-- default or fallback option when no other data is available.
--
-- The template data is a JSON object, typically consisting of key-value
-- pairs in which the keys correspond to replacement tags in the email
-- template.
--
-- 'replyToAddresses', 'sendBulkTemplatedEmail_replyToAddresses' - The reply-to email address(es) for the message. If the recipient replies
-- to the message, each reply-to address will receive the reply.
--
-- 'returnPath', 'sendBulkTemplatedEmail_returnPath' - The email address that bounces and complaints will be forwarded to when
-- feedback forwarding is enabled. If the message cannot be delivered to
-- the recipient, then an error message will be returned from the
-- recipient\'s ISP; this message will then be forwarded to the email
-- address specified by the @ReturnPath@ parameter. The @ReturnPath@
-- parameter is never overwritten. This email address must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
--
-- 'returnPathArn', 'sendBulkTemplatedEmail_returnPathArn' - This parameter is used only for sending authorization. It is the ARN of
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
-- 'sourceArn', 'sendBulkTemplatedEmail_sourceArn' - This parameter is used only for sending authorization. It is the ARN of
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
-- 'templateArn', 'sendBulkTemplatedEmail_templateArn' - The ARN of the template to use when sending this email.
--
-- 'source', 'sendBulkTemplatedEmail_source' - The email address that is sending the email. This email address must be
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
-- 'template', 'sendBulkTemplatedEmail_template' - The template to use when sending this email.
--
-- 'destinations', 'sendBulkTemplatedEmail_destinations' - One or more @Destination@ objects. All of the recipients in a
-- @Destination@ will receive the same version of the email. You can
-- specify up to 50 @Destination@ objects within a @Destinations@ array.
newSendBulkTemplatedEmail ::
  -- | 'source'
  Prelude.Text ->
  -- | 'template'
  Prelude.Text ->
  SendBulkTemplatedEmail
newSendBulkTemplatedEmail pSource_ pTemplate_ =
  SendBulkTemplatedEmail'
    { configurationSetName =
        Prelude.Nothing,
      defaultTags = Prelude.Nothing,
      defaultTemplateData = Prelude.Nothing,
      replyToAddresses = Prelude.Nothing,
      returnPath = Prelude.Nothing,
      returnPathArn = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      templateArn = Prelude.Nothing,
      source = pSource_,
      template = pTemplate_,
      destinations = Prelude.mempty
    }

-- | The name of the configuration set to use when you send an email using
-- @SendBulkTemplatedEmail@.
sendBulkTemplatedEmail_configurationSetName :: Lens.Lens' SendBulkTemplatedEmail (Prelude.Maybe Prelude.Text)
sendBulkTemplatedEmail_configurationSetName = Lens.lens (\SendBulkTemplatedEmail' {configurationSetName} -> configurationSetName) (\s@SendBulkTemplatedEmail' {} a -> s {configurationSetName = a} :: SendBulkTemplatedEmail)

-- | A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send to a destination using @SendBulkTemplatedEmail@.
sendBulkTemplatedEmail_defaultTags :: Lens.Lens' SendBulkTemplatedEmail (Prelude.Maybe [MessageTag])
sendBulkTemplatedEmail_defaultTags = Lens.lens (\SendBulkTemplatedEmail' {defaultTags} -> defaultTags) (\s@SendBulkTemplatedEmail' {} a -> s {defaultTags = a} :: SendBulkTemplatedEmail) Prelude.. Lens.mapping Lens.coerced

-- | A list of replacement values to apply to the template when replacement
-- data is not specified in a Destination object. These values act as a
-- default or fallback option when no other data is available.
--
-- The template data is a JSON object, typically consisting of key-value
-- pairs in which the keys correspond to replacement tags in the email
-- template.
sendBulkTemplatedEmail_defaultTemplateData :: Lens.Lens' SendBulkTemplatedEmail (Prelude.Maybe Prelude.Text)
sendBulkTemplatedEmail_defaultTemplateData = Lens.lens (\SendBulkTemplatedEmail' {defaultTemplateData} -> defaultTemplateData) (\s@SendBulkTemplatedEmail' {} a -> s {defaultTemplateData = a} :: SendBulkTemplatedEmail)

-- | The reply-to email address(es) for the message. If the recipient replies
-- to the message, each reply-to address will receive the reply.
sendBulkTemplatedEmail_replyToAddresses :: Lens.Lens' SendBulkTemplatedEmail (Prelude.Maybe [Prelude.Text])
sendBulkTemplatedEmail_replyToAddresses = Lens.lens (\SendBulkTemplatedEmail' {replyToAddresses} -> replyToAddresses) (\s@SendBulkTemplatedEmail' {} a -> s {replyToAddresses = a} :: SendBulkTemplatedEmail) Prelude.. Lens.mapping Lens.coerced

-- | The email address that bounces and complaints will be forwarded to when
-- feedback forwarding is enabled. If the message cannot be delivered to
-- the recipient, then an error message will be returned from the
-- recipient\'s ISP; this message will then be forwarded to the email
-- address specified by the @ReturnPath@ parameter. The @ReturnPath@
-- parameter is never overwritten. This email address must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
sendBulkTemplatedEmail_returnPath :: Lens.Lens' SendBulkTemplatedEmail (Prelude.Maybe Prelude.Text)
sendBulkTemplatedEmail_returnPath = Lens.lens (\SendBulkTemplatedEmail' {returnPath} -> returnPath) (\s@SendBulkTemplatedEmail' {} a -> s {returnPath = a} :: SendBulkTemplatedEmail)

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
sendBulkTemplatedEmail_returnPathArn :: Lens.Lens' SendBulkTemplatedEmail (Prelude.Maybe Prelude.Text)
sendBulkTemplatedEmail_returnPathArn = Lens.lens (\SendBulkTemplatedEmail' {returnPathArn} -> returnPathArn) (\s@SendBulkTemplatedEmail' {} a -> s {returnPathArn = a} :: SendBulkTemplatedEmail)

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
sendBulkTemplatedEmail_sourceArn :: Lens.Lens' SendBulkTemplatedEmail (Prelude.Maybe Prelude.Text)
sendBulkTemplatedEmail_sourceArn = Lens.lens (\SendBulkTemplatedEmail' {sourceArn} -> sourceArn) (\s@SendBulkTemplatedEmail' {} a -> s {sourceArn = a} :: SendBulkTemplatedEmail)

-- | The ARN of the template to use when sending this email.
sendBulkTemplatedEmail_templateArn :: Lens.Lens' SendBulkTemplatedEmail (Prelude.Maybe Prelude.Text)
sendBulkTemplatedEmail_templateArn = Lens.lens (\SendBulkTemplatedEmail' {templateArn} -> templateArn) (\s@SendBulkTemplatedEmail' {} a -> s {templateArn = a} :: SendBulkTemplatedEmail)

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
sendBulkTemplatedEmail_source :: Lens.Lens' SendBulkTemplatedEmail Prelude.Text
sendBulkTemplatedEmail_source = Lens.lens (\SendBulkTemplatedEmail' {source} -> source) (\s@SendBulkTemplatedEmail' {} a -> s {source = a} :: SendBulkTemplatedEmail)

-- | The template to use when sending this email.
sendBulkTemplatedEmail_template :: Lens.Lens' SendBulkTemplatedEmail Prelude.Text
sendBulkTemplatedEmail_template = Lens.lens (\SendBulkTemplatedEmail' {template} -> template) (\s@SendBulkTemplatedEmail' {} a -> s {template = a} :: SendBulkTemplatedEmail)

-- | One or more @Destination@ objects. All of the recipients in a
-- @Destination@ will receive the same version of the email. You can
-- specify up to 50 @Destination@ objects within a @Destinations@ array.
sendBulkTemplatedEmail_destinations :: Lens.Lens' SendBulkTemplatedEmail [BulkEmailDestination]
sendBulkTemplatedEmail_destinations = Lens.lens (\SendBulkTemplatedEmail' {destinations} -> destinations) (\s@SendBulkTemplatedEmail' {} a -> s {destinations = a} :: SendBulkTemplatedEmail) Prelude.. Lens.coerced

instance Core.AWSRequest SendBulkTemplatedEmail where
  type
    AWSResponse SendBulkTemplatedEmail =
      SendBulkTemplatedEmailResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SendBulkTemplatedEmailResult"
      ( \s h x ->
          SendBulkTemplatedEmailResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "Status" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable SendBulkTemplatedEmail where
  hashWithSalt _salt SendBulkTemplatedEmail' {..} =
    _salt `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` defaultTags
      `Prelude.hashWithSalt` defaultTemplateData
      `Prelude.hashWithSalt` replyToAddresses
      `Prelude.hashWithSalt` returnPath
      `Prelude.hashWithSalt` returnPathArn
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` templateArn
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` template
      `Prelude.hashWithSalt` destinations

instance Prelude.NFData SendBulkTemplatedEmail where
  rnf SendBulkTemplatedEmail' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf defaultTags
      `Prelude.seq` Prelude.rnf defaultTemplateData
      `Prelude.seq` Prelude.rnf replyToAddresses
      `Prelude.seq` Prelude.rnf returnPath
      `Prelude.seq` Prelude.rnf returnPathArn
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf template
      `Prelude.seq` Prelude.rnf destinations

instance Data.ToHeaders SendBulkTemplatedEmail where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SendBulkTemplatedEmail where
  toPath = Prelude.const "/"

instance Data.ToQuery SendBulkTemplatedEmail where
  toQuery SendBulkTemplatedEmail' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SendBulkTemplatedEmail" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName" Data.=: configurationSetName,
        "DefaultTags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> defaultTags),
        "DefaultTemplateData" Data.=: defaultTemplateData,
        "ReplyToAddresses"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> replyToAddresses
            ),
        "ReturnPath" Data.=: returnPath,
        "ReturnPathArn" Data.=: returnPathArn,
        "SourceArn" Data.=: sourceArn,
        "TemplateArn" Data.=: templateArn,
        "Source" Data.=: source,
        "Template" Data.=: template,
        "Destinations"
          Data.=: Data.toQueryList "member" destinations
      ]

-- | /See:/ 'newSendBulkTemplatedEmailResponse' smart constructor.
data SendBulkTemplatedEmailResponse = SendBulkTemplatedEmailResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique message identifier returned from the @SendBulkTemplatedEmail@
    -- action.
    status :: [BulkEmailDestinationStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendBulkTemplatedEmailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendBulkTemplatedEmailResponse_httpStatus' - The response's http status code.
--
-- 'status', 'sendBulkTemplatedEmailResponse_status' - The unique message identifier returned from the @SendBulkTemplatedEmail@
-- action.
newSendBulkTemplatedEmailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendBulkTemplatedEmailResponse
newSendBulkTemplatedEmailResponse pHttpStatus_ =
  SendBulkTemplatedEmailResponse'
    { httpStatus =
        pHttpStatus_,
      status = Prelude.mempty
    }

-- | The response's http status code.
sendBulkTemplatedEmailResponse_httpStatus :: Lens.Lens' SendBulkTemplatedEmailResponse Prelude.Int
sendBulkTemplatedEmailResponse_httpStatus = Lens.lens (\SendBulkTemplatedEmailResponse' {httpStatus} -> httpStatus) (\s@SendBulkTemplatedEmailResponse' {} a -> s {httpStatus = a} :: SendBulkTemplatedEmailResponse)

-- | The unique message identifier returned from the @SendBulkTemplatedEmail@
-- action.
sendBulkTemplatedEmailResponse_status :: Lens.Lens' SendBulkTemplatedEmailResponse [BulkEmailDestinationStatus]
sendBulkTemplatedEmailResponse_status = Lens.lens (\SendBulkTemplatedEmailResponse' {status} -> status) (\s@SendBulkTemplatedEmailResponse' {} a -> s {status = a} :: SendBulkTemplatedEmailResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    SendBulkTemplatedEmailResponse
  where
  rnf SendBulkTemplatedEmailResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
