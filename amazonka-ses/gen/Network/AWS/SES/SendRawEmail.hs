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
-- Module      : Network.AWS.SES.SendRawEmail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message and immediately queues it for sending.
--
-- This operation is more flexible than the @SendEmail@ API operation. When
-- you use the @SendRawEmail@ operation, you can specify the headers of the
-- message as well as its content. This flexibility is useful, for example,
-- when you want to send a multipart MIME email (such a message that
-- contains both a text and an HTML version). You can also use this
-- operation to send messages that include attachments.
--
-- The @SendRawEmail@ operation has the following requirements:
--
-- -   You can only send email from
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html verified email addresses or domains>.
--     If you try to send email from an address that isn\'t verified, the
--     operation results in an \"Email address not verified\" error.
--
-- -   If your account is still in the
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/request-production-access.html Amazon SES sandbox>,
--     you can only send email to other verified addresses in your account,
--     or to addresses that are associated with the
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mailbox-simulator.html Amazon SES mailbox simulator>.
--
-- -   The maximum message size, including attachments, is 10 MB.
--
-- -   Each message has to include at least one recipient address. A
--     recipient address includes any address on the To:, CC:, or BCC:
--     lines.
--
-- -   If you send a single message to more than one recipient address, and
--     one of the recipient addresses isn\'t in a valid format (that is,
--     it\'s not in the format
--     /UserName\@[SubDomain.]Domain.TopLevelDomain/), Amazon SES rejects
--     the entire message, even if the other addresses are valid.
--
-- -   Each message can include up to 50 recipient addresses across the
--     To:, CC:, or BCC: lines. If you need to send a single message to
--     more than 50 recipients, you have to split the list of recipient
--     addresses into groups of less than 50 recipients, and send separate
--     messages to each group.
--
-- -   Amazon SES allows you to specify 8-bit Content-Transfer-Encoding for
--     MIME message parts. However, if Amazon SES has to modify the
--     contents of your message (for example, if you use open and click
--     tracking), 8-bit content isn\'t preserved. For this reason, we
--     highly recommend that you encode all content that isn\'t 7-bit
--     ASCII. For more information, see
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html#send-email-mime-encoding MIME Encoding>
--     in the /Amazon SES Developer Guide/.
--
-- Additionally, keep the following considerations in mind when using the
-- @SendRawEmail@ operation:
--
-- -   Although you can customize the message headers when using the
--     @SendRawEmail@ operation, Amazon SES will automatically apply its
--     own @Message-ID@ and @Date@ headers; if you passed these headers
--     when creating the message, they will be overwritten by the values
--     that Amazon SES provides.
--
-- -   If you are using sending authorization to send on behalf of another
--     user, @SendRawEmail@ enables you to specify the cross-account
--     identity for the email\'s Source, From, and Return-Path parameters
--     in one of two ways: you can pass optional parameters @SourceArn@,
--     @FromArn@, and\/or @ReturnPathArn@ to the API, or you can include
--     the following X-headers in the header of your raw email:
--
--     -   @X-SES-SOURCE-ARN@
--
--     -   @X-SES-FROM-ARN@
--
--     -   @X-SES-RETURN-PATH-ARN@
--
--     Don\'t include these X-headers in the DKIM signature. Amazon SES
--     removes these before it sends the email.
--
--     If you only specify the @SourceIdentityArn@ parameter, Amazon SES
--     sets the From and Return-Path addresses to the same identity that
--     you specified.
--
--     For more information about sending authorization, see the
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Using Sending Authorization with Amazon SES>
--     in the /Amazon SES Developer Guide./
--
-- -   For every message that you send, the total number of recipients
--     (including each recipient in the To:, CC: and BCC: fields) is
--     counted against the maximum number of emails you can send in a
--     24-hour period (your /sending quota/). For more information about
--     sending quotas in Amazon SES, see
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html Managing Your Amazon SES Sending Limits>
--     in the /Amazon SES Developer Guide./
module Network.AWS.SES.SendRawEmail
  ( -- * Creating a Request
    SendRawEmail (..),
    newSendRawEmail,

    -- * Request Lenses
    sendRawEmail_fromArn,
    sendRawEmail_source,
    sendRawEmail_returnPathArn,
    sendRawEmail_destinations,
    sendRawEmail_tags,
    sendRawEmail_sourceArn,
    sendRawEmail_configurationSetName,
    sendRawEmail_rawMessage,

    -- * Destructuring the Response
    SendRawEmailResponse (..),
    newSendRawEmailResponse,

    -- * Response Lenses
    sendRawEmailResponse_httpStatus,
    sendRawEmailResponse_messageId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to send a single raw email using Amazon SES. For
-- more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSendRawEmail' smart constructor.
data SendRawEmail = SendRawEmail'
  { -- | This parameter is used only for sending authorization. It is the ARN of
    -- the identity that is associated with the sending authorization policy
    -- that permits you to specify a particular \"From\" address in the header
    -- of the raw email.
    --
    -- Instead of using this parameter, you can use the X-header
    -- @X-SES-FROM-ARN@ in the raw message of the email. If you use both the
    -- @FromArn@ parameter and the corresponding X-header, Amazon SES uses the
    -- value of the @FromArn@ parameter.
    --
    -- For information about when to use this parameter, see the description of
    -- @SendRawEmail@ in this guide, or see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
    fromArn :: Prelude.Maybe Prelude.Text,
    -- | The identity\'s email address. If you do not provide a value for this
    -- parameter, you must specify a \"From\" address in the raw text of the
    -- message. (You can also specify both.)
    --
    -- Amazon SES does not support the SMTPUTF8 extension, as described
    -- in<https://tools.ietf.org/html/rfc6531 RFC6531>. For this reason, the
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
    -- If you specify the @Source@ parameter and have feedback forwarding
    -- enabled, then bounces and complaints will be sent to this email address.
    -- This takes precedence over any Return-Path header that you might include
    -- in the raw text of the message.
    source :: Prelude.Maybe Prelude.Text,
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
    -- Instead of using this parameter, you can use the X-header
    -- @X-SES-RETURN-PATH-ARN@ in the raw message of the email. If you use both
    -- the @ReturnPathArn@ parameter and the corresponding X-header, Amazon SES
    -- uses the value of the @ReturnPathArn@ parameter.
    --
    -- For information about when to use this parameter, see the description of
    -- @SendRawEmail@ in this guide, or see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
    returnPathArn :: Prelude.Maybe Prelude.Text,
    -- | A list of destinations for the message, consisting of To:, CC:, and BCC:
    -- addresses.
    destinations :: Prelude.Maybe [Prelude.Text],
    -- | A list of tags, in the form of name\/value pairs, to apply to an email
    -- that you send using @SendRawEmail@. Tags correspond to characteristics
    -- of the email that you define, so that you can publish email sending
    -- events.
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
    -- Instead of using this parameter, you can use the X-header
    -- @X-SES-SOURCE-ARN@ in the raw message of the email. If you use both the
    -- @SourceArn@ parameter and the corresponding X-header, Amazon SES uses
    -- the value of the @SourceArn@ parameter.
    --
    -- For information about when to use this parameter, see the description of
    -- @SendRawEmail@ in this guide, or see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set to use when you send an email using
    -- @SendRawEmail@.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The raw email message itself. The message has to meet the following
    -- criteria:
    --
    -- -   The message has to contain a header and a body, separated by a blank
    --     line.
    --
    -- -   All of the required header fields must be present in the message.
    --
    -- -   Each part of a multipart MIME message must be formatted properly.
    --
    -- -   Attachments must be of a content type that Amazon SES supports. For
    --     a list on unsupported content types, see
    --     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mime-types.html Unsupported Attachment Types>
    --     in the /Amazon SES Developer Guide/.
    --
    -- -   The entire message must be base64-encoded.
    --
    -- -   If any of the MIME parts in your message contain content that is
    --     outside of the 7-bit ASCII character range, we highly recommend that
    --     you encode that content. For more information, see
    --     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Sending Raw Email>
    --     in the /Amazon SES Developer Guide/.
    --
    -- -   Per
    --     <https://tools.ietf.org/html/rfc5321#section-4.5.3.1.6 RFC 5321>,
    --     the maximum length of each line of text, including the \<CRLF>, must
    --     not exceed 1,000 characters.
    rawMessage :: RawMessage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendRawEmail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromArn', 'sendRawEmail_fromArn' - This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to specify a particular \"From\" address in the header
-- of the raw email.
--
-- Instead of using this parameter, you can use the X-header
-- @X-SES-FROM-ARN@ in the raw message of the email. If you use both the
-- @FromArn@ parameter and the corresponding X-header, Amazon SES uses the
-- value of the @FromArn@ parameter.
--
-- For information about when to use this parameter, see the description of
-- @SendRawEmail@ in this guide, or see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
--
-- 'source', 'sendRawEmail_source' - The identity\'s email address. If you do not provide a value for this
-- parameter, you must specify a \"From\" address in the raw text of the
-- message. (You can also specify both.)
--
-- Amazon SES does not support the SMTPUTF8 extension, as described
-- in<https://tools.ietf.org/html/rfc6531 RFC6531>. For this reason, the
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
-- If you specify the @Source@ parameter and have feedback forwarding
-- enabled, then bounces and complaints will be sent to this email address.
-- This takes precedence over any Return-Path header that you might include
-- in the raw text of the message.
--
-- 'returnPathArn', 'sendRawEmail_returnPathArn' - This parameter is used only for sending authorization. It is the ARN of
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
-- Instead of using this parameter, you can use the X-header
-- @X-SES-RETURN-PATH-ARN@ in the raw message of the email. If you use both
-- the @ReturnPathArn@ parameter and the corresponding X-header, Amazon SES
-- uses the value of the @ReturnPathArn@ parameter.
--
-- For information about when to use this parameter, see the description of
-- @SendRawEmail@ in this guide, or see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
--
-- 'destinations', 'sendRawEmail_destinations' - A list of destinations for the message, consisting of To:, CC:, and BCC:
-- addresses.
--
-- 'tags', 'sendRawEmail_tags' - A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using @SendRawEmail@. Tags correspond to characteristics
-- of the email that you define, so that you can publish email sending
-- events.
--
-- 'sourceArn', 'sendRawEmail_sourceArn' - This parameter is used only for sending authorization. It is the ARN of
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
-- Instead of using this parameter, you can use the X-header
-- @X-SES-SOURCE-ARN@ in the raw message of the email. If you use both the
-- @SourceArn@ parameter and the corresponding X-header, Amazon SES uses
-- the value of the @SourceArn@ parameter.
--
-- For information about when to use this parameter, see the description of
-- @SendRawEmail@ in this guide, or see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
--
-- 'configurationSetName', 'sendRawEmail_configurationSetName' - The name of the configuration set to use when you send an email using
-- @SendRawEmail@.
--
-- 'rawMessage', 'sendRawEmail_rawMessage' - The raw email message itself. The message has to meet the following
-- criteria:
--
-- -   The message has to contain a header and a body, separated by a blank
--     line.
--
-- -   All of the required header fields must be present in the message.
--
-- -   Each part of a multipart MIME message must be formatted properly.
--
-- -   Attachments must be of a content type that Amazon SES supports. For
--     a list on unsupported content types, see
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mime-types.html Unsupported Attachment Types>
--     in the /Amazon SES Developer Guide/.
--
-- -   The entire message must be base64-encoded.
--
-- -   If any of the MIME parts in your message contain content that is
--     outside of the 7-bit ASCII character range, we highly recommend that
--     you encode that content. For more information, see
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Sending Raw Email>
--     in the /Amazon SES Developer Guide/.
--
-- -   Per
--     <https://tools.ietf.org/html/rfc5321#section-4.5.3.1.6 RFC 5321>,
--     the maximum length of each line of text, including the \<CRLF>, must
--     not exceed 1,000 characters.
newSendRawEmail ::
  -- | 'rawMessage'
  RawMessage ->
  SendRawEmail
newSendRawEmail pRawMessage_ =
  SendRawEmail'
    { fromArn = Prelude.Nothing,
      source = Prelude.Nothing,
      returnPathArn = Prelude.Nothing,
      destinations = Prelude.Nothing,
      tags = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      rawMessage = pRawMessage_
    }

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to specify a particular \"From\" address in the header
-- of the raw email.
--
-- Instead of using this parameter, you can use the X-header
-- @X-SES-FROM-ARN@ in the raw message of the email. If you use both the
-- @FromArn@ parameter and the corresponding X-header, Amazon SES uses the
-- value of the @FromArn@ parameter.
--
-- For information about when to use this parameter, see the description of
-- @SendRawEmail@ in this guide, or see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
sendRawEmail_fromArn :: Lens.Lens' SendRawEmail (Prelude.Maybe Prelude.Text)
sendRawEmail_fromArn = Lens.lens (\SendRawEmail' {fromArn} -> fromArn) (\s@SendRawEmail' {} a -> s {fromArn = a} :: SendRawEmail)

-- | The identity\'s email address. If you do not provide a value for this
-- parameter, you must specify a \"From\" address in the raw text of the
-- message. (You can also specify both.)
--
-- Amazon SES does not support the SMTPUTF8 extension, as described
-- in<https://tools.ietf.org/html/rfc6531 RFC6531>. For this reason, the
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
-- If you specify the @Source@ parameter and have feedback forwarding
-- enabled, then bounces and complaints will be sent to this email address.
-- This takes precedence over any Return-Path header that you might include
-- in the raw text of the message.
sendRawEmail_source :: Lens.Lens' SendRawEmail (Prelude.Maybe Prelude.Text)
sendRawEmail_source = Lens.lens (\SendRawEmail' {source} -> source) (\s@SendRawEmail' {} a -> s {source = a} :: SendRawEmail)

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
-- Instead of using this parameter, you can use the X-header
-- @X-SES-RETURN-PATH-ARN@ in the raw message of the email. If you use both
-- the @ReturnPathArn@ parameter and the corresponding X-header, Amazon SES
-- uses the value of the @ReturnPathArn@ parameter.
--
-- For information about when to use this parameter, see the description of
-- @SendRawEmail@ in this guide, or see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
sendRawEmail_returnPathArn :: Lens.Lens' SendRawEmail (Prelude.Maybe Prelude.Text)
sendRawEmail_returnPathArn = Lens.lens (\SendRawEmail' {returnPathArn} -> returnPathArn) (\s@SendRawEmail' {} a -> s {returnPathArn = a} :: SendRawEmail)

-- | A list of destinations for the message, consisting of To:, CC:, and BCC:
-- addresses.
sendRawEmail_destinations :: Lens.Lens' SendRawEmail (Prelude.Maybe [Prelude.Text])
sendRawEmail_destinations = Lens.lens (\SendRawEmail' {destinations} -> destinations) (\s@SendRawEmail' {} a -> s {destinations = a} :: SendRawEmail) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using @SendRawEmail@. Tags correspond to characteristics
-- of the email that you define, so that you can publish email sending
-- events.
sendRawEmail_tags :: Lens.Lens' SendRawEmail (Prelude.Maybe [MessageTag])
sendRawEmail_tags = Lens.lens (\SendRawEmail' {tags} -> tags) (\s@SendRawEmail' {} a -> s {tags = a} :: SendRawEmail) Prelude.. Lens.mapping Prelude._Coerce

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
-- Instead of using this parameter, you can use the X-header
-- @X-SES-SOURCE-ARN@ in the raw message of the email. If you use both the
-- @SourceArn@ parameter and the corresponding X-header, Amazon SES uses
-- the value of the @SourceArn@ parameter.
--
-- For information about when to use this parameter, see the description of
-- @SendRawEmail@ in this guide, or see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-delegate-sender-tasks-email.html Amazon SES Developer Guide>.
sendRawEmail_sourceArn :: Lens.Lens' SendRawEmail (Prelude.Maybe Prelude.Text)
sendRawEmail_sourceArn = Lens.lens (\SendRawEmail' {sourceArn} -> sourceArn) (\s@SendRawEmail' {} a -> s {sourceArn = a} :: SendRawEmail)

-- | The name of the configuration set to use when you send an email using
-- @SendRawEmail@.
sendRawEmail_configurationSetName :: Lens.Lens' SendRawEmail (Prelude.Maybe Prelude.Text)
sendRawEmail_configurationSetName = Lens.lens (\SendRawEmail' {configurationSetName} -> configurationSetName) (\s@SendRawEmail' {} a -> s {configurationSetName = a} :: SendRawEmail)

-- | The raw email message itself. The message has to meet the following
-- criteria:
--
-- -   The message has to contain a header and a body, separated by a blank
--     line.
--
-- -   All of the required header fields must be present in the message.
--
-- -   Each part of a multipart MIME message must be formatted properly.
--
-- -   Attachments must be of a content type that Amazon SES supports. For
--     a list on unsupported content types, see
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mime-types.html Unsupported Attachment Types>
--     in the /Amazon SES Developer Guide/.
--
-- -   The entire message must be base64-encoded.
--
-- -   If any of the MIME parts in your message contain content that is
--     outside of the 7-bit ASCII character range, we highly recommend that
--     you encode that content. For more information, see
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Sending Raw Email>
--     in the /Amazon SES Developer Guide/.
--
-- -   Per
--     <https://tools.ietf.org/html/rfc5321#section-4.5.3.1.6 RFC 5321>,
--     the maximum length of each line of text, including the \<CRLF>, must
--     not exceed 1,000 characters.
sendRawEmail_rawMessage :: Lens.Lens' SendRawEmail RawMessage
sendRawEmail_rawMessage = Lens.lens (\SendRawEmail' {rawMessage} -> rawMessage) (\s@SendRawEmail' {} a -> s {rawMessage = a} :: SendRawEmail)

instance Prelude.AWSRequest SendRawEmail where
  type Rs SendRawEmail = SendRawEmailResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SendRawEmailResult"
      ( \s h x ->
          SendRawEmailResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "MessageId")
      )

instance Prelude.Hashable SendRawEmail

instance Prelude.NFData SendRawEmail

instance Prelude.ToHeaders SendRawEmail where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SendRawEmail where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SendRawEmail where
  toQuery SendRawEmail' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SendRawEmail" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "FromArn" Prelude.=: fromArn,
        "Source" Prelude.=: source,
        "ReturnPathArn" Prelude.=: returnPathArn,
        "Destinations"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> destinations
            ),
        "Tags"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> tags),
        "SourceArn" Prelude.=: sourceArn,
        "ConfigurationSetName"
          Prelude.=: configurationSetName,
        "RawMessage" Prelude.=: rawMessage
      ]

-- | Represents a unique message ID.
--
-- /See:/ 'newSendRawEmailResponse' smart constructor.
data SendRawEmailResponse = SendRawEmailResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique message identifier returned from the @SendRawEmail@ action.
    messageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendRawEmailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendRawEmailResponse_httpStatus' - The response's http status code.
--
-- 'messageId', 'sendRawEmailResponse_messageId' - The unique message identifier returned from the @SendRawEmail@ action.
newSendRawEmailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageId'
  Prelude.Text ->
  SendRawEmailResponse
newSendRawEmailResponse pHttpStatus_ pMessageId_ =
  SendRawEmailResponse'
    { httpStatus = pHttpStatus_,
      messageId = pMessageId_
    }

-- | The response's http status code.
sendRawEmailResponse_httpStatus :: Lens.Lens' SendRawEmailResponse Prelude.Int
sendRawEmailResponse_httpStatus = Lens.lens (\SendRawEmailResponse' {httpStatus} -> httpStatus) (\s@SendRawEmailResponse' {} a -> s {httpStatus = a} :: SendRawEmailResponse)

-- | The unique message identifier returned from the @SendRawEmail@ action.
sendRawEmailResponse_messageId :: Lens.Lens' SendRawEmailResponse Prelude.Text
sendRawEmailResponse_messageId = Lens.lens (\SendRawEmailResponse' {messageId} -> messageId) (\s@SendRawEmailResponse' {} a -> s {messageId = a} :: SendRawEmailResponse)

instance Prelude.NFData SendRawEmailResponse
