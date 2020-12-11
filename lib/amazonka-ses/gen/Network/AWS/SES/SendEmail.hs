{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SendEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message and immediately queues it for sending. In order to send email using the @SendEmail@ operation, your message must meet the following requirements:
--
--
--     * The message must be sent from a verified email address or domain. If you attempt to send email using a non-verified address or domain, the operation will result in an "Email address not verified" error.
--
--
--     * If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Verifying Email Addresses and Domains> in the /Amazon SES Developer Guide./
--
--
--     * The maximum message size is 10 MB.
--
--
--     * The message must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format /UserName@[SubDomain.]Domain.TopLevelDomain/ ), the entire message will be rejected, even if the message contains other recipients that are valid.
--
--
--     * The message may not include more than 50 recipients, across the To:, CC: and BCC: fields. If you need to send an email message to a larger audience, you can divide your recipient list into groups of 50 or fewer, and then call the @SendEmail@ operation several times to send the message to each group.
--
--
-- /Important:/ For every message that you send, the total number of recipients (including each recipient in the To:, CC: and BCC: fields) is counted against the maximum number of emails you can send in a 24-hour period (your /sending quota/ ). For more information about sending quotas in Amazon SES, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html Managing Your Amazon SES Sending Limits> in the /Amazon SES Developer Guide./
module Network.AWS.SES.SendEmail
  ( -- * Creating a request
    SendEmail (..),
    mkSendEmail,

    -- ** Request lenses
    seReturnPath,
    seConfigurationSetName,
    seSourceARN,
    seReturnPathARN,
    seTags,
    seReplyToAddresses,
    seSource,
    seDestination,
    seMessage,

    -- * Destructuring the response
    SendEmailResponse (..),
    mkSendEmailResponse,

    -- ** Response lenses
    sersResponseStatus,
    sersMessageId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to send a single formatted email using Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-formatted.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSendEmail' smart constructor.
data SendEmail = SendEmail'
  { returnPath :: Lude.Maybe Lude.Text,
    configurationSetName :: Lude.Maybe Lude.Text,
    sourceARN :: Lude.Maybe Lude.Text,
    returnPathARN :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [MessageTag],
    replyToAddresses :: Lude.Maybe [Lude.Text],
    source :: Lude.Text,
    destination :: Destination,
    message :: Message
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendEmail' with the minimum fields required to make a request.
--
-- * 'configurationSetName' - The name of the configuration set to use when you send an email using @SendEmail@ .
-- * 'destination' - The destination for this email, composed of To:, CC:, and BCC: fields.
-- * 'message' - The message to be sent.
-- * 'replyToAddresses' - The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
-- * 'returnPath' - The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
-- * 'returnPathARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- * 'source' - The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- * 'sourceARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- * 'tags' - A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
mkSendEmail ::
  -- | 'source'
  Lude.Text ->
  -- | 'destination'
  Destination ->
  -- | 'message'
  Message ->
  SendEmail
mkSendEmail pSource_ pDestination_ pMessage_ =
  SendEmail'
    { returnPath = Lude.Nothing,
      configurationSetName = Lude.Nothing,
      sourceARN = Lude.Nothing,
      returnPathARN = Lude.Nothing,
      tags = Lude.Nothing,
      replyToAddresses = Lude.Nothing,
      source = pSource_,
      destination = pDestination_,
      message = pMessage_
    }

-- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- /Note:/ Consider using 'returnPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seReturnPath :: Lens.Lens' SendEmail (Lude.Maybe Lude.Text)
seReturnPath = Lens.lens (returnPath :: SendEmail -> Lude.Maybe Lude.Text) (\s a -> s {returnPath = a} :: SendEmail)
{-# DEPRECATED seReturnPath "Use generic-lens or generic-optics with 'returnPath' instead." #-}

-- | The name of the configuration set to use when you send an email using @SendEmail@ .
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seConfigurationSetName :: Lens.Lens' SendEmail (Lude.Maybe Lude.Text)
seConfigurationSetName = Lens.lens (configurationSetName :: SendEmail -> Lude.Maybe Lude.Text) (\s a -> s {configurationSetName = a} :: SendEmail)
{-# DEPRECATED seConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seSourceARN :: Lens.Lens' SendEmail (Lude.Maybe Lude.Text)
seSourceARN = Lens.lens (sourceARN :: SendEmail -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: SendEmail)
{-# DEPRECATED seSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'returnPathARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seReturnPathARN :: Lens.Lens' SendEmail (Lude.Maybe Lude.Text)
seReturnPathARN = Lens.lens (returnPathARN :: SendEmail -> Lude.Maybe Lude.Text) (\s a -> s {returnPathARN = a} :: SendEmail)
{-# DEPRECATED seReturnPathARN "Use generic-lens or generic-optics with 'returnPathARN' instead." #-}

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTags :: Lens.Lens' SendEmail (Lude.Maybe [MessageTag])
seTags = Lens.lens (tags :: SendEmail -> Lude.Maybe [MessageTag]) (\s a -> s {tags = a} :: SendEmail)
{-# DEPRECATED seTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
--
-- /Note:/ Consider using 'replyToAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seReplyToAddresses :: Lens.Lens' SendEmail (Lude.Maybe [Lude.Text])
seReplyToAddresses = Lens.lens (replyToAddresses :: SendEmail -> Lude.Maybe [Lude.Text]) (\s a -> s {replyToAddresses = a} :: SendEmail)
{-# DEPRECATED seReplyToAddresses "Use generic-lens or generic-optics with 'replyToAddresses' instead." #-}

-- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seSource :: Lens.Lens' SendEmail Lude.Text
seSource = Lens.lens (source :: SendEmail -> Lude.Text) (\s a -> s {source = a} :: SendEmail)
{-# DEPRECATED seSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The destination for this email, composed of To:, CC:, and BCC: fields.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seDestination :: Lens.Lens' SendEmail Destination
seDestination = Lens.lens (destination :: SendEmail -> Destination) (\s a -> s {destination = a} :: SendEmail)
{-# DEPRECATED seDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The message to be sent.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMessage :: Lens.Lens' SendEmail Message
seMessage = Lens.lens (message :: SendEmail -> Message) (\s a -> s {message = a} :: SendEmail)
{-# DEPRECATED seMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.AWSRequest SendEmail where
  type Rs SendEmail = SendEmailResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SendEmailResult"
      ( \s h x ->
          SendEmailResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "MessageId")
      )

instance Lude.ToHeaders SendEmail where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SendEmail where
  toPath = Lude.const "/"

instance Lude.ToQuery SendEmail where
  toQuery SendEmail' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SendEmail" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ReturnPath" Lude.=: returnPath,
        "ConfigurationSetName" Lude.=: configurationSetName,
        "SourceArn" Lude.=: sourceARN,
        "ReturnPathArn" Lude.=: returnPathARN,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "ReplyToAddresses"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> replyToAddresses),
        "Source" Lude.=: source,
        "Destination" Lude.=: destination,
        "Message" Lude.=: message
      ]

-- | Represents a unique message ID.
--
-- /See:/ 'mkSendEmailResponse' smart constructor.
data SendEmailResponse = SendEmailResponse'
  { responseStatus ::
      Lude.Int,
    messageId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendEmailResponse' with the minimum fields required to make a request.
--
-- * 'messageId' - The unique message identifier returned from the @SendEmail@ action.
-- * 'responseStatus' - The response status code.
mkSendEmailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'messageId'
  Lude.Text ->
  SendEmailResponse
mkSendEmailResponse pResponseStatus_ pMessageId_ =
  SendEmailResponse'
    { responseStatus = pResponseStatus_,
      messageId = pMessageId_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sersResponseStatus :: Lens.Lens' SendEmailResponse Lude.Int
sersResponseStatus = Lens.lens (responseStatus :: SendEmailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendEmailResponse)
{-# DEPRECATED sersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The unique message identifier returned from the @SendEmail@ action.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sersMessageId :: Lens.Lens' SendEmailResponse Lude.Text
sersMessageId = Lens.lens (messageId :: SendEmailResponse -> Lude.Text) (\s a -> s {messageId = a} :: SendEmailResponse)
{-# DEPRECATED sersMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}
