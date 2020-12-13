{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SendTemplatedEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message using an email template and immediately queues it for sending.
--
-- In order to send email using the @SendTemplatedEmail@ operation, your call to the API must meet the following requirements:
--
--     * The call must refer to an existing email template. You can create email templates using the 'CreateTemplate' operation.
--
--
--     * The message must be sent from a verified email address or domain.
--
--
--     * If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Verifying Email Addresses and Domains> in the /Amazon SES Developer Guide./
--
--
--     * The maximum message size is 10 MB.
--
--
--     * Calls to the @SendTemplatedEmail@ operation may only include one @Destination@ parameter. A destination is a set of recipients who will receive the same version of the email. The @Destination@ parameter can include up to 50 recipients, across the To:, CC: and BCC: fields.
--
--
--     * The @Destination@ parameter must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format /UserName@[SubDomain.]Domain.TopLevelDomain/ ), the entire message will be rejected, even if the message contains other recipients that are valid.
--
--
-- /Important:/ If your call to the @SendTemplatedEmail@ operation includes all of the required parameters, Amazon SES accepts it and returns a Message ID. However, if Amazon SES can't render the email because the template contains errors, it doesn't send the email. Additionally, because it already accepted the message, Amazon SES doesn't return a message stating that it was unable to send the email.
-- For these reasons, we highly recommend that you set up Amazon SES to send you notifications when Rendering Failure events occur. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Sending Personalized Email Using the Amazon SES API> in the /Amazon Simple Email Service Developer Guide/ .
module Network.AWS.SES.SendTemplatedEmail
  ( -- * Creating a request
    SendTemplatedEmail (..),
    mkSendTemplatedEmail,

    -- ** Request lenses
    steDestination,
    steReturnPath,
    steConfigurationSetName,
    steSourceARN,
    steReturnPathARN,
    steSource,
    steTemplateARN,
    steTemplate,
    steTemplateData,
    steTags,
    steReplyToAddresses,

    -- * Destructuring the response
    SendTemplatedEmailResponse (..),
    mkSendTemplatedEmailResponse,

    -- ** Response lenses
    stersMessageId,
    stersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to send a templated email using Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSendTemplatedEmail' smart constructor.
data SendTemplatedEmail = SendTemplatedEmail'
  { -- | The destination for this email, composed of To:, CC:, and BCC: fields. A Destination can include up to 50 recipients across these three fields.
    destination :: Destination,
    -- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
    returnPath :: Lude.Maybe Lude.Text,
    -- | The name of the configuration set to use when you send an email using @SendTemplatedEmail@ .
    configurationSetName :: Lude.Maybe Lude.Text,
    -- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
    --
    -- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
    -- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    sourceARN :: Lude.Maybe Lude.Text,
    -- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
    --
    -- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
    -- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    returnPathARN :: Lude.Maybe Lude.Text,
    -- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
    --
    -- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    source :: Lude.Text,
    -- | The ARN of the template to use when sending this email.
    templateARN :: Lude.Maybe Lude.Text,
    -- | The template to use when sending this email.
    template :: Lude.Text,
    -- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
    templateData :: Lude.Text,
    -- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
    tags :: Lude.Maybe [MessageTag],
    -- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
    replyToAddresses :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendTemplatedEmail' with the minimum fields required to make a request.
--
-- * 'destination' - The destination for this email, composed of To:, CC:, and BCC: fields. A Destination can include up to 50 recipients across these three fields.
-- * 'returnPath' - The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
-- * 'configurationSetName' - The name of the configuration set to use when you send an email using @SendTemplatedEmail@ .
-- * 'sourceARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- * 'returnPathARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- * 'source' - The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- * 'templateARN' - The ARN of the template to use when sending this email.
-- * 'template' - The template to use when sending this email.
-- * 'templateData' - A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
-- * 'tags' - A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
-- * 'replyToAddresses' - The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
mkSendTemplatedEmail ::
  -- | 'destination'
  Destination ->
  -- | 'source'
  Lude.Text ->
  -- | 'template'
  Lude.Text ->
  -- | 'templateData'
  Lude.Text ->
  SendTemplatedEmail
mkSendTemplatedEmail
  pDestination_
  pSource_
  pTemplate_
  pTemplateData_ =
    SendTemplatedEmail'
      { destination = pDestination_,
        returnPath = Lude.Nothing,
        configurationSetName = Lude.Nothing,
        sourceARN = Lude.Nothing,
        returnPathARN = Lude.Nothing,
        source = pSource_,
        templateARN = Lude.Nothing,
        template = pTemplate_,
        templateData = pTemplateData_,
        tags = Lude.Nothing,
        replyToAddresses = Lude.Nothing
      }

-- | The destination for this email, composed of To:, CC:, and BCC: fields. A Destination can include up to 50 recipients across these three fields.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steDestination :: Lens.Lens' SendTemplatedEmail Destination
steDestination = Lens.lens (destination :: SendTemplatedEmail -> Destination) (\s a -> s {destination = a} :: SendTemplatedEmail)
{-# DEPRECATED steDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- /Note:/ Consider using 'returnPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steReturnPath :: Lens.Lens' SendTemplatedEmail (Lude.Maybe Lude.Text)
steReturnPath = Lens.lens (returnPath :: SendTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {returnPath = a} :: SendTemplatedEmail)
{-# DEPRECATED steReturnPath "Use generic-lens or generic-optics with 'returnPath' instead." #-}

-- | The name of the configuration set to use when you send an email using @SendTemplatedEmail@ .
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steConfigurationSetName :: Lens.Lens' SendTemplatedEmail (Lude.Maybe Lude.Text)
steConfigurationSetName = Lens.lens (configurationSetName :: SendTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {configurationSetName = a} :: SendTemplatedEmail)
{-# DEPRECATED steConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steSourceARN :: Lens.Lens' SendTemplatedEmail (Lude.Maybe Lude.Text)
steSourceARN = Lens.lens (sourceARN :: SendTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: SendTemplatedEmail)
{-# DEPRECATED steSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'returnPathARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steReturnPathARN :: Lens.Lens' SendTemplatedEmail (Lude.Maybe Lude.Text)
steReturnPathARN = Lens.lens (returnPathARN :: SendTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {returnPathARN = a} :: SendTemplatedEmail)
{-# DEPRECATED steReturnPathARN "Use generic-lens or generic-optics with 'returnPathARN' instead." #-}

-- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steSource :: Lens.Lens' SendTemplatedEmail Lude.Text
steSource = Lens.lens (source :: SendTemplatedEmail -> Lude.Text) (\s a -> s {source = a} :: SendTemplatedEmail)
{-# DEPRECATED steSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The ARN of the template to use when sending this email.
--
-- /Note:/ Consider using 'templateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTemplateARN :: Lens.Lens' SendTemplatedEmail (Lude.Maybe Lude.Text)
steTemplateARN = Lens.lens (templateARN :: SendTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {templateARN = a} :: SendTemplatedEmail)
{-# DEPRECATED steTemplateARN "Use generic-lens or generic-optics with 'templateARN' instead." #-}

-- | The template to use when sending this email.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTemplate :: Lens.Lens' SendTemplatedEmail Lude.Text
steTemplate = Lens.lens (template :: SendTemplatedEmail -> Lude.Text) (\s a -> s {template = a} :: SendTemplatedEmail)
{-# DEPRECATED steTemplate "Use generic-lens or generic-optics with 'template' instead." #-}

-- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- /Note:/ Consider using 'templateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTemplateData :: Lens.Lens' SendTemplatedEmail Lude.Text
steTemplateData = Lens.lens (templateData :: SendTemplatedEmail -> Lude.Text) (\s a -> s {templateData = a} :: SendTemplatedEmail)
{-# DEPRECATED steTemplateData "Use generic-lens or generic-optics with 'templateData' instead." #-}

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTags :: Lens.Lens' SendTemplatedEmail (Lude.Maybe [MessageTag])
steTags = Lens.lens (tags :: SendTemplatedEmail -> Lude.Maybe [MessageTag]) (\s a -> s {tags = a} :: SendTemplatedEmail)
{-# DEPRECATED steTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
--
-- /Note:/ Consider using 'replyToAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steReplyToAddresses :: Lens.Lens' SendTemplatedEmail (Lude.Maybe [Lude.Text])
steReplyToAddresses = Lens.lens (replyToAddresses :: SendTemplatedEmail -> Lude.Maybe [Lude.Text]) (\s a -> s {replyToAddresses = a} :: SendTemplatedEmail)
{-# DEPRECATED steReplyToAddresses "Use generic-lens or generic-optics with 'replyToAddresses' instead." #-}

instance Lude.AWSRequest SendTemplatedEmail where
  type Rs SendTemplatedEmail = SendTemplatedEmailResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SendTemplatedEmailResult"
      ( \s h x ->
          SendTemplatedEmailResponse'
            Lude.<$> (x Lude..@ "MessageId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendTemplatedEmail where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SendTemplatedEmail where
  toPath = Lude.const "/"

instance Lude.ToQuery SendTemplatedEmail where
  toQuery SendTemplatedEmail' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SendTemplatedEmail" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Destination" Lude.=: destination,
        "ReturnPath" Lude.=: returnPath,
        "ConfigurationSetName" Lude.=: configurationSetName,
        "SourceArn" Lude.=: sourceARN,
        "ReturnPathArn" Lude.=: returnPathARN,
        "Source" Lude.=: source,
        "TemplateArn" Lude.=: templateARN,
        "Template" Lude.=: template,
        "TemplateData" Lude.=: templateData,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "ReplyToAddresses"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> replyToAddresses)
      ]

-- | /See:/ 'mkSendTemplatedEmailResponse' smart constructor.
data SendTemplatedEmailResponse = SendTemplatedEmailResponse'
  { -- | The unique message identifier returned from the @SendTemplatedEmail@ action.
    messageId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendTemplatedEmailResponse' with the minimum fields required to make a request.
--
-- * 'messageId' - The unique message identifier returned from the @SendTemplatedEmail@ action.
-- * 'responseStatus' - The response status code.
mkSendTemplatedEmailResponse ::
  -- | 'messageId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  SendTemplatedEmailResponse
mkSendTemplatedEmailResponse pMessageId_ pResponseStatus_ =
  SendTemplatedEmailResponse'
    { messageId = pMessageId_,
      responseStatus = pResponseStatus_
    }

-- | The unique message identifier returned from the @SendTemplatedEmail@ action.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stersMessageId :: Lens.Lens' SendTemplatedEmailResponse Lude.Text
stersMessageId = Lens.lens (messageId :: SendTemplatedEmailResponse -> Lude.Text) (\s a -> s {messageId = a} :: SendTemplatedEmailResponse)
{-# DEPRECATED stersMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stersResponseStatus :: Lens.Lens' SendTemplatedEmailResponse Lude.Int
stersResponseStatus = Lens.lens (responseStatus :: SendTemplatedEmailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendTemplatedEmailResponse)
{-# DEPRECATED stersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
