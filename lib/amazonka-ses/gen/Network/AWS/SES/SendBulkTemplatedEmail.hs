{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SendBulkTemplatedEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message to multiple destinations. The message body is created using an email template.
--
-- In order to send email using the @SendBulkTemplatedEmail@ operation, your call to the API must meet the following requirements:
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
--     * Each @Destination@ parameter must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format /UserName@[SubDomain.]Domain.TopLevelDomain/ ), the entire message will be rejected, even if the message contains other recipients that are valid.
--
--
--     * The message may not include more than 50 recipients, across the To:, CC: and BCC: fields. If you need to send an email message to a larger audience, you can divide your recipient list into groups of 50 or fewer, and then call the @SendBulkTemplatedEmail@ operation several times to send the message to each group.
--
--
--     * The number of destinations you can contact in a single call to the API may be limited by your account's maximum sending rate.
module Network.AWS.SES.SendBulkTemplatedEmail
  ( -- * Creating a request
    SendBulkTemplatedEmail (..),
    mkSendBulkTemplatedEmail,

    -- ** Request lenses
    sbteReturnPath,
    sbteConfigurationSetName,
    sbteSourceARN,
    sbteDestinations,
    sbteDefaultTags,
    sbteReturnPathARN,
    sbteSource,
    sbteTemplateARN,
    sbteTemplate,
    sbteDefaultTemplateData,
    sbteReplyToAddresses,

    -- * Destructuring the response
    SendBulkTemplatedEmailResponse (..),
    mkSendBulkTemplatedEmailResponse,

    -- ** Response lenses
    sbtersStatus,
    sbtersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to send a templated email to multiple destinations using Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSendBulkTemplatedEmail' smart constructor.
data SendBulkTemplatedEmail = SendBulkTemplatedEmail'
  { -- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
    returnPath :: Lude.Maybe Lude.Text,
    -- | The name of the configuration set to use when you send an email using @SendBulkTemplatedEmail@ .
    configurationSetName :: Lude.Maybe Lude.Text,
    -- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
    --
    -- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
    -- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    sourceARN :: Lude.Maybe Lude.Text,
    -- | One or more @Destination@ objects. All of the recipients in a @Destination@ will receive the same version of the email. You can specify up to 50 @Destination@ objects within a @Destinations@ array.
    destinations :: [BulkEmailDestination],
    -- | A list of tags, in the form of name/value pairs, to apply to an email that you send to a destination using @SendBulkTemplatedEmail@ .
    defaultTags :: Lude.Maybe [MessageTag],
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
    -- | A list of replacement values to apply to the template when replacement data is not specified in a Destination object. These values act as a default or fallback option when no other data is available.
    --
    -- The template data is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
    defaultTemplateData :: Lude.Maybe Lude.Text,
    -- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
    replyToAddresses :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendBulkTemplatedEmail' with the minimum fields required to make a request.
--
-- * 'returnPath' - The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
-- * 'configurationSetName' - The name of the configuration set to use when you send an email using @SendBulkTemplatedEmail@ .
-- * 'sourceARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- * 'destinations' - One or more @Destination@ objects. All of the recipients in a @Destination@ will receive the same version of the email. You can specify up to 50 @Destination@ objects within a @Destinations@ array.
-- * 'defaultTags' - A list of tags, in the form of name/value pairs, to apply to an email that you send to a destination using @SendBulkTemplatedEmail@ .
-- * 'returnPathARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- * 'source' - The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- * 'templateARN' - The ARN of the template to use when sending this email.
-- * 'template' - The template to use when sending this email.
-- * 'defaultTemplateData' - A list of replacement values to apply to the template when replacement data is not specified in a Destination object. These values act as a default or fallback option when no other data is available.
--
-- The template data is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
-- * 'replyToAddresses' - The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
mkSendBulkTemplatedEmail ::
  -- | 'source'
  Lude.Text ->
  -- | 'template'
  Lude.Text ->
  SendBulkTemplatedEmail
mkSendBulkTemplatedEmail pSource_ pTemplate_ =
  SendBulkTemplatedEmail'
    { returnPath = Lude.Nothing,
      configurationSetName = Lude.Nothing,
      sourceARN = Lude.Nothing,
      destinations = Lude.mempty,
      defaultTags = Lude.Nothing,
      returnPathARN = Lude.Nothing,
      source = pSource_,
      templateARN = Lude.Nothing,
      template = pTemplate_,
      defaultTemplateData = Lude.Nothing,
      replyToAddresses = Lude.Nothing
    }

-- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- /Note:/ Consider using 'returnPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteReturnPath :: Lens.Lens' SendBulkTemplatedEmail (Lude.Maybe Lude.Text)
sbteReturnPath = Lens.lens (returnPath :: SendBulkTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {returnPath = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteReturnPath "Use generic-lens or generic-optics with 'returnPath' instead." #-}

-- | The name of the configuration set to use when you send an email using @SendBulkTemplatedEmail@ .
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteConfigurationSetName :: Lens.Lens' SendBulkTemplatedEmail (Lude.Maybe Lude.Text)
sbteConfigurationSetName = Lens.lens (configurationSetName :: SendBulkTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {configurationSetName = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteSourceARN :: Lens.Lens' SendBulkTemplatedEmail (Lude.Maybe Lude.Text)
sbteSourceARN = Lens.lens (sourceARN :: SendBulkTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | One or more @Destination@ objects. All of the recipients in a @Destination@ will receive the same version of the email. You can specify up to 50 @Destination@ objects within a @Destinations@ array.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteDestinations :: Lens.Lens' SendBulkTemplatedEmail [BulkEmailDestination]
sbteDestinations = Lens.lens (destinations :: SendBulkTemplatedEmail -> [BulkEmailDestination]) (\s a -> s {destinations = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send to a destination using @SendBulkTemplatedEmail@ .
--
-- /Note:/ Consider using 'defaultTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteDefaultTags :: Lens.Lens' SendBulkTemplatedEmail (Lude.Maybe [MessageTag])
sbteDefaultTags = Lens.lens (defaultTags :: SendBulkTemplatedEmail -> Lude.Maybe [MessageTag]) (\s a -> s {defaultTags = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteDefaultTags "Use generic-lens or generic-optics with 'defaultTags' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'returnPathARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteReturnPathARN :: Lens.Lens' SendBulkTemplatedEmail (Lude.Maybe Lude.Text)
sbteReturnPathARN = Lens.lens (returnPathARN :: SendBulkTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {returnPathARN = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteReturnPathARN "Use generic-lens or generic-optics with 'returnPathARN' instead." #-}

-- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteSource :: Lens.Lens' SendBulkTemplatedEmail Lude.Text
sbteSource = Lens.lens (source :: SendBulkTemplatedEmail -> Lude.Text) (\s a -> s {source = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The ARN of the template to use when sending this email.
--
-- /Note:/ Consider using 'templateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteTemplateARN :: Lens.Lens' SendBulkTemplatedEmail (Lude.Maybe Lude.Text)
sbteTemplateARN = Lens.lens (templateARN :: SendBulkTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {templateARN = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteTemplateARN "Use generic-lens or generic-optics with 'templateARN' instead." #-}

-- | The template to use when sending this email.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteTemplate :: Lens.Lens' SendBulkTemplatedEmail Lude.Text
sbteTemplate = Lens.lens (template :: SendBulkTemplatedEmail -> Lude.Text) (\s a -> s {template = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteTemplate "Use generic-lens or generic-optics with 'template' instead." #-}

-- | A list of replacement values to apply to the template when replacement data is not specified in a Destination object. These values act as a default or fallback option when no other data is available.
--
-- The template data is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- /Note:/ Consider using 'defaultTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteDefaultTemplateData :: Lens.Lens' SendBulkTemplatedEmail (Lude.Maybe Lude.Text)
sbteDefaultTemplateData = Lens.lens (defaultTemplateData :: SendBulkTemplatedEmail -> Lude.Maybe Lude.Text) (\s a -> s {defaultTemplateData = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteDefaultTemplateData "Use generic-lens or generic-optics with 'defaultTemplateData' instead." #-}

-- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
--
-- /Note:/ Consider using 'replyToAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteReplyToAddresses :: Lens.Lens' SendBulkTemplatedEmail (Lude.Maybe [Lude.Text])
sbteReplyToAddresses = Lens.lens (replyToAddresses :: SendBulkTemplatedEmail -> Lude.Maybe [Lude.Text]) (\s a -> s {replyToAddresses = a} :: SendBulkTemplatedEmail)
{-# DEPRECATED sbteReplyToAddresses "Use generic-lens or generic-optics with 'replyToAddresses' instead." #-}

instance Lude.AWSRequest SendBulkTemplatedEmail where
  type Rs SendBulkTemplatedEmail = SendBulkTemplatedEmailResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SendBulkTemplatedEmailResult"
      ( \s h x ->
          SendBulkTemplatedEmailResponse'
            Lude.<$> ( x Lude..@? "Status" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendBulkTemplatedEmail where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SendBulkTemplatedEmail where
  toPath = Lude.const "/"

instance Lude.ToQuery SendBulkTemplatedEmail where
  toQuery SendBulkTemplatedEmail' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SendBulkTemplatedEmail" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ReturnPath" Lude.=: returnPath,
        "ConfigurationSetName" Lude.=: configurationSetName,
        "SourceArn" Lude.=: sourceARN,
        "Destinations" Lude.=: Lude.toQueryList "member" destinations,
        "DefaultTags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> defaultTags),
        "ReturnPathArn" Lude.=: returnPathARN,
        "Source" Lude.=: source,
        "TemplateArn" Lude.=: templateARN,
        "Template" Lude.=: template,
        "DefaultTemplateData" Lude.=: defaultTemplateData,
        "ReplyToAddresses"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> replyToAddresses)
      ]

-- | /See:/ 'mkSendBulkTemplatedEmailResponse' smart constructor.
data SendBulkTemplatedEmailResponse = SendBulkTemplatedEmailResponse'
  { -- | The unique message identifier returned from the @SendBulkTemplatedEmail@ action.
    status :: [BulkEmailDestinationStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendBulkTemplatedEmailResponse' with the minimum fields required to make a request.
--
-- * 'status' - The unique message identifier returned from the @SendBulkTemplatedEmail@ action.
-- * 'responseStatus' - The response status code.
mkSendBulkTemplatedEmailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendBulkTemplatedEmailResponse
mkSendBulkTemplatedEmailResponse pResponseStatus_ =
  SendBulkTemplatedEmailResponse'
    { status = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The unique message identifier returned from the @SendBulkTemplatedEmail@ action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbtersStatus :: Lens.Lens' SendBulkTemplatedEmailResponse [BulkEmailDestinationStatus]
sbtersStatus = Lens.lens (status :: SendBulkTemplatedEmailResponse -> [BulkEmailDestinationStatus]) (\s a -> s {status = a} :: SendBulkTemplatedEmailResponse)
{-# DEPRECATED sbtersStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbtersResponseStatus :: Lens.Lens' SendBulkTemplatedEmailResponse Lude.Int
sbtersResponseStatus = Lens.lens (responseStatus :: SendBulkTemplatedEmailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendBulkTemplatedEmailResponse)
{-# DEPRECATED sbtersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
