{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    seSource,
    seDestination,
    seMessage,
    seConfigurationSetName,
    seReplyToAddresses,
    seReturnPath,
    seReturnPathArn,
    seSourceArn,
    seTags,

    -- * Destructuring the response
    SendEmailResponse (..),
    mkSendEmailResponse,

    -- ** Response lenses
    serrsMessageId,
    serrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to send a single formatted email using Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-formatted.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSendEmail' smart constructor.
data SendEmail = SendEmail'
  { -- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
    --
    -- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    source :: Types.Address,
    -- | The destination for this email, composed of To:, CC:, and BCC: fields.
    destination :: Types.Destination,
    -- | The message to be sent.
    message :: Types.Message,
    -- | The name of the configuration set to use when you send an email using @SendEmail@ .
    configurationSetName :: Core.Maybe Types.ConfigurationSetName,
    -- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
    replyToAddresses :: Core.Maybe [Types.Address],
    -- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
    returnPath :: Core.Maybe Types.Address,
    -- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
    --
    -- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
    -- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    returnPathArn :: Core.Maybe Types.ReturnPathArn,
    -- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
    --
    -- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
    -- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    sourceArn :: Core.Maybe Types.SourceArn,
    -- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
    tags :: Core.Maybe [Types.MessageTag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendEmail' value with any optional fields omitted.
mkSendEmail ::
  -- | 'source'
  Types.Address ->
  -- | 'destination'
  Types.Destination ->
  -- | 'message'
  Types.Message ->
  SendEmail
mkSendEmail source destination message =
  SendEmail'
    { source,
      destination,
      message,
      configurationSetName = Core.Nothing,
      replyToAddresses = Core.Nothing,
      returnPath = Core.Nothing,
      returnPathArn = Core.Nothing,
      sourceArn = Core.Nothing,
      tags = Core.Nothing
    }

-- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seSource :: Lens.Lens' SendEmail Types.Address
seSource = Lens.field @"source"
{-# DEPRECATED seSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The destination for this email, composed of To:, CC:, and BCC: fields.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seDestination :: Lens.Lens' SendEmail Types.Destination
seDestination = Lens.field @"destination"
{-# DEPRECATED seDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The message to be sent.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMessage :: Lens.Lens' SendEmail Types.Message
seMessage = Lens.field @"message"
{-# DEPRECATED seMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The name of the configuration set to use when you send an email using @SendEmail@ .
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seConfigurationSetName :: Lens.Lens' SendEmail (Core.Maybe Types.ConfigurationSetName)
seConfigurationSetName = Lens.field @"configurationSetName"
{-# DEPRECATED seConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
--
-- /Note:/ Consider using 'replyToAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seReplyToAddresses :: Lens.Lens' SendEmail (Core.Maybe [Types.Address])
seReplyToAddresses = Lens.field @"replyToAddresses"
{-# DEPRECATED seReplyToAddresses "Use generic-lens or generic-optics with 'replyToAddresses' instead." #-}

-- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- /Note:/ Consider using 'returnPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seReturnPath :: Lens.Lens' SendEmail (Core.Maybe Types.Address)
seReturnPath = Lens.field @"returnPath"
{-# DEPRECATED seReturnPath "Use generic-lens or generic-optics with 'returnPath' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'returnPathArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seReturnPathArn :: Lens.Lens' SendEmail (Core.Maybe Types.ReturnPathArn)
seReturnPathArn = Lens.field @"returnPathArn"
{-# DEPRECATED seReturnPathArn "Use generic-lens or generic-optics with 'returnPathArn' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seSourceArn :: Lens.Lens' SendEmail (Core.Maybe Types.SourceArn)
seSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED seSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTags :: Lens.Lens' SendEmail (Core.Maybe [Types.MessageTag])
seTags = Lens.field @"tags"
{-# DEPRECATED seTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest SendEmail where
  type Rs SendEmail = SendEmailResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SendEmail")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Source" source)
                Core.<> (Core.toQueryValue "Destination" destination)
                Core.<> (Core.toQueryValue "Message" message)
                Core.<> ( Core.toQueryValue "ConfigurationSetName"
                            Core.<$> configurationSetName
                        )
                Core.<> ( Core.toQueryValue
                            "ReplyToAddresses"
                            (Core.toQueryList "member" Core.<$> replyToAddresses)
                        )
                Core.<> (Core.toQueryValue "ReturnPath" Core.<$> returnPath)
                Core.<> (Core.toQueryValue "ReturnPathArn" Core.<$> returnPathArn)
                Core.<> (Core.toQueryValue "SourceArn" Core.<$> sourceArn)
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "SendEmailResult"
      ( \s h x ->
          SendEmailResponse'
            Core.<$> (x Core..@ "MessageId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents a unique message ID.
--
-- /See:/ 'mkSendEmailResponse' smart constructor.
data SendEmailResponse = SendEmailResponse'
  { -- | The unique message identifier returned from the @SendEmail@ action.
    messageId :: Types.MessageId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendEmailResponse' value with any optional fields omitted.
mkSendEmailResponse ::
  -- | 'messageId'
  Types.MessageId ->
  -- | 'responseStatus'
  Core.Int ->
  SendEmailResponse
mkSendEmailResponse messageId responseStatus =
  SendEmailResponse' {messageId, responseStatus}

-- | The unique message identifier returned from the @SendEmail@ action.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
serrsMessageId :: Lens.Lens' SendEmailResponse Types.MessageId
serrsMessageId = Lens.field @"messageId"
{-# DEPRECATED serrsMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
serrsResponseStatus :: Lens.Lens' SendEmailResponse Core.Int
serrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED serrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
