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
    sbteSource,
    sbteTemplate,
    sbteDestinations,
    sbteConfigurationSetName,
    sbteDefaultTags,
    sbteDefaultTemplateData,
    sbteReplyToAddresses,
    sbteReturnPath,
    sbteReturnPathArn,
    sbteSourceArn,
    sbteTemplateArn,

    -- * Destructuring the response
    SendBulkTemplatedEmailResponse (..),
    mkSendBulkTemplatedEmailResponse,

    -- ** Response lenses
    sbterrsStatus,
    sbterrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to send a templated email to multiple destinations using Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSendBulkTemplatedEmail' smart constructor.
data SendBulkTemplatedEmail = SendBulkTemplatedEmail'
  { -- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
    --
    -- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    source :: Types.Source,
    -- | The template to use when sending this email.
    template :: Types.TemplateName,
    -- | One or more @Destination@ objects. All of the recipients in a @Destination@ will receive the same version of the email. You can specify up to 50 @Destination@ objects within a @Destinations@ array.
    destinations :: [Types.BulkEmailDestination],
    -- | The name of the configuration set to use when you send an email using @SendBulkTemplatedEmail@ .
    configurationSetName :: Core.Maybe Types.ConfigurationSetName,
    -- | A list of tags, in the form of name/value pairs, to apply to an email that you send to a destination using @SendBulkTemplatedEmail@ .
    defaultTags :: Core.Maybe [Types.MessageTag],
    -- | A list of replacement values to apply to the template when replacement data is not specified in a Destination object. These values act as a default or fallback option when no other data is available.
    --
    -- The template data is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
    defaultTemplateData :: Core.Maybe Types.DefaultTemplateData,
    -- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
    replyToAddresses :: Core.Maybe [Types.Address],
    -- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
    returnPath :: Core.Maybe Types.ReturnPath,
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
    -- | The ARN of the template to use when sending this email.
    templateArn :: Core.Maybe Types.TemplateArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendBulkTemplatedEmail' value with any optional fields omitted.
mkSendBulkTemplatedEmail ::
  -- | 'source'
  Types.Source ->
  -- | 'template'
  Types.TemplateName ->
  SendBulkTemplatedEmail
mkSendBulkTemplatedEmail source template =
  SendBulkTemplatedEmail'
    { source,
      template,
      destinations = Core.mempty,
      configurationSetName = Core.Nothing,
      defaultTags = Core.Nothing,
      defaultTemplateData = Core.Nothing,
      replyToAddresses = Core.Nothing,
      returnPath = Core.Nothing,
      returnPathArn = Core.Nothing,
      sourceArn = Core.Nothing,
      templateArn = Core.Nothing
    }

-- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteSource :: Lens.Lens' SendBulkTemplatedEmail Types.Source
sbteSource = Lens.field @"source"
{-# DEPRECATED sbteSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The template to use when sending this email.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteTemplate :: Lens.Lens' SendBulkTemplatedEmail Types.TemplateName
sbteTemplate = Lens.field @"template"
{-# DEPRECATED sbteTemplate "Use generic-lens or generic-optics with 'template' instead." #-}

-- | One or more @Destination@ objects. All of the recipients in a @Destination@ will receive the same version of the email. You can specify up to 50 @Destination@ objects within a @Destinations@ array.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteDestinations :: Lens.Lens' SendBulkTemplatedEmail [Types.BulkEmailDestination]
sbteDestinations = Lens.field @"destinations"
{-# DEPRECATED sbteDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the configuration set to use when you send an email using @SendBulkTemplatedEmail@ .
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteConfigurationSetName :: Lens.Lens' SendBulkTemplatedEmail (Core.Maybe Types.ConfigurationSetName)
sbteConfigurationSetName = Lens.field @"configurationSetName"
{-# DEPRECATED sbteConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send to a destination using @SendBulkTemplatedEmail@ .
--
-- /Note:/ Consider using 'defaultTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteDefaultTags :: Lens.Lens' SendBulkTemplatedEmail (Core.Maybe [Types.MessageTag])
sbteDefaultTags = Lens.field @"defaultTags"
{-# DEPRECATED sbteDefaultTags "Use generic-lens or generic-optics with 'defaultTags' instead." #-}

-- | A list of replacement values to apply to the template when replacement data is not specified in a Destination object. These values act as a default or fallback option when no other data is available.
--
-- The template data is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- /Note:/ Consider using 'defaultTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteDefaultTemplateData :: Lens.Lens' SendBulkTemplatedEmail (Core.Maybe Types.DefaultTemplateData)
sbteDefaultTemplateData = Lens.field @"defaultTemplateData"
{-# DEPRECATED sbteDefaultTemplateData "Use generic-lens or generic-optics with 'defaultTemplateData' instead." #-}

-- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
--
-- /Note:/ Consider using 'replyToAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteReplyToAddresses :: Lens.Lens' SendBulkTemplatedEmail (Core.Maybe [Types.Address])
sbteReplyToAddresses = Lens.field @"replyToAddresses"
{-# DEPRECATED sbteReplyToAddresses "Use generic-lens or generic-optics with 'replyToAddresses' instead." #-}

-- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- /Note:/ Consider using 'returnPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteReturnPath :: Lens.Lens' SendBulkTemplatedEmail (Core.Maybe Types.ReturnPath)
sbteReturnPath = Lens.field @"returnPath"
{-# DEPRECATED sbteReturnPath "Use generic-lens or generic-optics with 'returnPath' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'returnPathArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteReturnPathArn :: Lens.Lens' SendBulkTemplatedEmail (Core.Maybe Types.ReturnPathArn)
sbteReturnPathArn = Lens.field @"returnPathArn"
{-# DEPRECATED sbteReturnPathArn "Use generic-lens or generic-optics with 'returnPathArn' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteSourceArn :: Lens.Lens' SendBulkTemplatedEmail (Core.Maybe Types.SourceArn)
sbteSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED sbteSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | The ARN of the template to use when sending this email.
--
-- /Note:/ Consider using 'templateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbteTemplateArn :: Lens.Lens' SendBulkTemplatedEmail (Core.Maybe Types.TemplateArn)
sbteTemplateArn = Lens.field @"templateArn"
{-# DEPRECATED sbteTemplateArn "Use generic-lens or generic-optics with 'templateArn' instead." #-}

instance Core.AWSRequest SendBulkTemplatedEmail where
  type Rs SendBulkTemplatedEmail = SendBulkTemplatedEmailResponse
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
            ( Core.pure ("Action", "SendBulkTemplatedEmail")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Source" source)
                Core.<> (Core.toQueryValue "Template" template)
                Core.<> ( Core.toQueryValue
                            "Destinations"
                            (Core.toQueryList "member" destinations)
                        )
                Core.<> ( Core.toQueryValue "ConfigurationSetName"
                            Core.<$> configurationSetName
                        )
                Core.<> ( Core.toQueryValue
                            "DefaultTags"
                            (Core.toQueryList "member" Core.<$> defaultTags)
                        )
                Core.<> ( Core.toQueryValue "DefaultTemplateData"
                            Core.<$> defaultTemplateData
                        )
                Core.<> ( Core.toQueryValue
                            "ReplyToAddresses"
                            (Core.toQueryList "member" Core.<$> replyToAddresses)
                        )
                Core.<> (Core.toQueryValue "ReturnPath" Core.<$> returnPath)
                Core.<> (Core.toQueryValue "ReturnPathArn" Core.<$> returnPathArn)
                Core.<> (Core.toQueryValue "SourceArn" Core.<$> sourceArn)
                Core.<> (Core.toQueryValue "TemplateArn" Core.<$> templateArn)
            )
      }
  response =
    Response.receiveXMLWrapper
      "SendBulkTemplatedEmailResult"
      ( \s h x ->
          SendBulkTemplatedEmailResponse'
            Core.<$> ( x Core..@? "Status" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSendBulkTemplatedEmailResponse' smart constructor.
data SendBulkTemplatedEmailResponse = SendBulkTemplatedEmailResponse'
  { -- | The unique message identifier returned from the @SendBulkTemplatedEmail@ action.
    status :: [Types.BulkEmailDestinationStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendBulkTemplatedEmailResponse' value with any optional fields omitted.
mkSendBulkTemplatedEmailResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SendBulkTemplatedEmailResponse
mkSendBulkTemplatedEmailResponse responseStatus =
  SendBulkTemplatedEmailResponse'
    { status = Core.mempty,
      responseStatus
    }

-- | The unique message identifier returned from the @SendBulkTemplatedEmail@ action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbterrsStatus :: Lens.Lens' SendBulkTemplatedEmailResponse [Types.BulkEmailDestinationStatus]
sbterrsStatus = Lens.field @"status"
{-# DEPRECATED sbterrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbterrsResponseStatus :: Lens.Lens' SendBulkTemplatedEmailResponse Core.Int
sbterrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sbterrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
