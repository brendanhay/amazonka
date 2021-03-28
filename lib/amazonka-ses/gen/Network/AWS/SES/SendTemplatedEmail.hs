{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SendTemplatedEmail (..)
    , mkSendTemplatedEmail
    -- ** Request lenses
    , steSource
    , steDestination
    , steTemplate
    , steTemplateData
    , steConfigurationSetName
    , steReplyToAddresses
    , steReturnPath
    , steReturnPathArn
    , steSourceArn
    , steTags
    , steTemplateArn

    -- * Destructuring the response
    , SendTemplatedEmailResponse (..)
    , mkSendTemplatedEmailResponse
    -- ** Response lenses
    , sterrsMessageId
    , sterrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to send a templated email using Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSendTemplatedEmail' smart constructor.
data SendTemplatedEmail = SendTemplatedEmail'
  { source :: Types.Source
    -- ^ The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
  , destination :: Types.Destination
    -- ^ The destination for this email, composed of To:, CC:, and BCC: fields. A Destination can include up to 50 recipients across these three fields.
  , template :: Types.TemplateName
    -- ^ The template to use when sending this email.
  , templateData :: Types.TemplateData
    -- ^ A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
  , configurationSetName :: Core.Maybe Types.ConfigurationSetName
    -- ^ The name of the configuration set to use when you send an email using @SendTemplatedEmail@ .
  , replyToAddresses :: Core.Maybe [Types.Address]
    -- ^ The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
  , returnPath :: Core.Maybe Types.ReturnPath
    -- ^ The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. 
  , returnPathArn :: Core.Maybe Types.ReturnPathArn
    -- ^ This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
  , sourceArn :: Core.Maybe Types.SourceArn
    -- ^ This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
  , tags :: Core.Maybe [Types.MessageTag]
    -- ^ A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
  , templateArn :: Core.Maybe Types.TemplateArn
    -- ^ The ARN of the template to use when sending this email.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendTemplatedEmail' value with any optional fields omitted.
mkSendTemplatedEmail
    :: Types.Source -- ^ 'source'
    -> Types.Destination -- ^ 'destination'
    -> Types.TemplateName -- ^ 'template'
    -> Types.TemplateData -- ^ 'templateData'
    -> SendTemplatedEmail
mkSendTemplatedEmail source destination template templateData
  = SendTemplatedEmail'{source, destination, template, templateData,
                        configurationSetName = Core.Nothing,
                        replyToAddresses = Core.Nothing, returnPath = Core.Nothing,
                        returnPathArn = Core.Nothing, sourceArn = Core.Nothing,
                        tags = Core.Nothing, templateArn = Core.Nothing}

-- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steSource :: Lens.Lens' SendTemplatedEmail Types.Source
steSource = Lens.field @"source"
{-# INLINEABLE steSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | The destination for this email, composed of To:, CC:, and BCC: fields. A Destination can include up to 50 recipients across these three fields.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steDestination :: Lens.Lens' SendTemplatedEmail Types.Destination
steDestination = Lens.field @"destination"
{-# INLINEABLE steDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | The template to use when sending this email.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTemplate :: Lens.Lens' SendTemplatedEmail Types.TemplateName
steTemplate = Lens.field @"template"
{-# INLINEABLE steTemplate #-}
{-# DEPRECATED template "Use generic-lens or generic-optics with 'template' instead"  #-}

-- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- /Note:/ Consider using 'templateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTemplateData :: Lens.Lens' SendTemplatedEmail Types.TemplateData
steTemplateData = Lens.field @"templateData"
{-# INLINEABLE steTemplateData #-}
{-# DEPRECATED templateData "Use generic-lens or generic-optics with 'templateData' instead"  #-}

-- | The name of the configuration set to use when you send an email using @SendTemplatedEmail@ .
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steConfigurationSetName :: Lens.Lens' SendTemplatedEmail (Core.Maybe Types.ConfigurationSetName)
steConfigurationSetName = Lens.field @"configurationSetName"
{-# INLINEABLE steConfigurationSetName #-}
{-# DEPRECATED configurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead"  #-}

-- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
--
-- /Note:/ Consider using 'replyToAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steReplyToAddresses :: Lens.Lens' SendTemplatedEmail (Core.Maybe [Types.Address])
steReplyToAddresses = Lens.field @"replyToAddresses"
{-# INLINEABLE steReplyToAddresses #-}
{-# DEPRECATED replyToAddresses "Use generic-lens or generic-optics with 'replyToAddresses' instead"  #-}

-- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. 
--
-- /Note:/ Consider using 'returnPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steReturnPath :: Lens.Lens' SendTemplatedEmail (Core.Maybe Types.ReturnPath)
steReturnPath = Lens.field @"returnPath"
{-# INLINEABLE steReturnPath #-}
{-# DEPRECATED returnPath "Use generic-lens or generic-optics with 'returnPath' instead"  #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'returnPathArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steReturnPathArn :: Lens.Lens' SendTemplatedEmail (Core.Maybe Types.ReturnPathArn)
steReturnPathArn = Lens.field @"returnPathArn"
{-# INLINEABLE steReturnPathArn #-}
{-# DEPRECATED returnPathArn "Use generic-lens or generic-optics with 'returnPathArn' instead"  #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter.
--
-- For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ .
-- For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steSourceArn :: Lens.Lens' SendTemplatedEmail (Core.Maybe Types.SourceArn)
steSourceArn = Lens.field @"sourceArn"
{-# INLINEABLE steSourceArn #-}
{-# DEPRECATED sourceArn "Use generic-lens or generic-optics with 'sourceArn' instead"  #-}

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTags :: Lens.Lens' SendTemplatedEmail (Core.Maybe [Types.MessageTag])
steTags = Lens.field @"tags"
{-# INLINEABLE steTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ARN of the template to use when sending this email.
--
-- /Note:/ Consider using 'templateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTemplateArn :: Lens.Lens' SendTemplatedEmail (Core.Maybe Types.TemplateArn)
steTemplateArn = Lens.field @"templateArn"
{-# INLINEABLE steTemplateArn #-}
{-# DEPRECATED templateArn "Use generic-lens or generic-optics with 'templateArn' instead"  #-}

instance Core.ToQuery SendTemplatedEmail where
        toQuery SendTemplatedEmail{..}
          = Core.toQueryPair "Action" ("SendTemplatedEmail" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "Source" source
              Core.<> Core.toQueryPair "Destination" destination
              Core.<> Core.toQueryPair "Template" template
              Core.<> Core.toQueryPair "TemplateData" templateData
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ConfigurationSetName")
                configurationSetName
              Core.<>
              Core.toQueryPair "ReplyToAddresses"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   replyToAddresses)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReturnPath") returnPath
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReturnPathArn")
                returnPathArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceArn") sourceArn
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateArn") templateArn

instance Core.ToHeaders SendTemplatedEmail where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SendTemplatedEmail where
        type Rs SendTemplatedEmail = SendTemplatedEmailResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "SendTemplatedEmailResult"
              (\ s h x ->
                 SendTemplatedEmailResponse' Core.<$>
                   (x Core..@ "MessageId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSendTemplatedEmailResponse' smart constructor.
data SendTemplatedEmailResponse = SendTemplatedEmailResponse'
  { messageId :: Types.MessageId
    -- ^ The unique message identifier returned from the @SendTemplatedEmail@ action. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendTemplatedEmailResponse' value with any optional fields omitted.
mkSendTemplatedEmailResponse
    :: Types.MessageId -- ^ 'messageId'
    -> Core.Int -- ^ 'responseStatus'
    -> SendTemplatedEmailResponse
mkSendTemplatedEmailResponse messageId responseStatus
  = SendTemplatedEmailResponse'{messageId, responseStatus}

-- | The unique message identifier returned from the @SendTemplatedEmail@ action. 
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sterrsMessageId :: Lens.Lens' SendTemplatedEmailResponse Types.MessageId
sterrsMessageId = Lens.field @"messageId"
{-# INLINEABLE sterrsMessageId #-}
{-# DEPRECATED messageId "Use generic-lens or generic-optics with 'messageId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sterrsResponseStatus :: Lens.Lens' SendTemplatedEmailResponse Core.Int
sterrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sterrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
