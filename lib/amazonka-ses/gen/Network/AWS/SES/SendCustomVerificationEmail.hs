{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SendCustomVerificationEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an email address to the list of identities for your Amazon SES account in the current AWS Region and attempts to verify it. As a result of executing this operation, a customized verification email is sent to the specified address.
--
-- To use this operation, you must first create a custom verification email template. For more information about creating and using custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SendCustomVerificationEmail
    (
    -- * Creating a request
      SendCustomVerificationEmail (..)
    , mkSendCustomVerificationEmail
    -- ** Request lenses
    , scveEmailAddress
    , scveTemplateName
    , scveConfigurationSetName

    -- * Destructuring the response
    , SendCustomVerificationEmailResponse (..)
    , mkSendCustomVerificationEmailResponse
    -- ** Response lenses
    , scverrsMessageId
    , scverrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to send a custom verification email to a specified recipient.
--
-- /See:/ 'mkSendCustomVerificationEmail' smart constructor.
data SendCustomVerificationEmail = SendCustomVerificationEmail'
  { emailAddress :: Types.Address
    -- ^ The email address to verify.
  , templateName :: Types.TemplateName
    -- ^ The name of the custom verification email template to use when sending the verification email.
  , configurationSetName :: Core.Maybe Types.ConfigurationSetName
    -- ^ Name of a configuration set to use when sending the verification email.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendCustomVerificationEmail' value with any optional fields omitted.
mkSendCustomVerificationEmail
    :: Types.Address -- ^ 'emailAddress'
    -> Types.TemplateName -- ^ 'templateName'
    -> SendCustomVerificationEmail
mkSendCustomVerificationEmail emailAddress templateName
  = SendCustomVerificationEmail'{emailAddress, templateName,
                                 configurationSetName = Core.Nothing}

-- | The email address to verify.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scveEmailAddress :: Lens.Lens' SendCustomVerificationEmail Types.Address
scveEmailAddress = Lens.field @"emailAddress"
{-# INLINEABLE scveEmailAddress #-}
{-# DEPRECATED emailAddress "Use generic-lens or generic-optics with 'emailAddress' instead"  #-}

-- | The name of the custom verification email template to use when sending the verification email.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scveTemplateName :: Lens.Lens' SendCustomVerificationEmail Types.TemplateName
scveTemplateName = Lens.field @"templateName"
{-# INLINEABLE scveTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | Name of a configuration set to use when sending the verification email.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scveConfigurationSetName :: Lens.Lens' SendCustomVerificationEmail (Core.Maybe Types.ConfigurationSetName)
scveConfigurationSetName = Lens.field @"configurationSetName"
{-# INLINEABLE scveConfigurationSetName #-}
{-# DEPRECATED configurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead"  #-}

instance Core.ToQuery SendCustomVerificationEmail where
        toQuery SendCustomVerificationEmail{..}
          = Core.toQueryPair "Action"
              ("SendCustomVerificationEmail" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "EmailAddress" emailAddress
              Core.<> Core.toQueryPair "TemplateName" templateName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ConfigurationSetName")
                configurationSetName

instance Core.ToHeaders SendCustomVerificationEmail where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SendCustomVerificationEmail where
        type Rs SendCustomVerificationEmail =
             SendCustomVerificationEmailResponse
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
          = Response.receiveXMLWrapper "SendCustomVerificationEmailResult"
              (\ s h x ->
                 SendCustomVerificationEmailResponse' Core.<$>
                   (x Core..@? "MessageId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response received when attempting to send the custom verification email.
--
-- /See:/ 'mkSendCustomVerificationEmailResponse' smart constructor.
data SendCustomVerificationEmailResponse = SendCustomVerificationEmailResponse'
  { messageId :: Core.Maybe Types.MessageId
    -- ^ The unique message identifier returned from the @SendCustomVerificationEmail@ operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendCustomVerificationEmailResponse' value with any optional fields omitted.
mkSendCustomVerificationEmailResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SendCustomVerificationEmailResponse
mkSendCustomVerificationEmailResponse responseStatus
  = SendCustomVerificationEmailResponse'{messageId = Core.Nothing,
                                         responseStatus}

-- | The unique message identifier returned from the @SendCustomVerificationEmail@ operation.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scverrsMessageId :: Lens.Lens' SendCustomVerificationEmailResponse (Core.Maybe Types.MessageId)
scverrsMessageId = Lens.field @"messageId"
{-# INLINEABLE scverrsMessageId #-}
{-# DEPRECATED messageId "Use generic-lens or generic-optics with 'messageId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scverrsResponseStatus :: Lens.Lens' SendCustomVerificationEmailResponse Core.Int
scverrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE scverrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
