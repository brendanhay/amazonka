{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the custom email verification template for the template name you specify.
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetCustomVerificationEmailTemplate
    (
    -- * Creating a request
      GetCustomVerificationEmailTemplate (..)
    , mkGetCustomVerificationEmailTemplate
    -- ** Request lenses
    , gcvetTemplateName

    -- * Destructuring the response
    , GetCustomVerificationEmailTemplateResponse (..)
    , mkGetCustomVerificationEmailTemplateResponse
    -- ** Response lenses
    , gcvetrrsFailureRedirectionURL
    , gcvetrrsFromEmailAddress
    , gcvetrrsSuccessRedirectionURL
    , gcvetrrsTemplateContent
    , gcvetrrsTemplateName
    , gcvetrrsTemplateSubject
    , gcvetrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to retrieve an existing custom verification email template.
--
-- /See:/ 'mkGetCustomVerificationEmailTemplate' smart constructor.
newtype GetCustomVerificationEmailTemplate = GetCustomVerificationEmailTemplate'
  { templateName :: Types.TemplateName
    -- ^ The name of the custom verification email template that you want to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCustomVerificationEmailTemplate' value with any optional fields omitted.
mkGetCustomVerificationEmailTemplate
    :: Types.TemplateName -- ^ 'templateName'
    -> GetCustomVerificationEmailTemplate
mkGetCustomVerificationEmailTemplate templateName
  = GetCustomVerificationEmailTemplate'{templateName}

-- | The name of the custom verification email template that you want to retrieve.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetTemplateName :: Lens.Lens' GetCustomVerificationEmailTemplate Types.TemplateName
gcvetTemplateName = Lens.field @"templateName"
{-# INLINEABLE gcvetTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

instance Core.ToQuery GetCustomVerificationEmailTemplate where
        toQuery GetCustomVerificationEmailTemplate{..}
          = Core.toQueryPair "Action"
              ("GetCustomVerificationEmailTemplate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "TemplateName" templateName

instance Core.ToHeaders GetCustomVerificationEmailTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetCustomVerificationEmailTemplate where
        type Rs GetCustomVerificationEmailTemplate =
             GetCustomVerificationEmailTemplateResponse
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
          = Response.receiveXMLWrapper
              "GetCustomVerificationEmailTemplateResult"
              (\ s h x ->
                 GetCustomVerificationEmailTemplateResponse' Core.<$>
                   (x Core..@? "FailureRedirectionURL") Core.<*>
                     x Core..@? "FromEmailAddress"
                     Core.<*> x Core..@? "SuccessRedirectionURL"
                     Core.<*> x Core..@? "TemplateContent"
                     Core.<*> x Core..@? "TemplateName"
                     Core.<*> x Core..@? "TemplateSubject"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The content of the custom verification email template.
--
-- /See:/ 'mkGetCustomVerificationEmailTemplateResponse' smart constructor.
data GetCustomVerificationEmailTemplateResponse = GetCustomVerificationEmailTemplateResponse'
  { failureRedirectionURL :: Core.Maybe Types.FailureRedirectionURL
    -- ^ The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
  , fromEmailAddress :: Core.Maybe Types.FromAddress
    -- ^ The email address that the custom verification email is sent from.
  , successRedirectionURL :: Core.Maybe Types.SuccessRedirectionURL
    -- ^ The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
  , templateContent :: Core.Maybe Types.TemplateContent
    -- ^ The content of the custom verification email.
  , templateName :: Core.Maybe Types.TemplateName
    -- ^ The name of the custom verification email template.
  , templateSubject :: Core.Maybe Types.Subject
    -- ^ The subject line of the custom verification email.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCustomVerificationEmailTemplateResponse' value with any optional fields omitted.
mkGetCustomVerificationEmailTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCustomVerificationEmailTemplateResponse
mkGetCustomVerificationEmailTemplateResponse responseStatus
  = GetCustomVerificationEmailTemplateResponse'{failureRedirectionURL
                                                  = Core.Nothing,
                                                fromEmailAddress = Core.Nothing,
                                                successRedirectionURL = Core.Nothing,
                                                templateContent = Core.Nothing,
                                                templateName = Core.Nothing,
                                                templateSubject = Core.Nothing, responseStatus}

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- /Note:/ Consider using 'failureRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrrsFailureRedirectionURL :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Types.FailureRedirectionURL)
gcvetrrsFailureRedirectionURL = Lens.field @"failureRedirectionURL"
{-# INLINEABLE gcvetrrsFailureRedirectionURL #-}
{-# DEPRECATED failureRedirectionURL "Use generic-lens or generic-optics with 'failureRedirectionURL' instead"  #-}

-- | The email address that the custom verification email is sent from.
--
-- /Note:/ Consider using 'fromEmailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrrsFromEmailAddress :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Types.FromAddress)
gcvetrrsFromEmailAddress = Lens.field @"fromEmailAddress"
{-# INLINEABLE gcvetrrsFromEmailAddress #-}
{-# DEPRECATED fromEmailAddress "Use generic-lens or generic-optics with 'fromEmailAddress' instead"  #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- /Note:/ Consider using 'successRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrrsSuccessRedirectionURL :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Types.SuccessRedirectionURL)
gcvetrrsSuccessRedirectionURL = Lens.field @"successRedirectionURL"
{-# INLINEABLE gcvetrrsSuccessRedirectionURL #-}
{-# DEPRECATED successRedirectionURL "Use generic-lens or generic-optics with 'successRedirectionURL' instead"  #-}

-- | The content of the custom verification email.
--
-- /Note:/ Consider using 'templateContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrrsTemplateContent :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Types.TemplateContent)
gcvetrrsTemplateContent = Lens.field @"templateContent"
{-# INLINEABLE gcvetrrsTemplateContent #-}
{-# DEPRECATED templateContent "Use generic-lens or generic-optics with 'templateContent' instead"  #-}

-- | The name of the custom verification email template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrrsTemplateName :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Types.TemplateName)
gcvetrrsTemplateName = Lens.field @"templateName"
{-# INLINEABLE gcvetrrsTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The subject line of the custom verification email.
--
-- /Note:/ Consider using 'templateSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrrsTemplateSubject :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Types.Subject)
gcvetrrsTemplateSubject = Lens.field @"templateSubject"
{-# INLINEABLE gcvetrrsTemplateSubject #-}
{-# DEPRECATED templateSubject "Use generic-lens or generic-optics with 'templateSubject' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrrsResponseStatus :: Lens.Lens' GetCustomVerificationEmailTemplateResponse Core.Int
gcvetrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcvetrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
