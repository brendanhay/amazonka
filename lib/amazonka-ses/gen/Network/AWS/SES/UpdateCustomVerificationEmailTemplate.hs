{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing custom verification email template.
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateCustomVerificationEmailTemplate
    (
    -- * Creating a request
      UpdateCustomVerificationEmailTemplate (..)
    , mkUpdateCustomVerificationEmailTemplate
    -- ** Request lenses
    , ucvetTemplateName
    , ucvetFailureRedirectionURL
    , ucvetFromEmailAddress
    , ucvetSuccessRedirectionURL
    , ucvetTemplateContent
    , ucvetTemplateSubject

    -- * Destructuring the response
    , UpdateCustomVerificationEmailTemplateResponse (..)
    , mkUpdateCustomVerificationEmailTemplateResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to update an existing custom verification email template.
--
-- /See:/ 'mkUpdateCustomVerificationEmailTemplate' smart constructor.
data UpdateCustomVerificationEmailTemplate = UpdateCustomVerificationEmailTemplate'
  { templateName :: Types.TemplateName
    -- ^ The name of the custom verification email template that you want to update.
  , failureRedirectionURL :: Core.Maybe Types.FailureRedirectionURL
    -- ^ The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
  , fromEmailAddress :: Core.Maybe Types.FromAddress
    -- ^ The email address that the custom verification email is sent from.
  , successRedirectionURL :: Core.Maybe Types.SuccessRedirectionURL
    -- ^ The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
  , templateContent :: Core.Maybe Types.TemplateContent
    -- ^ The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
  , templateSubject :: Core.Maybe Types.Subject
    -- ^ The subject line of the custom verification email.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCustomVerificationEmailTemplate' value with any optional fields omitted.
mkUpdateCustomVerificationEmailTemplate
    :: Types.TemplateName -- ^ 'templateName'
    -> UpdateCustomVerificationEmailTemplate
mkUpdateCustomVerificationEmailTemplate templateName
  = UpdateCustomVerificationEmailTemplate'{templateName,
                                           failureRedirectionURL = Core.Nothing,
                                           fromEmailAddress = Core.Nothing,
                                           successRedirectionURL = Core.Nothing,
                                           templateContent = Core.Nothing,
                                           templateSubject = Core.Nothing}

-- | The name of the custom verification email template that you want to update.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetTemplateName :: Lens.Lens' UpdateCustomVerificationEmailTemplate Types.TemplateName
ucvetTemplateName = Lens.field @"templateName"
{-# INLINEABLE ucvetTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- /Note:/ Consider using 'failureRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetFailureRedirectionURL :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Core.Maybe Types.FailureRedirectionURL)
ucvetFailureRedirectionURL = Lens.field @"failureRedirectionURL"
{-# INLINEABLE ucvetFailureRedirectionURL #-}
{-# DEPRECATED failureRedirectionURL "Use generic-lens or generic-optics with 'failureRedirectionURL' instead"  #-}

-- | The email address that the custom verification email is sent from.
--
-- /Note:/ Consider using 'fromEmailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetFromEmailAddress :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Core.Maybe Types.FromAddress)
ucvetFromEmailAddress = Lens.field @"fromEmailAddress"
{-# INLINEABLE ucvetFromEmailAddress #-}
{-# DEPRECATED fromEmailAddress "Use generic-lens or generic-optics with 'fromEmailAddress' instead"  #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- /Note:/ Consider using 'successRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetSuccessRedirectionURL :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Core.Maybe Types.SuccessRedirectionURL)
ucvetSuccessRedirectionURL = Lens.field @"successRedirectionURL"
{-# INLINEABLE ucvetSuccessRedirectionURL #-}
{-# DEPRECATED successRedirectionURL "Use generic-lens or generic-optics with 'successRedirectionURL' instead"  #-}

-- | The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
--
-- /Note:/ Consider using 'templateContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetTemplateContent :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Core.Maybe Types.TemplateContent)
ucvetTemplateContent = Lens.field @"templateContent"
{-# INLINEABLE ucvetTemplateContent #-}
{-# DEPRECATED templateContent "Use generic-lens or generic-optics with 'templateContent' instead"  #-}

-- | The subject line of the custom verification email.
--
-- /Note:/ Consider using 'templateSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetTemplateSubject :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Core.Maybe Types.Subject)
ucvetTemplateSubject = Lens.field @"templateSubject"
{-# INLINEABLE ucvetTemplateSubject #-}
{-# DEPRECATED templateSubject "Use generic-lens or generic-optics with 'templateSubject' instead"  #-}

instance Core.ToQuery UpdateCustomVerificationEmailTemplate where
        toQuery UpdateCustomVerificationEmailTemplate{..}
          = Core.toQueryPair "Action"
              ("UpdateCustomVerificationEmailTemplate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "TemplateName" templateName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FailureRedirectionURL")
                failureRedirectionURL
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FromEmailAddress")
                fromEmailAddress
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SuccessRedirectionURL")
                successRedirectionURL
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateContent")
                templateContent
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateSubject")
                templateSubject

instance Core.ToHeaders UpdateCustomVerificationEmailTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateCustomVerificationEmailTemplate
         where
        type Rs UpdateCustomVerificationEmailTemplate =
             UpdateCustomVerificationEmailTemplateResponse
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
          = Response.receiveNull
              UpdateCustomVerificationEmailTemplateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateCustomVerificationEmailTemplateResponse' smart constructor.
data UpdateCustomVerificationEmailTemplateResponse = UpdateCustomVerificationEmailTemplateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCustomVerificationEmailTemplateResponse' value with any optional fields omitted.
mkUpdateCustomVerificationEmailTemplateResponse
    :: UpdateCustomVerificationEmailTemplateResponse
mkUpdateCustomVerificationEmailTemplateResponse
  = UpdateCustomVerificationEmailTemplateResponse'
