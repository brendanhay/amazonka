{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom verification email template.
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateCustomVerificationEmailTemplate
    (
    -- * Creating a request
      CreateCustomVerificationEmailTemplate (..)
    , mkCreateCustomVerificationEmailTemplate
    -- ** Request lenses
    , ccvetTemplateName
    , ccvetFromEmailAddress
    , ccvetTemplateSubject
    , ccvetTemplateContent
    , ccvetSuccessRedirectionURL
    , ccvetFailureRedirectionURL

    -- * Destructuring the response
    , CreateCustomVerificationEmailTemplateResponse (..)
    , mkCreateCustomVerificationEmailTemplateResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to create a custom verification email template.
--
-- /See:/ 'mkCreateCustomVerificationEmailTemplate' smart constructor.
data CreateCustomVerificationEmailTemplate = CreateCustomVerificationEmailTemplate'
  { templateName :: Types.TemplateName
    -- ^ The name of the custom verification email template.
  , fromEmailAddress :: Types.FromAddress
    -- ^ The email address that the custom verification email is sent from.
  , templateSubject :: Types.Subject
    -- ^ The subject line of the custom verification email.
  , templateContent :: Types.TemplateContent
    -- ^ The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
  , successRedirectionURL :: Types.SuccessRedirectionURL
    -- ^ The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
  , failureRedirectionURL :: Types.FailureRedirectionURL
    -- ^ The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomVerificationEmailTemplate' value with any optional fields omitted.
mkCreateCustomVerificationEmailTemplate
    :: Types.TemplateName -- ^ 'templateName'
    -> Types.FromAddress -- ^ 'fromEmailAddress'
    -> Types.Subject -- ^ 'templateSubject'
    -> Types.TemplateContent -- ^ 'templateContent'
    -> Types.SuccessRedirectionURL -- ^ 'successRedirectionURL'
    -> Types.FailureRedirectionURL -- ^ 'failureRedirectionURL'
    -> CreateCustomVerificationEmailTemplate
mkCreateCustomVerificationEmailTemplate templateName
  fromEmailAddress templateSubject templateContent
  successRedirectionURL failureRedirectionURL
  = CreateCustomVerificationEmailTemplate'{templateName,
                                           fromEmailAddress, templateSubject, templateContent,
                                           successRedirectionURL, failureRedirectionURL}

-- | The name of the custom verification email template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetTemplateName :: Lens.Lens' CreateCustomVerificationEmailTemplate Types.TemplateName
ccvetTemplateName = Lens.field @"templateName"
{-# INLINEABLE ccvetTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The email address that the custom verification email is sent from.
--
-- /Note:/ Consider using 'fromEmailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetFromEmailAddress :: Lens.Lens' CreateCustomVerificationEmailTemplate Types.FromAddress
ccvetFromEmailAddress = Lens.field @"fromEmailAddress"
{-# INLINEABLE ccvetFromEmailAddress #-}
{-# DEPRECATED fromEmailAddress "Use generic-lens or generic-optics with 'fromEmailAddress' instead"  #-}

-- | The subject line of the custom verification email.
--
-- /Note:/ Consider using 'templateSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetTemplateSubject :: Lens.Lens' CreateCustomVerificationEmailTemplate Types.Subject
ccvetTemplateSubject = Lens.field @"templateSubject"
{-# INLINEABLE ccvetTemplateSubject #-}
{-# DEPRECATED templateSubject "Use generic-lens or generic-optics with 'templateSubject' instead"  #-}

-- | The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
--
-- /Note:/ Consider using 'templateContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetTemplateContent :: Lens.Lens' CreateCustomVerificationEmailTemplate Types.TemplateContent
ccvetTemplateContent = Lens.field @"templateContent"
{-# INLINEABLE ccvetTemplateContent #-}
{-# DEPRECATED templateContent "Use generic-lens or generic-optics with 'templateContent' instead"  #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- /Note:/ Consider using 'successRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetSuccessRedirectionURL :: Lens.Lens' CreateCustomVerificationEmailTemplate Types.SuccessRedirectionURL
ccvetSuccessRedirectionURL = Lens.field @"successRedirectionURL"
{-# INLINEABLE ccvetSuccessRedirectionURL #-}
{-# DEPRECATED successRedirectionURL "Use generic-lens or generic-optics with 'successRedirectionURL' instead"  #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- /Note:/ Consider using 'failureRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetFailureRedirectionURL :: Lens.Lens' CreateCustomVerificationEmailTemplate Types.FailureRedirectionURL
ccvetFailureRedirectionURL = Lens.field @"failureRedirectionURL"
{-# INLINEABLE ccvetFailureRedirectionURL #-}
{-# DEPRECATED failureRedirectionURL "Use generic-lens or generic-optics with 'failureRedirectionURL' instead"  #-}

instance Core.ToQuery CreateCustomVerificationEmailTemplate where
        toQuery CreateCustomVerificationEmailTemplate{..}
          = Core.toQueryPair "Action"
              ("CreateCustomVerificationEmailTemplate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "TemplateName" templateName
              Core.<> Core.toQueryPair "FromEmailAddress" fromEmailAddress
              Core.<> Core.toQueryPair "TemplateSubject" templateSubject
              Core.<> Core.toQueryPair "TemplateContent" templateContent
              Core.<>
              Core.toQueryPair "SuccessRedirectionURL" successRedirectionURL
              Core.<>
              Core.toQueryPair "FailureRedirectionURL" failureRedirectionURL

instance Core.ToHeaders CreateCustomVerificationEmailTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateCustomVerificationEmailTemplate
         where
        type Rs CreateCustomVerificationEmailTemplate =
             CreateCustomVerificationEmailTemplateResponse
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
              CreateCustomVerificationEmailTemplateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCustomVerificationEmailTemplateResponse' smart constructor.
data CreateCustomVerificationEmailTemplateResponse = CreateCustomVerificationEmailTemplateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomVerificationEmailTemplateResponse' value with any optional fields omitted.
mkCreateCustomVerificationEmailTemplateResponse
    :: CreateCustomVerificationEmailTemplateResponse
mkCreateCustomVerificationEmailTemplateResponse
  = CreateCustomVerificationEmailTemplateResponse'
