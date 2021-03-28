{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.CustomVerificationEmailTemplate
  ( CustomVerificationEmailTemplate (..)
  -- * Smart constructor
  , mkCustomVerificationEmailTemplate
  -- * Lenses
  , cvetFailureRedirectionURL
  , cvetFromEmailAddress
  , cvetSuccessRedirectionURL
  , cvetTemplateName
  , cvetTemplateSubject
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.FailureRedirectionURL as Types
import qualified Network.AWS.SES.Types.FromAddress as Types
import qualified Network.AWS.SES.Types.Subject as Types
import qualified Network.AWS.SES.Types.SuccessRedirectionURL as Types
import qualified Network.AWS.SES.Types.TemplateName as Types

-- | Contains information about a custom verification email template.
--
-- /See:/ 'mkCustomVerificationEmailTemplate' smart constructor.
data CustomVerificationEmailTemplate = CustomVerificationEmailTemplate'
  { failureRedirectionURL :: Core.Maybe Types.FailureRedirectionURL
    -- ^ The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
  , fromEmailAddress :: Core.Maybe Types.FromAddress
    -- ^ The email address that the custom verification email is sent from.
  , successRedirectionURL :: Core.Maybe Types.SuccessRedirectionURL
    -- ^ The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
  , templateName :: Core.Maybe Types.TemplateName
    -- ^ The name of the custom verification email template.
  , templateSubject :: Core.Maybe Types.Subject
    -- ^ The subject line of the custom verification email.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomVerificationEmailTemplate' value with any optional fields omitted.
mkCustomVerificationEmailTemplate
    :: CustomVerificationEmailTemplate
mkCustomVerificationEmailTemplate
  = CustomVerificationEmailTemplate'{failureRedirectionURL =
                                       Core.Nothing,
                                     fromEmailAddress = Core.Nothing,
                                     successRedirectionURL = Core.Nothing,
                                     templateName = Core.Nothing, templateSubject = Core.Nothing}

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- /Note:/ Consider using 'failureRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvetFailureRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplate (Core.Maybe Types.FailureRedirectionURL)
cvetFailureRedirectionURL = Lens.field @"failureRedirectionURL"
{-# INLINEABLE cvetFailureRedirectionURL #-}
{-# DEPRECATED failureRedirectionURL "Use generic-lens or generic-optics with 'failureRedirectionURL' instead"  #-}

-- | The email address that the custom verification email is sent from.
--
-- /Note:/ Consider using 'fromEmailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvetFromEmailAddress :: Lens.Lens' CustomVerificationEmailTemplate (Core.Maybe Types.FromAddress)
cvetFromEmailAddress = Lens.field @"fromEmailAddress"
{-# INLINEABLE cvetFromEmailAddress #-}
{-# DEPRECATED fromEmailAddress "Use generic-lens or generic-optics with 'fromEmailAddress' instead"  #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- /Note:/ Consider using 'successRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvetSuccessRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplate (Core.Maybe Types.SuccessRedirectionURL)
cvetSuccessRedirectionURL = Lens.field @"successRedirectionURL"
{-# INLINEABLE cvetSuccessRedirectionURL #-}
{-# DEPRECATED successRedirectionURL "Use generic-lens or generic-optics with 'successRedirectionURL' instead"  #-}

-- | The name of the custom verification email template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvetTemplateName :: Lens.Lens' CustomVerificationEmailTemplate (Core.Maybe Types.TemplateName)
cvetTemplateName = Lens.field @"templateName"
{-# INLINEABLE cvetTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The subject line of the custom verification email.
--
-- /Note:/ Consider using 'templateSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvetTemplateSubject :: Lens.Lens' CustomVerificationEmailTemplate (Core.Maybe Types.Subject)
cvetTemplateSubject = Lens.field @"templateSubject"
{-# INLINEABLE cvetTemplateSubject #-}
{-# DEPRECATED templateSubject "Use generic-lens or generic-optics with 'templateSubject' instead"  #-}

instance Core.FromXML CustomVerificationEmailTemplate where
        parseXML x
          = CustomVerificationEmailTemplate' Core.<$>
              (x Core..@? "FailureRedirectionURL") Core.<*>
                x Core..@? "FromEmailAddress"
                Core.<*> x Core..@? "SuccessRedirectionURL"
                Core.<*> x Core..@? "TemplateName"
                Core.<*> x Core..@? "TemplateSubject"
