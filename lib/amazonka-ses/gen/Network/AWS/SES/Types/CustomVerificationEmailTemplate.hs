-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CustomVerificationEmailTemplate
  ( CustomVerificationEmailTemplate (..),

    -- * Smart constructor
    mkCustomVerificationEmailTemplate,

    -- * Lenses
    cvetFromEmailAddress,
    cvetTemplateName,
    cvetFailureRedirectionURL,
    cvetTemplateSubject,
    cvetSuccessRedirectionURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a custom verification email template.
--
-- /See:/ 'mkCustomVerificationEmailTemplate' smart constructor.
data CustomVerificationEmailTemplate = CustomVerificationEmailTemplate'
  { fromEmailAddress ::
      Lude.Maybe Lude.Text,
    templateName ::
      Lude.Maybe Lude.Text,
    failureRedirectionURL ::
      Lude.Maybe Lude.Text,
    templateSubject ::
      Lude.Maybe Lude.Text,
    successRedirectionURL ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- * 'failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
-- * 'fromEmailAddress' - The email address that the custom verification email is sent from.
-- * 'successRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
-- * 'templateName' - The name of the custom verification email template.
-- * 'templateSubject' - The subject line of the custom verification email.
mkCustomVerificationEmailTemplate ::
  CustomVerificationEmailTemplate
mkCustomVerificationEmailTemplate =
  CustomVerificationEmailTemplate'
    { fromEmailAddress = Lude.Nothing,
      templateName = Lude.Nothing,
      failureRedirectionURL = Lude.Nothing,
      templateSubject = Lude.Nothing,
      successRedirectionURL = Lude.Nothing
    }

-- | The email address that the custom verification email is sent from.
--
-- /Note:/ Consider using 'fromEmailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvetFromEmailAddress :: Lens.Lens' CustomVerificationEmailTemplate (Lude.Maybe Lude.Text)
cvetFromEmailAddress = Lens.lens (fromEmailAddress :: CustomVerificationEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {fromEmailAddress = a} :: CustomVerificationEmailTemplate)
{-# DEPRECATED cvetFromEmailAddress "Use generic-lens or generic-optics with 'fromEmailAddress' instead." #-}

-- | The name of the custom verification email template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvetTemplateName :: Lens.Lens' CustomVerificationEmailTemplate (Lude.Maybe Lude.Text)
cvetTemplateName = Lens.lens (templateName :: CustomVerificationEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: CustomVerificationEmailTemplate)
{-# DEPRECATED cvetTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- /Note:/ Consider using 'failureRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvetFailureRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplate (Lude.Maybe Lude.Text)
cvetFailureRedirectionURL = Lens.lens (failureRedirectionURL :: CustomVerificationEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {failureRedirectionURL = a} :: CustomVerificationEmailTemplate)
{-# DEPRECATED cvetFailureRedirectionURL "Use generic-lens or generic-optics with 'failureRedirectionURL' instead." #-}

-- | The subject line of the custom verification email.
--
-- /Note:/ Consider using 'templateSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvetTemplateSubject :: Lens.Lens' CustomVerificationEmailTemplate (Lude.Maybe Lude.Text)
cvetTemplateSubject = Lens.lens (templateSubject :: CustomVerificationEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {templateSubject = a} :: CustomVerificationEmailTemplate)
{-# DEPRECATED cvetTemplateSubject "Use generic-lens or generic-optics with 'templateSubject' instead." #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- /Note:/ Consider using 'successRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvetSuccessRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplate (Lude.Maybe Lude.Text)
cvetSuccessRedirectionURL = Lens.lens (successRedirectionURL :: CustomVerificationEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {successRedirectionURL = a} :: CustomVerificationEmailTemplate)
{-# DEPRECATED cvetSuccessRedirectionURL "Use generic-lens or generic-optics with 'successRedirectionURL' instead." #-}

instance Lude.FromXML CustomVerificationEmailTemplate where
  parseXML x =
    CustomVerificationEmailTemplate'
      Lude.<$> (x Lude..@? "FromEmailAddress")
      Lude.<*> (x Lude..@? "TemplateName")
      Lude.<*> (x Lude..@? "FailureRedirectionURL")
      Lude.<*> (x Lude..@? "TemplateSubject")
      Lude.<*> (x Lude..@? "SuccessRedirectionURL")
