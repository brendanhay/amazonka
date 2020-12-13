{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetCustomVerificationEmailTemplate (..),
    mkGetCustomVerificationEmailTemplate,

    -- ** Request lenses
    gcvetTemplateName,

    -- * Destructuring the response
    GetCustomVerificationEmailTemplateResponse (..),
    mkGetCustomVerificationEmailTemplateResponse,

    -- ** Response lenses
    gcvetrsFromEmailAddress,
    gcvetrsTemplateName,
    gcvetrsFailureRedirectionURL,
    gcvetrsTemplateSubject,
    gcvetrsSuccessRedirectionURL,
    gcvetrsTemplateContent,
    gcvetrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to retrieve an existing custom verification email template.
--
-- /See:/ 'mkGetCustomVerificationEmailTemplate' smart constructor.
newtype GetCustomVerificationEmailTemplate = GetCustomVerificationEmailTemplate'
  { -- | The name of the custom verification email template that you want to retrieve.
    templateName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the custom verification email template that you want to retrieve.
mkGetCustomVerificationEmailTemplate ::
  -- | 'templateName'
  Lude.Text ->
  GetCustomVerificationEmailTemplate
mkGetCustomVerificationEmailTemplate pTemplateName_ =
  GetCustomVerificationEmailTemplate'
    { templateName =
        pTemplateName_
    }

-- | The name of the custom verification email template that you want to retrieve.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetTemplateName :: Lens.Lens' GetCustomVerificationEmailTemplate Lude.Text
gcvetTemplateName = Lens.lens (templateName :: GetCustomVerificationEmailTemplate -> Lude.Text) (\s a -> s {templateName = a} :: GetCustomVerificationEmailTemplate)
{-# DEPRECATED gcvetTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest GetCustomVerificationEmailTemplate where
  type
    Rs GetCustomVerificationEmailTemplate =
      GetCustomVerificationEmailTemplateResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "GetCustomVerificationEmailTemplateResult"
      ( \s h x ->
          GetCustomVerificationEmailTemplateResponse'
            Lude.<$> (x Lude..@? "FromEmailAddress")
            Lude.<*> (x Lude..@? "TemplateName")
            Lude.<*> (x Lude..@? "FailureRedirectionURL")
            Lude.<*> (x Lude..@? "TemplateSubject")
            Lude.<*> (x Lude..@? "SuccessRedirectionURL")
            Lude.<*> (x Lude..@? "TemplateContent")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCustomVerificationEmailTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCustomVerificationEmailTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCustomVerificationEmailTemplate where
  toQuery GetCustomVerificationEmailTemplate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetCustomVerificationEmailTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName
      ]

-- | The content of the custom verification email template.
--
-- /See:/ 'mkGetCustomVerificationEmailTemplateResponse' smart constructor.
data GetCustomVerificationEmailTemplateResponse = GetCustomVerificationEmailTemplateResponse'
  { -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Lude.Maybe Lude.Text,
    -- | The name of the custom verification email template.
    templateName :: Lude.Maybe Lude.Text,
    -- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
    failureRedirectionURL :: Lude.Maybe Lude.Text,
    -- | The subject line of the custom verification email.
    templateSubject :: Lude.Maybe Lude.Text,
    -- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
    successRedirectionURL :: Lude.Maybe Lude.Text,
    -- | The content of the custom verification email.
    templateContent :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCustomVerificationEmailTemplateResponse' with the minimum fields required to make a request.
--
-- * 'fromEmailAddress' - The email address that the custom verification email is sent from.
-- * 'templateName' - The name of the custom verification email template.
-- * 'failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
-- * 'templateSubject' - The subject line of the custom verification email.
-- * 'successRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
-- * 'templateContent' - The content of the custom verification email.
-- * 'responseStatus' - The response status code.
mkGetCustomVerificationEmailTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCustomVerificationEmailTemplateResponse
mkGetCustomVerificationEmailTemplateResponse pResponseStatus_ =
  GetCustomVerificationEmailTemplateResponse'
    { fromEmailAddress =
        Lude.Nothing,
      templateName = Lude.Nothing,
      failureRedirectionURL = Lude.Nothing,
      templateSubject = Lude.Nothing,
      successRedirectionURL = Lude.Nothing,
      templateContent = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The email address that the custom verification email is sent from.
--
-- /Note:/ Consider using 'fromEmailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrsFromEmailAddress :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Lude.Maybe Lude.Text)
gcvetrsFromEmailAddress = Lens.lens (fromEmailAddress :: GetCustomVerificationEmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {fromEmailAddress = a} :: GetCustomVerificationEmailTemplateResponse)
{-# DEPRECATED gcvetrsFromEmailAddress "Use generic-lens or generic-optics with 'fromEmailAddress' instead." #-}

-- | The name of the custom verification email template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrsTemplateName :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Lude.Maybe Lude.Text)
gcvetrsTemplateName = Lens.lens (templateName :: GetCustomVerificationEmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: GetCustomVerificationEmailTemplateResponse)
{-# DEPRECATED gcvetrsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- /Note:/ Consider using 'failureRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrsFailureRedirectionURL :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Lude.Maybe Lude.Text)
gcvetrsFailureRedirectionURL = Lens.lens (failureRedirectionURL :: GetCustomVerificationEmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureRedirectionURL = a} :: GetCustomVerificationEmailTemplateResponse)
{-# DEPRECATED gcvetrsFailureRedirectionURL "Use generic-lens or generic-optics with 'failureRedirectionURL' instead." #-}

-- | The subject line of the custom verification email.
--
-- /Note:/ Consider using 'templateSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrsTemplateSubject :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Lude.Maybe Lude.Text)
gcvetrsTemplateSubject = Lens.lens (templateSubject :: GetCustomVerificationEmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateSubject = a} :: GetCustomVerificationEmailTemplateResponse)
{-# DEPRECATED gcvetrsTemplateSubject "Use generic-lens or generic-optics with 'templateSubject' instead." #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- /Note:/ Consider using 'successRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrsSuccessRedirectionURL :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Lude.Maybe Lude.Text)
gcvetrsSuccessRedirectionURL = Lens.lens (successRedirectionURL :: GetCustomVerificationEmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {successRedirectionURL = a} :: GetCustomVerificationEmailTemplateResponse)
{-# DEPRECATED gcvetrsSuccessRedirectionURL "Use generic-lens or generic-optics with 'successRedirectionURL' instead." #-}

-- | The content of the custom verification email.
--
-- /Note:/ Consider using 'templateContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrsTemplateContent :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Lude.Maybe Lude.Text)
gcvetrsTemplateContent = Lens.lens (templateContent :: GetCustomVerificationEmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateContent = a} :: GetCustomVerificationEmailTemplateResponse)
{-# DEPRECATED gcvetrsTemplateContent "Use generic-lens or generic-optics with 'templateContent' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvetrsResponseStatus :: Lens.Lens' GetCustomVerificationEmailTemplateResponse Lude.Int
gcvetrsResponseStatus = Lens.lens (responseStatus :: GetCustomVerificationEmailTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCustomVerificationEmailTemplateResponse)
{-# DEPRECATED gcvetrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
