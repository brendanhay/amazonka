{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateCustomVerificationEmailTemplate (..),
    mkUpdateCustomVerificationEmailTemplate,

    -- ** Request lenses
    ucvetFromEmailAddress,
    ucvetTemplateName,
    ucvetFailureRedirectionURL,
    ucvetTemplateSubject,
    ucvetSuccessRedirectionURL,
    ucvetTemplateContent,

    -- * Destructuring the response
    UpdateCustomVerificationEmailTemplateResponse (..),
    mkUpdateCustomVerificationEmailTemplateResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to update an existing custom verification email template.
--
-- /See:/ 'mkUpdateCustomVerificationEmailTemplate' smart constructor.
data UpdateCustomVerificationEmailTemplate = UpdateCustomVerificationEmailTemplate'
  { -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Lude.Maybe Lude.Text,
    -- | The name of the custom verification email template that you want to update.
    templateName :: Lude.Text,
    -- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
    failureRedirectionURL :: Lude.Maybe Lude.Text,
    -- | The subject line of the custom verification email.
    templateSubject :: Lude.Maybe Lude.Text,
    -- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
    successRedirectionURL :: Lude.Maybe Lude.Text,
    -- | The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
    templateContent :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- * 'fromEmailAddress' - The email address that the custom verification email is sent from.
-- * 'templateName' - The name of the custom verification email template that you want to update.
-- * 'failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
-- * 'templateSubject' - The subject line of the custom verification email.
-- * 'successRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
-- * 'templateContent' - The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
mkUpdateCustomVerificationEmailTemplate ::
  -- | 'templateName'
  Lude.Text ->
  UpdateCustomVerificationEmailTemplate
mkUpdateCustomVerificationEmailTemplate pTemplateName_ =
  UpdateCustomVerificationEmailTemplate'
    { fromEmailAddress =
        Lude.Nothing,
      templateName = pTemplateName_,
      failureRedirectionURL = Lude.Nothing,
      templateSubject = Lude.Nothing,
      successRedirectionURL = Lude.Nothing,
      templateContent = Lude.Nothing
    }

-- | The email address that the custom verification email is sent from.
--
-- /Note:/ Consider using 'fromEmailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetFromEmailAddress :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Lude.Maybe Lude.Text)
ucvetFromEmailAddress = Lens.lens (fromEmailAddress :: UpdateCustomVerificationEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {fromEmailAddress = a} :: UpdateCustomVerificationEmailTemplate)
{-# DEPRECATED ucvetFromEmailAddress "Use generic-lens or generic-optics with 'fromEmailAddress' instead." #-}

-- | The name of the custom verification email template that you want to update.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetTemplateName :: Lens.Lens' UpdateCustomVerificationEmailTemplate Lude.Text
ucvetTemplateName = Lens.lens (templateName :: UpdateCustomVerificationEmailTemplate -> Lude.Text) (\s a -> s {templateName = a} :: UpdateCustomVerificationEmailTemplate)
{-# DEPRECATED ucvetTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- /Note:/ Consider using 'failureRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetFailureRedirectionURL :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Lude.Maybe Lude.Text)
ucvetFailureRedirectionURL = Lens.lens (failureRedirectionURL :: UpdateCustomVerificationEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {failureRedirectionURL = a} :: UpdateCustomVerificationEmailTemplate)
{-# DEPRECATED ucvetFailureRedirectionURL "Use generic-lens or generic-optics with 'failureRedirectionURL' instead." #-}

-- | The subject line of the custom verification email.
--
-- /Note:/ Consider using 'templateSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetTemplateSubject :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Lude.Maybe Lude.Text)
ucvetTemplateSubject = Lens.lens (templateSubject :: UpdateCustomVerificationEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {templateSubject = a} :: UpdateCustomVerificationEmailTemplate)
{-# DEPRECATED ucvetTemplateSubject "Use generic-lens or generic-optics with 'templateSubject' instead." #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- /Note:/ Consider using 'successRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetSuccessRedirectionURL :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Lude.Maybe Lude.Text)
ucvetSuccessRedirectionURL = Lens.lens (successRedirectionURL :: UpdateCustomVerificationEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {successRedirectionURL = a} :: UpdateCustomVerificationEmailTemplate)
{-# DEPRECATED ucvetSuccessRedirectionURL "Use generic-lens or generic-optics with 'successRedirectionURL' instead." #-}

-- | The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
--
-- /Note:/ Consider using 'templateContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucvetTemplateContent :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Lude.Maybe Lude.Text)
ucvetTemplateContent = Lens.lens (templateContent :: UpdateCustomVerificationEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {templateContent = a} :: UpdateCustomVerificationEmailTemplate)
{-# DEPRECATED ucvetTemplateContent "Use generic-lens or generic-optics with 'templateContent' instead." #-}

instance Lude.AWSRequest UpdateCustomVerificationEmailTemplate where
  type
    Rs UpdateCustomVerificationEmailTemplate =
      UpdateCustomVerificationEmailTemplateResponse
  request = Req.postQuery sesService
  response =
    Res.receiveNull UpdateCustomVerificationEmailTemplateResponse'

instance Lude.ToHeaders UpdateCustomVerificationEmailTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateCustomVerificationEmailTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCustomVerificationEmailTemplate where
  toQuery UpdateCustomVerificationEmailTemplate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateCustomVerificationEmailTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "FromEmailAddress" Lude.=: fromEmailAddress,
        "TemplateName" Lude.=: templateName,
        "FailureRedirectionURL" Lude.=: failureRedirectionURL,
        "TemplateSubject" Lude.=: templateSubject,
        "SuccessRedirectionURL" Lude.=: successRedirectionURL,
        "TemplateContent" Lude.=: templateContent
      ]

-- | /See:/ 'mkUpdateCustomVerificationEmailTemplateResponse' smart constructor.
data UpdateCustomVerificationEmailTemplateResponse = UpdateCustomVerificationEmailTemplateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCustomVerificationEmailTemplateResponse' with the minimum fields required to make a request.
mkUpdateCustomVerificationEmailTemplateResponse ::
  UpdateCustomVerificationEmailTemplateResponse
mkUpdateCustomVerificationEmailTemplateResponse =
  UpdateCustomVerificationEmailTemplateResponse'
