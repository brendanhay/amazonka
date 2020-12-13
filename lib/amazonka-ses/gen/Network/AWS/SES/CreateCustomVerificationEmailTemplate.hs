{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateCustomVerificationEmailTemplate (..),
    mkCreateCustomVerificationEmailTemplate,

    -- ** Request lenses
    ccvetFromEmailAddress,
    ccvetTemplateName,
    ccvetFailureRedirectionURL,
    ccvetTemplateSubject,
    ccvetSuccessRedirectionURL,
    ccvetTemplateContent,

    -- * Destructuring the response
    CreateCustomVerificationEmailTemplateResponse (..),
    mkCreateCustomVerificationEmailTemplateResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to create a custom verification email template.
--
-- /See:/ 'mkCreateCustomVerificationEmailTemplate' smart constructor.
data CreateCustomVerificationEmailTemplate = CreateCustomVerificationEmailTemplate'
  { -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Lude.Text,
    -- | The name of the custom verification email template.
    templateName :: Lude.Text,
    -- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
    failureRedirectionURL :: Lude.Text,
    -- | The subject line of the custom verification email.
    templateSubject :: Lude.Text,
    -- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
    successRedirectionURL :: Lude.Text,
    -- | The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
    templateContent :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- * 'fromEmailAddress' - The email address that the custom verification email is sent from.
-- * 'templateName' - The name of the custom verification email template.
-- * 'failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
-- * 'templateSubject' - The subject line of the custom verification email.
-- * 'successRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
-- * 'templateContent' - The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
mkCreateCustomVerificationEmailTemplate ::
  -- | 'fromEmailAddress'
  Lude.Text ->
  -- | 'templateName'
  Lude.Text ->
  -- | 'failureRedirectionURL'
  Lude.Text ->
  -- | 'templateSubject'
  Lude.Text ->
  -- | 'successRedirectionURL'
  Lude.Text ->
  -- | 'templateContent'
  Lude.Text ->
  CreateCustomVerificationEmailTemplate
mkCreateCustomVerificationEmailTemplate
  pFromEmailAddress_
  pTemplateName_
  pFailureRedirectionURL_
  pTemplateSubject_
  pSuccessRedirectionURL_
  pTemplateContent_ =
    CreateCustomVerificationEmailTemplate'
      { fromEmailAddress =
          pFromEmailAddress_,
        templateName = pTemplateName_,
        failureRedirectionURL = pFailureRedirectionURL_,
        templateSubject = pTemplateSubject_,
        successRedirectionURL = pSuccessRedirectionURL_,
        templateContent = pTemplateContent_
      }

-- | The email address that the custom verification email is sent from.
--
-- /Note:/ Consider using 'fromEmailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetFromEmailAddress :: Lens.Lens' CreateCustomVerificationEmailTemplate Lude.Text
ccvetFromEmailAddress = Lens.lens (fromEmailAddress :: CreateCustomVerificationEmailTemplate -> Lude.Text) (\s a -> s {fromEmailAddress = a} :: CreateCustomVerificationEmailTemplate)
{-# DEPRECATED ccvetFromEmailAddress "Use generic-lens or generic-optics with 'fromEmailAddress' instead." #-}

-- | The name of the custom verification email template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetTemplateName :: Lens.Lens' CreateCustomVerificationEmailTemplate Lude.Text
ccvetTemplateName = Lens.lens (templateName :: CreateCustomVerificationEmailTemplate -> Lude.Text) (\s a -> s {templateName = a} :: CreateCustomVerificationEmailTemplate)
{-# DEPRECATED ccvetTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- /Note:/ Consider using 'failureRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetFailureRedirectionURL :: Lens.Lens' CreateCustomVerificationEmailTemplate Lude.Text
ccvetFailureRedirectionURL = Lens.lens (failureRedirectionURL :: CreateCustomVerificationEmailTemplate -> Lude.Text) (\s a -> s {failureRedirectionURL = a} :: CreateCustomVerificationEmailTemplate)
{-# DEPRECATED ccvetFailureRedirectionURL "Use generic-lens or generic-optics with 'failureRedirectionURL' instead." #-}

-- | The subject line of the custom verification email.
--
-- /Note:/ Consider using 'templateSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetTemplateSubject :: Lens.Lens' CreateCustomVerificationEmailTemplate Lude.Text
ccvetTemplateSubject = Lens.lens (templateSubject :: CreateCustomVerificationEmailTemplate -> Lude.Text) (\s a -> s {templateSubject = a} :: CreateCustomVerificationEmailTemplate)
{-# DEPRECATED ccvetTemplateSubject "Use generic-lens or generic-optics with 'templateSubject' instead." #-}

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- /Note:/ Consider using 'successRedirectionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetSuccessRedirectionURL :: Lens.Lens' CreateCustomVerificationEmailTemplate Lude.Text
ccvetSuccessRedirectionURL = Lens.lens (successRedirectionURL :: CreateCustomVerificationEmailTemplate -> Lude.Text) (\s a -> s {successRedirectionURL = a} :: CreateCustomVerificationEmailTemplate)
{-# DEPRECATED ccvetSuccessRedirectionURL "Use generic-lens or generic-optics with 'successRedirectionURL' instead." #-}

-- | The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
--
-- /Note:/ Consider using 'templateContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvetTemplateContent :: Lens.Lens' CreateCustomVerificationEmailTemplate Lude.Text
ccvetTemplateContent = Lens.lens (templateContent :: CreateCustomVerificationEmailTemplate -> Lude.Text) (\s a -> s {templateContent = a} :: CreateCustomVerificationEmailTemplate)
{-# DEPRECATED ccvetTemplateContent "Use generic-lens or generic-optics with 'templateContent' instead." #-}

instance Lude.AWSRequest CreateCustomVerificationEmailTemplate where
  type
    Rs CreateCustomVerificationEmailTemplate =
      CreateCustomVerificationEmailTemplateResponse
  request = Req.postQuery sesService
  response =
    Res.receiveNull CreateCustomVerificationEmailTemplateResponse'

instance Lude.ToHeaders CreateCustomVerificationEmailTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCustomVerificationEmailTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCustomVerificationEmailTemplate where
  toQuery CreateCustomVerificationEmailTemplate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateCustomVerificationEmailTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "FromEmailAddress" Lude.=: fromEmailAddress,
        "TemplateName" Lude.=: templateName,
        "FailureRedirectionURL" Lude.=: failureRedirectionURL,
        "TemplateSubject" Lude.=: templateSubject,
        "SuccessRedirectionURL" Lude.=: successRedirectionURL,
        "TemplateContent" Lude.=: templateContent
      ]

-- | /See:/ 'mkCreateCustomVerificationEmailTemplateResponse' smart constructor.
data CreateCustomVerificationEmailTemplateResponse = CreateCustomVerificationEmailTemplateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomVerificationEmailTemplateResponse' with the minimum fields required to make a request.
mkCreateCustomVerificationEmailTemplateResponse ::
  CreateCustomVerificationEmailTemplateResponse
mkCreateCustomVerificationEmailTemplateResponse =
  CreateCustomVerificationEmailTemplateResponse'
