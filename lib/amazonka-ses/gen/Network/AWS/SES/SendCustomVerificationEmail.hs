{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SendCustomVerificationEmail (..),
    mkSendCustomVerificationEmail,

    -- ** Request lenses
    scveConfigurationSetName,
    scveEmailAddress,
    scveTemplateName,

    -- * Destructuring the response
    SendCustomVerificationEmailResponse (..),
    mkSendCustomVerificationEmailResponse,

    -- ** Response lenses
    scversMessageId,
    scversResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to send a custom verification email to a specified recipient.
--
-- /See:/ 'mkSendCustomVerificationEmail' smart constructor.
data SendCustomVerificationEmail = SendCustomVerificationEmail'
  { configurationSetName ::
      Lude.Maybe Lude.Text,
    emailAddress :: Lude.Text,
    templateName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendCustomVerificationEmail' with the minimum fields required to make a request.
--
-- * 'configurationSetName' - Name of a configuration set to use when sending the verification email.
-- * 'emailAddress' - The email address to verify.
-- * 'templateName' - The name of the custom verification email template to use when sending the verification email.
mkSendCustomVerificationEmail ::
  -- | 'emailAddress'
  Lude.Text ->
  -- | 'templateName'
  Lude.Text ->
  SendCustomVerificationEmail
mkSendCustomVerificationEmail pEmailAddress_ pTemplateName_ =
  SendCustomVerificationEmail'
    { configurationSetName = Lude.Nothing,
      emailAddress = pEmailAddress_,
      templateName = pTemplateName_
    }

-- | Name of a configuration set to use when sending the verification email.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scveConfigurationSetName :: Lens.Lens' SendCustomVerificationEmail (Lude.Maybe Lude.Text)
scveConfigurationSetName = Lens.lens (configurationSetName :: SendCustomVerificationEmail -> Lude.Maybe Lude.Text) (\s a -> s {configurationSetName = a} :: SendCustomVerificationEmail)
{-# DEPRECATED scveConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | The email address to verify.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scveEmailAddress :: Lens.Lens' SendCustomVerificationEmail Lude.Text
scveEmailAddress = Lens.lens (emailAddress :: SendCustomVerificationEmail -> Lude.Text) (\s a -> s {emailAddress = a} :: SendCustomVerificationEmail)
{-# DEPRECATED scveEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The name of the custom verification email template to use when sending the verification email.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scveTemplateName :: Lens.Lens' SendCustomVerificationEmail Lude.Text
scveTemplateName = Lens.lens (templateName :: SendCustomVerificationEmail -> Lude.Text) (\s a -> s {templateName = a} :: SendCustomVerificationEmail)
{-# DEPRECATED scveTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest SendCustomVerificationEmail where
  type
    Rs SendCustomVerificationEmail =
      SendCustomVerificationEmailResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SendCustomVerificationEmailResult"
      ( \s h x ->
          SendCustomVerificationEmailResponse'
            Lude.<$> (x Lude..@? "MessageId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendCustomVerificationEmail where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SendCustomVerificationEmail where
  toPath = Lude.const "/"

instance Lude.ToQuery SendCustomVerificationEmail where
  toQuery SendCustomVerificationEmail' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("SendCustomVerificationEmail" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ConfigurationSetName" Lude.=: configurationSetName,
        "EmailAddress" Lude.=: emailAddress,
        "TemplateName" Lude.=: templateName
      ]

-- | The response received when attempting to send the custom verification email.
--
-- /See:/ 'mkSendCustomVerificationEmailResponse' smart constructor.
data SendCustomVerificationEmailResponse = SendCustomVerificationEmailResponse'
  { messageId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendCustomVerificationEmailResponse' with the minimum fields required to make a request.
--
-- * 'messageId' - The unique message identifier returned from the @SendCustomVerificationEmail@ operation.
-- * 'responseStatus' - The response status code.
mkSendCustomVerificationEmailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendCustomVerificationEmailResponse
mkSendCustomVerificationEmailResponse pResponseStatus_ =
  SendCustomVerificationEmailResponse'
    { messageId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique message identifier returned from the @SendCustomVerificationEmail@ operation.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scversMessageId :: Lens.Lens' SendCustomVerificationEmailResponse (Lude.Maybe Lude.Text)
scversMessageId = Lens.lens (messageId :: SendCustomVerificationEmailResponse -> Lude.Maybe Lude.Text) (\s a -> s {messageId = a} :: SendCustomVerificationEmailResponse)
{-# DEPRECATED scversMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scversResponseStatus :: Lens.Lens' SendCustomVerificationEmailResponse Lude.Int
scversResponseStatus = Lens.lens (responseStatus :: SendCustomVerificationEmailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendCustomVerificationEmailResponse)
{-# DEPRECATED scversResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
