{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing custom verification email template.
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteCustomVerificationEmailTemplate
  ( -- * Creating a request
    DeleteCustomVerificationEmailTemplate (..),
    mkDeleteCustomVerificationEmailTemplate,

    -- ** Request lenses
    dcvetTemplateName,

    -- * Destructuring the response
    DeleteCustomVerificationEmailTemplateResponse (..),
    mkDeleteCustomVerificationEmailTemplateResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete an existing custom verification email template.
--
-- /See:/ 'mkDeleteCustomVerificationEmailTemplate' smart constructor.
newtype DeleteCustomVerificationEmailTemplate = DeleteCustomVerificationEmailTemplate'
  { templateName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the custom verification email template that you want to delete.
mkDeleteCustomVerificationEmailTemplate ::
  -- | 'templateName'
  Lude.Text ->
  DeleteCustomVerificationEmailTemplate
mkDeleteCustomVerificationEmailTemplate pTemplateName_ =
  DeleteCustomVerificationEmailTemplate'
    { templateName =
        pTemplateName_
    }

-- | The name of the custom verification email template that you want to delete.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvetTemplateName :: Lens.Lens' DeleteCustomVerificationEmailTemplate Lude.Text
dcvetTemplateName = Lens.lens (templateName :: DeleteCustomVerificationEmailTemplate -> Lude.Text) (\s a -> s {templateName = a} :: DeleteCustomVerificationEmailTemplate)
{-# DEPRECATED dcvetTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest DeleteCustomVerificationEmailTemplate where
  type
    Rs DeleteCustomVerificationEmailTemplate =
      DeleteCustomVerificationEmailTemplateResponse
  request = Req.postQuery sesService
  response =
    Res.receiveNull DeleteCustomVerificationEmailTemplateResponse'

instance Lude.ToHeaders DeleteCustomVerificationEmailTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCustomVerificationEmailTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCustomVerificationEmailTemplate where
  toQuery DeleteCustomVerificationEmailTemplate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteCustomVerificationEmailTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName
      ]

-- | /See:/ 'mkDeleteCustomVerificationEmailTemplateResponse' smart constructor.
data DeleteCustomVerificationEmailTemplateResponse = DeleteCustomVerificationEmailTemplateResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DeleteCustomVerificationEmailTemplateResponse' with the minimum fields required to make a request.
mkDeleteCustomVerificationEmailTemplateResponse ::
  DeleteCustomVerificationEmailTemplateResponse
mkDeleteCustomVerificationEmailTemplateResponse =
  DeleteCustomVerificationEmailTemplateResponse'
