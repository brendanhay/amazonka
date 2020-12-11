{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an email template.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteTemplate
  ( -- * Creating a request
    DeleteTemplate (..),
    mkDeleteTemplate,

    -- ** Request lenses
    dtTemplateName,

    -- * Destructuring the response
    DeleteTemplateResponse (..),
    mkDeleteTemplateResponse,

    -- ** Response lenses
    dtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete an email template. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteTemplate' smart constructor.
newtype DeleteTemplate = DeleteTemplate' {templateName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the template to be deleted.
mkDeleteTemplate ::
  -- | 'templateName'
  Lude.Text ->
  DeleteTemplate
mkDeleteTemplate pTemplateName_ =
  DeleteTemplate' {templateName = pTemplateName_}

-- | The name of the template to be deleted.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTemplateName :: Lens.Lens' DeleteTemplate Lude.Text
dtTemplateName = Lens.lens (templateName :: DeleteTemplate -> Lude.Text) (\s a -> s {templateName = a} :: DeleteTemplate)
{-# DEPRECATED dtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest DeleteTemplate where
  type Rs DeleteTemplate = DeleteTemplateResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DeleteTemplateResult"
      ( \s h x ->
          DeleteTemplateResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTemplate where
  toQuery DeleteTemplate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName
      ]

-- | /See:/ 'mkDeleteTemplateResponse' smart constructor.
newtype DeleteTemplateResponse = DeleteTemplateResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTemplateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTemplateResponse
mkDeleteTemplateResponse pResponseStatus_ =
  DeleteTemplateResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DeleteTemplateResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DeleteTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTemplateResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
