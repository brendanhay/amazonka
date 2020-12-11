{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the template object (which includes the Subject line, HTML part and text part) for the template you specify.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetTemplate
  ( -- * Creating a request
    GetTemplate (..),
    mkGetTemplate,

    -- ** Request lenses
    gtTemplateName,

    -- * Destructuring the response
    GetTemplateResponse (..),
    mkGetTemplateResponse,

    -- ** Response lenses
    gtrsTemplate,
    gtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | /See:/ 'mkGetTemplate' smart constructor.
newtype GetTemplate = GetTemplate' {templateName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the template you want to retrieve.
mkGetTemplate ::
  -- | 'templateName'
  Lude.Text ->
  GetTemplate
mkGetTemplate pTemplateName_ =
  GetTemplate' {templateName = pTemplateName_}

-- | The name of the template you want to retrieve.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTemplateName :: Lens.Lens' GetTemplate Lude.Text
gtTemplateName = Lens.lens (templateName :: GetTemplate -> Lude.Text) (\s a -> s {templateName = a} :: GetTemplate)
{-# DEPRECATED gtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest GetTemplate where
  type Rs GetTemplate = GetTemplateResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "GetTemplateResult"
      ( \s h x ->
          GetTemplateResponse'
            Lude.<$> (x Lude..@? "Template") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTemplate where
  toQuery GetTemplate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName
      ]

-- | /See:/ 'mkGetTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { template ::
      Lude.Maybe Template,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTemplateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'template' - Undocumented field.
mkGetTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTemplateResponse
mkGetTemplateResponse pResponseStatus_ =
  GetTemplateResponse'
    { template = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsTemplate :: Lens.Lens' GetTemplateResponse (Lude.Maybe Template)
gtrsTemplate = Lens.lens (template :: GetTemplateResponse -> Lude.Maybe Template) (\s a -> s {template = a} :: GetTemplateResponse)
{-# DEPRECATED gtrsTemplate "Use generic-lens or generic-optics with 'template' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsResponseStatus :: Lens.Lens' GetTemplateResponse Lude.Int
gtrsResponseStatus = Lens.lens (responseStatus :: GetTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTemplateResponse)
{-# DEPRECATED gtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
