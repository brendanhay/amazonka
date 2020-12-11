{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.TestRenderTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a preview of the MIME content of an email when provided with a template and a set of replacement data.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.TestRenderTemplate
  ( -- * Creating a request
    TestRenderTemplate (..),
    mkTestRenderTemplate,

    -- ** Request lenses
    trtTemplateName,
    trtTemplateData,

    -- * Destructuring the response
    TestRenderTemplateResponse (..),
    mkTestRenderTemplateResponse,

    -- ** Response lenses
    trtrsRenderedTemplate,
    trtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | /See:/ 'mkTestRenderTemplate' smart constructor.
data TestRenderTemplate = TestRenderTemplate'
  { templateName ::
      Lude.Text,
    templateData :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestRenderTemplate' with the minimum fields required to make a request.
--
-- * 'templateData' - A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
-- * 'templateName' - The name of the template that you want to render.
mkTestRenderTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'templateData'
  Lude.Text ->
  TestRenderTemplate
mkTestRenderTemplate pTemplateName_ pTemplateData_ =
  TestRenderTemplate'
    { templateName = pTemplateName_,
      templateData = pTemplateData_
    }

-- | The name of the template that you want to render.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtTemplateName :: Lens.Lens' TestRenderTemplate Lude.Text
trtTemplateName = Lens.lens (templateName :: TestRenderTemplate -> Lude.Text) (\s a -> s {templateName = a} :: TestRenderTemplate)
{-# DEPRECATED trtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- /Note:/ Consider using 'templateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtTemplateData :: Lens.Lens' TestRenderTemplate Lude.Text
trtTemplateData = Lens.lens (templateData :: TestRenderTemplate -> Lude.Text) (\s a -> s {templateData = a} :: TestRenderTemplate)
{-# DEPRECATED trtTemplateData "Use generic-lens or generic-optics with 'templateData' instead." #-}

instance Lude.AWSRequest TestRenderTemplate where
  type Rs TestRenderTemplate = TestRenderTemplateResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "TestRenderTemplateResult"
      ( \s h x ->
          TestRenderTemplateResponse'
            Lude.<$> (x Lude..@? "RenderedTemplate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestRenderTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TestRenderTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery TestRenderTemplate where
  toQuery TestRenderTemplate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("TestRenderTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName,
        "TemplateData" Lude.=: templateData
      ]

-- | /See:/ 'mkTestRenderTemplateResponse' smart constructor.
data TestRenderTemplateResponse = TestRenderTemplateResponse'
  { renderedTemplate ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'TestRenderTemplateResponse' with the minimum fields required to make a request.
--
-- * 'renderedTemplate' - The complete MIME message rendered by applying the data in the TemplateData parameter to the template specified in the TemplateName parameter.
-- * 'responseStatus' - The response status code.
mkTestRenderTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestRenderTemplateResponse
mkTestRenderTemplateResponse pResponseStatus_ =
  TestRenderTemplateResponse'
    { renderedTemplate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The complete MIME message rendered by applying the data in the TemplateData parameter to the template specified in the TemplateName parameter.
--
-- /Note:/ Consider using 'renderedTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrsRenderedTemplate :: Lens.Lens' TestRenderTemplateResponse (Lude.Maybe Lude.Text)
trtrsRenderedTemplate = Lens.lens (renderedTemplate :: TestRenderTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {renderedTemplate = a} :: TestRenderTemplateResponse)
{-# DEPRECATED trtrsRenderedTemplate "Use generic-lens or generic-optics with 'renderedTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrsResponseStatus :: Lens.Lens' TestRenderTemplateResponse Lude.Int
trtrsResponseStatus = Lens.lens (responseStatus :: TestRenderTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestRenderTemplateResponse)
{-# DEPRECATED trtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
