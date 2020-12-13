{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.RenderUiTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renders the UI template so that you can preview the worker's experience.
module Network.AWS.SageMaker.RenderUiTemplate
  ( -- * Creating a request
    RenderUiTemplate (..),
    mkRenderUiTemplate,

    -- ** Request lenses
    rutUiTemplate,
    rutTask,
    rutHumanTaskUiARN,
    rutRoleARN,

    -- * Destructuring the response
    RenderUiTemplateResponse (..),
    mkRenderUiTemplateResponse,

    -- ** Response lenses
    rutrsRenderedContent,
    rutrsErrors,
    rutrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkRenderUiTemplate' smart constructor.
data RenderUiTemplate = RenderUiTemplate'
  { -- | A @Template@ object containing the worker UI template to render.
    uiTemplate :: Lude.Maybe UiTemplate,
    -- | A @RenderableTask@ object containing a representative task to render.
    task :: RenderableTask,
    -- | The @HumanTaskUiArn@ of the worker UI that you want to render. Do not provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter.
    --
    -- See a list of available Human Ui Amazon Resource Names (ARNs) in 'UiConfig' .
    humanTaskUiARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) that has access to the S3 objects that are used by the template.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenderUiTemplate' with the minimum fields required to make a request.
--
-- * 'uiTemplate' - A @Template@ object containing the worker UI template to render.
-- * 'task' - A @RenderableTask@ object containing a representative task to render.
-- * 'humanTaskUiARN' - The @HumanTaskUiArn@ of the worker UI that you want to render. Do not provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter.
--
-- See a list of available Human Ui Amazon Resource Names (ARNs) in 'UiConfig' .
-- * 'roleARN' - The Amazon Resource Name (ARN) that has access to the S3 objects that are used by the template.
mkRenderUiTemplate ::
  -- | 'task'
  RenderableTask ->
  -- | 'roleARN'
  Lude.Text ->
  RenderUiTemplate
mkRenderUiTemplate pTask_ pRoleARN_ =
  RenderUiTemplate'
    { uiTemplate = Lude.Nothing,
      task = pTask_,
      humanTaskUiARN = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | A @Template@ object containing the worker UI template to render.
--
-- /Note:/ Consider using 'uiTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutUiTemplate :: Lens.Lens' RenderUiTemplate (Lude.Maybe UiTemplate)
rutUiTemplate = Lens.lens (uiTemplate :: RenderUiTemplate -> Lude.Maybe UiTemplate) (\s a -> s {uiTemplate = a} :: RenderUiTemplate)
{-# DEPRECATED rutUiTemplate "Use generic-lens or generic-optics with 'uiTemplate' instead." #-}

-- | A @RenderableTask@ object containing a representative task to render.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutTask :: Lens.Lens' RenderUiTemplate RenderableTask
rutTask = Lens.lens (task :: RenderUiTemplate -> RenderableTask) (\s a -> s {task = a} :: RenderUiTemplate)
{-# DEPRECATED rutTask "Use generic-lens or generic-optics with 'task' instead." #-}

-- | The @HumanTaskUiArn@ of the worker UI that you want to render. Do not provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter.
--
-- See a list of available Human Ui Amazon Resource Names (ARNs) in 'UiConfig' .
--
-- /Note:/ Consider using 'humanTaskUiARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutHumanTaskUiARN :: Lens.Lens' RenderUiTemplate (Lude.Maybe Lude.Text)
rutHumanTaskUiARN = Lens.lens (humanTaskUiARN :: RenderUiTemplate -> Lude.Maybe Lude.Text) (\s a -> s {humanTaskUiARN = a} :: RenderUiTemplate)
{-# DEPRECATED rutHumanTaskUiARN "Use generic-lens or generic-optics with 'humanTaskUiARN' instead." #-}

-- | The Amazon Resource Name (ARN) that has access to the S3 objects that are used by the template.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutRoleARN :: Lens.Lens' RenderUiTemplate Lude.Text
rutRoleARN = Lens.lens (roleARN :: RenderUiTemplate -> Lude.Text) (\s a -> s {roleARN = a} :: RenderUiTemplate)
{-# DEPRECATED rutRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest RenderUiTemplate where
  type Rs RenderUiTemplate = RenderUiTemplateResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          RenderUiTemplateResponse'
            Lude.<$> (x Lude..:> "RenderedContent")
            Lude.<*> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RenderUiTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.RenderUiTemplate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RenderUiTemplate where
  toJSON RenderUiTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UiTemplate" Lude..=) Lude.<$> uiTemplate,
            Lude.Just ("Task" Lude..= task),
            ("HumanTaskUiArn" Lude..=) Lude.<$> humanTaskUiARN,
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath RenderUiTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery RenderUiTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRenderUiTemplateResponse' smart constructor.
data RenderUiTemplateResponse = RenderUiTemplateResponse'
  { -- | A Liquid template that renders the HTML for the worker UI.
    renderedContent :: Lude.Text,
    -- | A list of one or more @RenderingError@ objects if any were encountered while rendering the template. If there were no errors, the list is empty.
    errors :: [RenderingError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenderUiTemplateResponse' with the minimum fields required to make a request.
--
-- * 'renderedContent' - A Liquid template that renders the HTML for the worker UI.
-- * 'errors' - A list of one or more @RenderingError@ objects if any were encountered while rendering the template. If there were no errors, the list is empty.
-- * 'responseStatus' - The response status code.
mkRenderUiTemplateResponse ::
  -- | 'renderedContent'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  RenderUiTemplateResponse
mkRenderUiTemplateResponse pRenderedContent_ pResponseStatus_ =
  RenderUiTemplateResponse'
    { renderedContent = pRenderedContent_,
      errors = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A Liquid template that renders the HTML for the worker UI.
--
-- /Note:/ Consider using 'renderedContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutrsRenderedContent :: Lens.Lens' RenderUiTemplateResponse Lude.Text
rutrsRenderedContent = Lens.lens (renderedContent :: RenderUiTemplateResponse -> Lude.Text) (\s a -> s {renderedContent = a} :: RenderUiTemplateResponse)
{-# DEPRECATED rutrsRenderedContent "Use generic-lens or generic-optics with 'renderedContent' instead." #-}

-- | A list of one or more @RenderingError@ objects if any were encountered while rendering the template. If there were no errors, the list is empty.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutrsErrors :: Lens.Lens' RenderUiTemplateResponse [RenderingError]
rutrsErrors = Lens.lens (errors :: RenderUiTemplateResponse -> [RenderingError]) (\s a -> s {errors = a} :: RenderUiTemplateResponse)
{-# DEPRECATED rutrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutrsResponseStatus :: Lens.Lens' RenderUiTemplateResponse Lude.Int
rutrsResponseStatus = Lens.lens (responseStatus :: RenderUiTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RenderUiTemplateResponse)
{-# DEPRECATED rutrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
