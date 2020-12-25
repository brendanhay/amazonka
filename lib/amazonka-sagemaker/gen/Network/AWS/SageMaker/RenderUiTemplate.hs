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
    rutTask,
    rutRoleArn,
    rutHumanTaskUiArn,
    rutUiTemplate,

    -- * Destructuring the response
    RenderUiTemplateResponse (..),
    mkRenderUiTemplateResponse,

    -- ** Response lenses
    rutrrsRenderedContent,
    rutrrsErrors,
    rutrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkRenderUiTemplate' smart constructor.
data RenderUiTemplate = RenderUiTemplate'
  { -- | A @RenderableTask@ object containing a representative task to render.
    task :: Types.RenderableTask,
    -- | The Amazon Resource Name (ARN) that has access to the S3 objects that are used by the template.
    roleArn :: Types.RoleArn,
    -- | The @HumanTaskUiArn@ of the worker UI that you want to render. Do not provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter.
    --
    -- See a list of available Human Ui Amazon Resource Names (ARNs) in 'UiConfig' .
    humanTaskUiArn :: Core.Maybe Types.HumanTaskUiArn,
    -- | A @Template@ object containing the worker UI template to render.
    uiTemplate :: Core.Maybe Types.UiTemplate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenderUiTemplate' value with any optional fields omitted.
mkRenderUiTemplate ::
  -- | 'task'
  Types.RenderableTask ->
  -- | 'roleArn'
  Types.RoleArn ->
  RenderUiTemplate
mkRenderUiTemplate task roleArn =
  RenderUiTemplate'
    { task,
      roleArn,
      humanTaskUiArn = Core.Nothing,
      uiTemplate = Core.Nothing
    }

-- | A @RenderableTask@ object containing a representative task to render.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutTask :: Lens.Lens' RenderUiTemplate Types.RenderableTask
rutTask = Lens.field @"task"
{-# DEPRECATED rutTask "Use generic-lens or generic-optics with 'task' instead." #-}

-- | The Amazon Resource Name (ARN) that has access to the S3 objects that are used by the template.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutRoleArn :: Lens.Lens' RenderUiTemplate Types.RoleArn
rutRoleArn = Lens.field @"roleArn"
{-# DEPRECATED rutRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The @HumanTaskUiArn@ of the worker UI that you want to render. Do not provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter.
--
-- See a list of available Human Ui Amazon Resource Names (ARNs) in 'UiConfig' .
--
-- /Note:/ Consider using 'humanTaskUiArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutHumanTaskUiArn :: Lens.Lens' RenderUiTemplate (Core.Maybe Types.HumanTaskUiArn)
rutHumanTaskUiArn = Lens.field @"humanTaskUiArn"
{-# DEPRECATED rutHumanTaskUiArn "Use generic-lens or generic-optics with 'humanTaskUiArn' instead." #-}

-- | A @Template@ object containing the worker UI template to render.
--
-- /Note:/ Consider using 'uiTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutUiTemplate :: Lens.Lens' RenderUiTemplate (Core.Maybe Types.UiTemplate)
rutUiTemplate = Lens.field @"uiTemplate"
{-# DEPRECATED rutUiTemplate "Use generic-lens or generic-optics with 'uiTemplate' instead." #-}

instance Core.FromJSON RenderUiTemplate where
  toJSON RenderUiTemplate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Task" Core..= task),
            Core.Just ("RoleArn" Core..= roleArn),
            ("HumanTaskUiArn" Core..=) Core.<$> humanTaskUiArn,
            ("UiTemplate" Core..=) Core.<$> uiTemplate
          ]
      )

instance Core.AWSRequest RenderUiTemplate where
  type Rs RenderUiTemplate = RenderUiTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.RenderUiTemplate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RenderUiTemplateResponse'
            Core.<$> (x Core..: "RenderedContent")
            Core.<*> (x Core..:? "Errors" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRenderUiTemplateResponse' smart constructor.
data RenderUiTemplateResponse = RenderUiTemplateResponse'
  { -- | A Liquid template that renders the HTML for the worker UI.
    renderedContent :: Types.String,
    -- | A list of one or more @RenderingError@ objects if any were encountered while rendering the template. If there were no errors, the list is empty.
    errors :: [Types.RenderingError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenderUiTemplateResponse' value with any optional fields omitted.
mkRenderUiTemplateResponse ::
  -- | 'renderedContent'
  Types.String ->
  -- | 'responseStatus'
  Core.Int ->
  RenderUiTemplateResponse
mkRenderUiTemplateResponse renderedContent responseStatus =
  RenderUiTemplateResponse'
    { renderedContent,
      errors = Core.mempty,
      responseStatus
    }

-- | A Liquid template that renders the HTML for the worker UI.
--
-- /Note:/ Consider using 'renderedContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutrrsRenderedContent :: Lens.Lens' RenderUiTemplateResponse Types.String
rutrrsRenderedContent = Lens.field @"renderedContent"
{-# DEPRECATED rutrrsRenderedContent "Use generic-lens or generic-optics with 'renderedContent' instead." #-}

-- | A list of one or more @RenderingError@ objects if any were encountered while rendering the template. If there were no errors, the list is empty.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutrrsErrors :: Lens.Lens' RenderUiTemplateResponse [Types.RenderingError]
rutrrsErrors = Lens.field @"errors"
{-# DEPRECATED rutrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutrrsResponseStatus :: Lens.Lens' RenderUiTemplateResponse Core.Int
rutrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rutrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
