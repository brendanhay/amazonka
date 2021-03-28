{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RenderUiTemplate (..)
    , mkRenderUiTemplate
    -- ** Request lenses
    , rutTask
    , rutRoleArn
    , rutHumanTaskUiArn
    , rutUiTemplate

    -- * Destructuring the response
    , RenderUiTemplateResponse (..)
    , mkRenderUiTemplateResponse
    -- ** Response lenses
    , rutrrsRenderedContent
    , rutrrsErrors
    , rutrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkRenderUiTemplate' smart constructor.
data RenderUiTemplate = RenderUiTemplate'
  { task :: Types.RenderableTask
    -- ^ A @RenderableTask@ object containing a representative task to render.
  , roleArn :: Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) that has access to the S3 objects that are used by the template.
  , humanTaskUiArn :: Core.Maybe Types.HumanTaskUiArn
    -- ^ The @HumanTaskUiArn@ of the worker UI that you want to render. Do not provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter.
--
-- See a list of available Human Ui Amazon Resource Names (ARNs) in 'UiConfig' .
  , uiTemplate :: Core.Maybe Types.UiTemplate
    -- ^ A @Template@ object containing the worker UI template to render.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenderUiTemplate' value with any optional fields omitted.
mkRenderUiTemplate
    :: Types.RenderableTask -- ^ 'task'
    -> Types.RoleArn -- ^ 'roleArn'
    -> RenderUiTemplate
mkRenderUiTemplate task roleArn
  = RenderUiTemplate'{task, roleArn, humanTaskUiArn = Core.Nothing,
                      uiTemplate = Core.Nothing}

-- | A @RenderableTask@ object containing a representative task to render.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutTask :: Lens.Lens' RenderUiTemplate Types.RenderableTask
rutTask = Lens.field @"task"
{-# INLINEABLE rutTask #-}
{-# DEPRECATED task "Use generic-lens or generic-optics with 'task' instead"  #-}

-- | The Amazon Resource Name (ARN) that has access to the S3 objects that are used by the template.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutRoleArn :: Lens.Lens' RenderUiTemplate Types.RoleArn
rutRoleArn = Lens.field @"roleArn"
{-# INLINEABLE rutRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The @HumanTaskUiArn@ of the worker UI that you want to render. Do not provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter.
--
-- See a list of available Human Ui Amazon Resource Names (ARNs) in 'UiConfig' .
--
-- /Note:/ Consider using 'humanTaskUiArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutHumanTaskUiArn :: Lens.Lens' RenderUiTemplate (Core.Maybe Types.HumanTaskUiArn)
rutHumanTaskUiArn = Lens.field @"humanTaskUiArn"
{-# INLINEABLE rutHumanTaskUiArn #-}
{-# DEPRECATED humanTaskUiArn "Use generic-lens or generic-optics with 'humanTaskUiArn' instead"  #-}

-- | A @Template@ object containing the worker UI template to render.
--
-- /Note:/ Consider using 'uiTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutUiTemplate :: Lens.Lens' RenderUiTemplate (Core.Maybe Types.UiTemplate)
rutUiTemplate = Lens.field @"uiTemplate"
{-# INLINEABLE rutUiTemplate #-}
{-# DEPRECATED uiTemplate "Use generic-lens or generic-optics with 'uiTemplate' instead"  #-}

instance Core.ToQuery RenderUiTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RenderUiTemplate where
        toHeaders RenderUiTemplate{..}
          = Core.pure ("X-Amz-Target", "SageMaker.RenderUiTemplate") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RenderUiTemplate where
        toJSON RenderUiTemplate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Task" Core..= task),
                  Core.Just ("RoleArn" Core..= roleArn),
                  ("HumanTaskUiArn" Core..=) Core.<$> humanTaskUiArn,
                  ("UiTemplate" Core..=) Core.<$> uiTemplate])

instance Core.AWSRequest RenderUiTemplate where
        type Rs RenderUiTemplate = RenderUiTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RenderUiTemplateResponse' Core.<$>
                   (x Core..: "RenderedContent") Core.<*>
                     x Core..:? "Errors" Core..!= Core.mempty
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRenderUiTemplateResponse' smart constructor.
data RenderUiTemplateResponse = RenderUiTemplateResponse'
  { renderedContent :: Core.Text
    -- ^ A Liquid template that renders the HTML for the worker UI.
  , errors :: [Types.RenderingError]
    -- ^ A list of one or more @RenderingError@ objects if any were encountered while rendering the template. If there were no errors, the list is empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenderUiTemplateResponse' value with any optional fields omitted.
mkRenderUiTemplateResponse
    :: Core.Text -- ^ 'renderedContent'
    -> Core.Int -- ^ 'responseStatus'
    -> RenderUiTemplateResponse
mkRenderUiTemplateResponse renderedContent responseStatus
  = RenderUiTemplateResponse'{renderedContent, errors = Core.mempty,
                              responseStatus}

-- | A Liquid template that renders the HTML for the worker UI.
--
-- /Note:/ Consider using 'renderedContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutrrsRenderedContent :: Lens.Lens' RenderUiTemplateResponse Core.Text
rutrrsRenderedContent = Lens.field @"renderedContent"
{-# INLINEABLE rutrrsRenderedContent #-}
{-# DEPRECATED renderedContent "Use generic-lens or generic-optics with 'renderedContent' instead"  #-}

-- | A list of one or more @RenderingError@ objects if any were encountered while rendering the template. If there were no errors, the list is empty.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutrrsErrors :: Lens.Lens' RenderUiTemplateResponse [Types.RenderingError]
rutrrsErrors = Lens.field @"errors"
{-# INLINEABLE rutrrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutrrsResponseStatus :: Lens.Lens' RenderUiTemplateResponse Core.Int
rutrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rutrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
