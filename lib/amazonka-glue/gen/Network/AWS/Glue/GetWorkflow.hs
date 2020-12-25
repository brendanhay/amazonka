{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves resource metadata for a workflow.
module Network.AWS.Glue.GetWorkflow
  ( -- * Creating a request
    GetWorkflow (..),
    mkGetWorkflow,

    -- ** Request lenses
    gwName,
    gwIncludeGraph,

    -- * Destructuring the response
    GetWorkflowResponse (..),
    mkGetWorkflowResponse,

    -- ** Response lenses
    gwrrsWorkflow,
    gwrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetWorkflow' smart constructor.
data GetWorkflow = GetWorkflow'
  { -- | The name of the workflow to retrieve.
    name :: Types.Name,
    -- | Specifies whether to include a graph when returning the workflow resource metadata.
    includeGraph :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetWorkflow' value with any optional fields omitted.
mkGetWorkflow ::
  -- | 'name'
  Types.Name ->
  GetWorkflow
mkGetWorkflow name =
  GetWorkflow' {name, includeGraph = Core.Nothing}

-- | The name of the workflow to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwName :: Lens.Lens' GetWorkflow Types.Name
gwName = Lens.field @"name"
{-# DEPRECATED gwName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies whether to include a graph when returning the workflow resource metadata.
--
-- /Note:/ Consider using 'includeGraph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwIncludeGraph :: Lens.Lens' GetWorkflow (Core.Maybe Core.Bool)
gwIncludeGraph = Lens.field @"includeGraph"
{-# DEPRECATED gwIncludeGraph "Use generic-lens or generic-optics with 'includeGraph' instead." #-}

instance Core.FromJSON GetWorkflow where
  toJSON GetWorkflow {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("IncludeGraph" Core..=) Core.<$> includeGraph
          ]
      )

instance Core.AWSRequest GetWorkflow where
  type Rs GetWorkflow = GetWorkflowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetWorkflow")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowResponse'
            Core.<$> (x Core..:? "Workflow") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetWorkflowResponse' smart constructor.
data GetWorkflowResponse = GetWorkflowResponse'
  { -- | The resource metadata for the workflow.
    workflow :: Core.Maybe Types.Workflow,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetWorkflowResponse' value with any optional fields omitted.
mkGetWorkflowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetWorkflowResponse
mkGetWorkflowResponse responseStatus =
  GetWorkflowResponse' {workflow = Core.Nothing, responseStatus}

-- | The resource metadata for the workflow.
--
-- /Note:/ Consider using 'workflow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrsWorkflow :: Lens.Lens' GetWorkflowResponse (Core.Maybe Types.Workflow)
gwrrsWorkflow = Lens.field @"workflow"
{-# DEPRECATED gwrrsWorkflow "Use generic-lens or generic-optics with 'workflow' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrsResponseStatus :: Lens.Lens' GetWorkflowResponse Core.Int
gwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
