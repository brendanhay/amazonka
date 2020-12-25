{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for a given workflow run.
module Network.AWS.Glue.GetWorkflowRun
  ( -- * Creating a request
    GetWorkflowRun (..),
    mkGetWorkflowRun,

    -- ** Request lenses
    gwrfName,
    gwrfRunId,
    gwrfIncludeGraph,

    -- * Destructuring the response
    GetWorkflowRunResponse (..),
    mkGetWorkflowRunResponse,

    -- ** Response lenses
    gwrrfrsRun,
    gwrrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetWorkflowRun' smart constructor.
data GetWorkflowRun = GetWorkflowRun'
  { -- | Name of the workflow being run.
    name :: Types.Name,
    -- | The ID of the workflow run.
    runId :: Types.RunId,
    -- | Specifies whether to include the workflow graph in response or not.
    includeGraph :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetWorkflowRun' value with any optional fields omitted.
mkGetWorkflowRun ::
  -- | 'name'
  Types.Name ->
  -- | 'runId'
  Types.RunId ->
  GetWorkflowRun
mkGetWorkflowRun name runId =
  GetWorkflowRun' {name, runId, includeGraph = Core.Nothing}

-- | Name of the workflow being run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrfName :: Lens.Lens' GetWorkflowRun Types.Name
gwrfName = Lens.field @"name"
{-# DEPRECATED gwrfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the workflow run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrfRunId :: Lens.Lens' GetWorkflowRun Types.RunId
gwrfRunId = Lens.field @"runId"
{-# DEPRECATED gwrfRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | Specifies whether to include the workflow graph in response or not.
--
-- /Note:/ Consider using 'includeGraph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrfIncludeGraph :: Lens.Lens' GetWorkflowRun (Core.Maybe Core.Bool)
gwrfIncludeGraph = Lens.field @"includeGraph"
{-# DEPRECATED gwrfIncludeGraph "Use generic-lens or generic-optics with 'includeGraph' instead." #-}

instance Core.FromJSON GetWorkflowRun where
  toJSON GetWorkflowRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("RunId" Core..= runId),
            ("IncludeGraph" Core..=) Core.<$> includeGraph
          ]
      )

instance Core.AWSRequest GetWorkflowRun where
  type Rs GetWorkflowRun = GetWorkflowRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetWorkflowRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowRunResponse'
            Core.<$> (x Core..:? "Run") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetWorkflowRunResponse' smart constructor.
data GetWorkflowRunResponse = GetWorkflowRunResponse'
  { -- | The requested workflow run metadata.
    run :: Core.Maybe Types.WorkflowRun,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetWorkflowRunResponse' value with any optional fields omitted.
mkGetWorkflowRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetWorkflowRunResponse
mkGetWorkflowRunResponse responseStatus =
  GetWorkflowRunResponse' {run = Core.Nothing, responseStatus}

-- | The requested workflow run metadata.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrfrsRun :: Lens.Lens' GetWorkflowRunResponse (Core.Maybe Types.WorkflowRun)
gwrrfrsRun = Lens.field @"run"
{-# DEPRECATED gwrrfrsRun "Use generic-lens or generic-optics with 'run' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrfrsResponseStatus :: Lens.Lens' GetWorkflowRunResponse Core.Int
gwrrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gwrrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
