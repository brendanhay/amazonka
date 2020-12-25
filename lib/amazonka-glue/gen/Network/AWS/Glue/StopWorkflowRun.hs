{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StopWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the execution of the specified workflow run.
module Network.AWS.Glue.StopWorkflowRun
  ( -- * Creating a request
    StopWorkflowRun (..),
    mkStopWorkflowRun,

    -- ** Request lenses
    swrfName,
    swrfRunId,

    -- * Destructuring the response
    StopWorkflowRunResponse (..),
    mkStopWorkflowRunResponse,

    -- ** Response lenses
    swrrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopWorkflowRun' smart constructor.
data StopWorkflowRun = StopWorkflowRun'
  { -- | The name of the workflow to stop.
    name :: Types.Name,
    -- | The ID of the workflow run to stop.
    runId :: Types.RunId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopWorkflowRun' value with any optional fields omitted.
mkStopWorkflowRun ::
  -- | 'name'
  Types.Name ->
  -- | 'runId'
  Types.RunId ->
  StopWorkflowRun
mkStopWorkflowRun name runId = StopWorkflowRun' {name, runId}

-- | The name of the workflow to stop.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrfName :: Lens.Lens' StopWorkflowRun Types.Name
swrfName = Lens.field @"name"
{-# DEPRECATED swrfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the workflow run to stop.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrfRunId :: Lens.Lens' StopWorkflowRun Types.RunId
swrfRunId = Lens.field @"runId"
{-# DEPRECATED swrfRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Core.FromJSON StopWorkflowRun where
  toJSON StopWorkflowRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("RunId" Core..= runId)
          ]
      )

instance Core.AWSRequest StopWorkflowRun where
  type Rs StopWorkflowRun = StopWorkflowRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.StopWorkflowRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopWorkflowRunResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopWorkflowRunResponse' smart constructor.
newtype StopWorkflowRunResponse = StopWorkflowRunResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopWorkflowRunResponse' value with any optional fields omitted.
mkStopWorkflowRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopWorkflowRunResponse
mkStopWorkflowRunResponse responseStatus =
  StopWorkflowRunResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrrfrsResponseStatus :: Lens.Lens' StopWorkflowRunResponse Core.Int
swrrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED swrrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
