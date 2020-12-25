{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new run of the specified workflow.
module Network.AWS.Glue.StartWorkflowRun
  ( -- * Creating a request
    StartWorkflowRun (..),
    mkStartWorkflowRun,

    -- ** Request lenses
    swrName,

    -- * Destructuring the response
    StartWorkflowRunResponse (..),
    mkStartWorkflowRunResponse,

    -- ** Response lenses
    swrrrsRunId,
    swrrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartWorkflowRun' smart constructor.
newtype StartWorkflowRun = StartWorkflowRun'
  { -- | The name of the workflow to start.
    name :: Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartWorkflowRun' value with any optional fields omitted.
mkStartWorkflowRun ::
  -- | 'name'
  Types.NameString ->
  StartWorkflowRun
mkStartWorkflowRun name = StartWorkflowRun' {name}

-- | The name of the workflow to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrName :: Lens.Lens' StartWorkflowRun Types.NameString
swrName = Lens.field @"name"
{-# DEPRECATED swrName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON StartWorkflowRun where
  toJSON StartWorkflowRun {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StartWorkflowRun where
  type Rs StartWorkflowRun = StartWorkflowRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.StartWorkflowRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartWorkflowRunResponse'
            Core.<$> (x Core..:? "RunId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartWorkflowRunResponse' smart constructor.
data StartWorkflowRunResponse = StartWorkflowRunResponse'
  { -- | An Id for the new run.
    runId :: Core.Maybe Types.RunId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartWorkflowRunResponse' value with any optional fields omitted.
mkStartWorkflowRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartWorkflowRunResponse
mkStartWorkflowRunResponse responseStatus =
  StartWorkflowRunResponse' {runId = Core.Nothing, responseStatus}

-- | An Id for the new run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrrrsRunId :: Lens.Lens' StartWorkflowRunResponse (Core.Maybe Types.RunId)
swrrrsRunId = Lens.field @"runId"
{-# DEPRECATED swrrrsRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrrrsResponseStatus :: Lens.Lens' StartWorkflowRunResponse Core.Int
swrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED swrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
