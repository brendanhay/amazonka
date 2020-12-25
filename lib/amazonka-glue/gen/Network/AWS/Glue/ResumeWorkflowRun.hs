{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ResumeWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts selected nodes of a previous partially completed workflow run and resumes the workflow run. The selected nodes and all nodes that are downstream from the selected nodes are run.
module Network.AWS.Glue.ResumeWorkflowRun
  ( -- * Creating a request
    ResumeWorkflowRun (..),
    mkResumeWorkflowRun,

    -- ** Request lenses
    rwrName,
    rwrRunId,
    rwrNodeIds,

    -- * Destructuring the response
    ResumeWorkflowRunResponse (..),
    mkResumeWorkflowRunResponse,

    -- ** Response lenses
    rwrrrsNodeIds,
    rwrrrsRunId,
    rwrrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResumeWorkflowRun' smart constructor.
data ResumeWorkflowRun = ResumeWorkflowRun'
  { -- | The name of the workflow to resume.
    name :: Types.Name,
    -- | The ID of the workflow run to resume.
    runId :: Types.RunId,
    -- | A list of the node IDs for the nodes you want to restart. The nodes that are to be restarted must have a run attempt in the original run.
    nodeIds :: [Types.NameString]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeWorkflowRun' value with any optional fields omitted.
mkResumeWorkflowRun ::
  -- | 'name'
  Types.Name ->
  -- | 'runId'
  Types.RunId ->
  ResumeWorkflowRun
mkResumeWorkflowRun name runId =
  ResumeWorkflowRun' {name, runId, nodeIds = Core.mempty}

-- | The name of the workflow to resume.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrName :: Lens.Lens' ResumeWorkflowRun Types.Name
rwrName = Lens.field @"name"
{-# DEPRECATED rwrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the workflow run to resume.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrRunId :: Lens.Lens' ResumeWorkflowRun Types.RunId
rwrRunId = Lens.field @"runId"
{-# DEPRECATED rwrRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | A list of the node IDs for the nodes you want to restart. The nodes that are to be restarted must have a run attempt in the original run.
--
-- /Note:/ Consider using 'nodeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrNodeIds :: Lens.Lens' ResumeWorkflowRun [Types.NameString]
rwrNodeIds = Lens.field @"nodeIds"
{-# DEPRECATED rwrNodeIds "Use generic-lens or generic-optics with 'nodeIds' instead." #-}

instance Core.FromJSON ResumeWorkflowRun where
  toJSON ResumeWorkflowRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("RunId" Core..= runId),
            Core.Just ("NodeIds" Core..= nodeIds)
          ]
      )

instance Core.AWSRequest ResumeWorkflowRun where
  type Rs ResumeWorkflowRun = ResumeWorkflowRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.ResumeWorkflowRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ResumeWorkflowRunResponse'
            Core.<$> (x Core..:? "NodeIds")
            Core.<*> (x Core..:? "RunId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkResumeWorkflowRunResponse' smart constructor.
data ResumeWorkflowRunResponse = ResumeWorkflowRunResponse'
  { -- | A list of the node IDs for the nodes that were actually restarted.
    nodeIds :: Core.Maybe [Types.NameString],
    -- | The new ID assigned to the resumed workflow run. Each resume of a workflow run will have a new run ID.
    runId :: Core.Maybe Types.RunId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeWorkflowRunResponse' value with any optional fields omitted.
mkResumeWorkflowRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ResumeWorkflowRunResponse
mkResumeWorkflowRunResponse responseStatus =
  ResumeWorkflowRunResponse'
    { nodeIds = Core.Nothing,
      runId = Core.Nothing,
      responseStatus
    }

-- | A list of the node IDs for the nodes that were actually restarted.
--
-- /Note:/ Consider using 'nodeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrrrsNodeIds :: Lens.Lens' ResumeWorkflowRunResponse (Core.Maybe [Types.NameString])
rwrrrsNodeIds = Lens.field @"nodeIds"
{-# DEPRECATED rwrrrsNodeIds "Use generic-lens or generic-optics with 'nodeIds' instead." #-}

-- | The new ID assigned to the resumed workflow run. Each resume of a workflow run will have a new run ID.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrrrsRunId :: Lens.Lens' ResumeWorkflowRunResponse (Core.Maybe Types.RunId)
rwrrrsRunId = Lens.field @"runId"
{-# DEPRECATED rwrrrsRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrrrsResponseStatus :: Lens.Lens' ResumeWorkflowRunResponse Core.Int
rwrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rwrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
