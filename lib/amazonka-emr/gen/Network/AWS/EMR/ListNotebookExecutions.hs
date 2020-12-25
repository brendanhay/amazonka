{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListNotebookExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summaries of all notebook executions. You can filter the list based on multiple criteria such as status, time range, and editor id. Returns a maximum of 50 notebook executions and a marker to track the paging of a longer notebook execution list across multiple @ListNotebookExecution@ calls.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListNotebookExecutions
  ( -- * Creating a request
    ListNotebookExecutions (..),
    mkListNotebookExecutions,

    -- ** Request lenses
    lneEditorId,
    lneFrom,
    lneMarker,
    lneStatus,
    lneTo,

    -- * Destructuring the response
    ListNotebookExecutionsResponse (..),
    mkListNotebookExecutionsResponse,

    -- ** Response lenses
    lnerrsMarker,
    lnerrsNotebookExecutions,
    lnerrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListNotebookExecutions' smart constructor.
data ListNotebookExecutions = ListNotebookExecutions'
  { -- | The unique ID of the editor associated with the notebook execution.
    editorId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The beginning of time range filter for listing notebook executions. The default is the timestamp of 30 days ago.
    from :: Core.Maybe Core.NominalDiffTime,
    -- | The pagination token, returned by a previous @ListNotebookExecutions@ call, that indicates the start of the list for this @ListNotebookExecutions@ call.
    marker :: Core.Maybe Types.Marker,
    -- | The status filter for listing notebook executions.
    --
    --
    --     * @START_PENDING@ indicates that the cluster has received the execution request but execution has not begun.
    --
    --
    --     * @STARTING@ indicates that the execution is starting on the cluster.
    --
    --
    --     * @RUNNING@ indicates that the execution is being processed by the cluster.
    --
    --
    --     * @FINISHING@ indicates that execution processing is in the final stages.
    --
    --
    --     * @FINISHED@ indicates that the execution has completed without error.
    --
    --
    --     * @FAILING@ indicates that the execution is failing and will not finish successfully.
    --
    --
    --     * @FAILED@ indicates that the execution failed.
    --
    --
    --     * @STOP_PENDING@ indicates that the cluster has received a @StopNotebookExecution@ request and the stop is pending.
    --
    --
    --     * @STOPPING@ indicates that the cluster is in the process of stopping the execution as a result of a @StopNotebookExecution@ request.
    --
    --
    --     * @STOPPED@ indicates that the execution stopped because of a @StopNotebookExecution@ request.
    status :: Core.Maybe Types.NotebookExecutionStatus,
    -- | The end of time range filter for listing notebook executions. The default is the current timestamp.
    to :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListNotebookExecutions' value with any optional fields omitted.
mkListNotebookExecutions ::
  ListNotebookExecutions
mkListNotebookExecutions =
  ListNotebookExecutions'
    { editorId = Core.Nothing,
      from = Core.Nothing,
      marker = Core.Nothing,
      status = Core.Nothing,
      to = Core.Nothing
    }

-- | The unique ID of the editor associated with the notebook execution.
--
-- /Note:/ Consider using 'editorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lneEditorId :: Lens.Lens' ListNotebookExecutions (Core.Maybe Types.XmlStringMaxLen256)
lneEditorId = Lens.field @"editorId"
{-# DEPRECATED lneEditorId "Use generic-lens or generic-optics with 'editorId' instead." #-}

-- | The beginning of time range filter for listing notebook executions. The default is the timestamp of 30 days ago.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lneFrom :: Lens.Lens' ListNotebookExecutions (Core.Maybe Core.NominalDiffTime)
lneFrom = Lens.field @"from"
{-# DEPRECATED lneFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The pagination token, returned by a previous @ListNotebookExecutions@ call, that indicates the start of the list for this @ListNotebookExecutions@ call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lneMarker :: Lens.Lens' ListNotebookExecutions (Core.Maybe Types.Marker)
lneMarker = Lens.field @"marker"
{-# DEPRECATED lneMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The status filter for listing notebook executions.
--
--
--     * @START_PENDING@ indicates that the cluster has received the execution request but execution has not begun.
--
--
--     * @STARTING@ indicates that the execution is starting on the cluster.
--
--
--     * @RUNNING@ indicates that the execution is being processed by the cluster.
--
--
--     * @FINISHING@ indicates that execution processing is in the final stages.
--
--
--     * @FINISHED@ indicates that the execution has completed without error.
--
--
--     * @FAILING@ indicates that the execution is failing and will not finish successfully.
--
--
--     * @FAILED@ indicates that the execution failed.
--
--
--     * @STOP_PENDING@ indicates that the cluster has received a @StopNotebookExecution@ request and the stop is pending.
--
--
--     * @STOPPING@ indicates that the cluster is in the process of stopping the execution as a result of a @StopNotebookExecution@ request.
--
--
--     * @STOPPED@ indicates that the execution stopped because of a @StopNotebookExecution@ request.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lneStatus :: Lens.Lens' ListNotebookExecutions (Core.Maybe Types.NotebookExecutionStatus)
lneStatus = Lens.field @"status"
{-# DEPRECATED lneStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The end of time range filter for listing notebook executions. The default is the current timestamp.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lneTo :: Lens.Lens' ListNotebookExecutions (Core.Maybe Core.NominalDiffTime)
lneTo = Lens.field @"to"
{-# DEPRECATED lneTo "Use generic-lens or generic-optics with 'to' instead." #-}

instance Core.FromJSON ListNotebookExecutions where
  toJSON ListNotebookExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ ("EditorId" Core..=) Core.<$> editorId,
            ("From" Core..=) Core.<$> from,
            ("Marker" Core..=) Core.<$> marker,
            ("Status" Core..=) Core.<$> status,
            ("To" Core..=) Core.<$> to
          ]
      )

instance Core.AWSRequest ListNotebookExecutions where
  type Rs ListNotebookExecutions = ListNotebookExecutionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ElasticMapReduce.ListNotebookExecutions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNotebookExecutionsResponse'
            Core.<$> (x Core..:? "Marker")
            Core.<*> (x Core..:? "NotebookExecutions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListNotebookExecutions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"notebookExecutions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkListNotebookExecutionsResponse' smart constructor.
data ListNotebookExecutionsResponse = ListNotebookExecutionsResponse'
  { -- | A pagination token that a subsequent @ListNotebookExecutions@ can use to determine the next set of results to retrieve.
    marker :: Core.Maybe Types.Marker,
    -- | A list of notebook executions.
    notebookExecutions :: Core.Maybe [Types.NotebookExecutionSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListNotebookExecutionsResponse' value with any optional fields omitted.
mkListNotebookExecutionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListNotebookExecutionsResponse
mkListNotebookExecutionsResponse responseStatus =
  ListNotebookExecutionsResponse'
    { marker = Core.Nothing,
      notebookExecutions = Core.Nothing,
      responseStatus
    }

-- | A pagination token that a subsequent @ListNotebookExecutions@ can use to determine the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnerrsMarker :: Lens.Lens' ListNotebookExecutionsResponse (Core.Maybe Types.Marker)
lnerrsMarker = Lens.field @"marker"
{-# DEPRECATED lnerrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of notebook executions.
--
-- /Note:/ Consider using 'notebookExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnerrsNotebookExecutions :: Lens.Lens' ListNotebookExecutionsResponse (Core.Maybe [Types.NotebookExecutionSummary])
lnerrsNotebookExecutions = Lens.field @"notebookExecutions"
{-# DEPRECATED lnerrsNotebookExecutions "Use generic-lens or generic-optics with 'notebookExecutions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnerrsResponseStatus :: Lens.Lens' ListNotebookExecutionsResponse Core.Int
lnerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lnerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
