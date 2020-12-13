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
    lneStatus,
    lneEditorId,
    lneTo,
    lneFrom,
    lneMarker,

    -- * Destructuring the response
    ListNotebookExecutionsResponse (..),
    mkListNotebookExecutionsResponse,

    -- ** Response lenses
    lnersNotebookExecutions,
    lnersMarker,
    lnersResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListNotebookExecutions' smart constructor.
data ListNotebookExecutions = ListNotebookExecutions'
  { -- | The status filter for listing notebook executions.
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
    status :: Lude.Maybe NotebookExecutionStatus,
    -- | The unique ID of the editor associated with the notebook execution.
    editorId :: Lude.Maybe Lude.Text,
    -- | The end of time range filter for listing notebook executions. The default is the current timestamp.
    to :: Lude.Maybe Lude.Timestamp,
    -- | The beginning of time range filter for listing notebook executions. The default is the timestamp of 30 days ago.
    from :: Lude.Maybe Lude.Timestamp,
    -- | The pagination token, returned by a previous @ListNotebookExecutions@ call, that indicates the start of the list for this @ListNotebookExecutions@ call.
    marker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListNotebookExecutions' with the minimum fields required to make a request.
--
-- * 'status' - The status filter for listing notebook executions.
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
-- * 'editorId' - The unique ID of the editor associated with the notebook execution.
-- * 'to' - The end of time range filter for listing notebook executions. The default is the current timestamp.
-- * 'from' - The beginning of time range filter for listing notebook executions. The default is the timestamp of 30 days ago.
-- * 'marker' - The pagination token, returned by a previous @ListNotebookExecutions@ call, that indicates the start of the list for this @ListNotebookExecutions@ call.
mkListNotebookExecutions ::
  ListNotebookExecutions
mkListNotebookExecutions =
  ListNotebookExecutions'
    { status = Lude.Nothing,
      editorId = Lude.Nothing,
      to = Lude.Nothing,
      from = Lude.Nothing,
      marker = Lude.Nothing
    }

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
lneStatus :: Lens.Lens' ListNotebookExecutions (Lude.Maybe NotebookExecutionStatus)
lneStatus = Lens.lens (status :: ListNotebookExecutions -> Lude.Maybe NotebookExecutionStatus) (\s a -> s {status = a} :: ListNotebookExecutions)
{-# DEPRECATED lneStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID of the editor associated with the notebook execution.
--
-- /Note:/ Consider using 'editorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lneEditorId :: Lens.Lens' ListNotebookExecutions (Lude.Maybe Lude.Text)
lneEditorId = Lens.lens (editorId :: ListNotebookExecutions -> Lude.Maybe Lude.Text) (\s a -> s {editorId = a} :: ListNotebookExecutions)
{-# DEPRECATED lneEditorId "Use generic-lens or generic-optics with 'editorId' instead." #-}

-- | The end of time range filter for listing notebook executions. The default is the current timestamp.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lneTo :: Lens.Lens' ListNotebookExecutions (Lude.Maybe Lude.Timestamp)
lneTo = Lens.lens (to :: ListNotebookExecutions -> Lude.Maybe Lude.Timestamp) (\s a -> s {to = a} :: ListNotebookExecutions)
{-# DEPRECATED lneTo "Use generic-lens or generic-optics with 'to' instead." #-}

-- | The beginning of time range filter for listing notebook executions. The default is the timestamp of 30 days ago.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lneFrom :: Lens.Lens' ListNotebookExecutions (Lude.Maybe Lude.Timestamp)
lneFrom = Lens.lens (from :: ListNotebookExecutions -> Lude.Maybe Lude.Timestamp) (\s a -> s {from = a} :: ListNotebookExecutions)
{-# DEPRECATED lneFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The pagination token, returned by a previous @ListNotebookExecutions@ call, that indicates the start of the list for this @ListNotebookExecutions@ call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lneMarker :: Lens.Lens' ListNotebookExecutions (Lude.Maybe Lude.Text)
lneMarker = Lens.lens (marker :: ListNotebookExecutions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListNotebookExecutions)
{-# DEPRECATED lneMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Page.AWSPager ListNotebookExecutions where
  page rq rs
    | Page.stop (rs Lens.^. lnersMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lnersNotebookExecutions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lneMarker Lens..~ rs Lens.^. lnersMarker

instance Lude.AWSRequest ListNotebookExecutions where
  type Rs ListNotebookExecutions = ListNotebookExecutionsResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListNotebookExecutionsResponse'
            Lude.<$> (x Lude..?> "NotebookExecutions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListNotebookExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ListNotebookExecutions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListNotebookExecutions where
  toJSON ListNotebookExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("EditorId" Lude..=) Lude.<$> editorId,
            ("To" Lude..=) Lude.<$> to,
            ("From" Lude..=) Lude.<$> from,
            ("Marker" Lude..=) Lude.<$> marker
          ]
      )

instance Lude.ToPath ListNotebookExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListNotebookExecutions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListNotebookExecutionsResponse' smart constructor.
data ListNotebookExecutionsResponse = ListNotebookExecutionsResponse'
  { -- | A list of notebook executions.
    notebookExecutions :: Lude.Maybe [NotebookExecutionSummary],
    -- | A pagination token that a subsequent @ListNotebookExecutions@ can use to determine the next set of results to retrieve.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListNotebookExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'notebookExecutions' - A list of notebook executions.
-- * 'marker' - A pagination token that a subsequent @ListNotebookExecutions@ can use to determine the next set of results to retrieve.
-- * 'responseStatus' - The response status code.
mkListNotebookExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListNotebookExecutionsResponse
mkListNotebookExecutionsResponse pResponseStatus_ =
  ListNotebookExecutionsResponse'
    { notebookExecutions =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of notebook executions.
--
-- /Note:/ Consider using 'notebookExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnersNotebookExecutions :: Lens.Lens' ListNotebookExecutionsResponse (Lude.Maybe [NotebookExecutionSummary])
lnersNotebookExecutions = Lens.lens (notebookExecutions :: ListNotebookExecutionsResponse -> Lude.Maybe [NotebookExecutionSummary]) (\s a -> s {notebookExecutions = a} :: ListNotebookExecutionsResponse)
{-# DEPRECATED lnersNotebookExecutions "Use generic-lens or generic-optics with 'notebookExecutions' instead." #-}

-- | A pagination token that a subsequent @ListNotebookExecutions@ can use to determine the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnersMarker :: Lens.Lens' ListNotebookExecutionsResponse (Lude.Maybe Lude.Text)
lnersMarker = Lens.lens (marker :: ListNotebookExecutionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListNotebookExecutionsResponse)
{-# DEPRECATED lnersMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnersResponseStatus :: Lens.Lens' ListNotebookExecutionsResponse Lude.Int
lnersResponseStatus = Lens.lens (responseStatus :: ListNotebookExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListNotebookExecutionsResponse)
{-# DEPRECATED lnersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
