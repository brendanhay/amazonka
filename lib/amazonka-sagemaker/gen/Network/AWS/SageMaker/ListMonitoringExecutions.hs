{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListMonitoringExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of all monitoring job executions.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListMonitoringExecutions
  ( -- * Creating a request
    ListMonitoringExecutions (..),
    mkListMonitoringExecutions,

    -- ** Request lenses
    lmeEndpointName,
    lmeLastModifiedTimeBefore,
    lmeScheduledTimeAfter,
    lmeCreationTimeAfter,
    lmeNextToken,
    lmeSortOrder,
    lmeLastModifiedTimeAfter,
    lmeCreationTimeBefore,
    lmeScheduledTimeBefore,
    lmeStatusEquals,
    lmeMonitoringScheduleName,
    lmeMaxResults,
    lmeSortBy,

    -- * Destructuring the response
    ListMonitoringExecutionsResponse (..),
    mkListMonitoringExecutionsResponse,

    -- ** Response lenses
    lmersNextToken,
    lmersMonitoringExecutionSummaries,
    lmersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListMonitoringExecutions' smart constructor.
data ListMonitoringExecutions = ListMonitoringExecutions'
  { -- | Name of a specific endpoint to fetch jobs for.
    endpointName :: Lude.Maybe Lude.Text,
    -- | A filter that returns only jobs modified after a specified time.
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | Filter for jobs scheduled after a specified time.
    scheduledTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only jobs created after a specified time.
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns only jobs modified before a specified time.
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only jobs created before a specified time.
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | Filter for jobs scheduled before a specified time.
    scheduledTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that retrieves only jobs with a specific status.
    statusEquals :: Lude.Maybe ExecutionStatus,
    -- | Name of a specific schedule to fetch jobs for.
    monitoringScheduleName :: Lude.Maybe Lude.Text,
    -- | The maximum number of jobs to return in the response. The default value is 10.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
    sortBy :: Lude.Maybe MonitoringExecutionSortKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMonitoringExecutions' with the minimum fields required to make a request.
--
-- * 'endpointName' - Name of a specific endpoint to fetch jobs for.
-- * 'lastModifiedTimeBefore' - A filter that returns only jobs modified after a specified time.
-- * 'scheduledTimeAfter' - Filter for jobs scheduled after a specified time.
-- * 'creationTimeAfter' - A filter that returns only jobs created after a specified time.
-- * 'nextToken' - The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
-- * 'sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
-- * 'lastModifiedTimeAfter' - A filter that returns only jobs modified before a specified time.
-- * 'creationTimeBefore' - A filter that returns only jobs created before a specified time.
-- * 'scheduledTimeBefore' - Filter for jobs scheduled before a specified time.
-- * 'statusEquals' - A filter that retrieves only jobs with a specific status.
-- * 'monitoringScheduleName' - Name of a specific schedule to fetch jobs for.
-- * 'maxResults' - The maximum number of jobs to return in the response. The default value is 10.
-- * 'sortBy' - Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
mkListMonitoringExecutions ::
  ListMonitoringExecutions
mkListMonitoringExecutions =
  ListMonitoringExecutions'
    { endpointName = Lude.Nothing,
      lastModifiedTimeBefore = Lude.Nothing,
      scheduledTimeAfter = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      lastModifiedTimeAfter = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      scheduledTimeBefore = Lude.Nothing,
      statusEquals = Lude.Nothing,
      monitoringScheduleName = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | Name of a specific endpoint to fetch jobs for.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeEndpointName :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe Lude.Text)
lmeEndpointName = Lens.lens (endpointName :: ListMonitoringExecutions -> Lude.Maybe Lude.Text) (\s a -> s {endpointName = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | A filter that returns only jobs modified after a specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeLastModifiedTimeBefore :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe Lude.Timestamp)
lmeLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListMonitoringExecutions -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | Filter for jobs scheduled after a specified time.
--
-- /Note:/ Consider using 'scheduledTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeScheduledTimeAfter :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe Lude.Timestamp)
lmeScheduledTimeAfter = Lens.lens (scheduledTimeAfter :: ListMonitoringExecutions -> Lude.Maybe Lude.Timestamp) (\s a -> s {scheduledTimeAfter = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeScheduledTimeAfter "Use generic-lens or generic-optics with 'scheduledTimeAfter' instead." #-}

-- | A filter that returns only jobs created after a specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeCreationTimeAfter :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe Lude.Timestamp)
lmeCreationTimeAfter = Lens.lens (creationTimeAfter :: ListMonitoringExecutions -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeNextToken :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe Lude.Text)
lmeNextToken = Lens.lens (nextToken :: ListMonitoringExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeSortOrder :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe SortOrder)
lmeSortOrder = Lens.lens (sortOrder :: ListMonitoringExecutions -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only jobs modified before a specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeLastModifiedTimeAfter :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe Lude.Timestamp)
lmeLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListMonitoringExecutions -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only jobs created before a specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeCreationTimeBefore :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe Lude.Timestamp)
lmeCreationTimeBefore = Lens.lens (creationTimeBefore :: ListMonitoringExecutions -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | Filter for jobs scheduled before a specified time.
--
-- /Note:/ Consider using 'scheduledTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeScheduledTimeBefore :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe Lude.Timestamp)
lmeScheduledTimeBefore = Lens.lens (scheduledTimeBefore :: ListMonitoringExecutions -> Lude.Maybe Lude.Timestamp) (\s a -> s {scheduledTimeBefore = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeScheduledTimeBefore "Use generic-lens or generic-optics with 'scheduledTimeBefore' instead." #-}

-- | A filter that retrieves only jobs with a specific status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeStatusEquals :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe ExecutionStatus)
lmeStatusEquals = Lens.lens (statusEquals :: ListMonitoringExecutions -> Lude.Maybe ExecutionStatus) (\s a -> s {statusEquals = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | Name of a specific schedule to fetch jobs for.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeMonitoringScheduleName :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe Lude.Text)
lmeMonitoringScheduleName = Lens.lens (monitoringScheduleName :: ListMonitoringExecutions -> Lude.Maybe Lude.Text) (\s a -> s {monitoringScheduleName = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

-- | The maximum number of jobs to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeMaxResults :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe Lude.Natural)
lmeMaxResults = Lens.lens (maxResults :: ListMonitoringExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmeSortBy :: Lens.Lens' ListMonitoringExecutions (Lude.Maybe MonitoringExecutionSortKey)
lmeSortBy = Lens.lens (sortBy :: ListMonitoringExecutions -> Lude.Maybe MonitoringExecutionSortKey) (\s a -> s {sortBy = a} :: ListMonitoringExecutions)
{-# DEPRECATED lmeSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListMonitoringExecutions where
  page rq rs
    | Page.stop (rs Lens.^. lmersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmersMonitoringExecutionSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmeNextToken Lens..~ rs Lens.^. lmersNextToken

instance Lude.AWSRequest ListMonitoringExecutions where
  type Rs ListMonitoringExecutions = ListMonitoringExecutionsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMonitoringExecutionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "MonitoringExecutionSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMonitoringExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListMonitoringExecutions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListMonitoringExecutions where
  toJSON ListMonitoringExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EndpointName" Lude..=) Lude.<$> endpointName,
            ("LastModifiedTimeBefore" Lude..=) Lude.<$> lastModifiedTimeBefore,
            ("ScheduledTimeAfter" Lude..=) Lude.<$> scheduledTimeAfter,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("LastModifiedTimeAfter" Lude..=) Lude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("ScheduledTimeBefore" Lude..=) Lude.<$> scheduledTimeBefore,
            ("StatusEquals" Lude..=) Lude.<$> statusEquals,
            ("MonitoringScheduleName" Lude..=) Lude.<$> monitoringScheduleName,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListMonitoringExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMonitoringExecutions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListMonitoringExecutionsResponse' smart constructor.
data ListMonitoringExecutionsResponse = ListMonitoringExecutionsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
    nextToken :: Lude.Maybe Lude.Text,
    -- | A JSON array in which each element is a summary for a monitoring execution.
    monitoringExecutionSummaries :: [MonitoringExecutionSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMonitoringExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
-- * 'monitoringExecutionSummaries' - A JSON array in which each element is a summary for a monitoring execution.
-- * 'responseStatus' - The response status code.
mkListMonitoringExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMonitoringExecutionsResponse
mkListMonitoringExecutionsResponse pResponseStatus_ =
  ListMonitoringExecutionsResponse'
    { nextToken = Lude.Nothing,
      monitoringExecutionSummaries = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmersNextToken :: Lens.Lens' ListMonitoringExecutionsResponse (Lude.Maybe Lude.Text)
lmersNextToken = Lens.lens (nextToken :: ListMonitoringExecutionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMonitoringExecutionsResponse)
{-# DEPRECATED lmersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A JSON array in which each element is a summary for a monitoring execution.
--
-- /Note:/ Consider using 'monitoringExecutionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmersMonitoringExecutionSummaries :: Lens.Lens' ListMonitoringExecutionsResponse [MonitoringExecutionSummary]
lmersMonitoringExecutionSummaries = Lens.lens (monitoringExecutionSummaries :: ListMonitoringExecutionsResponse -> [MonitoringExecutionSummary]) (\s a -> s {monitoringExecutionSummaries = a} :: ListMonitoringExecutionsResponse)
{-# DEPRECATED lmersMonitoringExecutionSummaries "Use generic-lens or generic-optics with 'monitoringExecutionSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmersResponseStatus :: Lens.Lens' ListMonitoringExecutionsResponse Lude.Int
lmersResponseStatus = Lens.lens (responseStatus :: ListMonitoringExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMonitoringExecutionsResponse)
{-# DEPRECATED lmersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
