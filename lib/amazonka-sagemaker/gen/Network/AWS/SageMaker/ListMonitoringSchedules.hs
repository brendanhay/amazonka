{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListMonitoringSchedules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of all monitoring schedules.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListMonitoringSchedules
  ( -- * Creating a request
    ListMonitoringSchedules (..),
    mkListMonitoringSchedules,

    -- ** Request lenses
    lmsNameContains,
    lmsEndpointName,
    lmsLastModifiedTimeBefore,
    lmsCreationTimeAfter,
    lmsNextToken,
    lmsSortOrder,
    lmsLastModifiedTimeAfter,
    lmsCreationTimeBefore,
    lmsStatusEquals,
    lmsMaxResults,
    lmsSortBy,

    -- * Destructuring the response
    ListMonitoringSchedulesResponse (..),
    mkListMonitoringSchedulesResponse,

    -- ** Response lenses
    lmsrsNextToken,
    lmsrsMonitoringScheduleSummaries,
    lmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListMonitoringSchedules' smart constructor.
data ListMonitoringSchedules = ListMonitoringSchedules'
  { -- | Filter for monitoring schedules whose name contains a specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | Name of a specific endpoint to fetch schedules for.
    endpointName :: Lude.Maybe Lude.Text,
    -- | A filter that returns only monitoring schedules modified before a specified time.
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only monitoring schedules created after a specified time.
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns only monitoring schedules modified after a specified time.
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only monitoring schedules created before a specified time.
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only monitoring schedules modified before a specified time.
    statusEquals :: Lude.Maybe ScheduleStatus,
    -- | The maximum number of jobs to return in the response. The default value is 10.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
    sortBy :: Lude.Maybe MonitoringScheduleSortKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMonitoringSchedules' with the minimum fields required to make a request.
--
-- * 'nameContains' - Filter for monitoring schedules whose name contains a specified string.
-- * 'endpointName' - Name of a specific endpoint to fetch schedules for.
-- * 'lastModifiedTimeBefore' - A filter that returns only monitoring schedules modified before a specified time.
-- * 'creationTimeAfter' - A filter that returns only monitoring schedules created after a specified time.
-- * 'nextToken' - The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
-- * 'sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
-- * 'lastModifiedTimeAfter' - A filter that returns only monitoring schedules modified after a specified time.
-- * 'creationTimeBefore' - A filter that returns only monitoring schedules created before a specified time.
-- * 'statusEquals' - A filter that returns only monitoring schedules modified before a specified time.
-- * 'maxResults' - The maximum number of jobs to return in the response. The default value is 10.
-- * 'sortBy' - Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
mkListMonitoringSchedules ::
  ListMonitoringSchedules
mkListMonitoringSchedules =
  ListMonitoringSchedules'
    { nameContains = Lude.Nothing,
      endpointName = Lude.Nothing,
      lastModifiedTimeBefore = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      lastModifiedTimeAfter = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      statusEquals = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | Filter for monitoring schedules whose name contains a specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsNameContains :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe Lude.Text)
lmsNameContains = Lens.lens (nameContains :: ListMonitoringSchedules -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | Name of a specific endpoint to fetch schedules for.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsEndpointName :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe Lude.Text)
lmsEndpointName = Lens.lens (endpointName :: ListMonitoringSchedules -> Lude.Maybe Lude.Text) (\s a -> s {endpointName = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | A filter that returns only monitoring schedules modified before a specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsLastModifiedTimeBefore :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe Lude.Timestamp)
lmsLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListMonitoringSchedules -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only monitoring schedules created after a specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsCreationTimeAfter :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe Lude.Timestamp)
lmsCreationTimeAfter = Lens.lens (creationTimeAfter :: ListMonitoringSchedules -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsNextToken :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe Lude.Text)
lmsNextToken = Lens.lens (nextToken :: ListMonitoringSchedules -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsSortOrder :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe SortOrder)
lmsSortOrder = Lens.lens (sortOrder :: ListMonitoringSchedules -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only monitoring schedules modified after a specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsLastModifiedTimeAfter :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe Lude.Timestamp)
lmsLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListMonitoringSchedules -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only monitoring schedules created before a specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsCreationTimeBefore :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe Lude.Timestamp)
lmsCreationTimeBefore = Lens.lens (creationTimeBefore :: ListMonitoringSchedules -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only monitoring schedules modified before a specified time.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsStatusEquals :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe ScheduleStatus)
lmsStatusEquals = Lens.lens (statusEquals :: ListMonitoringSchedules -> Lude.Maybe ScheduleStatus) (\s a -> s {statusEquals = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of jobs to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsMaxResults :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe Lude.Natural)
lmsMaxResults = Lens.lens (maxResults :: ListMonitoringSchedules -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsSortBy :: Lens.Lens' ListMonitoringSchedules (Lude.Maybe MonitoringScheduleSortKey)
lmsSortBy = Lens.lens (sortBy :: ListMonitoringSchedules -> Lude.Maybe MonitoringScheduleSortKey) (\s a -> s {sortBy = a} :: ListMonitoringSchedules)
{-# DEPRECATED lmsSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListMonitoringSchedules where
  page rq rs
    | Page.stop (rs Lens.^. lmsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmsrsMonitoringScheduleSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmsNextToken Lens..~ rs Lens.^. lmsrsNextToken

instance Lude.AWSRequest ListMonitoringSchedules where
  type Rs ListMonitoringSchedules = ListMonitoringSchedulesResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMonitoringSchedulesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "MonitoringScheduleSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMonitoringSchedules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListMonitoringSchedules" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListMonitoringSchedules where
  toJSON ListMonitoringSchedules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("EndpointName" Lude..=) Lude.<$> endpointName,
            ("LastModifiedTimeBefore" Lude..=) Lude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("LastModifiedTimeAfter" Lude..=) Lude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("StatusEquals" Lude..=) Lude.<$> statusEquals,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListMonitoringSchedules where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMonitoringSchedules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListMonitoringSchedulesResponse' smart constructor.
data ListMonitoringSchedulesResponse = ListMonitoringSchedulesResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
    nextToken :: Lude.Maybe Lude.Text,
    -- | A JSON array in which each element is a summary for a monitoring schedule.
    monitoringScheduleSummaries :: [MonitoringScheduleSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMonitoringSchedulesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
-- * 'monitoringScheduleSummaries' - A JSON array in which each element is a summary for a monitoring schedule.
-- * 'responseStatus' - The response status code.
mkListMonitoringSchedulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMonitoringSchedulesResponse
mkListMonitoringSchedulesResponse pResponseStatus_ =
  ListMonitoringSchedulesResponse'
    { nextToken = Lude.Nothing,
      monitoringScheduleSummaries = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsrsNextToken :: Lens.Lens' ListMonitoringSchedulesResponse (Lude.Maybe Lude.Text)
lmsrsNextToken = Lens.lens (nextToken :: ListMonitoringSchedulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMonitoringSchedulesResponse)
{-# DEPRECATED lmsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A JSON array in which each element is a summary for a monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsrsMonitoringScheduleSummaries :: Lens.Lens' ListMonitoringSchedulesResponse [MonitoringScheduleSummary]
lmsrsMonitoringScheduleSummaries = Lens.lens (monitoringScheduleSummaries :: ListMonitoringSchedulesResponse -> [MonitoringScheduleSummary]) (\s a -> s {monitoringScheduleSummaries = a} :: ListMonitoringSchedulesResponse)
{-# DEPRECATED lmsrsMonitoringScheduleSummaries "Use generic-lens or generic-optics with 'monitoringScheduleSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsrsResponseStatus :: Lens.Lens' ListMonitoringSchedulesResponse Lude.Int
lmsrsResponseStatus = Lens.lens (responseStatus :: ListMonitoringSchedulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMonitoringSchedulesResponse)
{-# DEPRECATED lmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
