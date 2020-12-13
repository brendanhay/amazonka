{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about upcoming executions of a maintenance window.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowSchedule
  ( -- * Creating a request
    DescribeMaintenanceWindowSchedule (..),
    mkDescribeMaintenanceWindowSchedule,

    -- ** Request lenses
    dmwsResourceType,
    dmwsFilters,
    dmwsNextToken,
    dmwsTargets,
    dmwsMaxResults,
    dmwsWindowId,

    -- * Destructuring the response
    DescribeMaintenanceWindowScheduleResponse (..),
    mkDescribeMaintenanceWindowScheduleResponse,

    -- ** Response lenses
    dmwsrsScheduledWindowExecutions,
    dmwsrsNextToken,
    dmwsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeMaintenanceWindowSchedule' smart constructor.
data DescribeMaintenanceWindowSchedule = DescribeMaintenanceWindowSchedule'
  { -- | The type of resource you want to retrieve information about. For example, "INSTANCE".
    resourceType :: Lude.Maybe MaintenanceWindowResourceType,
    -- | Filters used to limit the range of results. For example, you can limit maintenance window executions to only those scheduled before or after a certain date and time.
    filters :: Lude.Maybe [PatchOrchestratorFilter],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The instance ID or key/value pair to retrieve information about.
    targets :: Lude.Maybe [Target],
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The ID of the maintenance window to retrieve information about.
    windowId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowSchedule' with the minimum fields required to make a request.
--
-- * 'resourceType' - The type of resource you want to retrieve information about. For example, "INSTANCE".
-- * 'filters' - Filters used to limit the range of results. For example, you can limit maintenance window executions to only those scheduled before or after a certain date and time.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'targets' - The instance ID or key/value pair to retrieve information about.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'windowId' - The ID of the maintenance window to retrieve information about.
mkDescribeMaintenanceWindowSchedule ::
  DescribeMaintenanceWindowSchedule
mkDescribeMaintenanceWindowSchedule =
  DescribeMaintenanceWindowSchedule'
    { resourceType = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      targets = Lude.Nothing,
      maxResults = Lude.Nothing,
      windowId = Lude.Nothing
    }

-- | The type of resource you want to retrieve information about. For example, "INSTANCE".
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsResourceType :: Lens.Lens' DescribeMaintenanceWindowSchedule (Lude.Maybe MaintenanceWindowResourceType)
dmwsResourceType = Lens.lens (resourceType :: DescribeMaintenanceWindowSchedule -> Lude.Maybe MaintenanceWindowResourceType) (\s a -> s {resourceType = a} :: DescribeMaintenanceWindowSchedule)
{-# DEPRECATED dmwsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Filters used to limit the range of results. For example, you can limit maintenance window executions to only those scheduled before or after a certain date and time.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsFilters :: Lens.Lens' DescribeMaintenanceWindowSchedule (Lude.Maybe [PatchOrchestratorFilter])
dmwsFilters = Lens.lens (filters :: DescribeMaintenanceWindowSchedule -> Lude.Maybe [PatchOrchestratorFilter]) (\s a -> s {filters = a} :: DescribeMaintenanceWindowSchedule)
{-# DEPRECATED dmwsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsNextToken :: Lens.Lens' DescribeMaintenanceWindowSchedule (Lude.Maybe Lude.Text)
dmwsNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowSchedule -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowSchedule)
{-# DEPRECATED dmwsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The instance ID or key/value pair to retrieve information about.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsTargets :: Lens.Lens' DescribeMaintenanceWindowSchedule (Lude.Maybe [Target])
dmwsTargets = Lens.lens (targets :: DescribeMaintenanceWindowSchedule -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: DescribeMaintenanceWindowSchedule)
{-# DEPRECATED dmwsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsMaxResults :: Lens.Lens' DescribeMaintenanceWindowSchedule (Lude.Maybe Lude.Natural)
dmwsMaxResults = Lens.lens (maxResults :: DescribeMaintenanceWindowSchedule -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeMaintenanceWindowSchedule)
{-# DEPRECATED dmwsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the maintenance window to retrieve information about.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsWindowId :: Lens.Lens' DescribeMaintenanceWindowSchedule (Lude.Maybe Lude.Text)
dmwsWindowId = Lens.lens (windowId :: DescribeMaintenanceWindowSchedule -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: DescribeMaintenanceWindowSchedule)
{-# DEPRECATED dmwsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Page.AWSPager DescribeMaintenanceWindowSchedule where
  page rq rs
    | Page.stop (rs Lens.^. dmwsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmwsrsScheduledWindowExecutions) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmwsNextToken Lens..~ rs Lens.^. dmwsrsNextToken

instance Lude.AWSRequest DescribeMaintenanceWindowSchedule where
  type
    Rs DescribeMaintenanceWindowSchedule =
      DescribeMaintenanceWindowScheduleResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowScheduleResponse'
            Lude.<$> (x Lude..?> "ScheduledWindowExecutions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMaintenanceWindowSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeMaintenanceWindowSchedule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMaintenanceWindowSchedule where
  toJSON DescribeMaintenanceWindowSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceType" Lude..=) Lude.<$> resourceType,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Targets" Lude..=) Lude.<$> targets,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("WindowId" Lude..=) Lude.<$> windowId
          ]
      )

instance Lude.ToPath DescribeMaintenanceWindowSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMaintenanceWindowSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMaintenanceWindowScheduleResponse' smart constructor.
data DescribeMaintenanceWindowScheduleResponse = DescribeMaintenanceWindowScheduleResponse'
  { -- | Information about maintenance window executions scheduled for the specified time range.
    scheduledWindowExecutions :: Lude.Maybe [ScheduledWindowExecution],
    -- | The token for the next set of items to return. (You use this token in the next call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowScheduleResponse' with the minimum fields required to make a request.
--
-- * 'scheduledWindowExecutions' - Information about maintenance window executions scheduled for the specified time range.
-- * 'nextToken' - The token for the next set of items to return. (You use this token in the next call.)
-- * 'responseStatus' - The response status code.
mkDescribeMaintenanceWindowScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMaintenanceWindowScheduleResponse
mkDescribeMaintenanceWindowScheduleResponse pResponseStatus_ =
  DescribeMaintenanceWindowScheduleResponse'
    { scheduledWindowExecutions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about maintenance window executions scheduled for the specified time range.
--
-- /Note:/ Consider using 'scheduledWindowExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsrsScheduledWindowExecutions :: Lens.Lens' DescribeMaintenanceWindowScheduleResponse (Lude.Maybe [ScheduledWindowExecution])
dmwsrsScheduledWindowExecutions = Lens.lens (scheduledWindowExecutions :: DescribeMaintenanceWindowScheduleResponse -> Lude.Maybe [ScheduledWindowExecution]) (\s a -> s {scheduledWindowExecutions = a} :: DescribeMaintenanceWindowScheduleResponse)
{-# DEPRECATED dmwsrsScheduledWindowExecutions "Use generic-lens or generic-optics with 'scheduledWindowExecutions' instead." #-}

-- | The token for the next set of items to return. (You use this token in the next call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsrsNextToken :: Lens.Lens' DescribeMaintenanceWindowScheduleResponse (Lude.Maybe Lude.Text)
dmwsrsNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowScheduleResponse)
{-# DEPRECATED dmwsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowScheduleResponse Lude.Int
dmwsrsResponseStatus = Lens.lens (responseStatus :: DescribeMaintenanceWindowScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMaintenanceWindowScheduleResponse)
{-# DEPRECATED dmwsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
