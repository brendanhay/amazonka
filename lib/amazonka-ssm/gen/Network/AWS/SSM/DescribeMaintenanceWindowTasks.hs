{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tasks in a maintenance window.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowTasks
  ( -- * Creating a request
    DescribeMaintenanceWindowTasks (..),
    mkDescribeMaintenanceWindowTasks,

    -- ** Request lenses
    dFilters,
    dNextToken,
    dMaxResults,
    dWindowId,

    -- * Destructuring the response
    DescribeMaintenanceWindowTasksResponse (..),
    mkDescribeMaintenanceWindowTasksResponse,

    -- ** Response lenses
    dmwtsrsTasks,
    dmwtsrsNextToken,
    dmwtsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeMaintenanceWindowTasks' smart constructor.
data DescribeMaintenanceWindowTasks = DescribeMaintenanceWindowTasks'
  { -- | Optional filters used to narrow down the scope of the returned tasks. The supported filter keys are WindowTaskId, TaskArn, Priority, and TaskType.
    filters :: Lude.Maybe [MaintenanceWindowFilter],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The ID of the maintenance window whose tasks should be retrieved.
    windowId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowTasks' with the minimum fields required to make a request.
--
-- * 'filters' - Optional filters used to narrow down the scope of the returned tasks. The supported filter keys are WindowTaskId, TaskArn, Priority, and TaskType.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'windowId' - The ID of the maintenance window whose tasks should be retrieved.
mkDescribeMaintenanceWindowTasks ::
  -- | 'windowId'
  Lude.Text ->
  DescribeMaintenanceWindowTasks
mkDescribeMaintenanceWindowTasks pWindowId_ =
  DescribeMaintenanceWindowTasks'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      windowId = pWindowId_
    }

-- | Optional filters used to narrow down the scope of the returned tasks. The supported filter keys are WindowTaskId, TaskArn, Priority, and TaskType.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFilters :: Lens.Lens' DescribeMaintenanceWindowTasks (Lude.Maybe [MaintenanceWindowFilter])
dFilters = Lens.lens (filters :: DescribeMaintenanceWindowTasks -> Lude.Maybe [MaintenanceWindowFilter]) (\s a -> s {filters = a} :: DescribeMaintenanceWindowTasks)
{-# DEPRECATED dFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeMaintenanceWindowTasks (Lude.Maybe Lude.Text)
dNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowTasks)
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeMaintenanceWindowTasks (Lude.Maybe Lude.Natural)
dMaxResults = Lens.lens (maxResults :: DescribeMaintenanceWindowTasks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeMaintenanceWindowTasks)
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the maintenance window whose tasks should be retrieved.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dWindowId :: Lens.Lens' DescribeMaintenanceWindowTasks Lude.Text
dWindowId = Lens.lens (windowId :: DescribeMaintenanceWindowTasks -> Lude.Text) (\s a -> s {windowId = a} :: DescribeMaintenanceWindowTasks)
{-# DEPRECATED dWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Page.AWSPager DescribeMaintenanceWindowTasks where
  page rq rs
    | Page.stop (rs Lens.^. dmwtsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmwtsrsTasks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dNextToken Lens..~ rs Lens.^. dmwtsrsNextToken

instance Lude.AWSRequest DescribeMaintenanceWindowTasks where
  type
    Rs DescribeMaintenanceWindowTasks =
      DescribeMaintenanceWindowTasksResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowTasksResponse'
            Lude.<$> (x Lude..?> "Tasks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMaintenanceWindowTasks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeMaintenanceWindowTasks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMaintenanceWindowTasks where
  toJSON DescribeMaintenanceWindowTasks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("WindowId" Lude..= windowId)
          ]
      )

instance Lude.ToPath DescribeMaintenanceWindowTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMaintenanceWindowTasks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMaintenanceWindowTasksResponse' smart constructor.
data DescribeMaintenanceWindowTasksResponse = DescribeMaintenanceWindowTasksResponse'
  { -- | Information about the tasks in the maintenance window.
    tasks :: Lude.Maybe [MaintenanceWindowTask],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowTasksResponse' with the minimum fields required to make a request.
--
-- * 'tasks' - Information about the tasks in the maintenance window.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeMaintenanceWindowTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMaintenanceWindowTasksResponse
mkDescribeMaintenanceWindowTasksResponse pResponseStatus_ =
  DescribeMaintenanceWindowTasksResponse'
    { tasks = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the tasks in the maintenance window.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtsrsTasks :: Lens.Lens' DescribeMaintenanceWindowTasksResponse (Lude.Maybe [MaintenanceWindowTask])
dmwtsrsTasks = Lens.lens (tasks :: DescribeMaintenanceWindowTasksResponse -> Lude.Maybe [MaintenanceWindowTask]) (\s a -> s {tasks = a} :: DescribeMaintenanceWindowTasksResponse)
{-# DEPRECATED dmwtsrsTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtsrsNextToken :: Lens.Lens' DescribeMaintenanceWindowTasksResponse (Lude.Maybe Lude.Text)
dmwtsrsNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowTasksResponse)
{-# DEPRECATED dmwtsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtsrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowTasksResponse Lude.Int
dmwtsrsResponseStatus = Lens.lens (responseStatus :: DescribeMaintenanceWindowTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMaintenanceWindowTasksResponse)
{-# DEPRECATED dmwtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
