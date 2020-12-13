{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a given maintenance window execution, lists the tasks that were run.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks
  ( -- * Creating a request
    DescribeMaintenanceWindowExecutionTasks (..),
    mkDescribeMaintenanceWindowExecutionTasks,

    -- ** Request lenses
    dmwetFilters,
    dmwetWindowExecutionId,
    dmwetNextToken,
    dmwetMaxResults,

    -- * Destructuring the response
    DescribeMaintenanceWindowExecutionTasksResponse (..),
    mkDescribeMaintenanceWindowExecutionTasksResponse,

    -- ** Response lenses
    dmwetrsNextToken,
    dmwetrsWindowExecutionTaskIdentities,
    dmwetrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeMaintenanceWindowExecutionTasks' smart constructor.
data DescribeMaintenanceWindowExecutionTasks = DescribeMaintenanceWindowExecutionTasks'
  { -- | Optional filters used to scope down the returned tasks. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
    filters :: Lude.Maybe [MaintenanceWindowFilter],
    -- | The ID of the maintenance window execution whose task executions should be retrieved.
    windowExecutionId :: Lude.Text,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowExecutionTasks' with the minimum fields required to make a request.
--
-- * 'filters' - Optional filters used to scope down the returned tasks. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
-- * 'windowExecutionId' - The ID of the maintenance window execution whose task executions should be retrieved.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkDescribeMaintenanceWindowExecutionTasks ::
  -- | 'windowExecutionId'
  Lude.Text ->
  DescribeMaintenanceWindowExecutionTasks
mkDescribeMaintenanceWindowExecutionTasks pWindowExecutionId_ =
  DescribeMaintenanceWindowExecutionTasks'
    { filters = Lude.Nothing,
      windowExecutionId = pWindowExecutionId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Optional filters used to scope down the returned tasks. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetFilters :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Lude.Maybe [MaintenanceWindowFilter])
dmwetFilters = Lens.lens (filters :: DescribeMaintenanceWindowExecutionTasks -> Lude.Maybe [MaintenanceWindowFilter]) (\s a -> s {filters = a} :: DescribeMaintenanceWindowExecutionTasks)
{-# DEPRECATED dmwetFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The ID of the maintenance window execution whose task executions should be retrieved.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetWindowExecutionId :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks Lude.Text
dmwetWindowExecutionId = Lens.lens (windowExecutionId :: DescribeMaintenanceWindowExecutionTasks -> Lude.Text) (\s a -> s {windowExecutionId = a} :: DescribeMaintenanceWindowExecutionTasks)
{-# DEPRECATED dmwetWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetNextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Lude.Maybe Lude.Text)
dmwetNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowExecutionTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionTasks)
{-# DEPRECATED dmwetNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetMaxResults :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Lude.Maybe Lude.Natural)
dmwetMaxResults = Lens.lens (maxResults :: DescribeMaintenanceWindowExecutionTasks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeMaintenanceWindowExecutionTasks)
{-# DEPRECATED dmwetMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeMaintenanceWindowExecutionTasks where
  page rq rs
    | Page.stop (rs Lens.^. dmwetrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmwetrsWindowExecutionTaskIdentities) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmwetNextToken Lens..~ rs Lens.^. dmwetrsNextToken

instance Lude.AWSRequest DescribeMaintenanceWindowExecutionTasks where
  type
    Rs DescribeMaintenanceWindowExecutionTasks =
      DescribeMaintenanceWindowExecutionTasksResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowExecutionTasksResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "WindowExecutionTaskIdentities" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMaintenanceWindowExecutionTasks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DescribeMaintenanceWindowExecutionTasks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMaintenanceWindowExecutionTasks where
  toJSON DescribeMaintenanceWindowExecutionTasks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            Lude.Just ("WindowExecutionId" Lude..= windowExecutionId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeMaintenanceWindowExecutionTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMaintenanceWindowExecutionTasks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMaintenanceWindowExecutionTasksResponse' smart constructor.
data DescribeMaintenanceWindowExecutionTasksResponse = DescribeMaintenanceWindowExecutionTasksResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the task executions.
    windowExecutionTaskIdentities :: Lude.Maybe [MaintenanceWindowExecutionTaskIdentity],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowExecutionTasksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'windowExecutionTaskIdentities' - Information about the task executions.
-- * 'responseStatus' - The response status code.
mkDescribeMaintenanceWindowExecutionTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMaintenanceWindowExecutionTasksResponse
mkDescribeMaintenanceWindowExecutionTasksResponse pResponseStatus_ =
  DescribeMaintenanceWindowExecutionTasksResponse'
    { nextToken =
        Lude.Nothing,
      windowExecutionTaskIdentities = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetrsNextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse (Lude.Maybe Lude.Text)
dmwetrsNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowExecutionTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionTasksResponse)
{-# DEPRECATED dmwetrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the task executions.
--
-- /Note:/ Consider using 'windowExecutionTaskIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetrsWindowExecutionTaskIdentities :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse (Lude.Maybe [MaintenanceWindowExecutionTaskIdentity])
dmwetrsWindowExecutionTaskIdentities = Lens.lens (windowExecutionTaskIdentities :: DescribeMaintenanceWindowExecutionTasksResponse -> Lude.Maybe [MaintenanceWindowExecutionTaskIdentity]) (\s a -> s {windowExecutionTaskIdentities = a} :: DescribeMaintenanceWindowExecutionTasksResponse)
{-# DEPRECATED dmwetrsWindowExecutionTaskIdentities "Use generic-lens or generic-optics with 'windowExecutionTaskIdentities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse Lude.Int
dmwetrsResponseStatus = Lens.lens (responseStatus :: DescribeMaintenanceWindowExecutionTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMaintenanceWindowExecutionTasksResponse)
{-# DEPRECATED dmwetrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
