{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutionTaskInvocations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the individual task executions (one per target) for a particular task run as part of a maintenance window execution.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTaskInvocations
  ( -- * Creating a request
    DescribeMaintenanceWindowExecutionTaskInvocations (..),
    mkDescribeMaintenanceWindowExecutionTaskInvocations,

    -- ** Request lenses
    dmwetiTaskId,
    dmwetiFilters,
    dmwetiWindowExecutionId,
    dmwetiNextToken,
    dmwetiMaxResults,

    -- * Destructuring the response
    DescribeMaintenanceWindowExecutionTaskInvocationsResponse (..),
    mkDescribeMaintenanceWindowExecutionTaskInvocationsResponse,

    -- ** Response lenses
    dmwetirsWindowExecutionTaskInvocationIdentities,
    dmwetirsNextToken,
    dmwetirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeMaintenanceWindowExecutionTaskInvocations' smart constructor.
data DescribeMaintenanceWindowExecutionTaskInvocations = DescribeMaintenanceWindowExecutionTaskInvocations'
  { -- | The ID of the specific task in the maintenance window task that should be retrieved.
    taskId :: Lude.Text,
    -- | Optional filters used to scope down the returned task invocations. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
    filters :: Lude.Maybe [MaintenanceWindowFilter],
    -- | The ID of the maintenance window execution the task is part of.
    windowExecutionId :: Lude.Text,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowExecutionTaskInvocations' with the minimum fields required to make a request.
--
-- * 'taskId' - The ID of the specific task in the maintenance window task that should be retrieved.
-- * 'filters' - Optional filters used to scope down the returned task invocations. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
-- * 'windowExecutionId' - The ID of the maintenance window execution the task is part of.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkDescribeMaintenanceWindowExecutionTaskInvocations ::
  -- | 'taskId'
  Lude.Text ->
  -- | 'windowExecutionId'
  Lude.Text ->
  DescribeMaintenanceWindowExecutionTaskInvocations
mkDescribeMaintenanceWindowExecutionTaskInvocations
  pTaskId_
  pWindowExecutionId_ =
    DescribeMaintenanceWindowExecutionTaskInvocations'
      { taskId =
          pTaskId_,
        filters = Lude.Nothing,
        windowExecutionId = pWindowExecutionId_,
        nextToken = Lude.Nothing,
        maxResults = Lude.Nothing
      }

-- | The ID of the specific task in the maintenance window task that should be retrieved.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetiTaskId :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocations Lude.Text
dmwetiTaskId = Lens.lens (taskId :: DescribeMaintenanceWindowExecutionTaskInvocations -> Lude.Text) (\s a -> s {taskId = a} :: DescribeMaintenanceWindowExecutionTaskInvocations)
{-# DEPRECATED dmwetiTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | Optional filters used to scope down the returned task invocations. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetiFilters :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocations (Lude.Maybe [MaintenanceWindowFilter])
dmwetiFilters = Lens.lens (filters :: DescribeMaintenanceWindowExecutionTaskInvocations -> Lude.Maybe [MaintenanceWindowFilter]) (\s a -> s {filters = a} :: DescribeMaintenanceWindowExecutionTaskInvocations)
{-# DEPRECATED dmwetiFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The ID of the maintenance window execution the task is part of.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetiWindowExecutionId :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocations Lude.Text
dmwetiWindowExecutionId = Lens.lens (windowExecutionId :: DescribeMaintenanceWindowExecutionTaskInvocations -> Lude.Text) (\s a -> s {windowExecutionId = a} :: DescribeMaintenanceWindowExecutionTaskInvocations)
{-# DEPRECATED dmwetiWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetiNextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocations (Lude.Maybe Lude.Text)
dmwetiNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowExecutionTaskInvocations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionTaskInvocations)
{-# DEPRECATED dmwetiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetiMaxResults :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocations (Lude.Maybe Lude.Natural)
dmwetiMaxResults = Lens.lens (maxResults :: DescribeMaintenanceWindowExecutionTaskInvocations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeMaintenanceWindowExecutionTaskInvocations)
{-# DEPRECATED dmwetiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance
  Page.AWSPager
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  page rq rs
    | Page.stop (rs Lens.^. dmwetirsNextToken) = Lude.Nothing
    | Page.stop
        (rs Lens.^. dmwetirsWindowExecutionTaskInvocationIdentities) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmwetiNextToken Lens..~ rs Lens.^. dmwetirsNextToken

instance
  Lude.AWSRequest
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  type
    Rs DescribeMaintenanceWindowExecutionTaskInvocations =
      DescribeMaintenanceWindowExecutionTaskInvocationsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowExecutionTaskInvocationsResponse'
            Lude.<$> ( x Lude..?> "WindowExecutionTaskInvocationIdentities"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DescribeMaintenanceWindowExecutionTaskInvocations" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance
  Lude.ToJSON
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  toJSON DescribeMaintenanceWindowExecutionTaskInvocations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TaskId" Lude..= taskId),
            ("Filters" Lude..=) Lude.<$> filters,
            Lude.Just ("WindowExecutionId" Lude..= windowExecutionId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance
  Lude.ToPath
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMaintenanceWindowExecutionTaskInvocationsResponse' smart constructor.
data DescribeMaintenanceWindowExecutionTaskInvocationsResponse = DescribeMaintenanceWindowExecutionTaskInvocationsResponse'
  { -- | Information about the task invocation results per invocation.
    windowExecutionTaskInvocationIdentities :: Lude.Maybe [MaintenanceWindowExecutionTaskInvocationIdentity],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowExecutionTaskInvocationsResponse' with the minimum fields required to make a request.
--
-- * 'windowExecutionTaskInvocationIdentities' - Information about the task invocation results per invocation.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeMaintenanceWindowExecutionTaskInvocationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMaintenanceWindowExecutionTaskInvocationsResponse
mkDescribeMaintenanceWindowExecutionTaskInvocationsResponse
  pResponseStatus_ =
    DescribeMaintenanceWindowExecutionTaskInvocationsResponse'
      { windowExecutionTaskInvocationIdentities =
          Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the task invocation results per invocation.
--
-- /Note:/ Consider using 'windowExecutionTaskInvocationIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetirsWindowExecutionTaskInvocationIdentities :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocationsResponse (Lude.Maybe [MaintenanceWindowExecutionTaskInvocationIdentity])
dmwetirsWindowExecutionTaskInvocationIdentities = Lens.lens (windowExecutionTaskInvocationIdentities :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse -> Lude.Maybe [MaintenanceWindowExecutionTaskInvocationIdentity]) (\s a -> s {windowExecutionTaskInvocationIdentities = a} :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse)
{-# DEPRECATED dmwetirsWindowExecutionTaskInvocationIdentities "Use generic-lens or generic-optics with 'windowExecutionTaskInvocationIdentities' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetirsNextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocationsResponse (Lude.Maybe Lude.Text)
dmwetirsNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse)
{-# DEPRECATED dmwetirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetirsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocationsResponse Lude.Int
dmwetirsResponseStatus = Lens.lens (responseStatus :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse)
{-# DEPRECATED dmwetirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
