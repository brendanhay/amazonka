{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingRegistrationTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List bulk thing provisioning tasks.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingRegistrationTasks
  ( -- * Creating a request
    ListThingRegistrationTasks (..),
    mkListThingRegistrationTasks,

    -- ** Request lenses
    ltrtStatus,
    ltrtNextToken,
    ltrtMaxResults,

    -- * Destructuring the response
    ListThingRegistrationTasksResponse (..),
    mkListThingRegistrationTasksResponse,

    -- ** Response lenses
    ltrtrsNextToken,
    ltrtrsTaskIds,
    ltrtrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListThingRegistrationTasks' smart constructor.
data ListThingRegistrationTasks = ListThingRegistrationTasks'
  { -- | The status of the bulk thing provisioning task.
    status :: Lude.Maybe TaskStatus,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingRegistrationTasks' with the minimum fields required to make a request.
--
-- * 'status' - The status of the bulk thing provisioning task.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'maxResults' - The maximum number of results to return at one time.
mkListThingRegistrationTasks ::
  ListThingRegistrationTasks
mkListThingRegistrationTasks =
  ListThingRegistrationTasks'
    { status = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The status of the bulk thing provisioning task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtStatus :: Lens.Lens' ListThingRegistrationTasks (Lude.Maybe TaskStatus)
ltrtStatus = Lens.lens (status :: ListThingRegistrationTasks -> Lude.Maybe TaskStatus) (\s a -> s {status = a} :: ListThingRegistrationTasks)
{-# DEPRECATED ltrtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtNextToken :: Lens.Lens' ListThingRegistrationTasks (Lude.Maybe Lude.Text)
ltrtNextToken = Lens.lens (nextToken :: ListThingRegistrationTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingRegistrationTasks)
{-# DEPRECATED ltrtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtMaxResults :: Lens.Lens' ListThingRegistrationTasks (Lude.Maybe Lude.Natural)
ltrtMaxResults = Lens.lens (maxResults :: ListThingRegistrationTasks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListThingRegistrationTasks)
{-# DEPRECATED ltrtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListThingRegistrationTasks where
  page rq rs
    | Page.stop (rs Lens.^. ltrtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrtrsTaskIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltrtNextToken Lens..~ rs Lens.^. ltrtrsNextToken

instance Lude.AWSRequest ListThingRegistrationTasks where
  type
    Rs ListThingRegistrationTasks =
      ListThingRegistrationTasksResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListThingRegistrationTasksResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "taskIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListThingRegistrationTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListThingRegistrationTasks where
  toPath = Lude.const "/thing-registration-tasks"

instance Lude.ToQuery ListThingRegistrationTasks where
  toQuery ListThingRegistrationTasks' {..} =
    Lude.mconcat
      [ "status" Lude.=: status,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListThingRegistrationTasksResponse' smart constructor.
data ListThingRegistrationTasksResponse = ListThingRegistrationTasksResponse'
  { -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of bulk thing provisioning task IDs.
    taskIds :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingRegistrationTasksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
-- * 'taskIds' - A list of bulk thing provisioning task IDs.
-- * 'responseStatus' - The response status code.
mkListThingRegistrationTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListThingRegistrationTasksResponse
mkListThingRegistrationTasksResponse pResponseStatus_ =
  ListThingRegistrationTasksResponse'
    { nextToken = Lude.Nothing,
      taskIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrsNextToken :: Lens.Lens' ListThingRegistrationTasksResponse (Lude.Maybe Lude.Text)
ltrtrsNextToken = Lens.lens (nextToken :: ListThingRegistrationTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingRegistrationTasksResponse)
{-# DEPRECATED ltrtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of bulk thing provisioning task IDs.
--
-- /Note:/ Consider using 'taskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrsTaskIds :: Lens.Lens' ListThingRegistrationTasksResponse (Lude.Maybe [Lude.Text])
ltrtrsTaskIds = Lens.lens (taskIds :: ListThingRegistrationTasksResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {taskIds = a} :: ListThingRegistrationTasksResponse)
{-# DEPRECATED ltrtrsTaskIds "Use generic-lens or generic-optics with 'taskIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrsResponseStatus :: Lens.Lens' ListThingRegistrationTasksResponse Lude.Int
ltrtrsResponseStatus = Lens.lens (responseStatus :: ListThingRegistrationTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListThingRegistrationTasksResponse)
{-# DEPRECATED ltrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
