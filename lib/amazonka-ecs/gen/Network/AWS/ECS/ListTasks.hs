{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tasks for a specified cluster. You can filter the results by family name, by a particular container instance, or by the desired status of the task with the @family@ , @containerInstance@ , and @desiredStatus@ parameters.
--
-- Recently stopped tasks might appear in the returned results. Currently, stopped tasks appear in the returned results for at least one hour.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListTasks
  ( -- * Creating a request
    ListTasks (..),
    mkListTasks,

    -- ** Request lenses
    ltDesiredStatus,
    ltCluster,
    ltFamily,
    ltNextToken,
    ltStartedBy,
    ltServiceName,
    ltLaunchType,
    ltContainerInstance,
    ltMaxResults,

    -- * Destructuring the response
    ListTasksResponse (..),
    mkListTasksResponse,

    -- ** Response lenses
    ltrsNextToken,
    ltrsTaskARNs,
    ltrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTasks' smart constructor.
data ListTasks = ListTasks'
  { -- | The task desired status with which to filter the @ListTasks@ results. Specifying a @desiredStatus@ of @STOPPED@ limits the results to tasks that Amazon ECS has set the desired status to @STOPPED@ . This can be useful for debugging tasks that are not starting properly or have died or finished. The default status filter is @RUNNING@ , which shows tasks that Amazon ECS has set the desired status to @RUNNING@ .
    desiredStatus :: Lude.Maybe DesiredStatus,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the tasks to list. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Lude.Maybe Lude.Text,
    -- | The name of the family with which to filter the @ListTasks@ results. Specifying a @family@ limits the results to tasks that belong to that family.
    family :: Lude.Maybe Lude.Text,
    -- | The @nextToken@ value returned from a @ListTasks@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The @startedBy@ value with which to filter the task results. Specifying a @startedBy@ value limits the results to tasks that were started with that value.
    startedBy :: Lude.Maybe Lude.Text,
    -- | The name of the service with which to filter the @ListTasks@ results. Specifying a @serviceName@ limits the results to tasks that belong to that service.
    serviceName :: Lude.Maybe Lude.Text,
    -- | The launch type for services to list.
    launchType :: Lude.Maybe LaunchType,
    -- | The container instance ID or full ARN of the container instance with which to filter the @ListTasks@ results. Specifying a @containerInstance@ limits the results to tasks that belong to that container instance.
    containerInstance :: Lude.Maybe Lude.Text,
    -- | The maximum number of task results returned by @ListTasks@ in paginated output. When this parameter is used, @ListTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTasks@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTasks@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTasks' with the minimum fields required to make a request.
--
-- * 'desiredStatus' - The task desired status with which to filter the @ListTasks@ results. Specifying a @desiredStatus@ of @STOPPED@ limits the results to tasks that Amazon ECS has set the desired status to @STOPPED@ . This can be useful for debugging tasks that are not starting properly or have died or finished. The default status filter is @RUNNING@ , which shows tasks that Amazon ECS has set the desired status to @RUNNING@ .
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the tasks to list. If you do not specify a cluster, the default cluster is assumed.
-- * 'family' - The name of the family with which to filter the @ListTasks@ results. Specifying a @family@ limits the results to tasks that belong to that family.
-- * 'nextToken' - The @nextToken@ value returned from a @ListTasks@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
-- * 'startedBy' - The @startedBy@ value with which to filter the task results. Specifying a @startedBy@ value limits the results to tasks that were started with that value.
-- * 'serviceName' - The name of the service with which to filter the @ListTasks@ results. Specifying a @serviceName@ limits the results to tasks that belong to that service.
-- * 'launchType' - The launch type for services to list.
-- * 'containerInstance' - The container instance ID or full ARN of the container instance with which to filter the @ListTasks@ results. Specifying a @containerInstance@ limits the results to tasks that belong to that container instance.
-- * 'maxResults' - The maximum number of task results returned by @ListTasks@ in paginated output. When this parameter is used, @ListTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTasks@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTasks@ returns up to 100 results and a @nextToken@ value if applicable.
mkListTasks ::
  ListTasks
mkListTasks =
  ListTasks'
    { desiredStatus = Lude.Nothing,
      cluster = Lude.Nothing,
      family = Lude.Nothing,
      nextToken = Lude.Nothing,
      startedBy = Lude.Nothing,
      serviceName = Lude.Nothing,
      launchType = Lude.Nothing,
      containerInstance = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The task desired status with which to filter the @ListTasks@ results. Specifying a @desiredStatus@ of @STOPPED@ limits the results to tasks that Amazon ECS has set the desired status to @STOPPED@ . This can be useful for debugging tasks that are not starting properly or have died or finished. The default status filter is @RUNNING@ , which shows tasks that Amazon ECS has set the desired status to @RUNNING@ .
--
-- /Note:/ Consider using 'desiredStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDesiredStatus :: Lens.Lens' ListTasks (Lude.Maybe DesiredStatus)
ltDesiredStatus = Lens.lens (desiredStatus :: ListTasks -> Lude.Maybe DesiredStatus) (\s a -> s {desiredStatus = a} :: ListTasks)
{-# DEPRECATED ltDesiredStatus "Use generic-lens or generic-optics with 'desiredStatus' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the tasks to list. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCluster :: Lens.Lens' ListTasks (Lude.Maybe Lude.Text)
ltCluster = Lens.lens (cluster :: ListTasks -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: ListTasks)
{-# DEPRECATED ltCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The name of the family with which to filter the @ListTasks@ results. Specifying a @family@ limits the results to tasks that belong to that family.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltFamily :: Lens.Lens' ListTasks (Lude.Maybe Lude.Text)
ltFamily = Lens.lens (family :: ListTasks -> Lude.Maybe Lude.Text) (\s a -> s {family = a} :: ListTasks)
{-# DEPRECATED ltFamily "Use generic-lens or generic-optics with 'family' instead." #-}

-- | The @nextToken@ value returned from a @ListTasks@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTasks (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTasks)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The @startedBy@ value with which to filter the task results. Specifying a @startedBy@ value limits the results to tasks that were started with that value.
--
-- /Note:/ Consider using 'startedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltStartedBy :: Lens.Lens' ListTasks (Lude.Maybe Lude.Text)
ltStartedBy = Lens.lens (startedBy :: ListTasks -> Lude.Maybe Lude.Text) (\s a -> s {startedBy = a} :: ListTasks)
{-# DEPRECATED ltStartedBy "Use generic-lens or generic-optics with 'startedBy' instead." #-}

-- | The name of the service with which to filter the @ListTasks@ results. Specifying a @serviceName@ limits the results to tasks that belong to that service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltServiceName :: Lens.Lens' ListTasks (Lude.Maybe Lude.Text)
ltServiceName = Lens.lens (serviceName :: ListTasks -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: ListTasks)
{-# DEPRECATED ltServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The launch type for services to list.
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLaunchType :: Lens.Lens' ListTasks (Lude.Maybe LaunchType)
ltLaunchType = Lens.lens (launchType :: ListTasks -> Lude.Maybe LaunchType) (\s a -> s {launchType = a} :: ListTasks)
{-# DEPRECATED ltLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | The container instance ID or full ARN of the container instance with which to filter the @ListTasks@ results. Specifying a @containerInstance@ limits the results to tasks that belong to that container instance.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltContainerInstance :: Lens.Lens' ListTasks (Lude.Maybe Lude.Text)
ltContainerInstance = Lens.lens (containerInstance :: ListTasks -> Lude.Maybe Lude.Text) (\s a -> s {containerInstance = a} :: ListTasks)
{-# DEPRECATED ltContainerInstance "Use generic-lens or generic-optics with 'containerInstance' instead." #-}

-- | The maximum number of task results returned by @ListTasks@ in paginated output. When this parameter is used, @ListTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTasks@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTasks@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTasks (Lude.Maybe Lude.Int)
ltMaxResults = Lens.lens (maxResults :: ListTasks -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListTasks)
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTasks where
  page rq rs
    | Page.stop (rs Lens.^. ltrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTaskARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltNextToken Lens..~ rs Lens.^. ltrsNextToken

instance Lude.AWSRequest ListTasks where
  type Rs ListTasks = ListTasksResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTasksResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "taskArns" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTasks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.ListTasks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTasks where
  toJSON ListTasks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("desiredStatus" Lude..=) Lude.<$> desiredStatus,
            ("cluster" Lude..=) Lude.<$> cluster,
            ("family" Lude..=) Lude.<$> family,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("startedBy" Lude..=) Lude.<$> startedBy,
            ("serviceName" Lude..=) Lude.<$> serviceName,
            ("launchType" Lude..=) Lude.<$> launchType,
            ("containerInstance" Lude..=) Lude.<$> containerInstance,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTasks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTasksResponse' smart constructor.
data ListTasksResponse = ListTasksResponse'
  { -- | The @nextToken@ value to include in a future @ListTasks@ request. When the results of a @ListTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of task ARN entries for the @ListTasks@ request.
    taskARNs :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTasksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ value to include in a future @ListTasks@ request. When the results of a @ListTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'taskARNs' - The list of task ARN entries for the @ListTasks@ request.
-- * 'responseStatus' - The response status code.
mkListTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTasksResponse
mkListTasksResponse pResponseStatus_ =
  ListTasksResponse'
    { nextToken = Lude.Nothing,
      taskARNs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ value to include in a future @ListTasks@ request. When the results of a @ListTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTasksResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTasksResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of task ARN entries for the @ListTasks@ request.
--
-- /Note:/ Consider using 'taskARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTaskARNs :: Lens.Lens' ListTasksResponse (Lude.Maybe [Lude.Text])
ltrsTaskARNs = Lens.lens (taskARNs :: ListTasksResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {taskARNs = a} :: ListTasksResponse)
{-# DEPRECATED ltrsTaskARNs "Use generic-lens or generic-optics with 'taskARNs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTasksResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTasksResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
