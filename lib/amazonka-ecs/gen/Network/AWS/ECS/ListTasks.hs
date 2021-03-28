{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListTasks (..)
    , mkListTasks
    -- ** Request lenses
    , ltCluster
    , ltContainerInstance
    , ltDesiredStatus
    , ltFamily
    , ltLaunchType
    , ltMaxResults
    , ltNextToken
    , ltServiceName
    , ltStartedBy

    -- * Destructuring the response
    , ListTasksResponse (..)
    , mkListTasksResponse
    -- ** Response lenses
    , ltrrsNextToken
    , ltrrsTaskArns
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTasks' smart constructor.
data ListTasks = ListTasks'
  { cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that hosts the tasks to list. If you do not specify a cluster, the default cluster is assumed.
  , containerInstance :: Core.Maybe Core.Text
    -- ^ The container instance ID or full ARN of the container instance with which to filter the @ListTasks@ results. Specifying a @containerInstance@ limits the results to tasks that belong to that container instance.
  , desiredStatus :: Core.Maybe Types.DesiredStatus
    -- ^ The task desired status with which to filter the @ListTasks@ results. Specifying a @desiredStatus@ of @STOPPED@ limits the results to tasks that Amazon ECS has set the desired status to @STOPPED@ . This can be useful for debugging tasks that are not starting properly or have died or finished. The default status filter is @RUNNING@ , which shows tasks that Amazon ECS has set the desired status to @RUNNING@ .
  , family :: Core.Maybe Core.Text
    -- ^ The name of the family with which to filter the @ListTasks@ results. Specifying a @family@ limits the results to tasks that belong to that family.
  , launchType :: Core.Maybe Types.LaunchType
    -- ^ The launch type for services to list.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of task results returned by @ListTasks@ in paginated output. When this parameter is used, @ListTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTasks@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTasks@ returns up to 100 results and a @nextToken@ value if applicable.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value returned from a @ListTasks@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
  , serviceName :: Core.Maybe Core.Text
    -- ^ The name of the service with which to filter the @ListTasks@ results. Specifying a @serviceName@ limits the results to tasks that belong to that service.
  , startedBy :: Core.Maybe Core.Text
    -- ^ The @startedBy@ value with which to filter the task results. Specifying a @startedBy@ value limits the results to tasks that were started with that value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTasks' value with any optional fields omitted.
mkListTasks
    :: ListTasks
mkListTasks
  = ListTasks'{cluster = Core.Nothing,
               containerInstance = Core.Nothing, desiredStatus = Core.Nothing,
               family = Core.Nothing, launchType = Core.Nothing,
               maxResults = Core.Nothing, nextToken = Core.Nothing,
               serviceName = Core.Nothing, startedBy = Core.Nothing}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the tasks to list. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCluster :: Lens.Lens' ListTasks (Core.Maybe Core.Text)
ltCluster = Lens.field @"cluster"
{-# INLINEABLE ltCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The container instance ID or full ARN of the container instance with which to filter the @ListTasks@ results. Specifying a @containerInstance@ limits the results to tasks that belong to that container instance.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltContainerInstance :: Lens.Lens' ListTasks (Core.Maybe Core.Text)
ltContainerInstance = Lens.field @"containerInstance"
{-# INLINEABLE ltContainerInstance #-}
{-# DEPRECATED containerInstance "Use generic-lens or generic-optics with 'containerInstance' instead"  #-}

-- | The task desired status with which to filter the @ListTasks@ results. Specifying a @desiredStatus@ of @STOPPED@ limits the results to tasks that Amazon ECS has set the desired status to @STOPPED@ . This can be useful for debugging tasks that are not starting properly or have died or finished. The default status filter is @RUNNING@ , which shows tasks that Amazon ECS has set the desired status to @RUNNING@ .
--
-- /Note:/ Consider using 'desiredStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDesiredStatus :: Lens.Lens' ListTasks (Core.Maybe Types.DesiredStatus)
ltDesiredStatus = Lens.field @"desiredStatus"
{-# INLINEABLE ltDesiredStatus #-}
{-# DEPRECATED desiredStatus "Use generic-lens or generic-optics with 'desiredStatus' instead"  #-}

-- | The name of the family with which to filter the @ListTasks@ results. Specifying a @family@ limits the results to tasks that belong to that family.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltFamily :: Lens.Lens' ListTasks (Core.Maybe Core.Text)
ltFamily = Lens.field @"family"
{-# INLINEABLE ltFamily #-}
{-# DEPRECATED family "Use generic-lens or generic-optics with 'family' instead"  #-}

-- | The launch type for services to list.
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLaunchType :: Lens.Lens' ListTasks (Core.Maybe Types.LaunchType)
ltLaunchType = Lens.field @"launchType"
{-# INLINEABLE ltLaunchType #-}
{-# DEPRECATED launchType "Use generic-lens or generic-optics with 'launchType' instead"  #-}

-- | The maximum number of task results returned by @ListTasks@ in paginated output. When this parameter is used, @ListTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTasks@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTasks@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTasks (Core.Maybe Core.Int)
ltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @nextToken@ value returned from a @ListTasks@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTasks (Core.Maybe Core.Text)
ltNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The name of the service with which to filter the @ListTasks@ results. Specifying a @serviceName@ limits the results to tasks that belong to that service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltServiceName :: Lens.Lens' ListTasks (Core.Maybe Core.Text)
ltServiceName = Lens.field @"serviceName"
{-# INLINEABLE ltServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The @startedBy@ value with which to filter the task results. Specifying a @startedBy@ value limits the results to tasks that were started with that value.
--
-- /Note:/ Consider using 'startedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltStartedBy :: Lens.Lens' ListTasks (Core.Maybe Core.Text)
ltStartedBy = Lens.field @"startedBy"
{-# INLINEABLE ltStartedBy #-}
{-# DEPRECATED startedBy "Use generic-lens or generic-optics with 'startedBy' instead"  #-}

instance Core.ToQuery ListTasks where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTasks where
        toHeaders ListTasks{..}
          = Core.pure
              ("X-Amz-Target", "AmazonEC2ContainerServiceV20141113.ListTasks")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTasks where
        toJSON ListTasks{..}
          = Core.object
              (Core.catMaybes
                 [("cluster" Core..=) Core.<$> cluster,
                  ("containerInstance" Core..=) Core.<$> containerInstance,
                  ("desiredStatus" Core..=) Core.<$> desiredStatus,
                  ("family" Core..=) Core.<$> family,
                  ("launchType" Core..=) Core.<$> launchType,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("serviceName" Core..=) Core.<$> serviceName,
                  ("startedBy" Core..=) Core.<$> startedBy])

instance Core.AWSRequest ListTasks where
        type Rs ListTasks = ListTasksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTasksResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "taskArns" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTasks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"taskArns" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTasksResponse' smart constructor.
data ListTasksResponse = ListTasksResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value to include in a future @ListTasks@ request. When the results of a @ListTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , taskArns :: Core.Maybe [Core.Text]
    -- ^ The list of task ARN entries for the @ListTasks@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTasksResponse' value with any optional fields omitted.
mkListTasksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTasksResponse
mkListTasksResponse responseStatus
  = ListTasksResponse'{nextToken = Core.Nothing,
                       taskArns = Core.Nothing, responseStatus}

-- | The @nextToken@ value to include in a future @ListTasks@ request. When the results of a @ListTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTasksResponse (Core.Maybe Core.Text)
ltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of task ARN entries for the @ListTasks@ request.
--
-- /Note:/ Consider using 'taskArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTaskArns :: Lens.Lens' ListTasksResponse (Core.Maybe [Core.Text])
ltrrsTaskArns = Lens.field @"taskArns"
{-# INLINEABLE ltrrsTaskArns #-}
{-# DEPRECATED taskArns "Use generic-lens or generic-optics with 'taskArns' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTasksResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
