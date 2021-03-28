{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specified task or tasks.
module Network.AWS.ECS.DescribeTasks
    (
    -- * Creating a request
      DescribeTasks (..)
    , mkDescribeTasks
    -- ** Request lenses
    , dtTasks
    , dtCluster
    , dtInclude

    -- * Destructuring the response
    , DescribeTasksResponse (..)
    , mkDescribeTasksResponse
    -- ** Response lenses
    , dtrrsFailures
    , dtrrsTasks
    , dtrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTasks' smart constructor.
data DescribeTasks = DescribeTasks'
  { tasks :: [Core.Text]
    -- ^ A list of up to 100 task IDs or full ARN entries.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task or tasks to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the task or tasks you are describing were launched in any cluster other than the default cluster.
  , include :: Core.Maybe [Types.TaskField]
    -- ^ Specifies whether you want to see the resource tags for the task. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTasks' value with any optional fields omitted.
mkDescribeTasks
    :: DescribeTasks
mkDescribeTasks
  = DescribeTasks'{tasks = Core.mempty, cluster = Core.Nothing,
                   include = Core.Nothing}

-- | A list of up to 100 task IDs or full ARN entries.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTasks :: Lens.Lens' DescribeTasks [Core.Text]
dtTasks = Lens.field @"tasks"
{-# INLINEABLE dtTasks #-}
{-# DEPRECATED tasks "Use generic-lens or generic-optics with 'tasks' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task or tasks to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the task or tasks you are describing were launched in any cluster other than the default cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtCluster :: Lens.Lens' DescribeTasks (Core.Maybe Core.Text)
dtCluster = Lens.field @"cluster"
{-# INLINEABLE dtCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | Specifies whether you want to see the resource tags for the task. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtInclude :: Lens.Lens' DescribeTasks (Core.Maybe [Types.TaskField])
dtInclude = Lens.field @"include"
{-# INLINEABLE dtInclude #-}
{-# DEPRECATED include "Use generic-lens or generic-optics with 'include' instead"  #-}

instance Core.ToQuery DescribeTasks where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTasks where
        toHeaders DescribeTasks{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.DescribeTasks")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTasks where
        toJSON DescribeTasks{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("tasks" Core..= tasks),
                  ("cluster" Core..=) Core.<$> cluster,
                  ("include" Core..=) Core.<$> include])

instance Core.AWSRequest DescribeTasks where
        type Rs DescribeTasks = DescribeTasksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTasksResponse' Core.<$>
                   (x Core..:? "failures") Core.<*> x Core..:? "tasks" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeTasksResponse' smart constructor.
data DescribeTasksResponse = DescribeTasksResponse'
  { failures :: Core.Maybe [Types.Failure]
    -- ^ Any failures associated with the call.
  , tasks :: Core.Maybe [Types.Task]
    -- ^ The list of tasks.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTasksResponse' value with any optional fields omitted.
mkDescribeTasksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTasksResponse
mkDescribeTasksResponse responseStatus
  = DescribeTasksResponse'{failures = Core.Nothing,
                           tasks = Core.Nothing, responseStatus}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsFailures :: Lens.Lens' DescribeTasksResponse (Core.Maybe [Types.Failure])
dtrrsFailures = Lens.field @"failures"
{-# INLINEABLE dtrrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The list of tasks.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTasks :: Lens.Lens' DescribeTasksResponse (Core.Maybe [Types.Task])
dtrrsTasks = Lens.field @"tasks"
{-# INLINEABLE dtrrsTasks #-}
{-# DEPRECATED tasks "Use generic-lens or generic-optics with 'tasks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTasksResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
