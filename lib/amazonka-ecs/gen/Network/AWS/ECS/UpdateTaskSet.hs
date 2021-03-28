{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task set. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.UpdateTaskSet
    (
    -- * Creating a request
      UpdateTaskSet (..)
    , mkUpdateTaskSet
    -- ** Request lenses
    , utsCluster
    , utsService
    , utsTaskSet
    , utsScale

    -- * Destructuring the response
    , UpdateTaskSetResponse (..)
    , mkUpdateTaskSetResponse
    -- ** Response lenses
    , utsrrsTaskSet
    , utsrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTaskSet' smart constructor.
data UpdateTaskSet = UpdateTaskSet'
  { cluster :: Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
  , service :: Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
  , taskSet :: Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the task set to update.
  , scale :: Types.Scale
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTaskSet' value with any optional fields omitted.
mkUpdateTaskSet
    :: Core.Text -- ^ 'cluster'
    -> Core.Text -- ^ 'service'
    -> Core.Text -- ^ 'taskSet'
    -> Types.Scale -- ^ 'scale'
    -> UpdateTaskSet
mkUpdateTaskSet cluster service taskSet scale
  = UpdateTaskSet'{cluster, service, taskSet, scale}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsCluster :: Lens.Lens' UpdateTaskSet Core.Text
utsCluster = Lens.field @"cluster"
{-# INLINEABLE utsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsService :: Lens.Lens' UpdateTaskSet Core.Text
utsService = Lens.field @"service"
{-# INLINEABLE utsService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the task set to update.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsTaskSet :: Lens.Lens' UpdateTaskSet Core.Text
utsTaskSet = Lens.field @"taskSet"
{-# INLINEABLE utsTaskSet #-}
{-# DEPRECATED taskSet "Use generic-lens or generic-optics with 'taskSet' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsScale :: Lens.Lens' UpdateTaskSet Types.Scale
utsScale = Lens.field @"scale"
{-# INLINEABLE utsScale #-}
{-# DEPRECATED scale "Use generic-lens or generic-optics with 'scale' instead"  #-}

instance Core.ToQuery UpdateTaskSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTaskSet where
        toHeaders UpdateTaskSet{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.UpdateTaskSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateTaskSet where
        toJSON UpdateTaskSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("cluster" Core..= cluster),
                  Core.Just ("service" Core..= service),
                  Core.Just ("taskSet" Core..= taskSet),
                  Core.Just ("scale" Core..= scale)])

instance Core.AWSRequest UpdateTaskSet where
        type Rs UpdateTaskSet = UpdateTaskSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateTaskSetResponse' Core.<$>
                   (x Core..:? "taskSet") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTaskSetResponse' smart constructor.
data UpdateTaskSetResponse = UpdateTaskSetResponse'
  { taskSet :: Core.Maybe Types.TaskSet
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateTaskSetResponse' value with any optional fields omitted.
mkUpdateTaskSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTaskSetResponse
mkUpdateTaskSetResponse responseStatus
  = UpdateTaskSetResponse'{taskSet = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrrsTaskSet :: Lens.Lens' UpdateTaskSetResponse (Core.Maybe Types.TaskSet)
utsrrsTaskSet = Lens.field @"taskSet"
{-# INLINEABLE utsrrsTaskSet #-}
{-# DEPRECATED taskSet "Use generic-lens or generic-optics with 'taskSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrrsResponseStatus :: Lens.Lens' UpdateTaskSetResponse Core.Int
utsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
