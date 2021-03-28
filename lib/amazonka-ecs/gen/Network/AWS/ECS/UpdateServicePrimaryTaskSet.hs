{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateServicePrimaryTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies which task set in a service is the primary task set. Any parameters that are updated on the primary task set in a service will transition to the service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.UpdateServicePrimaryTaskSet
    (
    -- * Creating a request
      UpdateServicePrimaryTaskSet (..)
    , mkUpdateServicePrimaryTaskSet
    -- ** Request lenses
    , usptsCluster
    , usptsService
    , usptsPrimaryTaskSet

    -- * Destructuring the response
    , UpdateServicePrimaryTaskSetResponse (..)
    , mkUpdateServicePrimaryTaskSetResponse
    -- ** Response lenses
    , usptsrrsTaskSet
    , usptsrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateServicePrimaryTaskSet' smart constructor.
data UpdateServicePrimaryTaskSet = UpdateServicePrimaryTaskSet'
  { cluster :: Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
  , service :: Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
  , primaryTaskSet :: Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the task set to set as the primary task set in the deployment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServicePrimaryTaskSet' value with any optional fields omitted.
mkUpdateServicePrimaryTaskSet
    :: Core.Text -- ^ 'cluster'
    -> Core.Text -- ^ 'service'
    -> Core.Text -- ^ 'primaryTaskSet'
    -> UpdateServicePrimaryTaskSet
mkUpdateServicePrimaryTaskSet cluster service primaryTaskSet
  = UpdateServicePrimaryTaskSet'{cluster, service, primaryTaskSet}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsCluster :: Lens.Lens' UpdateServicePrimaryTaskSet Core.Text
usptsCluster = Lens.field @"cluster"
{-# INLINEABLE usptsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsService :: Lens.Lens' UpdateServicePrimaryTaskSet Core.Text
usptsService = Lens.field @"service"
{-# INLINEABLE usptsService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the task set to set as the primary task set in the deployment.
--
-- /Note:/ Consider using 'primaryTaskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsPrimaryTaskSet :: Lens.Lens' UpdateServicePrimaryTaskSet Core.Text
usptsPrimaryTaskSet = Lens.field @"primaryTaskSet"
{-# INLINEABLE usptsPrimaryTaskSet #-}
{-# DEPRECATED primaryTaskSet "Use generic-lens or generic-optics with 'primaryTaskSet' instead"  #-}

instance Core.ToQuery UpdateServicePrimaryTaskSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateServicePrimaryTaskSet where
        toHeaders UpdateServicePrimaryTaskSet{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.UpdateServicePrimaryTaskSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateServicePrimaryTaskSet where
        toJSON UpdateServicePrimaryTaskSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("cluster" Core..= cluster),
                  Core.Just ("service" Core..= service),
                  Core.Just ("primaryTaskSet" Core..= primaryTaskSet)])

instance Core.AWSRequest UpdateServicePrimaryTaskSet where
        type Rs UpdateServicePrimaryTaskSet =
             UpdateServicePrimaryTaskSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateServicePrimaryTaskSetResponse' Core.<$>
                   (x Core..:? "taskSet") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateServicePrimaryTaskSetResponse' smart constructor.
data UpdateServicePrimaryTaskSetResponse = UpdateServicePrimaryTaskSetResponse'
  { taskSet :: Core.Maybe Types.TaskSet
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateServicePrimaryTaskSetResponse' value with any optional fields omitted.
mkUpdateServicePrimaryTaskSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateServicePrimaryTaskSetResponse
mkUpdateServicePrimaryTaskSetResponse responseStatus
  = UpdateServicePrimaryTaskSetResponse'{taskSet = Core.Nothing,
                                         responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsrrsTaskSet :: Lens.Lens' UpdateServicePrimaryTaskSetResponse (Core.Maybe Types.TaskSet)
usptsrrsTaskSet = Lens.field @"taskSet"
{-# INLINEABLE usptsrrsTaskSet #-}
{-# DEPRECATED taskSet "Use generic-lens or generic-optics with 'taskSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsrrsResponseStatus :: Lens.Lens' UpdateServicePrimaryTaskSetResponse Core.Int
usptsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usptsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
