{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeregisterTaskDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified task definition by family and revision. Upon deregistration, the task definition is marked as @INACTIVE@ . Existing tasks and services that reference an @INACTIVE@ task definition continue to run without disruption. Existing services that reference an @INACTIVE@ task definition can still scale up or down by modifying the service's desired count.
--
-- You cannot use an @INACTIVE@ task definition to run new tasks or create new services, and you cannot update an existing service to reference an @INACTIVE@ task definition. However, there may be up to a 10-minute window following deregistration where these restrictions have not yet taken effect.
module Network.AWS.ECS.DeregisterTaskDefinition
    (
    -- * Creating a request
      DeregisterTaskDefinition (..)
    , mkDeregisterTaskDefinition
    -- ** Request lenses
    , dtdfTaskDefinition

    -- * Destructuring the response
    , DeregisterTaskDefinitionResponse (..)
    , mkDeregisterTaskDefinitionResponse
    -- ** Response lenses
    , dtdrrsTaskDefinition
    , dtdrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterTaskDefinition' smart constructor.
newtype DeregisterTaskDefinition = DeregisterTaskDefinition'
  { taskDefinition :: Core.Text
    -- ^ The @family@ and @revision@ (@family:revision@ ) or full Amazon Resource Name (ARN) of the task definition to deregister. You must specify a @revision@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTaskDefinition' value with any optional fields omitted.
mkDeregisterTaskDefinition
    :: Core.Text -- ^ 'taskDefinition'
    -> DeregisterTaskDefinition
mkDeregisterTaskDefinition taskDefinition
  = DeregisterTaskDefinition'{taskDefinition}

-- | The @family@ and @revision@ (@family:revision@ ) or full Amazon Resource Name (ARN) of the task definition to deregister. You must specify a @revision@ .
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdfTaskDefinition :: Lens.Lens' DeregisterTaskDefinition Core.Text
dtdfTaskDefinition = Lens.field @"taskDefinition"
{-# INLINEABLE dtdfTaskDefinition #-}
{-# DEPRECATED taskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead"  #-}

instance Core.ToQuery DeregisterTaskDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterTaskDefinition where
        toHeaders DeregisterTaskDefinition{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.DeregisterTaskDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterTaskDefinition where
        toJSON DeregisterTaskDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("taskDefinition" Core..= taskDefinition)])

instance Core.AWSRequest DeregisterTaskDefinition where
        type Rs DeregisterTaskDefinition = DeregisterTaskDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeregisterTaskDefinitionResponse' Core.<$>
                   (x Core..:? "taskDefinition") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterTaskDefinitionResponse' smart constructor.
data DeregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse'
  { taskDefinition :: Core.Maybe Types.TaskDefinition
    -- ^ The full description of the deregistered task.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTaskDefinitionResponse' value with any optional fields omitted.
mkDeregisterTaskDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterTaskDefinitionResponse
mkDeregisterTaskDefinitionResponse responseStatus
  = DeregisterTaskDefinitionResponse'{taskDefinition = Core.Nothing,
                                      responseStatus}

-- | The full description of the deregistered task.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdrrsTaskDefinition :: Lens.Lens' DeregisterTaskDefinitionResponse (Core.Maybe Types.TaskDefinition)
dtdrrsTaskDefinition = Lens.field @"taskDefinition"
{-# INLINEABLE dtdrrsTaskDefinition #-}
{-# DEPRECATED taskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdrrsResponseStatus :: Lens.Lens' DeregisterTaskDefinitionResponse Core.Int
dtdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
