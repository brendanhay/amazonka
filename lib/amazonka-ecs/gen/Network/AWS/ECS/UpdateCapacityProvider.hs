{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateCapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters for a capacity provider.
module Network.AWS.ECS.UpdateCapacityProvider
    (
    -- * Creating a request
      UpdateCapacityProvider (..)
    , mkUpdateCapacityProvider
    -- ** Request lenses
    , ucpName
    , ucpAutoScalingGroupProvider

    -- * Destructuring the response
    , UpdateCapacityProviderResponse (..)
    , mkUpdateCapacityProviderResponse
    -- ** Response lenses
    , ucprrsCapacityProvider
    , ucprrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCapacityProvider' smart constructor.
data UpdateCapacityProvider = UpdateCapacityProvider'
  { name :: Core.Text
    -- ^ An object representing the parameters to update for the Auto Scaling group capacity provider.
  , autoScalingGroupProvider :: Types.AutoScalingGroupProviderUpdate
    -- ^ The name of the capacity provider to update.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCapacityProvider' value with any optional fields omitted.
mkUpdateCapacityProvider
    :: Core.Text -- ^ 'name'
    -> Types.AutoScalingGroupProviderUpdate -- ^ 'autoScalingGroupProvider'
    -> UpdateCapacityProvider
mkUpdateCapacityProvider name autoScalingGroupProvider
  = UpdateCapacityProvider'{name, autoScalingGroupProvider}

-- | An object representing the parameters to update for the Auto Scaling group capacity provider.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpName :: Lens.Lens' UpdateCapacityProvider Core.Text
ucpName = Lens.field @"name"
{-# INLINEABLE ucpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name of the capacity provider to update.
--
-- /Note:/ Consider using 'autoScalingGroupProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpAutoScalingGroupProvider :: Lens.Lens' UpdateCapacityProvider Types.AutoScalingGroupProviderUpdate
ucpAutoScalingGroupProvider = Lens.field @"autoScalingGroupProvider"
{-# INLINEABLE ucpAutoScalingGroupProvider #-}
{-# DEPRECATED autoScalingGroupProvider "Use generic-lens or generic-optics with 'autoScalingGroupProvider' instead"  #-}

instance Core.ToQuery UpdateCapacityProvider where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateCapacityProvider where
        toHeaders UpdateCapacityProvider{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.UpdateCapacityProvider")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateCapacityProvider where
        toJSON UpdateCapacityProvider{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just
                    ("autoScalingGroupProvider" Core..= autoScalingGroupProvider)])

instance Core.AWSRequest UpdateCapacityProvider where
        type Rs UpdateCapacityProvider = UpdateCapacityProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateCapacityProviderResponse' Core.<$>
                   (x Core..:? "capacityProvider") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateCapacityProviderResponse' smart constructor.
data UpdateCapacityProviderResponse = UpdateCapacityProviderResponse'
  { capacityProvider :: Core.Maybe Types.CapacityProvider
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCapacityProviderResponse' value with any optional fields omitted.
mkUpdateCapacityProviderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateCapacityProviderResponse
mkUpdateCapacityProviderResponse responseStatus
  = UpdateCapacityProviderResponse'{capacityProvider = Core.Nothing,
                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsCapacityProvider :: Lens.Lens' UpdateCapacityProviderResponse (Core.Maybe Types.CapacityProvider)
ucprrsCapacityProvider = Lens.field @"capacityProvider"
{-# INLINEABLE ucprrsCapacityProvider #-}
{-# DEPRECATED capacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsResponseStatus :: Lens.Lens' UpdateCapacityProviderResponse Core.Int
ucprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
