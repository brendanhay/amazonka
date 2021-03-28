{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteCapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified capacity provider.
--
-- Prior to a capacity provider being deleted, the capacity provider must be removed from the capacity provider strategy from all services. The 'UpdateService' API can be used to remove a capacity provider from a service's capacity provider strategy. When updating a service, the @forceNewDeployment@ option can be used to ensure that any tasks using the Amazon EC2 instance capacity provided by the capacity provider are transitioned to use the capacity from the remaining capacity providers. Only capacity providers that are not associated with a cluster can be deleted. To remove a capacity provider from a cluster, you can either use 'PutClusterCapacityProviders' or delete the cluster.
module Network.AWS.ECS.DeleteCapacityProvider
    (
    -- * Creating a request
      DeleteCapacityProvider (..)
    , mkDeleteCapacityProvider
    -- ** Request lenses
    , dcpCapacityProvider

    -- * Destructuring the response
    , DeleteCapacityProviderResponse (..)
    , mkDeleteCapacityProviderResponse
    -- ** Response lenses
    , dcprfrsCapacityProvider
    , dcprfrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCapacityProvider' smart constructor.
newtype DeleteCapacityProvider = DeleteCapacityProvider'
  { capacityProvider :: Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the capacity provider to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCapacityProvider' value with any optional fields omitted.
mkDeleteCapacityProvider
    :: Core.Text -- ^ 'capacityProvider'
    -> DeleteCapacityProvider
mkDeleteCapacityProvider capacityProvider
  = DeleteCapacityProvider'{capacityProvider}

-- | The short name or full Amazon Resource Name (ARN) of the capacity provider to delete.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpCapacityProvider :: Lens.Lens' DeleteCapacityProvider Core.Text
dcpCapacityProvider = Lens.field @"capacityProvider"
{-# INLINEABLE dcpCapacityProvider #-}
{-# DEPRECATED capacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead"  #-}

instance Core.ToQuery DeleteCapacityProvider where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCapacityProvider where
        toHeaders DeleteCapacityProvider{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.DeleteCapacityProvider")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCapacityProvider where
        toJSON DeleteCapacityProvider{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("capacityProvider" Core..= capacityProvider)])

instance Core.AWSRequest DeleteCapacityProvider where
        type Rs DeleteCapacityProvider = DeleteCapacityProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteCapacityProviderResponse' Core.<$>
                   (x Core..:? "capacityProvider") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCapacityProviderResponse' smart constructor.
data DeleteCapacityProviderResponse = DeleteCapacityProviderResponse'
  { capacityProvider :: Core.Maybe Types.CapacityProvider
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCapacityProviderResponse' value with any optional fields omitted.
mkDeleteCapacityProviderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCapacityProviderResponse
mkDeleteCapacityProviderResponse responseStatus
  = DeleteCapacityProviderResponse'{capacityProvider = Core.Nothing,
                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprfrsCapacityProvider :: Lens.Lens' DeleteCapacityProviderResponse (Core.Maybe Types.CapacityProvider)
dcprfrsCapacityProvider = Lens.field @"capacityProvider"
{-# INLINEABLE dcprfrsCapacityProvider #-}
{-# DEPRECATED capacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprfrsResponseStatus :: Lens.Lens' DeleteCapacityProviderResponse Core.Int
dcprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
