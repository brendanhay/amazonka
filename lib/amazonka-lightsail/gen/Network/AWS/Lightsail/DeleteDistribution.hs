{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail content delivery network (CDN) distribution.
module Network.AWS.Lightsail.DeleteDistribution
    (
    -- * Creating a request
      DeleteDistribution (..)
    , mkDeleteDistribution
    -- ** Request lenses
    , ddDistributionName

    -- * Destructuring the response
    , DeleteDistributionResponse (..)
    , mkDeleteDistributionResponse
    -- ** Response lenses
    , ddrfrsOperation
    , ddrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDistribution' smart constructor.
newtype DeleteDistribution = DeleteDistribution'
  { distributionName :: Core.Maybe Types.ResourceName
    -- ^ The name of the distribution to delete.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDistribution' value with any optional fields omitted.
mkDeleteDistribution
    :: DeleteDistribution
mkDeleteDistribution
  = DeleteDistribution'{distributionName = Core.Nothing}

-- | The name of the distribution to delete.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDistributionName :: Lens.Lens' DeleteDistribution (Core.Maybe Types.ResourceName)
ddDistributionName = Lens.field @"distributionName"
{-# INLINEABLE ddDistributionName #-}
{-# DEPRECATED distributionName "Use generic-lens or generic-optics with 'distributionName' instead"  #-}

instance Core.ToQuery DeleteDistribution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDistribution where
        toHeaders DeleteDistribution{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.DeleteDistribution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDistribution where
        toJSON DeleteDistribution{..}
          = Core.object
              (Core.catMaybes
                 [("distributionName" Core..=) Core.<$> distributionName])

instance Core.AWSRequest DeleteDistribution where
        type Rs DeleteDistribution = DeleteDistributionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteDistributionResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDistributionResponse' smart constructor.
data DeleteDistributionResponse = DeleteDistributionResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteDistributionResponse' value with any optional fields omitted.
mkDeleteDistributionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDistributionResponse
mkDeleteDistributionResponse responseStatus
  = DeleteDistributionResponse'{operation = Core.Nothing,
                                responseStatus}

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsOperation :: Lens.Lens' DeleteDistributionResponse (Core.Maybe Types.Operation)
ddrfrsOperation = Lens.field @"operation"
{-# INLINEABLE ddrfrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsResponseStatus :: Lens.Lens' DeleteDistributionResponse Core.Int
ddrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
