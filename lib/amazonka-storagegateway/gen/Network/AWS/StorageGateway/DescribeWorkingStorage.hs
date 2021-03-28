{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeWorkingStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the working storage of a gateway. This operation is only supported in the stored volumes gateway type. This operation is deprecated in cached volumes API version (20120630). Use DescribeUploadBuffer instead.
--
-- The response includes disk IDs that are configured as working storage, and it includes the amount of working storage allocated and used.
module Network.AWS.StorageGateway.DescribeWorkingStorage
    (
    -- * Creating a request
      DescribeWorkingStorage (..)
    , mkDescribeWorkingStorage
    -- ** Request lenses
    , dwsGatewayARN

    -- * Destructuring the response
    , DescribeWorkingStorageResponse (..)
    , mkDescribeWorkingStorageResponse
    -- ** Response lenses
    , dwsrrsDiskIds
    , dwsrrsGatewayARN
    , dwsrrsWorkingStorageAllocatedInBytes
    , dwsrrsWorkingStorageUsedInBytes
    , dwsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'mkDescribeWorkingStorage' smart constructor.
newtype DescribeWorkingStorage = DescribeWorkingStorage'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkingStorage' value with any optional fields omitted.
mkDescribeWorkingStorage
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> DescribeWorkingStorage
mkDescribeWorkingStorage gatewayARN
  = DescribeWorkingStorage'{gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsGatewayARN :: Lens.Lens' DescribeWorkingStorage Types.GatewayARN
dwsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE dwsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

instance Core.ToQuery DescribeWorkingStorage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeWorkingStorage where
        toHeaders DescribeWorkingStorage{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DescribeWorkingStorage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeWorkingStorage where
        toJSON DescribeWorkingStorage{..}
          = Core.object
              (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DescribeWorkingStorage where
        type Rs DescribeWorkingStorage = DescribeWorkingStorageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeWorkingStorageResponse' Core.<$>
                   (x Core..:? "DiskIds") Core.<*> x Core..:? "GatewayARN" Core.<*>
                     x Core..:? "WorkingStorageAllocatedInBytes"
                     Core.<*> x Core..:? "WorkingStorageUsedInBytes"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDescribeWorkingStorageResponse' smart constructor.
data DescribeWorkingStorageResponse = DescribeWorkingStorageResponse'
  { diskIds :: Core.Maybe [Types.DiskId]
    -- ^ An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
  , gatewayARN :: Core.Maybe Types.GatewayARN
  , workingStorageAllocatedInBytes :: Core.Maybe Core.Integer
    -- ^ The total working storage in bytes allocated for the gateway. If no working storage is configured for the gateway, this field returns 0.
  , workingStorageUsedInBytes :: Core.Maybe Core.Integer
    -- ^ The total working storage in bytes in use by the gateway. If no working storage is configured for the gateway, this field returns 0.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkingStorageResponse' value with any optional fields omitted.
mkDescribeWorkingStorageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeWorkingStorageResponse
mkDescribeWorkingStorageResponse responseStatus
  = DescribeWorkingStorageResponse'{diskIds = Core.Nothing,
                                    gatewayARN = Core.Nothing,
                                    workingStorageAllocatedInBytes = Core.Nothing,
                                    workingStorageUsedInBytes = Core.Nothing, responseStatus}

-- | An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsDiskIds :: Lens.Lens' DescribeWorkingStorageResponse (Core.Maybe [Types.DiskId])
dwsrrsDiskIds = Lens.field @"diskIds"
{-# INLINEABLE dwsrrsDiskIds #-}
{-# DEPRECATED diskIds "Use generic-lens or generic-optics with 'diskIds' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsGatewayARN :: Lens.Lens' DescribeWorkingStorageResponse (Core.Maybe Types.GatewayARN)
dwsrrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE dwsrrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The total working storage in bytes allocated for the gateway. If no working storage is configured for the gateway, this field returns 0.
--
-- /Note:/ Consider using 'workingStorageAllocatedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsWorkingStorageAllocatedInBytes :: Lens.Lens' DescribeWorkingStorageResponse (Core.Maybe Core.Integer)
dwsrrsWorkingStorageAllocatedInBytes = Lens.field @"workingStorageAllocatedInBytes"
{-# INLINEABLE dwsrrsWorkingStorageAllocatedInBytes #-}
{-# DEPRECATED workingStorageAllocatedInBytes "Use generic-lens or generic-optics with 'workingStorageAllocatedInBytes' instead"  #-}

-- | The total working storage in bytes in use by the gateway. If no working storage is configured for the gateway, this field returns 0.
--
-- /Note:/ Consider using 'workingStorageUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsWorkingStorageUsedInBytes :: Lens.Lens' DescribeWorkingStorageResponse (Core.Maybe Core.Integer)
dwsrrsWorkingStorageUsedInBytes = Lens.field @"workingStorageUsedInBytes"
{-# INLINEABLE dwsrrsWorkingStorageUsedInBytes #-}
{-# DEPRECATED workingStorageUsedInBytes "Use generic-lens or generic-optics with 'workingStorageUsedInBytes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsResponseStatus :: Lens.Lens' DescribeWorkingStorageResponse Core.Int
dwsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
