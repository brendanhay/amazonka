{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the cache of a gateway. This operation is only supported in the cached volume, tape, and file gateway types.
--
-- The response includes disk IDs that are configured as cache, and it includes the amount of cache allocated and used.
module Network.AWS.StorageGateway.DescribeCache
    (
    -- * Creating a request
      DescribeCache (..)
    , mkDescribeCache
    -- ** Request lenses
    , dcGatewayARN

    -- * Destructuring the response
    , DescribeCacheResponse (..)
    , mkDescribeCacheResponse
    -- ** Response lenses
    , dcrrsCacheAllocatedInBytes
    , dcrrsCacheDirtyPercentage
    , dcrrsCacheHitPercentage
    , dcrrsCacheMissPercentage
    , dcrrsCacheUsedPercentage
    , dcrrsDiskIds
    , dcrrsGatewayARN
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkDescribeCache' smart constructor.
newtype DescribeCache = DescribeCache'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCache' value with any optional fields omitted.
mkDescribeCache
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> DescribeCache
mkDescribeCache gatewayARN = DescribeCache'{gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcGatewayARN :: Lens.Lens' DescribeCache Types.GatewayARN
dcGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE dcGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

instance Core.ToQuery DescribeCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCache where
        toHeaders DescribeCache{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DescribeCache")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCache where
        toJSON DescribeCache{..}
          = Core.object
              (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DescribeCache where
        type Rs DescribeCache = DescribeCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCacheResponse' Core.<$>
                   (x Core..:? "CacheAllocatedInBytes") Core.<*>
                     x Core..:? "CacheDirtyPercentage"
                     Core.<*> x Core..:? "CacheHitPercentage"
                     Core.<*> x Core..:? "CacheMissPercentage"
                     Core.<*> x Core..:? "CacheUsedPercentage"
                     Core.<*> x Core..:? "DiskIds"
                     Core.<*> x Core..:? "GatewayARN"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCacheResponse' smart constructor.
data DescribeCacheResponse = DescribeCacheResponse'
  { cacheAllocatedInBytes :: Core.Maybe Core.Integer
    -- ^ The amount of cache in bytes allocated to a gateway.
  , cacheDirtyPercentage :: Core.Maybe Core.Double
    -- ^ The file share's contribution to the overall percentage of the gateway's cache that has not been persisted to AWS. The sample is taken at the end of the reporting period.
  , cacheHitPercentage :: Core.Maybe Core.Double
    -- ^ Percent of application read operations from the file shares that are served from cache. The sample is taken at the end of the reporting period.
  , cacheMissPercentage :: Core.Maybe Core.Double
    -- ^ Percent of application read operations from the file shares that are not served from cache. The sample is taken at the end of the reporting period.
  , cacheUsedPercentage :: Core.Maybe Core.Double
    -- ^ Percent use of the gateway's cache storage. This metric applies only to the gateway-cached volume setup. The sample is taken at the end of the reporting period.
  , diskIds :: Core.Maybe [Types.DiskId]
    -- ^ An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
  , gatewayARN :: Core.Maybe Types.GatewayARN
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheResponse' value with any optional fields omitted.
mkDescribeCacheResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCacheResponse
mkDescribeCacheResponse responseStatus
  = DescribeCacheResponse'{cacheAllocatedInBytes = Core.Nothing,
                           cacheDirtyPercentage = Core.Nothing,
                           cacheHitPercentage = Core.Nothing,
                           cacheMissPercentage = Core.Nothing,
                           cacheUsedPercentage = Core.Nothing, diskIds = Core.Nothing,
                           gatewayARN = Core.Nothing, responseStatus}

-- | The amount of cache in bytes allocated to a gateway.
--
-- /Note:/ Consider using 'cacheAllocatedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCacheAllocatedInBytes :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Integer)
dcrrsCacheAllocatedInBytes = Lens.field @"cacheAllocatedInBytes"
{-# INLINEABLE dcrrsCacheAllocatedInBytes #-}
{-# DEPRECATED cacheAllocatedInBytes "Use generic-lens or generic-optics with 'cacheAllocatedInBytes' instead"  #-}

-- | The file share's contribution to the overall percentage of the gateway's cache that has not been persisted to AWS. The sample is taken at the end of the reporting period.
--
-- /Note:/ Consider using 'cacheDirtyPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCacheDirtyPercentage :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Double)
dcrrsCacheDirtyPercentage = Lens.field @"cacheDirtyPercentage"
{-# INLINEABLE dcrrsCacheDirtyPercentage #-}
{-# DEPRECATED cacheDirtyPercentage "Use generic-lens or generic-optics with 'cacheDirtyPercentage' instead"  #-}

-- | Percent of application read operations from the file shares that are served from cache. The sample is taken at the end of the reporting period.
--
-- /Note:/ Consider using 'cacheHitPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCacheHitPercentage :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Double)
dcrrsCacheHitPercentage = Lens.field @"cacheHitPercentage"
{-# INLINEABLE dcrrsCacheHitPercentage #-}
{-# DEPRECATED cacheHitPercentage "Use generic-lens or generic-optics with 'cacheHitPercentage' instead"  #-}

-- | Percent of application read operations from the file shares that are not served from cache. The sample is taken at the end of the reporting period.
--
-- /Note:/ Consider using 'cacheMissPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCacheMissPercentage :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Double)
dcrrsCacheMissPercentage = Lens.field @"cacheMissPercentage"
{-# INLINEABLE dcrrsCacheMissPercentage #-}
{-# DEPRECATED cacheMissPercentage "Use generic-lens or generic-optics with 'cacheMissPercentage' instead"  #-}

-- | Percent use of the gateway's cache storage. This metric applies only to the gateway-cached volume setup. The sample is taken at the end of the reporting period.
--
-- /Note:/ Consider using 'cacheUsedPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCacheUsedPercentage :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Double)
dcrrsCacheUsedPercentage = Lens.field @"cacheUsedPercentage"
{-# INLINEABLE dcrrsCacheUsedPercentage #-}
{-# DEPRECATED cacheUsedPercentage "Use generic-lens or generic-optics with 'cacheUsedPercentage' instead"  #-}

-- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsDiskIds :: Lens.Lens' DescribeCacheResponse (Core.Maybe [Types.DiskId])
dcrrsDiskIds = Lens.field @"diskIds"
{-# INLINEABLE dcrrsDiskIds #-}
{-# DEPRECATED diskIds "Use generic-lens or generic-optics with 'diskIds' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsGatewayARN :: Lens.Lens' DescribeCacheResponse (Core.Maybe Types.GatewayARN)
dcrrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE dcrrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeCacheResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
