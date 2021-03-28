{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as cache for a gateway. This operation is only supported in the cached volume, tape, and file gateway type (see <https://docs.aws.amazon.com/storagegateway/latest/userguide/StorageGatewayConcepts.html How AWS Storage Gateway works (architecture)> .
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add cache, and one or more disk IDs that you want to configure as cache.
module Network.AWS.StorageGateway.AddCache
    (
    -- * Creating a request
      AddCache (..)
    , mkAddCache
    -- ** Request lenses
    , acGatewayARN
    , acDiskIds

    -- * Destructuring the response
    , AddCacheResponse (..)
    , mkAddCacheResponse
    -- ** Response lenses
    , acrrsGatewayARN
    , acrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkAddCache' smart constructor.
data AddCache = AddCache'
  { gatewayARN :: Types.GatewayARN
  , diskIds :: [Types.DiskId]
    -- ^ An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddCache' value with any optional fields omitted.
mkAddCache
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> AddCache
mkAddCache gatewayARN
  = AddCache'{gatewayARN, diskIds = Core.mempty}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acGatewayARN :: Lens.Lens' AddCache Types.GatewayARN
acGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE acGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acDiskIds :: Lens.Lens' AddCache [Types.DiskId]
acDiskIds = Lens.field @"diskIds"
{-# INLINEABLE acDiskIds #-}
{-# DEPRECATED diskIds "Use generic-lens or generic-optics with 'diskIds' instead"  #-}

instance Core.ToQuery AddCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddCache where
        toHeaders AddCache{..}
          = Core.pure ("X-Amz-Target", "StorageGateway_20130630.AddCache")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddCache where
        toJSON AddCache{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("DiskIds" Core..= diskIds)])

instance Core.AWSRequest AddCache where
        type Rs AddCache = AddCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddCacheResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddCacheResponse' smart constructor.
data AddCacheResponse = AddCacheResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddCacheResponse' value with any optional fields omitted.
mkAddCacheResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddCacheResponse
mkAddCacheResponse responseStatus
  = AddCacheResponse'{gatewayARN = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrrsGatewayARN :: Lens.Lens' AddCacheResponse (Core.Maybe Types.GatewayARN)
acrrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE acrrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrrsResponseStatus :: Lens.Lens' AddCacheResponse Core.Int
acrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE acrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
