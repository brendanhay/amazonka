{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddUploadBuffer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as upload buffer for a specified gateway. This operation is supported for the stored volume, cached volume and tape gateway types.
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add upload buffer, and one or more disk IDs that you want to configure as upload buffer.
module Network.AWS.StorageGateway.AddUploadBuffer
    (
    -- * Creating a request
      AddUploadBuffer (..)
    , mkAddUploadBuffer
    -- ** Request lenses
    , aubGatewayARN
    , aubDiskIds

    -- * Destructuring the response
    , AddUploadBufferResponse (..)
    , mkAddUploadBufferResponse
    -- ** Response lenses
    , aubrrsGatewayARN
    , aubrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkAddUploadBuffer' smart constructor.
data AddUploadBuffer = AddUploadBuffer'
  { gatewayARN :: Types.GatewayARN
  , diskIds :: [Types.DiskId]
    -- ^ An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddUploadBuffer' value with any optional fields omitted.
mkAddUploadBuffer
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> AddUploadBuffer
mkAddUploadBuffer gatewayARN
  = AddUploadBuffer'{gatewayARN, diskIds = Core.mempty}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aubGatewayARN :: Lens.Lens' AddUploadBuffer Types.GatewayARN
aubGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE aubGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aubDiskIds :: Lens.Lens' AddUploadBuffer [Types.DiskId]
aubDiskIds = Lens.field @"diskIds"
{-# INLINEABLE aubDiskIds #-}
{-# DEPRECATED diskIds "Use generic-lens or generic-optics with 'diskIds' instead"  #-}

instance Core.ToQuery AddUploadBuffer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddUploadBuffer where
        toHeaders AddUploadBuffer{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.AddUploadBuffer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddUploadBuffer where
        toJSON AddUploadBuffer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("DiskIds" Core..= diskIds)])

instance Core.AWSRequest AddUploadBuffer where
        type Rs AddUploadBuffer = AddUploadBufferResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddUploadBufferResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddUploadBufferResponse' smart constructor.
data AddUploadBufferResponse = AddUploadBufferResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddUploadBufferResponse' value with any optional fields omitted.
mkAddUploadBufferResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddUploadBufferResponse
mkAddUploadBufferResponse responseStatus
  = AddUploadBufferResponse'{gatewayARN = Core.Nothing,
                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aubrrsGatewayARN :: Lens.Lens' AddUploadBufferResponse (Core.Maybe Types.GatewayARN)
aubrrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE aubrrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aubrrsResponseStatus :: Lens.Lens' AddUploadBufferResponse Core.Int
aubrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aubrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
