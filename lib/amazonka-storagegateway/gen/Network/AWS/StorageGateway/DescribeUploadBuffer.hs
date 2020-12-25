{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeUploadBuffer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the upload buffer of a gateway. This operation is supported for the stored volume, cached volume, and tape gateway types.
--
-- The response includes disk IDs that are configured as upload buffer space, and it includes the amount of upload buffer space allocated and used.
module Network.AWS.StorageGateway.DescribeUploadBuffer
  ( -- * Creating a request
    DescribeUploadBuffer (..),
    mkDescribeUploadBuffer,

    -- ** Request lenses
    dubGatewayARN,

    -- * Destructuring the response
    DescribeUploadBufferResponse (..),
    mkDescribeUploadBufferResponse,

    -- ** Response lenses
    dubrrsDiskIds,
    dubrrsGatewayARN,
    dubrrsUploadBufferAllocatedInBytes,
    dubrrsUploadBufferUsedInBytes,
    dubrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkDescribeUploadBuffer' smart constructor.
newtype DescribeUploadBuffer = DescribeUploadBuffer'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUploadBuffer' value with any optional fields omitted.
mkDescribeUploadBuffer ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  DescribeUploadBuffer
mkDescribeUploadBuffer gatewayARN =
  DescribeUploadBuffer' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubGatewayARN :: Lens.Lens' DescribeUploadBuffer Types.GatewayARN
dubGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dubGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON DescribeUploadBuffer where
  toJSON DescribeUploadBuffer {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DescribeUploadBuffer where
  type Rs DescribeUploadBuffer = DescribeUploadBufferResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.DescribeUploadBuffer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUploadBufferResponse'
            Core.<$> (x Core..:? "DiskIds")
            Core.<*> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "UploadBufferAllocatedInBytes")
            Core.<*> (x Core..:? "UploadBufferUsedInBytes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeUploadBufferResponse' smart constructor.
data DescribeUploadBufferResponse = DescribeUploadBufferResponse'
  { -- | An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
    diskIds :: Core.Maybe [Types.DiskId],
    gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The total number of bytes allocated in the gateway's as upload buffer.
    uploadBufferAllocatedInBytes :: Core.Maybe Core.Integer,
    -- | The total number of bytes being used in the gateway's upload buffer.
    uploadBufferUsedInBytes :: Core.Maybe Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUploadBufferResponse' value with any optional fields omitted.
mkDescribeUploadBufferResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUploadBufferResponse
mkDescribeUploadBufferResponse responseStatus =
  DescribeUploadBufferResponse'
    { diskIds = Core.Nothing,
      gatewayARN = Core.Nothing,
      uploadBufferAllocatedInBytes = Core.Nothing,
      uploadBufferUsedInBytes = Core.Nothing,
      responseStatus
    }

-- | An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubrrsDiskIds :: Lens.Lens' DescribeUploadBufferResponse (Core.Maybe [Types.DiskId])
dubrrsDiskIds = Lens.field @"diskIds"
{-# DEPRECATED dubrrsDiskIds "Use generic-lens or generic-optics with 'diskIds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubrrsGatewayARN :: Lens.Lens' DescribeUploadBufferResponse (Core.Maybe Types.GatewayARN)
dubrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dubrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The total number of bytes allocated in the gateway's as upload buffer.
--
-- /Note:/ Consider using 'uploadBufferAllocatedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubrrsUploadBufferAllocatedInBytes :: Lens.Lens' DescribeUploadBufferResponse (Core.Maybe Core.Integer)
dubrrsUploadBufferAllocatedInBytes = Lens.field @"uploadBufferAllocatedInBytes"
{-# DEPRECATED dubrrsUploadBufferAllocatedInBytes "Use generic-lens or generic-optics with 'uploadBufferAllocatedInBytes' instead." #-}

-- | The total number of bytes being used in the gateway's upload buffer.
--
-- /Note:/ Consider using 'uploadBufferUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubrrsUploadBufferUsedInBytes :: Lens.Lens' DescribeUploadBufferResponse (Core.Maybe Core.Integer)
dubrrsUploadBufferUsedInBytes = Lens.field @"uploadBufferUsedInBytes"
{-# DEPRECATED dubrrsUploadBufferUsedInBytes "Use generic-lens or generic-optics with 'uploadBufferUsedInBytes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubrrsResponseStatus :: Lens.Lens' DescribeUploadBufferResponse Core.Int
dubrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dubrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
