{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListLocalDisks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the gateway's local disks. To specify which gateway to describe, you use the Amazon Resource Name (ARN) of the gateway in the body of the request.
--
-- The request returns a list of all disks, specifying which are configured as working storage, cache storage, or stored volume or not configured at all. The response includes a @DiskStatus@ field. This field can have a value of present (the disk is available to use), missing (the disk is no longer connected to the gateway), or mismatch (the disk node is occupied by a disk that has incorrect metadata or the disk content is corrupted).
module Network.AWS.StorageGateway.ListLocalDisks
  ( -- * Creating a request
    ListLocalDisks (..),
    mkListLocalDisks,

    -- ** Request lenses
    lldGatewayARN,

    -- * Destructuring the response
    ListLocalDisksResponse (..),
    mkListLocalDisksResponse,

    -- ** Response lenses
    lldrrsDisks,
    lldrrsGatewayARN,
    lldrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'mkListLocalDisks' smart constructor.
newtype ListLocalDisks = ListLocalDisks'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListLocalDisks' value with any optional fields omitted.
mkListLocalDisks ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  ListLocalDisks
mkListLocalDisks gatewayARN = ListLocalDisks' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldGatewayARN :: Lens.Lens' ListLocalDisks Types.GatewayARN
lldGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED lldGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON ListLocalDisks where
  toJSON ListLocalDisks {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest ListLocalDisks where
  type Rs ListLocalDisks = ListLocalDisksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.ListLocalDisks")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLocalDisksResponse'
            Core.<$> (x Core..:? "Disks")
            Core.<*> (x Core..:? "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListLocalDisksResponse' smart constructor.
data ListLocalDisksResponse = ListLocalDisksResponse'
  { -- | A JSON object containing the following fields:
    --
    --
    --     * 'ListLocalDisksOutput$Disks'
    disks :: Core.Maybe [Types.Disk],
    gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLocalDisksResponse' value with any optional fields omitted.
mkListLocalDisksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLocalDisksResponse
mkListLocalDisksResponse responseStatus =
  ListLocalDisksResponse'
    { disks = Core.Nothing,
      gatewayARN = Core.Nothing,
      responseStatus
    }

-- | A JSON object containing the following fields:
--
--
--     * 'ListLocalDisksOutput$Disks'
--
--
--
-- /Note:/ Consider using 'disks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrrsDisks :: Lens.Lens' ListLocalDisksResponse (Core.Maybe [Types.Disk])
lldrrsDisks = Lens.field @"disks"
{-# DEPRECATED lldrrsDisks "Use generic-lens or generic-optics with 'disks' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrrsGatewayARN :: Lens.Lens' ListLocalDisksResponse (Core.Maybe Types.GatewayARN)
lldrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED lldrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrrsResponseStatus :: Lens.Lens' ListLocalDisksResponse Core.Int
lldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
