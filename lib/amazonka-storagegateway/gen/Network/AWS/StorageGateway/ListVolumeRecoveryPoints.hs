{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListVolumeRecoveryPoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the recovery points for a specified gateway. This operation is only supported in the cached volume gateway type.
--
-- Each cache volume has one recovery point. A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot or clone a new cached volume from a source volume. To create a snapshot from a volume recovery point use the 'CreateSnapshotFromVolumeRecoveryPoint' operation.
module Network.AWS.StorageGateway.ListVolumeRecoveryPoints
  ( -- * Creating a request
    ListVolumeRecoveryPoints (..),
    mkListVolumeRecoveryPoints,

    -- ** Request lenses
    lvrpGatewayARN,

    -- * Destructuring the response
    ListVolumeRecoveryPointsResponse (..),
    mkListVolumeRecoveryPointsResponse,

    -- ** Response lenses
    lvrprrsGatewayARN,
    lvrprrsVolumeRecoveryPointInfos,
    lvrprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkListVolumeRecoveryPoints' smart constructor.
newtype ListVolumeRecoveryPoints = ListVolumeRecoveryPoints'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListVolumeRecoveryPoints' value with any optional fields omitted.
mkListVolumeRecoveryPoints ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  ListVolumeRecoveryPoints
mkListVolumeRecoveryPoints gatewayARN =
  ListVolumeRecoveryPoints' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrpGatewayARN :: Lens.Lens' ListVolumeRecoveryPoints Types.GatewayARN
lvrpGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED lvrpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON ListVolumeRecoveryPoints where
  toJSON ListVolumeRecoveryPoints {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest ListVolumeRecoveryPoints where
  type Rs ListVolumeRecoveryPoints = ListVolumeRecoveryPointsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.ListVolumeRecoveryPoints"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVolumeRecoveryPointsResponse'
            Core.<$> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "VolumeRecoveryPointInfos")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListVolumeRecoveryPointsResponse' smart constructor.
data ListVolumeRecoveryPointsResponse = ListVolumeRecoveryPointsResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | An array of 'VolumeRecoveryPointInfo' objects.
    volumeRecoveryPointInfos :: Core.Maybe [Types.VolumeRecoveryPointInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVolumeRecoveryPointsResponse' value with any optional fields omitted.
mkListVolumeRecoveryPointsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListVolumeRecoveryPointsResponse
mkListVolumeRecoveryPointsResponse responseStatus =
  ListVolumeRecoveryPointsResponse'
    { gatewayARN = Core.Nothing,
      volumeRecoveryPointInfos = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrprrsGatewayARN :: Lens.Lens' ListVolumeRecoveryPointsResponse (Core.Maybe Types.GatewayARN)
lvrprrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED lvrprrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array of 'VolumeRecoveryPointInfo' objects.
--
-- /Note:/ Consider using 'volumeRecoveryPointInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrprrsVolumeRecoveryPointInfos :: Lens.Lens' ListVolumeRecoveryPointsResponse (Core.Maybe [Types.VolumeRecoveryPointInfo])
lvrprrsVolumeRecoveryPointInfos = Lens.field @"volumeRecoveryPointInfos"
{-# DEPRECATED lvrprrsVolumeRecoveryPointInfos "Use generic-lens or generic-optics with 'volumeRecoveryPointInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrprrsResponseStatus :: Lens.Lens' ListVolumeRecoveryPointsResponse Core.Int
lvrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lvrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
