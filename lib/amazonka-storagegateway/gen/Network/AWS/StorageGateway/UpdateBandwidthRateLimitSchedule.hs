{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bandwidth rate limit schedule for a specified gateway. By default, gateways do not have bandwidth rate limit schedules, which means no bandwidth rate limiting is in effect. Use this to initiate or update a gateway's bandwidth rate limit schedule. This operation is supported in the volume and tape gateway types.
module Network.AWS.StorageGateway.UpdateBandwidthRateLimitSchedule
  ( -- * Creating a request
    UpdateBandwidthRateLimitSchedule (..),
    mkUpdateBandwidthRateLimitSchedule,

    -- ** Request lenses
    ubrlsGatewayARN,
    ubrlsBandwidthRateLimitIntervals,

    -- * Destructuring the response
    UpdateBandwidthRateLimitScheduleResponse (..),
    mkUpdateBandwidthRateLimitScheduleResponse,

    -- ** Response lenses
    ubrlsrrsGatewayARN,
    ubrlsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkUpdateBandwidthRateLimitSchedule' smart constructor.
data UpdateBandwidthRateLimitSchedule = UpdateBandwidthRateLimitSchedule'
  { gatewayARN :: Types.GatewayARN,
    -- | An array containing bandwidth rate limit schedule intervals for a gateway. When no bandwidth rate limit intervals have been scheduled, the array is empty.
    bandwidthRateLimitIntervals :: [Types.BandwidthRateLimitInterval]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBandwidthRateLimitSchedule' value with any optional fields omitted.
mkUpdateBandwidthRateLimitSchedule ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  UpdateBandwidthRateLimitSchedule
mkUpdateBandwidthRateLimitSchedule gatewayARN =
  UpdateBandwidthRateLimitSchedule'
    { gatewayARN,
      bandwidthRateLimitIntervals = Core.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlsGatewayARN :: Lens.Lens' UpdateBandwidthRateLimitSchedule Types.GatewayARN
ubrlsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED ubrlsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array containing bandwidth rate limit schedule intervals for a gateway. When no bandwidth rate limit intervals have been scheduled, the array is empty.
--
-- /Note:/ Consider using 'bandwidthRateLimitIntervals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlsBandwidthRateLimitIntervals :: Lens.Lens' UpdateBandwidthRateLimitSchedule [Types.BandwidthRateLimitInterval]
ubrlsBandwidthRateLimitIntervals = Lens.field @"bandwidthRateLimitIntervals"
{-# DEPRECATED ubrlsBandwidthRateLimitIntervals "Use generic-lens or generic-optics with 'bandwidthRateLimitIntervals' instead." #-}

instance Core.FromJSON UpdateBandwidthRateLimitSchedule where
  toJSON UpdateBandwidthRateLimitSchedule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just
              ( "BandwidthRateLimitIntervals"
                  Core..= bandwidthRateLimitIntervals
              )
          ]
      )

instance Core.AWSRequest UpdateBandwidthRateLimitSchedule where
  type
    Rs UpdateBandwidthRateLimitSchedule =
      UpdateBandwidthRateLimitScheduleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.UpdateBandwidthRateLimitSchedule"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBandwidthRateLimitScheduleResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateBandwidthRateLimitScheduleResponse' smart constructor.
data UpdateBandwidthRateLimitScheduleResponse = UpdateBandwidthRateLimitScheduleResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBandwidthRateLimitScheduleResponse' value with any optional fields omitted.
mkUpdateBandwidthRateLimitScheduleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateBandwidthRateLimitScheduleResponse
mkUpdateBandwidthRateLimitScheduleResponse responseStatus =
  UpdateBandwidthRateLimitScheduleResponse'
    { gatewayARN =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlsrrsGatewayARN :: Lens.Lens' UpdateBandwidthRateLimitScheduleResponse (Core.Maybe Types.GatewayARN)
ubrlsrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED ubrlsrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlsrrsResponseStatus :: Lens.Lens' UpdateBandwidthRateLimitScheduleResponse Core.Int
ubrlsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ubrlsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
