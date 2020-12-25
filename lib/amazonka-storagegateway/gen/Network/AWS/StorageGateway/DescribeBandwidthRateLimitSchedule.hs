{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the bandwidth rate limit schedule of a gateway. By default, gateways do not have bandwidth rate limit schedules, which means no bandwidth rate limiting is in effect. This operation is supported only in the volume and tape gateway types.
--
-- This operation returns information about a gateway's bandwidth rate limit schedule. A bandwidth rate limit schedule consists of one or more bandwidth rate limit intervals. A bandwidth rate limit interval defines a period of time on one or more days of the week, during which bandwidth rate limits are specified for uploading, downloading, or both.
-- A bandwidth rate limit interval consists of one or more days of the week, a start hour and minute, an ending hour and minute, and bandwidth rate limits for uploading and downloading
-- If no bandwidth rate limit schedule intervals are set for the gateway, this operation returns an empty response. To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.DescribeBandwidthRateLimitSchedule
  ( -- * Creating a request
    DescribeBandwidthRateLimitSchedule (..),
    mkDescribeBandwidthRateLimitSchedule,

    -- ** Request lenses
    dbrlsGatewayARN,

    -- * Destructuring the response
    DescribeBandwidthRateLimitScheduleResponse (..),
    mkDescribeBandwidthRateLimitScheduleResponse,

    -- ** Response lenses
    dbrlsrrsBandwidthRateLimitIntervals,
    dbrlsrrsGatewayARN,
    dbrlsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkDescribeBandwidthRateLimitSchedule' smart constructor.
newtype DescribeBandwidthRateLimitSchedule = DescribeBandwidthRateLimitSchedule'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBandwidthRateLimitSchedule' value with any optional fields omitted.
mkDescribeBandwidthRateLimitSchedule ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  DescribeBandwidthRateLimitSchedule
mkDescribeBandwidthRateLimitSchedule gatewayARN =
  DescribeBandwidthRateLimitSchedule' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlsGatewayARN :: Lens.Lens' DescribeBandwidthRateLimitSchedule Types.GatewayARN
dbrlsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dbrlsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON DescribeBandwidthRateLimitSchedule where
  toJSON DescribeBandwidthRateLimitSchedule {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DescribeBandwidthRateLimitSchedule where
  type
    Rs DescribeBandwidthRateLimitSchedule =
      DescribeBandwidthRateLimitScheduleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.DescribeBandwidthRateLimitSchedule"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBandwidthRateLimitScheduleResponse'
            Core.<$> (x Core..:? "BandwidthRateLimitIntervals")
            Core.<*> (x Core..:? "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeBandwidthRateLimitScheduleResponse' smart constructor.
data DescribeBandwidthRateLimitScheduleResponse = DescribeBandwidthRateLimitScheduleResponse'
  { -- | An array that contains the bandwidth rate limit intervals for a tape or volume gateway.
    bandwidthRateLimitIntervals :: Core.Maybe [Types.BandwidthRateLimitInterval],
    gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBandwidthRateLimitScheduleResponse' value with any optional fields omitted.
mkDescribeBandwidthRateLimitScheduleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeBandwidthRateLimitScheduleResponse
mkDescribeBandwidthRateLimitScheduleResponse responseStatus =
  DescribeBandwidthRateLimitScheduleResponse'
    { bandwidthRateLimitIntervals =
        Core.Nothing,
      gatewayARN = Core.Nothing,
      responseStatus
    }

-- | An array that contains the bandwidth rate limit intervals for a tape or volume gateway.
--
-- /Note:/ Consider using 'bandwidthRateLimitIntervals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlsrrsBandwidthRateLimitIntervals :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse (Core.Maybe [Types.BandwidthRateLimitInterval])
dbrlsrrsBandwidthRateLimitIntervals = Lens.field @"bandwidthRateLimitIntervals"
{-# DEPRECATED dbrlsrrsBandwidthRateLimitIntervals "Use generic-lens or generic-optics with 'bandwidthRateLimitIntervals' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlsrrsGatewayARN :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse (Core.Maybe Types.GatewayARN)
dbrlsrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dbrlsrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlsrrsResponseStatus :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse Core.Int
dbrlsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbrlsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
