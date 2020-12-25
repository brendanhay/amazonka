{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeAvailabilityMonitorTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the most recent High Availability monitoring test that was performed on the host in a cluster. If a test isn't performed, the status and start time in the response would be null.
module Network.AWS.StorageGateway.DescribeAvailabilityMonitorTest
  ( -- * Creating a request
    DescribeAvailabilityMonitorTest (..),
    mkDescribeAvailabilityMonitorTest,

    -- ** Request lenses
    damtGatewayARN,

    -- * Destructuring the response
    DescribeAvailabilityMonitorTestResponse (..),
    mkDescribeAvailabilityMonitorTestResponse,

    -- ** Response lenses
    damtrrsGatewayARN,
    damtrrsStartTime,
    damtrrsStatus,
    damtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkDescribeAvailabilityMonitorTest' smart constructor.
newtype DescribeAvailabilityMonitorTest = DescribeAvailabilityMonitorTest'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAvailabilityMonitorTest' value with any optional fields omitted.
mkDescribeAvailabilityMonitorTest ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  DescribeAvailabilityMonitorTest
mkDescribeAvailabilityMonitorTest gatewayARN =
  DescribeAvailabilityMonitorTest' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damtGatewayARN :: Lens.Lens' DescribeAvailabilityMonitorTest Types.GatewayARN
damtGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED damtGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON DescribeAvailabilityMonitorTest where
  toJSON DescribeAvailabilityMonitorTest {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DescribeAvailabilityMonitorTest where
  type
    Rs DescribeAvailabilityMonitorTest =
      DescribeAvailabilityMonitorTestResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.DescribeAvailabilityMonitorTest"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAvailabilityMonitorTestResponse'
            Core.<$> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAvailabilityMonitorTestResponse' smart constructor.
data DescribeAvailabilityMonitorTestResponse = DescribeAvailabilityMonitorTestResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The time the High Availability monitoring test was started. If a test hasn't been performed, the value of this field is null.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the High Availability monitoring test. If a test hasn't been performed, the value of this field is null.
    status :: Core.Maybe Types.AvailabilityMonitorTestStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAvailabilityMonitorTestResponse' value with any optional fields omitted.
mkDescribeAvailabilityMonitorTestResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAvailabilityMonitorTestResponse
mkDescribeAvailabilityMonitorTestResponse responseStatus =
  DescribeAvailabilityMonitorTestResponse'
    { gatewayARN =
        Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damtrrsGatewayARN :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Core.Maybe Types.GatewayARN)
damtrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED damtrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The time the High Availability monitoring test was started. If a test hasn't been performed, the value of this field is null.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damtrrsStartTime :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Core.Maybe Core.NominalDiffTime)
damtrrsStartTime = Lens.field @"startTime"
{-# DEPRECATED damtrrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The status of the High Availability monitoring test. If a test hasn't been performed, the value of this field is null.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damtrrsStatus :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Core.Maybe Types.AvailabilityMonitorTestStatus)
damtrrsStatus = Lens.field @"status"
{-# DEPRECATED damtrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damtrrsResponseStatus :: Lens.Lens' DescribeAvailabilityMonitorTestResponse Core.Int
damtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED damtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
