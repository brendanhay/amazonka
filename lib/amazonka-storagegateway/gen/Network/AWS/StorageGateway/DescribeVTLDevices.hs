{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeVTLDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of virtual tape library (VTL) devices for the specified tape gateway. In the response, AWS Storage Gateway returns VTL device information.
--
-- This operation is only supported in the tape gateway type.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeVTLDevices
  ( -- * Creating a request
    DescribeVTLDevices (..),
    mkDescribeVTLDevices,

    -- ** Request lenses
    dvtldGatewayARN,
    dvtldLimit,
    dvtldMarker,
    dvtldVTLDeviceARNs,

    -- * Destructuring the response
    DescribeVTLDevicesResponse (..),
    mkDescribeVTLDevicesResponse,

    -- ** Response lenses
    dvtldrrsGatewayARN,
    dvtldrrsMarker,
    dvtldrrsVTLDevices,
    dvtldrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DescribeVTLDevicesInput
--
-- /See:/ 'mkDescribeVTLDevices' smart constructor.
data DescribeVTLDevices = DescribeVTLDevices'
  { gatewayARN :: Types.GatewayARN,
    -- | Specifies that the number of VTL devices described be limited to the specified number.
    limit :: Core.Maybe Core.Natural,
    -- | An opaque string that indicates the position at which to begin describing the VTL devices.
    marker :: Core.Maybe Types.Marker,
    -- | An array of strings, where each string represents the Amazon Resource Name (ARN) of a VTL device.
    vTLDeviceARNs :: Core.Maybe [Types.VTLDeviceARN]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVTLDevices' value with any optional fields omitted.
mkDescribeVTLDevices ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  DescribeVTLDevices
mkDescribeVTLDevices gatewayARN =
  DescribeVTLDevices'
    { gatewayARN,
      limit = Core.Nothing,
      marker = Core.Nothing,
      vTLDeviceARNs = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldGatewayARN :: Lens.Lens' DescribeVTLDevices Types.GatewayARN
dvtldGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dvtldGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Specifies that the number of VTL devices described be limited to the specified number.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldLimit :: Lens.Lens' DescribeVTLDevices (Core.Maybe Core.Natural)
dvtldLimit = Lens.field @"limit"
{-# DEPRECATED dvtldLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | An opaque string that indicates the position at which to begin describing the VTL devices.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldMarker :: Lens.Lens' DescribeVTLDevices (Core.Maybe Types.Marker)
dvtldMarker = Lens.field @"marker"
{-# DEPRECATED dvtldMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An array of strings, where each string represents the Amazon Resource Name (ARN) of a VTL device.
--
-- /Note:/ Consider using 'vTLDeviceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldVTLDeviceARNs :: Lens.Lens' DescribeVTLDevices (Core.Maybe [Types.VTLDeviceARN])
dvtldVTLDeviceARNs = Lens.field @"vTLDeviceARNs"
{-# DEPRECATED dvtldVTLDeviceARNs "Use generic-lens or generic-optics with 'vTLDeviceARNs' instead." #-}

instance Core.FromJSON DescribeVTLDevices where
  toJSON DescribeVTLDevices {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker,
            ("VTLDeviceARNs" Core..=) Core.<$> vTLDeviceARNs
          ]
      )

instance Core.AWSRequest DescribeVTLDevices where
  type Rs DescribeVTLDevices = DescribeVTLDevicesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.DescribeVTLDevices")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVTLDevicesResponse'
            Core.<$> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (x Core..:? "VTLDevices")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeVTLDevices where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"vTLDevices" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | DescribeVTLDevicesOutput
--
-- /See:/ 'mkDescribeVTLDevicesResponse' smart constructor.
data DescribeVTLDevicesResponse = DescribeVTLDevicesResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | An opaque string that indicates the position at which the VTL devices that were fetched for description ended. Use the marker in your next request to fetch the next set of VTL devices in the list. If there are no more VTL devices to describe, this field does not appear in the response.
    marker :: Core.Maybe Types.Marker,
    -- | An array of VTL device objects composed of the Amazon Resource Name (ARN) of the VTL devices.
    vTLDevices :: Core.Maybe [Types.VTLDevice],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVTLDevicesResponse' value with any optional fields omitted.
mkDescribeVTLDevicesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeVTLDevicesResponse
mkDescribeVTLDevicesResponse responseStatus =
  DescribeVTLDevicesResponse'
    { gatewayARN = Core.Nothing,
      marker = Core.Nothing,
      vTLDevices = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldrrsGatewayARN :: Lens.Lens' DescribeVTLDevicesResponse (Core.Maybe Types.GatewayARN)
dvtldrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dvtldrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An opaque string that indicates the position at which the VTL devices that were fetched for description ended. Use the marker in your next request to fetch the next set of VTL devices in the list. If there are no more VTL devices to describe, this field does not appear in the response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldrrsMarker :: Lens.Lens' DescribeVTLDevicesResponse (Core.Maybe Types.Marker)
dvtldrrsMarker = Lens.field @"marker"
{-# DEPRECATED dvtldrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An array of VTL device objects composed of the Amazon Resource Name (ARN) of the VTL devices.
--
-- /Note:/ Consider using 'vTLDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldrrsVTLDevices :: Lens.Lens' DescribeVTLDevicesResponse (Core.Maybe [Types.VTLDevice])
dvtldrrsVTLDevices = Lens.field @"vTLDevices"
{-# DEPRECATED dvtldrrsVTLDevices "Use generic-lens or generic-optics with 'vTLDevices' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldrrsResponseStatus :: Lens.Lens' DescribeVTLDevicesResponse Core.Int
dvtldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvtldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
