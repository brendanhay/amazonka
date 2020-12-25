{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists gateways owned by an AWS account in an AWS Region specified in the request. The returned list is ordered by gateway Amazon Resource Name (ARN).
--
-- By default, the operation returns a maximum of 100 gateways. This operation supports pagination that allows you to optionally reduce the number of gateways returned in a response.
-- If you have more gateways than are returned in a response (that is, the response returns only a truncated list of your gateways), the response contains a marker that you can specify in your next request to fetch the next page of gateways.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListGateways
  ( -- * Creating a request
    ListGateways (..),
    mkListGateways,

    -- ** Request lenses
    lgLimit,
    lgMarker,

    -- * Destructuring the response
    ListGatewaysResponse (..),
    mkListGatewaysResponse,

    -- ** Response lenses
    lgrrsGateways,
    lgrrsMarker,
    lgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing zero or more of the following fields:
--
--
--     * 'ListGatewaysInput$Limit'
--
--
--     * 'ListGatewaysInput$Marker'
--
--
--
-- /See:/ 'mkListGateways' smart constructor.
data ListGateways = ListGateways'
  { -- | Specifies that the list of gateways returned be limited to the specified number of items.
    limit :: Core.Maybe Core.Natural,
    -- | An opaque string that indicates the position at which to begin the returned list of gateways.
    marker :: Core.Maybe Types.Marker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGateways' value with any optional fields omitted.
mkListGateways ::
  ListGateways
mkListGateways =
  ListGateways' {limit = Core.Nothing, marker = Core.Nothing}

-- | Specifies that the list of gateways returned be limited to the specified number of items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgLimit :: Lens.Lens' ListGateways (Core.Maybe Core.Natural)
lgLimit = Lens.field @"limit"
{-# DEPRECATED lgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | An opaque string that indicates the position at which to begin the returned list of gateways.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMarker :: Lens.Lens' ListGateways (Core.Maybe Types.Marker)
lgMarker = Lens.field @"marker"
{-# DEPRECATED lgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.FromJSON ListGateways where
  toJSON ListGateways {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.AWSRequest ListGateways where
  type Rs ListGateways = ListGatewaysResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StorageGateway_20130630.ListGateways")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewaysResponse'
            Core.<$> (x Core..:? "Gateways")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListGateways where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"gateways" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkListGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { -- | An array of 'GatewayInfo' objects.
    gateways :: Core.Maybe [Types.GatewayInfo],
    -- | Use the marker in your next request to fetch the next set of gateways in the list. If there are no more gateways to list, this field does not appear in the response.
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGatewaysResponse' value with any optional fields omitted.
mkListGatewaysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListGatewaysResponse
mkListGatewaysResponse responseStatus =
  ListGatewaysResponse'
    { gateways = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | An array of 'GatewayInfo' objects.
--
-- /Note:/ Consider using 'gateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsGateways :: Lens.Lens' ListGatewaysResponse (Core.Maybe [Types.GatewayInfo])
lgrrsGateways = Lens.field @"gateways"
{-# DEPRECATED lgrrsGateways "Use generic-lens or generic-optics with 'gateways' instead." #-}

-- | Use the marker in your next request to fetch the next set of gateways in the list. If there are no more gateways to list, this field does not appear in the response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsMarker :: Lens.Lens' ListGatewaysResponse (Core.Maybe Types.Marker)
lgrrsMarker = Lens.field @"marker"
{-# DEPRECATED lgrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsResponseStatus :: Lens.Lens' ListGatewaysResponse Core.Int
lgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
