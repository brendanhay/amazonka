{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListFileShares
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the file shares for a specific file gateway, or the list of file shares that belong to the calling user account. This operation is only supported for file gateways.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListFileShares
  ( -- * Creating a request
    ListFileShares (..),
    mkListFileShares,

    -- ** Request lenses
    lfsGatewayARN,
    lfsLimit,
    lfsMarker,

    -- * Destructuring the response
    ListFileSharesResponse (..),
    mkListFileSharesResponse,

    -- ** Response lenses
    lfsrrsFileShareInfoList,
    lfsrrsMarker,
    lfsrrsNextMarker,
    lfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | ListFileShareInput
--
-- /See:/ 'mkListFileShares' smart constructor.
data ListFileShares = ListFileShares'
  { -- | The Amazon Resource Name (ARN) of the gateway whose file shares you want to list. If this field is not present, all file shares under your account are listed.
    gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The maximum number of file shares to return in the response. The value must be an integer with a value greater than zero. Optional.
    limit :: Core.Maybe Core.Natural,
    -- | Opaque pagination token returned from a previous ListFileShares operation. If present, @Marker@ specifies where to continue the list from after a previous call to ListFileShares. Optional.
    marker :: Core.Maybe Types.Marker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFileShares' value with any optional fields omitted.
mkListFileShares ::
  ListFileShares
mkListFileShares =
  ListFileShares'
    { gatewayARN = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway whose file shares you want to list. If this field is not present, all file shares under your account are listed.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsGatewayARN :: Lens.Lens' ListFileShares (Core.Maybe Types.GatewayARN)
lfsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED lfsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The maximum number of file shares to return in the response. The value must be an integer with a value greater than zero. Optional.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsLimit :: Lens.Lens' ListFileShares (Core.Maybe Core.Natural)
lfsLimit = Lens.field @"limit"
{-# DEPRECATED lfsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Opaque pagination token returned from a previous ListFileShares operation. If present, @Marker@ specifies where to continue the list from after a previous call to ListFileShares. Optional.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsMarker :: Lens.Lens' ListFileShares (Core.Maybe Types.Marker)
lfsMarker = Lens.field @"marker"
{-# DEPRECATED lfsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.FromJSON ListFileShares where
  toJSON ListFileShares {..} =
    Core.object
      ( Core.catMaybes
          [ ("GatewayARN" Core..=) Core.<$> gatewayARN,
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.AWSRequest ListFileShares where
  type Rs ListFileShares = ListFileSharesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.ListFileShares")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFileSharesResponse'
            Core.<$> (x Core..:? "FileShareInfoList")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (x Core..:? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListFileShares where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"fileShareInfoList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | ListFileShareOutput
--
-- /See:/ 'mkListFileSharesResponse' smart constructor.
data ListFileSharesResponse = ListFileSharesResponse'
  { -- | An array of information about the file gateway's file shares.
    fileShareInfoList :: Core.Maybe [Types.FileShareInfo],
    -- | If the request includes @Marker@ , the response returns that value in this field.
    marker :: Core.Maybe Types.Marker,
    -- | If a value is present, there are more file shares to return. In a subsequent request, use @NextMarker@ as the value for @Marker@ to retrieve the next set of file shares.
    nextMarker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFileSharesResponse' value with any optional fields omitted.
mkListFileSharesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListFileSharesResponse
mkListFileSharesResponse responseStatus =
  ListFileSharesResponse'
    { fileShareInfoList = Core.Nothing,
      marker = Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | An array of information about the file gateway's file shares.
--
-- /Note:/ Consider using 'fileShareInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsrrsFileShareInfoList :: Lens.Lens' ListFileSharesResponse (Core.Maybe [Types.FileShareInfo])
lfsrrsFileShareInfoList = Lens.field @"fileShareInfoList"
{-# DEPRECATED lfsrrsFileShareInfoList "Use generic-lens or generic-optics with 'fileShareInfoList' instead." #-}

-- | If the request includes @Marker@ , the response returns that value in this field.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsrrsMarker :: Lens.Lens' ListFileSharesResponse (Core.Maybe Types.Marker)
lfsrrsMarker = Lens.field @"marker"
{-# DEPRECATED lfsrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | If a value is present, there are more file shares to return. In a subsequent request, use @NextMarker@ as the value for @Marker@ to retrieve the next set of file shares.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsrrsNextMarker :: Lens.Lens' ListFileSharesResponse (Core.Maybe Types.Marker)
lfsrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lfsrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsrrsResponseStatus :: Lens.Lens' ListFileSharesResponse Core.Int
lfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
