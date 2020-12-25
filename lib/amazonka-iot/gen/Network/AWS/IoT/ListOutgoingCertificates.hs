{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListOutgoingCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists certificates that are being transferred but not yet accepted.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListOutgoingCertificates
  ( -- * Creating a request
    ListOutgoingCertificates (..),
    mkListOutgoingCertificates,

    -- ** Request lenses
    locAscendingOrder,
    locMarker,
    locPageSize,

    -- * Destructuring the response
    ListOutgoingCertificatesResponse (..),
    mkListOutgoingCertificatesResponse,

    -- ** Response lenses
    locrrsNextMarker,
    locrrsOutgoingCertificates,
    locrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the ListOutgoingCertificates operation.
--
-- /See:/ 'mkListOutgoingCertificates' smart constructor.
data ListOutgoingCertificates = ListOutgoingCertificates'
  { -- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
    ascendingOrder :: Core.Maybe Core.Bool,
    -- | The marker for the next set of results.
    marker :: Core.Maybe Types.Marker,
    -- | The result page size.
    pageSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOutgoingCertificates' value with any optional fields omitted.
mkListOutgoingCertificates ::
  ListOutgoingCertificates
mkListOutgoingCertificates =
  ListOutgoingCertificates'
    { ascendingOrder = Core.Nothing,
      marker = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locAscendingOrder :: Lens.Lens' ListOutgoingCertificates (Core.Maybe Core.Bool)
locAscendingOrder = Lens.field @"ascendingOrder"
{-# DEPRECATED locAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locMarker :: Lens.Lens' ListOutgoingCertificates (Core.Maybe Types.Marker)
locMarker = Lens.field @"marker"
{-# DEPRECATED locMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locPageSize :: Lens.Lens' ListOutgoingCertificates (Core.Maybe Core.Natural)
locPageSize = Lens.field @"pageSize"
{-# DEPRECATED locPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Core.AWSRequest ListOutgoingCertificates where
  type Rs ListOutgoingCertificates = ListOutgoingCertificatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/certificates-out-going",
        Core._rqQuery =
          Core.toQueryValue "isAscendingOrder" Core.<$> ascendingOrder
            Core.<> (Core.toQueryValue "marker" Core.<$> marker)
            Core.<> (Core.toQueryValue "pageSize" Core.<$> pageSize),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOutgoingCertificatesResponse'
            Core.<$> (x Core..:? "nextMarker")
            Core.<*> (x Core..:? "outgoingCertificates")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListOutgoingCertificates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"outgoingCertificates" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | The output from the ListOutgoingCertificates operation.
--
-- /See:/ 'mkListOutgoingCertificatesResponse' smart constructor.
data ListOutgoingCertificatesResponse = ListOutgoingCertificatesResponse'
  { -- | The marker for the next set of results.
    nextMarker :: Core.Maybe Types.Marker,
    -- | The certificates that are being transferred but not yet accepted.
    outgoingCertificates :: Core.Maybe [Types.OutgoingCertificate],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListOutgoingCertificatesResponse' value with any optional fields omitted.
mkListOutgoingCertificatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListOutgoingCertificatesResponse
mkListOutgoingCertificatesResponse responseStatus =
  ListOutgoingCertificatesResponse'
    { nextMarker = Core.Nothing,
      outgoingCertificates = Core.Nothing,
      responseStatus
    }

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsNextMarker :: Lens.Lens' ListOutgoingCertificatesResponse (Core.Maybe Types.Marker)
locrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED locrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The certificates that are being transferred but not yet accepted.
--
-- /Note:/ Consider using 'outgoingCertificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsOutgoingCertificates :: Lens.Lens' ListOutgoingCertificatesResponse (Core.Maybe [Types.OutgoingCertificate])
locrrsOutgoingCertificates = Lens.field @"outgoingCertificates"
{-# DEPRECATED locrrsOutgoingCertificates "Use generic-lens or generic-optics with 'outgoingCertificates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsResponseStatus :: Lens.Lens' ListOutgoingCertificatesResponse Core.Int
locrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED locrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
