{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the certificates registered in your AWS account.
--
-- The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCertificates
  ( -- * Creating a request
    ListCertificates (..),
    mkListCertificates,

    -- ** Request lenses
    lcAscendingOrder,
    lcMarker,
    lcPageSize,

    -- * Destructuring the response
    ListCertificatesResponse (..),
    mkListCertificatesResponse,

    -- ** Response lenses
    lcrrsCertificates,
    lcrrsNextMarker,
    lcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListCertificates operation.
--
-- /See:/ 'mkListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { -- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
    ascendingOrder :: Core.Maybe Core.Bool,
    -- | The marker for the next set of results.
    marker :: Core.Maybe Types.Marker,
    -- | The result page size.
    pageSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCertificates' value with any optional fields omitted.
mkListCertificates ::
  ListCertificates
mkListCertificates =
  ListCertificates'
    { ascendingOrder = Core.Nothing,
      marker = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcAscendingOrder :: Lens.Lens' ListCertificates (Core.Maybe Core.Bool)
lcAscendingOrder = Lens.field @"ascendingOrder"
{-# DEPRECATED lcAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMarker :: Lens.Lens' ListCertificates (Core.Maybe Types.Marker)
lcMarker = Lens.field @"marker"
{-# DEPRECATED lcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcPageSize :: Lens.Lens' ListCertificates (Core.Maybe Core.Natural)
lcPageSize = Lens.field @"pageSize"
{-# DEPRECATED lcPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Core.AWSRequest ListCertificates where
  type Rs ListCertificates = ListCertificatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/certificates",
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
          ListCertificatesResponse'
            Core.<$> (x Core..:? "certificates")
            Core.<*> (x Core..:? "nextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCertificates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"certificates" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | The output of the ListCertificates operation.
--
-- /See:/ 'mkListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | The descriptions of the certificates.
    certificates :: Core.Maybe [Types.Certificate],
    -- | The marker for the next set of results, or null if there are no additional results.
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCertificatesResponse' value with any optional fields omitted.
mkListCertificatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCertificatesResponse
mkListCertificatesResponse responseStatus =
  ListCertificatesResponse'
    { certificates = Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | The descriptions of the certificates.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsCertificates :: Lens.Lens' ListCertificatesResponse (Core.Maybe [Types.Certificate])
lcrrsCertificates = Lens.field @"certificates"
{-# DEPRECATED lcrrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The marker for the next set of results, or null if there are no additional results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextMarker :: Lens.Lens' ListCertificatesResponse (Core.Maybe Types.NextMarker)
lcrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lcrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListCertificatesResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
