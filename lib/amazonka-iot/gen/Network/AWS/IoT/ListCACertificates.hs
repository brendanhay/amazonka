{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListCACertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the CA certificates registered for your AWS account.
--
-- The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCACertificates
  ( -- * Creating a request
    ListCACertificates (..),
    mkListCACertificates,

    -- ** Request lenses
    lcacAscendingOrder,
    lcacMarker,
    lcacPageSize,

    -- * Destructuring the response
    ListCACertificatesResponse (..),
    mkListCACertificatesResponse,

    -- ** Response lenses
    lcacrrsCertificates,
    lcacrrsNextMarker,
    lcacrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input for the ListCACertificates operation.
--
-- /See:/ 'mkListCACertificates' smart constructor.
data ListCACertificates = ListCACertificates'
  { -- | Determines the order of the results.
    ascendingOrder :: Core.Maybe Core.Bool,
    -- | The marker for the next set of results.
    marker :: Core.Maybe Types.Marker,
    -- | The result page size.
    pageSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCACertificates' value with any optional fields omitted.
mkListCACertificates ::
  ListCACertificates
mkListCACertificates =
  ListCACertificates'
    { ascendingOrder = Core.Nothing,
      marker = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | Determines the order of the results.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacAscendingOrder :: Lens.Lens' ListCACertificates (Core.Maybe Core.Bool)
lcacAscendingOrder = Lens.field @"ascendingOrder"
{-# DEPRECATED lcacAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacMarker :: Lens.Lens' ListCACertificates (Core.Maybe Types.Marker)
lcacMarker = Lens.field @"marker"
{-# DEPRECATED lcacMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacPageSize :: Lens.Lens' ListCACertificates (Core.Maybe Core.Natural)
lcacPageSize = Lens.field @"pageSize"
{-# DEPRECATED lcacPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Core.AWSRequest ListCACertificates where
  type Rs ListCACertificates = ListCACertificatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/cacertificates",
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
          ListCACertificatesResponse'
            Core.<$> (x Core..:? "certificates")
            Core.<*> (x Core..:? "nextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCACertificates where
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

-- | The output from the ListCACertificates operation.
--
-- /See:/ 'mkListCACertificatesResponse' smart constructor.
data ListCACertificatesResponse = ListCACertificatesResponse'
  { -- | The CA certificates registered in your AWS account.
    certificates :: Core.Maybe [Types.CACertificate],
    -- | The current position within the list of CA certificates.
    nextMarker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCACertificatesResponse' value with any optional fields omitted.
mkListCACertificatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCACertificatesResponse
mkListCACertificatesResponse responseStatus =
  ListCACertificatesResponse'
    { certificates = Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | The CA certificates registered in your AWS account.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacrrsCertificates :: Lens.Lens' ListCACertificatesResponse (Core.Maybe [Types.CACertificate])
lcacrrsCertificates = Lens.field @"certificates"
{-# DEPRECATED lcacrrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The current position within the list of CA certificates.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacrrsNextMarker :: Lens.Lens' ListCACertificatesResponse (Core.Maybe Types.Marker)
lcacrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lcacrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcacrrsResponseStatus :: Lens.Lens' ListCACertificatesResponse Core.Int
lcacrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcacrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
