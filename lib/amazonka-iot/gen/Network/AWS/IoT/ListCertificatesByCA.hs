{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListCertificatesByCA
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the device certificates signed by the specified CA certificate.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCertificatesByCA
  ( -- * Creating a request
    ListCertificatesByCA (..),
    mkListCertificatesByCA,

    -- ** Request lenses
    lcbcaCaCertificateId,
    lcbcaAscendingOrder,
    lcbcaMarker,
    lcbcaPageSize,

    -- * Destructuring the response
    ListCertificatesByCAResponse (..),
    mkListCertificatesByCAResponse,

    -- ** Response lenses
    lcbcarrsCertificates,
    lcbcarrsNextMarker,
    lcbcarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the ListCertificatesByCA operation.
--
-- /See:/ 'mkListCertificatesByCA' smart constructor.
data ListCertificatesByCA = ListCertificatesByCA'
  { -- | The ID of the CA certificate. This operation will list all registered device certificate that were signed by this CA certificate.
    caCertificateId :: Types.CertificateId,
    -- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
    ascendingOrder :: Core.Maybe Core.Bool,
    -- | The marker for the next set of results.
    marker :: Core.Maybe Types.Marker,
    -- | The result page size.
    pageSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCertificatesByCA' value with any optional fields omitted.
mkListCertificatesByCA ::
  -- | 'caCertificateId'
  Types.CertificateId ->
  ListCertificatesByCA
mkListCertificatesByCA caCertificateId =
  ListCertificatesByCA'
    { caCertificateId,
      ascendingOrder = Core.Nothing,
      marker = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | The ID of the CA certificate. This operation will list all registered device certificate that were signed by this CA certificate.
--
-- /Note:/ Consider using 'caCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcaCaCertificateId :: Lens.Lens' ListCertificatesByCA Types.CertificateId
lcbcaCaCertificateId = Lens.field @"caCertificateId"
{-# DEPRECATED lcbcaCaCertificateId "Use generic-lens or generic-optics with 'caCertificateId' instead." #-}

-- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcaAscendingOrder :: Lens.Lens' ListCertificatesByCA (Core.Maybe Core.Bool)
lcbcaAscendingOrder = Lens.field @"ascendingOrder"
{-# DEPRECATED lcbcaAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcaMarker :: Lens.Lens' ListCertificatesByCA (Core.Maybe Types.Marker)
lcbcaMarker = Lens.field @"marker"
{-# DEPRECATED lcbcaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcaPageSize :: Lens.Lens' ListCertificatesByCA (Core.Maybe Core.Natural)
lcbcaPageSize = Lens.field @"pageSize"
{-# DEPRECATED lcbcaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Core.AWSRequest ListCertificatesByCA where
  type Rs ListCertificatesByCA = ListCertificatesByCAResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/certificates-by-ca/" Core.<> (Core.toText caCertificateId)),
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
          ListCertificatesByCAResponse'
            Core.<$> (x Core..:? "certificates")
            Core.<*> (x Core..:? "nextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCertificatesByCA where
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

-- | The output of the ListCertificatesByCA operation.
--
-- /See:/ 'mkListCertificatesByCAResponse' smart constructor.
data ListCertificatesByCAResponse = ListCertificatesByCAResponse'
  { -- | The device certificates signed by the specified CA certificate.
    certificates :: Core.Maybe [Types.Certificate],
    -- | The marker for the next set of results, or null if there are no additional results.
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCertificatesByCAResponse' value with any optional fields omitted.
mkListCertificatesByCAResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCertificatesByCAResponse
mkListCertificatesByCAResponse responseStatus =
  ListCertificatesByCAResponse'
    { certificates = Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | The device certificates signed by the specified CA certificate.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcarrsCertificates :: Lens.Lens' ListCertificatesByCAResponse (Core.Maybe [Types.Certificate])
lcbcarrsCertificates = Lens.field @"certificates"
{-# DEPRECATED lcbcarrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The marker for the next set of results, or null if there are no additional results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcarrsNextMarker :: Lens.Lens' ListCertificatesByCAResponse (Core.Maybe Types.NextMarker)
lcbcarrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lcbcarrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcbcarrsResponseStatus :: Lens.Lens' ListCertificatesByCAResponse Core.Int
lcbcarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcbcarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
