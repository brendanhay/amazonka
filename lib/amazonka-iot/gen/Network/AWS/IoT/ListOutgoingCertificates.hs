{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListOutgoingCertificates (..)
    , mkListOutgoingCertificates
    -- ** Request lenses
    , locAscendingOrder
    , locMarker
    , locPageSize

    -- * Destructuring the response
    , ListOutgoingCertificatesResponse (..)
    , mkListOutgoingCertificatesResponse
    -- ** Response lenses
    , locrrsNextMarker
    , locrrsOutgoingCertificates
    , locrrsResponseStatus
    ) where

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
  { ascendingOrder :: Core.Maybe Core.Bool
    -- ^ Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
  , marker :: Core.Maybe Types.Marker
    -- ^ The marker for the next set of results.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The result page size.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOutgoingCertificates' value with any optional fields omitted.
mkListOutgoingCertificates
    :: ListOutgoingCertificates
mkListOutgoingCertificates
  = ListOutgoingCertificates'{ascendingOrder = Core.Nothing,
                              marker = Core.Nothing, pageSize = Core.Nothing}

-- | Specifies the order for results. If True, the results are returned in ascending order, based on the creation date.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locAscendingOrder :: Lens.Lens' ListOutgoingCertificates (Core.Maybe Core.Bool)
locAscendingOrder = Lens.field @"ascendingOrder"
{-# INLINEABLE locAscendingOrder #-}
{-# DEPRECATED ascendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead"  #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locMarker :: Lens.Lens' ListOutgoingCertificates (Core.Maybe Types.Marker)
locMarker = Lens.field @"marker"
{-# INLINEABLE locMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locPageSize :: Lens.Lens' ListOutgoingCertificates (Core.Maybe Core.Natural)
locPageSize = Lens.field @"pageSize"
{-# INLINEABLE locPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

instance Core.ToQuery ListOutgoingCertificates where
        toQuery ListOutgoingCertificates{..}
          = Core.maybe Core.mempty (Core.toQueryPair "isAscendingOrder")
              ascendingOrder
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "pageSize") pageSize

instance Core.ToHeaders ListOutgoingCertificates where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListOutgoingCertificates where
        type Rs ListOutgoingCertificates = ListOutgoingCertificatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/certificates-out-going",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListOutgoingCertificatesResponse' Core.<$>
                   (x Core..:? "nextMarker") Core.<*>
                     x Core..:? "outgoingCertificates"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListOutgoingCertificates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"outgoingCertificates" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | The output from the ListOutgoingCertificates operation.
--
-- /See:/ 'mkListOutgoingCertificatesResponse' smart constructor.
data ListOutgoingCertificatesResponse = ListOutgoingCertificatesResponse'
  { nextMarker :: Core.Maybe Types.Marker
    -- ^ The marker for the next set of results.
  , outgoingCertificates :: Core.Maybe [Types.OutgoingCertificate]
    -- ^ The certificates that are being transferred but not yet accepted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListOutgoingCertificatesResponse' value with any optional fields omitted.
mkListOutgoingCertificatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListOutgoingCertificatesResponse
mkListOutgoingCertificatesResponse responseStatus
  = ListOutgoingCertificatesResponse'{nextMarker = Core.Nothing,
                                      outgoingCertificates = Core.Nothing, responseStatus}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsNextMarker :: Lens.Lens' ListOutgoingCertificatesResponse (Core.Maybe Types.Marker)
locrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE locrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The certificates that are being transferred but not yet accepted.
--
-- /Note:/ Consider using 'outgoingCertificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsOutgoingCertificates :: Lens.Lens' ListOutgoingCertificatesResponse (Core.Maybe [Types.OutgoingCertificate])
locrrsOutgoingCertificates = Lens.field @"outgoingCertificates"
{-# INLINEABLE locrrsOutgoingCertificates #-}
{-# DEPRECATED outgoingCertificates "Use generic-lens or generic-optics with 'outgoingCertificates' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsResponseStatus :: Lens.Lens' ListOutgoingCertificatesResponse Core.Int
locrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE locrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
