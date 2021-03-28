{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a description of the certificate.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeCertificates
    (
    -- * Creating a request
      DescribeCertificates (..)
    , mkDescribeCertificates
    -- ** Request lenses
    , dcsFilters
    , dcsMarker
    , dcsMaxRecords

    -- * Destructuring the response
    , DescribeCertificatesResponse (..)
    , mkDescribeCertificatesResponse
    -- ** Response lenses
    , drsCertificates
    , drsMarker
    , drsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ Filters applied to the certificates described in the form of key-value pairs.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 10
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCertificates' value with any optional fields omitted.
mkDescribeCertificates
    :: DescribeCertificates
mkDescribeCertificates
  = DescribeCertificates'{filters = Core.Nothing,
                          marker = Core.Nothing, maxRecords = Core.Nothing}

-- | Filters applied to the certificates described in the form of key-value pairs.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsFilters :: Lens.Lens' DescribeCertificates (Core.Maybe [Types.Filter])
dcsFilters = Lens.field @"filters"
{-# INLINEABLE dcsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsMarker :: Lens.Lens' DescribeCertificates (Core.Maybe Core.Text)
dcsMarker = Lens.field @"marker"
{-# INLINEABLE dcsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 10
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsMaxRecords :: Lens.Lens' DescribeCertificates (Core.Maybe Core.Int)
dcsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dcsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeCertificates where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCertificates where
        toHeaders DescribeCertificates{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.DescribeCertificates")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCertificates where
        toJSON DescribeCertificates{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords])

instance Core.AWSRequest DescribeCertificates where
        type Rs DescribeCertificates = DescribeCertificatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCertificatesResponse' Core.<$>
                   (x Core..:? "Certificates") Core.<*> x Core..:? "Marker" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeCertificates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"certificates" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeCertificatesResponse' smart constructor.
data DescribeCertificatesResponse = DescribeCertificatesResponse'
  { certificates :: Core.Maybe [Types.Certificate]
    -- ^ The Secure Sockets Layer (SSL) certificates associated with the replication instance.
  , marker :: Core.Maybe Core.Text
    -- ^ The pagination token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCertificatesResponse' value with any optional fields omitted.
mkDescribeCertificatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCertificatesResponse
mkDescribeCertificatesResponse responseStatus
  = DescribeCertificatesResponse'{certificates = Core.Nothing,
                                  marker = Core.Nothing, responseStatus}

-- | The Secure Sockets Layer (SSL) certificates associated with the replication instance.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCertificates :: Lens.Lens' DescribeCertificatesResponse (Core.Maybe [Types.Certificate])
drsCertificates = Lens.field @"certificates"
{-# INLINEABLE drsCertificates #-}
{-# DEPRECATED certificates "Use generic-lens or generic-optics with 'certificates' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsMarker :: Lens.Lens' DescribeCertificatesResponse (Core.Maybe Core.Text)
drsMarker = Lens.field @"marker"
{-# INLINEABLE drsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCertificatesResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
