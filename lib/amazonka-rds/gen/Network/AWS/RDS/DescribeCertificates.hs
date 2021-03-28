{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the set of CA certificates provided by Amazon RDS for this AWS account.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeCertificates
    (
    -- * Creating a request
      DescribeCertificates (..)
    , mkDescribeCertificates
    -- ** Request lenses
    , dcCertificateIdentifier
    , dcFilters
    , dcMarker
    , dcMaxRecords

    -- * Destructuring the response
    , DescribeCertificatesResponse (..)
    , mkDescribeCertificatesResponse
    -- ** Response lenses
    , dcrrsCertificates
    , dcrrsMarker
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'
  { certificateIdentifier :: Core.Maybe Core.Text
    -- ^ The user-supplied certificate identifier. If this parameter is specified, information for only the identified certificate is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match an existing CertificateIdentifier.
--
--
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeCertificates@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCertificates' value with any optional fields omitted.
mkDescribeCertificates
    :: DescribeCertificates
mkDescribeCertificates
  = DescribeCertificates'{certificateIdentifier = Core.Nothing,
                          filters = Core.Nothing, marker = Core.Nothing,
                          maxRecords = Core.Nothing}

-- | The user-supplied certificate identifier. If this parameter is specified, information for only the identified certificate is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match an existing CertificateIdentifier.
--
--
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCertificateIdentifier :: Lens.Lens' DescribeCertificates (Core.Maybe Core.Text)
dcCertificateIdentifier = Lens.field @"certificateIdentifier"
{-# INLINEABLE dcCertificateIdentifier #-}
{-# DEPRECATED certificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcFilters :: Lens.Lens' DescribeCertificates (Core.Maybe [Types.Filter])
dcFilters = Lens.field @"filters"
{-# INLINEABLE dcFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeCertificates@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMarker :: Lens.Lens' DescribeCertificates (Core.Maybe Core.Text)
dcMarker = Lens.field @"marker"
{-# INLINEABLE dcMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxRecords :: Lens.Lens' DescribeCertificates (Core.Maybe Core.Int)
dcMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dcMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeCertificates where
        toQuery DescribeCertificates{..}
          = Core.toQueryPair "Action" ("DescribeCertificates" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CertificateIdentifier")
                certificateIdentifier
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeCertificates where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeCertificates where
        type Rs DescribeCertificates = DescribeCertificatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeCertificatesResult"
              (\ s h x ->
                 DescribeCertificatesResponse' Core.<$>
                   (x Core..@? "Certificates" Core..<@>
                      Core.parseXMLList "Certificate")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
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

-- | Data returned by the __DescribeCertificates__ action.
--
-- /See:/ 'mkDescribeCertificatesResponse' smart constructor.
data DescribeCertificatesResponse = DescribeCertificatesResponse'
  { certificates :: Core.Maybe [Types.Certificate]
    -- ^ The list of @Certificate@ objects for the AWS account.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeCertificates@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
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

-- | The list of @Certificate@ objects for the AWS account.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCertificates :: Lens.Lens' DescribeCertificatesResponse (Core.Maybe [Types.Certificate])
dcrrsCertificates = Lens.field @"certificates"
{-# INLINEABLE dcrrsCertificates #-}
{-# DEPRECATED certificates "Use generic-lens or generic-optics with 'certificates' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeCertificates@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsMarker :: Lens.Lens' DescribeCertificatesResponse (Core.Maybe Core.Text)
dcrrsMarker = Lens.field @"marker"
{-# INLINEABLE dcrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeCertificatesResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
