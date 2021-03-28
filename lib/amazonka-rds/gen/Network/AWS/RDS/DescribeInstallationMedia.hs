{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeInstallationMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available installation media for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeInstallationMedia
    (
    -- * Creating a request
      DescribeInstallationMedia (..)
    , mkDescribeInstallationMedia
    -- ** Request lenses
    , dimFilters
    , dimInstallationMediaId
    , dimMarker
    , dimMaxRecords

    -- * Destructuring the response
    , DescribeInstallationMediaResponse (..)
    , mkDescribeInstallationMediaResponse
    -- ** Response lenses
    , dimrrsInstallationMedia
    , dimrrsMarker
    , dimrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstallationMedia' smart constructor.
data DescribeInstallationMedia = DescribeInstallationMedia'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ A filter that specifies one or more installation media to describe. Supported filters include the following:
--
--
--     * @custom-availability-zone-id@ - Accepts custom Availability Zone (AZ) identifiers. The results list includes information about only the custom AZs identified by these identifiers.
--
--
--     * @engine@ - Accepts database engines. The results list includes information about only the database engines identified by these identifiers.
-- For more information about the valid engines for installation media, see 'ImportInstallationMedia' .
--
--
  , installationMediaId :: Core.Maybe Core.Text
    -- ^ The installation medium ID.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ An optional pagination token provided by a previous DescribeInstallationMedia request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstallationMedia' value with any optional fields omitted.
mkDescribeInstallationMedia
    :: DescribeInstallationMedia
mkDescribeInstallationMedia
  = DescribeInstallationMedia'{filters = Core.Nothing,
                               installationMediaId = Core.Nothing, marker = Core.Nothing,
                               maxRecords = Core.Nothing}

-- | A filter that specifies one or more installation media to describe. Supported filters include the following:
--
--
--     * @custom-availability-zone-id@ - Accepts custom Availability Zone (AZ) identifiers. The results list includes information about only the custom AZs identified by these identifiers.
--
--
--     * @engine@ - Accepts database engines. The results list includes information about only the database engines identified by these identifiers.
-- For more information about the valid engines for installation media, see 'ImportInstallationMedia' .
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimFilters :: Lens.Lens' DescribeInstallationMedia (Core.Maybe [Types.Filter])
dimFilters = Lens.field @"filters"
{-# INLINEABLE dimFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The installation medium ID.
--
-- /Note:/ Consider using 'installationMediaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimInstallationMediaId :: Lens.Lens' DescribeInstallationMedia (Core.Maybe Core.Text)
dimInstallationMediaId = Lens.field @"installationMediaId"
{-# INLINEABLE dimInstallationMediaId #-}
{-# DEPRECATED installationMediaId "Use generic-lens or generic-optics with 'installationMediaId' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimMarker :: Lens.Lens' DescribeInstallationMedia (Core.Maybe Core.Text)
dimMarker = Lens.field @"marker"
{-# INLINEABLE dimMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | An optional pagination token provided by a previous DescribeInstallationMedia request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimMaxRecords :: Lens.Lens' DescribeInstallationMedia (Core.Maybe Core.Int)
dimMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dimMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeInstallationMedia where
        toQuery DescribeInstallationMedia{..}
          = Core.toQueryPair "Action"
              ("DescribeInstallationMedia" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstallationMediaId")
                installationMediaId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeInstallationMedia where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeInstallationMedia where
        type Rs DescribeInstallationMedia =
             DescribeInstallationMediaResponse
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
          = Response.receiveXMLWrapper "DescribeInstallationMediaResult"
              (\ s h x ->
                 DescribeInstallationMediaResponse' Core.<$>
                   (x Core..@? "InstallationMedia" Core..<@>
                      Core.parseXMLList "InstallationMedia")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeInstallationMedia where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"installationMedia" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeInstallationMediaResponse' smart constructor.
data DescribeInstallationMediaResponse = DescribeInstallationMediaResponse'
  { installationMedia :: Core.Maybe [Types.InstallationMedia]
    -- ^ The list of 'InstallationMedia' objects for the AWS account.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous 'DescribeInstallationMedia' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstallationMediaResponse' value with any optional fields omitted.
mkDescribeInstallationMediaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInstallationMediaResponse
mkDescribeInstallationMediaResponse responseStatus
  = DescribeInstallationMediaResponse'{installationMedia =
                                         Core.Nothing,
                                       marker = Core.Nothing, responseStatus}

-- | The list of 'InstallationMedia' objects for the AWS account.
--
-- /Note:/ Consider using 'installationMedia' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimrrsInstallationMedia :: Lens.Lens' DescribeInstallationMediaResponse (Core.Maybe [Types.InstallationMedia])
dimrrsInstallationMedia = Lens.field @"installationMedia"
{-# INLINEABLE dimrrsInstallationMedia #-}
{-# DEPRECATED installationMedia "Use generic-lens or generic-optics with 'installationMedia' instead"  #-}

-- | An optional pagination token provided by a previous 'DescribeInstallationMedia' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimrrsMarker :: Lens.Lens' DescribeInstallationMediaResponse (Core.Maybe Core.Text)
dimrrsMarker = Lens.field @"marker"
{-# INLINEABLE dimrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dimrrsResponseStatus :: Lens.Lens' DescribeInstallationMediaResponse Core.Int
dimrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dimrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
