{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeSourceRegions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the source AWS Regions where the current AWS Region can create a read replica or copy a DB snapshot from. This API action supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeSourceRegions
  ( -- * Creating a request
    DescribeSourceRegions (..),
    mkDescribeSourceRegions,

    -- ** Request lenses
    dsrFilters,
    dsrMarker,
    dsrMaxRecords,
    dsrRegionName,

    -- * Destructuring the response
    DescribeSourceRegionsResponse (..),
    mkDescribeSourceRegionsResponse,

    -- ** Response lenses
    dsrrrsMarker,
    dsrrrsSourceRegions,
    dsrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeSourceRegions' smart constructor.
data DescribeSourceRegions = DescribeSourceRegions'
  { -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous @DescribeSourceRegions@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The source AWS Region name. For example, @us-east-1@ .
    --
    -- Constraints:
    --
    --     * Must specify a valid AWS Region name.
    regionName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSourceRegions' value with any optional fields omitted.
mkDescribeSourceRegions ::
  DescribeSourceRegions
mkDescribeSourceRegions =
  DescribeSourceRegions'
    { filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      regionName = Core.Nothing
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrFilters :: Lens.Lens' DescribeSourceRegions (Core.Maybe [Types.Filter])
dsrFilters = Lens.field @"filters"
{-# DEPRECATED dsrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeSourceRegions@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrMarker :: Lens.Lens' DescribeSourceRegions (Core.Maybe Types.String)
dsrMarker = Lens.field @"marker"
{-# DEPRECATED dsrMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrMaxRecords :: Lens.Lens' DescribeSourceRegions (Core.Maybe Core.Int)
dsrMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dsrMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The source AWS Region name. For example, @us-east-1@ .
--
-- Constraints:
--
--     * Must specify a valid AWS Region name.
--
--
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrRegionName :: Lens.Lens' DescribeSourceRegions (Core.Maybe Types.String)
dsrRegionName = Lens.field @"regionName"
{-# DEPRECATED dsrRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Core.AWSRequest DescribeSourceRegions where
  type Rs DescribeSourceRegions = DescribeSourceRegionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeSourceRegions")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "RegionName" Core.<$> regionName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeSourceRegionsResult"
      ( \s h x ->
          DescribeSourceRegionsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "SourceRegions"
                         Core..<@> Core.parseXMLList "SourceRegion"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSourceRegions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"sourceRegions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeSourceRegions@ action.
--
-- /See:/ 'mkDescribeSourceRegionsResponse' smart constructor.
data DescribeSourceRegionsResponse = DescribeSourceRegionsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | A list of SourceRegion instances that contains each source AWS Region that the current AWS Region can get a read replica or a DB snapshot from.
    sourceRegions :: Core.Maybe [Types.SourceRegion],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSourceRegionsResponse' value with any optional fields omitted.
mkDescribeSourceRegionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSourceRegionsResponse
mkDescribeSourceRegionsResponse responseStatus =
  DescribeSourceRegionsResponse'
    { marker = Core.Nothing,
      sourceRegions = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsMarker :: Lens.Lens' DescribeSourceRegionsResponse (Core.Maybe Types.String)
dsrrrsMarker = Lens.field @"marker"
{-# DEPRECATED dsrrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of SourceRegion instances that contains each source AWS Region that the current AWS Region can get a read replica or a DB snapshot from.
--
-- /Note:/ Consider using 'sourceRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsSourceRegions :: Lens.Lens' DescribeSourceRegionsResponse (Core.Maybe [Types.SourceRegion])
dsrrrsSourceRegions = Lens.field @"sourceRegions"
{-# DEPRECATED dsrrrsSourceRegions "Use generic-lens or generic-optics with 'sourceRegions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsResponseStatus :: Lens.Lens' DescribeSourceRegionsResponse Core.Int
dsrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
