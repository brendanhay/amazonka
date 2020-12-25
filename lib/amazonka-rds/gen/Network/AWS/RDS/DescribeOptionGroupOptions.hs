{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeOptionGroupOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all available options.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeOptionGroupOptions
  ( -- * Creating a request
    DescribeOptionGroupOptions (..),
    mkDescribeOptionGroupOptions,

    -- ** Request lenses
    dogoEngineName,
    dogoFilters,
    dogoMajorEngineVersion,
    dogoMarker,
    dogoMaxRecords,

    -- * Destructuring the response
    DescribeOptionGroupOptionsResponse (..),
    mkDescribeOptionGroupOptionsResponse,

    -- ** Response lenses
    dogorrsMarker,
    dogorrsOptionGroupOptions,
    dogorrsResponseStatus,
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
-- /See:/ 'mkDescribeOptionGroupOptions' smart constructor.
data DescribeOptionGroupOptions = DescribeOptionGroupOptions'
  { -- | A required parameter. Options available for the given engine name are described.
    engineName :: Types.EngineName,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | If specified, filters the results to include only options for the specified major engine version.
    majorEngineVersion :: Core.Maybe Types.MajorEngineVersion,
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOptionGroupOptions' value with any optional fields omitted.
mkDescribeOptionGroupOptions ::
  -- | 'engineName'
  Types.EngineName ->
  DescribeOptionGroupOptions
mkDescribeOptionGroupOptions engineName =
  DescribeOptionGroupOptions'
    { engineName,
      filters = Core.Nothing,
      majorEngineVersion = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | A required parameter. Options available for the given engine name are described.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogoEngineName :: Lens.Lens' DescribeOptionGroupOptions Types.EngineName
dogoEngineName = Lens.field @"engineName"
{-# DEPRECATED dogoEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogoFilters :: Lens.Lens' DescribeOptionGroupOptions (Core.Maybe [Types.Filter])
dogoFilters = Lens.field @"filters"
{-# DEPRECATED dogoFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | If specified, filters the results to include only options for the specified major engine version.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogoMajorEngineVersion :: Lens.Lens' DescribeOptionGroupOptions (Core.Maybe Types.MajorEngineVersion)
dogoMajorEngineVersion = Lens.field @"majorEngineVersion"
{-# DEPRECATED dogoMajorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogoMarker :: Lens.Lens' DescribeOptionGroupOptions (Core.Maybe Types.Marker)
dogoMarker = Lens.field @"marker"
{-# DEPRECATED dogoMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogoMaxRecords :: Lens.Lens' DescribeOptionGroupOptions (Core.Maybe Core.Int)
dogoMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dogoMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeOptionGroupOptions where
  type
    Rs DescribeOptionGroupOptions =
      DescribeOptionGroupOptionsResponse
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
            ( Core.pure ("Action", "DescribeOptionGroupOptions")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "EngineName" engineName)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> ( Core.toQueryValue "MajorEngineVersion"
                            Core.<$> majorEngineVersion
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeOptionGroupOptionsResult"
      ( \s h x ->
          DescribeOptionGroupOptionsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "OptionGroupOptions"
                         Core..<@> Core.parseXMLList "OptionGroupOption"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeOptionGroupOptions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"optionGroupOptions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- |
--
-- /See:/ 'mkDescribeOptionGroupOptionsResponse' smart constructor.
data DescribeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    optionGroupOptions :: Core.Maybe [Types.OptionGroupOption],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOptionGroupOptionsResponse' value with any optional fields omitted.
mkDescribeOptionGroupOptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeOptionGroupOptionsResponse
mkDescribeOptionGroupOptionsResponse responseStatus =
  DescribeOptionGroupOptionsResponse'
    { marker = Core.Nothing,
      optionGroupOptions = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogorrsMarker :: Lens.Lens' DescribeOptionGroupOptionsResponse (Core.Maybe Types.Marker)
dogorrsMarker = Lens.field @"marker"
{-# DEPRECATED dogorrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroupOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogorrsOptionGroupOptions :: Lens.Lens' DescribeOptionGroupOptionsResponse (Core.Maybe [Types.OptionGroupOption])
dogorrsOptionGroupOptions = Lens.field @"optionGroupOptions"
{-# DEPRECATED dogorrsOptionGroupOptions "Use generic-lens or generic-optics with 'optionGroupOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogorrsResponseStatus :: Lens.Lens' DescribeOptionGroupOptionsResponse Core.Int
dogorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dogorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
