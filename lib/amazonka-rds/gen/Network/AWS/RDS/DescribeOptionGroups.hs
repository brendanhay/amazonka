{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeOptionGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available option groups.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeOptionGroups
  ( -- * Creating a request
    DescribeOptionGroups (..),
    mkDescribeOptionGroups,

    -- ** Request lenses
    dogEngineName,
    dogFilters,
    dogMajorEngineVersion,
    dogMarker,
    dogMaxRecords,
    dogOptionGroupName,

    -- * Destructuring the response
    DescribeOptionGroupsResponse (..),
    mkDescribeOptionGroupsResponse,

    -- ** Response lenses
    dogrrsMarker,
    dogrrsOptionGroupsList,
    dogrrsResponseStatus,
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
-- /See:/ 'mkDescribeOptionGroups' smart constructor.
data DescribeOptionGroups = DescribeOptionGroups'
  { -- | Filters the list of option groups to only include groups associated with a specific database engine.
    engineName :: Core.Maybe Types.String,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | Filters the list of option groups to only include groups associated with a specific database engine version. If specified, then EngineName must also be specified.
    majorEngineVersion :: Core.Maybe Types.String,
    -- | An optional pagination token provided by a previous DescribeOptionGroups request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of the option group to describe. Can't be supplied together with EngineName or MajorEngineVersion.
    optionGroupName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOptionGroups' value with any optional fields omitted.
mkDescribeOptionGroups ::
  DescribeOptionGroups
mkDescribeOptionGroups =
  DescribeOptionGroups'
    { engineName = Core.Nothing,
      filters = Core.Nothing,
      majorEngineVersion = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      optionGroupName = Core.Nothing
    }

-- | Filters the list of option groups to only include groups associated with a specific database engine.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogEngineName :: Lens.Lens' DescribeOptionGroups (Core.Maybe Types.String)
dogEngineName = Lens.field @"engineName"
{-# DEPRECATED dogEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogFilters :: Lens.Lens' DescribeOptionGroups (Core.Maybe [Types.Filter])
dogFilters = Lens.field @"filters"
{-# DEPRECATED dogFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Filters the list of option groups to only include groups associated with a specific database engine version. If specified, then EngineName must also be specified.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogMajorEngineVersion :: Lens.Lens' DescribeOptionGroups (Core.Maybe Types.String)
dogMajorEngineVersion = Lens.field @"majorEngineVersion"
{-# DEPRECATED dogMajorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead." #-}

-- | An optional pagination token provided by a previous DescribeOptionGroups request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogMarker :: Lens.Lens' DescribeOptionGroups (Core.Maybe Types.String)
dogMarker = Lens.field @"marker"
{-# DEPRECATED dogMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogMaxRecords :: Lens.Lens' DescribeOptionGroups (Core.Maybe Core.Int)
dogMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dogMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the option group to describe. Can't be supplied together with EngineName or MajorEngineVersion.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogOptionGroupName :: Lens.Lens' DescribeOptionGroups (Core.Maybe Types.String)
dogOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED dogOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

instance Core.AWSRequest DescribeOptionGroups where
  type Rs DescribeOptionGroups = DescribeOptionGroupsResponse
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
            ( Core.pure ("Action", "DescribeOptionGroups")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "EngineName" Core.<$> engineName)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> ( Core.toQueryValue "MajorEngineVersion"
                            Core.<$> majorEngineVersion
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "OptionGroupName" Core.<$> optionGroupName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeOptionGroupsResult"
      ( \s h x ->
          DescribeOptionGroupsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "OptionGroupsList"
                         Core..<@> Core.parseXMLList "OptionGroup"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeOptionGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"optionGroupsList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | List of option groups.
--
-- /See:/ 'mkDescribeOptionGroupsResponse' smart constructor.
data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | List of option groups.
    optionGroupsList :: Core.Maybe [Types.OptionGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOptionGroupsResponse' value with any optional fields omitted.
mkDescribeOptionGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeOptionGroupsResponse
mkDescribeOptionGroupsResponse responseStatus =
  DescribeOptionGroupsResponse'
    { marker = Core.Nothing,
      optionGroupsList = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogrrsMarker :: Lens.Lens' DescribeOptionGroupsResponse (Core.Maybe Types.String)
dogrrsMarker = Lens.field @"marker"
{-# DEPRECATED dogrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | List of option groups.
--
-- /Note:/ Consider using 'optionGroupsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogrrsOptionGroupsList :: Lens.Lens' DescribeOptionGroupsResponse (Core.Maybe [Types.OptionGroup])
dogrrsOptionGroupsList = Lens.field @"optionGroupsList"
{-# DEPRECATED dogrrsOptionGroupsList "Use generic-lens or generic-optics with 'optionGroupsList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogrrsResponseStatus :: Lens.Lens' DescribeOptionGroupsResponse Core.Int
dogrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dogrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
