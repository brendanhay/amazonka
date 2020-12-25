{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB parameter group.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBParameters
  ( -- * Creating a request
    DescribeDBParameters (..),
    mkDescribeDBParameters,

    -- ** Request lenses
    ddbpDBParameterGroupName,
    ddbpFilters,
    ddbpMarker,
    ddbpMaxRecords,
    ddbpSource,

    -- * Destructuring the response
    DescribeDBParametersResponse (..),
    mkDescribeDBParametersResponse,

    -- ** Response lenses
    ddbprfrsMarker,
    ddbprfrsParameters,
    ddbprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDBParameters' smart constructor.
data DescribeDBParameters = DescribeDBParameters'
  { -- | The name of a specific DB parameter group to return details for.
    --
    -- Constraints:
    --
    --     * If supplied, must match the name of an existing DBParameterGroup.
    dBParameterGroupName :: Types.DBParameterGroupName,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous @DescribeDBParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The parameter types to return.
    --
    -- Default: All parameter types returned
    -- Valid Values: @user | system | engine-default@
    source :: Core.Maybe Types.Source
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBParameters' value with any optional fields omitted.
mkDescribeDBParameters ::
  -- | 'dBParameterGroupName'
  Types.DBParameterGroupName ->
  DescribeDBParameters
mkDescribeDBParameters dBParameterGroupName =
  DescribeDBParameters'
    { dBParameterGroupName,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      source = Core.Nothing
    }

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBParameterGroup.
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpDBParameterGroupName :: Lens.Lens' DescribeDBParameters Types.DBParameterGroupName
ddbpDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# DEPRECATED ddbpDBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpFilters :: Lens.Lens' DescribeDBParameters (Core.Maybe [Types.Filter])
ddbpFilters = Lens.field @"filters"
{-# DEPRECATED ddbpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpMarker :: Lens.Lens' DescribeDBParameters (Core.Maybe Types.Marker)
ddbpMarker = Lens.field @"marker"
{-# DEPRECATED ddbpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpMaxRecords :: Lens.Lens' DescribeDBParameters (Core.Maybe Core.Int)
ddbpMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The parameter types to return.
--
-- Default: All parameter types returned
-- Valid Values: @user | system | engine-default@
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpSource :: Lens.Lens' DescribeDBParameters (Core.Maybe Types.Source)
ddbpSource = Lens.field @"source"
{-# DEPRECATED ddbpSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.AWSRequest DescribeDBParameters where
  type Rs DescribeDBParameters = DescribeDBParametersResponse
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
            ( Core.pure ("Action", "DescribeDBParameters")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBParameterGroupName" dBParameterGroupName)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "Source" Core.<$> source)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeDBParametersResult"
      ( \s h x ->
          DescribeDBParametersResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "Parameter")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBParameters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeDBParameters@ action.
--
-- /See:/ 'mkDescribeDBParametersResponse' smart constructor.
data DescribeDBParametersResponse = DescribeDBParametersResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | A list of @Parameter@ values.
    parameters :: Core.Maybe [Types.Parameter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBParametersResponse' value with any optional fields omitted.
mkDescribeDBParametersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBParametersResponse
mkDescribeDBParametersResponse responseStatus =
  DescribeDBParametersResponse'
    { marker = Core.Nothing,
      parameters = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbprfrsMarker :: Lens.Lens' DescribeDBParametersResponse (Core.Maybe Types.String)
ddbprfrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbprfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of @Parameter@ values.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbprfrsParameters :: Lens.Lens' DescribeDBParametersResponse (Core.Maybe [Types.Parameter])
ddbprfrsParameters = Lens.field @"parameters"
{-# DEPRECATED ddbprfrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbprfrsResponseStatus :: Lens.Lens' DescribeDBParametersResponse Core.Int
ddbprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
