{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBEngineVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available DB engines.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBEngineVersions
  ( -- * Creating a request
    DescribeDBEngineVersions (..),
    mkDescribeDBEngineVersions,

    -- ** Request lenses
    ddbevDBParameterGroupFamily,
    ddbevDefaultOnly,
    ddbevEngine,
    ddbevEngineVersion,
    ddbevFilters,
    ddbevIncludeAll,
    ddbevListSupportedCharacterSets,
    ddbevListSupportedTimezones,
    ddbevMarker,
    ddbevMaxRecords,

    -- * Destructuring the response
    DescribeDBEngineVersionsResponse (..),
    mkDescribeDBEngineVersionsResponse,

    -- ** Response lenses
    ddbevrrsDBEngineVersions,
    ddbevrrsMarker,
    ddbevrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDBEngineVersions' smart constructor.
data DescribeDBEngineVersions = DescribeDBEngineVersions'
  { -- | The name of a specific DB parameter group family to return details for.
    --
    -- Constraints:
    --
    --     * If supplied, must match an existing DBParameterGroupFamily.
    dBParameterGroupFamily :: Core.Maybe Types.String,
    -- | A value that indicates whether only the default version of the specified engine or engine and major version combination is returned.
    defaultOnly :: Core.Maybe Core.Bool,
    -- | The database engine to return.
    engine :: Core.Maybe Types.String,
    -- | The database engine version to return.
    --
    -- Example: @5.1.49@
    engineVersion :: Core.Maybe Types.String,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | A value that indicates whether to include engine versions that aren't available in the list. The default is to list only available engine versions.
    includeAll :: Core.Maybe Core.Bool,
    -- | A value that indicates whether to list the supported character sets for each engine version.
    --
    -- If this parameter is enabled and the requested engine supports the @CharacterSetName@ parameter for @CreateDBInstance@ , the response includes a list of supported character sets for each engine version.
    listSupportedCharacterSets :: Core.Maybe Core.Bool,
    -- | A value that indicates whether to list the supported time zones for each engine version.
    --
    -- If this parameter is enabled and the requested engine supports the @TimeZone@ parameter for @CreateDBInstance@ , the response includes a list of supported time zones for each engine version.
    listSupportedTimezones :: Core.Maybe Core.Bool,
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBEngineVersions' value with any optional fields omitted.
mkDescribeDBEngineVersions ::
  DescribeDBEngineVersions
mkDescribeDBEngineVersions =
  DescribeDBEngineVersions'
    { dBParameterGroupFamily = Core.Nothing,
      defaultOnly = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      filters = Core.Nothing,
      includeAll = Core.Nothing,
      listSupportedCharacterSets = Core.Nothing,
      listSupportedTimezones = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of a specific DB parameter group family to return details for.
--
-- Constraints:
--
--     * If supplied, must match an existing DBParameterGroupFamily.
--
--
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevDBParameterGroupFamily :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Types.String)
ddbevDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# DEPRECATED ddbevDBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead." #-}

-- | A value that indicates whether only the default version of the specified engine or engine and major version combination is returned.
--
-- /Note:/ Consider using 'defaultOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevDefaultOnly :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Bool)
ddbevDefaultOnly = Lens.field @"defaultOnly"
{-# DEPRECATED ddbevDefaultOnly "Use generic-lens or generic-optics with 'defaultOnly' instead." #-}

-- | The database engine to return.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevEngine :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Types.String)
ddbevEngine = Lens.field @"engine"
{-# DEPRECATED ddbevEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The database engine version to return.
--
-- Example: @5.1.49@
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevEngineVersion :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Types.String)
ddbevEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED ddbevEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevFilters :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe [Types.Filter])
ddbevFilters = Lens.field @"filters"
{-# DEPRECATED ddbevFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A value that indicates whether to include engine versions that aren't available in the list. The default is to list only available engine versions.
--
-- /Note:/ Consider using 'includeAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevIncludeAll :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Bool)
ddbevIncludeAll = Lens.field @"includeAll"
{-# DEPRECATED ddbevIncludeAll "Use generic-lens or generic-optics with 'includeAll' instead." #-}

-- | A value that indicates whether to list the supported character sets for each engine version.
--
-- If this parameter is enabled and the requested engine supports the @CharacterSetName@ parameter for @CreateDBInstance@ , the response includes a list of supported character sets for each engine version.
--
-- /Note:/ Consider using 'listSupportedCharacterSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevListSupportedCharacterSets :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Bool)
ddbevListSupportedCharacterSets = Lens.field @"listSupportedCharacterSets"
{-# DEPRECATED ddbevListSupportedCharacterSets "Use generic-lens or generic-optics with 'listSupportedCharacterSets' instead." #-}

-- | A value that indicates whether to list the supported time zones for each engine version.
--
-- If this parameter is enabled and the requested engine supports the @TimeZone@ parameter for @CreateDBInstance@ , the response includes a list of supported time zones for each engine version.
--
-- /Note:/ Consider using 'listSupportedTimezones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevListSupportedTimezones :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Bool)
ddbevListSupportedTimezones = Lens.field @"listSupportedTimezones"
{-# DEPRECATED ddbevListSupportedTimezones "Use generic-lens or generic-optics with 'listSupportedTimezones' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevMarker :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Types.String)
ddbevMarker = Lens.field @"marker"
{-# DEPRECATED ddbevMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevMaxRecords :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Int)
ddbevMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbevMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeDBEngineVersions where
  type Rs DescribeDBEngineVersions = DescribeDBEngineVersionsResponse
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
            ( Core.pure ("Action", "DescribeDBEngineVersions")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue "DBParameterGroupFamily"
                            Core.<$> dBParameterGroupFamily
                        )
                Core.<> (Core.toQueryValue "DefaultOnly" Core.<$> defaultOnly)
                Core.<> (Core.toQueryValue "Engine" Core.<$> engine)
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "IncludeAll" Core.<$> includeAll)
                Core.<> ( Core.toQueryValue "ListSupportedCharacterSets"
                            Core.<$> listSupportedCharacterSets
                        )
                Core.<> ( Core.toQueryValue "ListSupportedTimezones"
                            Core.<$> listSupportedTimezones
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeDBEngineVersionsResult"
      ( \s h x ->
          DescribeDBEngineVersionsResponse'
            Core.<$> ( x Core..@? "DBEngineVersions"
                         Core..<@> Core.parseXMLList "DBEngineVersion"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBEngineVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"dBEngineVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeDBEngineVersions@ action.
--
-- /See:/ 'mkDescribeDBEngineVersionsResponse' smart constructor.
data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse'
  { -- | A list of @DBEngineVersion@ elements.
    dBEngineVersions :: Core.Maybe [Types.DBEngineVersion],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBEngineVersionsResponse' value with any optional fields omitted.
mkDescribeDBEngineVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBEngineVersionsResponse
mkDescribeDBEngineVersionsResponse responseStatus =
  DescribeDBEngineVersionsResponse'
    { dBEngineVersions =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of @DBEngineVersion@ elements.
--
-- /Note:/ Consider using 'dBEngineVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevrrsDBEngineVersions :: Lens.Lens' DescribeDBEngineVersionsResponse (Core.Maybe [Types.DBEngineVersion])
ddbevrrsDBEngineVersions = Lens.field @"dBEngineVersions"
{-# DEPRECATED ddbevrrsDBEngineVersions "Use generic-lens or generic-optics with 'dBEngineVersions' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevrrsMarker :: Lens.Lens' DescribeDBEngineVersionsResponse (Core.Maybe Types.String)
ddbevrrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbevrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevrrsResponseStatus :: Lens.Lens' DescribeDBEngineVersionsResponse Core.Int
ddbevrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbevrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
