{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeDBEngineVersions (..)
    , mkDescribeDBEngineVersions
    -- ** Request lenses
    , ddbevDBParameterGroupFamily
    , ddbevDefaultOnly
    , ddbevEngine
    , ddbevEngineVersion
    , ddbevFilters
    , ddbevIncludeAll
    , ddbevListSupportedCharacterSets
    , ddbevListSupportedTimezones
    , ddbevMarker
    , ddbevMaxRecords

    -- * Destructuring the response
    , DescribeDBEngineVersionsResponse (..)
    , mkDescribeDBEngineVersionsResponse
    -- ** Response lenses
    , ddbevrrsDBEngineVersions
    , ddbevrrsMarker
    , ddbevrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDBEngineVersions' smart constructor.
data DescribeDBEngineVersions = DescribeDBEngineVersions'
  { dBParameterGroupFamily :: Core.Maybe Core.Text
    -- ^ The name of a specific DB parameter group family to return details for.
--
-- Constraints:
--
--     * If supplied, must match an existing DBParameterGroupFamily.
--
--
  , defaultOnly :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether only the default version of the specified engine or engine and major version combination is returned.
  , engine :: Core.Maybe Core.Text
    -- ^ The database engine to return.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The database engine version to return.
--
-- Example: @5.1.49@ 
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , includeAll :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to include engine versions that aren't available in the list. The default is to list only available engine versions.
  , listSupportedCharacterSets :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to list the supported character sets for each engine version.
--
-- If this parameter is enabled and the requested engine supports the @CharacterSetName@ parameter for @CreateDBInstance@ , the response includes a list of supported character sets for each engine version. 
  , listSupportedTimezones :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to list the supported time zones for each engine version.
--
-- If this parameter is enabled and the requested engine supports the @TimeZone@ parameter for @CreateDBInstance@ , the response includes a list of supported time zones for each engine version. 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBEngineVersions' value with any optional fields omitted.
mkDescribeDBEngineVersions
    :: DescribeDBEngineVersions
mkDescribeDBEngineVersions
  = DescribeDBEngineVersions'{dBParameterGroupFamily = Core.Nothing,
                              defaultOnly = Core.Nothing, engine = Core.Nothing,
                              engineVersion = Core.Nothing, filters = Core.Nothing,
                              includeAll = Core.Nothing,
                              listSupportedCharacterSets = Core.Nothing,
                              listSupportedTimezones = Core.Nothing, marker = Core.Nothing,
                              maxRecords = Core.Nothing}

-- | The name of a specific DB parameter group family to return details for.
--
-- Constraints:
--
--     * If supplied, must match an existing DBParameterGroupFamily.
--
--
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevDBParameterGroupFamily :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Text)
ddbevDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# INLINEABLE ddbevDBParameterGroupFamily #-}
{-# DEPRECATED dBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead"  #-}

-- | A value that indicates whether only the default version of the specified engine or engine and major version combination is returned.
--
-- /Note:/ Consider using 'defaultOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevDefaultOnly :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Bool)
ddbevDefaultOnly = Lens.field @"defaultOnly"
{-# INLINEABLE ddbevDefaultOnly #-}
{-# DEPRECATED defaultOnly "Use generic-lens or generic-optics with 'defaultOnly' instead"  #-}

-- | The database engine to return.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevEngine :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Text)
ddbevEngine = Lens.field @"engine"
{-# INLINEABLE ddbevEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The database engine version to return.
--
-- Example: @5.1.49@ 
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevEngineVersion :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Text)
ddbevEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE ddbevEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevFilters :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe [Types.Filter])
ddbevFilters = Lens.field @"filters"
{-# INLINEABLE ddbevFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | A value that indicates whether to include engine versions that aren't available in the list. The default is to list only available engine versions.
--
-- /Note:/ Consider using 'includeAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevIncludeAll :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Bool)
ddbevIncludeAll = Lens.field @"includeAll"
{-# INLINEABLE ddbevIncludeAll #-}
{-# DEPRECATED includeAll "Use generic-lens or generic-optics with 'includeAll' instead"  #-}

-- | A value that indicates whether to list the supported character sets for each engine version.
--
-- If this parameter is enabled and the requested engine supports the @CharacterSetName@ parameter for @CreateDBInstance@ , the response includes a list of supported character sets for each engine version. 
--
-- /Note:/ Consider using 'listSupportedCharacterSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevListSupportedCharacterSets :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Bool)
ddbevListSupportedCharacterSets = Lens.field @"listSupportedCharacterSets"
{-# INLINEABLE ddbevListSupportedCharacterSets #-}
{-# DEPRECATED listSupportedCharacterSets "Use generic-lens or generic-optics with 'listSupportedCharacterSets' instead"  #-}

-- | A value that indicates whether to list the supported time zones for each engine version.
--
-- If this parameter is enabled and the requested engine supports the @TimeZone@ parameter for @CreateDBInstance@ , the response includes a list of supported time zones for each engine version. 
--
-- /Note:/ Consider using 'listSupportedTimezones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevListSupportedTimezones :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Bool)
ddbevListSupportedTimezones = Lens.field @"listSupportedTimezones"
{-# INLINEABLE ddbevListSupportedTimezones #-}
{-# DEPRECATED listSupportedTimezones "Use generic-lens or generic-optics with 'listSupportedTimezones' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevMarker :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Text)
ddbevMarker = Lens.field @"marker"
{-# INLINEABLE ddbevMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevMaxRecords :: Lens.Lens' DescribeDBEngineVersions (Core.Maybe Core.Int)
ddbevMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbevMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeDBEngineVersions where
        toQuery DescribeDBEngineVersions{..}
          = Core.toQueryPair "Action"
              ("DescribeDBEngineVersions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBParameterGroupFamily")
                dBParameterGroupFamily
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DefaultOnly") defaultOnly
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Engine") engine
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EngineVersion")
                engineVersion
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IncludeAll") includeAll
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ListSupportedCharacterSets")
                listSupportedCharacterSets
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ListSupportedTimezones")
                listSupportedTimezones
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeDBEngineVersions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBEngineVersions where
        type Rs DescribeDBEngineVersions = DescribeDBEngineVersionsResponse
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
          = Response.receiveXMLWrapper "DescribeDBEngineVersionsResult"
              (\ s h x ->
                 DescribeDBEngineVersionsResponse' Core.<$>
                   (x Core..@? "DBEngineVersions" Core..<@>
                      Core.parseXMLList "DBEngineVersion")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBEngineVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"dBEngineVersions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeDBEngineVersions@ action. 
--
-- /See:/ 'mkDescribeDBEngineVersionsResponse' smart constructor.
data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse'
  { dBEngineVersions :: Core.Maybe [Types.DBEngineVersion]
    -- ^ A list of @DBEngineVersion@ elements. 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBEngineVersionsResponse' value with any optional fields omitted.
mkDescribeDBEngineVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBEngineVersionsResponse
mkDescribeDBEngineVersionsResponse responseStatus
  = DescribeDBEngineVersionsResponse'{dBEngineVersions =
                                        Core.Nothing,
                                      marker = Core.Nothing, responseStatus}

-- | A list of @DBEngineVersion@ elements. 
--
-- /Note:/ Consider using 'dBEngineVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevrrsDBEngineVersions :: Lens.Lens' DescribeDBEngineVersionsResponse (Core.Maybe [Types.DBEngineVersion])
ddbevrrsDBEngineVersions = Lens.field @"dBEngineVersions"
{-# INLINEABLE ddbevrrsDBEngineVersions #-}
{-# DEPRECATED dBEngineVersions "Use generic-lens or generic-optics with 'dBEngineVersions' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevrrsMarker :: Lens.Lens' DescribeDBEngineVersionsResponse (Core.Maybe Core.Text)
ddbevrrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbevrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbevrrsResponseStatus :: Lens.Lens' DescribeDBEngineVersionsResponse Core.Int
ddbevrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbevrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
