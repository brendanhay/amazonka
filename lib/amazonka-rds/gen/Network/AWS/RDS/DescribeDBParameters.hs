{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeDBParameters (..)
    , mkDescribeDBParameters
    -- ** Request lenses
    , ddbpDBParameterGroupName
    , ddbpFilters
    , ddbpMarker
    , ddbpMaxRecords
    , ddbpSource

    -- * Destructuring the response
    , DescribeDBParametersResponse (..)
    , mkDescribeDBParametersResponse
    -- ** Response lenses
    , ddbprfrsMarker
    , ddbprfrsParameters
    , ddbprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDBParameters' smart constructor.
data DescribeDBParameters = DescribeDBParameters'
  { dBParameterGroupName :: Core.Text
    -- ^ The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBParameterGroup.
--
--
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , source :: Core.Maybe Core.Text
    -- ^ The parameter types to return.
--
-- Default: All parameter types returned
-- Valid Values: @user | system | engine-default@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBParameters' value with any optional fields omitted.
mkDescribeDBParameters
    :: Core.Text -- ^ 'dBParameterGroupName'
    -> DescribeDBParameters
mkDescribeDBParameters dBParameterGroupName
  = DescribeDBParameters'{dBParameterGroupName,
                          filters = Core.Nothing, marker = Core.Nothing,
                          maxRecords = Core.Nothing, source = Core.Nothing}

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBParameterGroup.
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpDBParameterGroupName :: Lens.Lens' DescribeDBParameters Core.Text
ddbpDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE ddbpDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpFilters :: Lens.Lens' DescribeDBParameters (Core.Maybe [Types.Filter])
ddbpFilters = Lens.field @"filters"
{-# INLINEABLE ddbpFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpMarker :: Lens.Lens' DescribeDBParameters (Core.Maybe Core.Text)
ddbpMarker = Lens.field @"marker"
{-# INLINEABLE ddbpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpMaxRecords :: Lens.Lens' DescribeDBParameters (Core.Maybe Core.Int)
ddbpMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbpMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The parameter types to return.
--
-- Default: All parameter types returned
-- Valid Values: @user | system | engine-default@ 
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpSource :: Lens.Lens' DescribeDBParameters (Core.Maybe Core.Text)
ddbpSource = Lens.field @"source"
{-# INLINEABLE ddbpSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

instance Core.ToQuery DescribeDBParameters where
        toQuery DescribeDBParameters{..}
          = Core.toQueryPair "Action" ("DescribeDBParameters" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBParameterGroupName" dBParameterGroupName
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Source") source

instance Core.ToHeaders DescribeDBParameters where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBParameters where
        type Rs DescribeDBParameters = DescribeDBParametersResponse
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
          = Response.receiveXMLWrapper "DescribeDBParametersResult"
              (\ s h x ->
                 DescribeDBParametersResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "Parameters" Core..<@> Core.parseXMLList "Parameter"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBParameters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeDBParameters@ action. 
--
-- /See:/ 'mkDescribeDBParametersResponse' smart constructor.
data DescribeDBParametersResponse = DescribeDBParametersResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ A list of @Parameter@ values. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBParametersResponse' value with any optional fields omitted.
mkDescribeDBParametersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBParametersResponse
mkDescribeDBParametersResponse responseStatus
  = DescribeDBParametersResponse'{marker = Core.Nothing,
                                  parameters = Core.Nothing, responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbprfrsMarker :: Lens.Lens' DescribeDBParametersResponse (Core.Maybe Core.Text)
ddbprfrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbprfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of @Parameter@ values. 
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbprfrsParameters :: Lens.Lens' DescribeDBParametersResponse (Core.Maybe [Types.Parameter])
ddbprfrsParameters = Lens.field @"parameters"
{-# INLINEABLE ddbprfrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbprfrsResponseStatus :: Lens.Lens' DescribeDBParametersResponse Core.Int
ddbprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
