{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB cluster parameter group.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterParameters
    (
    -- * Creating a request
      DescribeDBClusterParameters (..)
    , mkDescribeDBClusterParameters
    -- ** Request lenses
    , ddbcpDBClusterParameterGroupName
    , ddbcpFilters
    , ddbcpMarker
    , ddbcpMaxRecords
    , ddbcpSource

    -- * Destructuring the response
    , DescribeDBClusterParametersResponse (..)
    , mkDescribeDBClusterParametersResponse
    -- ** Response lenses
    , ddbcprrsMarker
    , ddbcprrsParameters
    , ddbcprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeDBClusterParameters' smart constructor.
data DescribeDBClusterParameters = DescribeDBClusterParameters'
  { dBClusterParameterGroupName :: Core.Text
    -- ^ The name of a specific DB cluster parameter group to return parameter details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , source :: Core.Maybe Core.Text
    -- ^ A value that indicates to return only parameters for a specific source. Parameter sources can be @engine@ , @service@ , or @customer@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusterParameters' value with any optional fields omitted.
mkDescribeDBClusterParameters
    :: Core.Text -- ^ 'dBClusterParameterGroupName'
    -> DescribeDBClusterParameters
mkDescribeDBClusterParameters dBClusterParameterGroupName
  = DescribeDBClusterParameters'{dBClusterParameterGroupName,
                                 filters = Core.Nothing, marker = Core.Nothing,
                                 maxRecords = Core.Nothing, source = Core.Nothing}

-- | The name of a specific DB cluster parameter group to return parameter details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpDBClusterParameterGroupName :: Lens.Lens' DescribeDBClusterParameters Core.Text
ddbcpDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# INLINEABLE ddbcpDBClusterParameterGroupName #-}
{-# DEPRECATED dBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpFilters :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe [Types.Filter])
ddbcpFilters = Lens.field @"filters"
{-# INLINEABLE ddbcpFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpMarker :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe Core.Text)
ddbcpMarker = Lens.field @"marker"
{-# INLINEABLE ddbcpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpMaxRecords :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe Core.Int)
ddbcpMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbcpMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | A value that indicates to return only parameters for a specific source. Parameter sources can be @engine@ , @service@ , or @customer@ . 
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpSource :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe Core.Text)
ddbcpSource = Lens.field @"source"
{-# INLINEABLE ddbcpSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

instance Core.ToQuery DescribeDBClusterParameters where
        toQuery DescribeDBClusterParameters{..}
          = Core.toQueryPair "Action"
              ("DescribeDBClusterParameters" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBClusterParameterGroupName"
                dBClusterParameterGroupName
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Source") source

instance Core.ToHeaders DescribeDBClusterParameters where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBClusterParameters where
        type Rs DescribeDBClusterParameters =
             DescribeDBClusterParametersResponse
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
          = Response.receiveXMLWrapper "DescribeDBClusterParametersResult"
              (\ s h x ->
                 DescribeDBClusterParametersResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "Parameters" Core..<@> Core.parseXMLList "Parameter"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBClusterParameters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Provides details about a DB cluster parameter group including the parameters in the DB cluster parameter group.
--
-- /See:/ 'mkDescribeDBClusterParametersResponse' smart constructor.
data DescribeDBClusterParametersResponse = DescribeDBClusterParametersResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous DescribeDBClusterParameters request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ Provides a list of parameters for the DB cluster parameter group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusterParametersResponse' value with any optional fields omitted.
mkDescribeDBClusterParametersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBClusterParametersResponse
mkDescribeDBClusterParametersResponse responseStatus
  = DescribeDBClusterParametersResponse'{marker = Core.Nothing,
                                         parameters = Core.Nothing, responseStatus}

-- | An optional pagination token provided by a previous DescribeDBClusterParameters request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcprrsMarker :: Lens.Lens' DescribeDBClusterParametersResponse (Core.Maybe Core.Text)
ddbcprrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbcprrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Provides a list of parameters for the DB cluster parameter group.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcprrsParameters :: Lens.Lens' DescribeDBClusterParametersResponse (Core.Maybe [Types.Parameter])
ddbcprrsParameters = Lens.field @"parameters"
{-# INLINEABLE ddbcprrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcprrsResponseStatus :: Lens.Lens' DescribeDBClusterParametersResponse Core.Int
ddbcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
