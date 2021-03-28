{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEngineDefaultClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the cluster database engine.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEngineDefaultClusterParameters
    (
    -- * Creating a request
      DescribeEngineDefaultClusterParameters (..)
    , mkDescribeEngineDefaultClusterParameters
    -- ** Request lenses
    , dedcpDBParameterGroupFamily
    , dedcpFilters
    , dedcpMarker
    , dedcpMaxRecords

    -- * Destructuring the response
    , DescribeEngineDefaultClusterParametersResponse (..)
    , mkDescribeEngineDefaultClusterParametersResponse
    -- ** Response lenses
    , dedcprrsEngineDefaults
    , dedcprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeEngineDefaultClusterParameters' smart constructor.
data DescribeEngineDefaultClusterParameters = DescribeEngineDefaultClusterParameters'
  { dBParameterGroupFamily :: Core.Text
    -- ^ The name of the DB cluster parameter group family to return engine parameter information for.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeEngineDefaultClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEngineDefaultClusterParameters' value with any optional fields omitted.
mkDescribeEngineDefaultClusterParameters
    :: Core.Text -- ^ 'dBParameterGroupFamily'
    -> DescribeEngineDefaultClusterParameters
mkDescribeEngineDefaultClusterParameters dBParameterGroupFamily
  = DescribeEngineDefaultClusterParameters'{dBParameterGroupFamily,
                                            filters = Core.Nothing, marker = Core.Nothing,
                                            maxRecords = Core.Nothing}

-- | The name of the DB cluster parameter group family to return engine parameter information for.
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcpDBParameterGroupFamily :: Lens.Lens' DescribeEngineDefaultClusterParameters Core.Text
dedcpDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# INLINEABLE dedcpDBParameterGroupFamily #-}
{-# DEPRECATED dBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcpFilters :: Lens.Lens' DescribeEngineDefaultClusterParameters (Core.Maybe [Types.Filter])
dedcpFilters = Lens.field @"filters"
{-# INLINEABLE dedcpFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeEngineDefaultClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcpMarker :: Lens.Lens' DescribeEngineDefaultClusterParameters (Core.Maybe Core.Text)
dedcpMarker = Lens.field @"marker"
{-# INLINEABLE dedcpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcpMaxRecords :: Lens.Lens' DescribeEngineDefaultClusterParameters (Core.Maybe Core.Int)
dedcpMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dedcpMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeEngineDefaultClusterParameters where
        toQuery DescribeEngineDefaultClusterParameters{..}
          = Core.toQueryPair "Action"
              ("DescribeEngineDefaultClusterParameters" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBParameterGroupFamily" dBParameterGroupFamily
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeEngineDefaultClusterParameters
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeEngineDefaultClusterParameters
         where
        type Rs DescribeEngineDefaultClusterParameters =
             DescribeEngineDefaultClusterParametersResponse
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
          = Response.receiveXMLWrapper
              "DescribeEngineDefaultClusterParametersResult"
              (\ s h x ->
                 DescribeEngineDefaultClusterParametersResponse' Core.<$>
                   (x Core..@? "EngineDefaults") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEngineDefaultClusterParameters
         where
        page rq rs
          | Pager.stop
              (rs Lens.^.
                 Lens.field @"engineDefaults" Core.. Lens.field @"marker")
            = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"engineDefaults" Core.. Lens._Just Core..
                   Lens.field @"parameters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~
                   rs Lens.^.
                     Lens.field @"engineDefaults" Core.. Lens.field @"marker")

-- | /See:/ 'mkDescribeEngineDefaultClusterParametersResponse' smart constructor.
data DescribeEngineDefaultClusterParametersResponse = DescribeEngineDefaultClusterParametersResponse'
  { engineDefaults :: Core.Maybe Types.EngineDefaults
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEngineDefaultClusterParametersResponse' value with any optional fields omitted.
mkDescribeEngineDefaultClusterParametersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEngineDefaultClusterParametersResponse
mkDescribeEngineDefaultClusterParametersResponse responseStatus
  = DescribeEngineDefaultClusterParametersResponse'{engineDefaults =
                                                      Core.Nothing,
                                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'engineDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcprrsEngineDefaults :: Lens.Lens' DescribeEngineDefaultClusterParametersResponse (Core.Maybe Types.EngineDefaults)
dedcprrsEngineDefaults = Lens.field @"engineDefaults"
{-# INLINEABLE dedcprrsEngineDefaults #-}
{-# DEPRECATED engineDefaults "Use generic-lens or generic-optics with 'engineDefaults' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcprrsResponseStatus :: Lens.Lens' DescribeEngineDefaultClusterParametersResponse Core.Int
dedcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dedcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
