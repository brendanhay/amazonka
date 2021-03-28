{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the specified database engine.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEngineDefaultParameters
    (
    -- * Creating a request
      DescribeEngineDefaultParameters (..)
    , mkDescribeEngineDefaultParameters
    -- ** Request lenses
    , dedpDBParameterGroupFamily
    , dedpFilters
    , dedpMarker
    , dedpMaxRecords

    -- * Destructuring the response
    , DescribeEngineDefaultParametersResponse (..)
    , mkDescribeEngineDefaultParametersResponse
    -- ** Response lenses
    , dedprrsEngineDefaults
    , dedprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeEngineDefaultParameters' smart constructor.
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
  { dBParameterGroupFamily :: Core.Text
    -- ^ The name of the DB parameter group family.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeEngineDefaultParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEngineDefaultParameters' value with any optional fields omitted.
mkDescribeEngineDefaultParameters
    :: Core.Text -- ^ 'dBParameterGroupFamily'
    -> DescribeEngineDefaultParameters
mkDescribeEngineDefaultParameters dBParameterGroupFamily
  = DescribeEngineDefaultParameters'{dBParameterGroupFamily,
                                     filters = Core.Nothing, marker = Core.Nothing,
                                     maxRecords = Core.Nothing}

-- | The name of the DB parameter group family.
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpDBParameterGroupFamily :: Lens.Lens' DescribeEngineDefaultParameters Core.Text
dedpDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# INLINEABLE dedpDBParameterGroupFamily #-}
{-# DEPRECATED dBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpFilters :: Lens.Lens' DescribeEngineDefaultParameters (Core.Maybe [Types.Filter])
dedpFilters = Lens.field @"filters"
{-# INLINEABLE dedpFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeEngineDefaultParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpMarker :: Lens.Lens' DescribeEngineDefaultParameters (Core.Maybe Core.Text)
dedpMarker = Lens.field @"marker"
{-# INLINEABLE dedpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpMaxRecords :: Lens.Lens' DescribeEngineDefaultParameters (Core.Maybe Core.Int)
dedpMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dedpMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeEngineDefaultParameters where
        toQuery DescribeEngineDefaultParameters{..}
          = Core.toQueryPair "Action"
              ("DescribeEngineDefaultParameters" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBParameterGroupFamily" dBParameterGroupFamily
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeEngineDefaultParameters where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeEngineDefaultParameters where
        type Rs DescribeEngineDefaultParameters =
             DescribeEngineDefaultParametersResponse
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
              "DescribeEngineDefaultParametersResult"
              (\ s h x ->
                 DescribeEngineDefaultParametersResponse' Core.<$>
                   (x Core..@ "EngineDefaults") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEngineDefaultParameters where
        page rq rs
          | Pager.stop
              (rs Lens.^.
                 Lens.field @"engineDefaults" Core.. Lens.field @"marker")
            = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"engineDefaults" Core..
                   Lens.field @"parameters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~
                   rs Lens.^.
                     Lens.field @"engineDefaults" Core.. Lens.field @"marker")

-- | /See:/ 'mkDescribeEngineDefaultParametersResponse' smart constructor.
data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse'
  { engineDefaults :: Types.EngineDefaults
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEngineDefaultParametersResponse' value with any optional fields omitted.
mkDescribeEngineDefaultParametersResponse
    :: Types.EngineDefaults -- ^ 'engineDefaults'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeEngineDefaultParametersResponse
mkDescribeEngineDefaultParametersResponse engineDefaults
  responseStatus
  = DescribeEngineDefaultParametersResponse'{engineDefaults,
                                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'engineDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedprrsEngineDefaults :: Lens.Lens' DescribeEngineDefaultParametersResponse Types.EngineDefaults
dedprrsEngineDefaults = Lens.field @"engineDefaults"
{-# INLINEABLE dedprrsEngineDefaults #-}
{-# DEPRECATED engineDefaults "Use generic-lens or generic-optics with 'engineDefaults' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedprrsResponseStatus :: Lens.Lens' DescribeEngineDefaultParametersResponse Core.Int
dedprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dedprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
