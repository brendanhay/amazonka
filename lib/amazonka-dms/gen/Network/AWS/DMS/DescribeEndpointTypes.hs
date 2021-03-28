{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeEndpointTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the type of endpoints available.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEndpointTypes
    (
    -- * Creating a request
      DescribeEndpointTypes (..)
    , mkDescribeEndpointTypes
    -- ** Request lenses
    , detFilters
    , detMarker
    , detMaxRecords

    -- * Destructuring the response
    , DescribeEndpointTypesResponse (..)
    , mkDescribeEndpointTypesResponse
    -- ** Response lenses
    , detrrsMarker
    , detrrsSupportedEndpointTypes
    , detrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeEndpointTypes' smart constructor.
data DescribeEndpointTypes = DescribeEndpointTypes'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ Filters applied to the endpoint types.
--
-- Valid filter names: engine-name | endpoint-type
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpointTypes' value with any optional fields omitted.
mkDescribeEndpointTypes
    :: DescribeEndpointTypes
mkDescribeEndpointTypes
  = DescribeEndpointTypes'{filters = Core.Nothing,
                           marker = Core.Nothing, maxRecords = Core.Nothing}

-- | Filters applied to the endpoint types.
--
-- Valid filter names: engine-name | endpoint-type
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detFilters :: Lens.Lens' DescribeEndpointTypes (Core.Maybe [Types.Filter])
detFilters = Lens.field @"filters"
{-# INLINEABLE detFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMarker :: Lens.Lens' DescribeEndpointTypes (Core.Maybe Core.Text)
detMarker = Lens.field @"marker"
{-# INLINEABLE detMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMaxRecords :: Lens.Lens' DescribeEndpointTypes (Core.Maybe Core.Int)
detMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE detMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeEndpointTypes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEndpointTypes where
        toHeaders DescribeEndpointTypes{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.DescribeEndpointTypes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEndpointTypes where
        toJSON DescribeEndpointTypes{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords])

instance Core.AWSRequest DescribeEndpointTypes where
        type Rs DescribeEndpointTypes = DescribeEndpointTypesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEndpointTypesResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "SupportedEndpointTypes"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEndpointTypes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"supportedEndpointTypes" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeEndpointTypesResponse' smart constructor.
data DescribeEndpointTypesResponse = DescribeEndpointTypesResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , supportedEndpointTypes :: Core.Maybe [Types.SupportedEndpointType]
    -- ^ The types of endpoints that are supported.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpointTypesResponse' value with any optional fields omitted.
mkDescribeEndpointTypesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEndpointTypesResponse
mkDescribeEndpointTypesResponse responseStatus
  = DescribeEndpointTypesResponse'{marker = Core.Nothing,
                                   supportedEndpointTypes = Core.Nothing, responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsMarker :: Lens.Lens' DescribeEndpointTypesResponse (Core.Maybe Core.Text)
detrrsMarker = Lens.field @"marker"
{-# INLINEABLE detrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The types of endpoints that are supported.
--
-- /Note:/ Consider using 'supportedEndpointTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsSupportedEndpointTypes :: Lens.Lens' DescribeEndpointTypesResponse (Core.Maybe [Types.SupportedEndpointType])
detrrsSupportedEndpointTypes = Lens.field @"supportedEndpointTypes"
{-# INLINEABLE detrrsSupportedEndpointTypes #-}
{-# DEPRECATED supportedEndpointTypes "Use generic-lens or generic-optics with 'supportedEndpointTypes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsResponseStatus :: Lens.Lens' DescribeEndpointTypesResponse Core.Int
detrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE detrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
