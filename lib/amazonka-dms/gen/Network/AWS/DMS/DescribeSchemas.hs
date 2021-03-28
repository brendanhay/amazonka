{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeSchemas
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the schema for the specified endpoint.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeSchemas
    (
    -- * Creating a request
      DescribeSchemas (..)
    , mkDescribeSchemas
    -- ** Request lenses
    , dsEndpointArn
    , dsMarker
    , dsMaxRecords

    -- * Destructuring the response
    , DescribeSchemasResponse (..)
    , mkDescribeSchemasResponse
    -- ** Response lenses
    , dsrrsMarker
    , dsrrsSchemas
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeSchemas' smart constructor.
data DescribeSchemas = DescribeSchemas'
  { endpointArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
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

-- | Creates a 'DescribeSchemas' value with any optional fields omitted.
mkDescribeSchemas
    :: Core.Text -- ^ 'endpointArn'
    -> DescribeSchemas
mkDescribeSchemas endpointArn
  = DescribeSchemas'{endpointArn, marker = Core.Nothing,
                     maxRecords = Core.Nothing}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsEndpointArn :: Lens.Lens' DescribeSchemas Core.Text
dsEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE dsEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMarker :: Lens.Lens' DescribeSchemas (Core.Maybe Core.Text)
dsMarker = Lens.field @"marker"
{-# INLINEABLE dsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxRecords :: Lens.Lens' DescribeSchemas (Core.Maybe Core.Int)
dsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeSchemas where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSchemas where
        toHeaders DescribeSchemas{..}
          = Core.pure ("X-Amz-Target", "AmazonDMSv20160101.DescribeSchemas")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSchemas where
        toJSON DescribeSchemas{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointArn" Core..= endpointArn),
                  ("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords])

instance Core.AWSRequest DescribeSchemas where
        type Rs DescribeSchemas = DescribeSchemasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSchemasResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "Schemas" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSchemas where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"schemas" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeSchemasResponse' smart constructor.
data DescribeSchemasResponse = DescribeSchemasResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , schemas :: Core.Maybe [Core.Text]
    -- ^ The described schema.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSchemasResponse' value with any optional fields omitted.
mkDescribeSchemasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSchemasResponse
mkDescribeSchemasResponse responseStatus
  = DescribeSchemasResponse'{marker = Core.Nothing,
                             schemas = Core.Nothing, responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsMarker :: Lens.Lens' DescribeSchemasResponse (Core.Maybe Core.Text)
dsrrsMarker = Lens.field @"marker"
{-# INLINEABLE dsrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The described schema.
--
-- /Note:/ Consider using 'schemas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSchemas :: Lens.Lens' DescribeSchemasResponse (Core.Maybe [Core.Text])
dsrrsSchemas = Lens.field @"schemas"
{-# INLINEABLE dsrrsSchemas #-}
{-# DEPRECATED schemas "Use generic-lens or generic-optics with 'schemas' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeSchemasResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
