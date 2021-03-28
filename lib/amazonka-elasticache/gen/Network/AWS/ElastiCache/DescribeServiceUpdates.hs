{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeServiceUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details of the service updates
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeServiceUpdates
    (
    -- * Creating a request
      DescribeServiceUpdates (..)
    , mkDescribeServiceUpdates
    -- ** Request lenses
    , dsuMarker
    , dsuMaxRecords
    , dsuServiceUpdateName
    , dsuServiceUpdateStatus

    -- * Destructuring the response
    , DescribeServiceUpdatesResponse (..)
    , mkDescribeServiceUpdatesResponse
    -- ** Response lenses
    , dsurrsMarker
    , dsurrsServiceUpdates
    , dsurrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeServiceUpdates' smart constructor.
data DescribeServiceUpdates = DescribeServiceUpdates'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response
  , serviceUpdateName :: Core.Maybe Core.Text
    -- ^ The unique ID of the service update
  , serviceUpdateStatus :: Core.Maybe [Types.ServiceUpdateStatus]
    -- ^ The status of the service update
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServiceUpdates' value with any optional fields omitted.
mkDescribeServiceUpdates
    :: DescribeServiceUpdates
mkDescribeServiceUpdates
  = DescribeServiceUpdates'{marker = Core.Nothing,
                            maxRecords = Core.Nothing, serviceUpdateName = Core.Nothing,
                            serviceUpdateStatus = Core.Nothing}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuMarker :: Lens.Lens' DescribeServiceUpdates (Core.Maybe Core.Text)
dsuMarker = Lens.field @"marker"
{-# INLINEABLE dsuMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuMaxRecords :: Lens.Lens' DescribeServiceUpdates (Core.Maybe Core.Int)
dsuMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dsuMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuServiceUpdateName :: Lens.Lens' DescribeServiceUpdates (Core.Maybe Core.Text)
dsuServiceUpdateName = Lens.field @"serviceUpdateName"
{-# INLINEABLE dsuServiceUpdateName #-}
{-# DEPRECATED serviceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead"  #-}

-- | The status of the service update
--
-- /Note:/ Consider using 'serviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuServiceUpdateStatus :: Lens.Lens' DescribeServiceUpdates (Core.Maybe [Types.ServiceUpdateStatus])
dsuServiceUpdateStatus = Lens.field @"serviceUpdateStatus"
{-# INLINEABLE dsuServiceUpdateStatus #-}
{-# DEPRECATED serviceUpdateStatus "Use generic-lens or generic-optics with 'serviceUpdateStatus' instead"  #-}

instance Core.ToQuery DescribeServiceUpdates where
        toQuery DescribeServiceUpdates{..}
          = Core.toQueryPair "Action" ("DescribeServiceUpdates" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ServiceUpdateName")
                serviceUpdateName
              Core.<>
              Core.toQueryPair "ServiceUpdateStatus"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   serviceUpdateStatus)

instance Core.ToHeaders DescribeServiceUpdates where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeServiceUpdates where
        type Rs DescribeServiceUpdates = DescribeServiceUpdatesResponse
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
          = Response.receiveXMLWrapper "DescribeServiceUpdatesResult"
              (\ s h x ->
                 DescribeServiceUpdatesResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "ServiceUpdates" Core..<@>
                       Core.parseXMLList "ServiceUpdate"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeServiceUpdates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"serviceUpdates" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeServiceUpdatesResponse' smart constructor.
data DescribeServiceUpdatesResponse = DescribeServiceUpdatesResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , serviceUpdates :: Core.Maybe [Types.ServiceUpdate]
    -- ^ A list of service updates
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeServiceUpdatesResponse' value with any optional fields omitted.
mkDescribeServiceUpdatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeServiceUpdatesResponse
mkDescribeServiceUpdatesResponse responseStatus
  = DescribeServiceUpdatesResponse'{marker = Core.Nothing,
                                    serviceUpdates = Core.Nothing, responseStatus}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsurrsMarker :: Lens.Lens' DescribeServiceUpdatesResponse (Core.Maybe Core.Text)
dsurrsMarker = Lens.field @"marker"
{-# INLINEABLE dsurrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of service updates
--
-- /Note:/ Consider using 'serviceUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsurrsServiceUpdates :: Lens.Lens' DescribeServiceUpdatesResponse (Core.Maybe [Types.ServiceUpdate])
dsurrsServiceUpdates = Lens.field @"serviceUpdates"
{-# INLINEABLE dsurrsServiceUpdates #-}
{-# DEPRECATED serviceUpdates "Use generic-lens or generic-optics with 'serviceUpdates' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsurrsResponseStatus :: Lens.Lens' DescribeServiceUpdatesResponse Core.Int
dsurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
