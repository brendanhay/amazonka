{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeServiceUpdates (..),
    mkDescribeServiceUpdates,

    -- ** Request lenses
    dsuMarker,
    dsuMaxRecords,
    dsuServiceUpdateName,
    dsuServiceUpdateStatus,

    -- * Destructuring the response
    DescribeServiceUpdatesResponse (..),
    mkDescribeServiceUpdatesResponse,

    -- ** Response lenses
    dsurrsMarker,
    dsurrsServiceUpdates,
    dsurrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeServiceUpdates' smart constructor.
data DescribeServiceUpdates = DescribeServiceUpdates'
  { -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response
    maxRecords :: Core.Maybe Core.Int,
    -- | The unique ID of the service update
    serviceUpdateName :: Core.Maybe Types.String,
    -- | The status of the service update
    serviceUpdateStatus :: Core.Maybe [Types.ServiceUpdateStatus]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServiceUpdates' value with any optional fields omitted.
mkDescribeServiceUpdates ::
  DescribeServiceUpdates
mkDescribeServiceUpdates =
  DescribeServiceUpdates'
    { marker = Core.Nothing,
      maxRecords = Core.Nothing,
      serviceUpdateName = Core.Nothing,
      serviceUpdateStatus = Core.Nothing
    }

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuMarker :: Lens.Lens' DescribeServiceUpdates (Core.Maybe Types.String)
dsuMarker = Lens.field @"marker"
{-# DEPRECATED dsuMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuMaxRecords :: Lens.Lens' DescribeServiceUpdates (Core.Maybe Core.Int)
dsuMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dsuMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuServiceUpdateName :: Lens.Lens' DescribeServiceUpdates (Core.Maybe Types.String)
dsuServiceUpdateName = Lens.field @"serviceUpdateName"
{-# DEPRECATED dsuServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The status of the service update
--
-- /Note:/ Consider using 'serviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsuServiceUpdateStatus :: Lens.Lens' DescribeServiceUpdates (Core.Maybe [Types.ServiceUpdateStatus])
dsuServiceUpdateStatus = Lens.field @"serviceUpdateStatus"
{-# DEPRECATED dsuServiceUpdateStatus "Use generic-lens or generic-optics with 'serviceUpdateStatus' instead." #-}

instance Core.AWSRequest DescribeServiceUpdates where
  type Rs DescribeServiceUpdates = DescribeServiceUpdatesResponse
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
            ( Core.pure ("Action", "DescribeServiceUpdates")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "ServiceUpdateName" Core.<$> serviceUpdateName)
                Core.<> ( Core.toQueryValue
                            "ServiceUpdateStatus"
                            (Core.toQueryList "member" Core.<$> serviceUpdateStatus)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeServiceUpdatesResult"
      ( \s h x ->
          DescribeServiceUpdatesResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "ServiceUpdates"
                         Core..<@> Core.parseXMLList "ServiceUpdate"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeServiceUpdates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"serviceUpdates" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeServiceUpdatesResponse' smart constructor.
data DescribeServiceUpdatesResponse = DescribeServiceUpdatesResponse'
  { -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | A list of service updates
    serviceUpdates :: Core.Maybe [Types.ServiceUpdate],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeServiceUpdatesResponse' value with any optional fields omitted.
mkDescribeServiceUpdatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeServiceUpdatesResponse
mkDescribeServiceUpdatesResponse responseStatus =
  DescribeServiceUpdatesResponse'
    { marker = Core.Nothing,
      serviceUpdates = Core.Nothing,
      responseStatus
    }

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsurrsMarker :: Lens.Lens' DescribeServiceUpdatesResponse (Core.Maybe Types.Marker)
dsurrsMarker = Lens.field @"marker"
{-# DEPRECATED dsurrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of service updates
--
-- /Note:/ Consider using 'serviceUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsurrsServiceUpdates :: Lens.Lens' DescribeServiceUpdatesResponse (Core.Maybe [Types.ServiceUpdate])
dsurrsServiceUpdates = Lens.field @"serviceUpdates"
{-# DEPRECATED dsurrsServiceUpdates "Use generic-lens or generic-optics with 'serviceUpdates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsurrsResponseStatus :: Lens.Lens' DescribeServiceUpdatesResponse Core.Int
dsurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
