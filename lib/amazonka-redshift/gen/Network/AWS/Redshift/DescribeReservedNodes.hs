{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeReservedNodes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of the reserved nodes.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeReservedNodes
  ( -- * Creating a request
    DescribeReservedNodes (..),
    mkDescribeReservedNodes,

    -- ** Request lenses
    drnMarker,
    drnMaxRecords,
    drnReservedNodeId,

    -- * Destructuring the response
    DescribeReservedNodesResponse (..),
    mkDescribeReservedNodesResponse,

    -- ** Response lenses
    drnrrsMarker,
    drnrrsReservedNodes,
    drnrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeReservedNodes' smart constructor.
data DescribeReservedNodes = DescribeReservedNodes'
  { -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeReservedNodes' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | Identifier for the node reservation.
    reservedNodeId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedNodes' value with any optional fields omitted.
mkDescribeReservedNodes ::
  DescribeReservedNodes
mkDescribeReservedNodes =
  DescribeReservedNodes'
    { marker = Core.Nothing,
      maxRecords = Core.Nothing,
      reservedNodeId = Core.Nothing
    }

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeReservedNodes' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnMarker :: Lens.Lens' DescribeReservedNodes (Core.Maybe Types.String)
drnMarker = Lens.field @"marker"
{-# DEPRECATED drnMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnMaxRecords :: Lens.Lens' DescribeReservedNodes (Core.Maybe Core.Int)
drnMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED drnMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | Identifier for the node reservation.
--
-- /Note:/ Consider using 'reservedNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnReservedNodeId :: Lens.Lens' DescribeReservedNodes (Core.Maybe Types.String)
drnReservedNodeId = Lens.field @"reservedNodeId"
{-# DEPRECATED drnReservedNodeId "Use generic-lens or generic-optics with 'reservedNodeId' instead." #-}

instance Core.AWSRequest DescribeReservedNodes where
  type Rs DescribeReservedNodes = DescribeReservedNodesResponse
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
            ( Core.pure ("Action", "DescribeReservedNodes")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "ReservedNodeId" Core.<$> reservedNodeId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeReservedNodesResult"
      ( \s h x ->
          DescribeReservedNodesResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "ReservedNodes"
                         Core..<@> Core.parseXMLList "ReservedNode"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeReservedNodes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"reservedNodes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- |
--
-- /See:/ 'mkDescribeReservedNodesResponse' smart constructor.
data DescribeReservedNodesResponse = DescribeReservedNodesResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.Marker,
    -- | The list of @ReservedNode@ objects.
    reservedNodes :: Core.Maybe [Types.ReservedNode],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeReservedNodesResponse' value with any optional fields omitted.
mkDescribeReservedNodesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeReservedNodesResponse
mkDescribeReservedNodesResponse responseStatus =
  DescribeReservedNodesResponse'
    { marker = Core.Nothing,
      reservedNodes = Core.Nothing,
      responseStatus
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnrrsMarker :: Lens.Lens' DescribeReservedNodesResponse (Core.Maybe Types.Marker)
drnrrsMarker = Lens.field @"marker"
{-# DEPRECATED drnrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The list of @ReservedNode@ objects.
--
-- /Note:/ Consider using 'reservedNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnrrsReservedNodes :: Lens.Lens' DescribeReservedNodesResponse (Core.Maybe [Types.ReservedNode])
drnrrsReservedNodes = Lens.field @"reservedNodes"
{-# DEPRECATED drnrrsReservedNodes "Use generic-lens or generic-optics with 'reservedNodes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnrrsResponseStatus :: Lens.Lens' DescribeReservedNodesResponse Core.Int
drnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
