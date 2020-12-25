{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListInstanceFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available details about the instance fleets in a cluster.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstanceFleets
  ( -- * Creating a request
    ListInstanceFleets (..),
    mkListInstanceFleets,

    -- ** Request lenses
    lifClusterId,
    lifMarker,

    -- * Destructuring the response
    ListInstanceFleetsResponse (..),
    mkListInstanceFleetsResponse,

    -- ** Response lenses
    lifrrsInstanceFleets,
    lifrrsMarker,
    lifrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListInstanceFleets' smart constructor.
data ListInstanceFleets = ListInstanceFleets'
  { -- | The unique identifier of the cluster.
    clusterId :: Types.ClusterId,
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Types.Marker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceFleets' value with any optional fields omitted.
mkListInstanceFleets ::
  -- | 'clusterId'
  Types.ClusterId ->
  ListInstanceFleets
mkListInstanceFleets clusterId =
  ListInstanceFleets' {clusterId, marker = Core.Nothing}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifClusterId :: Lens.Lens' ListInstanceFleets Types.ClusterId
lifClusterId = Lens.field @"clusterId"
{-# DEPRECATED lifClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifMarker :: Lens.Lens' ListInstanceFleets (Core.Maybe Types.Marker)
lifMarker = Lens.field @"marker"
{-# DEPRECATED lifMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.FromJSON ListInstanceFleets where
  toJSON ListInstanceFleets {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.AWSRequest ListInstanceFleets where
  type Rs ListInstanceFleets = ListInstanceFleetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.ListInstanceFleets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceFleetsResponse'
            Core.<$> (x Core..:? "InstanceFleets")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListInstanceFleets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"instanceFleets" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkListInstanceFleetsResponse' smart constructor.
data ListInstanceFleetsResponse = ListInstanceFleetsResponse'
  { -- | The list of instance fleets for the cluster and given filters.
    instanceFleets :: Core.Maybe [Types.InstanceFleet],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListInstanceFleetsResponse' value with any optional fields omitted.
mkListInstanceFleetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListInstanceFleetsResponse
mkListInstanceFleetsResponse responseStatus =
  ListInstanceFleetsResponse'
    { instanceFleets = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | The list of instance fleets for the cluster and given filters.
--
-- /Note:/ Consider using 'instanceFleets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifrrsInstanceFleets :: Lens.Lens' ListInstanceFleetsResponse (Core.Maybe [Types.InstanceFleet])
lifrrsInstanceFleets = Lens.field @"instanceFleets"
{-# DEPRECATED lifrrsInstanceFleets "Use generic-lens or generic-optics with 'instanceFleets' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifrrsMarker :: Lens.Lens' ListInstanceFleetsResponse (Core.Maybe Types.Marker)
lifrrsMarker = Lens.field @"marker"
{-# DEPRECATED lifrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifrrsResponseStatus :: Lens.Lens' ListInstanceFleetsResponse Core.Int
lifrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lifrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
