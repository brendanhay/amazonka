{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information for all active EC2 instances and EC2 instances terminated in the last 30 days, up to a maximum of 2,000. EC2 instances in any of the following states are considered active: AWAITING_FULFILLMENT, PROVISIONING, BOOTSTRAPPING, RUNNING.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstances
  ( -- * Creating a request
    ListInstances (..),
    mkListInstances,

    -- ** Request lenses
    liClusterId,
    liInstanceFleetId,
    liInstanceFleetType,
    liInstanceGroupId,
    liInstanceGroupTypes,
    liInstanceStates,
    liMarker,

    -- * Destructuring the response
    ListInstancesResponse (..),
    mkListInstancesResponse,

    -- ** Response lenses
    lirrsInstances,
    lirrsMarker,
    lirrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which instances to list.
--
-- /See:/ 'mkListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | The identifier of the cluster for which to list the instances.
    clusterId :: Types.ClusterId,
    -- | The unique identifier of the instance fleet.
    instanceFleetId :: Core.Maybe Types.InstanceFleetId,
    -- | The node type of the instance fleet. For example MASTER, CORE, or TASK.
    instanceFleetType :: Core.Maybe Types.InstanceFleetType,
    -- | The identifier of the instance group for which to list the instances.
    instanceGroupId :: Core.Maybe Types.InstanceGroupId,
    -- | The type of instance group for which to list the instances.
    instanceGroupTypes :: Core.Maybe [Types.InstanceGroupType],
    -- | A list of instance states that will filter the instances returned with this request.
    instanceStates :: Core.Maybe [Types.InstanceState],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Types.Marker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstances' value with any optional fields omitted.
mkListInstances ::
  -- | 'clusterId'
  Types.ClusterId ->
  ListInstances
mkListInstances clusterId =
  ListInstances'
    { clusterId,
      instanceFleetId = Core.Nothing,
      instanceFleetType = Core.Nothing,
      instanceGroupId = Core.Nothing,
      instanceGroupTypes = Core.Nothing,
      instanceStates = Core.Nothing,
      marker = Core.Nothing
    }

-- | The identifier of the cluster for which to list the instances.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liClusterId :: Lens.Lens' ListInstances Types.ClusterId
liClusterId = Lens.field @"clusterId"
{-# DEPRECATED liClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The unique identifier of the instance fleet.
--
-- /Note:/ Consider using 'instanceFleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liInstanceFleetId :: Lens.Lens' ListInstances (Core.Maybe Types.InstanceFleetId)
liInstanceFleetId = Lens.field @"instanceFleetId"
{-# DEPRECATED liInstanceFleetId "Use generic-lens or generic-optics with 'instanceFleetId' instead." #-}

-- | The node type of the instance fleet. For example MASTER, CORE, or TASK.
--
-- /Note:/ Consider using 'instanceFleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liInstanceFleetType :: Lens.Lens' ListInstances (Core.Maybe Types.InstanceFleetType)
liInstanceFleetType = Lens.field @"instanceFleetType"
{-# DEPRECATED liInstanceFleetType "Use generic-lens or generic-optics with 'instanceFleetType' instead." #-}

-- | The identifier of the instance group for which to list the instances.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liInstanceGroupId :: Lens.Lens' ListInstances (Core.Maybe Types.InstanceGroupId)
liInstanceGroupId = Lens.field @"instanceGroupId"
{-# DEPRECATED liInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

-- | The type of instance group for which to list the instances.
--
-- /Note:/ Consider using 'instanceGroupTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liInstanceGroupTypes :: Lens.Lens' ListInstances (Core.Maybe [Types.InstanceGroupType])
liInstanceGroupTypes = Lens.field @"instanceGroupTypes"
{-# DEPRECATED liInstanceGroupTypes "Use generic-lens or generic-optics with 'instanceGroupTypes' instead." #-}

-- | A list of instance states that will filter the instances returned with this request.
--
-- /Note:/ Consider using 'instanceStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liInstanceStates :: Lens.Lens' ListInstances (Core.Maybe [Types.InstanceState])
liInstanceStates = Lens.field @"instanceStates"
{-# DEPRECATED liInstanceStates "Use generic-lens or generic-optics with 'instanceStates' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMarker :: Lens.Lens' ListInstances (Core.Maybe Types.Marker)
liMarker = Lens.field @"marker"
{-# DEPRECATED liMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.FromJSON ListInstances where
  toJSON ListInstances {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            ("InstanceFleetId" Core..=) Core.<$> instanceFleetId,
            ("InstanceFleetType" Core..=) Core.<$> instanceFleetType,
            ("InstanceGroupId" Core..=) Core.<$> instanceGroupId,
            ("InstanceGroupTypes" Core..=) Core.<$> instanceGroupTypes,
            ("InstanceStates" Core..=) Core.<$> instanceStates,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.AWSRequest ListInstances where
  type Rs ListInstances = ListInstancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.ListInstances")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Core.<$> (x Core..:? "Instances")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListInstances where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"instances" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | This output contains the list of instances.
--
-- /See:/ 'mkListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | The list of instances for the cluster and given filters.
    instances :: Core.Maybe [Types.Instance],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListInstancesResponse' value with any optional fields omitted.
mkListInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListInstancesResponse
mkListInstancesResponse responseStatus =
  ListInstancesResponse'
    { instances = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | The list of instances for the cluster and given filters.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsInstances :: Lens.Lens' ListInstancesResponse (Core.Maybe [Types.Instance])
lirrsInstances = Lens.field @"instances"
{-# DEPRECATED lirrsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsMarker :: Lens.Lens' ListInstancesResponse (Core.Maybe Types.Marker)
lirrsMarker = Lens.field @"marker"
{-# DEPRECATED lirrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListInstancesResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
