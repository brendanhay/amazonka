{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.IncreaseReplicationFactor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more nodes to a DAX cluster.
module Network.AWS.DAX.IncreaseReplicationFactor
  ( -- * Creating a request
    IncreaseReplicationFactor (..),
    mkIncreaseReplicationFactor,

    -- ** Request lenses
    irfClusterName,
    irfNewReplicationFactor,
    irfAvailabilityZones,

    -- * Destructuring the response
    IncreaseReplicationFactorResponse (..),
    mkIncreaseReplicationFactorResponse,

    -- ** Response lenses
    irfrrsCluster,
    irfrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkIncreaseReplicationFactor' smart constructor.
data IncreaseReplicationFactor = IncreaseReplicationFactor'
  { -- | The name of the DAX cluster that will receive additional nodes.
    clusterName :: Types.ClusterName,
    -- | The new number of nodes for the DAX cluster.
    newReplicationFactor :: Core.Int,
    -- | The Availability Zones (AZs) in which the cluster nodes will be created. All nodes belonging to the cluster are placed in these Availability Zones. Use this parameter if you want to distribute the nodes across multiple AZs.
    availabilityZones :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IncreaseReplicationFactor' value with any optional fields omitted.
mkIncreaseReplicationFactor ::
  -- | 'clusterName'
  Types.ClusterName ->
  -- | 'newReplicationFactor'
  Core.Int ->
  IncreaseReplicationFactor
mkIncreaseReplicationFactor clusterName newReplicationFactor =
  IncreaseReplicationFactor'
    { clusterName,
      newReplicationFactor,
      availabilityZones = Core.Nothing
    }

-- | The name of the DAX cluster that will receive additional nodes.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irfClusterName :: Lens.Lens' IncreaseReplicationFactor Types.ClusterName
irfClusterName = Lens.field @"clusterName"
{-# DEPRECATED irfClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The new number of nodes for the DAX cluster.
--
-- /Note:/ Consider using 'newReplicationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irfNewReplicationFactor :: Lens.Lens' IncreaseReplicationFactor Core.Int
irfNewReplicationFactor = Lens.field @"newReplicationFactor"
{-# DEPRECATED irfNewReplicationFactor "Use generic-lens or generic-optics with 'newReplicationFactor' instead." #-}

-- | The Availability Zones (AZs) in which the cluster nodes will be created. All nodes belonging to the cluster are placed in these Availability Zones. Use this parameter if you want to distribute the nodes across multiple AZs.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irfAvailabilityZones :: Lens.Lens' IncreaseReplicationFactor (Core.Maybe [Types.String])
irfAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED irfAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

instance Core.FromJSON IncreaseReplicationFactor where
  toJSON IncreaseReplicationFactor {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterName" Core..= clusterName),
            Core.Just ("NewReplicationFactor" Core..= newReplicationFactor),
            ("AvailabilityZones" Core..=) Core.<$> availabilityZones
          ]
      )

instance Core.AWSRequest IncreaseReplicationFactor where
  type
    Rs IncreaseReplicationFactor =
      IncreaseReplicationFactorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.IncreaseReplicationFactor")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          IncreaseReplicationFactorResponse'
            Core.<$> (x Core..:? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkIncreaseReplicationFactorResponse' smart constructor.
data IncreaseReplicationFactorResponse = IncreaseReplicationFactorResponse'
  { -- | A description of the DAX cluster. with its new replication factor.
    cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'IncreaseReplicationFactorResponse' value with any optional fields omitted.
mkIncreaseReplicationFactorResponse ::
  -- | 'responseStatus'
  Core.Int ->
  IncreaseReplicationFactorResponse
mkIncreaseReplicationFactorResponse responseStatus =
  IncreaseReplicationFactorResponse'
    { cluster = Core.Nothing,
      responseStatus
    }

-- | A description of the DAX cluster. with its new replication factor.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irfrrsCluster :: Lens.Lens' IncreaseReplicationFactorResponse (Core.Maybe Types.Cluster)
irfrrsCluster = Lens.field @"cluster"
{-# DEPRECATED irfrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irfrrsResponseStatus :: Lens.Lens' IncreaseReplicationFactorResponse Core.Int
irfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED irfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
