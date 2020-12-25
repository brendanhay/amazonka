{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddInstanceFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an instance fleet to a running cluster.
module Network.AWS.EMR.AddInstanceFleet
  ( -- * Creating a request
    AddInstanceFleet (..),
    mkAddInstanceFleet,

    -- ** Request lenses
    aifClusterId,
    aifInstanceFleet,

    -- * Destructuring the response
    AddInstanceFleetResponse (..),
    mkAddInstanceFleetResponse,

    -- ** Response lenses
    aifrrsClusterArn,
    aifrrsClusterId,
    aifrrsInstanceFleetId,
    aifrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddInstanceFleet' smart constructor.
data AddInstanceFleet = AddInstanceFleet'
  { -- | The unique identifier of the cluster.
    clusterId :: Types.ClusterId,
    -- | Specifies the configuration of the instance fleet.
    instanceFleet :: Types.InstanceFleetConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddInstanceFleet' value with any optional fields omitted.
mkAddInstanceFleet ::
  -- | 'clusterId'
  Types.ClusterId ->
  -- | 'instanceFleet'
  Types.InstanceFleetConfig ->
  AddInstanceFleet
mkAddInstanceFleet clusterId instanceFleet =
  AddInstanceFleet' {clusterId, instanceFleet}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifClusterId :: Lens.Lens' AddInstanceFleet Types.ClusterId
aifClusterId = Lens.field @"clusterId"
{-# DEPRECATED aifClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | Specifies the configuration of the instance fleet.
--
-- /Note:/ Consider using 'instanceFleet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifInstanceFleet :: Lens.Lens' AddInstanceFleet Types.InstanceFleetConfig
aifInstanceFleet = Lens.field @"instanceFleet"
{-# DEPRECATED aifInstanceFleet "Use generic-lens or generic-optics with 'instanceFleet' instead." #-}

instance Core.FromJSON AddInstanceFleet where
  toJSON AddInstanceFleet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            Core.Just ("InstanceFleet" Core..= instanceFleet)
          ]
      )

instance Core.AWSRequest AddInstanceFleet where
  type Rs AddInstanceFleet = AddInstanceFleetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.AddInstanceFleet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AddInstanceFleetResponse'
            Core.<$> (x Core..:? "ClusterArn")
            Core.<*> (x Core..:? "ClusterId")
            Core.<*> (x Core..:? "InstanceFleetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddInstanceFleetResponse' smart constructor.
data AddInstanceFleetResponse = AddInstanceFleetResponse'
  { -- | The Amazon Resource Name of the cluster.
    clusterArn :: Core.Maybe Types.ClusterArn,
    -- | The unique identifier of the cluster.
    clusterId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The unique identifier of the instance fleet.
    instanceFleetId :: Core.Maybe Types.InstanceFleetId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddInstanceFleetResponse' value with any optional fields omitted.
mkAddInstanceFleetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddInstanceFleetResponse
mkAddInstanceFleetResponse responseStatus =
  AddInstanceFleetResponse'
    { clusterArn = Core.Nothing,
      clusterId = Core.Nothing,
      instanceFleetId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrrsClusterArn :: Lens.Lens' AddInstanceFleetResponse (Core.Maybe Types.ClusterArn)
aifrrsClusterArn = Lens.field @"clusterArn"
{-# DEPRECATED aifrrsClusterArn "Use generic-lens or generic-optics with 'clusterArn' instead." #-}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrrsClusterId :: Lens.Lens' AddInstanceFleetResponse (Core.Maybe Types.XmlStringMaxLen256)
aifrrsClusterId = Lens.field @"clusterId"
{-# DEPRECATED aifrrsClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The unique identifier of the instance fleet.
--
-- /Note:/ Consider using 'instanceFleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrrsInstanceFleetId :: Lens.Lens' AddInstanceFleetResponse (Core.Maybe Types.InstanceFleetId)
aifrrsInstanceFleetId = Lens.field @"instanceFleetId"
{-# DEPRECATED aifrrsInstanceFleetId "Use generic-lens or generic-optics with 'instanceFleetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrrsResponseStatus :: Lens.Lens' AddInstanceFleetResponse Core.Int
aifrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aifrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
