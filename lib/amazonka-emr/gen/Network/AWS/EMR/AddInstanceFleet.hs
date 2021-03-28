{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AddInstanceFleet (..)
    , mkAddInstanceFleet
    -- ** Request lenses
    , aifClusterId
    , aifInstanceFleet

    -- * Destructuring the response
    , AddInstanceFleetResponse (..)
    , mkAddInstanceFleetResponse
    -- ** Response lenses
    , aifrrsClusterArn
    , aifrrsClusterId
    , aifrrsInstanceFleetId
    , aifrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddInstanceFleet' smart constructor.
data AddInstanceFleet = AddInstanceFleet'
  { clusterId :: Types.ClusterId
    -- ^ The unique identifier of the cluster.
  , instanceFleet :: Types.InstanceFleetConfig
    -- ^ Specifies the configuration of the instance fleet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddInstanceFleet' value with any optional fields omitted.
mkAddInstanceFleet
    :: Types.ClusterId -- ^ 'clusterId'
    -> Types.InstanceFleetConfig -- ^ 'instanceFleet'
    -> AddInstanceFleet
mkAddInstanceFleet clusterId instanceFleet
  = AddInstanceFleet'{clusterId, instanceFleet}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifClusterId :: Lens.Lens' AddInstanceFleet Types.ClusterId
aifClusterId = Lens.field @"clusterId"
{-# INLINEABLE aifClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | Specifies the configuration of the instance fleet.
--
-- /Note:/ Consider using 'instanceFleet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifInstanceFleet :: Lens.Lens' AddInstanceFleet Types.InstanceFleetConfig
aifInstanceFleet = Lens.field @"instanceFleet"
{-# INLINEABLE aifInstanceFleet #-}
{-# DEPRECATED instanceFleet "Use generic-lens or generic-optics with 'instanceFleet' instead"  #-}

instance Core.ToQuery AddInstanceFleet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddInstanceFleet where
        toHeaders AddInstanceFleet{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.AddInstanceFleet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddInstanceFleet where
        toJSON AddInstanceFleet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  Core.Just ("InstanceFleet" Core..= instanceFleet)])

instance Core.AWSRequest AddInstanceFleet where
        type Rs AddInstanceFleet = AddInstanceFleetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddInstanceFleetResponse' Core.<$>
                   (x Core..:? "ClusterArn") Core.<*> x Core..:? "ClusterId" Core.<*>
                     x Core..:? "InstanceFleetId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddInstanceFleetResponse' smart constructor.
data AddInstanceFleetResponse = AddInstanceFleetResponse'
  { clusterArn :: Core.Maybe Types.ClusterArn
    -- ^ The Amazon Resource Name of the cluster.
  , clusterId :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The unique identifier of the cluster.
  , instanceFleetId :: Core.Maybe Types.InstanceFleetId
    -- ^ The unique identifier of the instance fleet.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddInstanceFleetResponse' value with any optional fields omitted.
mkAddInstanceFleetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddInstanceFleetResponse
mkAddInstanceFleetResponse responseStatus
  = AddInstanceFleetResponse'{clusterArn = Core.Nothing,
                              clusterId = Core.Nothing, instanceFleetId = Core.Nothing,
                              responseStatus}

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrrsClusterArn :: Lens.Lens' AddInstanceFleetResponse (Core.Maybe Types.ClusterArn)
aifrrsClusterArn = Lens.field @"clusterArn"
{-# INLINEABLE aifrrsClusterArn #-}
{-# DEPRECATED clusterArn "Use generic-lens or generic-optics with 'clusterArn' instead"  #-}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrrsClusterId :: Lens.Lens' AddInstanceFleetResponse (Core.Maybe Types.XmlStringMaxLen256)
aifrrsClusterId = Lens.field @"clusterId"
{-# INLINEABLE aifrrsClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The unique identifier of the instance fleet.
--
-- /Note:/ Consider using 'instanceFleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrrsInstanceFleetId :: Lens.Lens' AddInstanceFleetResponse (Core.Maybe Types.InstanceFleetId)
aifrrsInstanceFleetId = Lens.field @"instanceFleetId"
{-# INLINEABLE aifrrsInstanceFleetId #-}
{-# DEPRECATED instanceFleetId "Use generic-lens or generic-optics with 'instanceFleetId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrrsResponseStatus :: Lens.Lens' AddInstanceFleetResponse Core.Int
aifrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aifrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
