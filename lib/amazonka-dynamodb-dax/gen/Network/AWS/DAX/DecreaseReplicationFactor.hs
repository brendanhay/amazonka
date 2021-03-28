{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DecreaseReplicationFactor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more nodes from a DAX cluster.
module Network.AWS.DAX.DecreaseReplicationFactor
    (
    -- * Creating a request
      DecreaseReplicationFactor (..)
    , mkDecreaseReplicationFactor
    -- ** Request lenses
    , drfClusterName
    , drfNewReplicationFactor
    , drfAvailabilityZones
    , drfNodeIdsToRemove

    -- * Destructuring the response
    , DecreaseReplicationFactorResponse (..)
    , mkDecreaseReplicationFactorResponse
    -- ** Response lenses
    , drfrrsCluster
    , drfrrsResponseStatus
    ) where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDecreaseReplicationFactor' smart constructor.
data DecreaseReplicationFactor = DecreaseReplicationFactor'
  { clusterName :: Core.Text
    -- ^ The name of the DAX cluster from which you want to remove nodes.
  , newReplicationFactor :: Core.Int
    -- ^ The new number of nodes for the DAX cluster.
  , availabilityZones :: Core.Maybe [Core.Text]
    -- ^ The Availability Zone(s) from which to remove nodes.
  , nodeIdsToRemove :: Core.Maybe [Core.Text]
    -- ^ The unique identifiers of the nodes to be removed from the cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecreaseReplicationFactor' value with any optional fields omitted.
mkDecreaseReplicationFactor
    :: Core.Text -- ^ 'clusterName'
    -> Core.Int -- ^ 'newReplicationFactor'
    -> DecreaseReplicationFactor
mkDecreaseReplicationFactor clusterName newReplicationFactor
  = DecreaseReplicationFactor'{clusterName, newReplicationFactor,
                               availabilityZones = Core.Nothing, nodeIdsToRemove = Core.Nothing}

-- | The name of the DAX cluster from which you want to remove nodes.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfClusterName :: Lens.Lens' DecreaseReplicationFactor Core.Text
drfClusterName = Lens.field @"clusterName"
{-# INLINEABLE drfClusterName #-}
{-# DEPRECATED clusterName "Use generic-lens or generic-optics with 'clusterName' instead"  #-}

-- | The new number of nodes for the DAX cluster.
--
-- /Note:/ Consider using 'newReplicationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfNewReplicationFactor :: Lens.Lens' DecreaseReplicationFactor Core.Int
drfNewReplicationFactor = Lens.field @"newReplicationFactor"
{-# INLINEABLE drfNewReplicationFactor #-}
{-# DEPRECATED newReplicationFactor "Use generic-lens or generic-optics with 'newReplicationFactor' instead"  #-}

-- | The Availability Zone(s) from which to remove nodes.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfAvailabilityZones :: Lens.Lens' DecreaseReplicationFactor (Core.Maybe [Core.Text])
drfAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE drfAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The unique identifiers of the nodes to be removed from the cluster.
--
-- /Note:/ Consider using 'nodeIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfNodeIdsToRemove :: Lens.Lens' DecreaseReplicationFactor (Core.Maybe [Core.Text])
drfNodeIdsToRemove = Lens.field @"nodeIdsToRemove"
{-# INLINEABLE drfNodeIdsToRemove #-}
{-# DEPRECATED nodeIdsToRemove "Use generic-lens or generic-optics with 'nodeIdsToRemove' instead"  #-}

instance Core.ToQuery DecreaseReplicationFactor where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DecreaseReplicationFactor where
        toHeaders DecreaseReplicationFactor{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDAXV3.DecreaseReplicationFactor")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DecreaseReplicationFactor where
        toJSON DecreaseReplicationFactor{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterName" Core..= clusterName),
                  Core.Just ("NewReplicationFactor" Core..= newReplicationFactor),
                  ("AvailabilityZones" Core..=) Core.<$> availabilityZones,
                  ("NodeIdsToRemove" Core..=) Core.<$> nodeIdsToRemove])

instance Core.AWSRequest DecreaseReplicationFactor where
        type Rs DecreaseReplicationFactor =
             DecreaseReplicationFactorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DecreaseReplicationFactorResponse' Core.<$>
                   (x Core..:? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDecreaseReplicationFactorResponse' smart constructor.
data DecreaseReplicationFactorResponse = DecreaseReplicationFactorResponse'
  { cluster :: Core.Maybe Types.Cluster
    -- ^ A description of the DAX cluster, after you have decreased its replication factor.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DecreaseReplicationFactorResponse' value with any optional fields omitted.
mkDecreaseReplicationFactorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DecreaseReplicationFactorResponse
mkDecreaseReplicationFactorResponse responseStatus
  = DecreaseReplicationFactorResponse'{cluster = Core.Nothing,
                                       responseStatus}

-- | A description of the DAX cluster, after you have decreased its replication factor.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrrsCluster :: Lens.Lens' DecreaseReplicationFactorResponse (Core.Maybe Types.Cluster)
drfrrsCluster = Lens.field @"cluster"
{-# INLINEABLE drfrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrrsResponseStatus :: Lens.Lens' DecreaseReplicationFactorResponse Core.Int
drfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
