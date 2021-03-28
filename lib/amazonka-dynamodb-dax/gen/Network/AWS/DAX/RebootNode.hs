{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.RebootNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a single node of a DAX cluster. The reboot action takes place as soon as possible. During the reboot, the node status is set to REBOOTING.
module Network.AWS.DAX.RebootNode
    (
    -- * Creating a request
      RebootNode (..)
    , mkRebootNode
    -- ** Request lenses
    , rnClusterName
    , rnNodeId

    -- * Destructuring the response
    , RebootNodeResponse (..)
    , mkRebootNodeResponse
    -- ** Response lenses
    , rnrrsCluster
    , rnrrsResponseStatus
    ) where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRebootNode' smart constructor.
data RebootNode = RebootNode'
  { clusterName :: Core.Text
    -- ^ The name of the DAX cluster containing the node to be rebooted.
  , nodeId :: Core.Text
    -- ^ The system-assigned ID of the node to be rebooted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebootNode' value with any optional fields omitted.
mkRebootNode
    :: Core.Text -- ^ 'clusterName'
    -> Core.Text -- ^ 'nodeId'
    -> RebootNode
mkRebootNode clusterName nodeId = RebootNode'{clusterName, nodeId}

-- | The name of the DAX cluster containing the node to be rebooted.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnClusterName :: Lens.Lens' RebootNode Core.Text
rnClusterName = Lens.field @"clusterName"
{-# INLINEABLE rnClusterName #-}
{-# DEPRECATED clusterName "Use generic-lens or generic-optics with 'clusterName' instead"  #-}

-- | The system-assigned ID of the node to be rebooted.
--
-- /Note:/ Consider using 'nodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnNodeId :: Lens.Lens' RebootNode Core.Text
rnNodeId = Lens.field @"nodeId"
{-# INLINEABLE rnNodeId #-}
{-# DEPRECATED nodeId "Use generic-lens or generic-optics with 'nodeId' instead"  #-}

instance Core.ToQuery RebootNode where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RebootNode where
        toHeaders RebootNode{..}
          = Core.pure ("X-Amz-Target", "AmazonDAXV3.RebootNode") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RebootNode where
        toJSON RebootNode{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterName" Core..= clusterName),
                  Core.Just ("NodeId" Core..= nodeId)])

instance Core.AWSRequest RebootNode where
        type Rs RebootNode = RebootNodeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RebootNodeResponse' Core.<$>
                   (x Core..:? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRebootNodeResponse' smart constructor.
data RebootNodeResponse = RebootNodeResponse'
  { cluster :: Core.Maybe Types.Cluster
    -- ^ A description of the DAX cluster after a node has been rebooted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RebootNodeResponse' value with any optional fields omitted.
mkRebootNodeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RebootNodeResponse
mkRebootNodeResponse responseStatus
  = RebootNodeResponse'{cluster = Core.Nothing, responseStatus}

-- | A description of the DAX cluster after a node has been rebooted.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnrrsCluster :: Lens.Lens' RebootNodeResponse (Core.Maybe Types.Cluster)
rnrrsCluster = Lens.field @"cluster"
{-# INLINEABLE rnrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnrrsResponseStatus :: Lens.Lens' RebootNodeResponse Core.Int
rnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
