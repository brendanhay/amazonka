{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RebootNode (..),
    mkRebootNode,

    -- ** Request lenses
    rnClusterName,
    rnNodeId,

    -- * Destructuring the response
    RebootNodeResponse (..),
    mkRebootNodeResponse,

    -- ** Response lenses
    rnrrsCluster,
    rnrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRebootNode' smart constructor.
data RebootNode = RebootNode'
  { -- | The name of the DAX cluster containing the node to be rebooted.
    clusterName :: Types.String,
    -- | The system-assigned ID of the node to be rebooted.
    nodeId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebootNode' value with any optional fields omitted.
mkRebootNode ::
  -- | 'clusterName'
  Types.String ->
  -- | 'nodeId'
  Types.String ->
  RebootNode
mkRebootNode clusterName nodeId = RebootNode' {clusterName, nodeId}

-- | The name of the DAX cluster containing the node to be rebooted.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnClusterName :: Lens.Lens' RebootNode Types.String
rnClusterName = Lens.field @"clusterName"
{-# DEPRECATED rnClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The system-assigned ID of the node to be rebooted.
--
-- /Note:/ Consider using 'nodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnNodeId :: Lens.Lens' RebootNode Types.String
rnNodeId = Lens.field @"nodeId"
{-# DEPRECATED rnNodeId "Use generic-lens or generic-optics with 'nodeId' instead." #-}

instance Core.FromJSON RebootNode where
  toJSON RebootNode {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterName" Core..= clusterName),
            Core.Just ("NodeId" Core..= nodeId)
          ]
      )

instance Core.AWSRequest RebootNode where
  type Rs RebootNode = RebootNodeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.RebootNode")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootNodeResponse'
            Core.<$> (x Core..:? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRebootNodeResponse' smart constructor.
data RebootNodeResponse = RebootNodeResponse'
  { -- | A description of the DAX cluster after a node has been rebooted.
    cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RebootNodeResponse' value with any optional fields omitted.
mkRebootNodeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RebootNodeResponse
mkRebootNodeResponse responseStatus =
  RebootNodeResponse' {cluster = Core.Nothing, responseStatus}

-- | A description of the DAX cluster after a node has been rebooted.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnrrsCluster :: Lens.Lens' RebootNodeResponse (Core.Maybe Types.Cluster)
rnrrsCluster = Lens.field @"cluster"
{-# DEPRECATED rnrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnrrsResponseStatus :: Lens.Lens' RebootNodeResponse Core.Int
rnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
