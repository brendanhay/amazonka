{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ResizeCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the size of the cluster. You can change the cluster's type, or change the number or type of nodes. The default behavior is to use the elastic resize method. With an elastic resize, your cluster is available for read and write operations more quickly than with the classic resize method. 
--
-- Elastic resize operations have the following restrictions:
--
--     * You can only resize clusters of the following types:
--
--     * dc1.large (if your cluster is in a VPC)
--
--
--     * dc1.8xlarge (if your cluster is in a VPC)
--
--
--     * dc2.large
--
--
--     * dc2.8xlarge
--
--
--     * ds2.xlarge
--
--
--     * ds2.8xlarge
--
--
--     * ra3.4xlarge
--
--
--     * ra3.16xlarge
--
--
--
--
--     * The type of nodes that you add must match the node type for the cluster.
--
--
module Network.AWS.Redshift.ResizeCluster
    (
    -- * Creating a request
      ResizeCluster (..)
    , mkResizeCluster
    -- ** Request lenses
    , rcClusterIdentifier
    , rcClassic
    , rcClusterType
    , rcNodeType
    , rcNumberOfNodes

    -- * Destructuring the response
    , ResizeClusterResponse (..)
    , mkResizeClusterResponse
    -- ** Response lenses
    , rcrrsCluster
    , rcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a resize cluster operation. For example, a scheduled action to run the @ResizeCluster@ API operation. 
--
-- /See:/ 'mkResizeCluster' smart constructor.
data ResizeCluster = ResizeCluster'
  { clusterIdentifier :: Core.Text
    -- ^ The unique identifier for the cluster to resize.
  , classic :: Core.Maybe Core.Bool
    -- ^ A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic. 
  , clusterType :: Core.Maybe Core.Text
    -- ^ The new cluster type for the specified cluster.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResizeCluster' value with any optional fields omitted.
mkResizeCluster
    :: Core.Text -- ^ 'clusterIdentifier'
    -> ResizeCluster
mkResizeCluster clusterIdentifier
  = ResizeCluster'{clusterIdentifier, classic = Core.Nothing,
                   clusterType = Core.Nothing, nodeType = Core.Nothing,
                   numberOfNodes = Core.Nothing}

-- | The unique identifier for the cluster to resize.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcClusterIdentifier :: Lens.Lens' ResizeCluster Core.Text
rcClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE rcClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic. 
--
-- /Note:/ Consider using 'classic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcClassic :: Lens.Lens' ResizeCluster (Core.Maybe Core.Bool)
rcClassic = Lens.field @"classic"
{-# INLINEABLE rcClassic #-}
{-# DEPRECATED classic "Use generic-lens or generic-optics with 'classic' instead"  #-}

-- | The new cluster type for the specified cluster.
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcClusterType :: Lens.Lens' ResizeCluster (Core.Maybe Core.Text)
rcClusterType = Lens.field @"clusterType"
{-# INLINEABLE rcClusterType #-}
{-# DEPRECATED clusterType "Use generic-lens or generic-optics with 'clusterType' instead"  #-}

-- | The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcNodeType :: Lens.Lens' ResizeCluster (Core.Maybe Core.Text)
rcNodeType = Lens.field @"nodeType"
{-# INLINEABLE rcNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcNumberOfNodes :: Lens.Lens' ResizeCluster (Core.Maybe Core.Int)
rcNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE rcNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

instance Core.ToQuery ResizeCluster where
        toQuery ResizeCluster{..}
          = Core.toQueryPair "Action" ("ResizeCluster" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Classic") classic
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterType") clusterType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NodeType") nodeType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NumberOfNodes")
                numberOfNodes

instance Core.ToHeaders ResizeCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ResizeCluster where
        type Rs ResizeCluster = ResizeClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ResizeClusterResult"
              (\ s h x ->
                 ResizeClusterResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResizeClusterResponse' smart constructor.
data ResizeClusterResponse = ResizeClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResizeClusterResponse' value with any optional fields omitted.
mkResizeClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResizeClusterResponse
mkResizeClusterResponse responseStatus
  = ResizeClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsCluster :: Lens.Lens' ResizeClusterResponse (Core.Maybe Types.Cluster)
rcrrsCluster = Lens.field @"cluster"
{-# INLINEABLE rcrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsResponseStatus :: Lens.Lens' ResizeClusterResponse Core.Int
rcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
