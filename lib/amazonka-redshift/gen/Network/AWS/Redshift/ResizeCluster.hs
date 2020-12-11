{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.Redshift.ResizeCluster
  ( -- * Creating a request
    ResizeCluster (..),
    mkResizeCluster,

    -- ** Request lenses
    rcNumberOfNodes,
    rcClassic,
    rcClusterType,
    rcNodeType,
    rcClusterIdentifier,

    -- * Destructuring the response
    ResizeClusterResponse (..),
    mkResizeClusterResponse,

    -- ** Response lenses
    resrsCluster,
    resrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Describes a resize cluster operation. For example, a scheduled action to run the @ResizeCluster@ API operation.
--
-- /See:/ 'mkResizeCluster' smart constructor.
data ResizeCluster = ResizeCluster'
  { numberOfNodes ::
      Lude.Maybe Lude.Int,
    classic :: Lude.Maybe Lude.Bool,
    clusterType :: Lude.Maybe Lude.Text,
    nodeType :: Lude.Maybe Lude.Text,
    clusterIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResizeCluster' with the minimum fields required to make a request.
--
-- * 'classic' - A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
-- * 'clusterIdentifier' - The unique identifier for the cluster to resize.
-- * 'clusterType' - The new cluster type for the specified cluster.
-- * 'nodeType' - The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
-- * 'numberOfNodes' - The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
mkResizeCluster ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  ResizeCluster
mkResizeCluster pClusterIdentifier_ =
  ResizeCluster'
    { numberOfNodes = Lude.Nothing,
      classic = Lude.Nothing,
      clusterType = Lude.Nothing,
      nodeType = Lude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcNumberOfNodes :: Lens.Lens' ResizeCluster (Lude.Maybe Lude.Int)
rcNumberOfNodes = Lens.lens (numberOfNodes :: ResizeCluster -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: ResizeCluster)
{-# DEPRECATED rcNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
--
-- /Note:/ Consider using 'classic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcClassic :: Lens.Lens' ResizeCluster (Lude.Maybe Lude.Bool)
rcClassic = Lens.lens (classic :: ResizeCluster -> Lude.Maybe Lude.Bool) (\s a -> s {classic = a} :: ResizeCluster)
{-# DEPRECATED rcClassic "Use generic-lens or generic-optics with 'classic' instead." #-}

-- | The new cluster type for the specified cluster.
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcClusterType :: Lens.Lens' ResizeCluster (Lude.Maybe Lude.Text)
rcClusterType = Lens.lens (clusterType :: ResizeCluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterType = a} :: ResizeCluster)
{-# DEPRECATED rcClusterType "Use generic-lens or generic-optics with 'clusterType' instead." #-}

-- | The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcNodeType :: Lens.Lens' ResizeCluster (Lude.Maybe Lude.Text)
rcNodeType = Lens.lens (nodeType :: ResizeCluster -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: ResizeCluster)
{-# DEPRECATED rcNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The unique identifier for the cluster to resize.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcClusterIdentifier :: Lens.Lens' ResizeCluster Lude.Text
rcClusterIdentifier = Lens.lens (clusterIdentifier :: ResizeCluster -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ResizeCluster)
{-# DEPRECATED rcClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest ResizeCluster where
  type Rs ResizeCluster = ResizeClusterResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ResizeClusterResult"
      ( \s h x ->
          ResizeClusterResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResizeCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResizeCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery ResizeCluster where
  toQuery ResizeCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ResizeCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "NumberOfNodes" Lude.=: numberOfNodes,
        "Classic" Lude.=: classic,
        "ClusterType" Lude.=: clusterType,
        "NodeType" Lude.=: nodeType,
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]

-- | /See:/ 'mkResizeClusterResponse' smart constructor.
data ResizeClusterResponse = ResizeClusterResponse'
  { cluster ::
      Lude.Maybe Cluster,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResizeClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkResizeClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResizeClusterResponse
mkResizeClusterResponse pResponseStatus_ =
  ResizeClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resrsCluster :: Lens.Lens' ResizeClusterResponse (Lude.Maybe Cluster)
resrsCluster = Lens.lens (cluster :: ResizeClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: ResizeClusterResponse)
{-# DEPRECATED resrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resrsResponseStatus :: Lens.Lens' ResizeClusterResponse Lude.Int
resrsResponseStatus = Lens.lens (responseStatus :: ResizeClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResizeClusterResponse)
{-# DEPRECATED resrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
