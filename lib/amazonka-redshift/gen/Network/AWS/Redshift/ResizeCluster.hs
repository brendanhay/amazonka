{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    rcfClusterIdentifier,
    rcfNumberOfNodes,
    rcfClassic,
    rcfClusterType,
    rcfNodeType,

    -- * Destructuring the response
    ResizeClusterResponse (..),
    mkResizeClusterResponse,

    -- ** Response lenses
    rcfrsCluster,
    rcfrsResponseStatus,
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
  { -- | The unique identifier for the cluster to resize.
    clusterIdentifier :: Lude.Text,
    -- | The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
    numberOfNodes :: Lude.Maybe Lude.Int,
    -- | A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
    classic :: Lude.Maybe Lude.Bool,
    -- | The new cluster type for the specified cluster.
    clusterType :: Lude.Maybe Lude.Text,
    -- | The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
    nodeType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResizeCluster' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The unique identifier for the cluster to resize.
-- * 'numberOfNodes' - The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
-- * 'classic' - A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
-- * 'clusterType' - The new cluster type for the specified cluster.
-- * 'nodeType' - The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
mkResizeCluster ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  ResizeCluster
mkResizeCluster pClusterIdentifier_ =
  ResizeCluster'
    { clusterIdentifier = pClusterIdentifier_,
      numberOfNodes = Lude.Nothing,
      classic = Lude.Nothing,
      clusterType = Lude.Nothing,
      nodeType = Lude.Nothing
    }

-- | The unique identifier for the cluster to resize.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfClusterIdentifier :: Lens.Lens' ResizeCluster Lude.Text
rcfClusterIdentifier = Lens.lens (clusterIdentifier :: ResizeCluster -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ResizeCluster)
{-# DEPRECATED rcfClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfNumberOfNodes :: Lens.Lens' ResizeCluster (Lude.Maybe Lude.Int)
rcfNumberOfNodes = Lens.lens (numberOfNodes :: ResizeCluster -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: ResizeCluster)
{-# DEPRECATED rcfNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
--
-- /Note:/ Consider using 'classic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfClassic :: Lens.Lens' ResizeCluster (Lude.Maybe Lude.Bool)
rcfClassic = Lens.lens (classic :: ResizeCluster -> Lude.Maybe Lude.Bool) (\s a -> s {classic = a} :: ResizeCluster)
{-# DEPRECATED rcfClassic "Use generic-lens or generic-optics with 'classic' instead." #-}

-- | The new cluster type for the specified cluster.
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfClusterType :: Lens.Lens' ResizeCluster (Lude.Maybe Lude.Text)
rcfClusterType = Lens.lens (clusterType :: ResizeCluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterType = a} :: ResizeCluster)
{-# DEPRECATED rcfClusterType "Use generic-lens or generic-optics with 'clusterType' instead." #-}

-- | The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfNodeType :: Lens.Lens' ResizeCluster (Lude.Maybe Lude.Text)
rcfNodeType = Lens.lens (nodeType :: ResizeCluster -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: ResizeCluster)
{-# DEPRECATED rcfNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

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
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "NumberOfNodes" Lude.=: numberOfNodes,
        "Classic" Lude.=: classic,
        "ClusterType" Lude.=: clusterType,
        "NodeType" Lude.=: nodeType
      ]

-- | /See:/ 'mkResizeClusterResponse' smart constructor.
data ResizeClusterResponse = ResizeClusterResponse'
  { cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResizeClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' -
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
rcfrsCluster :: Lens.Lens' ResizeClusterResponse (Lude.Maybe Cluster)
rcfrsCluster = Lens.lens (cluster :: ResizeClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: ResizeClusterResponse)
{-# DEPRECATED rcfrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfrsResponseStatus :: Lens.Lens' ResizeClusterResponse Lude.Int
rcfrsResponseStatus = Lens.lens (responseStatus :: ResizeClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResizeClusterResponse)
{-# DEPRECATED rcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
