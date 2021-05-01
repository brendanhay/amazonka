{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ResizeCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the size of the cluster. You can change the cluster\'s type, or
-- change the number or type of nodes. The default behavior is to use the
-- elastic resize method. With an elastic resize, your cluster is available
-- for read and write operations more quickly than with the classic resize
-- method.
--
-- Elastic resize operations have the following restrictions:
--
-- -   You can only resize clusters of the following types:
--
--     -   dc1.large (if your cluster is in a VPC)
--
--     -   dc1.8xlarge (if your cluster is in a VPC)
--
--     -   dc2.large
--
--     -   dc2.8xlarge
--
--     -   ds2.xlarge
--
--     -   ds2.8xlarge
--
--     -   ra3.xlplus
--
--     -   ra3.4xlarge
--
--     -   ra3.16xlarge
--
-- -   The type of nodes that you add must match the node type for the
--     cluster.
module Network.AWS.Redshift.ResizeCluster
  ( -- * Creating a Request
    ResizeCluster (..),
    newResizeCluster,

    -- * Request Lenses
    resizeCluster_classic,
    resizeCluster_clusterType,
    resizeCluster_numberOfNodes,
    resizeCluster_nodeType,
    resizeCluster_clusterIdentifier,

    -- * Destructuring the Response
    ResizeClusterResponse (..),
    newResizeClusterResponse,

    -- * Response Lenses
    resizeClusterResponse_cluster,
    resizeClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a resize cluster operation. For example, a scheduled action to
-- run the @ResizeCluster@ API operation.
--
-- /See:/ 'newResizeCluster' smart constructor.
data ResizeCluster = ResizeCluster'
  { -- | A boolean value indicating whether the resize operation is using the
    -- classic resize process. If you don\'t provide this parameter or set the
    -- value to @false@, the resize type is elastic.
    classic :: Prelude.Maybe Prelude.Bool,
    -- | The new cluster type for the specified cluster.
    clusterType :: Prelude.Maybe Prelude.Text,
    -- | The new number of nodes for the cluster. If not specified, the
    -- cluster\'s current number of nodes is used.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The new node type for the nodes you are adding. If not specified, the
    -- cluster\'s current node type is used.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the cluster to resize.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResizeCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classic', 'resizeCluster_classic' - A boolean value indicating whether the resize operation is using the
-- classic resize process. If you don\'t provide this parameter or set the
-- value to @false@, the resize type is elastic.
--
-- 'clusterType', 'resizeCluster_clusterType' - The new cluster type for the specified cluster.
--
-- 'numberOfNodes', 'resizeCluster_numberOfNodes' - The new number of nodes for the cluster. If not specified, the
-- cluster\'s current number of nodes is used.
--
-- 'nodeType', 'resizeCluster_nodeType' - The new node type for the nodes you are adding. If not specified, the
-- cluster\'s current node type is used.
--
-- 'clusterIdentifier', 'resizeCluster_clusterIdentifier' - The unique identifier for the cluster to resize.
newResizeCluster ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ResizeCluster
newResizeCluster pClusterIdentifier_ =
  ResizeCluster'
    { classic = Prelude.Nothing,
      clusterType = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | A boolean value indicating whether the resize operation is using the
-- classic resize process. If you don\'t provide this parameter or set the
-- value to @false@, the resize type is elastic.
resizeCluster_classic :: Lens.Lens' ResizeCluster (Prelude.Maybe Prelude.Bool)
resizeCluster_classic = Lens.lens (\ResizeCluster' {classic} -> classic) (\s@ResizeCluster' {} a -> s {classic = a} :: ResizeCluster)

-- | The new cluster type for the specified cluster.
resizeCluster_clusterType :: Lens.Lens' ResizeCluster (Prelude.Maybe Prelude.Text)
resizeCluster_clusterType = Lens.lens (\ResizeCluster' {clusterType} -> clusterType) (\s@ResizeCluster' {} a -> s {clusterType = a} :: ResizeCluster)

-- | The new number of nodes for the cluster. If not specified, the
-- cluster\'s current number of nodes is used.
resizeCluster_numberOfNodes :: Lens.Lens' ResizeCluster (Prelude.Maybe Prelude.Int)
resizeCluster_numberOfNodes = Lens.lens (\ResizeCluster' {numberOfNodes} -> numberOfNodes) (\s@ResizeCluster' {} a -> s {numberOfNodes = a} :: ResizeCluster)

-- | The new node type for the nodes you are adding. If not specified, the
-- cluster\'s current node type is used.
resizeCluster_nodeType :: Lens.Lens' ResizeCluster (Prelude.Maybe Prelude.Text)
resizeCluster_nodeType = Lens.lens (\ResizeCluster' {nodeType} -> nodeType) (\s@ResizeCluster' {} a -> s {nodeType = a} :: ResizeCluster)

-- | The unique identifier for the cluster to resize.
resizeCluster_clusterIdentifier :: Lens.Lens' ResizeCluster Prelude.Text
resizeCluster_clusterIdentifier = Lens.lens (\ResizeCluster' {clusterIdentifier} -> clusterIdentifier) (\s@ResizeCluster' {} a -> s {clusterIdentifier = a} :: ResizeCluster)

instance Prelude.AWSRequest ResizeCluster where
  type Rs ResizeCluster = ResizeClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ResizeClusterResult"
      ( \s h x ->
          ResizeClusterResponse'
            Prelude.<$> (x Prelude..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResizeCluster

instance Prelude.NFData ResizeCluster

instance Prelude.ToHeaders ResizeCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ResizeCluster where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResizeCluster where
  toQuery ResizeCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ResizeCluster" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "Classic" Prelude.=: classic,
        "ClusterType" Prelude.=: clusterType,
        "NumberOfNodes" Prelude.=: numberOfNodes,
        "NodeType" Prelude.=: nodeType,
        "ClusterIdentifier" Prelude.=: clusterIdentifier
      ]

-- | /See:/ 'newResizeClusterResponse' smart constructor.
data ResizeClusterResponse = ResizeClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResizeClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'resizeClusterResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'resizeClusterResponse_httpStatus' - The response's http status code.
newResizeClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResizeClusterResponse
newResizeClusterResponse pHttpStatus_ =
  ResizeClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
resizeClusterResponse_cluster :: Lens.Lens' ResizeClusterResponse (Prelude.Maybe Cluster)
resizeClusterResponse_cluster = Lens.lens (\ResizeClusterResponse' {cluster} -> cluster) (\s@ResizeClusterResponse' {} a -> s {cluster = a} :: ResizeClusterResponse)

-- | The response's http status code.
resizeClusterResponse_httpStatus :: Lens.Lens' ResizeClusterResponse Prelude.Int
resizeClusterResponse_httpStatus = Lens.lens (\ResizeClusterResponse' {httpStatus} -> httpStatus) (\s@ResizeClusterResponse' {} a -> s {httpStatus = a} :: ResizeClusterResponse)

instance Prelude.NFData ResizeClusterResponse
