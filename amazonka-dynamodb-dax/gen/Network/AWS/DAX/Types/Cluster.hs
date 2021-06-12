{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Cluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Cluster where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types.Endpoint
import Network.AWS.DAX.Types.Node
import Network.AWS.DAX.Types.NotificationConfiguration
import Network.AWS.DAX.Types.ParameterGroupStatus
import Network.AWS.DAX.Types.SSEDescription
import Network.AWS.DAX.Types.SecurityGroupMembership
import qualified Network.AWS.Lens as Lens

-- | Contains all of the attributes of a specific DAX cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Core.Maybe Core.Text,
    -- | The subnet group where the DAX cluster is running.
    subnetGroup :: Core.Maybe Core.Text,
    -- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At
    -- runtime, DAX will assume this role and use the role\'s permissions to
    -- access DynamoDB on your behalf.
    iamRoleArn :: Core.Maybe Core.Text,
    -- | The current status of the cluster.
    status :: Core.Maybe Core.Text,
    -- | The total number of nodes in the cluster.
    totalNodes :: Core.Maybe Core.Int,
    -- | The parameter group being used by nodes in the cluster.
    parameterGroup :: Core.Maybe ParameterGroupStatus,
    -- | A list of nodes that are currently in the cluster.
    nodes :: Core.Maybe [Node],
    -- | Describes a notification topic and its status. Notification topics are
    -- used for publishing DAX events to subscribers using Amazon Simple
    -- Notification Service (SNS).
    notificationConfiguration :: Core.Maybe NotificationConfiguration,
    -- | A list of security groups, and the status of each, for the nodes in the
    -- cluster.
    securityGroups :: Core.Maybe [SecurityGroupMembership],
    -- | The number of nodes in the cluster that are active (i.e., capable of
    -- serving requests).
    activeNodes :: Core.Maybe Core.Int,
    -- | A range of time when maintenance of DAX cluster software will be
    -- performed. For example: @sun:01:00-sun:09:00@. Cluster maintenance
    -- normally takes less than 30 minutes, and is performed automatically
    -- within the maintenance window.
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | The description of the cluster.
    description :: Core.Maybe Core.Text,
    -- | The description of the server-side encryption status on the specified
    -- DAX cluster.
    sSEDescription :: Core.Maybe SSEDescription,
    -- | The configuration endpoint for this DAX cluster, consisting of a DNS
    -- name and a port number. Client applications can specify this endpoint,
    -- rather than an individual node endpoint, and allow the DAX client
    -- software to intelligently route requests and responses to nodes in the
    -- DAX cluster.
    clusterDiscoveryEndpoint :: Core.Maybe Endpoint,
    -- | A list of nodes to be removed from the cluster.
    nodeIdsToRemove :: Core.Maybe [Core.Text],
    -- | The node type for the nodes in the cluster. (All nodes in a DAX cluster
    -- are of the same type.)
    nodeType :: Core.Maybe Core.Text,
    -- | The name of the DAX cluster.
    clusterName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'cluster_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- 'subnetGroup', 'cluster_subnetGroup' - The subnet group where the DAX cluster is running.
--
-- 'iamRoleArn', 'cluster_iamRoleArn' - A valid Amazon Resource Name (ARN) that identifies an IAM role. At
-- runtime, DAX will assume this role and use the role\'s permissions to
-- access DynamoDB on your behalf.
--
-- 'status', 'cluster_status' - The current status of the cluster.
--
-- 'totalNodes', 'cluster_totalNodes' - The total number of nodes in the cluster.
--
-- 'parameterGroup', 'cluster_parameterGroup' - The parameter group being used by nodes in the cluster.
--
-- 'nodes', 'cluster_nodes' - A list of nodes that are currently in the cluster.
--
-- 'notificationConfiguration', 'cluster_notificationConfiguration' - Describes a notification topic and its status. Notification topics are
-- used for publishing DAX events to subscribers using Amazon Simple
-- Notification Service (SNS).
--
-- 'securityGroups', 'cluster_securityGroups' - A list of security groups, and the status of each, for the nodes in the
-- cluster.
--
-- 'activeNodes', 'cluster_activeNodes' - The number of nodes in the cluster that are active (i.e., capable of
-- serving requests).
--
-- 'preferredMaintenanceWindow', 'cluster_preferredMaintenanceWindow' - A range of time when maintenance of DAX cluster software will be
-- performed. For example: @sun:01:00-sun:09:00@. Cluster maintenance
-- normally takes less than 30 minutes, and is performed automatically
-- within the maintenance window.
--
-- 'description', 'cluster_description' - The description of the cluster.
--
-- 'sSEDescription', 'cluster_sSEDescription' - The description of the server-side encryption status on the specified
-- DAX cluster.
--
-- 'clusterDiscoveryEndpoint', 'cluster_clusterDiscoveryEndpoint' - The configuration endpoint for this DAX cluster, consisting of a DNS
-- name and a port number. Client applications can specify this endpoint,
-- rather than an individual node endpoint, and allow the DAX client
-- software to intelligently route requests and responses to nodes in the
-- DAX cluster.
--
-- 'nodeIdsToRemove', 'cluster_nodeIdsToRemove' - A list of nodes to be removed from the cluster.
--
-- 'nodeType', 'cluster_nodeType' - The node type for the nodes in the cluster. (All nodes in a DAX cluster
-- are of the same type.)
--
-- 'clusterName', 'cluster_clusterName' - The name of the DAX cluster.
newCluster ::
  Cluster
newCluster =
  Cluster'
    { clusterArn = Core.Nothing,
      subnetGroup = Core.Nothing,
      iamRoleArn = Core.Nothing,
      status = Core.Nothing,
      totalNodes = Core.Nothing,
      parameterGroup = Core.Nothing,
      nodes = Core.Nothing,
      notificationConfiguration = Core.Nothing,
      securityGroups = Core.Nothing,
      activeNodes = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      description = Core.Nothing,
      sSEDescription = Core.Nothing,
      clusterDiscoveryEndpoint = Core.Nothing,
      nodeIdsToRemove = Core.Nothing,
      nodeType = Core.Nothing,
      clusterName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
cluster_clusterArn :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterArn = Lens.lens (\Cluster' {clusterArn} -> clusterArn) (\s@Cluster' {} a -> s {clusterArn = a} :: Cluster)

-- | The subnet group where the DAX cluster is running.
cluster_subnetGroup :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_subnetGroup = Lens.lens (\Cluster' {subnetGroup} -> subnetGroup) (\s@Cluster' {} a -> s {subnetGroup = a} :: Cluster)

-- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At
-- runtime, DAX will assume this role and use the role\'s permissions to
-- access DynamoDB on your behalf.
cluster_iamRoleArn :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_iamRoleArn = Lens.lens (\Cluster' {iamRoleArn} -> iamRoleArn) (\s@Cluster' {} a -> s {iamRoleArn = a} :: Cluster)

-- | The current status of the cluster.
cluster_status :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

-- | The total number of nodes in the cluster.
cluster_totalNodes :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cluster_totalNodes = Lens.lens (\Cluster' {totalNodes} -> totalNodes) (\s@Cluster' {} a -> s {totalNodes = a} :: Cluster)

-- | The parameter group being used by nodes in the cluster.
cluster_parameterGroup :: Lens.Lens' Cluster (Core.Maybe ParameterGroupStatus)
cluster_parameterGroup = Lens.lens (\Cluster' {parameterGroup} -> parameterGroup) (\s@Cluster' {} a -> s {parameterGroup = a} :: Cluster)

-- | A list of nodes that are currently in the cluster.
cluster_nodes :: Lens.Lens' Cluster (Core.Maybe [Node])
cluster_nodes = Lens.lens (\Cluster' {nodes} -> nodes) (\s@Cluster' {} a -> s {nodes = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | Describes a notification topic and its status. Notification topics are
-- used for publishing DAX events to subscribers using Amazon Simple
-- Notification Service (SNS).
cluster_notificationConfiguration :: Lens.Lens' Cluster (Core.Maybe NotificationConfiguration)
cluster_notificationConfiguration = Lens.lens (\Cluster' {notificationConfiguration} -> notificationConfiguration) (\s@Cluster' {} a -> s {notificationConfiguration = a} :: Cluster)

-- | A list of security groups, and the status of each, for the nodes in the
-- cluster.
cluster_securityGroups :: Lens.Lens' Cluster (Core.Maybe [SecurityGroupMembership])
cluster_securityGroups = Lens.lens (\Cluster' {securityGroups} -> securityGroups) (\s@Cluster' {} a -> s {securityGroups = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | The number of nodes in the cluster that are active (i.e., capable of
-- serving requests).
cluster_activeNodes :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cluster_activeNodes = Lens.lens (\Cluster' {activeNodes} -> activeNodes) (\s@Cluster' {} a -> s {activeNodes = a} :: Cluster)

-- | A range of time when maintenance of DAX cluster software will be
-- performed. For example: @sun:01:00-sun:09:00@. Cluster maintenance
-- normally takes less than 30 minutes, and is performed automatically
-- within the maintenance window.
cluster_preferredMaintenanceWindow :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_preferredMaintenanceWindow = Lens.lens (\Cluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@Cluster' {} a -> s {preferredMaintenanceWindow = a} :: Cluster)

-- | The description of the cluster.
cluster_description :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_description = Lens.lens (\Cluster' {description} -> description) (\s@Cluster' {} a -> s {description = a} :: Cluster)

-- | The description of the server-side encryption status on the specified
-- DAX cluster.
cluster_sSEDescription :: Lens.Lens' Cluster (Core.Maybe SSEDescription)
cluster_sSEDescription = Lens.lens (\Cluster' {sSEDescription} -> sSEDescription) (\s@Cluster' {} a -> s {sSEDescription = a} :: Cluster)

-- | The configuration endpoint for this DAX cluster, consisting of a DNS
-- name and a port number. Client applications can specify this endpoint,
-- rather than an individual node endpoint, and allow the DAX client
-- software to intelligently route requests and responses to nodes in the
-- DAX cluster.
cluster_clusterDiscoveryEndpoint :: Lens.Lens' Cluster (Core.Maybe Endpoint)
cluster_clusterDiscoveryEndpoint = Lens.lens (\Cluster' {clusterDiscoveryEndpoint} -> clusterDiscoveryEndpoint) (\s@Cluster' {} a -> s {clusterDiscoveryEndpoint = a} :: Cluster)

-- | A list of nodes to be removed from the cluster.
cluster_nodeIdsToRemove :: Lens.Lens' Cluster (Core.Maybe [Core.Text])
cluster_nodeIdsToRemove = Lens.lens (\Cluster' {nodeIdsToRemove} -> nodeIdsToRemove) (\s@Cluster' {} a -> s {nodeIdsToRemove = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | The node type for the nodes in the cluster. (All nodes in a DAX cluster
-- are of the same type.)
cluster_nodeType :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_nodeType = Lens.lens (\Cluster' {nodeType} -> nodeType) (\s@Cluster' {} a -> s {nodeType = a} :: Cluster)

-- | The name of the DAX cluster.
cluster_clusterName :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterName = Lens.lens (\Cluster' {clusterName} -> clusterName) (\s@Cluster' {} a -> s {clusterName = a} :: Cluster)

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Core.<$> (x Core..:? "ClusterArn")
            Core.<*> (x Core..:? "SubnetGroup")
            Core.<*> (x Core..:? "IamRoleArn")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "TotalNodes")
            Core.<*> (x Core..:? "ParameterGroup")
            Core.<*> (x Core..:? "Nodes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NotificationConfiguration")
            Core.<*> (x Core..:? "SecurityGroups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ActiveNodes")
            Core.<*> (x Core..:? "PreferredMaintenanceWindow")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "SSEDescription")
            Core.<*> (x Core..:? "ClusterDiscoveryEndpoint")
            Core.<*> (x Core..:? "NodeIdsToRemove" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NodeType")
            Core.<*> (x Core..:? "ClusterName")
      )

instance Core.Hashable Cluster

instance Core.NFData Cluster
