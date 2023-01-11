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
-- Module      : Amazonka.DAX.Types.Cluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.Cluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types.ClusterEndpointEncryptionType
import Amazonka.DAX.Types.Endpoint
import Amazonka.DAX.Types.Node
import Amazonka.DAX.Types.NotificationConfiguration
import Amazonka.DAX.Types.ParameterGroupStatus
import Amazonka.DAX.Types.SSEDescription
import Amazonka.DAX.Types.SecurityGroupMembership
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains all of the attributes of a specific DAX cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The number of nodes in the cluster that are active (i.e., capable of
    -- serving requests).
    activeNodes :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The endpoint for this DAX cluster, consisting of a DNS name, a port
    -- number, and a URL. Applications should use the URL to configure the DAX
    -- client to find their cluster.
    clusterDiscoveryEndpoint :: Prelude.Maybe Endpoint,
    -- | The type of encryption supported by the cluster\'s endpoint. Values are:
    --
    -- -   @NONE@ for no encryption
    --
    --     @TLS@ for Transport Layer Security
    clusterEndpointEncryptionType :: Prelude.Maybe ClusterEndpointEncryptionType,
    -- | The name of the DAX cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The description of the cluster.
    description :: Prelude.Maybe Prelude.Text,
    -- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At
    -- runtime, DAX will assume this role and use the role\'s permissions to
    -- access DynamoDB on your behalf.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of nodes to be removed from the cluster.
    nodeIdsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The node type for the nodes in the cluster. (All nodes in a DAX cluster
    -- are of the same type.)
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | A list of nodes that are currently in the cluster.
    nodes :: Prelude.Maybe [Node],
    -- | Describes a notification topic and its status. Notification topics are
    -- used for publishing DAX events to subscribers using Amazon Simple
    -- Notification Service (SNS).
    notificationConfiguration :: Prelude.Maybe NotificationConfiguration,
    -- | The parameter group being used by nodes in the cluster.
    parameterGroup :: Prelude.Maybe ParameterGroupStatus,
    -- | A range of time when maintenance of DAX cluster software will be
    -- performed. For example: @sun:01:00-sun:09:00@. Cluster maintenance
    -- normally takes less than 30 minutes, and is performed automatically
    -- within the maintenance window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The description of the server-side encryption status on the specified
    -- DAX cluster.
    sSEDescription :: Prelude.Maybe SSEDescription,
    -- | A list of security groups, and the status of each, for the nodes in the
    -- cluster.
    securityGroups :: Prelude.Maybe [SecurityGroupMembership],
    -- | The current status of the cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | The subnet group where the DAX cluster is running.
    subnetGroup :: Prelude.Maybe Prelude.Text,
    -- | The total number of nodes in the cluster.
    totalNodes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeNodes', 'cluster_activeNodes' - The number of nodes in the cluster that are active (i.e., capable of
-- serving requests).
--
-- 'clusterArn', 'cluster_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- 'clusterDiscoveryEndpoint', 'cluster_clusterDiscoveryEndpoint' - The endpoint for this DAX cluster, consisting of a DNS name, a port
-- number, and a URL. Applications should use the URL to configure the DAX
-- client to find their cluster.
--
-- 'clusterEndpointEncryptionType', 'cluster_clusterEndpointEncryptionType' - The type of encryption supported by the cluster\'s endpoint. Values are:
--
-- -   @NONE@ for no encryption
--
--     @TLS@ for Transport Layer Security
--
-- 'clusterName', 'cluster_clusterName' - The name of the DAX cluster.
--
-- 'description', 'cluster_description' - The description of the cluster.
--
-- 'iamRoleArn', 'cluster_iamRoleArn' - A valid Amazon Resource Name (ARN) that identifies an IAM role. At
-- runtime, DAX will assume this role and use the role\'s permissions to
-- access DynamoDB on your behalf.
--
-- 'nodeIdsToRemove', 'cluster_nodeIdsToRemove' - A list of nodes to be removed from the cluster.
--
-- 'nodeType', 'cluster_nodeType' - The node type for the nodes in the cluster. (All nodes in a DAX cluster
-- are of the same type.)
--
-- 'nodes', 'cluster_nodes' - A list of nodes that are currently in the cluster.
--
-- 'notificationConfiguration', 'cluster_notificationConfiguration' - Describes a notification topic and its status. Notification topics are
-- used for publishing DAX events to subscribers using Amazon Simple
-- Notification Service (SNS).
--
-- 'parameterGroup', 'cluster_parameterGroup' - The parameter group being used by nodes in the cluster.
--
-- 'preferredMaintenanceWindow', 'cluster_preferredMaintenanceWindow' - A range of time when maintenance of DAX cluster software will be
-- performed. For example: @sun:01:00-sun:09:00@. Cluster maintenance
-- normally takes less than 30 minutes, and is performed automatically
-- within the maintenance window.
--
-- 'sSEDescription', 'cluster_sSEDescription' - The description of the server-side encryption status on the specified
-- DAX cluster.
--
-- 'securityGroups', 'cluster_securityGroups' - A list of security groups, and the status of each, for the nodes in the
-- cluster.
--
-- 'status', 'cluster_status' - The current status of the cluster.
--
-- 'subnetGroup', 'cluster_subnetGroup' - The subnet group where the DAX cluster is running.
--
-- 'totalNodes', 'cluster_totalNodes' - The total number of nodes in the cluster.
newCluster ::
  Cluster
newCluster =
  Cluster'
    { activeNodes = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      clusterDiscoveryEndpoint = Prelude.Nothing,
      clusterEndpointEncryptionType = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      description = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      nodeIdsToRemove = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      nodes = Prelude.Nothing,
      notificationConfiguration = Prelude.Nothing,
      parameterGroup = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      sSEDescription = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      status = Prelude.Nothing,
      subnetGroup = Prelude.Nothing,
      totalNodes = Prelude.Nothing
    }

-- | The number of nodes in the cluster that are active (i.e., capable of
-- serving requests).
cluster_activeNodes :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_activeNodes = Lens.lens (\Cluster' {activeNodes} -> activeNodes) (\s@Cluster' {} a -> s {activeNodes = a} :: Cluster)

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
cluster_clusterArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clusterArn = Lens.lens (\Cluster' {clusterArn} -> clusterArn) (\s@Cluster' {} a -> s {clusterArn = a} :: Cluster)

-- | The endpoint for this DAX cluster, consisting of a DNS name, a port
-- number, and a URL. Applications should use the URL to configure the DAX
-- client to find their cluster.
cluster_clusterDiscoveryEndpoint :: Lens.Lens' Cluster (Prelude.Maybe Endpoint)
cluster_clusterDiscoveryEndpoint = Lens.lens (\Cluster' {clusterDiscoveryEndpoint} -> clusterDiscoveryEndpoint) (\s@Cluster' {} a -> s {clusterDiscoveryEndpoint = a} :: Cluster)

-- | The type of encryption supported by the cluster\'s endpoint. Values are:
--
-- -   @NONE@ for no encryption
--
--     @TLS@ for Transport Layer Security
cluster_clusterEndpointEncryptionType :: Lens.Lens' Cluster (Prelude.Maybe ClusterEndpointEncryptionType)
cluster_clusterEndpointEncryptionType = Lens.lens (\Cluster' {clusterEndpointEncryptionType} -> clusterEndpointEncryptionType) (\s@Cluster' {} a -> s {clusterEndpointEncryptionType = a} :: Cluster)

-- | The name of the DAX cluster.
cluster_clusterName :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clusterName = Lens.lens (\Cluster' {clusterName} -> clusterName) (\s@Cluster' {} a -> s {clusterName = a} :: Cluster)

-- | The description of the cluster.
cluster_description :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_description = Lens.lens (\Cluster' {description} -> description) (\s@Cluster' {} a -> s {description = a} :: Cluster)

-- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At
-- runtime, DAX will assume this role and use the role\'s permissions to
-- access DynamoDB on your behalf.
cluster_iamRoleArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_iamRoleArn = Lens.lens (\Cluster' {iamRoleArn} -> iamRoleArn) (\s@Cluster' {} a -> s {iamRoleArn = a} :: Cluster)

-- | A list of nodes to be removed from the cluster.
cluster_nodeIdsToRemove :: Lens.Lens' Cluster (Prelude.Maybe [Prelude.Text])
cluster_nodeIdsToRemove = Lens.lens (\Cluster' {nodeIdsToRemove} -> nodeIdsToRemove) (\s@Cluster' {} a -> s {nodeIdsToRemove = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The node type for the nodes in the cluster. (All nodes in a DAX cluster
-- are of the same type.)
cluster_nodeType :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_nodeType = Lens.lens (\Cluster' {nodeType} -> nodeType) (\s@Cluster' {} a -> s {nodeType = a} :: Cluster)

-- | A list of nodes that are currently in the cluster.
cluster_nodes :: Lens.Lens' Cluster (Prelude.Maybe [Node])
cluster_nodes = Lens.lens (\Cluster' {nodes} -> nodes) (\s@Cluster' {} a -> s {nodes = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | Describes a notification topic and its status. Notification topics are
-- used for publishing DAX events to subscribers using Amazon Simple
-- Notification Service (SNS).
cluster_notificationConfiguration :: Lens.Lens' Cluster (Prelude.Maybe NotificationConfiguration)
cluster_notificationConfiguration = Lens.lens (\Cluster' {notificationConfiguration} -> notificationConfiguration) (\s@Cluster' {} a -> s {notificationConfiguration = a} :: Cluster)

-- | The parameter group being used by nodes in the cluster.
cluster_parameterGroup :: Lens.Lens' Cluster (Prelude.Maybe ParameterGroupStatus)
cluster_parameterGroup = Lens.lens (\Cluster' {parameterGroup} -> parameterGroup) (\s@Cluster' {} a -> s {parameterGroup = a} :: Cluster)

-- | A range of time when maintenance of DAX cluster software will be
-- performed. For example: @sun:01:00-sun:09:00@. Cluster maintenance
-- normally takes less than 30 minutes, and is performed automatically
-- within the maintenance window.
cluster_preferredMaintenanceWindow :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_preferredMaintenanceWindow = Lens.lens (\Cluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@Cluster' {} a -> s {preferredMaintenanceWindow = a} :: Cluster)

-- | The description of the server-side encryption status on the specified
-- DAX cluster.
cluster_sSEDescription :: Lens.Lens' Cluster (Prelude.Maybe SSEDescription)
cluster_sSEDescription = Lens.lens (\Cluster' {sSEDescription} -> sSEDescription) (\s@Cluster' {} a -> s {sSEDescription = a} :: Cluster)

-- | A list of security groups, and the status of each, for the nodes in the
-- cluster.
cluster_securityGroups :: Lens.Lens' Cluster (Prelude.Maybe [SecurityGroupMembership])
cluster_securityGroups = Lens.lens (\Cluster' {securityGroups} -> securityGroups) (\s@Cluster' {} a -> s {securityGroups = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the cluster.
cluster_status :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

-- | The subnet group where the DAX cluster is running.
cluster_subnetGroup :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_subnetGroup = Lens.lens (\Cluster' {subnetGroup} -> subnetGroup) (\s@Cluster' {} a -> s {subnetGroup = a} :: Cluster)

-- | The total number of nodes in the cluster.
cluster_totalNodes :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_totalNodes = Lens.lens (\Cluster' {totalNodes} -> totalNodes) (\s@Cluster' {} a -> s {totalNodes = a} :: Cluster)

instance Data.FromJSON Cluster where
  parseJSON =
    Data.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Data..:? "ActiveNodes")
            Prelude.<*> (x Data..:? "ClusterArn")
            Prelude.<*> (x Data..:? "ClusterDiscoveryEndpoint")
            Prelude.<*> (x Data..:? "ClusterEndpointEncryptionType")
            Prelude.<*> (x Data..:? "ClusterName")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "IamRoleArn")
            Prelude.<*> ( x Data..:? "NodeIdsToRemove"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "Nodes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NotificationConfiguration")
            Prelude.<*> (x Data..:? "ParameterGroup")
            Prelude.<*> (x Data..:? "PreferredMaintenanceWindow")
            Prelude.<*> (x Data..:? "SSEDescription")
            Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "SubnetGroup")
            Prelude.<*> (x Data..:? "TotalNodes")
      )

instance Prelude.Hashable Cluster where
  hashWithSalt _salt Cluster' {..} =
    _salt `Prelude.hashWithSalt` activeNodes
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` clusterDiscoveryEndpoint
      `Prelude.hashWithSalt` clusterEndpointEncryptionType
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` nodeIdsToRemove
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` nodes
      `Prelude.hashWithSalt` notificationConfiguration
      `Prelude.hashWithSalt` parameterGroup
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` sSEDescription
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subnetGroup
      `Prelude.hashWithSalt` totalNodes

instance Prelude.NFData Cluster where
  rnf Cluster' {..} =
    Prelude.rnf activeNodes
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterDiscoveryEndpoint
      `Prelude.seq` Prelude.rnf clusterEndpointEncryptionType
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf nodeIdsToRemove
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf nodes
      `Prelude.seq` Prelude.rnf notificationConfiguration
      `Prelude.seq` Prelude.rnf parameterGroup
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf sSEDescription
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subnetGroup
      `Prelude.seq` Prelude.rnf totalNodes
