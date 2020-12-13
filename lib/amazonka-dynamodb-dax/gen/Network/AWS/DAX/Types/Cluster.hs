{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Cluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Cluster
  ( Cluster (..),

    -- * Smart constructor
    mkCluster,

    -- * Lenses
    cStatus,
    cIAMRoleARN,
    cClusterARN,
    cActiveNodes,
    cSecurityGroups,
    cNotificationConfiguration,
    cNodeIdsToRemove,
    cTotalNodes,
    cPreferredMaintenanceWindow,
    cSubnetGroup,
    cClusterName,
    cNodeType,
    cNodes,
    cClusterDiscoveryEndpoint,
    cSSEDescription,
    cDescription,
    cParameterGroup,
  )
where

import Network.AWS.DAX.Types.Endpoint
import Network.AWS.DAX.Types.Node
import Network.AWS.DAX.Types.NotificationConfiguration
import Network.AWS.DAX.Types.ParameterGroupStatus
import Network.AWS.DAX.Types.SSEDescription
import Network.AWS.DAX.Types.SecurityGroupMembership
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains all of the attributes of a specific DAX cluster.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { -- | The current status of the cluster.
    status :: Lude.Maybe Lude.Text,
    -- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
    iamRoleARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterARN :: Lude.Maybe Lude.Text,
    -- | The number of nodes in the cluster that are active (i.e., capable of serving requests).
    activeNodes :: Lude.Maybe Lude.Int,
    -- | A list of security groups, and the status of each, for the nodes in the cluster.
    securityGroups :: Lude.Maybe [SecurityGroupMembership],
    -- | Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
    notificationConfiguration :: Lude.Maybe NotificationConfiguration,
    -- | A list of nodes to be removed from the cluster.
    nodeIdsToRemove :: Lude.Maybe [Lude.Text],
    -- | The total number of nodes in the cluster.
    totalNodes :: Lude.Maybe Lude.Int,
    -- | A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    -- | The subnet group where the DAX cluster is running.
    subnetGroup :: Lude.Maybe Lude.Text,
    -- | The name of the DAX cluster.
    clusterName :: Lude.Maybe Lude.Text,
    -- | The node type for the nodes in the cluster. (All nodes in a DAX cluster are of the same type.)
    nodeType :: Lude.Maybe Lude.Text,
    -- | A list of nodes that are currently in the cluster.
    nodes :: Lude.Maybe [Node],
    -- | The configuration endpoint for this DAX cluster, consisting of a DNS name and a port number. Client applications can specify this endpoint, rather than an individual node endpoint, and allow the DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
    clusterDiscoveryEndpoint :: Lude.Maybe Endpoint,
    -- | The description of the server-side encryption status on the specified DAX cluster.
    sSEDescription :: Lude.Maybe SSEDescription,
    -- | The description of the cluster.
    description :: Lude.Maybe Lude.Text,
    -- | The parameter group being used by nodes in the cluster.
    parameterGroup :: Lude.Maybe ParameterGroupStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the cluster.
-- * 'iamRoleARN' - A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
-- * 'clusterARN' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
-- * 'activeNodes' - The number of nodes in the cluster that are active (i.e., capable of serving requests).
-- * 'securityGroups' - A list of security groups, and the status of each, for the nodes in the cluster.
-- * 'notificationConfiguration' - Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
-- * 'nodeIdsToRemove' - A list of nodes to be removed from the cluster.
-- * 'totalNodes' - The total number of nodes in the cluster.
-- * 'preferredMaintenanceWindow' - A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
-- * 'subnetGroup' - The subnet group where the DAX cluster is running.
-- * 'clusterName' - The name of the DAX cluster.
-- * 'nodeType' - The node type for the nodes in the cluster. (All nodes in a DAX cluster are of the same type.)
-- * 'nodes' - A list of nodes that are currently in the cluster.
-- * 'clusterDiscoveryEndpoint' - The configuration endpoint for this DAX cluster, consisting of a DNS name and a port number. Client applications can specify this endpoint, rather than an individual node endpoint, and allow the DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
-- * 'sSEDescription' - The description of the server-side encryption status on the specified DAX cluster.
-- * 'description' - The description of the cluster.
-- * 'parameterGroup' - The parameter group being used by nodes in the cluster.
mkCluster ::
  Cluster
mkCluster =
  Cluster'
    { status = Lude.Nothing,
      iamRoleARN = Lude.Nothing,
      clusterARN = Lude.Nothing,
      activeNodes = Lude.Nothing,
      securityGroups = Lude.Nothing,
      notificationConfiguration = Lude.Nothing,
      nodeIdsToRemove = Lude.Nothing,
      totalNodes = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      subnetGroup = Lude.Nothing,
      clusterName = Lude.Nothing,
      nodeType = Lude.Nothing,
      nodes = Lude.Nothing,
      clusterDiscoveryEndpoint = Lude.Nothing,
      sSEDescription = Lude.Nothing,
      description = Lude.Nothing,
      parameterGroup = Lude.Nothing
    }

-- | The current status of the cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cStatus = Lens.lens (status :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Cluster)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIAMRoleARN :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cIAMRoleARN = Lens.lens (iamRoleARN :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: Cluster)
{-# DEPRECATED cIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterARN :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterARN = Lens.lens (clusterARN :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: Cluster)
{-# DEPRECATED cClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | The number of nodes in the cluster that are active (i.e., capable of serving requests).
--
-- /Note:/ Consider using 'activeNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cActiveNodes :: Lens.Lens' Cluster (Lude.Maybe Lude.Int)
cActiveNodes = Lens.lens (activeNodes :: Cluster -> Lude.Maybe Lude.Int) (\s a -> s {activeNodes = a} :: Cluster)
{-# DEPRECATED cActiveNodes "Use generic-lens or generic-optics with 'activeNodes' instead." #-}

-- | A list of security groups, and the status of each, for the nodes in the cluster.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSecurityGroups :: Lens.Lens' Cluster (Lude.Maybe [SecurityGroupMembership])
cSecurityGroups = Lens.lens (securityGroups :: Cluster -> Lude.Maybe [SecurityGroupMembership]) (\s a -> s {securityGroups = a} :: Cluster)
{-# DEPRECATED cSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNotificationConfiguration :: Lens.Lens' Cluster (Lude.Maybe NotificationConfiguration)
cNotificationConfiguration = Lens.lens (notificationConfiguration :: Cluster -> Lude.Maybe NotificationConfiguration) (\s a -> s {notificationConfiguration = a} :: Cluster)
{-# DEPRECATED cNotificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead." #-}

-- | A list of nodes to be removed from the cluster.
--
-- /Note:/ Consider using 'nodeIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodeIdsToRemove :: Lens.Lens' Cluster (Lude.Maybe [Lude.Text])
cNodeIdsToRemove = Lens.lens (nodeIdsToRemove :: Cluster -> Lude.Maybe [Lude.Text]) (\s a -> s {nodeIdsToRemove = a} :: Cluster)
{-# DEPRECATED cNodeIdsToRemove "Use generic-lens or generic-optics with 'nodeIdsToRemove' instead." #-}

-- | The total number of nodes in the cluster.
--
-- /Note:/ Consider using 'totalNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTotalNodes :: Lens.Lens' Cluster (Lude.Maybe Lude.Int)
cTotalNodes = Lens.lens (totalNodes :: Cluster -> Lude.Maybe Lude.Int) (\s a -> s {totalNodes = a} :: Cluster)
{-# DEPRECATED cTotalNodes "Use generic-lens or generic-optics with 'totalNodes' instead." #-}

-- | A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPreferredMaintenanceWindow :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: Cluster)
{-# DEPRECATED cPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The subnet group where the DAX cluster is running.
--
-- /Note:/ Consider using 'subnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSubnetGroup :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cSubnetGroup = Lens.lens (subnetGroup :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {subnetGroup = a} :: Cluster)
{-# DEPRECATED cSubnetGroup "Use generic-lens or generic-optics with 'subnetGroup' instead." #-}

-- | The name of the DAX cluster.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterName :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterName = Lens.lens (clusterName :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterName = a} :: Cluster)
{-# DEPRECATED cClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The node type for the nodes in the cluster. (All nodes in a DAX cluster are of the same type.)
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodeType :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cNodeType = Lens.lens (nodeType :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: Cluster)
{-# DEPRECATED cNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | A list of nodes that are currently in the cluster.
--
-- /Note:/ Consider using 'nodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodes :: Lens.Lens' Cluster (Lude.Maybe [Node])
cNodes = Lens.lens (nodes :: Cluster -> Lude.Maybe [Node]) (\s a -> s {nodes = a} :: Cluster)
{-# DEPRECATED cNodes "Use generic-lens or generic-optics with 'nodes' instead." #-}

-- | The configuration endpoint for this DAX cluster, consisting of a DNS name and a port number. Client applications can specify this endpoint, rather than an individual node endpoint, and allow the DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
--
-- /Note:/ Consider using 'clusterDiscoveryEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterDiscoveryEndpoint :: Lens.Lens' Cluster (Lude.Maybe Endpoint)
cClusterDiscoveryEndpoint = Lens.lens (clusterDiscoveryEndpoint :: Cluster -> Lude.Maybe Endpoint) (\s a -> s {clusterDiscoveryEndpoint = a} :: Cluster)
{-# DEPRECATED cClusterDiscoveryEndpoint "Use generic-lens or generic-optics with 'clusterDiscoveryEndpoint' instead." #-}

-- | The description of the server-side encryption status on the specified DAX cluster.
--
-- /Note:/ Consider using 'sSEDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSSEDescription :: Lens.Lens' Cluster (Lude.Maybe SSEDescription)
cSSEDescription = Lens.lens (sSEDescription :: Cluster -> Lude.Maybe SSEDescription) (\s a -> s {sSEDescription = a} :: Cluster)
{-# DEPRECATED cSSEDescription "Use generic-lens or generic-optics with 'sSEDescription' instead." #-}

-- | The description of the cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cDescription = Lens.lens (description :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Cluster)
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The parameter group being used by nodes in the cluster.
--
-- /Note:/ Consider using 'parameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParameterGroup :: Lens.Lens' Cluster (Lude.Maybe ParameterGroupStatus)
cParameterGroup = Lens.lens (parameterGroup :: Cluster -> Lude.Maybe ParameterGroupStatus) (\s a -> s {parameterGroup = a} :: Cluster)
{-# DEPRECATED cParameterGroup "Use generic-lens or generic-optics with 'parameterGroup' instead." #-}

instance Lude.FromJSON Cluster where
  parseJSON =
    Lude.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "IamRoleArn")
            Lude.<*> (x Lude..:? "ClusterArn")
            Lude.<*> (x Lude..:? "ActiveNodes")
            Lude.<*> (x Lude..:? "SecurityGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NotificationConfiguration")
            Lude.<*> (x Lude..:? "NodeIdsToRemove" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TotalNodes")
            Lude.<*> (x Lude..:? "PreferredMaintenanceWindow")
            Lude.<*> (x Lude..:? "SubnetGroup")
            Lude.<*> (x Lude..:? "ClusterName")
            Lude.<*> (x Lude..:? "NodeType")
            Lude.<*> (x Lude..:? "Nodes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ClusterDiscoveryEndpoint")
            Lude.<*> (x Lude..:? "SSEDescription")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ParameterGroup")
      )
