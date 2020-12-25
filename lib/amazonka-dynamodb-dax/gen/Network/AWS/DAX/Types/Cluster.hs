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
    cActiveNodes,
    cClusterArn,
    cClusterDiscoveryEndpoint,
    cClusterName,
    cDescription,
    cIamRoleArn,
    cNodeIdsToRemove,
    cNodeType,
    cNodes,
    cNotificationConfiguration,
    cParameterGroup,
    cPreferredMaintenanceWindow,
    cSSEDescription,
    cSecurityGroups,
    cStatus,
    cSubnetGroup,
    cTotalNodes,
  )
where

import qualified Network.AWS.DAX.Types.ClusterArn as Types
import qualified Network.AWS.DAX.Types.ClusterName as Types
import qualified Network.AWS.DAX.Types.Description as Types
import qualified Network.AWS.DAX.Types.Endpoint as Types
import qualified Network.AWS.DAX.Types.IamRoleArn as Types
import qualified Network.AWS.DAX.Types.Node as Types
import qualified Network.AWS.DAX.Types.NodeType as Types
import qualified Network.AWS.DAX.Types.NotificationConfiguration as Types
import qualified Network.AWS.DAX.Types.ParameterGroupStatus as Types
import qualified Network.AWS.DAX.Types.PreferredMaintenanceWindow as Types
import qualified Network.AWS.DAX.Types.SSEDescription as Types
import qualified Network.AWS.DAX.Types.SecurityGroupMembership as Types
import qualified Network.AWS.DAX.Types.Status as Types
import qualified Network.AWS.DAX.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains all of the attributes of a specific DAX cluster.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { -- | The number of nodes in the cluster that are active (i.e., capable of serving requests).
    activeNodes :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Core.Maybe Types.ClusterArn,
    -- | The configuration endpoint for this DAX cluster, consisting of a DNS name and a port number. Client applications can specify this endpoint, rather than an individual node endpoint, and allow the DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
    clusterDiscoveryEndpoint :: Core.Maybe Types.Endpoint,
    -- | The name of the DAX cluster.
    clusterName :: Core.Maybe Types.ClusterName,
    -- | The description of the cluster.
    description :: Core.Maybe Types.Description,
    -- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
    iamRoleArn :: Core.Maybe Types.IamRoleArn,
    -- | A list of nodes to be removed from the cluster.
    nodeIdsToRemove :: Core.Maybe [Types.String],
    -- | The node type for the nodes in the cluster. (All nodes in a DAX cluster are of the same type.)
    nodeType :: Core.Maybe Types.NodeType,
    -- | A list of nodes that are currently in the cluster.
    nodes :: Core.Maybe [Types.Node],
    -- | Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
    notificationConfiguration :: Core.Maybe Types.NotificationConfiguration,
    -- | The parameter group being used by nodes in the cluster.
    parameterGroup :: Core.Maybe Types.ParameterGroupStatus,
    -- | A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
    preferredMaintenanceWindow :: Core.Maybe Types.PreferredMaintenanceWindow,
    -- | The description of the server-side encryption status on the specified DAX cluster.
    sSEDescription :: Core.Maybe Types.SSEDescription,
    -- | A list of security groups, and the status of each, for the nodes in the cluster.
    securityGroups :: Core.Maybe [Types.SecurityGroupMembership],
    -- | The current status of the cluster.
    status :: Core.Maybe Types.Status,
    -- | The subnet group where the DAX cluster is running.
    subnetGroup :: Core.Maybe Types.String,
    -- | The total number of nodes in the cluster.
    totalNodes :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Cluster' value with any optional fields omitted.
mkCluster ::
  Cluster
mkCluster =
  Cluster'
    { activeNodes = Core.Nothing,
      clusterArn = Core.Nothing,
      clusterDiscoveryEndpoint = Core.Nothing,
      clusterName = Core.Nothing,
      description = Core.Nothing,
      iamRoleArn = Core.Nothing,
      nodeIdsToRemove = Core.Nothing,
      nodeType = Core.Nothing,
      nodes = Core.Nothing,
      notificationConfiguration = Core.Nothing,
      parameterGroup = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      sSEDescription = Core.Nothing,
      securityGroups = Core.Nothing,
      status = Core.Nothing,
      subnetGroup = Core.Nothing,
      totalNodes = Core.Nothing
    }

-- | The number of nodes in the cluster that are active (i.e., capable of serving requests).
--
-- /Note:/ Consider using 'activeNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cActiveNodes :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cActiveNodes = Lens.field @"activeNodes"
{-# DEPRECATED cActiveNodes "Use generic-lens or generic-optics with 'activeNodes' instead." #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterArn :: Lens.Lens' Cluster (Core.Maybe Types.ClusterArn)
cClusterArn = Lens.field @"clusterArn"
{-# DEPRECATED cClusterArn "Use generic-lens or generic-optics with 'clusterArn' instead." #-}

-- | The configuration endpoint for this DAX cluster, consisting of a DNS name and a port number. Client applications can specify this endpoint, rather than an individual node endpoint, and allow the DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
--
-- /Note:/ Consider using 'clusterDiscoveryEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterDiscoveryEndpoint :: Lens.Lens' Cluster (Core.Maybe Types.Endpoint)
cClusterDiscoveryEndpoint = Lens.field @"clusterDiscoveryEndpoint"
{-# DEPRECATED cClusterDiscoveryEndpoint "Use generic-lens or generic-optics with 'clusterDiscoveryEndpoint' instead." #-}

-- | The name of the DAX cluster.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterName :: Lens.Lens' Cluster (Core.Maybe Types.ClusterName)
cClusterName = Lens.field @"clusterName"
{-# DEPRECATED cClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The description of the cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' Cluster (Core.Maybe Types.Description)
cDescription = Lens.field @"description"
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIamRoleArn :: Lens.Lens' Cluster (Core.Maybe Types.IamRoleArn)
cIamRoleArn = Lens.field @"iamRoleArn"
{-# DEPRECATED cIamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead." #-}

-- | A list of nodes to be removed from the cluster.
--
-- /Note:/ Consider using 'nodeIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodeIdsToRemove :: Lens.Lens' Cluster (Core.Maybe [Types.String])
cNodeIdsToRemove = Lens.field @"nodeIdsToRemove"
{-# DEPRECATED cNodeIdsToRemove "Use generic-lens or generic-optics with 'nodeIdsToRemove' instead." #-}

-- | The node type for the nodes in the cluster. (All nodes in a DAX cluster are of the same type.)
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodeType :: Lens.Lens' Cluster (Core.Maybe Types.NodeType)
cNodeType = Lens.field @"nodeType"
{-# DEPRECATED cNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | A list of nodes that are currently in the cluster.
--
-- /Note:/ Consider using 'nodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodes :: Lens.Lens' Cluster (Core.Maybe [Types.Node])
cNodes = Lens.field @"nodes"
{-# DEPRECATED cNodes "Use generic-lens or generic-optics with 'nodes' instead." #-}

-- | Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNotificationConfiguration :: Lens.Lens' Cluster (Core.Maybe Types.NotificationConfiguration)
cNotificationConfiguration = Lens.field @"notificationConfiguration"
{-# DEPRECATED cNotificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead." #-}

-- | The parameter group being used by nodes in the cluster.
--
-- /Note:/ Consider using 'parameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParameterGroup :: Lens.Lens' Cluster (Core.Maybe Types.ParameterGroupStatus)
cParameterGroup = Lens.field @"parameterGroup"
{-# DEPRECATED cParameterGroup "Use generic-lens or generic-optics with 'parameterGroup' instead." #-}

-- | A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPreferredMaintenanceWindow :: Lens.Lens' Cluster (Core.Maybe Types.PreferredMaintenanceWindow)
cPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED cPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The description of the server-side encryption status on the specified DAX cluster.
--
-- /Note:/ Consider using 'sSEDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSSEDescription :: Lens.Lens' Cluster (Core.Maybe Types.SSEDescription)
cSSEDescription = Lens.field @"sSEDescription"
{-# DEPRECATED cSSEDescription "Use generic-lens or generic-optics with 'sSEDescription' instead." #-}

-- | A list of security groups, and the status of each, for the nodes in the cluster.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSecurityGroups :: Lens.Lens' Cluster (Core.Maybe [Types.SecurityGroupMembership])
cSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED cSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The current status of the cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Cluster (Core.Maybe Types.Status)
cStatus = Lens.field @"status"
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The subnet group where the DAX cluster is running.
--
-- /Note:/ Consider using 'subnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSubnetGroup :: Lens.Lens' Cluster (Core.Maybe Types.String)
cSubnetGroup = Lens.field @"subnetGroup"
{-# DEPRECATED cSubnetGroup "Use generic-lens or generic-optics with 'subnetGroup' instead." #-}

-- | The total number of nodes in the cluster.
--
-- /Note:/ Consider using 'totalNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTotalNodes :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cTotalNodes = Lens.field @"totalNodes"
{-# DEPRECATED cTotalNodes "Use generic-lens or generic-optics with 'totalNodes' instead." #-}

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject "Cluster" Core.$
      \x ->
        Cluster'
          Core.<$> (x Core..:? "ActiveNodes")
          Core.<*> (x Core..:? "ClusterArn")
          Core.<*> (x Core..:? "ClusterDiscoveryEndpoint")
          Core.<*> (x Core..:? "ClusterName")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "IamRoleArn")
          Core.<*> (x Core..:? "NodeIdsToRemove")
          Core.<*> (x Core..:? "NodeType")
          Core.<*> (x Core..:? "Nodes")
          Core.<*> (x Core..:? "NotificationConfiguration")
          Core.<*> (x Core..:? "ParameterGroup")
          Core.<*> (x Core..:? "PreferredMaintenanceWindow")
          Core.<*> (x Core..:? "SSEDescription")
          Core.<*> (x Core..:? "SecurityGroups")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "SubnetGroup")
          Core.<*> (x Core..:? "TotalNodes")
