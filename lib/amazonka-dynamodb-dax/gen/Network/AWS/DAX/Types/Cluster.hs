{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Cluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.Cluster
  ( Cluster (..)
  -- * Smart constructor
  , mkCluster
  -- * Lenses
  , cActiveNodes
  , cClusterArn
  , cClusterDiscoveryEndpoint
  , cClusterName
  , cDescription
  , cIamRoleArn
  , cNodeIdsToRemove
  , cNodeType
  , cNodes
  , cNotificationConfiguration
  , cParameterGroup
  , cPreferredMaintenanceWindow
  , cSSEDescription
  , cSecurityGroups
  , cStatus
  , cSubnetGroup
  , cTotalNodes
  ) where

import qualified Network.AWS.DAX.Types.Endpoint as Types
import qualified Network.AWS.DAX.Types.Node as Types
import qualified Network.AWS.DAX.Types.NotificationConfiguration as Types
import qualified Network.AWS.DAX.Types.ParameterGroupStatus as Types
import qualified Network.AWS.DAX.Types.SSEDescription as Types
import qualified Network.AWS.DAX.Types.SecurityGroupMembership as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains all of the attributes of a specific DAX cluster.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { activeNodes :: Core.Maybe Core.Int
    -- ^ The number of nodes in the cluster that are active (i.e., capable of serving requests).
  , clusterArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) that uniquely identifies the cluster. 
  , clusterDiscoveryEndpoint :: Core.Maybe Types.Endpoint
    -- ^ The configuration endpoint for this DAX cluster, consisting of a DNS name and a port number. Client applications can specify this endpoint, rather than an individual node endpoint, and allow the DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
  , clusterName :: Core.Maybe Core.Text
    -- ^ The name of the DAX cluster.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the cluster.
  , iamRoleArn :: Core.Maybe Core.Text
    -- ^ A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
  , nodeIdsToRemove :: Core.Maybe [Core.Text]
    -- ^ A list of nodes to be removed from the cluster.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The node type for the nodes in the cluster. (All nodes in a DAX cluster are of the same type.)
  , nodes :: Core.Maybe [Types.Node]
    -- ^ A list of nodes that are currently in the cluster.
  , notificationConfiguration :: Core.Maybe Types.NotificationConfiguration
    -- ^ Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
  , parameterGroup :: Core.Maybe Types.ParameterGroupStatus
    -- ^ The parameter group being used by nodes in the cluster.
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
  , sSEDescription :: Core.Maybe Types.SSEDescription
    -- ^ The description of the server-side encryption status on the specified DAX cluster.
  , securityGroups :: Core.Maybe [Types.SecurityGroupMembership]
    -- ^ A list of security groups, and the status of each, for the nodes in the cluster.
  , status :: Core.Maybe Core.Text
    -- ^ The current status of the cluster.
  , subnetGroup :: Core.Maybe Core.Text
    -- ^ The subnet group where the DAX cluster is running.
  , totalNodes :: Core.Maybe Core.Int
    -- ^ The total number of nodes in the cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Cluster' value with any optional fields omitted.
mkCluster
    :: Cluster
mkCluster
  = Cluster'{activeNodes = Core.Nothing, clusterArn = Core.Nothing,
             clusterDiscoveryEndpoint = Core.Nothing,
             clusterName = Core.Nothing, description = Core.Nothing,
             iamRoleArn = Core.Nothing, nodeIdsToRemove = Core.Nothing,
             nodeType = Core.Nothing, nodes = Core.Nothing,
             notificationConfiguration = Core.Nothing,
             parameterGroup = Core.Nothing,
             preferredMaintenanceWindow = Core.Nothing,
             sSEDescription = Core.Nothing, securityGroups = Core.Nothing,
             status = Core.Nothing, subnetGroup = Core.Nothing,
             totalNodes = Core.Nothing}

-- | The number of nodes in the cluster that are active (i.e., capable of serving requests).
--
-- /Note:/ Consider using 'activeNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cActiveNodes :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cActiveNodes = Lens.field @"activeNodes"
{-# INLINEABLE cActiveNodes #-}
{-# DEPRECATED activeNodes "Use generic-lens or generic-optics with 'activeNodes' instead"  #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster. 
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterArn :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cClusterArn = Lens.field @"clusterArn"
{-# INLINEABLE cClusterArn #-}
{-# DEPRECATED clusterArn "Use generic-lens or generic-optics with 'clusterArn' instead"  #-}

-- | The configuration endpoint for this DAX cluster, consisting of a DNS name and a port number. Client applications can specify this endpoint, rather than an individual node endpoint, and allow the DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
--
-- /Note:/ Consider using 'clusterDiscoveryEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterDiscoveryEndpoint :: Lens.Lens' Cluster (Core.Maybe Types.Endpoint)
cClusterDiscoveryEndpoint = Lens.field @"clusterDiscoveryEndpoint"
{-# INLINEABLE cClusterDiscoveryEndpoint #-}
{-# DEPRECATED clusterDiscoveryEndpoint "Use generic-lens or generic-optics with 'clusterDiscoveryEndpoint' instead"  #-}

-- | The name of the DAX cluster.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterName :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cClusterName = Lens.field @"clusterName"
{-# INLINEABLE cClusterName #-}
{-# DEPRECATED clusterName "Use generic-lens or generic-optics with 'clusterName' instead"  #-}

-- | The description of the cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cDescription = Lens.field @"description"
{-# INLINEABLE cDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIamRoleArn :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cIamRoleArn = Lens.field @"iamRoleArn"
{-# INLINEABLE cIamRoleArn #-}
{-# DEPRECATED iamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead"  #-}

-- | A list of nodes to be removed from the cluster.
--
-- /Note:/ Consider using 'nodeIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodeIdsToRemove :: Lens.Lens' Cluster (Core.Maybe [Core.Text])
cNodeIdsToRemove = Lens.field @"nodeIdsToRemove"
{-# INLINEABLE cNodeIdsToRemove #-}
{-# DEPRECATED nodeIdsToRemove "Use generic-lens or generic-optics with 'nodeIdsToRemove' instead"  #-}

-- | The node type for the nodes in the cluster. (All nodes in a DAX cluster are of the same type.)
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodeType :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cNodeType = Lens.field @"nodeType"
{-# INLINEABLE cNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | A list of nodes that are currently in the cluster.
--
-- /Note:/ Consider using 'nodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodes :: Lens.Lens' Cluster (Core.Maybe [Types.Node])
cNodes = Lens.field @"nodes"
{-# INLINEABLE cNodes #-}
{-# DEPRECATED nodes "Use generic-lens or generic-optics with 'nodes' instead"  #-}

-- | Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNotificationConfiguration :: Lens.Lens' Cluster (Core.Maybe Types.NotificationConfiguration)
cNotificationConfiguration = Lens.field @"notificationConfiguration"
{-# INLINEABLE cNotificationConfiguration #-}
{-# DEPRECATED notificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead"  #-}

-- | The parameter group being used by nodes in the cluster.
--
-- /Note:/ Consider using 'parameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParameterGroup :: Lens.Lens' Cluster (Core.Maybe Types.ParameterGroupStatus)
cParameterGroup = Lens.field @"parameterGroup"
{-# INLINEABLE cParameterGroup #-}
{-# DEPRECATED parameterGroup "Use generic-lens or generic-optics with 'parameterGroup' instead"  #-}

-- | A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPreferredMaintenanceWindow :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE cPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | The description of the server-side encryption status on the specified DAX cluster.
--
-- /Note:/ Consider using 'sSEDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSSEDescription :: Lens.Lens' Cluster (Core.Maybe Types.SSEDescription)
cSSEDescription = Lens.field @"sSEDescription"
{-# INLINEABLE cSSEDescription #-}
{-# DEPRECATED sSEDescription "Use generic-lens or generic-optics with 'sSEDescription' instead"  #-}

-- | A list of security groups, and the status of each, for the nodes in the cluster.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSecurityGroups :: Lens.Lens' Cluster (Core.Maybe [Types.SecurityGroupMembership])
cSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE cSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The current status of the cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cStatus = Lens.field @"status"
{-# INLINEABLE cStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The subnet group where the DAX cluster is running.
--
-- /Note:/ Consider using 'subnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSubnetGroup :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cSubnetGroup = Lens.field @"subnetGroup"
{-# INLINEABLE cSubnetGroup #-}
{-# DEPRECATED subnetGroup "Use generic-lens or generic-optics with 'subnetGroup' instead"  #-}

-- | The total number of nodes in the cluster.
--
-- /Note:/ Consider using 'totalNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTotalNodes :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cTotalNodes = Lens.field @"totalNodes"
{-# INLINEABLE cTotalNodes #-}
{-# DEPRECATED totalNodes "Use generic-lens or generic-optics with 'totalNodes' instead"  #-}

instance Core.FromJSON Cluster where
        parseJSON
          = Core.withObject "Cluster" Core.$
              \ x ->
                Cluster' Core.<$>
                  (x Core..:? "ActiveNodes") Core.<*> x Core..:? "ClusterArn"
                    Core.<*> x Core..:? "ClusterDiscoveryEndpoint"
                    Core.<*> x Core..:? "ClusterName"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "IamRoleArn"
                    Core.<*> x Core..:? "NodeIdsToRemove"
                    Core.<*> x Core..:? "NodeType"
                    Core.<*> x Core..:? "Nodes"
                    Core.<*> x Core..:? "NotificationConfiguration"
                    Core.<*> x Core..:? "ParameterGroup"
                    Core.<*> x Core..:? "PreferredMaintenanceWindow"
                    Core.<*> x Core..:? "SSEDescription"
                    Core.<*> x Core..:? "SecurityGroups"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "SubnetGroup"
                    Core.<*> x Core..:? "TotalNodes"
