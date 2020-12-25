{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.CreateCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a DAX cluster. All nodes in the cluster run the same DAX caching software.
module Network.AWS.DAX.CreateCluster
  ( -- * Creating a request
    CreateCluster (..),
    mkCreateCluster,

    -- ** Request lenses
    ccClusterName,
    ccNodeType,
    ccReplicationFactor,
    ccIamRoleArn,
    ccAvailabilityZones,
    ccDescription,
    ccNotificationTopicArn,
    ccParameterGroupName,
    ccPreferredMaintenanceWindow,
    ccSSESpecification,
    ccSecurityGroupIds,
    ccSubnetGroupName,
    ccTags,

    -- * Destructuring the response
    CreateClusterResponse (..),
    mkCreateClusterResponse,

    -- ** Response lenses
    ccrrsCluster,
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | The cluster identifier. This parameter is stored as a lowercase string.
    --
    -- __Constraints:__
    --
    --     * A name must contain from 1 to 20 alphanumeric characters or hyphens.
    --
    --
    --     * The first character must be a letter.
    --
    --
    --     * A name cannot end with a hyphen or contain two consecutive hyphens.
    clusterName :: Types.ClusterName,
    -- | The compute and memory capacity of the nodes in the cluster.
    nodeType :: Types.NodeType,
    -- | The number of nodes in the DAX cluster. A replication factor of 1 will create a single-node cluster, without any read replicas. For additional fault tolerance, you can create a multiple node cluster with one or more read replicas. To do this, set @ReplicationFactor@ to a number between 3 (one primary and two read replicas) and 10 (one primary and nine read replicas). @If the AvailabilityZones@ parameter is provided, its length must equal the @ReplicationFactor@ .
    replicationFactor :: Core.Int,
    -- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
    iamRoleArn :: Types.IamRoleArn,
    -- | The Availability Zones (AZs) in which the cluster nodes will reside after the cluster has been created or updated. If provided, the length of this list must equal the @ReplicationFactor@ parameter. If you omit this parameter, DAX will spread the nodes across Availability Zones for the highest availability.
    availabilityZones :: Core.Maybe [Types.String],
    -- | A description of the cluster.
    description :: Core.Maybe Types.Description,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications will be sent.
    notificationTopicArn :: Core.Maybe Types.NotificationTopicArn,
    -- | The parameter group to be associated with the DAX cluster.
    parameterGroupName :: Core.Maybe Types.ParameterGroupName,
    -- | Specifies the weekly time range during which maintenance on the DAX cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:
    --
    --
    --     * @sun@
    --
    --
    --     * @mon@
    --
    --
    --     * @tue@
    --
    --
    --     * @wed@
    --
    --
    --     * @thu@
    --
    --
    --     * @fri@
    --
    --
    --     * @sat@
    --
    --
    -- Example: @sun:05:00-sun:09:00@
    preferredMaintenanceWindow :: Core.Maybe Types.PreferredMaintenanceWindow,
    -- | Represents the settings used to enable server-side encryption on the cluster.
    sSESpecification :: Core.Maybe Types.SSESpecification,
    -- | A list of security group IDs to be assigned to each node in the DAX cluster. (Each of the security group ID is system-generated.)
    --
    -- If this parameter is not specified, DAX assigns the default VPC security group to each node.
    securityGroupIds :: Core.Maybe [Types.String],
    -- | The name of the subnet group to be used for the replication group.
    --
    -- /Important:/ DAX clusters can only run in an Amazon VPC environment. All of the subnets that you specify in a subnet group must exist in the same VPC.
    subnetGroupName :: Core.Maybe Types.SubnetGroupName,
    -- | A set of tags to associate with the DAX cluster.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCluster' value with any optional fields omitted.
mkCreateCluster ::
  -- | 'clusterName'
  Types.ClusterName ->
  -- | 'nodeType'
  Types.NodeType ->
  -- | 'replicationFactor'
  Core.Int ->
  -- | 'iamRoleArn'
  Types.IamRoleArn ->
  CreateCluster
mkCreateCluster clusterName nodeType replicationFactor iamRoleArn =
  CreateCluster'
    { clusterName,
      nodeType,
      replicationFactor,
      iamRoleArn,
      availabilityZones = Core.Nothing,
      description = Core.Nothing,
      notificationTopicArn = Core.Nothing,
      parameterGroupName = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      sSESpecification = Core.Nothing,
      securityGroupIds = Core.Nothing,
      subnetGroupName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The cluster identifier. This parameter is stored as a lowercase string.
--
-- __Constraints:__
--
--     * A name must contain from 1 to 20 alphanumeric characters or hyphens.
--
--
--     * The first character must be a letter.
--
--
--     * A name cannot end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterName :: Lens.Lens' CreateCluster Types.ClusterName
ccClusterName = Lens.field @"clusterName"
{-# DEPRECATED ccClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The compute and memory capacity of the nodes in the cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNodeType :: Lens.Lens' CreateCluster Types.NodeType
ccNodeType = Lens.field @"nodeType"
{-# DEPRECATED ccNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The number of nodes in the DAX cluster. A replication factor of 1 will create a single-node cluster, without any read replicas. For additional fault tolerance, you can create a multiple node cluster with one or more read replicas. To do this, set @ReplicationFactor@ to a number between 3 (one primary and two read replicas) and 10 (one primary and nine read replicas). @If the AvailabilityZones@ parameter is provided, its length must equal the @ReplicationFactor@ .
--
-- /Note:/ Consider using 'replicationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccReplicationFactor :: Lens.Lens' CreateCluster Core.Int
ccReplicationFactor = Lens.field @"replicationFactor"
{-# DEPRECATED ccReplicationFactor "Use generic-lens or generic-optics with 'replicationFactor' instead." #-}

-- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccIamRoleArn :: Lens.Lens' CreateCluster Types.IamRoleArn
ccIamRoleArn = Lens.field @"iamRoleArn"
{-# DEPRECATED ccIamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead." #-}

-- | The Availability Zones (AZs) in which the cluster nodes will reside after the cluster has been created or updated. If provided, the length of this list must equal the @ReplicationFactor@ parameter. If you omit this parameter, DAX will spread the nodes across Availability Zones for the highest availability.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAvailabilityZones :: Lens.Lens' CreateCluster (Core.Maybe [Types.String])
ccAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED ccAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A description of the cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' CreateCluster (Core.Maybe Types.Description)
ccDescription = Lens.field @"description"
{-# DEPRECATED ccDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications will be sent.
--
-- /Note:/ Consider using 'notificationTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNotificationTopicArn :: Lens.Lens' CreateCluster (Core.Maybe Types.NotificationTopicArn)
ccNotificationTopicArn = Lens.field @"notificationTopicArn"
{-# DEPRECATED ccNotificationTopicArn "Use generic-lens or generic-optics with 'notificationTopicArn' instead." #-}

-- | The parameter group to be associated with the DAX cluster.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccParameterGroupName :: Lens.Lens' CreateCluster (Core.Maybe Types.ParameterGroupName)
ccParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED ccParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | Specifies the weekly time range during which maintenance on the DAX cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:
--
--
--     * @sun@
--
--
--     * @mon@
--
--
--     * @tue@
--
--
--     * @wed@
--
--
--     * @thu@
--
--
--     * @fri@
--
--
--     * @sat@
--
--
-- Example: @sun:05:00-sun:09:00@
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPreferredMaintenanceWindow :: Lens.Lens' CreateCluster (Core.Maybe Types.PreferredMaintenanceWindow)
ccPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED ccPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | Represents the settings used to enable server-side encryption on the cluster.
--
-- /Note:/ Consider using 'sSESpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSSESpecification :: Lens.Lens' CreateCluster (Core.Maybe Types.SSESpecification)
ccSSESpecification = Lens.field @"sSESpecification"
{-# DEPRECATED ccSSESpecification "Use generic-lens or generic-optics with 'sSESpecification' instead." #-}

-- | A list of security group IDs to be assigned to each node in the DAX cluster. (Each of the security group ID is system-generated.)
--
-- If this parameter is not specified, DAX assigns the default VPC security group to each node.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSecurityGroupIds :: Lens.Lens' CreateCluster (Core.Maybe [Types.String])
ccSecurityGroupIds = Lens.field @"securityGroupIds"
{-# DEPRECATED ccSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The name of the subnet group to be used for the replication group.
--
-- /Important:/ DAX clusters can only run in an Amazon VPC environment. All of the subnets that you specify in a subnet group must exist in the same VPC.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSubnetGroupName :: Lens.Lens' CreateCluster (Core.Maybe Types.SubnetGroupName)
ccSubnetGroupName = Lens.field @"subnetGroupName"
{-# DEPRECATED ccSubnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead." #-}

-- | A set of tags to associate with the DAX cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateCluster (Core.Maybe [Types.Tag])
ccTags = Lens.field @"tags"
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateCluster where
  toJSON CreateCluster {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterName" Core..= clusterName),
            Core.Just ("NodeType" Core..= nodeType),
            Core.Just ("ReplicationFactor" Core..= replicationFactor),
            Core.Just ("IamRoleArn" Core..= iamRoleArn),
            ("AvailabilityZones" Core..=) Core.<$> availabilityZones,
            ("Description" Core..=) Core.<$> description,
            ("NotificationTopicArn" Core..=) Core.<$> notificationTopicArn,
            ("ParameterGroupName" Core..=) Core.<$> parameterGroupName,
            ("PreferredMaintenanceWindow" Core..=)
              Core.<$> preferredMaintenanceWindow,
            ("SSESpecification" Core..=) Core.<$> sSESpecification,
            ("SecurityGroupIds" Core..=) Core.<$> securityGroupIds,
            ("SubnetGroupName" Core..=) Core.<$> subnetGroupName,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateCluster where
  type Rs CreateCluster = CreateClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.CreateCluster")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Core.<$> (x Core..:? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | A description of the DAX cluster that you have created.
    cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateClusterResponse' value with any optional fields omitted.
mkCreateClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateClusterResponse
mkCreateClusterResponse responseStatus =
  CreateClusterResponse' {cluster = Core.Nothing, responseStatus}

-- | A description of the DAX cluster that you have created.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCluster :: Lens.Lens' CreateClusterResponse (Core.Maybe Types.Cluster)
ccrrsCluster = Lens.field @"cluster"
{-# DEPRECATED ccrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateClusterResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
