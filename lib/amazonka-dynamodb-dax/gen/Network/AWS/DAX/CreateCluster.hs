{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ccSecurityGroupIds,
    ccSSESpecification,
    ccSubnetGroupName,
    ccPreferredMaintenanceWindow,
    ccAvailabilityZones,
    ccDescription,
    ccNotificationTopicARN,
    ccTags,
    ccParameterGroupName,
    ccClusterName,
    ccNodeType,
    ccReplicationFactor,
    ccIAMRoleARN,

    -- * Destructuring the response
    CreateClusterResponse (..),
    mkCreateClusterResponse,

    -- ** Response lenses
    ccrsCluster,
    ccrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    sSESpecification :: Lude.Maybe SSESpecification,
    subnetGroupName :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    availabilityZones :: Lude.Maybe [Lude.Text],
    description :: Lude.Maybe Lude.Text,
    notificationTopicARN :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    parameterGroupName :: Lude.Maybe Lude.Text,
    clusterName :: Lude.Text,
    nodeType :: Lude.Text,
    replicationFactor :: Lude.Int,
    iamRoleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - The Availability Zones (AZs) in which the cluster nodes will reside after the cluster has been created or updated. If provided, the length of this list must equal the @ReplicationFactor@ parameter. If you omit this parameter, DAX will spread the nodes across Availability Zones for the highest availability.
-- * 'clusterName' - The cluster identifier. This parameter is stored as a lowercase string.
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
-- * 'description' - A description of the cluster.
-- * 'iamRoleARN' - A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
-- * 'nodeType' - The compute and memory capacity of the nodes in the cluster.
-- * 'notificationTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications will be sent.
-- * 'parameterGroupName' - The parameter group to be associated with the DAX cluster.
-- * 'preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the DAX cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:
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
-- * 'replicationFactor' - The number of nodes in the DAX cluster. A replication factor of 1 will create a single-node cluster, without any read replicas. For additional fault tolerance, you can create a multiple node cluster with one or more read replicas. To do this, set @ReplicationFactor@ to a number between 3 (one primary and two read replicas) and 10 (one primary and nine read replicas). @If the AvailabilityZones@ parameter is provided, its length must equal the @ReplicationFactor@ .
-- * 'sSESpecification' - Represents the settings used to enable server-side encryption on the cluster.
-- * 'securityGroupIds' - A list of security group IDs to be assigned to each node in the DAX cluster. (Each of the security group ID is system-generated.)
--
-- If this parameter is not specified, DAX assigns the default VPC security group to each node.
-- * 'subnetGroupName' - The name of the subnet group to be used for the replication group.
--
-- /Important:/ DAX clusters can only run in an Amazon VPC environment. All of the subnets that you specify in a subnet group must exist in the same VPC.
-- * 'tags' - A set of tags to associate with the DAX cluster.
mkCreateCluster ::
  -- | 'clusterName'
  Lude.Text ->
  -- | 'nodeType'
  Lude.Text ->
  -- | 'replicationFactor'
  Lude.Int ->
  -- | 'iamRoleARN'
  Lude.Text ->
  CreateCluster
mkCreateCluster
  pClusterName_
  pNodeType_
  pReplicationFactor_
  pIAMRoleARN_ =
    CreateCluster'
      { securityGroupIds = Lude.Nothing,
        sSESpecification = Lude.Nothing,
        subnetGroupName = Lude.Nothing,
        preferredMaintenanceWindow = Lude.Nothing,
        availabilityZones = Lude.Nothing,
        description = Lude.Nothing,
        notificationTopicARN = Lude.Nothing,
        tags = Lude.Nothing,
        parameterGroupName = Lude.Nothing,
        clusterName = pClusterName_,
        nodeType = pNodeType_,
        replicationFactor = pReplicationFactor_,
        iamRoleARN = pIAMRoleARN_
      }

-- | A list of security group IDs to be assigned to each node in the DAX cluster. (Each of the security group ID is system-generated.)
--
-- If this parameter is not specified, DAX assigns the default VPC security group to each node.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSecurityGroupIds :: Lens.Lens' CreateCluster (Lude.Maybe [Lude.Text])
ccSecurityGroupIds = Lens.lens (securityGroupIds :: CreateCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: CreateCluster)
{-# DEPRECATED ccSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | Represents the settings used to enable server-side encryption on the cluster.
--
-- /Note:/ Consider using 'sSESpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSSESpecification :: Lens.Lens' CreateCluster (Lude.Maybe SSESpecification)
ccSSESpecification = Lens.lens (sSESpecification :: CreateCluster -> Lude.Maybe SSESpecification) (\s a -> s {sSESpecification = a} :: CreateCluster)
{-# DEPRECATED ccSSESpecification "Use generic-lens or generic-optics with 'sSESpecification' instead." #-}

-- | The name of the subnet group to be used for the replication group.
--
-- /Important:/ DAX clusters can only run in an Amazon VPC environment. All of the subnets that you specify in a subnet group must exist in the same VPC.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSubnetGroupName :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccSubnetGroupName = Lens.lens (subnetGroupName :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {subnetGroupName = a} :: CreateCluster)
{-# DEPRECATED ccSubnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead." #-}

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
ccPreferredMaintenanceWindow :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: CreateCluster)
{-# DEPRECATED ccPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The Availability Zones (AZs) in which the cluster nodes will reside after the cluster has been created or updated. If provided, the length of this list must equal the @ReplicationFactor@ parameter. If you omit this parameter, DAX will spread the nodes across Availability Zones for the highest availability.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAvailabilityZones :: Lens.Lens' CreateCluster (Lude.Maybe [Lude.Text])
ccAvailabilityZones = Lens.lens (availabilityZones :: CreateCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: CreateCluster)
{-# DEPRECATED ccAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A description of the cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccDescription = Lens.lens (description :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateCluster)
{-# DEPRECATED ccDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications will be sent.
--
-- /Note:/ Consider using 'notificationTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNotificationTopicARN :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccNotificationTopicARN = Lens.lens (notificationTopicARN :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {notificationTopicARN = a} :: CreateCluster)
{-# DEPRECATED ccNotificationTopicARN "Use generic-lens or generic-optics with 'notificationTopicARN' instead." #-}

-- | A set of tags to associate with the DAX cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateCluster (Lude.Maybe [Tag])
ccTags = Lens.lens (tags :: CreateCluster -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCluster)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The parameter group to be associated with the DAX cluster.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccParameterGroupName :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccParameterGroupName = Lens.lens (parameterGroupName :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupName = a} :: CreateCluster)
{-# DEPRECATED ccParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

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
ccClusterName :: Lens.Lens' CreateCluster Lude.Text
ccClusterName = Lens.lens (clusterName :: CreateCluster -> Lude.Text) (\s a -> s {clusterName = a} :: CreateCluster)
{-# DEPRECATED ccClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The compute and memory capacity of the nodes in the cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNodeType :: Lens.Lens' CreateCluster Lude.Text
ccNodeType = Lens.lens (nodeType :: CreateCluster -> Lude.Text) (\s a -> s {nodeType = a} :: CreateCluster)
{-# DEPRECATED ccNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The number of nodes in the DAX cluster. A replication factor of 1 will create a single-node cluster, without any read replicas. For additional fault tolerance, you can create a multiple node cluster with one or more read replicas. To do this, set @ReplicationFactor@ to a number between 3 (one primary and two read replicas) and 10 (one primary and nine read replicas). @If the AvailabilityZones@ parameter is provided, its length must equal the @ReplicationFactor@ .
--
-- /Note:/ Consider using 'replicationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccReplicationFactor :: Lens.Lens' CreateCluster Lude.Int
ccReplicationFactor = Lens.lens (replicationFactor :: CreateCluster -> Lude.Int) (\s a -> s {replicationFactor = a} :: CreateCluster)
{-# DEPRECATED ccReplicationFactor "Use generic-lens or generic-optics with 'replicationFactor' instead." #-}

-- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccIAMRoleARN :: Lens.Lens' CreateCluster Lude.Text
ccIAMRoleARN = Lens.lens (iamRoleARN :: CreateCluster -> Lude.Text) (\s a -> s {iamRoleARN = a} :: CreateCluster)
{-# DEPRECATED ccIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

instance Lude.AWSRequest CreateCluster where
  type Rs CreateCluster = CreateClusterResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Lude.<$> (x Lude..?> "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.CreateCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecurityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            ("SSESpecification" Lude..=) Lude.<$> sSESpecification,
            ("SubnetGroupName" Lude..=) Lude.<$> subnetGroupName,
            ("PreferredMaintenanceWindow" Lude..=)
              Lude.<$> preferredMaintenanceWindow,
            ("AvailabilityZones" Lude..=) Lude.<$> availabilityZones,
            ("Description" Lude..=) Lude.<$> description,
            ("NotificationTopicArn" Lude..=) Lude.<$> notificationTopicARN,
            ("Tags" Lude..=) Lude.<$> tags,
            ("ParameterGroupName" Lude..=) Lude.<$> parameterGroupName,
            Lude.Just ("ClusterName" Lude..= clusterName),
            Lude.Just ("NodeType" Lude..= nodeType),
            Lude.Just ("ReplicationFactor" Lude..= replicationFactor),
            Lude.Just ("IamRoleArn" Lude..= iamRoleARN)
          ]
      )

instance Lude.ToPath CreateCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
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

-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - A description of the DAX cluster that you have created.
-- * 'responseStatus' - The response status code.
mkCreateClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClusterResponse
mkCreateClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A description of the DAX cluster that you have created.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsCluster :: Lens.Lens' CreateClusterResponse (Lude.Maybe Cluster)
ccrsCluster = Lens.lens (cluster :: CreateClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: CreateClusterResponse)
{-# DEPRECATED ccrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateClusterResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClusterResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
