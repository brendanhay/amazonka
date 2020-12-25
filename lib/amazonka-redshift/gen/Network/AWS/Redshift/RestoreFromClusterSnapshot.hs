{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RestoreFromClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cluster from a snapshot. By default, Amazon Redshift creates the resulting cluster with the same configuration as the original cluster from which the snapshot was created, except that the new cluster is created with the default cluster security and parameter groups. After Amazon Redshift creates the cluster, you can use the 'ModifyCluster' API to associate a different security group and different parameter group with the restored cluster. If you are using a DS node type, you can also choose to change to another DS node type of the same size during restore.
--
-- If you restore a cluster into a VPC, you must provide a cluster subnet group where you want the cluster restored.
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.RestoreFromClusterSnapshot
  ( -- * Creating a request
    RestoreFromClusterSnapshot (..),
    mkRestoreFromClusterSnapshot,

    -- ** Request lenses
    rfcsClusterIdentifier,
    rfcsSnapshotIdentifier,
    rfcsAdditionalInfo,
    rfcsAllowVersionUpgrade,
    rfcsAutomatedSnapshotRetentionPeriod,
    rfcsAvailabilityZone,
    rfcsClusterParameterGroupName,
    rfcsClusterSecurityGroups,
    rfcsClusterSubnetGroupName,
    rfcsElasticIp,
    rfcsEnhancedVpcRouting,
    rfcsHsmClientCertificateIdentifier,
    rfcsHsmConfigurationIdentifier,
    rfcsIamRoles,
    rfcsKmsKeyId,
    rfcsMaintenanceTrackName,
    rfcsManualSnapshotRetentionPeriod,
    rfcsNodeType,
    rfcsNumberOfNodes,
    rfcsOwnerAccount,
    rfcsPort,
    rfcsPreferredMaintenanceWindow,
    rfcsPubliclyAccessible,
    rfcsSnapshotClusterIdentifier,
    rfcsSnapshotScheduleIdentifier,
    rfcsVpcSecurityGroupIds,

    -- * Destructuring the response
    RestoreFromClusterSnapshotResponse (..),
    mkRestoreFromClusterSnapshotResponse,

    -- ** Response lenses
    rfcsrrsCluster,
    rfcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkRestoreFromClusterSnapshot' smart constructor.
data RestoreFromClusterSnapshot = RestoreFromClusterSnapshot'
  { -- | The identifier of the cluster that will be created from restoring the snapshot.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 alphanumeric characters or hyphens.
    --
    --
    --     * Alphabetic characters must be lowercase.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens.
    --
    --
    --     * Must be unique for all clusters within an AWS account.
    clusterIdentifier :: Types.String,
    -- | The name of the snapshot from which to create the new cluster. This parameter isn't case sensitive.
    --
    -- Example: @my-snapshot-id@
    snapshotIdentifier :: Types.String,
    -- | Reserved.
    additionalInfo :: Core.Maybe Types.String,
    -- | If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster.
    --
    -- Default: @true@
    allowVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
    --
    -- Default: The value selected for the cluster from which the snapshot was taken.
    -- Constraints: Must be a value from 0 to 35.
    automatedSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The Amazon EC2 Availability Zone in which to restore the cluster.
    --
    -- Default: A random, system-chosen Availability Zone.
    -- Example: @us-east-2a@
    availabilityZone :: Core.Maybe Types.String,
    -- | The name of the parameter group to be associated with this cluster.
    --
    -- Default: The default Amazon Redshift cluster parameter group. For information about the default parameter group, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups> .
    -- Constraints:
    --
    --     * Must be 1 to 255 alphanumeric characters or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens.
    clusterParameterGroupName :: Core.Maybe Types.String,
    -- | A list of security groups to be associated with this cluster.
    --
    -- Default: The default cluster security group for Amazon Redshift.
    -- Cluster security groups only apply to clusters outside of VPCs.
    clusterSecurityGroups :: Core.Maybe [Types.String],
    -- | The name of the subnet group where you want to cluster restored.
    --
    -- A snapshot of cluster in VPC can be restored only in VPC. Therefore, you must provide subnet group name where you want the cluster restored.
    clusterSubnetGroupName :: Core.Maybe Types.String,
    -- | The elastic IP (EIP) address for the cluster.
    elasticIp :: Core.Maybe Types.String,
    -- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@ , enhanced VPC routing is enabled.
    -- Default: false
    enhancedVpcRouting :: Core.Maybe Core.Bool,
    -- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
    hsmClientCertificateIdentifier :: Core.Maybe Types.String,
    -- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
    hsmConfigurationIdentifier :: Core.Maybe Types.String,
    -- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
    --
    -- A cluster can have up to 10 IAM roles associated at any time.
    iamRoles :: Core.Maybe [Types.String],
    -- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster that you restore from a shared snapshot.
    kmsKeyId :: Core.Maybe Types.String,
    -- | The name of the maintenance track for the restored cluster. When you take a snapshot, the snapshot inherits the @MaintenanceTrack@ value from the cluster. The snapshot might be on a different track than the cluster that was the source for the snapshot. For example, suppose that you take a snapshot of a cluster that is on the current track and then change the cluster to be on the trailing track. In this case, the snapshot and the source cluster are on different tracks.
    maintenanceTrackName :: Core.Maybe Types.String,
    -- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The node type that the restored cluster will be provisioned with.
    --
    -- Default: The node type of the cluster from which the snapshot was taken. You can modify this if you are using any DS node type. In that case, you can choose to restore into another DS node type of the same size. For example, you can restore ds1.8xlarge into ds2.8xlarge, or ds1.xlarge into ds2.xlarge. If you have a DC instance type, you must restore into that same instance type and size. In other words, you can only restore a dc1.large instance type into another dc1.large instance type or dc2.large instance type. You can't restore dc1.8xlarge to dc2.8xlarge. First restore to a dc1.8xlarge cluster, then resize to a dc2.8large cluster. For more information about node types, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-about-clusters-and-nodes About Clusters and Nodes> in the /Amazon Redshift Cluster Management Guide/ .
    nodeType :: Core.Maybe Types.String,
    -- | The number of nodes specified when provisioning the restored cluster.
    numberOfNodes :: Core.Maybe Core.Int,
    -- | The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
    ownerAccount :: Core.Maybe Types.String,
    -- | The port number on which the cluster accepts connections.
    --
    -- Default: The same port as the original cluster.
    -- Constraints: Must be between @1115@ and @65535@ .
    port :: Core.Maybe Core.Int,
    -- | The weekly time range (in UTC) during which automated cluster maintenance can occur.
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    -- Default: The value selected for the cluster from which the snapshot was taken. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide.
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Core.Maybe Types.String,
    -- | If @true@ , the cluster can be accessed from a public network.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | The name of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
    snapshotClusterIdentifier :: Core.Maybe Types.String,
    -- | A unique identifier for the snapshot schedule.
    snapshotScheduleIdentifier :: Core.Maybe Types.String,
    -- | A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
    --
    -- Default: The default VPC security group is associated with the cluster.
    -- VPC security groups only apply to clusters in VPCs.
    vpcSecurityGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreFromClusterSnapshot' value with any optional fields omitted.
mkRestoreFromClusterSnapshot ::
  -- | 'clusterIdentifier'
  Types.String ->
  -- | 'snapshotIdentifier'
  Types.String ->
  RestoreFromClusterSnapshot
mkRestoreFromClusterSnapshot clusterIdentifier snapshotIdentifier =
  RestoreFromClusterSnapshot'
    { clusterIdentifier,
      snapshotIdentifier,
      additionalInfo = Core.Nothing,
      allowVersionUpgrade = Core.Nothing,
      automatedSnapshotRetentionPeriod = Core.Nothing,
      availabilityZone = Core.Nothing,
      clusterParameterGroupName = Core.Nothing,
      clusterSecurityGroups = Core.Nothing,
      clusterSubnetGroupName = Core.Nothing,
      elasticIp = Core.Nothing,
      enhancedVpcRouting = Core.Nothing,
      hsmClientCertificateIdentifier = Core.Nothing,
      hsmConfigurationIdentifier = Core.Nothing,
      iamRoles = Core.Nothing,
      kmsKeyId = Core.Nothing,
      maintenanceTrackName = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing,
      nodeType = Core.Nothing,
      numberOfNodes = Core.Nothing,
      ownerAccount = Core.Nothing,
      port = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      snapshotClusterIdentifier = Core.Nothing,
      snapshotScheduleIdentifier = Core.Nothing,
      vpcSecurityGroupIds = Core.Nothing
    }

-- | The identifier of the cluster that will be created from restoring the snapshot.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * Alphabetic characters must be lowercase.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for all clusters within an AWS account.
--
--
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsClusterIdentifier :: Lens.Lens' RestoreFromClusterSnapshot Types.String
rfcsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED rfcsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The name of the snapshot from which to create the new cluster. This parameter isn't case sensitive.
--
-- Example: @my-snapshot-id@
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsSnapshotIdentifier :: Lens.Lens' RestoreFromClusterSnapshot Types.String
rfcsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED rfcsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAdditionalInfo :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsAdditionalInfo = Lens.field @"additionalInfo"
{-# DEPRECATED rfcsAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster.
--
-- Default: @true@
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAllowVersionUpgrade :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Bool)
rfcsAllowVersionUpgrade = Lens.field @"allowVersionUpgrade"
{-# DEPRECATED rfcsAllowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead." #-}

-- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
--
-- Default: The value selected for the cluster from which the snapshot was taken.
-- Constraints: Must be a value from 0 to 35.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAutomatedSnapshotRetentionPeriod :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Int)
rfcsAutomatedSnapshotRetentionPeriod = Lens.field @"automatedSnapshotRetentionPeriod"
{-# DEPRECATED rfcsAutomatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead." #-}

-- | The Amazon EC2 Availability Zone in which to restore the cluster.
--
-- Default: A random, system-chosen Availability Zone.
-- Example: @us-east-2a@
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAvailabilityZone :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED rfcsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The name of the parameter group to be associated with this cluster.
--
-- Default: The default Amazon Redshift cluster parameter group. For information about the default parameter group, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups> .
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'clusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsClusterParameterGroupName :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsClusterParameterGroupName = Lens.field @"clusterParameterGroupName"
{-# DEPRECATED rfcsClusterParameterGroupName "Use generic-lens or generic-optics with 'clusterParameterGroupName' instead." #-}

-- | A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
-- Cluster security groups only apply to clusters outside of VPCs.
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsClusterSecurityGroups :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe [Types.String])
rfcsClusterSecurityGroups = Lens.field @"clusterSecurityGroups"
{-# DEPRECATED rfcsClusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead." #-}

-- | The name of the subnet group where you want to cluster restored.
--
-- A snapshot of cluster in VPC can be restored only in VPC. Therefore, you must provide subnet group name where you want the cluster restored.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsClusterSubnetGroupName :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# DEPRECATED rfcsClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | The elastic IP (EIP) address for the cluster.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsElasticIp :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsElasticIp = Lens.field @"elasticIp"
{-# DEPRECATED rfcsElasticIp "Use generic-lens or generic-optics with 'elasticIp' instead." #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
--
-- /Note:/ Consider using 'enhancedVpcRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsEnhancedVpcRouting :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Bool)
rfcsEnhancedVpcRouting = Lens.field @"enhancedVpcRouting"
{-# DEPRECATED rfcsEnhancedVpcRouting "Use generic-lens or generic-optics with 'enhancedVpcRouting' instead." #-}

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsHsmClientCertificateIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsHsmClientCertificateIdentifier = Lens.field @"hsmClientCertificateIdentifier"
{-# DEPRECATED rfcsHsmClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead." #-}

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsHsmConfigurationIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsHsmConfigurationIdentifier = Lens.field @"hsmConfigurationIdentifier"
{-# DEPRECATED rfcsHsmConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead." #-}

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
--
-- A cluster can have up to 10 IAM roles associated at any time.
--
-- /Note:/ Consider using 'iamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsIamRoles :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe [Types.String])
rfcsIamRoles = Lens.field @"iamRoles"
{-# DEPRECATED rfcsIamRoles "Use generic-lens or generic-optics with 'iamRoles' instead." #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster that you restore from a shared snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsKmsKeyId :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED rfcsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the maintenance track for the restored cluster. When you take a snapshot, the snapshot inherits the @MaintenanceTrack@ value from the cluster. The snapshot might be on a different track than the cluster that was the source for the snapshot. For example, suppose that you take a snapshot of a cluster that is on the current track and then change the cluster to be on the trailing track. In this case, the snapshot and the source cluster are on different tracks.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsMaintenanceTrackName :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# DEPRECATED rfcsMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsManualSnapshotRetentionPeriod :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Int)
rfcsManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# DEPRECATED rfcsManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | The node type that the restored cluster will be provisioned with.
--
-- Default: The node type of the cluster from which the snapshot was taken. You can modify this if you are using any DS node type. In that case, you can choose to restore into another DS node type of the same size. For example, you can restore ds1.8xlarge into ds2.8xlarge, or ds1.xlarge into ds2.xlarge. If you have a DC instance type, you must restore into that same instance type and size. In other words, you can only restore a dc1.large instance type into another dc1.large instance type or dc2.large instance type. You can't restore dc1.8xlarge to dc2.8xlarge. First restore to a dc1.8xlarge cluster, then resize to a dc2.8large cluster. For more information about node types, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-about-clusters-and-nodes About Clusters and Nodes> in the /Amazon Redshift Cluster Management Guide/ .
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsNodeType :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsNodeType = Lens.field @"nodeType"
{-# DEPRECATED rfcsNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The number of nodes specified when provisioning the restored cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsNumberOfNodes :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Int)
rfcsNumberOfNodes = Lens.field @"numberOfNodes"
{-# DEPRECATED rfcsNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsOwnerAccount :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED rfcsOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The port number on which the cluster accepts connections.
--
-- Default: The same port as the original cluster.
-- Constraints: Must be between @1115@ and @65535@ .
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsPort :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Int)
rfcsPort = Lens.field @"port"
{-# DEPRECATED rfcsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The weekly time range (in UTC) during which automated cluster maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- Default: The value selected for the cluster from which the snapshot was taken. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide.
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsPreferredMaintenanceWindow :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED rfcsPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | If @true@ , the cluster can be accessed from a public network.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsPubliclyAccessible :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Bool)
rfcsPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# DEPRECATED rfcsPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | The name of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsSnapshotClusterIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsSnapshotClusterIdentifier = Lens.field @"snapshotClusterIdentifier"
{-# DEPRECATED rfcsSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}

-- | A unique identifier for the snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsSnapshotScheduleIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Types.String)
rfcsSnapshotScheduleIdentifier = Lens.field @"snapshotScheduleIdentifier"
{-# DEPRECATED rfcsSnapshotScheduleIdentifier "Use generic-lens or generic-optics with 'snapshotScheduleIdentifier' instead." #-}

-- | A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
-- VPC security groups only apply to clusters in VPCs.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsVpcSecurityGroupIds :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe [Types.String])
rfcsVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED rfcsVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

instance Core.AWSRequest RestoreFromClusterSnapshot where
  type
    Rs RestoreFromClusterSnapshot =
      RestoreFromClusterSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RestoreFromClusterSnapshot")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> (Core.toQueryValue "SnapshotIdentifier" snapshotIdentifier)
                Core.<> (Core.toQueryValue "AdditionalInfo" Core.<$> additionalInfo)
                Core.<> ( Core.toQueryValue "AllowVersionUpgrade"
                            Core.<$> allowVersionUpgrade
                        )
                Core.<> ( Core.toQueryValue "AutomatedSnapshotRetentionPeriod"
                            Core.<$> automatedSnapshotRetentionPeriod
                        )
                Core.<> (Core.toQueryValue "AvailabilityZone" Core.<$> availabilityZone)
                Core.<> ( Core.toQueryValue "ClusterParameterGroupName"
                            Core.<$> clusterParameterGroupName
                        )
                Core.<> ( Core.toQueryValue
                            "ClusterSecurityGroups"
                            ( Core.toQueryList "ClusterSecurityGroupName"
                                Core.<$> clusterSecurityGroups
                            )
                        )
                Core.<> ( Core.toQueryValue "ClusterSubnetGroupName"
                            Core.<$> clusterSubnetGroupName
                        )
                Core.<> (Core.toQueryValue "ElasticIp" Core.<$> elasticIp)
                Core.<> ( Core.toQueryValue "EnhancedVpcRouting"
                            Core.<$> enhancedVpcRouting
                        )
                Core.<> ( Core.toQueryValue "HsmClientCertificateIdentifier"
                            Core.<$> hsmClientCertificateIdentifier
                        )
                Core.<> ( Core.toQueryValue "HsmConfigurationIdentifier"
                            Core.<$> hsmConfigurationIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "IamRoles"
                            (Core.toQueryList "IamRoleArn" Core.<$> iamRoles)
                        )
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> ( Core.toQueryValue "MaintenanceTrackName"
                            Core.<$> maintenanceTrackName
                        )
                Core.<> ( Core.toQueryValue "ManualSnapshotRetentionPeriod"
                            Core.<$> manualSnapshotRetentionPeriod
                        )
                Core.<> (Core.toQueryValue "NodeType" Core.<$> nodeType)
                Core.<> (Core.toQueryValue "NumberOfNodes" Core.<$> numberOfNodes)
                Core.<> (Core.toQueryValue "OwnerAccount" Core.<$> ownerAccount)
                Core.<> (Core.toQueryValue "Port" Core.<$> port)
                Core.<> ( Core.toQueryValue "PreferredMaintenanceWindow"
                            Core.<$> preferredMaintenanceWindow
                        )
                Core.<> ( Core.toQueryValue "PubliclyAccessible"
                            Core.<$> publiclyAccessible
                        )
                Core.<> ( Core.toQueryValue "SnapshotClusterIdentifier"
                            Core.<$> snapshotClusterIdentifier
                        )
                Core.<> ( Core.toQueryValue "SnapshotScheduleIdentifier"
                            Core.<$> snapshotScheduleIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "VpcSecurityGroupIds"
                            ( Core.toQueryList "VpcSecurityGroupId"
                                Core.<$> vpcSecurityGroupIds
                            )
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "RestoreFromClusterSnapshotResult"
      ( \s h x ->
          RestoreFromClusterSnapshotResponse'
            Core.<$> (x Core..@? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreFromClusterSnapshotResponse' smart constructor.
data RestoreFromClusterSnapshotResponse = RestoreFromClusterSnapshotResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RestoreFromClusterSnapshotResponse' value with any optional fields omitted.
mkRestoreFromClusterSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreFromClusterSnapshotResponse
mkRestoreFromClusterSnapshotResponse responseStatus =
  RestoreFromClusterSnapshotResponse'
    { cluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsrrsCluster :: Lens.Lens' RestoreFromClusterSnapshotResponse (Core.Maybe Types.Cluster)
rfcsrrsCluster = Lens.field @"cluster"
{-# DEPRECATED rfcsrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsrrsResponseStatus :: Lens.Lens' RestoreFromClusterSnapshotResponse Core.Int
rfcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rfcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
