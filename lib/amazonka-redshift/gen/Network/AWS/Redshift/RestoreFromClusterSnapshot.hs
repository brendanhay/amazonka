{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RestoreFromClusterSnapshot (..)
    , mkRestoreFromClusterSnapshot
    -- ** Request lenses
    , rfcsClusterIdentifier
    , rfcsSnapshotIdentifier
    , rfcsAdditionalInfo
    , rfcsAllowVersionUpgrade
    , rfcsAutomatedSnapshotRetentionPeriod
    , rfcsAvailabilityZone
    , rfcsClusterParameterGroupName
    , rfcsClusterSecurityGroups
    , rfcsClusterSubnetGroupName
    , rfcsElasticIp
    , rfcsEnhancedVpcRouting
    , rfcsHsmClientCertificateIdentifier
    , rfcsHsmConfigurationIdentifier
    , rfcsIamRoles
    , rfcsKmsKeyId
    , rfcsMaintenanceTrackName
    , rfcsManualSnapshotRetentionPeriod
    , rfcsNodeType
    , rfcsNumberOfNodes
    , rfcsOwnerAccount
    , rfcsPort
    , rfcsPreferredMaintenanceWindow
    , rfcsPubliclyAccessible
    , rfcsSnapshotClusterIdentifier
    , rfcsSnapshotScheduleIdentifier
    , rfcsVpcSecurityGroupIds

    -- * Destructuring the response
    , RestoreFromClusterSnapshotResponse (..)
    , mkRestoreFromClusterSnapshotResponse
    -- ** Response lenses
    , rfcsrrsCluster
    , rfcsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRestoreFromClusterSnapshot' smart constructor.
data RestoreFromClusterSnapshot = RestoreFromClusterSnapshot'
  { clusterIdentifier :: Core.Text
    -- ^ The identifier of the cluster that will be created from restoring the snapshot.
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
  , snapshotIdentifier :: Core.Text
    -- ^ The name of the snapshot from which to create the new cluster. This parameter isn't case sensitive.
--
-- Example: @my-snapshot-id@ 
  , additionalInfo :: Core.Maybe Core.Text
    -- ^ Reserved.
  , allowVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster. 
--
-- Default: @true@ 
  , automatedSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' . 
--
-- Default: The value selected for the cluster from which the snapshot was taken.
-- Constraints: Must be a value from 0 to 35.
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Amazon EC2 Availability Zone in which to restore the cluster.
--
-- Default: A random, system-chosen Availability Zone.
-- Example: @us-east-2a@ 
  , clusterParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the parameter group to be associated with this cluster.
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
  , clusterSecurityGroups :: Core.Maybe [Core.Text]
    -- ^ A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
-- Cluster security groups only apply to clusters outside of VPCs.
  , clusterSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The name of the subnet group where you want to cluster restored.
--
-- A snapshot of cluster in VPC can be restored only in VPC. Therefore, you must provide subnet group name where you want the cluster restored.
  , elasticIp :: Core.Maybe Core.Text
    -- ^ The elastic IP (EIP) address for the cluster.
  , enhancedVpcRouting :: Core.Maybe Core.Bool
    -- ^ An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
  , hsmClientCertificateIdentifier :: Core.Maybe Core.Text
    -- ^ Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
  , hsmConfigurationIdentifier :: Core.Maybe Core.Text
    -- ^ Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
  , iamRoles :: Core.Maybe [Core.Text]
    -- ^ A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
--
-- A cluster can have up to 10 IAM roles associated at any time.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster that you restore from a shared snapshot.
  , maintenanceTrackName :: Core.Maybe Core.Text
    -- ^ The name of the maintenance track for the restored cluster. When you take a snapshot, the snapshot inherits the @MaintenanceTrack@ value from the cluster. The snapshot might be on a different track than the cluster that was the source for the snapshot. For example, suppose that you take a snapshot of a cluster that is on the current track and then change the cluster to be on the trailing track. In this case, the snapshot and the source cluster are on different tracks.
  , manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The node type that the restored cluster will be provisioned with.
--
-- Default: The node type of the cluster from which the snapshot was taken. You can modify this if you are using any DS node type. In that case, you can choose to restore into another DS node type of the same size. For example, you can restore ds1.8xlarge into ds2.8xlarge, or ds1.xlarge into ds2.xlarge. If you have a DC instance type, you must restore into that same instance type and size. In other words, you can only restore a dc1.large instance type into another dc1.large instance type or dc2.large instance type. You can't restore dc1.8xlarge to dc2.8xlarge. First restore to a dc1.8xlarge cluster, then resize to a dc2.8large cluster. For more information about node types, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-about-clusters-and-nodes About Clusters and Nodes> in the /Amazon Redshift Cluster Management Guide/ . 
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The number of nodes specified when provisioning the restored cluster.
  , ownerAccount :: Core.Maybe Core.Text
    -- ^ The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
  , port :: Core.Maybe Core.Int
    -- ^ The port number on which the cluster accepts connections.
--
-- Default: The same port as the original cluster.
-- Constraints: Must be between @1115@ and @65535@ .
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ The weekly time range (in UTC) during which automated cluster maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@ 
-- Default: The value selected for the cluster from which the snapshot was taken. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide. 
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Minimum 30-minute window.
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ If @true@ , the cluster can be accessed from a public network. 
  , snapshotClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The name of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
  , snapshotScheduleIdentifier :: Core.Maybe Core.Text
    -- ^ A unique identifier for the snapshot schedule.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
-- VPC security groups only apply to clusters in VPCs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreFromClusterSnapshot' value with any optional fields omitted.
mkRestoreFromClusterSnapshot
    :: Core.Text -- ^ 'clusterIdentifier'
    -> Core.Text -- ^ 'snapshotIdentifier'
    -> RestoreFromClusterSnapshot
mkRestoreFromClusterSnapshot clusterIdentifier snapshotIdentifier
  = RestoreFromClusterSnapshot'{clusterIdentifier,
                                snapshotIdentifier, additionalInfo = Core.Nothing,
                                allowVersionUpgrade = Core.Nothing,
                                automatedSnapshotRetentionPeriod = Core.Nothing,
                                availabilityZone = Core.Nothing,
                                clusterParameterGroupName = Core.Nothing,
                                clusterSecurityGroups = Core.Nothing,
                                clusterSubnetGroupName = Core.Nothing, elasticIp = Core.Nothing,
                                enhancedVpcRouting = Core.Nothing,
                                hsmClientCertificateIdentifier = Core.Nothing,
                                hsmConfigurationIdentifier = Core.Nothing, iamRoles = Core.Nothing,
                                kmsKeyId = Core.Nothing, maintenanceTrackName = Core.Nothing,
                                manualSnapshotRetentionPeriod = Core.Nothing,
                                nodeType = Core.Nothing, numberOfNodes = Core.Nothing,
                                ownerAccount = Core.Nothing, port = Core.Nothing,
                                preferredMaintenanceWindow = Core.Nothing,
                                publiclyAccessible = Core.Nothing,
                                snapshotClusterIdentifier = Core.Nothing,
                                snapshotScheduleIdentifier = Core.Nothing,
                                vpcSecurityGroupIds = Core.Nothing}

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
rfcsClusterIdentifier :: Lens.Lens' RestoreFromClusterSnapshot Core.Text
rfcsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE rfcsClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | The name of the snapshot from which to create the new cluster. This parameter isn't case sensitive.
--
-- Example: @my-snapshot-id@ 
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsSnapshotIdentifier :: Lens.Lens' RestoreFromClusterSnapshot Core.Text
rfcsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE rfcsSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAdditionalInfo :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsAdditionalInfo = Lens.field @"additionalInfo"
{-# INLINEABLE rfcsAdditionalInfo #-}
{-# DEPRECATED additionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead"  #-}

-- | If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster. 
--
-- Default: @true@ 
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAllowVersionUpgrade :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Bool)
rfcsAllowVersionUpgrade = Lens.field @"allowVersionUpgrade"
{-# INLINEABLE rfcsAllowVersionUpgrade #-}
{-# DEPRECATED allowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead"  #-}

-- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' . 
--
-- Default: The value selected for the cluster from which the snapshot was taken.
-- Constraints: Must be a value from 0 to 35.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAutomatedSnapshotRetentionPeriod :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Int)
rfcsAutomatedSnapshotRetentionPeriod = Lens.field @"automatedSnapshotRetentionPeriod"
{-# INLINEABLE rfcsAutomatedSnapshotRetentionPeriod #-}
{-# DEPRECATED automatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead"  #-}

-- | The Amazon EC2 Availability Zone in which to restore the cluster.
--
-- Default: A random, system-chosen Availability Zone.
-- Example: @us-east-2a@ 
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAvailabilityZone :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE rfcsAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

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
rfcsClusterParameterGroupName :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsClusterParameterGroupName = Lens.field @"clusterParameterGroupName"
{-# INLINEABLE rfcsClusterParameterGroupName #-}
{-# DEPRECATED clusterParameterGroupName "Use generic-lens or generic-optics with 'clusterParameterGroupName' instead"  #-}

-- | A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
-- Cluster security groups only apply to clusters outside of VPCs.
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsClusterSecurityGroups :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe [Core.Text])
rfcsClusterSecurityGroups = Lens.field @"clusterSecurityGroups"
{-# INLINEABLE rfcsClusterSecurityGroups #-}
{-# DEPRECATED clusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead"  #-}

-- | The name of the subnet group where you want to cluster restored.
--
-- A snapshot of cluster in VPC can be restored only in VPC. Therefore, you must provide subnet group name where you want the cluster restored.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsClusterSubnetGroupName :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# INLINEABLE rfcsClusterSubnetGroupName #-}
{-# DEPRECATED clusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead"  #-}

-- | The elastic IP (EIP) address for the cluster.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsElasticIp :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsElasticIp = Lens.field @"elasticIp"
{-# INLINEABLE rfcsElasticIp #-}
{-# DEPRECATED elasticIp "Use generic-lens or generic-optics with 'elasticIp' instead"  #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
--
-- /Note:/ Consider using 'enhancedVpcRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsEnhancedVpcRouting :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Bool)
rfcsEnhancedVpcRouting = Lens.field @"enhancedVpcRouting"
{-# INLINEABLE rfcsEnhancedVpcRouting #-}
{-# DEPRECATED enhancedVpcRouting "Use generic-lens or generic-optics with 'enhancedVpcRouting' instead"  #-}

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsHsmClientCertificateIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsHsmClientCertificateIdentifier = Lens.field @"hsmClientCertificateIdentifier"
{-# INLINEABLE rfcsHsmClientCertificateIdentifier #-}
{-# DEPRECATED hsmClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead"  #-}

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsHsmConfigurationIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsHsmConfigurationIdentifier = Lens.field @"hsmConfigurationIdentifier"
{-# INLINEABLE rfcsHsmConfigurationIdentifier #-}
{-# DEPRECATED hsmConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead"  #-}

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
--
-- A cluster can have up to 10 IAM roles associated at any time.
--
-- /Note:/ Consider using 'iamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsIamRoles :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe [Core.Text])
rfcsIamRoles = Lens.field @"iamRoles"
{-# INLINEABLE rfcsIamRoles #-}
{-# DEPRECATED iamRoles "Use generic-lens or generic-optics with 'iamRoles' instead"  #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster that you restore from a shared snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsKmsKeyId :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE rfcsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The name of the maintenance track for the restored cluster. When you take a snapshot, the snapshot inherits the @MaintenanceTrack@ value from the cluster. The snapshot might be on a different track than the cluster that was the source for the snapshot. For example, suppose that you take a snapshot of a cluster that is on the current track and then change the cluster to be on the trailing track. In this case, the snapshot and the source cluster are on different tracks.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsMaintenanceTrackName :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# INLINEABLE rfcsMaintenanceTrackName #-}
{-# DEPRECATED maintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead"  #-}

-- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsManualSnapshotRetentionPeriod :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Int)
rfcsManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# INLINEABLE rfcsManualSnapshotRetentionPeriod #-}
{-# DEPRECATED manualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead"  #-}

-- | The node type that the restored cluster will be provisioned with.
--
-- Default: The node type of the cluster from which the snapshot was taken. You can modify this if you are using any DS node type. In that case, you can choose to restore into another DS node type of the same size. For example, you can restore ds1.8xlarge into ds2.8xlarge, or ds1.xlarge into ds2.xlarge. If you have a DC instance type, you must restore into that same instance type and size. In other words, you can only restore a dc1.large instance type into another dc1.large instance type or dc2.large instance type. You can't restore dc1.8xlarge to dc2.8xlarge. First restore to a dc1.8xlarge cluster, then resize to a dc2.8large cluster. For more information about node types, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-about-clusters-and-nodes About Clusters and Nodes> in the /Amazon Redshift Cluster Management Guide/ . 
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsNodeType :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsNodeType = Lens.field @"nodeType"
{-# INLINEABLE rfcsNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | The number of nodes specified when provisioning the restored cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsNumberOfNodes :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Int)
rfcsNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE rfcsNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

-- | The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsOwnerAccount :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsOwnerAccount = Lens.field @"ownerAccount"
{-# INLINEABLE rfcsOwnerAccount #-}
{-# DEPRECATED ownerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead"  #-}

-- | The port number on which the cluster accepts connections.
--
-- Default: The same port as the original cluster.
-- Constraints: Must be between @1115@ and @65535@ .
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsPort :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Int)
rfcsPort = Lens.field @"port"
{-# INLINEABLE rfcsPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The weekly time range (in UTC) during which automated cluster maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@ 
-- Default: The value selected for the cluster from which the snapshot was taken. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide. 
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsPreferredMaintenanceWindow :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE rfcsPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | If @true@ , the cluster can be accessed from a public network. 
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsPubliclyAccessible :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Bool)
rfcsPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE rfcsPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

-- | The name of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsSnapshotClusterIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsSnapshotClusterIdentifier = Lens.field @"snapshotClusterIdentifier"
{-# INLINEABLE rfcsSnapshotClusterIdentifier #-}
{-# DEPRECATED snapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead"  #-}

-- | A unique identifier for the snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsSnapshotScheduleIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe Core.Text)
rfcsSnapshotScheduleIdentifier = Lens.field @"snapshotScheduleIdentifier"
{-# INLINEABLE rfcsSnapshotScheduleIdentifier #-}
{-# DEPRECATED snapshotScheduleIdentifier "Use generic-lens or generic-optics with 'snapshotScheduleIdentifier' instead"  #-}

-- | A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
-- VPC security groups only apply to clusters in VPCs.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsVpcSecurityGroupIds :: Lens.Lens' RestoreFromClusterSnapshot (Core.Maybe [Core.Text])
rfcsVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE rfcsVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery RestoreFromClusterSnapshot where
        toQuery RestoreFromClusterSnapshot{..}
          = Core.toQueryPair "Action"
              ("RestoreFromClusterSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<> Core.toQueryPair "SnapshotIdentifier" snapshotIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AdditionalInfo")
                additionalInfo
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AllowVersionUpgrade")
                allowVersionUpgrade
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AutomatedSnapshotRetentionPeriod")
                automatedSnapshotRetentionPeriod
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZone")
                availabilityZone
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ClusterParameterGroupName")
                clusterParameterGroupName
              Core.<>
              Core.toQueryPair "ClusterSecurityGroups"
                (Core.maybe Core.mempty
                   (Core.toQueryList "ClusterSecurityGroupName")
                   clusterSecurityGroups)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterSubnetGroupName")
                clusterSubnetGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ElasticIp") elasticIp
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnhancedVpcRouting")
                enhancedVpcRouting
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "HsmClientCertificateIdentifier")
                hsmClientCertificateIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "HsmConfigurationIdentifier")
                hsmConfigurationIdentifier
              Core.<>
              Core.toQueryPair "IamRoles"
                (Core.maybe Core.mempty (Core.toQueryList "IamRoleArn") iamRoles)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaintenanceTrackName")
                maintenanceTrackName
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ManualSnapshotRetentionPeriod")
                manualSnapshotRetentionPeriod
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NodeType") nodeType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NumberOfNodes")
                numberOfNodes
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OwnerAccount")
                ownerAccount
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PreferredMaintenanceWindow")
                preferredMaintenanceWindow
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PubliclyAccessible")
                publiclyAccessible
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SnapshotClusterIdentifier")
                snapshotClusterIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SnapshotScheduleIdentifier")
                snapshotScheduleIdentifier
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupIds)

instance Core.ToHeaders RestoreFromClusterSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RestoreFromClusterSnapshot where
        type Rs RestoreFromClusterSnapshot =
             RestoreFromClusterSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "RestoreFromClusterSnapshotResult"
              (\ s h x ->
                 RestoreFromClusterSnapshotResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreFromClusterSnapshotResponse' smart constructor.
data RestoreFromClusterSnapshotResponse = RestoreFromClusterSnapshotResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreFromClusterSnapshotResponse' value with any optional fields omitted.
mkRestoreFromClusterSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreFromClusterSnapshotResponse
mkRestoreFromClusterSnapshotResponse responseStatus
  = RestoreFromClusterSnapshotResponse'{cluster = Core.Nothing,
                                        responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsrrsCluster :: Lens.Lens' RestoreFromClusterSnapshotResponse (Core.Maybe Types.Cluster)
rfcsrrsCluster = Lens.field @"cluster"
{-# INLINEABLE rfcsrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsrrsResponseStatus :: Lens.Lens' RestoreFromClusterSnapshotResponse Core.Int
rfcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rfcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
