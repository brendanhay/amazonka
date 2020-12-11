{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rfcsManualSnapshotRetentionPeriod,
    rfcsEnhancedVPCRouting,
    rfcsAdditionalInfo,
    rfcsSnapshotScheduleIdentifier,
    rfcsPubliclyAccessible,
    rfcsSnapshotClusterIdentifier,
    rfcsMaintenanceTrackName,
    rfcsHSMConfigurationIdentifier,
    rfcsClusterSecurityGroups,
    rfcsAutomatedSnapshotRetentionPeriod,
    rfcsClusterSubnetGroupName,
    rfcsHSMClientCertificateIdentifier,
    rfcsNumberOfNodes,
    rfcsElasticIP,
    rfcsPreferredMaintenanceWindow,
    rfcsKMSKeyId,
    rfcsAvailabilityZone,
    rfcsVPCSecurityGroupIds,
    rfcsIAMRoles,
    rfcsOwnerAccount,
    rfcsNodeType,
    rfcsAllowVersionUpgrade,
    rfcsClusterParameterGroupName,
    rfcsPort,
    rfcsClusterIdentifier,
    rfcsSnapshotIdentifier,

    -- * Destructuring the response
    RestoreFromClusterSnapshotResponse (..),
    mkRestoreFromClusterSnapshotResponse,

    -- ** Response lenses
    rfcsrsCluster,
    rfcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRestoreFromClusterSnapshot' smart constructor.
data RestoreFromClusterSnapshot = RestoreFromClusterSnapshot'
  { manualSnapshotRetentionPeriod ::
      Lude.Maybe Lude.Int,
    enhancedVPCRouting ::
      Lude.Maybe Lude.Bool,
    additionalInfo ::
      Lude.Maybe Lude.Text,
    snapshotScheduleIdentifier ::
      Lude.Maybe Lude.Text,
    publiclyAccessible ::
      Lude.Maybe Lude.Bool,
    snapshotClusterIdentifier ::
      Lude.Maybe Lude.Text,
    maintenanceTrackName ::
      Lude.Maybe Lude.Text,
    hsmConfigurationIdentifier ::
      Lude.Maybe Lude.Text,
    clusterSecurityGroups ::
      Lude.Maybe [Lude.Text],
    automatedSnapshotRetentionPeriod ::
      Lude.Maybe Lude.Int,
    clusterSubnetGroupName ::
      Lude.Maybe Lude.Text,
    hsmClientCertificateIdentifier ::
      Lude.Maybe Lude.Text,
    numberOfNodes :: Lude.Maybe Lude.Int,
    elasticIP :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow ::
      Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    availabilityZone ::
      Lude.Maybe Lude.Text,
    vpcSecurityGroupIds ::
      Lude.Maybe [Lude.Text],
    iamRoles :: Lude.Maybe [Lude.Text],
    ownerAccount :: Lude.Maybe Lude.Text,
    nodeType :: Lude.Maybe Lude.Text,
    allowVersionUpgrade ::
      Lude.Maybe Lude.Bool,
    clusterParameterGroupName ::
      Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Int,
    clusterIdentifier :: Lude.Text,
    snapshotIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreFromClusterSnapshot' with the minimum fields required to make a request.
--
-- * 'additionalInfo' - Reserved.
-- * 'allowVersionUpgrade' - If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster.
--
-- Default: @true@
-- * 'automatedSnapshotRetentionPeriod' - The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
--
-- Default: The value selected for the cluster from which the snapshot was taken.
-- Constraints: Must be a value from 0 to 35.
-- * 'availabilityZone' - The Amazon EC2 Availability Zone in which to restore the cluster.
--
-- Default: A random, system-chosen Availability Zone.
-- Example: @us-east-2a@
-- * 'clusterIdentifier' - The identifier of the cluster that will be created from restoring the snapshot.
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
-- * 'clusterParameterGroupName' - The name of the parameter group to be associated with this cluster.
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
-- * 'clusterSecurityGroups' - A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
-- Cluster security groups only apply to clusters outside of VPCs.
-- * 'clusterSubnetGroupName' - The name of the subnet group where you want to cluster restored.
--
-- A snapshot of cluster in VPC can be restored only in VPC. Therefore, you must provide subnet group name where you want the cluster restored.
-- * 'elasticIP' - The elastic IP (EIP) address for the cluster.
-- * 'enhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
-- * 'hsmClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
-- * 'hsmConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
-- * 'iamRoles' - A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
--
-- A cluster can have up to 10 IAM roles associated at any time.
-- * 'kmsKeyId' - The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster that you restore from a shared snapshot.
-- * 'maintenanceTrackName' - The name of the maintenance track for the restored cluster. When you take a snapshot, the snapshot inherits the @MaintenanceTrack@ value from the cluster. The snapshot might be on a different track than the cluster that was the source for the snapshot. For example, suppose that you take a snapshot of a cluster that is on the current track and then change the cluster to be on the trailing track. In this case, the snapshot and the source cluster are on different tracks.
-- * 'manualSnapshotRetentionPeriod' - The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- * 'nodeType' - The node type that the restored cluster will be provisioned with.
--
-- Default: The node type of the cluster from which the snapshot was taken. You can modify this if you are using any DS node type. In that case, you can choose to restore into another DS node type of the same size. For example, you can restore ds1.8xlarge into ds2.8xlarge, or ds1.xlarge into ds2.xlarge. If you have a DC instance type, you must restore into that same instance type and size. In other words, you can only restore a dc1.large instance type into another dc1.large instance type or dc2.large instance type. You can't restore dc1.8xlarge to dc2.8xlarge. First restore to a dc1.8xlarge cluster, then resize to a dc2.8large cluster. For more information about node types, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-about-clusters-and-nodes About Clusters and Nodes> in the /Amazon Redshift Cluster Management Guide/ .
-- * 'numberOfNodes' - The number of nodes specified when provisioning the restored cluster.
-- * 'ownerAccount' - The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
-- * 'port' - The port number on which the cluster accepts connections.
--
-- Default: The same port as the original cluster.
-- Constraints: Must be between @1115@ and @65535@ .
-- * 'preferredMaintenanceWindow' - The weekly time range (in UTC) during which automated cluster maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- Default: The value selected for the cluster from which the snapshot was taken. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide.
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Minimum 30-minute window.
-- * 'publiclyAccessible' - If @true@ , the cluster can be accessed from a public network.
-- * 'snapshotClusterIdentifier' - The name of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
-- * 'snapshotIdentifier' - The name of the snapshot from which to create the new cluster. This parameter isn't case sensitive.
--
-- Example: @my-snapshot-id@
-- * 'snapshotScheduleIdentifier' - A unique identifier for the snapshot schedule.
-- * 'vpcSecurityGroupIds' - A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
-- VPC security groups only apply to clusters in VPCs.
mkRestoreFromClusterSnapshot ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  -- | 'snapshotIdentifier'
  Lude.Text ->
  RestoreFromClusterSnapshot
mkRestoreFromClusterSnapshot
  pClusterIdentifier_
  pSnapshotIdentifier_ =
    RestoreFromClusterSnapshot'
      { manualSnapshotRetentionPeriod =
          Lude.Nothing,
        enhancedVPCRouting = Lude.Nothing,
        additionalInfo = Lude.Nothing,
        snapshotScheduleIdentifier = Lude.Nothing,
        publiclyAccessible = Lude.Nothing,
        snapshotClusterIdentifier = Lude.Nothing,
        maintenanceTrackName = Lude.Nothing,
        hsmConfigurationIdentifier = Lude.Nothing,
        clusterSecurityGroups = Lude.Nothing,
        automatedSnapshotRetentionPeriod = Lude.Nothing,
        clusterSubnetGroupName = Lude.Nothing,
        hsmClientCertificateIdentifier = Lude.Nothing,
        numberOfNodes = Lude.Nothing,
        elasticIP = Lude.Nothing,
        preferredMaintenanceWindow = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        availabilityZone = Lude.Nothing,
        vpcSecurityGroupIds = Lude.Nothing,
        iamRoles = Lude.Nothing,
        ownerAccount = Lude.Nothing,
        nodeType = Lude.Nothing,
        allowVersionUpgrade = Lude.Nothing,
        clusterParameterGroupName = Lude.Nothing,
        port = Lude.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        snapshotIdentifier = pSnapshotIdentifier_
      }

-- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsManualSnapshotRetentionPeriod :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Int)
rfcsManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
--
-- /Note:/ Consider using 'enhancedVPCRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsEnhancedVPCRouting :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Bool)
rfcsEnhancedVPCRouting = Lens.lens (enhancedVPCRouting :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {enhancedVPCRouting = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsEnhancedVPCRouting "Use generic-lens or generic-optics with 'enhancedVPCRouting' instead." #-}

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAdditionalInfo :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsAdditionalInfo = Lens.lens (additionalInfo :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {additionalInfo = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | A unique identifier for the snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsSnapshotScheduleIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsSnapshotScheduleIdentifier = Lens.lens (snapshotScheduleIdentifier :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotScheduleIdentifier = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsSnapshotScheduleIdentifier "Use generic-lens or generic-optics with 'snapshotScheduleIdentifier' instead." #-}

-- | If @true@ , the cluster can be accessed from a public network.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsPubliclyAccessible :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Bool)
rfcsPubliclyAccessible = Lens.lens (publiclyAccessible :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | The name of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsSnapshotClusterIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsSnapshotClusterIdentifier = Lens.lens (snapshotClusterIdentifier :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotClusterIdentifier = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}

-- | The name of the maintenance track for the restored cluster. When you take a snapshot, the snapshot inherits the @MaintenanceTrack@ value from the cluster. The snapshot might be on a different track than the cluster that was the source for the snapshot. For example, suppose that you take a snapshot of a cluster that is on the current track and then change the cluster to be on the trailing track. In this case, the snapshot and the source cluster are on different tracks.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsMaintenanceTrackName :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsMaintenanceTrackName = Lens.lens (maintenanceTrackName :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {maintenanceTrackName = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsHSMConfigurationIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsHSMConfigurationIdentifier = Lens.lens (hsmConfigurationIdentifier :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {hsmConfigurationIdentifier = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsHSMConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead." #-}

-- | A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
-- Cluster security groups only apply to clusters outside of VPCs.
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsClusterSecurityGroups :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe [Lude.Text])
rfcsClusterSecurityGroups = Lens.lens (clusterSecurityGroups :: RestoreFromClusterSnapshot -> Lude.Maybe [Lude.Text]) (\s a -> s {clusterSecurityGroups = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsClusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead." #-}

-- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
--
-- Default: The value selected for the cluster from which the snapshot was taken.
-- Constraints: Must be a value from 0 to 35.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAutomatedSnapshotRetentionPeriod :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Int)
rfcsAutomatedSnapshotRetentionPeriod = Lens.lens (automatedSnapshotRetentionPeriod :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {automatedSnapshotRetentionPeriod = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsAutomatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead." #-}

-- | The name of the subnet group where you want to cluster restored.
--
-- A snapshot of cluster in VPC can be restored only in VPC. Therefore, you must provide subnet group name where you want the cluster restored.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsClusterSubnetGroupName :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsClusterSubnetGroupName = Lens.lens (clusterSubnetGroupName :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {clusterSubnetGroupName = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsHSMClientCertificateIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsHSMClientCertificateIdentifier = Lens.lens (hsmClientCertificateIdentifier :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {hsmClientCertificateIdentifier = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsHSMClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead." #-}

-- | The number of nodes specified when provisioning the restored cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsNumberOfNodes :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Int)
rfcsNumberOfNodes = Lens.lens (numberOfNodes :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The elastic IP (EIP) address for the cluster.
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsElasticIP :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsElasticIP = Lens.lens (elasticIP :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {elasticIP = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

-- | The weekly time range (in UTC) during which automated cluster maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- Default: The value selected for the cluster from which the snapshot was taken. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide.
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsPreferredMaintenanceWindow :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster that you restore from a shared snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsKMSKeyId :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsKMSKeyId = Lens.lens (kmsKeyId :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The Amazon EC2 Availability Zone in which to restore the cluster.
--
-- Default: A random, system-chosen Availability Zone.
-- Example: @us-east-2a@
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAvailabilityZone :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsAvailabilityZone = Lens.lens (availabilityZone :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
-- VPC security groups only apply to clusters in VPCs.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsVPCSecurityGroupIds :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe [Lude.Text])
rfcsVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: RestoreFromClusterSnapshot -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
--
-- A cluster can have up to 10 IAM roles associated at any time.
--
-- /Note:/ Consider using 'iamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsIAMRoles :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe [Lude.Text])
rfcsIAMRoles = Lens.lens (iamRoles :: RestoreFromClusterSnapshot -> Lude.Maybe [Lude.Text]) (\s a -> s {iamRoles = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsIAMRoles "Use generic-lens or generic-optics with 'iamRoles' instead." #-}

-- | The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsOwnerAccount :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsOwnerAccount = Lens.lens (ownerAccount :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The node type that the restored cluster will be provisioned with.
--
-- Default: The node type of the cluster from which the snapshot was taken. You can modify this if you are using any DS node type. In that case, you can choose to restore into another DS node type of the same size. For example, you can restore ds1.8xlarge into ds2.8xlarge, or ds1.xlarge into ds2.xlarge. If you have a DC instance type, you must restore into that same instance type and size. In other words, you can only restore a dc1.large instance type into another dc1.large instance type or dc2.large instance type. You can't restore dc1.8xlarge to dc2.8xlarge. First restore to a dc1.8xlarge cluster, then resize to a dc2.8large cluster. For more information about node types, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-about-clusters-and-nodes About Clusters and Nodes> in the /Amazon Redshift Cluster Management Guide/ .
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsNodeType :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsNodeType = Lens.lens (nodeType :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster.
--
-- Default: @true@
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsAllowVersionUpgrade :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Bool)
rfcsAllowVersionUpgrade = Lens.lens (allowVersionUpgrade :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {allowVersionUpgrade = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsAllowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead." #-}

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
rfcsClusterParameterGroupName :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Text)
rfcsClusterParameterGroupName = Lens.lens (clusterParameterGroupName :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {clusterParameterGroupName = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsClusterParameterGroupName "Use generic-lens or generic-optics with 'clusterParameterGroupName' instead." #-}

-- | The port number on which the cluster accepts connections.
--
-- Default: The same port as the original cluster.
-- Constraints: Must be between @1115@ and @65535@ .
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsPort :: Lens.Lens' RestoreFromClusterSnapshot (Lude.Maybe Lude.Int)
rfcsPort = Lens.lens (port :: RestoreFromClusterSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsPort "Use generic-lens or generic-optics with 'port' instead." #-}

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
rfcsClusterIdentifier :: Lens.Lens' RestoreFromClusterSnapshot Lude.Text
rfcsClusterIdentifier = Lens.lens (clusterIdentifier :: RestoreFromClusterSnapshot -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The name of the snapshot from which to create the new cluster. This parameter isn't case sensitive.
--
-- Example: @my-snapshot-id@
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsSnapshotIdentifier :: Lens.Lens' RestoreFromClusterSnapshot Lude.Text
rfcsSnapshotIdentifier = Lens.lens (snapshotIdentifier :: RestoreFromClusterSnapshot -> Lude.Text) (\s a -> s {snapshotIdentifier = a} :: RestoreFromClusterSnapshot)
{-# DEPRECATED rfcsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

instance Lude.AWSRequest RestoreFromClusterSnapshot where
  type
    Rs RestoreFromClusterSnapshot =
      RestoreFromClusterSnapshotResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "RestoreFromClusterSnapshotResult"
      ( \s h x ->
          RestoreFromClusterSnapshotResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreFromClusterSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestoreFromClusterSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreFromClusterSnapshot where
  toQuery RestoreFromClusterSnapshot' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RestoreFromClusterSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Lude.=: manualSnapshotRetentionPeriod,
        "EnhancedVpcRouting" Lude.=: enhancedVPCRouting,
        "AdditionalInfo" Lude.=: additionalInfo,
        "SnapshotScheduleIdentifier" Lude.=: snapshotScheduleIdentifier,
        "PubliclyAccessible" Lude.=: publiclyAccessible,
        "SnapshotClusterIdentifier" Lude.=: snapshotClusterIdentifier,
        "MaintenanceTrackName" Lude.=: maintenanceTrackName,
        "HsmConfigurationIdentifier" Lude.=: hsmConfigurationIdentifier,
        "ClusterSecurityGroups"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "ClusterSecurityGroupName"
                Lude.<$> clusterSecurityGroups
            ),
        "AutomatedSnapshotRetentionPeriod"
          Lude.=: automatedSnapshotRetentionPeriod,
        "ClusterSubnetGroupName" Lude.=: clusterSubnetGroupName,
        "HsmClientCertificateIdentifier"
          Lude.=: hsmClientCertificateIdentifier,
        "NumberOfNodes" Lude.=: numberOfNodes,
        "ElasticIp" Lude.=: elasticIP,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "KmsKeyId" Lude.=: kmsKeyId,
        "AvailabilityZone" Lude.=: availabilityZone,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "IamRoles"
          Lude.=: Lude.toQuery (Lude.toQueryList "IamRoleArn" Lude.<$> iamRoles),
        "OwnerAccount" Lude.=: ownerAccount,
        "NodeType" Lude.=: nodeType,
        "AllowVersionUpgrade" Lude.=: allowVersionUpgrade,
        "ClusterParameterGroupName" Lude.=: clusterParameterGroupName,
        "Port" Lude.=: port,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "SnapshotIdentifier" Lude.=: snapshotIdentifier
      ]

-- | /See:/ 'mkRestoreFromClusterSnapshotResponse' smart constructor.
data RestoreFromClusterSnapshotResponse = RestoreFromClusterSnapshotResponse'
  { cluster ::
      Lude.Maybe Cluster,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreFromClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkRestoreFromClusterSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreFromClusterSnapshotResponse
mkRestoreFromClusterSnapshotResponse pResponseStatus_ =
  RestoreFromClusterSnapshotResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsrsCluster :: Lens.Lens' RestoreFromClusterSnapshotResponse (Lude.Maybe Cluster)
rfcsrsCluster = Lens.lens (cluster :: RestoreFromClusterSnapshotResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: RestoreFromClusterSnapshotResponse)
{-# DEPRECATED rfcsrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcsrsResponseStatus :: Lens.Lens' RestoreFromClusterSnapshotResponse Lude.Int
rfcsrsResponseStatus = Lens.lens (responseStatus :: RestoreFromClusterSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreFromClusterSnapshotResponse)
{-# DEPRECATED rfcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
