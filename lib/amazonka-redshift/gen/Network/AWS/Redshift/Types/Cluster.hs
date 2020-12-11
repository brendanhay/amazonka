-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Cluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Cluster
  ( Cluster (..),

    -- * Smart constructor
    mkCluster,

    -- * Lenses
    cResizeInfo,
    cRestoreStatus,
    cManualSnapshotRetentionPeriod,
    cEnhancedVPCRouting,
    cClusterSnapshotCopyStatus,
    cClusterAvailabilityStatus,
    cClusterRevisionNumber,
    cSnapshotScheduleIdentifier,
    cPubliclyAccessible,
    cMasterUsername,
    cMaintenanceTrackName,
    cExpectedNextSnapshotScheduleTime,
    cElasticResizeNumberOfNodeOptions,
    cVPCId,
    cClusterSecurityGroups,
    cAutomatedSnapshotRetentionPeriod,
    cSnapshotScheduleState,
    cDataTransferProgress,
    cEncrypted,
    cClusterSubnetGroupName,
    cExpectedNextSnapshotScheduleTimeStatus,
    cClusterIdentifier,
    cDeferredMaintenanceWindows,
    cNumberOfNodes,
    cClusterPublicKey,
    cPreferredMaintenanceWindow,
    cModifyStatus,
    cClusterNamespaceARN,
    cKMSKeyId,
    cClusterParameterGroups,
    cAvailabilityZone,
    cVPCSecurityGroups,
    cHSMStatus,
    cIAMRoles,
    cPendingActions,
    cElasticIPStatus,
    cClusterVersion,
    cNodeType,
    cNextMaintenanceWindowStartTime,
    cClusterCreateTime,
    cEndpoint,
    cAllowVersionUpgrade,
    cClusterStatus,
    cPendingModifiedValues,
    cTags,
    cClusterNodes,
    cDBName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ClusterIAMRole
import Network.AWS.Redshift.Types.ClusterNode
import Network.AWS.Redshift.Types.ClusterParameterGroupStatus
import Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
import Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
import Network.AWS.Redshift.Types.DataTransferProgress
import Network.AWS.Redshift.Types.DeferredMaintenanceWindow
import Network.AWS.Redshift.Types.ElasticIPStatus
import Network.AWS.Redshift.Types.Endpoint
import Network.AWS.Redshift.Types.HSMStatus
import Network.AWS.Redshift.Types.PendingModifiedValues
import Network.AWS.Redshift.Types.ResizeInfo
import Network.AWS.Redshift.Types.RestoreStatus
import Network.AWS.Redshift.Types.ScheduleState
import Network.AWS.Redshift.Types.Tag
import Network.AWS.Redshift.Types.VPCSecurityGroupMembership

-- | Describes a cluster.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { resizeInfo :: Lude.Maybe ResizeInfo,
    restoreStatus :: Lude.Maybe RestoreStatus,
    manualSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    enhancedVPCRouting :: Lude.Maybe Lude.Bool,
    clusterSnapshotCopyStatus :: Lude.Maybe ClusterSnapshotCopyStatus,
    clusterAvailabilityStatus :: Lude.Maybe Lude.Text,
    clusterRevisionNumber :: Lude.Maybe Lude.Text,
    snapshotScheduleIdentifier :: Lude.Maybe Lude.Text,
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    masterUsername :: Lude.Maybe Lude.Text,
    maintenanceTrackName :: Lude.Maybe Lude.Text,
    expectedNextSnapshotScheduleTime :: Lude.Maybe Lude.ISO8601,
    elasticResizeNumberOfNodeOptions :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    clusterSecurityGroups ::
      Lude.Maybe [ClusterSecurityGroupMembership],
    automatedSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    snapshotScheduleState :: Lude.Maybe ScheduleState,
    dataTransferProgress :: Lude.Maybe DataTransferProgress,
    encrypted :: Lude.Maybe Lude.Bool,
    clusterSubnetGroupName :: Lude.Maybe Lude.Text,
    expectedNextSnapshotScheduleTimeStatus :: Lude.Maybe Lude.Text,
    clusterIdentifier :: Lude.Maybe Lude.Text,
    deferredMaintenanceWindows ::
      Lude.Maybe [DeferredMaintenanceWindow],
    numberOfNodes :: Lude.Maybe Lude.Int,
    clusterPublicKey :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    modifyStatus :: Lude.Maybe Lude.Text,
    clusterNamespaceARN :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    clusterParameterGroups :: Lude.Maybe [ClusterParameterGroupStatus],
    availabilityZone :: Lude.Maybe Lude.Text,
    vpcSecurityGroups :: Lude.Maybe [VPCSecurityGroupMembership],
    hsmStatus :: Lude.Maybe HSMStatus,
    iamRoles :: Lude.Maybe [ClusterIAMRole],
    pendingActions :: Lude.Maybe [Lude.Text],
    elasticIPStatus :: Lude.Maybe ElasticIPStatus,
    clusterVersion :: Lude.Maybe Lude.Text,
    nodeType :: Lude.Maybe Lude.Text,
    nextMaintenanceWindowStartTime :: Lude.Maybe Lude.ISO8601,
    clusterCreateTime :: Lude.Maybe Lude.ISO8601,
    endpoint :: Lude.Maybe Endpoint,
    allowVersionUpgrade :: Lude.Maybe Lude.Bool,
    clusterStatus :: Lude.Maybe Lude.Text,
    pendingModifiedValues :: Lude.Maybe PendingModifiedValues,
    tags :: Lude.Maybe [Tag],
    clusterNodes :: Lude.Maybe [ClusterNode],
    dbName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- * 'allowVersionUpgrade' - A boolean value that, if @true@ , indicates that major version upgrades will be applied automatically to the cluster during the maintenance window.
-- * 'automatedSnapshotRetentionPeriod' - The number of days that automatic cluster snapshots are retained.
-- * 'availabilityZone' - The name of the Availability Zone in which the cluster is located.
-- * 'clusterAvailabilityStatus' - The availability status of the cluster for queries. Possible values are the following:
--
--
--     * Available - The cluster is available for queries.
--
--
--     * Unavailable - The cluster is not available for queries.
--
--
--     * Maintenance - The cluster is intermittently available for queries due to maintenance activities.
--
--
--     * Modifying - The cluster is intermittently available for queries due to changes that modify the cluster.
--
--
--     * Failed - The cluster failed and is not available for queries.
--
--
-- * 'clusterCreateTime' - The date and time that the cluster was created.
-- * 'clusterIdentifier' - The unique identifier of the cluster.
-- * 'clusterNamespaceARN' - The namespace Amazon Resource Name (ARN) of the cluster.
-- * 'clusterNodes' - The nodes in the cluster.
-- * 'clusterParameterGroups' - The list of cluster parameter groups that are associated with this cluster. Each parameter group in the list is returned with its status.
-- * 'clusterPublicKey' - The public key for the cluster.
-- * 'clusterRevisionNumber' - The specific revision number of the database in the cluster.
-- * 'clusterSecurityGroups' - A list of cluster security group that are associated with the cluster. Each security group is represented by an element that contains @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@ subelements.
--
-- Cluster security groups are used when the cluster is not created in an Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC use VPC security groups, which are listed by the __VpcSecurityGroups__ parameter.
-- * 'clusterSnapshotCopyStatus' - A value that returns the destination region and retention period that are configured for cross-region snapshot copy.
-- * 'clusterStatus' - The current state of the cluster. Possible values are the following:
--
--
--     * @available@
--
--
--     * @available, prep-for-resize@
--
--
--     * @available, resize-cleanup@
--
--
--     * @cancelling-resize@
--
--
--     * @creating@
--
--
--     * @deleting@
--
--
--     * @final-snapshot@
--
--
--     * @hardware-failure@
--
--
--     * @incompatible-hsm@
--
--
--     * @incompatible-network@
--
--
--     * @incompatible-parameters@
--
--
--     * @incompatible-restore@
--
--
--     * @modifying@
--
--
--     * @paused@
--
--
--     * @rebooting@
--
--
--     * @renaming@
--
--
--     * @resizing@
--
--
--     * @rotating-keys@
--
--
--     * @storage-full@
--
--
--     * @updating-hsm@
--
--
-- * 'clusterSubnetGroupName' - The name of the subnet group that is associated with the cluster. This parameter is valid only when the cluster is in a VPC.
-- * 'clusterVersion' - The version ID of the Amazon Redshift engine that is running on the cluster.
-- * 'dataTransferProgress' -
-- * 'dbName' - The name of the initial database that was created when the cluster was created. This same name is returned for the life of the cluster. If an initial database was not specified, a database named @dev@ dev was created by default.
-- * 'deferredMaintenanceWindows' - Describes a group of @DeferredMaintenanceWindow@ objects.
-- * 'elasticIPStatus' - The status of the elastic IP (EIP) address.
-- * 'elasticResizeNumberOfNodeOptions' - The number of nodes that you can resize the cluster to with the elastic resize method.
-- * 'encrypted' - A boolean value that, if @true@ , indicates that data in the cluster is encrypted at rest.
-- * 'endpoint' - The connection endpoint.
-- * 'enhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
-- * 'expectedNextSnapshotScheduleTime' - The date and time when the next snapshot is expected to be taken for clusters with a valid snapshot schedule and backups enabled.
-- * 'expectedNextSnapshotScheduleTimeStatus' - The status of next expected snapshot for clusters having a valid snapshot schedule and backups enabled. Possible values are the following:
--
--
--     * OnTrack - The next snapshot is expected to be taken on time.
--
--
--     * Pending - The next snapshot is pending to be taken.
--
--
-- * 'hsmStatus' - A value that reports whether the Amazon Redshift cluster has finished applying any hardware security module (HSM) settings changes specified in a modify cluster command.
--
-- Values: active, applying
-- * 'iamRoles' - A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
-- * 'kmsKeyId' - The AWS Key Management Service (AWS KMS) key ID of the encryption key used to encrypt data in the cluster.
-- * 'maintenanceTrackName' - The name of the maintenance track for the cluster.
-- * 'manualSnapshotRetentionPeriod' - The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- * 'masterUsername' - The master user name for the cluster. This name is used to connect to the database that is specified in the __DBName__ parameter.
-- * 'modifyStatus' - The status of a modify operation, if any, initiated for the cluster.
-- * 'nextMaintenanceWindowStartTime' - The date and time in UTC when system maintenance can begin.
-- * 'nodeType' - The node type for the nodes in the cluster.
-- * 'numberOfNodes' - The number of compute nodes in the cluster.
-- * 'pendingActions' - Cluster operations that are waiting to be started.
-- * 'pendingModifiedValues' - A value that, if present, indicates that changes to the cluster are pending. Specific pending changes are identified by subelements.
-- * 'preferredMaintenanceWindow' - The weekly time range, in Universal Coordinated Time (UTC), during which system maintenance can occur.
-- * 'publiclyAccessible' - A boolean value that, if @true@ , indicates that the cluster can be accessed from a public network.
-- * 'resizeInfo' - Returns the following:
--
--
--     * AllowCancelResize: a boolean value indicating if the resize operation can be cancelled.
--
--
--     * ResizeType: Returns ClassicResize
--
--
-- * 'restoreStatus' - A value that describes the status of a cluster restore action. This parameter returns null if the cluster was not created by restoring a snapshot.
-- * 'snapshotScheduleIdentifier' - A unique identifier for the cluster snapshot schedule.
-- * 'snapshotScheduleState' - The current state of the cluster snapshot schedule.
-- * 'tags' - The list of tags for the cluster.
-- * 'vpcId' - The identifier of the VPC the cluster is in, if the cluster is in a VPC.
-- * 'vpcSecurityGroups' - A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that are associated with the cluster. This parameter is returned only if the cluster is in a VPC.
mkCluster ::
  Cluster
mkCluster =
  Cluster'
    { resizeInfo = Lude.Nothing,
      restoreStatus = Lude.Nothing,
      manualSnapshotRetentionPeriod = Lude.Nothing,
      enhancedVPCRouting = Lude.Nothing,
      clusterSnapshotCopyStatus = Lude.Nothing,
      clusterAvailabilityStatus = Lude.Nothing,
      clusterRevisionNumber = Lude.Nothing,
      snapshotScheduleIdentifier = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      masterUsername = Lude.Nothing,
      maintenanceTrackName = Lude.Nothing,
      expectedNextSnapshotScheduleTime = Lude.Nothing,
      elasticResizeNumberOfNodeOptions = Lude.Nothing,
      vpcId = Lude.Nothing,
      clusterSecurityGroups = Lude.Nothing,
      automatedSnapshotRetentionPeriod = Lude.Nothing,
      snapshotScheduleState = Lude.Nothing,
      dataTransferProgress = Lude.Nothing,
      encrypted = Lude.Nothing,
      clusterSubnetGroupName = Lude.Nothing,
      expectedNextSnapshotScheduleTimeStatus = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      deferredMaintenanceWindows = Lude.Nothing,
      numberOfNodes = Lude.Nothing,
      clusterPublicKey = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      modifyStatus = Lude.Nothing,
      clusterNamespaceARN = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      clusterParameterGroups = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      vpcSecurityGroups = Lude.Nothing,
      hsmStatus = Lude.Nothing,
      iamRoles = Lude.Nothing,
      pendingActions = Lude.Nothing,
      elasticIPStatus = Lude.Nothing,
      clusterVersion = Lude.Nothing,
      nodeType = Lude.Nothing,
      nextMaintenanceWindowStartTime = Lude.Nothing,
      clusterCreateTime = Lude.Nothing,
      endpoint = Lude.Nothing,
      allowVersionUpgrade = Lude.Nothing,
      clusterStatus = Lude.Nothing,
      pendingModifiedValues = Lude.Nothing,
      tags = Lude.Nothing,
      clusterNodes = Lude.Nothing,
      dbName = Lude.Nothing
    }

-- | Returns the following:
--
--
--     * AllowCancelResize: a boolean value indicating if the resize operation can be cancelled.
--
--
--     * ResizeType: Returns ClassicResize
--
--
--
-- /Note:/ Consider using 'resizeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cResizeInfo :: Lens.Lens' Cluster (Lude.Maybe ResizeInfo)
cResizeInfo = Lens.lens (resizeInfo :: Cluster -> Lude.Maybe ResizeInfo) (\s a -> s {resizeInfo = a} :: Cluster)
{-# DEPRECATED cResizeInfo "Use generic-lens or generic-optics with 'resizeInfo' instead." #-}

-- | A value that describes the status of a cluster restore action. This parameter returns null if the cluster was not created by restoring a snapshot.
--
-- /Note:/ Consider using 'restoreStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRestoreStatus :: Lens.Lens' Cluster (Lude.Maybe RestoreStatus)
cRestoreStatus = Lens.lens (restoreStatus :: Cluster -> Lude.Maybe RestoreStatus) (\s a -> s {restoreStatus = a} :: Cluster)
{-# DEPRECATED cRestoreStatus "Use generic-lens or generic-optics with 'restoreStatus' instead." #-}

-- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cManualSnapshotRetentionPeriod :: Lens.Lens' Cluster (Lude.Maybe Lude.Int)
cManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: Cluster -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: Cluster)
{-# DEPRECATED cManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
--
-- /Note:/ Consider using 'enhancedVPCRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnhancedVPCRouting :: Lens.Lens' Cluster (Lude.Maybe Lude.Bool)
cEnhancedVPCRouting = Lens.lens (enhancedVPCRouting :: Cluster -> Lude.Maybe Lude.Bool) (\s a -> s {enhancedVPCRouting = a} :: Cluster)
{-# DEPRECATED cEnhancedVPCRouting "Use generic-lens or generic-optics with 'enhancedVPCRouting' instead." #-}

-- | A value that returns the destination region and retention period that are configured for cross-region snapshot copy.
--
-- /Note:/ Consider using 'clusterSnapshotCopyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterSnapshotCopyStatus :: Lens.Lens' Cluster (Lude.Maybe ClusterSnapshotCopyStatus)
cClusterSnapshotCopyStatus = Lens.lens (clusterSnapshotCopyStatus :: Cluster -> Lude.Maybe ClusterSnapshotCopyStatus) (\s a -> s {clusterSnapshotCopyStatus = a} :: Cluster)
{-# DEPRECATED cClusterSnapshotCopyStatus "Use generic-lens or generic-optics with 'clusterSnapshotCopyStatus' instead." #-}

-- | The availability status of the cluster for queries. Possible values are the following:
--
--
--     * Available - The cluster is available for queries.
--
--
--     * Unavailable - The cluster is not available for queries.
--
--
--     * Maintenance - The cluster is intermittently available for queries due to maintenance activities.
--
--
--     * Modifying - The cluster is intermittently available for queries due to changes that modify the cluster.
--
--
--     * Failed - The cluster failed and is not available for queries.
--
--
--
-- /Note:/ Consider using 'clusterAvailabilityStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterAvailabilityStatus :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterAvailabilityStatus = Lens.lens (clusterAvailabilityStatus :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterAvailabilityStatus = a} :: Cluster)
{-# DEPRECATED cClusterAvailabilityStatus "Use generic-lens or generic-optics with 'clusterAvailabilityStatus' instead." #-}

-- | The specific revision number of the database in the cluster.
--
-- /Note:/ Consider using 'clusterRevisionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterRevisionNumber :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterRevisionNumber = Lens.lens (clusterRevisionNumber :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterRevisionNumber = a} :: Cluster)
{-# DEPRECATED cClusterRevisionNumber "Use generic-lens or generic-optics with 'clusterRevisionNumber' instead." #-}

-- | A unique identifier for the cluster snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSnapshotScheduleIdentifier :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cSnapshotScheduleIdentifier = Lens.lens (snapshotScheduleIdentifier :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {snapshotScheduleIdentifier = a} :: Cluster)
{-# DEPRECATED cSnapshotScheduleIdentifier "Use generic-lens or generic-optics with 'snapshotScheduleIdentifier' instead." #-}

-- | A boolean value that, if @true@ , indicates that the cluster can be accessed from a public network.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPubliclyAccessible :: Lens.Lens' Cluster (Lude.Maybe Lude.Bool)
cPubliclyAccessible = Lens.lens (publiclyAccessible :: Cluster -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: Cluster)
{-# DEPRECATED cPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | The master user name for the cluster. This name is used to connect to the database that is specified in the __DBName__ parameter.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMasterUsername :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cMasterUsername = Lens.lens (masterUsername :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: Cluster)
{-# DEPRECATED cMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The name of the maintenance track for the cluster.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMaintenanceTrackName :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cMaintenanceTrackName = Lens.lens (maintenanceTrackName :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {maintenanceTrackName = a} :: Cluster)
{-# DEPRECATED cMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | The date and time when the next snapshot is expected to be taken for clusters with a valid snapshot schedule and backups enabled.
--
-- /Note:/ Consider using 'expectedNextSnapshotScheduleTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpectedNextSnapshotScheduleTime :: Lens.Lens' Cluster (Lude.Maybe Lude.ISO8601)
cExpectedNextSnapshotScheduleTime = Lens.lens (expectedNextSnapshotScheduleTime :: Cluster -> Lude.Maybe Lude.ISO8601) (\s a -> s {expectedNextSnapshotScheduleTime = a} :: Cluster)
{-# DEPRECATED cExpectedNextSnapshotScheduleTime "Use generic-lens or generic-optics with 'expectedNextSnapshotScheduleTime' instead." #-}

-- | The number of nodes that you can resize the cluster to with the elastic resize method.
--
-- /Note:/ Consider using 'elasticResizeNumberOfNodeOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cElasticResizeNumberOfNodeOptions :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cElasticResizeNumberOfNodeOptions = Lens.lens (elasticResizeNumberOfNodeOptions :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {elasticResizeNumberOfNodeOptions = a} :: Cluster)
{-# DEPRECATED cElasticResizeNumberOfNodeOptions "Use generic-lens or generic-optics with 'elasticResizeNumberOfNodeOptions' instead." #-}

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVPCId :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cVPCId = Lens.lens (vpcId :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: Cluster)
{-# DEPRECATED cVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of cluster security group that are associated with the cluster. Each security group is represented by an element that contains @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@ subelements.
--
-- Cluster security groups are used when the cluster is not created in an Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC use VPC security groups, which are listed by the __VpcSecurityGroups__ parameter.
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterSecurityGroups :: Lens.Lens' Cluster (Lude.Maybe [ClusterSecurityGroupMembership])
cClusterSecurityGroups = Lens.lens (clusterSecurityGroups :: Cluster -> Lude.Maybe [ClusterSecurityGroupMembership]) (\s a -> s {clusterSecurityGroups = a} :: Cluster)
{-# DEPRECATED cClusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead." #-}

-- | The number of days that automatic cluster snapshots are retained.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAutomatedSnapshotRetentionPeriod :: Lens.Lens' Cluster (Lude.Maybe Lude.Int)
cAutomatedSnapshotRetentionPeriod = Lens.lens (automatedSnapshotRetentionPeriod :: Cluster -> Lude.Maybe Lude.Int) (\s a -> s {automatedSnapshotRetentionPeriod = a} :: Cluster)
{-# DEPRECATED cAutomatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead." #-}

-- | The current state of the cluster snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSnapshotScheduleState :: Lens.Lens' Cluster (Lude.Maybe ScheduleState)
cSnapshotScheduleState = Lens.lens (snapshotScheduleState :: Cluster -> Lude.Maybe ScheduleState) (\s a -> s {snapshotScheduleState = a} :: Cluster)
{-# DEPRECATED cSnapshotScheduleState "Use generic-lens or generic-optics with 'snapshotScheduleState' instead." #-}

-- |
--
-- /Note:/ Consider using 'dataTransferProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDataTransferProgress :: Lens.Lens' Cluster (Lude.Maybe DataTransferProgress)
cDataTransferProgress = Lens.lens (dataTransferProgress :: Cluster -> Lude.Maybe DataTransferProgress) (\s a -> s {dataTransferProgress = a} :: Cluster)
{-# DEPRECATED cDataTransferProgress "Use generic-lens or generic-optics with 'dataTransferProgress' instead." #-}

-- | A boolean value that, if @true@ , indicates that data in the cluster is encrypted at rest.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEncrypted :: Lens.Lens' Cluster (Lude.Maybe Lude.Bool)
cEncrypted = Lens.lens (encrypted :: Cluster -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: Cluster)
{-# DEPRECATED cEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The name of the subnet group that is associated with the cluster. This parameter is valid only when the cluster is in a VPC.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterSubnetGroupName :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterSubnetGroupName = Lens.lens (clusterSubnetGroupName :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterSubnetGroupName = a} :: Cluster)
{-# DEPRECATED cClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | The status of next expected snapshot for clusters having a valid snapshot schedule and backups enabled. Possible values are the following:
--
--
--     * OnTrack - The next snapshot is expected to be taken on time.
--
--
--     * Pending - The next snapshot is pending to be taken.
--
--
--
-- /Note:/ Consider using 'expectedNextSnapshotScheduleTimeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpectedNextSnapshotScheduleTimeStatus :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cExpectedNextSnapshotScheduleTimeStatus = Lens.lens (expectedNextSnapshotScheduleTimeStatus :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {expectedNextSnapshotScheduleTimeStatus = a} :: Cluster)
{-# DEPRECATED cExpectedNextSnapshotScheduleTimeStatus "Use generic-lens or generic-optics with 'expectedNextSnapshotScheduleTimeStatus' instead." #-}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterIdentifier :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterIdentifier = Lens.lens (clusterIdentifier :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: Cluster)
{-# DEPRECATED cClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | Describes a group of @DeferredMaintenanceWindow@ objects.
--
-- /Note:/ Consider using 'deferredMaintenanceWindows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeferredMaintenanceWindows :: Lens.Lens' Cluster (Lude.Maybe [DeferredMaintenanceWindow])
cDeferredMaintenanceWindows = Lens.lens (deferredMaintenanceWindows :: Cluster -> Lude.Maybe [DeferredMaintenanceWindow]) (\s a -> s {deferredMaintenanceWindows = a} :: Cluster)
{-# DEPRECATED cDeferredMaintenanceWindows "Use generic-lens or generic-optics with 'deferredMaintenanceWindows' instead." #-}

-- | The number of compute nodes in the cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNumberOfNodes :: Lens.Lens' Cluster (Lude.Maybe Lude.Int)
cNumberOfNodes = Lens.lens (numberOfNodes :: Cluster -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: Cluster)
{-# DEPRECATED cNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The public key for the cluster.
--
-- /Note:/ Consider using 'clusterPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterPublicKey :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterPublicKey = Lens.lens (clusterPublicKey :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterPublicKey = a} :: Cluster)
{-# DEPRECATED cClusterPublicKey "Use generic-lens or generic-optics with 'clusterPublicKey' instead." #-}

-- | The weekly time range, in Universal Coordinated Time (UTC), during which system maintenance can occur.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPreferredMaintenanceWindow :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: Cluster)
{-# DEPRECATED cPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The status of a modify operation, if any, initiated for the cluster.
--
-- /Note:/ Consider using 'modifyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cModifyStatus :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cModifyStatus = Lens.lens (modifyStatus :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {modifyStatus = a} :: Cluster)
{-# DEPRECATED cModifyStatus "Use generic-lens or generic-optics with 'modifyStatus' instead." #-}

-- | The namespace Amazon Resource Name (ARN) of the cluster.
--
-- /Note:/ Consider using 'clusterNamespaceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterNamespaceARN :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterNamespaceARN = Lens.lens (clusterNamespaceARN :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterNamespaceARN = a} :: Cluster)
{-# DEPRECATED cClusterNamespaceARN "Use generic-lens or generic-optics with 'clusterNamespaceARN' instead." #-}

-- | The AWS Key Management Service (AWS KMS) key ID of the encryption key used to encrypt data in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKMSKeyId :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cKMSKeyId = Lens.lens (kmsKeyId :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: Cluster)
{-# DEPRECATED cKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The list of cluster parameter groups that are associated with this cluster. Each parameter group in the list is returned with its status.
--
-- /Note:/ Consider using 'clusterParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterParameterGroups :: Lens.Lens' Cluster (Lude.Maybe [ClusterParameterGroupStatus])
cClusterParameterGroups = Lens.lens (clusterParameterGroups :: Cluster -> Lude.Maybe [ClusterParameterGroupStatus]) (\s a -> s {clusterParameterGroups = a} :: Cluster)
{-# DEPRECATED cClusterParameterGroups "Use generic-lens or generic-optics with 'clusterParameterGroups' instead." #-}

-- | The name of the Availability Zone in which the cluster is located.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAvailabilityZone :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cAvailabilityZone = Lens.lens (availabilityZone :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: Cluster)
{-# DEPRECATED cAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that are associated with the cluster. This parameter is returned only if the cluster is in a VPC.
--
-- /Note:/ Consider using 'vpcSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVPCSecurityGroups :: Lens.Lens' Cluster (Lude.Maybe [VPCSecurityGroupMembership])
cVPCSecurityGroups = Lens.lens (vpcSecurityGroups :: Cluster -> Lude.Maybe [VPCSecurityGroupMembership]) (\s a -> s {vpcSecurityGroups = a} :: Cluster)
{-# DEPRECATED cVPCSecurityGroups "Use generic-lens or generic-optics with 'vpcSecurityGroups' instead." #-}

-- | A value that reports whether the Amazon Redshift cluster has finished applying any hardware security module (HSM) settings changes specified in a modify cluster command.
--
-- Values: active, applying
--
-- /Note:/ Consider using 'hsmStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHSMStatus :: Lens.Lens' Cluster (Lude.Maybe HSMStatus)
cHSMStatus = Lens.lens (hsmStatus :: Cluster -> Lude.Maybe HSMStatus) (\s a -> s {hsmStatus = a} :: Cluster)
{-# DEPRECATED cHSMStatus "Use generic-lens or generic-optics with 'hsmStatus' instead." #-}

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
--
-- /Note:/ Consider using 'iamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIAMRoles :: Lens.Lens' Cluster (Lude.Maybe [ClusterIAMRole])
cIAMRoles = Lens.lens (iamRoles :: Cluster -> Lude.Maybe [ClusterIAMRole]) (\s a -> s {iamRoles = a} :: Cluster)
{-# DEPRECATED cIAMRoles "Use generic-lens or generic-optics with 'iamRoles' instead." #-}

-- | Cluster operations that are waiting to be started.
--
-- /Note:/ Consider using 'pendingActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPendingActions :: Lens.Lens' Cluster (Lude.Maybe [Lude.Text])
cPendingActions = Lens.lens (pendingActions :: Cluster -> Lude.Maybe [Lude.Text]) (\s a -> s {pendingActions = a} :: Cluster)
{-# DEPRECATED cPendingActions "Use generic-lens or generic-optics with 'pendingActions' instead." #-}

-- | The status of the elastic IP (EIP) address.
--
-- /Note:/ Consider using 'elasticIPStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cElasticIPStatus :: Lens.Lens' Cluster (Lude.Maybe ElasticIPStatus)
cElasticIPStatus = Lens.lens (elasticIPStatus :: Cluster -> Lude.Maybe ElasticIPStatus) (\s a -> s {elasticIPStatus = a} :: Cluster)
{-# DEPRECATED cElasticIPStatus "Use generic-lens or generic-optics with 'elasticIPStatus' instead." #-}

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterVersion :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterVersion = Lens.lens (clusterVersion :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterVersion = a} :: Cluster)
{-# DEPRECATED cClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | The node type for the nodes in the cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodeType :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cNodeType = Lens.lens (nodeType :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: Cluster)
{-# DEPRECATED cNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The date and time in UTC when system maintenance can begin.
--
-- /Note:/ Consider using 'nextMaintenanceWindowStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNextMaintenanceWindowStartTime :: Lens.Lens' Cluster (Lude.Maybe Lude.ISO8601)
cNextMaintenanceWindowStartTime = Lens.lens (nextMaintenanceWindowStartTime :: Cluster -> Lude.Maybe Lude.ISO8601) (\s a -> s {nextMaintenanceWindowStartTime = a} :: Cluster)
{-# DEPRECATED cNextMaintenanceWindowStartTime "Use generic-lens or generic-optics with 'nextMaintenanceWindowStartTime' instead." #-}

-- | The date and time that the cluster was created.
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterCreateTime :: Lens.Lens' Cluster (Lude.Maybe Lude.ISO8601)
cClusterCreateTime = Lens.lens (clusterCreateTime :: Cluster -> Lude.Maybe Lude.ISO8601) (\s a -> s {clusterCreateTime = a} :: Cluster)
{-# DEPRECATED cClusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead." #-}

-- | The connection endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEndpoint :: Lens.Lens' Cluster (Lude.Maybe Endpoint)
cEndpoint = Lens.lens (endpoint :: Cluster -> Lude.Maybe Endpoint) (\s a -> s {endpoint = a} :: Cluster)
{-# DEPRECATED cEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | A boolean value that, if @true@ , indicates that major version upgrades will be applied automatically to the cluster during the maintenance window.
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAllowVersionUpgrade :: Lens.Lens' Cluster (Lude.Maybe Lude.Bool)
cAllowVersionUpgrade = Lens.lens (allowVersionUpgrade :: Cluster -> Lude.Maybe Lude.Bool) (\s a -> s {allowVersionUpgrade = a} :: Cluster)
{-# DEPRECATED cAllowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead." #-}

-- | The current state of the cluster. Possible values are the following:
--
--
--     * @available@
--
--
--     * @available, prep-for-resize@
--
--
--     * @available, resize-cleanup@
--
--
--     * @cancelling-resize@
--
--
--     * @creating@
--
--
--     * @deleting@
--
--
--     * @final-snapshot@
--
--
--     * @hardware-failure@
--
--
--     * @incompatible-hsm@
--
--
--     * @incompatible-network@
--
--
--     * @incompatible-parameters@
--
--
--     * @incompatible-restore@
--
--
--     * @modifying@
--
--
--     * @paused@
--
--
--     * @rebooting@
--
--
--     * @renaming@
--
--
--     * @resizing@
--
--
--     * @rotating-keys@
--
--
--     * @storage-full@
--
--
--     * @updating-hsm@
--
--
--
-- /Note:/ Consider using 'clusterStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterStatus :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterStatus = Lens.lens (clusterStatus :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterStatus = a} :: Cluster)
{-# DEPRECATED cClusterStatus "Use generic-lens or generic-optics with 'clusterStatus' instead." #-}

-- | A value that, if present, indicates that changes to the cluster are pending. Specific pending changes are identified by subelements.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPendingModifiedValues :: Lens.Lens' Cluster (Lude.Maybe PendingModifiedValues)
cPendingModifiedValues = Lens.lens (pendingModifiedValues :: Cluster -> Lude.Maybe PendingModifiedValues) (\s a -> s {pendingModifiedValues = a} :: Cluster)
{-# DEPRECATED cPendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead." #-}

-- | The list of tags for the cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Cluster (Lude.Maybe [Tag])
cTags = Lens.lens (tags :: Cluster -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Cluster)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The nodes in the cluster.
--
-- /Note:/ Consider using 'clusterNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterNodes :: Lens.Lens' Cluster (Lude.Maybe [ClusterNode])
cClusterNodes = Lens.lens (clusterNodes :: Cluster -> Lude.Maybe [ClusterNode]) (\s a -> s {clusterNodes = a} :: Cluster)
{-# DEPRECATED cClusterNodes "Use generic-lens or generic-optics with 'clusterNodes' instead." #-}

-- | The name of the initial database that was created when the cluster was created. This same name is returned for the life of the cluster. If an initial database was not specified, a database named @dev@ dev was created by default.
--
-- /Note:/ Consider using 'dbName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDBName :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cDBName = Lens.lens (dbName :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {dbName = a} :: Cluster)
{-# DEPRECATED cDBName "Use generic-lens or generic-optics with 'dbName' instead." #-}

instance Lude.FromXML Cluster where
  parseXML x =
    Cluster'
      Lude.<$> (x Lude..@? "ResizeInfo")
      Lude.<*> (x Lude..@? "RestoreStatus")
      Lude.<*> (x Lude..@? "ManualSnapshotRetentionPeriod")
      Lude.<*> (x Lude..@? "EnhancedVpcRouting")
      Lude.<*> (x Lude..@? "ClusterSnapshotCopyStatus")
      Lude.<*> (x Lude..@? "ClusterAvailabilityStatus")
      Lude.<*> (x Lude..@? "ClusterRevisionNumber")
      Lude.<*> (x Lude..@? "SnapshotScheduleIdentifier")
      Lude.<*> (x Lude..@? "PubliclyAccessible")
      Lude.<*> (x Lude..@? "MasterUsername")
      Lude.<*> (x Lude..@? "MaintenanceTrackName")
      Lude.<*> (x Lude..@? "ExpectedNextSnapshotScheduleTime")
      Lude.<*> (x Lude..@? "ElasticResizeNumberOfNodeOptions")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> ( x Lude..@? "ClusterSecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ClusterSecurityGroup")
               )
      Lude.<*> (x Lude..@? "AutomatedSnapshotRetentionPeriod")
      Lude.<*> (x Lude..@? "SnapshotScheduleState")
      Lude.<*> (x Lude..@? "DataTransferProgress")
      Lude.<*> (x Lude..@? "Encrypted")
      Lude.<*> (x Lude..@? "ClusterSubnetGroupName")
      Lude.<*> (x Lude..@? "ExpectedNextSnapshotScheduleTimeStatus")
      Lude.<*> (x Lude..@? "ClusterIdentifier")
      Lude.<*> ( x Lude..@? "DeferredMaintenanceWindows" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DeferredMaintenanceWindow")
               )
      Lude.<*> (x Lude..@? "NumberOfNodes")
      Lude.<*> (x Lude..@? "ClusterPublicKey")
      Lude.<*> (x Lude..@? "PreferredMaintenanceWindow")
      Lude.<*> (x Lude..@? "ModifyStatus")
      Lude.<*> (x Lude..@? "ClusterNamespaceArn")
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> ( x Lude..@? "ClusterParameterGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ClusterParameterGroup")
               )
      Lude.<*> (x Lude..@? "AvailabilityZone")
      Lude.<*> ( x Lude..@? "VpcSecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "VpcSecurityGroup")
               )
      Lude.<*> (x Lude..@? "HsmStatus")
      Lude.<*> ( x Lude..@? "IamRoles" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ClusterIamRole")
               )
      Lude.<*> ( x Lude..@? "PendingActions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ElasticIpStatus")
      Lude.<*> (x Lude..@? "ClusterVersion")
      Lude.<*> (x Lude..@? "NodeType")
      Lude.<*> (x Lude..@? "NextMaintenanceWindowStartTime")
      Lude.<*> (x Lude..@? "ClusterCreateTime")
      Lude.<*> (x Lude..@? "Endpoint")
      Lude.<*> (x Lude..@? "AllowVersionUpgrade")
      Lude.<*> (x Lude..@? "ClusterStatus")
      Lude.<*> (x Lude..@? "PendingModifiedValues")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
      Lude.<*> ( x Lude..@? "ClusterNodes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "DBName")
