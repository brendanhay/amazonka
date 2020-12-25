{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cAllowVersionUpgrade,
    cAutomatedSnapshotRetentionPeriod,
    cAvailabilityZone,
    cClusterAvailabilityStatus,
    cClusterCreateTime,
    cClusterIdentifier,
    cClusterNamespaceArn,
    cClusterNodes,
    cClusterParameterGroups,
    cClusterPublicKey,
    cClusterRevisionNumber,
    cClusterSecurityGroups,
    cClusterSnapshotCopyStatus,
    cClusterStatus,
    cClusterSubnetGroupName,
    cClusterVersion,
    cDBName,
    cDataTransferProgress,
    cDeferredMaintenanceWindows,
    cElasticIpStatus,
    cElasticResizeNumberOfNodeOptions,
    cEncrypted,
    cEndpoint,
    cEnhancedVpcRouting,
    cExpectedNextSnapshotScheduleTime,
    cExpectedNextSnapshotScheduleTimeStatus,
    cHsmStatus,
    cIamRoles,
    cKmsKeyId,
    cMaintenanceTrackName,
    cManualSnapshotRetentionPeriod,
    cMasterUsername,
    cModifyStatus,
    cNextMaintenanceWindowStartTime,
    cNodeType,
    cNumberOfNodes,
    cPendingActions,
    cPendingModifiedValues,
    cPreferredMaintenanceWindow,
    cPubliclyAccessible,
    cResizeInfo,
    cRestoreStatus,
    cSnapshotScheduleIdentifier,
    cSnapshotScheduleState,
    cTags,
    cVpcId,
    cVpcSecurityGroups,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.ClusterAvailabilityStatus as Types
import qualified Network.AWS.Redshift.Types.ClusterIamRole as Types
import qualified Network.AWS.Redshift.Types.ClusterIdentifier as Types
import qualified Network.AWS.Redshift.Types.ClusterNamespaceArn as Types
import qualified Network.AWS.Redshift.Types.ClusterNode as Types
import qualified Network.AWS.Redshift.Types.ClusterParameterGroupStatus as Types
import qualified Network.AWS.Redshift.Types.ClusterPublicKey as Types
import qualified Network.AWS.Redshift.Types.ClusterRevisionNumber as Types
import qualified Network.AWS.Redshift.Types.ClusterSecurityGroupMembership as Types
import qualified Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus as Types
import qualified Network.AWS.Redshift.Types.ClusterStatus as Types
import qualified Network.AWS.Redshift.Types.ClusterSubnetGroupName as Types
import qualified Network.AWS.Redshift.Types.DBName as Types
import qualified Network.AWS.Redshift.Types.DataTransferProgress as Types
import qualified Network.AWS.Redshift.Types.DeferredMaintenanceWindow as Types
import qualified Network.AWS.Redshift.Types.ElasticIpStatus as Types
import qualified Network.AWS.Redshift.Types.ElasticResizeNumberOfNodeOptions as Types
import qualified Network.AWS.Redshift.Types.Endpoint as Types
import qualified Network.AWS.Redshift.Types.ExpectedNextSnapshotScheduleTimeStatus as Types
import qualified Network.AWS.Redshift.Types.HsmStatus as Types
import qualified Network.AWS.Redshift.Types.KmsKeyId as Types
import qualified Network.AWS.Redshift.Types.MaintenanceTrackName as Types
import qualified Network.AWS.Redshift.Types.MasterUsername as Types
import qualified Network.AWS.Redshift.Types.ModifyStatus as Types
import qualified Network.AWS.Redshift.Types.NodeType as Types
import qualified Network.AWS.Redshift.Types.PendingModifiedValues as Types
import qualified Network.AWS.Redshift.Types.PreferredMaintenanceWindow as Types
import qualified Network.AWS.Redshift.Types.ResizeInfo as Types
import qualified Network.AWS.Redshift.Types.RestoreStatus as Types
import qualified Network.AWS.Redshift.Types.ScheduleState as Types
import qualified Network.AWS.Redshift.Types.SnapshotScheduleIdentifier as Types
import qualified Network.AWS.Redshift.Types.String as Types
import qualified Network.AWS.Redshift.Types.Tag as Types
import qualified Network.AWS.Redshift.Types.VpcId as Types
import qualified Network.AWS.Redshift.Types.VpcSecurityGroupMembership as Types

-- | Describes a cluster.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { -- | A boolean value that, if @true@ , indicates that major version upgrades will be applied automatically to the cluster during the maintenance window.
    allowVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The number of days that automatic cluster snapshots are retained.
    automatedSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The name of the Availability Zone in which the cluster is located.
    availabilityZone :: Core.Maybe Types.String,
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
    clusterAvailabilityStatus :: Core.Maybe Types.ClusterAvailabilityStatus,
    -- | The date and time that the cluster was created.
    clusterCreateTime :: Core.Maybe Core.UTCTime,
    -- | The unique identifier of the cluster.
    clusterIdentifier :: Core.Maybe Types.ClusterIdentifier,
    -- | The namespace Amazon Resource Name (ARN) of the cluster.
    clusterNamespaceArn :: Core.Maybe Types.ClusterNamespaceArn,
    -- | The nodes in the cluster.
    clusterNodes :: Core.Maybe [Types.ClusterNode],
    -- | The list of cluster parameter groups that are associated with this cluster. Each parameter group in the list is returned with its status.
    clusterParameterGroups :: Core.Maybe [Types.ClusterParameterGroupStatus],
    -- | The public key for the cluster.
    clusterPublicKey :: Core.Maybe Types.ClusterPublicKey,
    -- | The specific revision number of the database in the cluster.
    clusterRevisionNumber :: Core.Maybe Types.ClusterRevisionNumber,
    -- | A list of cluster security group that are associated with the cluster. Each security group is represented by an element that contains @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@ subelements.
    --
    -- Cluster security groups are used when the cluster is not created in an Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC use VPC security groups, which are listed by the __VpcSecurityGroups__ parameter.
    clusterSecurityGroups :: Core.Maybe [Types.ClusterSecurityGroupMembership],
    -- | A value that returns the destination region and retention period that are configured for cross-region snapshot copy.
    clusterSnapshotCopyStatus :: Core.Maybe Types.ClusterSnapshotCopyStatus,
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
    clusterStatus :: Core.Maybe Types.ClusterStatus,
    -- | The name of the subnet group that is associated with the cluster. This parameter is valid only when the cluster is in a VPC.
    clusterSubnetGroupName :: Core.Maybe Types.ClusterSubnetGroupName,
    -- | The version ID of the Amazon Redshift engine that is running on the cluster.
    clusterVersion :: Core.Maybe Types.String,
    -- | The name of the initial database that was created when the cluster was created. This same name is returned for the life of the cluster. If an initial database was not specified, a database named @dev@ dev was created by default.
    dBName :: Core.Maybe Types.DBName,
    -- |
    dataTransferProgress :: Core.Maybe Types.DataTransferProgress,
    -- | Describes a group of @DeferredMaintenanceWindow@ objects.
    deferredMaintenanceWindows :: Core.Maybe [Types.DeferredMaintenanceWindow],
    -- | The status of the elastic IP (EIP) address.
    elasticIpStatus :: Core.Maybe Types.ElasticIpStatus,
    -- | The number of nodes that you can resize the cluster to with the elastic resize method.
    elasticResizeNumberOfNodeOptions :: Core.Maybe Types.ElasticResizeNumberOfNodeOptions,
    -- | A boolean value that, if @true@ , indicates that data in the cluster is encrypted at rest.
    encrypted :: Core.Maybe Core.Bool,
    -- | The connection endpoint.
    endpoint :: Core.Maybe Types.Endpoint,
    -- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@ , enhanced VPC routing is enabled.
    -- Default: false
    enhancedVpcRouting :: Core.Maybe Core.Bool,
    -- | The date and time when the next snapshot is expected to be taken for clusters with a valid snapshot schedule and backups enabled.
    expectedNextSnapshotScheduleTime :: Core.Maybe Core.UTCTime,
    -- | The status of next expected snapshot for clusters having a valid snapshot schedule and backups enabled. Possible values are the following:
    --
    --
    --     * OnTrack - The next snapshot is expected to be taken on time.
    --
    --
    --     * Pending - The next snapshot is pending to be taken.
    expectedNextSnapshotScheduleTimeStatus :: Core.Maybe Types.ExpectedNextSnapshotScheduleTimeStatus,
    -- | A value that reports whether the Amazon Redshift cluster has finished applying any hardware security module (HSM) settings changes specified in a modify cluster command.
    --
    -- Values: active, applying
    hsmStatus :: Core.Maybe Types.HsmStatus,
    -- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
    iamRoles :: Core.Maybe [Types.ClusterIamRole],
    -- | The AWS Key Management Service (AWS KMS) key ID of the encryption key used to encrypt data in the cluster.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The name of the maintenance track for the cluster.
    maintenanceTrackName :: Core.Maybe Types.MaintenanceTrackName,
    -- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The master user name for the cluster. This name is used to connect to the database that is specified in the __DBName__ parameter.
    masterUsername :: Core.Maybe Types.MasterUsername,
    -- | The status of a modify operation, if any, initiated for the cluster.
    modifyStatus :: Core.Maybe Types.ModifyStatus,
    -- | The date and time in UTC when system maintenance can begin.
    nextMaintenanceWindowStartTime :: Core.Maybe Core.UTCTime,
    -- | The node type for the nodes in the cluster.
    nodeType :: Core.Maybe Types.NodeType,
    -- | The number of compute nodes in the cluster.
    numberOfNodes :: Core.Maybe Core.Int,
    -- | Cluster operations that are waiting to be started.
    pendingActions :: Core.Maybe [Types.String],
    -- | A value that, if present, indicates that changes to the cluster are pending. Specific pending changes are identified by subelements.
    pendingModifiedValues :: Core.Maybe Types.PendingModifiedValues,
    -- | The weekly time range, in Universal Coordinated Time (UTC), during which system maintenance can occur.
    preferredMaintenanceWindow :: Core.Maybe Types.PreferredMaintenanceWindow,
    -- | A boolean value that, if @true@ , indicates that the cluster can be accessed from a public network.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | Returns the following:
    --
    --
    --     * AllowCancelResize: a boolean value indicating if the resize operation can be cancelled.
    --
    --
    --     * ResizeType: Returns ClassicResize
    resizeInfo :: Core.Maybe Types.ResizeInfo,
    -- | A value that describes the status of a cluster restore action. This parameter returns null if the cluster was not created by restoring a snapshot.
    restoreStatus :: Core.Maybe Types.RestoreStatus,
    -- | A unique identifier for the cluster snapshot schedule.
    snapshotScheduleIdentifier :: Core.Maybe Types.SnapshotScheduleIdentifier,
    -- | The current state of the cluster snapshot schedule.
    snapshotScheduleState :: Core.Maybe Types.ScheduleState,
    -- | The list of tags for the cluster.
    tags :: Core.Maybe [Types.Tag],
    -- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
    vpcId :: Core.Maybe Types.VpcId,
    -- | A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that are associated with the cluster. This parameter is returned only if the cluster is in a VPC.
    vpcSecurityGroups :: Core.Maybe [Types.VpcSecurityGroupMembership]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Cluster' value with any optional fields omitted.
mkCluster ::
  Cluster
mkCluster =
  Cluster'
    { allowVersionUpgrade = Core.Nothing,
      automatedSnapshotRetentionPeriod = Core.Nothing,
      availabilityZone = Core.Nothing,
      clusterAvailabilityStatus = Core.Nothing,
      clusterCreateTime = Core.Nothing,
      clusterIdentifier = Core.Nothing,
      clusterNamespaceArn = Core.Nothing,
      clusterNodes = Core.Nothing,
      clusterParameterGroups = Core.Nothing,
      clusterPublicKey = Core.Nothing,
      clusterRevisionNumber = Core.Nothing,
      clusterSecurityGroups = Core.Nothing,
      clusterSnapshotCopyStatus = Core.Nothing,
      clusterStatus = Core.Nothing,
      clusterSubnetGroupName = Core.Nothing,
      clusterVersion = Core.Nothing,
      dBName = Core.Nothing,
      dataTransferProgress = Core.Nothing,
      deferredMaintenanceWindows = Core.Nothing,
      elasticIpStatus = Core.Nothing,
      elasticResizeNumberOfNodeOptions = Core.Nothing,
      encrypted = Core.Nothing,
      endpoint = Core.Nothing,
      enhancedVpcRouting = Core.Nothing,
      expectedNextSnapshotScheduleTime = Core.Nothing,
      expectedNextSnapshotScheduleTimeStatus = Core.Nothing,
      hsmStatus = Core.Nothing,
      iamRoles = Core.Nothing,
      kmsKeyId = Core.Nothing,
      maintenanceTrackName = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing,
      masterUsername = Core.Nothing,
      modifyStatus = Core.Nothing,
      nextMaintenanceWindowStartTime = Core.Nothing,
      nodeType = Core.Nothing,
      numberOfNodes = Core.Nothing,
      pendingActions = Core.Nothing,
      pendingModifiedValues = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      resizeInfo = Core.Nothing,
      restoreStatus = Core.Nothing,
      snapshotScheduleIdentifier = Core.Nothing,
      snapshotScheduleState = Core.Nothing,
      tags = Core.Nothing,
      vpcId = Core.Nothing,
      vpcSecurityGroups = Core.Nothing
    }

-- | A boolean value that, if @true@ , indicates that major version upgrades will be applied automatically to the cluster during the maintenance window.
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAllowVersionUpgrade :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cAllowVersionUpgrade = Lens.field @"allowVersionUpgrade"
{-# DEPRECATED cAllowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead." #-}

-- | The number of days that automatic cluster snapshots are retained.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAutomatedSnapshotRetentionPeriod :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cAutomatedSnapshotRetentionPeriod = Lens.field @"automatedSnapshotRetentionPeriod"
{-# DEPRECATED cAutomatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead." #-}

-- | The name of the Availability Zone in which the cluster is located.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAvailabilityZone :: Lens.Lens' Cluster (Core.Maybe Types.String)
cAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED cAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

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
cClusterAvailabilityStatus :: Lens.Lens' Cluster (Core.Maybe Types.ClusterAvailabilityStatus)
cClusterAvailabilityStatus = Lens.field @"clusterAvailabilityStatus"
{-# DEPRECATED cClusterAvailabilityStatus "Use generic-lens or generic-optics with 'clusterAvailabilityStatus' instead." #-}

-- | The date and time that the cluster was created.
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterCreateTime :: Lens.Lens' Cluster (Core.Maybe Core.UTCTime)
cClusterCreateTime = Lens.field @"clusterCreateTime"
{-# DEPRECATED cClusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead." #-}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterIdentifier :: Lens.Lens' Cluster (Core.Maybe Types.ClusterIdentifier)
cClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED cClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The namespace Amazon Resource Name (ARN) of the cluster.
--
-- /Note:/ Consider using 'clusterNamespaceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterNamespaceArn :: Lens.Lens' Cluster (Core.Maybe Types.ClusterNamespaceArn)
cClusterNamespaceArn = Lens.field @"clusterNamespaceArn"
{-# DEPRECATED cClusterNamespaceArn "Use generic-lens or generic-optics with 'clusterNamespaceArn' instead." #-}

-- | The nodes in the cluster.
--
-- /Note:/ Consider using 'clusterNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterNodes :: Lens.Lens' Cluster (Core.Maybe [Types.ClusterNode])
cClusterNodes = Lens.field @"clusterNodes"
{-# DEPRECATED cClusterNodes "Use generic-lens or generic-optics with 'clusterNodes' instead." #-}

-- | The list of cluster parameter groups that are associated with this cluster. Each parameter group in the list is returned with its status.
--
-- /Note:/ Consider using 'clusterParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterParameterGroups :: Lens.Lens' Cluster (Core.Maybe [Types.ClusterParameterGroupStatus])
cClusterParameterGroups = Lens.field @"clusterParameterGroups"
{-# DEPRECATED cClusterParameterGroups "Use generic-lens or generic-optics with 'clusterParameterGroups' instead." #-}

-- | The public key for the cluster.
--
-- /Note:/ Consider using 'clusterPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterPublicKey :: Lens.Lens' Cluster (Core.Maybe Types.ClusterPublicKey)
cClusterPublicKey = Lens.field @"clusterPublicKey"
{-# DEPRECATED cClusterPublicKey "Use generic-lens or generic-optics with 'clusterPublicKey' instead." #-}

-- | The specific revision number of the database in the cluster.
--
-- /Note:/ Consider using 'clusterRevisionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterRevisionNumber :: Lens.Lens' Cluster (Core.Maybe Types.ClusterRevisionNumber)
cClusterRevisionNumber = Lens.field @"clusterRevisionNumber"
{-# DEPRECATED cClusterRevisionNumber "Use generic-lens or generic-optics with 'clusterRevisionNumber' instead." #-}

-- | A list of cluster security group that are associated with the cluster. Each security group is represented by an element that contains @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@ subelements.
--
-- Cluster security groups are used when the cluster is not created in an Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC use VPC security groups, which are listed by the __VpcSecurityGroups__ parameter.
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterSecurityGroups :: Lens.Lens' Cluster (Core.Maybe [Types.ClusterSecurityGroupMembership])
cClusterSecurityGroups = Lens.field @"clusterSecurityGroups"
{-# DEPRECATED cClusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead." #-}

-- | A value that returns the destination region and retention period that are configured for cross-region snapshot copy.
--
-- /Note:/ Consider using 'clusterSnapshotCopyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterSnapshotCopyStatus :: Lens.Lens' Cluster (Core.Maybe Types.ClusterSnapshotCopyStatus)
cClusterSnapshotCopyStatus = Lens.field @"clusterSnapshotCopyStatus"
{-# DEPRECATED cClusterSnapshotCopyStatus "Use generic-lens or generic-optics with 'clusterSnapshotCopyStatus' instead." #-}

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
cClusterStatus :: Lens.Lens' Cluster (Core.Maybe Types.ClusterStatus)
cClusterStatus = Lens.field @"clusterStatus"
{-# DEPRECATED cClusterStatus "Use generic-lens or generic-optics with 'clusterStatus' instead." #-}

-- | The name of the subnet group that is associated with the cluster. This parameter is valid only when the cluster is in a VPC.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterSubnetGroupName :: Lens.Lens' Cluster (Core.Maybe Types.ClusterSubnetGroupName)
cClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# DEPRECATED cClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterVersion :: Lens.Lens' Cluster (Core.Maybe Types.String)
cClusterVersion = Lens.field @"clusterVersion"
{-# DEPRECATED cClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | The name of the initial database that was created when the cluster was created. This same name is returned for the life of the cluster. If an initial database was not specified, a database named @dev@ dev was created by default.
--
-- /Note:/ Consider using 'dBName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDBName :: Lens.Lens' Cluster (Core.Maybe Types.DBName)
cDBName = Lens.field @"dBName"
{-# DEPRECATED cDBName "Use generic-lens or generic-optics with 'dBName' instead." #-}

-- |
--
-- /Note:/ Consider using 'dataTransferProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDataTransferProgress :: Lens.Lens' Cluster (Core.Maybe Types.DataTransferProgress)
cDataTransferProgress = Lens.field @"dataTransferProgress"
{-# DEPRECATED cDataTransferProgress "Use generic-lens or generic-optics with 'dataTransferProgress' instead." #-}

-- | Describes a group of @DeferredMaintenanceWindow@ objects.
--
-- /Note:/ Consider using 'deferredMaintenanceWindows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeferredMaintenanceWindows :: Lens.Lens' Cluster (Core.Maybe [Types.DeferredMaintenanceWindow])
cDeferredMaintenanceWindows = Lens.field @"deferredMaintenanceWindows"
{-# DEPRECATED cDeferredMaintenanceWindows "Use generic-lens or generic-optics with 'deferredMaintenanceWindows' instead." #-}

-- | The status of the elastic IP (EIP) address.
--
-- /Note:/ Consider using 'elasticIpStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cElasticIpStatus :: Lens.Lens' Cluster (Core.Maybe Types.ElasticIpStatus)
cElasticIpStatus = Lens.field @"elasticIpStatus"
{-# DEPRECATED cElasticIpStatus "Use generic-lens or generic-optics with 'elasticIpStatus' instead." #-}

-- | The number of nodes that you can resize the cluster to with the elastic resize method.
--
-- /Note:/ Consider using 'elasticResizeNumberOfNodeOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cElasticResizeNumberOfNodeOptions :: Lens.Lens' Cluster (Core.Maybe Types.ElasticResizeNumberOfNodeOptions)
cElasticResizeNumberOfNodeOptions = Lens.field @"elasticResizeNumberOfNodeOptions"
{-# DEPRECATED cElasticResizeNumberOfNodeOptions "Use generic-lens or generic-optics with 'elasticResizeNumberOfNodeOptions' instead." #-}

-- | A boolean value that, if @true@ , indicates that data in the cluster is encrypted at rest.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEncrypted :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cEncrypted = Lens.field @"encrypted"
{-# DEPRECATED cEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The connection endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEndpoint :: Lens.Lens' Cluster (Core.Maybe Types.Endpoint)
cEndpoint = Lens.field @"endpoint"
{-# DEPRECATED cEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
--
-- /Note:/ Consider using 'enhancedVpcRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnhancedVpcRouting :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cEnhancedVpcRouting = Lens.field @"enhancedVpcRouting"
{-# DEPRECATED cEnhancedVpcRouting "Use generic-lens or generic-optics with 'enhancedVpcRouting' instead." #-}

-- | The date and time when the next snapshot is expected to be taken for clusters with a valid snapshot schedule and backups enabled.
--
-- /Note:/ Consider using 'expectedNextSnapshotScheduleTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpectedNextSnapshotScheduleTime :: Lens.Lens' Cluster (Core.Maybe Core.UTCTime)
cExpectedNextSnapshotScheduleTime = Lens.field @"expectedNextSnapshotScheduleTime"
{-# DEPRECATED cExpectedNextSnapshotScheduleTime "Use generic-lens or generic-optics with 'expectedNextSnapshotScheduleTime' instead." #-}

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
cExpectedNextSnapshotScheduleTimeStatus :: Lens.Lens' Cluster (Core.Maybe Types.ExpectedNextSnapshotScheduleTimeStatus)
cExpectedNextSnapshotScheduleTimeStatus = Lens.field @"expectedNextSnapshotScheduleTimeStatus"
{-# DEPRECATED cExpectedNextSnapshotScheduleTimeStatus "Use generic-lens or generic-optics with 'expectedNextSnapshotScheduleTimeStatus' instead." #-}

-- | A value that reports whether the Amazon Redshift cluster has finished applying any hardware security module (HSM) settings changes specified in a modify cluster command.
--
-- Values: active, applying
--
-- /Note:/ Consider using 'hsmStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHsmStatus :: Lens.Lens' Cluster (Core.Maybe Types.HsmStatus)
cHsmStatus = Lens.field @"hsmStatus"
{-# DEPRECATED cHsmStatus "Use generic-lens or generic-optics with 'hsmStatus' instead." #-}

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
--
-- /Note:/ Consider using 'iamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIamRoles :: Lens.Lens' Cluster (Core.Maybe [Types.ClusterIamRole])
cIamRoles = Lens.field @"iamRoles"
{-# DEPRECATED cIamRoles "Use generic-lens or generic-optics with 'iamRoles' instead." #-}

-- | The AWS Key Management Service (AWS KMS) key ID of the encryption key used to encrypt data in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKmsKeyId :: Lens.Lens' Cluster (Core.Maybe Types.KmsKeyId)
cKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED cKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the maintenance track for the cluster.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMaintenanceTrackName :: Lens.Lens' Cluster (Core.Maybe Types.MaintenanceTrackName)
cMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# DEPRECATED cMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cManualSnapshotRetentionPeriod :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# DEPRECATED cManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | The master user name for the cluster. This name is used to connect to the database that is specified in the __DBName__ parameter.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMasterUsername :: Lens.Lens' Cluster (Core.Maybe Types.MasterUsername)
cMasterUsername = Lens.field @"masterUsername"
{-# DEPRECATED cMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The status of a modify operation, if any, initiated for the cluster.
--
-- /Note:/ Consider using 'modifyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cModifyStatus :: Lens.Lens' Cluster (Core.Maybe Types.ModifyStatus)
cModifyStatus = Lens.field @"modifyStatus"
{-# DEPRECATED cModifyStatus "Use generic-lens or generic-optics with 'modifyStatus' instead." #-}

-- | The date and time in UTC when system maintenance can begin.
--
-- /Note:/ Consider using 'nextMaintenanceWindowStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNextMaintenanceWindowStartTime :: Lens.Lens' Cluster (Core.Maybe Core.UTCTime)
cNextMaintenanceWindowStartTime = Lens.field @"nextMaintenanceWindowStartTime"
{-# DEPRECATED cNextMaintenanceWindowStartTime "Use generic-lens or generic-optics with 'nextMaintenanceWindowStartTime' instead." #-}

-- | The node type for the nodes in the cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodeType :: Lens.Lens' Cluster (Core.Maybe Types.NodeType)
cNodeType = Lens.field @"nodeType"
{-# DEPRECATED cNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The number of compute nodes in the cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNumberOfNodes :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cNumberOfNodes = Lens.field @"numberOfNodes"
{-# DEPRECATED cNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | Cluster operations that are waiting to be started.
--
-- /Note:/ Consider using 'pendingActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPendingActions :: Lens.Lens' Cluster (Core.Maybe [Types.String])
cPendingActions = Lens.field @"pendingActions"
{-# DEPRECATED cPendingActions "Use generic-lens or generic-optics with 'pendingActions' instead." #-}

-- | A value that, if present, indicates that changes to the cluster are pending. Specific pending changes are identified by subelements.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPendingModifiedValues :: Lens.Lens' Cluster (Core.Maybe Types.PendingModifiedValues)
cPendingModifiedValues = Lens.field @"pendingModifiedValues"
{-# DEPRECATED cPendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead." #-}

-- | The weekly time range, in Universal Coordinated Time (UTC), during which system maintenance can occur.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPreferredMaintenanceWindow :: Lens.Lens' Cluster (Core.Maybe Types.PreferredMaintenanceWindow)
cPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED cPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | A boolean value that, if @true@ , indicates that the cluster can be accessed from a public network.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPubliclyAccessible :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# DEPRECATED cPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

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
cResizeInfo :: Lens.Lens' Cluster (Core.Maybe Types.ResizeInfo)
cResizeInfo = Lens.field @"resizeInfo"
{-# DEPRECATED cResizeInfo "Use generic-lens or generic-optics with 'resizeInfo' instead." #-}

-- | A value that describes the status of a cluster restore action. This parameter returns null if the cluster was not created by restoring a snapshot.
--
-- /Note:/ Consider using 'restoreStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRestoreStatus :: Lens.Lens' Cluster (Core.Maybe Types.RestoreStatus)
cRestoreStatus = Lens.field @"restoreStatus"
{-# DEPRECATED cRestoreStatus "Use generic-lens or generic-optics with 'restoreStatus' instead." #-}

-- | A unique identifier for the cluster snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSnapshotScheduleIdentifier :: Lens.Lens' Cluster (Core.Maybe Types.SnapshotScheduleIdentifier)
cSnapshotScheduleIdentifier = Lens.field @"snapshotScheduleIdentifier"
{-# DEPRECATED cSnapshotScheduleIdentifier "Use generic-lens or generic-optics with 'snapshotScheduleIdentifier' instead." #-}

-- | The current state of the cluster snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSnapshotScheduleState :: Lens.Lens' Cluster (Core.Maybe Types.ScheduleState)
cSnapshotScheduleState = Lens.field @"snapshotScheduleState"
{-# DEPRECATED cSnapshotScheduleState "Use generic-lens or generic-optics with 'snapshotScheduleState' instead." #-}

-- | The list of tags for the cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Cluster (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVpcId :: Lens.Lens' Cluster (Core.Maybe Types.VpcId)
cVpcId = Lens.field @"vpcId"
{-# DEPRECATED cVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that are associated with the cluster. This parameter is returned only if the cluster is in a VPC.
--
-- /Note:/ Consider using 'vpcSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVpcSecurityGroups :: Lens.Lens' Cluster (Core.Maybe [Types.VpcSecurityGroupMembership])
cVpcSecurityGroups = Lens.field @"vpcSecurityGroups"
{-# DEPRECATED cVpcSecurityGroups "Use generic-lens or generic-optics with 'vpcSecurityGroups' instead." #-}

instance Core.FromXML Cluster where
  parseXML x =
    Cluster'
      Core.<$> (x Core..@? "AllowVersionUpgrade")
      Core.<*> (x Core..@? "AutomatedSnapshotRetentionPeriod")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> (x Core..@? "ClusterAvailabilityStatus")
      Core.<*> (x Core..@? "ClusterCreateTime")
      Core.<*> (x Core..@? "ClusterIdentifier")
      Core.<*> (x Core..@? "ClusterNamespaceArn")
      Core.<*> (x Core..@? "ClusterNodes" Core..<@> Core.parseXMLList "member")
      Core.<*> ( x Core..@? "ClusterParameterGroups"
                   Core..<@> Core.parseXMLList "ClusterParameterGroup"
               )
      Core.<*> (x Core..@? "ClusterPublicKey")
      Core.<*> (x Core..@? "ClusterRevisionNumber")
      Core.<*> ( x Core..@? "ClusterSecurityGroups"
                   Core..<@> Core.parseXMLList "ClusterSecurityGroup"
               )
      Core.<*> (x Core..@? "ClusterSnapshotCopyStatus")
      Core.<*> (x Core..@? "ClusterStatus")
      Core.<*> (x Core..@? "ClusterSubnetGroupName")
      Core.<*> (x Core..@? "ClusterVersion")
      Core.<*> (x Core..@? "DBName")
      Core.<*> (x Core..@? "DataTransferProgress")
      Core.<*> ( x Core..@? "DeferredMaintenanceWindows"
                   Core..<@> Core.parseXMLList "DeferredMaintenanceWindow"
               )
      Core.<*> (x Core..@? "ElasticIpStatus")
      Core.<*> (x Core..@? "ElasticResizeNumberOfNodeOptions")
      Core.<*> (x Core..@? "Encrypted")
      Core.<*> (x Core..@? "Endpoint")
      Core.<*> (x Core..@? "EnhancedVpcRouting")
      Core.<*> (x Core..@? "ExpectedNextSnapshotScheduleTime")
      Core.<*> (x Core..@? "ExpectedNextSnapshotScheduleTimeStatus")
      Core.<*> (x Core..@? "HsmStatus")
      Core.<*> ( x Core..@? "IamRoles"
                   Core..<@> Core.parseXMLList "ClusterIamRole"
               )
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "MaintenanceTrackName")
      Core.<*> (x Core..@? "ManualSnapshotRetentionPeriod")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "ModifyStatus")
      Core.<*> (x Core..@? "NextMaintenanceWindowStartTime")
      Core.<*> (x Core..@? "NodeType")
      Core.<*> (x Core..@? "NumberOfNodes")
      Core.<*> (x Core..@? "PendingActions" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "PendingModifiedValues")
      Core.<*> (x Core..@? "PreferredMaintenanceWindow")
      Core.<*> (x Core..@? "PubliclyAccessible")
      Core.<*> (x Core..@? "ResizeInfo")
      Core.<*> (x Core..@? "RestoreStatus")
      Core.<*> (x Core..@? "SnapshotScheduleIdentifier")
      Core.<*> (x Core..@? "SnapshotScheduleState")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag")
      Core.<*> (x Core..@? "VpcId")
      Core.<*> ( x Core..@? "VpcSecurityGroups"
                   Core..<@> Core.parseXMLList "VpcSecurityGroup"
               )
