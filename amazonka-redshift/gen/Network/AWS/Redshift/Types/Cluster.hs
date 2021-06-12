{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Cluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Cluster where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ClusterIamRole
import Network.AWS.Redshift.Types.ClusterNode
import Network.AWS.Redshift.Types.ClusterParameterGroupStatus
import Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
import Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
import Network.AWS.Redshift.Types.DataTransferProgress
import Network.AWS.Redshift.Types.DeferredMaintenanceWindow
import Network.AWS.Redshift.Types.ElasticIpStatus
import Network.AWS.Redshift.Types.Endpoint
import Network.AWS.Redshift.Types.HsmStatus
import Network.AWS.Redshift.Types.PendingModifiedValues
import Network.AWS.Redshift.Types.ResizeInfo
import Network.AWS.Redshift.Types.RestoreStatus
import Network.AWS.Redshift.Types.ScheduleState
import Network.AWS.Redshift.Types.Tag
import Network.AWS.Redshift.Types.VpcSecurityGroupMembership

-- | Describes a cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | An option that specifies whether to create the cluster with enhanced VPC
    -- routing enabled. To create a cluster that uses enhanced VPC routing, the
    -- cluster must be in a VPC. For more information, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
    -- in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@, enhanced VPC routing is enabled.
    --
    -- Default: false
    enhancedVpcRouting :: Core.Maybe Core.Bool,
    -- | A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that
    -- are associated with the cluster. This parameter is returned only if the
    -- cluster is in a VPC.
    vpcSecurityGroups :: Core.Maybe [VpcSecurityGroupMembership],
    -- | Returns the following:
    --
    -- -   AllowCancelResize: a boolean value indicating if the resize
    --     operation can be cancelled.
    --
    -- -   ResizeType: Returns ClassicResize
    resizeInfo :: Core.Maybe ResizeInfo,
    -- | The namespace Amazon Resource Name (ARN) of the cluster.
    clusterNamespaceArn :: Core.Maybe Core.Text,
    -- | The name of the subnet group that is associated with the cluster. This
    -- parameter is valid only when the cluster is in a VPC.
    clusterSubnetGroupName :: Core.Maybe Core.Text,
    -- | Describes a group of @DeferredMaintenanceWindow@ objects.
    deferredMaintenanceWindows :: Core.Maybe [DeferredMaintenanceWindow],
    -- | The status of next expected snapshot for clusters having a valid
    -- snapshot schedule and backups enabled. Possible values are the
    -- following:
    --
    -- -   OnTrack - The next snapshot is expected to be taken on time.
    --
    -- -   Pending - The next snapshot is pending to be taken.
    expectedNextSnapshotScheduleTimeStatus :: Core.Maybe Core.Text,
    -- | The current state of the cluster snapshot schedule.
    snapshotScheduleState :: Core.Maybe ScheduleState,
    -- | A boolean value that, if @true@, indicates that data in the cluster is
    -- encrypted at rest.
    encrypted :: Core.Maybe Core.Bool,
    -- | A boolean value that, if @true@, indicates that major version upgrades
    -- will be applied automatically to the cluster during the maintenance
    -- window.
    allowVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The date and time that the cluster was created.
    clusterCreateTime :: Core.Maybe Core.ISO8601,
    -- | The number of days that automatic cluster snapshots are retained.
    automatedSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The status of the elastic IP (EIP) address.
    elasticIpStatus :: Core.Maybe ElasticIpStatus,
    -- | A value that reports whether the Amazon Redshift cluster has finished
    -- applying any hardware security module (HSM) settings changes specified
    -- in a modify cluster command.
    --
    -- Values: active, applying
    hsmStatus :: Core.Maybe HsmStatus,
    -- | A unique identifier for the cluster snapshot schedule.
    snapshotScheduleIdentifier :: Core.Maybe Core.Text,
    -- | The master user name for the cluster. This name is used to connect to
    -- the database that is specified in the __DBName__ parameter.
    masterUsername :: Core.Maybe Core.Text,
    -- | A boolean value that, if @true@, indicates that the cluster can be
    -- accessed from a public network.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | The availability status of the cluster for queries. Possible values are
    -- the following:
    --
    -- -   Available - The cluster is available for queries.
    --
    -- -   Unavailable - The cluster is not available for queries.
    --
    -- -   Maintenance - The cluster is intermittently available for queries
    --     due to maintenance activities.
    --
    -- -   Modifying - The cluster is intermittently available for queries due
    --     to changes that modify the cluster.
    --
    -- -   Failed - The cluster failed and is not available for queries.
    clusterAvailabilityStatus :: Core.Maybe Core.Text,
    -- | The default number of days to retain a manual snapshot. If the value is
    -- -1, the snapshot is retained indefinitely. This setting doesn\'t change
    -- the retention period of existing snapshots.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The AWS Key Management Service (AWS KMS) key ID of the encryption key
    -- used to encrypt data in the cluster.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | A value that returns the destination region and retention period that
    -- are configured for cross-region snapshot copy.
    clusterSnapshotCopyStatus :: Core.Maybe ClusterSnapshotCopyStatus,
    -- | The name of the Availability Zone in which the cluster is located.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The list of cluster parameter groups that are associated with this
    -- cluster. Each parameter group in the list is returned with its status.
    clusterParameterGroups :: Core.Maybe [ClusterParameterGroupStatus],
    -- | The weekly time range, in Universal Coordinated Time (UTC), during which
    -- system maintenance can occur.
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | The public key for the cluster.
    clusterPublicKey :: Core.Maybe Core.Text,
    -- | A value that describes the status of a cluster restore action. This
    -- parameter returns null if the cluster was not created by restoring a
    -- snapshot.
    restoreStatus :: Core.Maybe RestoreStatus,
    -- | The status of a modify operation, if any, initiated for the cluster.
    modifyStatus :: Core.Maybe Core.Text,
    -- | The unique identifier of the cluster.
    clusterIdentifier :: Core.Maybe Core.Text,
    -- | The list of tags for the cluster.
    tags :: Core.Maybe [Tag],
    -- | The nodes in the cluster.
    clusterNodes :: Core.Maybe [ClusterNode],
    -- | The number of compute nodes in the cluster.
    numberOfNodes :: Core.Maybe Core.Int,
    -- | Describes the status of the Availability Zone relocation operation.
    availabilityZoneRelocationStatus :: Core.Maybe Core.Text,
    -- | The name of the initial database that was created when the cluster was
    -- created. This same name is returned for the life of the cluster. If an
    -- initial database was not specified, a database named @dev@dev was
    -- created by default.
    dbName :: Core.Maybe Core.Text,
    dataTransferProgress :: Core.Maybe DataTransferProgress,
    -- | The current state of the cluster. Possible values are the following:
    --
    -- -   @available@
    --
    -- -   @available, prep-for-resize@
    --
    -- -   @available, resize-cleanup@
    --
    -- -   @cancelling-resize@
    --
    -- -   @creating@
    --
    -- -   @deleting@
    --
    -- -   @final-snapshot@
    --
    -- -   @hardware-failure@
    --
    -- -   @incompatible-hsm@
    --
    -- -   @incompatible-network@
    --
    -- -   @incompatible-parameters@
    --
    -- -   @incompatible-restore@
    --
    -- -   @modifying@
    --
    -- -   @paused@
    --
    -- -   @rebooting@
    --
    -- -   @renaming@
    --
    -- -   @resizing@
    --
    -- -   @rotating-keys@
    --
    -- -   @storage-full@
    --
    -- -   @updating-hsm@
    clusterStatus :: Core.Maybe Core.Text,
    -- | A value that, if present, indicates that changes to the cluster are
    -- pending. Specific pending changes are identified by subelements.
    pendingModifiedValues :: Core.Maybe PendingModifiedValues,
    -- | The connection endpoint.
    endpoint :: Core.Maybe Endpoint,
    -- | The date and time in UTC when system maintenance can begin.
    nextMaintenanceWindowStartTime :: Core.Maybe Core.ISO8601,
    -- | The node type for the nodes in the cluster.
    nodeType :: Core.Maybe Core.Text,
    -- | The version ID of the Amazon Redshift engine that is running on the
    -- cluster.
    clusterVersion :: Core.Maybe Core.Text,
    -- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
    vpcId :: Core.Maybe Core.Text,
    -- | A list of cluster security group that are associated with the cluster.
    -- Each security group is represented by an element that contains
    -- @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@
    -- subelements.
    --
    -- Cluster security groups are used when the cluster is not created in an
    -- Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC
    -- use VPC security groups, which are listed by the __VpcSecurityGroups__
    -- parameter.
    clusterSecurityGroups :: Core.Maybe [ClusterSecurityGroupMembership],
    -- | The number of nodes that you can resize the cluster to with the elastic
    -- resize method.
    elasticResizeNumberOfNodeOptions :: Core.Maybe Core.Text,
    -- | The date and time when the next snapshot is expected to be taken for
    -- clusters with a valid snapshot schedule and backups enabled.
    expectedNextSnapshotScheduleTime :: Core.Maybe Core.ISO8601,
    -- | The name of the maintenance track for the cluster.
    maintenanceTrackName :: Core.Maybe Core.Text,
    -- | The specific revision number of the database in the cluster.
    clusterRevisionNumber :: Core.Maybe Core.Text,
    -- | A list of AWS Identity and Access Management (IAM) roles that can be
    -- used by the cluster to access other AWS services.
    iamRoles :: Core.Maybe [ClusterIamRole],
    -- | Cluster operations that are waiting to be started.
    pendingActions :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enhancedVpcRouting', 'cluster_enhancedVpcRouting' - An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
--
-- 'vpcSecurityGroups', 'cluster_vpcSecurityGroups' - A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that
-- are associated with the cluster. This parameter is returned only if the
-- cluster is in a VPC.
--
-- 'resizeInfo', 'cluster_resizeInfo' - Returns the following:
--
-- -   AllowCancelResize: a boolean value indicating if the resize
--     operation can be cancelled.
--
-- -   ResizeType: Returns ClassicResize
--
-- 'clusterNamespaceArn', 'cluster_clusterNamespaceArn' - The namespace Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterSubnetGroupName', 'cluster_clusterSubnetGroupName' - The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
--
-- 'deferredMaintenanceWindows', 'cluster_deferredMaintenanceWindows' - Describes a group of @DeferredMaintenanceWindow@ objects.
--
-- 'expectedNextSnapshotScheduleTimeStatus', 'cluster_expectedNextSnapshotScheduleTimeStatus' - The status of next expected snapshot for clusters having a valid
-- snapshot schedule and backups enabled. Possible values are the
-- following:
--
-- -   OnTrack - The next snapshot is expected to be taken on time.
--
-- -   Pending - The next snapshot is pending to be taken.
--
-- 'snapshotScheduleState', 'cluster_snapshotScheduleState' - The current state of the cluster snapshot schedule.
--
-- 'encrypted', 'cluster_encrypted' - A boolean value that, if @true@, indicates that data in the cluster is
-- encrypted at rest.
--
-- 'allowVersionUpgrade', 'cluster_allowVersionUpgrade' - A boolean value that, if @true@, indicates that major version upgrades
-- will be applied automatically to the cluster during the maintenance
-- window.
--
-- 'clusterCreateTime', 'cluster_clusterCreateTime' - The date and time that the cluster was created.
--
-- 'automatedSnapshotRetentionPeriod', 'cluster_automatedSnapshotRetentionPeriod' - The number of days that automatic cluster snapshots are retained.
--
-- 'elasticIpStatus', 'cluster_elasticIpStatus' - The status of the elastic IP (EIP) address.
--
-- 'hsmStatus', 'cluster_hsmStatus' - A value that reports whether the Amazon Redshift cluster has finished
-- applying any hardware security module (HSM) settings changes specified
-- in a modify cluster command.
--
-- Values: active, applying
--
-- 'snapshotScheduleIdentifier', 'cluster_snapshotScheduleIdentifier' - A unique identifier for the cluster snapshot schedule.
--
-- 'masterUsername', 'cluster_masterUsername' - The master user name for the cluster. This name is used to connect to
-- the database that is specified in the __DBName__ parameter.
--
-- 'publiclyAccessible', 'cluster_publiclyAccessible' - A boolean value that, if @true@, indicates that the cluster can be
-- accessed from a public network.
--
-- 'clusterAvailabilityStatus', 'cluster_clusterAvailabilityStatus' - The availability status of the cluster for queries. Possible values are
-- the following:
--
-- -   Available - The cluster is available for queries.
--
-- -   Unavailable - The cluster is not available for queries.
--
-- -   Maintenance - The cluster is intermittently available for queries
--     due to maintenance activities.
--
-- -   Modifying - The cluster is intermittently available for queries due
--     to changes that modify the cluster.
--
-- -   Failed - The cluster failed and is not available for queries.
--
-- 'manualSnapshotRetentionPeriod', 'cluster_manualSnapshotRetentionPeriod' - The default number of days to retain a manual snapshot. If the value is
-- -1, the snapshot is retained indefinitely. This setting doesn\'t change
-- the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- 'kmsKeyId', 'cluster_kmsKeyId' - The AWS Key Management Service (AWS KMS) key ID of the encryption key
-- used to encrypt data in the cluster.
--
-- 'clusterSnapshotCopyStatus', 'cluster_clusterSnapshotCopyStatus' - A value that returns the destination region and retention period that
-- are configured for cross-region snapshot copy.
--
-- 'availabilityZone', 'cluster_availabilityZone' - The name of the Availability Zone in which the cluster is located.
--
-- 'clusterParameterGroups', 'cluster_clusterParameterGroups' - The list of cluster parameter groups that are associated with this
-- cluster. Each parameter group in the list is returned with its status.
--
-- 'preferredMaintenanceWindow', 'cluster_preferredMaintenanceWindow' - The weekly time range, in Universal Coordinated Time (UTC), during which
-- system maintenance can occur.
--
-- 'clusterPublicKey', 'cluster_clusterPublicKey' - The public key for the cluster.
--
-- 'restoreStatus', 'cluster_restoreStatus' - A value that describes the status of a cluster restore action. This
-- parameter returns null if the cluster was not created by restoring a
-- snapshot.
--
-- 'modifyStatus', 'cluster_modifyStatus' - The status of a modify operation, if any, initiated for the cluster.
--
-- 'clusterIdentifier', 'cluster_clusterIdentifier' - The unique identifier of the cluster.
--
-- 'tags', 'cluster_tags' - The list of tags for the cluster.
--
-- 'clusterNodes', 'cluster_clusterNodes' - The nodes in the cluster.
--
-- 'numberOfNodes', 'cluster_numberOfNodes' - The number of compute nodes in the cluster.
--
-- 'availabilityZoneRelocationStatus', 'cluster_availabilityZoneRelocationStatus' - Describes the status of the Availability Zone relocation operation.
--
-- 'dbName', 'cluster_dbName' - The name of the initial database that was created when the cluster was
-- created. This same name is returned for the life of the cluster. If an
-- initial database was not specified, a database named @dev@dev was
-- created by default.
--
-- 'dataTransferProgress', 'cluster_dataTransferProgress' -
--
-- 'clusterStatus', 'cluster_clusterStatus' - The current state of the cluster. Possible values are the following:
--
-- -   @available@
--
-- -   @available, prep-for-resize@
--
-- -   @available, resize-cleanup@
--
-- -   @cancelling-resize@
--
-- -   @creating@
--
-- -   @deleting@
--
-- -   @final-snapshot@
--
-- -   @hardware-failure@
--
-- -   @incompatible-hsm@
--
-- -   @incompatible-network@
--
-- -   @incompatible-parameters@
--
-- -   @incompatible-restore@
--
-- -   @modifying@
--
-- -   @paused@
--
-- -   @rebooting@
--
-- -   @renaming@
--
-- -   @resizing@
--
-- -   @rotating-keys@
--
-- -   @storage-full@
--
-- -   @updating-hsm@
--
-- 'pendingModifiedValues', 'cluster_pendingModifiedValues' - A value that, if present, indicates that changes to the cluster are
-- pending. Specific pending changes are identified by subelements.
--
-- 'endpoint', 'cluster_endpoint' - The connection endpoint.
--
-- 'nextMaintenanceWindowStartTime', 'cluster_nextMaintenanceWindowStartTime' - The date and time in UTC when system maintenance can begin.
--
-- 'nodeType', 'cluster_nodeType' - The node type for the nodes in the cluster.
--
-- 'clusterVersion', 'cluster_clusterVersion' - The version ID of the Amazon Redshift engine that is running on the
-- cluster.
--
-- 'vpcId', 'cluster_vpcId' - The identifier of the VPC the cluster is in, if the cluster is in a VPC.
--
-- 'clusterSecurityGroups', 'cluster_clusterSecurityGroups' - A list of cluster security group that are associated with the cluster.
-- Each security group is represented by an element that contains
-- @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@
-- subelements.
--
-- Cluster security groups are used when the cluster is not created in an
-- Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC
-- use VPC security groups, which are listed by the __VpcSecurityGroups__
-- parameter.
--
-- 'elasticResizeNumberOfNodeOptions', 'cluster_elasticResizeNumberOfNodeOptions' - The number of nodes that you can resize the cluster to with the elastic
-- resize method.
--
-- 'expectedNextSnapshotScheduleTime', 'cluster_expectedNextSnapshotScheduleTime' - The date and time when the next snapshot is expected to be taken for
-- clusters with a valid snapshot schedule and backups enabled.
--
-- 'maintenanceTrackName', 'cluster_maintenanceTrackName' - The name of the maintenance track for the cluster.
--
-- 'clusterRevisionNumber', 'cluster_clusterRevisionNumber' - The specific revision number of the database in the cluster.
--
-- 'iamRoles', 'cluster_iamRoles' - A list of AWS Identity and Access Management (IAM) roles that can be
-- used by the cluster to access other AWS services.
--
-- 'pendingActions', 'cluster_pendingActions' - Cluster operations that are waiting to be started.
newCluster ::
  Cluster
newCluster =
  Cluster'
    { enhancedVpcRouting = Core.Nothing,
      vpcSecurityGroups = Core.Nothing,
      resizeInfo = Core.Nothing,
      clusterNamespaceArn = Core.Nothing,
      clusterSubnetGroupName = Core.Nothing,
      deferredMaintenanceWindows = Core.Nothing,
      expectedNextSnapshotScheduleTimeStatus =
        Core.Nothing,
      snapshotScheduleState = Core.Nothing,
      encrypted = Core.Nothing,
      allowVersionUpgrade = Core.Nothing,
      clusterCreateTime = Core.Nothing,
      automatedSnapshotRetentionPeriod = Core.Nothing,
      elasticIpStatus = Core.Nothing,
      hsmStatus = Core.Nothing,
      snapshotScheduleIdentifier = Core.Nothing,
      masterUsername = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      clusterAvailabilityStatus = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing,
      kmsKeyId = Core.Nothing,
      clusterSnapshotCopyStatus = Core.Nothing,
      availabilityZone = Core.Nothing,
      clusterParameterGroups = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      clusterPublicKey = Core.Nothing,
      restoreStatus = Core.Nothing,
      modifyStatus = Core.Nothing,
      clusterIdentifier = Core.Nothing,
      tags = Core.Nothing,
      clusterNodes = Core.Nothing,
      numberOfNodes = Core.Nothing,
      availabilityZoneRelocationStatus = Core.Nothing,
      dbName = Core.Nothing,
      dataTransferProgress = Core.Nothing,
      clusterStatus = Core.Nothing,
      pendingModifiedValues = Core.Nothing,
      endpoint = Core.Nothing,
      nextMaintenanceWindowStartTime = Core.Nothing,
      nodeType = Core.Nothing,
      clusterVersion = Core.Nothing,
      vpcId = Core.Nothing,
      clusterSecurityGroups = Core.Nothing,
      elasticResizeNumberOfNodeOptions = Core.Nothing,
      expectedNextSnapshotScheduleTime = Core.Nothing,
      maintenanceTrackName = Core.Nothing,
      clusterRevisionNumber = Core.Nothing,
      iamRoles = Core.Nothing,
      pendingActions = Core.Nothing
    }

-- | An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
cluster_enhancedVpcRouting :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cluster_enhancedVpcRouting = Lens.lens (\Cluster' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@Cluster' {} a -> s {enhancedVpcRouting = a} :: Cluster)

-- | A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that
-- are associated with the cluster. This parameter is returned only if the
-- cluster is in a VPC.
cluster_vpcSecurityGroups :: Lens.Lens' Cluster (Core.Maybe [VpcSecurityGroupMembership])
cluster_vpcSecurityGroups = Lens.lens (\Cluster' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@Cluster' {} a -> s {vpcSecurityGroups = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | Returns the following:
--
-- -   AllowCancelResize: a boolean value indicating if the resize
--     operation can be cancelled.
--
-- -   ResizeType: Returns ClassicResize
cluster_resizeInfo :: Lens.Lens' Cluster (Core.Maybe ResizeInfo)
cluster_resizeInfo = Lens.lens (\Cluster' {resizeInfo} -> resizeInfo) (\s@Cluster' {} a -> s {resizeInfo = a} :: Cluster)

-- | The namespace Amazon Resource Name (ARN) of the cluster.
cluster_clusterNamespaceArn :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterNamespaceArn = Lens.lens (\Cluster' {clusterNamespaceArn} -> clusterNamespaceArn) (\s@Cluster' {} a -> s {clusterNamespaceArn = a} :: Cluster)

-- | The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
cluster_clusterSubnetGroupName :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterSubnetGroupName = Lens.lens (\Cluster' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@Cluster' {} a -> s {clusterSubnetGroupName = a} :: Cluster)

-- | Describes a group of @DeferredMaintenanceWindow@ objects.
cluster_deferredMaintenanceWindows :: Lens.Lens' Cluster (Core.Maybe [DeferredMaintenanceWindow])
cluster_deferredMaintenanceWindows = Lens.lens (\Cluster' {deferredMaintenanceWindows} -> deferredMaintenanceWindows) (\s@Cluster' {} a -> s {deferredMaintenanceWindows = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | The status of next expected snapshot for clusters having a valid
-- snapshot schedule and backups enabled. Possible values are the
-- following:
--
-- -   OnTrack - The next snapshot is expected to be taken on time.
--
-- -   Pending - The next snapshot is pending to be taken.
cluster_expectedNextSnapshotScheduleTimeStatus :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_expectedNextSnapshotScheduleTimeStatus = Lens.lens (\Cluster' {expectedNextSnapshotScheduleTimeStatus} -> expectedNextSnapshotScheduleTimeStatus) (\s@Cluster' {} a -> s {expectedNextSnapshotScheduleTimeStatus = a} :: Cluster)

-- | The current state of the cluster snapshot schedule.
cluster_snapshotScheduleState :: Lens.Lens' Cluster (Core.Maybe ScheduleState)
cluster_snapshotScheduleState = Lens.lens (\Cluster' {snapshotScheduleState} -> snapshotScheduleState) (\s@Cluster' {} a -> s {snapshotScheduleState = a} :: Cluster)

-- | A boolean value that, if @true@, indicates that data in the cluster is
-- encrypted at rest.
cluster_encrypted :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cluster_encrypted = Lens.lens (\Cluster' {encrypted} -> encrypted) (\s@Cluster' {} a -> s {encrypted = a} :: Cluster)

-- | A boolean value that, if @true@, indicates that major version upgrades
-- will be applied automatically to the cluster during the maintenance
-- window.
cluster_allowVersionUpgrade :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cluster_allowVersionUpgrade = Lens.lens (\Cluster' {allowVersionUpgrade} -> allowVersionUpgrade) (\s@Cluster' {} a -> s {allowVersionUpgrade = a} :: Cluster)

-- | The date and time that the cluster was created.
cluster_clusterCreateTime :: Lens.Lens' Cluster (Core.Maybe Core.UTCTime)
cluster_clusterCreateTime = Lens.lens (\Cluster' {clusterCreateTime} -> clusterCreateTime) (\s@Cluster' {} a -> s {clusterCreateTime = a} :: Cluster) Core.. Lens.mapping Core._Time

-- | The number of days that automatic cluster snapshots are retained.
cluster_automatedSnapshotRetentionPeriod :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cluster_automatedSnapshotRetentionPeriod = Lens.lens (\Cluster' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@Cluster' {} a -> s {automatedSnapshotRetentionPeriod = a} :: Cluster)

-- | The status of the elastic IP (EIP) address.
cluster_elasticIpStatus :: Lens.Lens' Cluster (Core.Maybe ElasticIpStatus)
cluster_elasticIpStatus = Lens.lens (\Cluster' {elasticIpStatus} -> elasticIpStatus) (\s@Cluster' {} a -> s {elasticIpStatus = a} :: Cluster)

-- | A value that reports whether the Amazon Redshift cluster has finished
-- applying any hardware security module (HSM) settings changes specified
-- in a modify cluster command.
--
-- Values: active, applying
cluster_hsmStatus :: Lens.Lens' Cluster (Core.Maybe HsmStatus)
cluster_hsmStatus = Lens.lens (\Cluster' {hsmStatus} -> hsmStatus) (\s@Cluster' {} a -> s {hsmStatus = a} :: Cluster)

-- | A unique identifier for the cluster snapshot schedule.
cluster_snapshotScheduleIdentifier :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_snapshotScheduleIdentifier = Lens.lens (\Cluster' {snapshotScheduleIdentifier} -> snapshotScheduleIdentifier) (\s@Cluster' {} a -> s {snapshotScheduleIdentifier = a} :: Cluster)

-- | The master user name for the cluster. This name is used to connect to
-- the database that is specified in the __DBName__ parameter.
cluster_masterUsername :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_masterUsername = Lens.lens (\Cluster' {masterUsername} -> masterUsername) (\s@Cluster' {} a -> s {masterUsername = a} :: Cluster)

-- | A boolean value that, if @true@, indicates that the cluster can be
-- accessed from a public network.
cluster_publiclyAccessible :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cluster_publiclyAccessible = Lens.lens (\Cluster' {publiclyAccessible} -> publiclyAccessible) (\s@Cluster' {} a -> s {publiclyAccessible = a} :: Cluster)

-- | The availability status of the cluster for queries. Possible values are
-- the following:
--
-- -   Available - The cluster is available for queries.
--
-- -   Unavailable - The cluster is not available for queries.
--
-- -   Maintenance - The cluster is intermittently available for queries
--     due to maintenance activities.
--
-- -   Modifying - The cluster is intermittently available for queries due
--     to changes that modify the cluster.
--
-- -   Failed - The cluster failed and is not available for queries.
cluster_clusterAvailabilityStatus :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterAvailabilityStatus = Lens.lens (\Cluster' {clusterAvailabilityStatus} -> clusterAvailabilityStatus) (\s@Cluster' {} a -> s {clusterAvailabilityStatus = a} :: Cluster)

-- | The default number of days to retain a manual snapshot. If the value is
-- -1, the snapshot is retained indefinitely. This setting doesn\'t change
-- the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
cluster_manualSnapshotRetentionPeriod :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cluster_manualSnapshotRetentionPeriod = Lens.lens (\Cluster' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@Cluster' {} a -> s {manualSnapshotRetentionPeriod = a} :: Cluster)

-- | The AWS Key Management Service (AWS KMS) key ID of the encryption key
-- used to encrypt data in the cluster.
cluster_kmsKeyId :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_kmsKeyId = Lens.lens (\Cluster' {kmsKeyId} -> kmsKeyId) (\s@Cluster' {} a -> s {kmsKeyId = a} :: Cluster)

-- | A value that returns the destination region and retention period that
-- are configured for cross-region snapshot copy.
cluster_clusterSnapshotCopyStatus :: Lens.Lens' Cluster (Core.Maybe ClusterSnapshotCopyStatus)
cluster_clusterSnapshotCopyStatus = Lens.lens (\Cluster' {clusterSnapshotCopyStatus} -> clusterSnapshotCopyStatus) (\s@Cluster' {} a -> s {clusterSnapshotCopyStatus = a} :: Cluster)

-- | The name of the Availability Zone in which the cluster is located.
cluster_availabilityZone :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_availabilityZone = Lens.lens (\Cluster' {availabilityZone} -> availabilityZone) (\s@Cluster' {} a -> s {availabilityZone = a} :: Cluster)

-- | The list of cluster parameter groups that are associated with this
-- cluster. Each parameter group in the list is returned with its status.
cluster_clusterParameterGroups :: Lens.Lens' Cluster (Core.Maybe [ClusterParameterGroupStatus])
cluster_clusterParameterGroups = Lens.lens (\Cluster' {clusterParameterGroups} -> clusterParameterGroups) (\s@Cluster' {} a -> s {clusterParameterGroups = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | The weekly time range, in Universal Coordinated Time (UTC), during which
-- system maintenance can occur.
cluster_preferredMaintenanceWindow :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_preferredMaintenanceWindow = Lens.lens (\Cluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@Cluster' {} a -> s {preferredMaintenanceWindow = a} :: Cluster)

-- | The public key for the cluster.
cluster_clusterPublicKey :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterPublicKey = Lens.lens (\Cluster' {clusterPublicKey} -> clusterPublicKey) (\s@Cluster' {} a -> s {clusterPublicKey = a} :: Cluster)

-- | A value that describes the status of a cluster restore action. This
-- parameter returns null if the cluster was not created by restoring a
-- snapshot.
cluster_restoreStatus :: Lens.Lens' Cluster (Core.Maybe RestoreStatus)
cluster_restoreStatus = Lens.lens (\Cluster' {restoreStatus} -> restoreStatus) (\s@Cluster' {} a -> s {restoreStatus = a} :: Cluster)

-- | The status of a modify operation, if any, initiated for the cluster.
cluster_modifyStatus :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_modifyStatus = Lens.lens (\Cluster' {modifyStatus} -> modifyStatus) (\s@Cluster' {} a -> s {modifyStatus = a} :: Cluster)

-- | The unique identifier of the cluster.
cluster_clusterIdentifier :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterIdentifier = Lens.lens (\Cluster' {clusterIdentifier} -> clusterIdentifier) (\s@Cluster' {} a -> s {clusterIdentifier = a} :: Cluster)

-- | The list of tags for the cluster.
cluster_tags :: Lens.Lens' Cluster (Core.Maybe [Tag])
cluster_tags = Lens.lens (\Cluster' {tags} -> tags) (\s@Cluster' {} a -> s {tags = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | The nodes in the cluster.
cluster_clusterNodes :: Lens.Lens' Cluster (Core.Maybe [ClusterNode])
cluster_clusterNodes = Lens.lens (\Cluster' {clusterNodes} -> clusterNodes) (\s@Cluster' {} a -> s {clusterNodes = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | The number of compute nodes in the cluster.
cluster_numberOfNodes :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cluster_numberOfNodes = Lens.lens (\Cluster' {numberOfNodes} -> numberOfNodes) (\s@Cluster' {} a -> s {numberOfNodes = a} :: Cluster)

-- | Describes the status of the Availability Zone relocation operation.
cluster_availabilityZoneRelocationStatus :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_availabilityZoneRelocationStatus = Lens.lens (\Cluster' {availabilityZoneRelocationStatus} -> availabilityZoneRelocationStatus) (\s@Cluster' {} a -> s {availabilityZoneRelocationStatus = a} :: Cluster)

-- | The name of the initial database that was created when the cluster was
-- created. This same name is returned for the life of the cluster. If an
-- initial database was not specified, a database named @dev@dev was
-- created by default.
cluster_dbName :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_dbName = Lens.lens (\Cluster' {dbName} -> dbName) (\s@Cluster' {} a -> s {dbName = a} :: Cluster)

-- |
cluster_dataTransferProgress :: Lens.Lens' Cluster (Core.Maybe DataTransferProgress)
cluster_dataTransferProgress = Lens.lens (\Cluster' {dataTransferProgress} -> dataTransferProgress) (\s@Cluster' {} a -> s {dataTransferProgress = a} :: Cluster)

-- | The current state of the cluster. Possible values are the following:
--
-- -   @available@
--
-- -   @available, prep-for-resize@
--
-- -   @available, resize-cleanup@
--
-- -   @cancelling-resize@
--
-- -   @creating@
--
-- -   @deleting@
--
-- -   @final-snapshot@
--
-- -   @hardware-failure@
--
-- -   @incompatible-hsm@
--
-- -   @incompatible-network@
--
-- -   @incompatible-parameters@
--
-- -   @incompatible-restore@
--
-- -   @modifying@
--
-- -   @paused@
--
-- -   @rebooting@
--
-- -   @renaming@
--
-- -   @resizing@
--
-- -   @rotating-keys@
--
-- -   @storage-full@
--
-- -   @updating-hsm@
cluster_clusterStatus :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterStatus = Lens.lens (\Cluster' {clusterStatus} -> clusterStatus) (\s@Cluster' {} a -> s {clusterStatus = a} :: Cluster)

-- | A value that, if present, indicates that changes to the cluster are
-- pending. Specific pending changes are identified by subelements.
cluster_pendingModifiedValues :: Lens.Lens' Cluster (Core.Maybe PendingModifiedValues)
cluster_pendingModifiedValues = Lens.lens (\Cluster' {pendingModifiedValues} -> pendingModifiedValues) (\s@Cluster' {} a -> s {pendingModifiedValues = a} :: Cluster)

-- | The connection endpoint.
cluster_endpoint :: Lens.Lens' Cluster (Core.Maybe Endpoint)
cluster_endpoint = Lens.lens (\Cluster' {endpoint} -> endpoint) (\s@Cluster' {} a -> s {endpoint = a} :: Cluster)

-- | The date and time in UTC when system maintenance can begin.
cluster_nextMaintenanceWindowStartTime :: Lens.Lens' Cluster (Core.Maybe Core.UTCTime)
cluster_nextMaintenanceWindowStartTime = Lens.lens (\Cluster' {nextMaintenanceWindowStartTime} -> nextMaintenanceWindowStartTime) (\s@Cluster' {} a -> s {nextMaintenanceWindowStartTime = a} :: Cluster) Core.. Lens.mapping Core._Time

-- | The node type for the nodes in the cluster.
cluster_nodeType :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_nodeType = Lens.lens (\Cluster' {nodeType} -> nodeType) (\s@Cluster' {} a -> s {nodeType = a} :: Cluster)

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
cluster_clusterVersion :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterVersion = Lens.lens (\Cluster' {clusterVersion} -> clusterVersion) (\s@Cluster' {} a -> s {clusterVersion = a} :: Cluster)

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
cluster_vpcId :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_vpcId = Lens.lens (\Cluster' {vpcId} -> vpcId) (\s@Cluster' {} a -> s {vpcId = a} :: Cluster)

-- | A list of cluster security group that are associated with the cluster.
-- Each security group is represented by an element that contains
-- @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@
-- subelements.
--
-- Cluster security groups are used when the cluster is not created in an
-- Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC
-- use VPC security groups, which are listed by the __VpcSecurityGroups__
-- parameter.
cluster_clusterSecurityGroups :: Lens.Lens' Cluster (Core.Maybe [ClusterSecurityGroupMembership])
cluster_clusterSecurityGroups = Lens.lens (\Cluster' {clusterSecurityGroups} -> clusterSecurityGroups) (\s@Cluster' {} a -> s {clusterSecurityGroups = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | The number of nodes that you can resize the cluster to with the elastic
-- resize method.
cluster_elasticResizeNumberOfNodeOptions :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_elasticResizeNumberOfNodeOptions = Lens.lens (\Cluster' {elasticResizeNumberOfNodeOptions} -> elasticResizeNumberOfNodeOptions) (\s@Cluster' {} a -> s {elasticResizeNumberOfNodeOptions = a} :: Cluster)

-- | The date and time when the next snapshot is expected to be taken for
-- clusters with a valid snapshot schedule and backups enabled.
cluster_expectedNextSnapshotScheduleTime :: Lens.Lens' Cluster (Core.Maybe Core.UTCTime)
cluster_expectedNextSnapshotScheduleTime = Lens.lens (\Cluster' {expectedNextSnapshotScheduleTime} -> expectedNextSnapshotScheduleTime) (\s@Cluster' {} a -> s {expectedNextSnapshotScheduleTime = a} :: Cluster) Core.. Lens.mapping Core._Time

-- | The name of the maintenance track for the cluster.
cluster_maintenanceTrackName :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_maintenanceTrackName = Lens.lens (\Cluster' {maintenanceTrackName} -> maintenanceTrackName) (\s@Cluster' {} a -> s {maintenanceTrackName = a} :: Cluster)

-- | The specific revision number of the database in the cluster.
cluster_clusterRevisionNumber :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterRevisionNumber = Lens.lens (\Cluster' {clusterRevisionNumber} -> clusterRevisionNumber) (\s@Cluster' {} a -> s {clusterRevisionNumber = a} :: Cluster)

-- | A list of AWS Identity and Access Management (IAM) roles that can be
-- used by the cluster to access other AWS services.
cluster_iamRoles :: Lens.Lens' Cluster (Core.Maybe [ClusterIamRole])
cluster_iamRoles = Lens.lens (\Cluster' {iamRoles} -> iamRoles) (\s@Cluster' {} a -> s {iamRoles = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | Cluster operations that are waiting to be started.
cluster_pendingActions :: Lens.Lens' Cluster (Core.Maybe [Core.Text])
cluster_pendingActions = Lens.lens (\Cluster' {pendingActions} -> pendingActions) (\s@Cluster' {} a -> s {pendingActions = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML Cluster where
  parseXML x =
    Cluster'
      Core.<$> (x Core..@? "EnhancedVpcRouting")
      Core.<*> ( x Core..@? "VpcSecurityGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "VpcSecurityGroup")
               )
      Core.<*> (x Core..@? "ResizeInfo")
      Core.<*> (x Core..@? "ClusterNamespaceArn")
      Core.<*> (x Core..@? "ClusterSubnetGroupName")
      Core.<*> ( x Core..@? "DeferredMaintenanceWindows"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "DeferredMaintenanceWindow")
               )
      Core.<*> (x Core..@? "ExpectedNextSnapshotScheduleTimeStatus")
      Core.<*> (x Core..@? "SnapshotScheduleState")
      Core.<*> (x Core..@? "Encrypted")
      Core.<*> (x Core..@? "AllowVersionUpgrade")
      Core.<*> (x Core..@? "ClusterCreateTime")
      Core.<*> (x Core..@? "AutomatedSnapshotRetentionPeriod")
      Core.<*> (x Core..@? "ElasticIpStatus")
      Core.<*> (x Core..@? "HsmStatus")
      Core.<*> (x Core..@? "SnapshotScheduleIdentifier")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "PubliclyAccessible")
      Core.<*> (x Core..@? "ClusterAvailabilityStatus")
      Core.<*> (x Core..@? "ManualSnapshotRetentionPeriod")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "ClusterSnapshotCopyStatus")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> ( x Core..@? "ClusterParameterGroups"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "ClusterParameterGroup")
               )
      Core.<*> (x Core..@? "PreferredMaintenanceWindow")
      Core.<*> (x Core..@? "ClusterPublicKey")
      Core.<*> (x Core..@? "RestoreStatus")
      Core.<*> (x Core..@? "ModifyStatus")
      Core.<*> (x Core..@? "ClusterIdentifier")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> ( x Core..@? "ClusterNodes" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "NumberOfNodes")
      Core.<*> (x Core..@? "AvailabilityZoneRelocationStatus")
      Core.<*> (x Core..@? "DBName")
      Core.<*> (x Core..@? "DataTransferProgress")
      Core.<*> (x Core..@? "ClusterStatus")
      Core.<*> (x Core..@? "PendingModifiedValues")
      Core.<*> (x Core..@? "Endpoint")
      Core.<*> (x Core..@? "NextMaintenanceWindowStartTime")
      Core.<*> (x Core..@? "NodeType")
      Core.<*> (x Core..@? "ClusterVersion")
      Core.<*> (x Core..@? "VpcId")
      Core.<*> ( x Core..@? "ClusterSecurityGroups"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "ClusterSecurityGroup")
               )
      Core.<*> (x Core..@? "ElasticResizeNumberOfNodeOptions")
      Core.<*> (x Core..@? "ExpectedNextSnapshotScheduleTime")
      Core.<*> (x Core..@? "MaintenanceTrackName")
      Core.<*> (x Core..@? "ClusterRevisionNumber")
      Core.<*> ( x Core..@? "IamRoles" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "ClusterIamRole")
               )
      Core.<*> ( x Core..@? "PendingActions" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable Cluster

instance Core.NFData Cluster
