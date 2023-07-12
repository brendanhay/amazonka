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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterNode
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterGroup
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSecurityGroup
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSnapshotCopyStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterDeferredMaintenanceWindow
import Amazonka.SecurityHub.Types.AwsRedshiftClusterElasticIpStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterEndpoint
import Amazonka.SecurityHub.Types.AwsRedshiftClusterHsmStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterIamRole
import Amazonka.SecurityHub.Types.AwsRedshiftClusterLoggingStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterPendingModifiedValues
import Amazonka.SecurityHub.Types.AwsRedshiftClusterResizeInfo
import Amazonka.SecurityHub.Types.AwsRedshiftClusterRestoreStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterVpcSecurityGroup

-- | Details about an Amazon Redshift cluster.
--
-- /See:/ 'newAwsRedshiftClusterDetails' smart constructor.
data AwsRedshiftClusterDetails = AwsRedshiftClusterDetails'
  { -- | Indicates whether major version upgrades are applied automatically to
    -- the cluster during the maintenance window.
    allowVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The number of days that automatic cluster snapshots are retained.
    automatedSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The name of the Availability Zone in which the cluster is located.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The availability status of the cluster for queries. Possible values are
    -- the following:
    --
    -- -   @Available@ - The cluster is available for queries.
    --
    -- -   @Unavailable@ - The cluster is not available for queries.
    --
    -- -   @Maintenance@ - The cluster is intermittently available for queries
    --     due to maintenance activities.
    --
    -- -   @Modifying@ -The cluster is intermittently available for queries due
    --     to changes that modify the cluster.
    --
    -- -   @Failed@ - The cluster failed and is not available for queries.
    clusterAvailabilityStatus :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the cluster was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    clusterCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the cluster.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The nodes in the cluster.
    clusterNodes :: Prelude.Maybe [AwsRedshiftClusterClusterNode],
    -- | The list of cluster parameter groups that are associated with this
    -- cluster.
    clusterParameterGroups :: Prelude.Maybe [AwsRedshiftClusterClusterParameterGroup],
    -- | The public key for the cluster.
    clusterPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The specific revision number of the database in the cluster.
    clusterRevisionNumber :: Prelude.Maybe Prelude.Text,
    -- | A list of cluster security groups that are associated with the cluster.
    clusterSecurityGroups :: Prelude.Maybe [AwsRedshiftClusterClusterSecurityGroup],
    -- | Information about the destination Region and retention period for the
    -- cross-Region snapshot copy.
    clusterSnapshotCopyStatus :: Prelude.Maybe AwsRedshiftClusterClusterSnapshotCopyStatus,
    -- | The current status of the cluster.
    --
    -- Valid values: @available@ | @available, prep-for-resize@ |
    -- @available, resize-cleanup@ |@ cancelling-resize@ | @creating@ |
    -- @deleting@ | @final-snapshot@ | @hardware-failure@ | @incompatible-hsm@
    -- |@ incompatible-network@ | @incompatible-parameters@ |
    -- @incompatible-restore@ | @modifying@ | @paused@ | @rebooting@ |
    -- @renaming@ | @resizing@ | @rotating-keys@ | @storage-full@ |
    -- @updating-hsm@
    clusterStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the subnet group that is associated with the cluster. This
    -- parameter is valid only when the cluster is in a VPC.
    clusterSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The version ID of the Amazon Redshift engine that runs on the cluster.
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the initial database that was created when the cluster was
    -- created.
    --
    -- The same name is returned for the life of the cluster.
    --
    -- If an initial database is not specified, a database named @devdev@ is
    -- created by default.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | List of time windows during which maintenance was deferred.
    deferredMaintenanceWindows :: Prelude.Maybe [AwsRedshiftClusterDeferredMaintenanceWindow],
    -- | Information about the status of the Elastic IP (EIP) address.
    elasticIpStatus :: Prelude.Maybe AwsRedshiftClusterElasticIpStatus,
    -- | The number of nodes that you can use the elastic resize method to resize
    -- the cluster to.
    elasticResizeNumberOfNodeOptions :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the data in the cluster is encrypted at rest.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The connection endpoint.
    endpoint :: Prelude.Maybe AwsRedshiftClusterEndpoint,
    -- | Indicates whether to create the cluster with enhanced VPC routing
    -- enabled.
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | Indicates when the next snapshot is expected to be taken. The cluster
    -- must have a valid snapshot schedule and have backups enabled.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    expectedNextSnapshotScheduleTime :: Prelude.Maybe Prelude.Text,
    -- | The status of the next expected snapshot.
    --
    -- Valid values: @OnTrack@ | @Pending@
    expectedNextSnapshotScheduleTimeStatus :: Prelude.Maybe Prelude.Text,
    -- | Information about whether the Amazon Redshift cluster finished applying
    -- any changes to hardware security module (HSM) settings that were
    -- specified in a modify cluster command.
    hsmStatus :: Prelude.Maybe AwsRedshiftClusterHsmStatus,
    -- | A list of IAM roles that the cluster can use to access other Amazon Web
    -- Services services.
    iamRoles :: Prelude.Maybe [AwsRedshiftClusterIamRole],
    -- | The identifier of the KMS encryption key that is used to encrypt data in
    -- the cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Information about the logging status of the cluster.
    loggingStatus :: Prelude.Maybe AwsRedshiftClusterLoggingStatus,
    -- | The name of the maintenance track for the cluster.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | The default number of days to retain a manual snapshot.
    --
    -- If the value is @-1@, the snapshot is retained indefinitely.
    --
    -- This setting doesn\'t change the retention period of existing snapshots.
    --
    -- Valid values: Either @-1@ or an integer between 1 and 3,653
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The master user name for the cluster. This name is used to connect to
    -- the database that is specified in as the value of @DBName@.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | Indicates the start of the next maintenance window.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    nextMaintenanceWindowStartTime :: Prelude.Maybe Prelude.Text,
    -- | The node type for the nodes in the cluster.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The number of compute nodes in the cluster.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | A list of cluster operations that are waiting to start.
    pendingActions :: Prelude.Maybe [Prelude.Text],
    -- | A list of changes to the cluster that are currently pending.
    pendingModifiedValues :: Prelude.Maybe AwsRedshiftClusterPendingModifiedValues,
    -- | The weekly time range, in Universal Coordinated Time (UTC), during which
    -- system maintenance can occur.
    --
    -- Format: @ @/@\<day>@/@:HH:MM-@/@\<day>@/@:HH:MM@
    --
    -- For the day values, use @mon@ | @tue@ | @wed@ | @thu@ | @fri@ | @sat@ |
    -- @sun@
    --
    -- For example, @sun:09:32-sun:10:02@
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | Whether the cluster can be accessed from a public network.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Information about the resize operation for the cluster.
    resizeInfo :: Prelude.Maybe AwsRedshiftClusterResizeInfo,
    -- | Information about the status of a cluster restore action. Only applies
    -- to a cluster that was created by restoring a snapshot.
    restoreStatus :: Prelude.Maybe AwsRedshiftClusterRestoreStatus,
    -- | A unique identifier for the cluster snapshot schedule.
    snapshotScheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The current state of the cluster snapshot schedule.
    --
    -- Valid values: @MODIFYING@ | @ACTIVE@ | @FAILED@
    snapshotScheduleState :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC that the cluster is in, if the cluster is in a
    -- VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The list of VPC security groups that the cluster belongs to, if the
    -- cluster is in a VPC.
    vpcSecurityGroups :: Prelude.Maybe [AwsRedshiftClusterVpcSecurityGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowVersionUpgrade', 'awsRedshiftClusterDetails_allowVersionUpgrade' - Indicates whether major version upgrades are applied automatically to
-- the cluster during the maintenance window.
--
-- 'automatedSnapshotRetentionPeriod', 'awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod' - The number of days that automatic cluster snapshots are retained.
--
-- 'availabilityZone', 'awsRedshiftClusterDetails_availabilityZone' - The name of the Availability Zone in which the cluster is located.
--
-- 'clusterAvailabilityStatus', 'awsRedshiftClusterDetails_clusterAvailabilityStatus' - The availability status of the cluster for queries. Possible values are
-- the following:
--
-- -   @Available@ - The cluster is available for queries.
--
-- -   @Unavailable@ - The cluster is not available for queries.
--
-- -   @Maintenance@ - The cluster is intermittently available for queries
--     due to maintenance activities.
--
-- -   @Modifying@ -The cluster is intermittently available for queries due
--     to changes that modify the cluster.
--
-- -   @Failed@ - The cluster failed and is not available for queries.
--
-- 'clusterCreateTime', 'awsRedshiftClusterDetails_clusterCreateTime' - Indicates when the cluster was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'clusterIdentifier', 'awsRedshiftClusterDetails_clusterIdentifier' - The unique identifier of the cluster.
--
-- 'clusterNodes', 'awsRedshiftClusterDetails_clusterNodes' - The nodes in the cluster.
--
-- 'clusterParameterGroups', 'awsRedshiftClusterDetails_clusterParameterGroups' - The list of cluster parameter groups that are associated with this
-- cluster.
--
-- 'clusterPublicKey', 'awsRedshiftClusterDetails_clusterPublicKey' - The public key for the cluster.
--
-- 'clusterRevisionNumber', 'awsRedshiftClusterDetails_clusterRevisionNumber' - The specific revision number of the database in the cluster.
--
-- 'clusterSecurityGroups', 'awsRedshiftClusterDetails_clusterSecurityGroups' - A list of cluster security groups that are associated with the cluster.
--
-- 'clusterSnapshotCopyStatus', 'awsRedshiftClusterDetails_clusterSnapshotCopyStatus' - Information about the destination Region and retention period for the
-- cross-Region snapshot copy.
--
-- 'clusterStatus', 'awsRedshiftClusterDetails_clusterStatus' - The current status of the cluster.
--
-- Valid values: @available@ | @available, prep-for-resize@ |
-- @available, resize-cleanup@ |@ cancelling-resize@ | @creating@ |
-- @deleting@ | @final-snapshot@ | @hardware-failure@ | @incompatible-hsm@
-- |@ incompatible-network@ | @incompatible-parameters@ |
-- @incompatible-restore@ | @modifying@ | @paused@ | @rebooting@ |
-- @renaming@ | @resizing@ | @rotating-keys@ | @storage-full@ |
-- @updating-hsm@
--
-- 'clusterSubnetGroupName', 'awsRedshiftClusterDetails_clusterSubnetGroupName' - The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
--
-- 'clusterVersion', 'awsRedshiftClusterDetails_clusterVersion' - The version ID of the Amazon Redshift engine that runs on the cluster.
--
-- 'dbName', 'awsRedshiftClusterDetails_dbName' - The name of the initial database that was created when the cluster was
-- created.
--
-- The same name is returned for the life of the cluster.
--
-- If an initial database is not specified, a database named @devdev@ is
-- created by default.
--
-- 'deferredMaintenanceWindows', 'awsRedshiftClusterDetails_deferredMaintenanceWindows' - List of time windows during which maintenance was deferred.
--
-- 'elasticIpStatus', 'awsRedshiftClusterDetails_elasticIpStatus' - Information about the status of the Elastic IP (EIP) address.
--
-- 'elasticResizeNumberOfNodeOptions', 'awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions' - The number of nodes that you can use the elastic resize method to resize
-- the cluster to.
--
-- 'encrypted', 'awsRedshiftClusterDetails_encrypted' - Indicates whether the data in the cluster is encrypted at rest.
--
-- 'endpoint', 'awsRedshiftClusterDetails_endpoint' - The connection endpoint.
--
-- 'enhancedVpcRouting', 'awsRedshiftClusterDetails_enhancedVpcRouting' - Indicates whether to create the cluster with enhanced VPC routing
-- enabled.
--
-- 'expectedNextSnapshotScheduleTime', 'awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime' - Indicates when the next snapshot is expected to be taken. The cluster
-- must have a valid snapshot schedule and have backups enabled.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'expectedNextSnapshotScheduleTimeStatus', 'awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus' - The status of the next expected snapshot.
--
-- Valid values: @OnTrack@ | @Pending@
--
-- 'hsmStatus', 'awsRedshiftClusterDetails_hsmStatus' - Information about whether the Amazon Redshift cluster finished applying
-- any changes to hardware security module (HSM) settings that were
-- specified in a modify cluster command.
--
-- 'iamRoles', 'awsRedshiftClusterDetails_iamRoles' - A list of IAM roles that the cluster can use to access other Amazon Web
-- Services services.
--
-- 'kmsKeyId', 'awsRedshiftClusterDetails_kmsKeyId' - The identifier of the KMS encryption key that is used to encrypt data in
-- the cluster.
--
-- 'loggingStatus', 'awsRedshiftClusterDetails_loggingStatus' - Information about the logging status of the cluster.
--
-- 'maintenanceTrackName', 'awsRedshiftClusterDetails_maintenanceTrackName' - The name of the maintenance track for the cluster.
--
-- 'manualSnapshotRetentionPeriod', 'awsRedshiftClusterDetails_manualSnapshotRetentionPeriod' - The default number of days to retain a manual snapshot.
--
-- If the value is @-1@, the snapshot is retained indefinitely.
--
-- This setting doesn\'t change the retention period of existing snapshots.
--
-- Valid values: Either @-1@ or an integer between 1 and 3,653
--
-- 'masterUsername', 'awsRedshiftClusterDetails_masterUsername' - The master user name for the cluster. This name is used to connect to
-- the database that is specified in as the value of @DBName@.
--
-- 'nextMaintenanceWindowStartTime', 'awsRedshiftClusterDetails_nextMaintenanceWindowStartTime' - Indicates the start of the next maintenance window.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'nodeType', 'awsRedshiftClusterDetails_nodeType' - The node type for the nodes in the cluster.
--
-- 'numberOfNodes', 'awsRedshiftClusterDetails_numberOfNodes' - The number of compute nodes in the cluster.
--
-- 'pendingActions', 'awsRedshiftClusterDetails_pendingActions' - A list of cluster operations that are waiting to start.
--
-- 'pendingModifiedValues', 'awsRedshiftClusterDetails_pendingModifiedValues' - A list of changes to the cluster that are currently pending.
--
-- 'preferredMaintenanceWindow', 'awsRedshiftClusterDetails_preferredMaintenanceWindow' - The weekly time range, in Universal Coordinated Time (UTC), during which
-- system maintenance can occur.
--
-- Format: @ @/@\<day>@/@:HH:MM-@/@\<day>@/@:HH:MM@
--
-- For the day values, use @mon@ | @tue@ | @wed@ | @thu@ | @fri@ | @sat@ |
-- @sun@
--
-- For example, @sun:09:32-sun:10:02@
--
-- 'publiclyAccessible', 'awsRedshiftClusterDetails_publiclyAccessible' - Whether the cluster can be accessed from a public network.
--
-- 'resizeInfo', 'awsRedshiftClusterDetails_resizeInfo' - Information about the resize operation for the cluster.
--
-- 'restoreStatus', 'awsRedshiftClusterDetails_restoreStatus' - Information about the status of a cluster restore action. Only applies
-- to a cluster that was created by restoring a snapshot.
--
-- 'snapshotScheduleIdentifier', 'awsRedshiftClusterDetails_snapshotScheduleIdentifier' - A unique identifier for the cluster snapshot schedule.
--
-- 'snapshotScheduleState', 'awsRedshiftClusterDetails_snapshotScheduleState' - The current state of the cluster snapshot schedule.
--
-- Valid values: @MODIFYING@ | @ACTIVE@ | @FAILED@
--
-- 'vpcId', 'awsRedshiftClusterDetails_vpcId' - The identifier of the VPC that the cluster is in, if the cluster is in a
-- VPC.
--
-- 'vpcSecurityGroups', 'awsRedshiftClusterDetails_vpcSecurityGroups' - The list of VPC security groups that the cluster belongs to, if the
-- cluster is in a VPC.
newAwsRedshiftClusterDetails ::
  AwsRedshiftClusterDetails
newAwsRedshiftClusterDetails =
  AwsRedshiftClusterDetails'
    { allowVersionUpgrade =
        Prelude.Nothing,
      automatedSnapshotRetentionPeriod =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      clusterAvailabilityStatus = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      clusterNodes = Prelude.Nothing,
      clusterParameterGroups = Prelude.Nothing,
      clusterPublicKey = Prelude.Nothing,
      clusterRevisionNumber = Prelude.Nothing,
      clusterSecurityGroups = Prelude.Nothing,
      clusterSnapshotCopyStatus = Prelude.Nothing,
      clusterStatus = Prelude.Nothing,
      clusterSubnetGroupName = Prelude.Nothing,
      clusterVersion = Prelude.Nothing,
      dbName = Prelude.Nothing,
      deferredMaintenanceWindows = Prelude.Nothing,
      elasticIpStatus = Prelude.Nothing,
      elasticResizeNumberOfNodeOptions =
        Prelude.Nothing,
      encrypted = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      enhancedVpcRouting = Prelude.Nothing,
      expectedNextSnapshotScheduleTime =
        Prelude.Nothing,
      expectedNextSnapshotScheduleTimeStatus =
        Prelude.Nothing,
      hsmStatus = Prelude.Nothing,
      iamRoles = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      loggingStatus = Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing,
      manualSnapshotRetentionPeriod = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      nextMaintenanceWindowStartTime = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      pendingActions = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      resizeInfo = Prelude.Nothing,
      restoreStatus = Prelude.Nothing,
      snapshotScheduleIdentifier = Prelude.Nothing,
      snapshotScheduleState = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing
    }

-- | Indicates whether major version upgrades are applied automatically to
-- the cluster during the maintenance window.
awsRedshiftClusterDetails_allowVersionUpgrade :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_allowVersionUpgrade = Lens.lens (\AwsRedshiftClusterDetails' {allowVersionUpgrade} -> allowVersionUpgrade) (\s@AwsRedshiftClusterDetails' {} a -> s {allowVersionUpgrade = a} :: AwsRedshiftClusterDetails)

-- | The number of days that automatic cluster snapshots are retained.
awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Int)
awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod = Lens.lens (\AwsRedshiftClusterDetails' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@AwsRedshiftClusterDetails' {} a -> s {automatedSnapshotRetentionPeriod = a} :: AwsRedshiftClusterDetails)

-- | The name of the Availability Zone in which the cluster is located.
awsRedshiftClusterDetails_availabilityZone :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_availabilityZone = Lens.lens (\AwsRedshiftClusterDetails' {availabilityZone} -> availabilityZone) (\s@AwsRedshiftClusterDetails' {} a -> s {availabilityZone = a} :: AwsRedshiftClusterDetails)

-- | The availability status of the cluster for queries. Possible values are
-- the following:
--
-- -   @Available@ - The cluster is available for queries.
--
-- -   @Unavailable@ - The cluster is not available for queries.
--
-- -   @Maintenance@ - The cluster is intermittently available for queries
--     due to maintenance activities.
--
-- -   @Modifying@ -The cluster is intermittently available for queries due
--     to changes that modify the cluster.
--
-- -   @Failed@ - The cluster failed and is not available for queries.
awsRedshiftClusterDetails_clusterAvailabilityStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterAvailabilityStatus = Lens.lens (\AwsRedshiftClusterDetails' {clusterAvailabilityStatus} -> clusterAvailabilityStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterAvailabilityStatus = a} :: AwsRedshiftClusterDetails)

-- | Indicates when the cluster was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDetails_clusterCreateTime :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterCreateTime = Lens.lens (\AwsRedshiftClusterDetails' {clusterCreateTime} -> clusterCreateTime) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterCreateTime = a} :: AwsRedshiftClusterDetails)

-- | The unique identifier of the cluster.
awsRedshiftClusterDetails_clusterIdentifier :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterIdentifier = Lens.lens (\AwsRedshiftClusterDetails' {clusterIdentifier} -> clusterIdentifier) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterIdentifier = a} :: AwsRedshiftClusterDetails)

-- | The nodes in the cluster.
awsRedshiftClusterDetails_clusterNodes :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterClusterNode])
awsRedshiftClusterDetails_clusterNodes = Lens.lens (\AwsRedshiftClusterDetails' {clusterNodes} -> clusterNodes) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterNodes = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The list of cluster parameter groups that are associated with this
-- cluster.
awsRedshiftClusterDetails_clusterParameterGroups :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterClusterParameterGroup])
awsRedshiftClusterDetails_clusterParameterGroups = Lens.lens (\AwsRedshiftClusterDetails' {clusterParameterGroups} -> clusterParameterGroups) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterParameterGroups = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The public key for the cluster.
awsRedshiftClusterDetails_clusterPublicKey :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterPublicKey = Lens.lens (\AwsRedshiftClusterDetails' {clusterPublicKey} -> clusterPublicKey) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterPublicKey = a} :: AwsRedshiftClusterDetails)

-- | The specific revision number of the database in the cluster.
awsRedshiftClusterDetails_clusterRevisionNumber :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterRevisionNumber = Lens.lens (\AwsRedshiftClusterDetails' {clusterRevisionNumber} -> clusterRevisionNumber) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterRevisionNumber = a} :: AwsRedshiftClusterDetails)

-- | A list of cluster security groups that are associated with the cluster.
awsRedshiftClusterDetails_clusterSecurityGroups :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterClusterSecurityGroup])
awsRedshiftClusterDetails_clusterSecurityGroups = Lens.lens (\AwsRedshiftClusterDetails' {clusterSecurityGroups} -> clusterSecurityGroups) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterSecurityGroups = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the destination Region and retention period for the
-- cross-Region snapshot copy.
awsRedshiftClusterDetails_clusterSnapshotCopyStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterClusterSnapshotCopyStatus)
awsRedshiftClusterDetails_clusterSnapshotCopyStatus = Lens.lens (\AwsRedshiftClusterDetails' {clusterSnapshotCopyStatus} -> clusterSnapshotCopyStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterSnapshotCopyStatus = a} :: AwsRedshiftClusterDetails)

-- | The current status of the cluster.
--
-- Valid values: @available@ | @available, prep-for-resize@ |
-- @available, resize-cleanup@ |@ cancelling-resize@ | @creating@ |
-- @deleting@ | @final-snapshot@ | @hardware-failure@ | @incompatible-hsm@
-- |@ incompatible-network@ | @incompatible-parameters@ |
-- @incompatible-restore@ | @modifying@ | @paused@ | @rebooting@ |
-- @renaming@ | @resizing@ | @rotating-keys@ | @storage-full@ |
-- @updating-hsm@
awsRedshiftClusterDetails_clusterStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterStatus = Lens.lens (\AwsRedshiftClusterDetails' {clusterStatus} -> clusterStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterStatus = a} :: AwsRedshiftClusterDetails)

-- | The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
awsRedshiftClusterDetails_clusterSubnetGroupName :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterSubnetGroupName = Lens.lens (\AwsRedshiftClusterDetails' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterSubnetGroupName = a} :: AwsRedshiftClusterDetails)

-- | The version ID of the Amazon Redshift engine that runs on the cluster.
awsRedshiftClusterDetails_clusterVersion :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterVersion = Lens.lens (\AwsRedshiftClusterDetails' {clusterVersion} -> clusterVersion) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterVersion = a} :: AwsRedshiftClusterDetails)

-- | The name of the initial database that was created when the cluster was
-- created.
--
-- The same name is returned for the life of the cluster.
--
-- If an initial database is not specified, a database named @devdev@ is
-- created by default.
awsRedshiftClusterDetails_dbName :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_dbName = Lens.lens (\AwsRedshiftClusterDetails' {dbName} -> dbName) (\s@AwsRedshiftClusterDetails' {} a -> s {dbName = a} :: AwsRedshiftClusterDetails)

-- | List of time windows during which maintenance was deferred.
awsRedshiftClusterDetails_deferredMaintenanceWindows :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterDeferredMaintenanceWindow])
awsRedshiftClusterDetails_deferredMaintenanceWindows = Lens.lens (\AwsRedshiftClusterDetails' {deferredMaintenanceWindows} -> deferredMaintenanceWindows) (\s@AwsRedshiftClusterDetails' {} a -> s {deferredMaintenanceWindows = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the status of the Elastic IP (EIP) address.
awsRedshiftClusterDetails_elasticIpStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterElasticIpStatus)
awsRedshiftClusterDetails_elasticIpStatus = Lens.lens (\AwsRedshiftClusterDetails' {elasticIpStatus} -> elasticIpStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {elasticIpStatus = a} :: AwsRedshiftClusterDetails)

-- | The number of nodes that you can use the elastic resize method to resize
-- the cluster to.
awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions = Lens.lens (\AwsRedshiftClusterDetails' {elasticResizeNumberOfNodeOptions} -> elasticResizeNumberOfNodeOptions) (\s@AwsRedshiftClusterDetails' {} a -> s {elasticResizeNumberOfNodeOptions = a} :: AwsRedshiftClusterDetails)

-- | Indicates whether the data in the cluster is encrypted at rest.
awsRedshiftClusterDetails_encrypted :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_encrypted = Lens.lens (\AwsRedshiftClusterDetails' {encrypted} -> encrypted) (\s@AwsRedshiftClusterDetails' {} a -> s {encrypted = a} :: AwsRedshiftClusterDetails)

-- | The connection endpoint.
awsRedshiftClusterDetails_endpoint :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterEndpoint)
awsRedshiftClusterDetails_endpoint = Lens.lens (\AwsRedshiftClusterDetails' {endpoint} -> endpoint) (\s@AwsRedshiftClusterDetails' {} a -> s {endpoint = a} :: AwsRedshiftClusterDetails)

-- | Indicates whether to create the cluster with enhanced VPC routing
-- enabled.
awsRedshiftClusterDetails_enhancedVpcRouting :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_enhancedVpcRouting = Lens.lens (\AwsRedshiftClusterDetails' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@AwsRedshiftClusterDetails' {} a -> s {enhancedVpcRouting = a} :: AwsRedshiftClusterDetails)

-- | Indicates when the next snapshot is expected to be taken. The cluster
-- must have a valid snapshot schedule and have backups enabled.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime = Lens.lens (\AwsRedshiftClusterDetails' {expectedNextSnapshotScheduleTime} -> expectedNextSnapshotScheduleTime) (\s@AwsRedshiftClusterDetails' {} a -> s {expectedNextSnapshotScheduleTime = a} :: AwsRedshiftClusterDetails)

-- | The status of the next expected snapshot.
--
-- Valid values: @OnTrack@ | @Pending@
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus = Lens.lens (\AwsRedshiftClusterDetails' {expectedNextSnapshotScheduleTimeStatus} -> expectedNextSnapshotScheduleTimeStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {expectedNextSnapshotScheduleTimeStatus = a} :: AwsRedshiftClusterDetails)

-- | Information about whether the Amazon Redshift cluster finished applying
-- any changes to hardware security module (HSM) settings that were
-- specified in a modify cluster command.
awsRedshiftClusterDetails_hsmStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterHsmStatus)
awsRedshiftClusterDetails_hsmStatus = Lens.lens (\AwsRedshiftClusterDetails' {hsmStatus} -> hsmStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {hsmStatus = a} :: AwsRedshiftClusterDetails)

-- | A list of IAM roles that the cluster can use to access other Amazon Web
-- Services services.
awsRedshiftClusterDetails_iamRoles :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterIamRole])
awsRedshiftClusterDetails_iamRoles = Lens.lens (\AwsRedshiftClusterDetails' {iamRoles} -> iamRoles) (\s@AwsRedshiftClusterDetails' {} a -> s {iamRoles = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the KMS encryption key that is used to encrypt data in
-- the cluster.
awsRedshiftClusterDetails_kmsKeyId :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_kmsKeyId = Lens.lens (\AwsRedshiftClusterDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsRedshiftClusterDetails' {} a -> s {kmsKeyId = a} :: AwsRedshiftClusterDetails)

-- | Information about the logging status of the cluster.
awsRedshiftClusterDetails_loggingStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterLoggingStatus)
awsRedshiftClusterDetails_loggingStatus = Lens.lens (\AwsRedshiftClusterDetails' {loggingStatus} -> loggingStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {loggingStatus = a} :: AwsRedshiftClusterDetails)

-- | The name of the maintenance track for the cluster.
awsRedshiftClusterDetails_maintenanceTrackName :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_maintenanceTrackName = Lens.lens (\AwsRedshiftClusterDetails' {maintenanceTrackName} -> maintenanceTrackName) (\s@AwsRedshiftClusterDetails' {} a -> s {maintenanceTrackName = a} :: AwsRedshiftClusterDetails)

-- | The default number of days to retain a manual snapshot.
--
-- If the value is @-1@, the snapshot is retained indefinitely.
--
-- This setting doesn\'t change the retention period of existing snapshots.
--
-- Valid values: Either @-1@ or an integer between 1 and 3,653
awsRedshiftClusterDetails_manualSnapshotRetentionPeriod :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Int)
awsRedshiftClusterDetails_manualSnapshotRetentionPeriod = Lens.lens (\AwsRedshiftClusterDetails' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@AwsRedshiftClusterDetails' {} a -> s {manualSnapshotRetentionPeriod = a} :: AwsRedshiftClusterDetails)

-- | The master user name for the cluster. This name is used to connect to
-- the database that is specified in as the value of @DBName@.
awsRedshiftClusterDetails_masterUsername :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_masterUsername = Lens.lens (\AwsRedshiftClusterDetails' {masterUsername} -> masterUsername) (\s@AwsRedshiftClusterDetails' {} a -> s {masterUsername = a} :: AwsRedshiftClusterDetails)

-- | Indicates the start of the next maintenance window.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDetails_nextMaintenanceWindowStartTime :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_nextMaintenanceWindowStartTime = Lens.lens (\AwsRedshiftClusterDetails' {nextMaintenanceWindowStartTime} -> nextMaintenanceWindowStartTime) (\s@AwsRedshiftClusterDetails' {} a -> s {nextMaintenanceWindowStartTime = a} :: AwsRedshiftClusterDetails)

-- | The node type for the nodes in the cluster.
awsRedshiftClusterDetails_nodeType :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_nodeType = Lens.lens (\AwsRedshiftClusterDetails' {nodeType} -> nodeType) (\s@AwsRedshiftClusterDetails' {} a -> s {nodeType = a} :: AwsRedshiftClusterDetails)

-- | The number of compute nodes in the cluster.
awsRedshiftClusterDetails_numberOfNodes :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Int)
awsRedshiftClusterDetails_numberOfNodes = Lens.lens (\AwsRedshiftClusterDetails' {numberOfNodes} -> numberOfNodes) (\s@AwsRedshiftClusterDetails' {} a -> s {numberOfNodes = a} :: AwsRedshiftClusterDetails)

-- | A list of cluster operations that are waiting to start.
awsRedshiftClusterDetails_pendingActions :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [Prelude.Text])
awsRedshiftClusterDetails_pendingActions = Lens.lens (\AwsRedshiftClusterDetails' {pendingActions} -> pendingActions) (\s@AwsRedshiftClusterDetails' {} a -> s {pendingActions = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of changes to the cluster that are currently pending.
awsRedshiftClusterDetails_pendingModifiedValues :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterPendingModifiedValues)
awsRedshiftClusterDetails_pendingModifiedValues = Lens.lens (\AwsRedshiftClusterDetails' {pendingModifiedValues} -> pendingModifiedValues) (\s@AwsRedshiftClusterDetails' {} a -> s {pendingModifiedValues = a} :: AwsRedshiftClusterDetails)

-- | The weekly time range, in Universal Coordinated Time (UTC), during which
-- system maintenance can occur.
--
-- Format: @ @/@\<day>@/@:HH:MM-@/@\<day>@/@:HH:MM@
--
-- For the day values, use @mon@ | @tue@ | @wed@ | @thu@ | @fri@ | @sat@ |
-- @sun@
--
-- For example, @sun:09:32-sun:10:02@
awsRedshiftClusterDetails_preferredMaintenanceWindow :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_preferredMaintenanceWindow = Lens.lens (\AwsRedshiftClusterDetails' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@AwsRedshiftClusterDetails' {} a -> s {preferredMaintenanceWindow = a} :: AwsRedshiftClusterDetails)

-- | Whether the cluster can be accessed from a public network.
awsRedshiftClusterDetails_publiclyAccessible :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_publiclyAccessible = Lens.lens (\AwsRedshiftClusterDetails' {publiclyAccessible} -> publiclyAccessible) (\s@AwsRedshiftClusterDetails' {} a -> s {publiclyAccessible = a} :: AwsRedshiftClusterDetails)

-- | Information about the resize operation for the cluster.
awsRedshiftClusterDetails_resizeInfo :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterResizeInfo)
awsRedshiftClusterDetails_resizeInfo = Lens.lens (\AwsRedshiftClusterDetails' {resizeInfo} -> resizeInfo) (\s@AwsRedshiftClusterDetails' {} a -> s {resizeInfo = a} :: AwsRedshiftClusterDetails)

-- | Information about the status of a cluster restore action. Only applies
-- to a cluster that was created by restoring a snapshot.
awsRedshiftClusterDetails_restoreStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterRestoreStatus)
awsRedshiftClusterDetails_restoreStatus = Lens.lens (\AwsRedshiftClusterDetails' {restoreStatus} -> restoreStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {restoreStatus = a} :: AwsRedshiftClusterDetails)

-- | A unique identifier for the cluster snapshot schedule.
awsRedshiftClusterDetails_snapshotScheduleIdentifier :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_snapshotScheduleIdentifier = Lens.lens (\AwsRedshiftClusterDetails' {snapshotScheduleIdentifier} -> snapshotScheduleIdentifier) (\s@AwsRedshiftClusterDetails' {} a -> s {snapshotScheduleIdentifier = a} :: AwsRedshiftClusterDetails)

-- | The current state of the cluster snapshot schedule.
--
-- Valid values: @MODIFYING@ | @ACTIVE@ | @FAILED@
awsRedshiftClusterDetails_snapshotScheduleState :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_snapshotScheduleState = Lens.lens (\AwsRedshiftClusterDetails' {snapshotScheduleState} -> snapshotScheduleState) (\s@AwsRedshiftClusterDetails' {} a -> s {snapshotScheduleState = a} :: AwsRedshiftClusterDetails)

-- | The identifier of the VPC that the cluster is in, if the cluster is in a
-- VPC.
awsRedshiftClusterDetails_vpcId :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_vpcId = Lens.lens (\AwsRedshiftClusterDetails' {vpcId} -> vpcId) (\s@AwsRedshiftClusterDetails' {} a -> s {vpcId = a} :: AwsRedshiftClusterDetails)

-- | The list of VPC security groups that the cluster belongs to, if the
-- cluster is in a VPC.
awsRedshiftClusterDetails_vpcSecurityGroups :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterVpcSecurityGroup])
awsRedshiftClusterDetails_vpcSecurityGroups = Lens.lens (\AwsRedshiftClusterDetails' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@AwsRedshiftClusterDetails' {} a -> s {vpcSecurityGroups = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsRedshiftClusterDetails where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterDetails"
      ( \x ->
          AwsRedshiftClusterDetails'
            Prelude.<$> (x Data..:? "AllowVersionUpgrade")
            Prelude.<*> (x Data..:? "AutomatedSnapshotRetentionPeriod")
            Prelude.<*> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "ClusterAvailabilityStatus")
            Prelude.<*> (x Data..:? "ClusterCreateTime")
            Prelude.<*> (x Data..:? "ClusterIdentifier")
            Prelude.<*> (x Data..:? "ClusterNodes" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "ClusterParameterGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ClusterPublicKey")
            Prelude.<*> (x Data..:? "ClusterRevisionNumber")
            Prelude.<*> ( x
                            Data..:? "ClusterSecurityGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ClusterSnapshotCopyStatus")
            Prelude.<*> (x Data..:? "ClusterStatus")
            Prelude.<*> (x Data..:? "ClusterSubnetGroupName")
            Prelude.<*> (x Data..:? "ClusterVersion")
            Prelude.<*> (x Data..:? "DBName")
            Prelude.<*> ( x
                            Data..:? "DeferredMaintenanceWindows"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ElasticIpStatus")
            Prelude.<*> (x Data..:? "ElasticResizeNumberOfNodeOptions")
            Prelude.<*> (x Data..:? "Encrypted")
            Prelude.<*> (x Data..:? "Endpoint")
            Prelude.<*> (x Data..:? "EnhancedVpcRouting")
            Prelude.<*> (x Data..:? "ExpectedNextSnapshotScheduleTime")
            Prelude.<*> (x Data..:? "ExpectedNextSnapshotScheduleTimeStatus")
            Prelude.<*> (x Data..:? "HsmStatus")
            Prelude.<*> (x Data..:? "IamRoles" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "LoggingStatus")
            Prelude.<*> (x Data..:? "MaintenanceTrackName")
            Prelude.<*> (x Data..:? "ManualSnapshotRetentionPeriod")
            Prelude.<*> (x Data..:? "MasterUsername")
            Prelude.<*> (x Data..:? "NextMaintenanceWindowStartTime")
            Prelude.<*> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "NumberOfNodes")
            Prelude.<*> (x Data..:? "PendingActions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PendingModifiedValues")
            Prelude.<*> (x Data..:? "PreferredMaintenanceWindow")
            Prelude.<*> (x Data..:? "PubliclyAccessible")
            Prelude.<*> (x Data..:? "ResizeInfo")
            Prelude.<*> (x Data..:? "RestoreStatus")
            Prelude.<*> (x Data..:? "SnapshotScheduleIdentifier")
            Prelude.<*> (x Data..:? "SnapshotScheduleState")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> ( x
                            Data..:? "VpcSecurityGroups"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsRedshiftClusterDetails where
  hashWithSalt _salt AwsRedshiftClusterDetails' {..} =
    _salt
      `Prelude.hashWithSalt` allowVersionUpgrade
      `Prelude.hashWithSalt` automatedSnapshotRetentionPeriod
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` clusterAvailabilityStatus
      `Prelude.hashWithSalt` clusterCreateTime
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` clusterNodes
      `Prelude.hashWithSalt` clusterParameterGroups
      `Prelude.hashWithSalt` clusterPublicKey
      `Prelude.hashWithSalt` clusterRevisionNumber
      `Prelude.hashWithSalt` clusterSecurityGroups
      `Prelude.hashWithSalt` clusterSnapshotCopyStatus
      `Prelude.hashWithSalt` clusterStatus
      `Prelude.hashWithSalt` clusterSubnetGroupName
      `Prelude.hashWithSalt` clusterVersion
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` deferredMaintenanceWindows
      `Prelude.hashWithSalt` elasticIpStatus
      `Prelude.hashWithSalt` elasticResizeNumberOfNodeOptions
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` enhancedVpcRouting
      `Prelude.hashWithSalt` expectedNextSnapshotScheduleTime
      `Prelude.hashWithSalt` expectedNextSnapshotScheduleTimeStatus
      `Prelude.hashWithSalt` hsmStatus
      `Prelude.hashWithSalt` iamRoles
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` loggingStatus
      `Prelude.hashWithSalt` maintenanceTrackName
      `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` nextMaintenanceWindowStartTime
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` pendingActions
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` resizeInfo
      `Prelude.hashWithSalt` restoreStatus
      `Prelude.hashWithSalt` snapshotScheduleIdentifier
      `Prelude.hashWithSalt` snapshotScheduleState
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpcSecurityGroups

instance Prelude.NFData AwsRedshiftClusterDetails where
  rnf AwsRedshiftClusterDetails' {..} =
    Prelude.rnf allowVersionUpgrade
      `Prelude.seq` Prelude.rnf automatedSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf clusterAvailabilityStatus
      `Prelude.seq` Prelude.rnf clusterCreateTime
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf clusterNodes
      `Prelude.seq` Prelude.rnf clusterParameterGroups
      `Prelude.seq` Prelude.rnf clusterPublicKey
      `Prelude.seq` Prelude.rnf clusterRevisionNumber
      `Prelude.seq` Prelude.rnf clusterSecurityGroups
      `Prelude.seq` Prelude.rnf clusterSnapshotCopyStatus
      `Prelude.seq` Prelude.rnf clusterStatus
      `Prelude.seq` Prelude.rnf clusterSubnetGroupName
      `Prelude.seq` Prelude.rnf clusterVersion
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf
        deferredMaintenanceWindows
      `Prelude.seq` Prelude.rnf elasticIpStatus
      `Prelude.seq` Prelude.rnf
        elasticResizeNumberOfNodeOptions
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf
        enhancedVpcRouting
      `Prelude.seq` Prelude.rnf
        expectedNextSnapshotScheduleTime
      `Prelude.seq` Prelude.rnf
        expectedNextSnapshotScheduleTimeStatus
      `Prelude.seq` Prelude.rnf
        hsmStatus
      `Prelude.seq` Prelude.rnf
        iamRoles
      `Prelude.seq` Prelude.rnf
        kmsKeyId
      `Prelude.seq` Prelude.rnf
        loggingStatus
      `Prelude.seq` Prelude.rnf
        maintenanceTrackName
      `Prelude.seq` Prelude.rnf
        manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf
        masterUsername
      `Prelude.seq` Prelude.rnf
        nextMaintenanceWindowStartTime
      `Prelude.seq` Prelude.rnf
        nodeType
      `Prelude.seq` Prelude.rnf
        numberOfNodes
      `Prelude.seq` Prelude.rnf
        pendingActions
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        resizeInfo
      `Prelude.seq` Prelude.rnf
        restoreStatus
      `Prelude.seq` Prelude.rnf
        snapshotScheduleIdentifier
      `Prelude.seq` Prelude.rnf
        snapshotScheduleState
      `Prelude.seq` Prelude.rnf
        vpcId
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroups

instance Data.ToJSON AwsRedshiftClusterDetails where
  toJSON AwsRedshiftClusterDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowVersionUpgrade" Data..=)
              Prelude.<$> allowVersionUpgrade,
            ("AutomatedSnapshotRetentionPeriod" Data..=)
              Prelude.<$> automatedSnapshotRetentionPeriod,
            ("AvailabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("ClusterAvailabilityStatus" Data..=)
              Prelude.<$> clusterAvailabilityStatus,
            ("ClusterCreateTime" Data..=)
              Prelude.<$> clusterCreateTime,
            ("ClusterIdentifier" Data..=)
              Prelude.<$> clusterIdentifier,
            ("ClusterNodes" Data..=) Prelude.<$> clusterNodes,
            ("ClusterParameterGroups" Data..=)
              Prelude.<$> clusterParameterGroups,
            ("ClusterPublicKey" Data..=)
              Prelude.<$> clusterPublicKey,
            ("ClusterRevisionNumber" Data..=)
              Prelude.<$> clusterRevisionNumber,
            ("ClusterSecurityGroups" Data..=)
              Prelude.<$> clusterSecurityGroups,
            ("ClusterSnapshotCopyStatus" Data..=)
              Prelude.<$> clusterSnapshotCopyStatus,
            ("ClusterStatus" Data..=) Prelude.<$> clusterStatus,
            ("ClusterSubnetGroupName" Data..=)
              Prelude.<$> clusterSubnetGroupName,
            ("ClusterVersion" Data..=)
              Prelude.<$> clusterVersion,
            ("DBName" Data..=) Prelude.<$> dbName,
            ("DeferredMaintenanceWindows" Data..=)
              Prelude.<$> deferredMaintenanceWindows,
            ("ElasticIpStatus" Data..=)
              Prelude.<$> elasticIpStatus,
            ("ElasticResizeNumberOfNodeOptions" Data..=)
              Prelude.<$> elasticResizeNumberOfNodeOptions,
            ("Encrypted" Data..=) Prelude.<$> encrypted,
            ("Endpoint" Data..=) Prelude.<$> endpoint,
            ("EnhancedVpcRouting" Data..=)
              Prelude.<$> enhancedVpcRouting,
            ("ExpectedNextSnapshotScheduleTime" Data..=)
              Prelude.<$> expectedNextSnapshotScheduleTime,
            ("ExpectedNextSnapshotScheduleTimeStatus" Data..=)
              Prelude.<$> expectedNextSnapshotScheduleTimeStatus,
            ("HsmStatus" Data..=) Prelude.<$> hsmStatus,
            ("IamRoles" Data..=) Prelude.<$> iamRoles,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("LoggingStatus" Data..=) Prelude.<$> loggingStatus,
            ("MaintenanceTrackName" Data..=)
              Prelude.<$> maintenanceTrackName,
            ("ManualSnapshotRetentionPeriod" Data..=)
              Prelude.<$> manualSnapshotRetentionPeriod,
            ("MasterUsername" Data..=)
              Prelude.<$> masterUsername,
            ("NextMaintenanceWindowStartTime" Data..=)
              Prelude.<$> nextMaintenanceWindowStartTime,
            ("NodeType" Data..=) Prelude.<$> nodeType,
            ("NumberOfNodes" Data..=) Prelude.<$> numberOfNodes,
            ("PendingActions" Data..=)
              Prelude.<$> pendingActions,
            ("PendingModifiedValues" Data..=)
              Prelude.<$> pendingModifiedValues,
            ("PreferredMaintenanceWindow" Data..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("PubliclyAccessible" Data..=)
              Prelude.<$> publiclyAccessible,
            ("ResizeInfo" Data..=) Prelude.<$> resizeInfo,
            ("RestoreStatus" Data..=) Prelude.<$> restoreStatus,
            ("SnapshotScheduleIdentifier" Data..=)
              Prelude.<$> snapshotScheduleIdentifier,
            ("SnapshotScheduleState" Data..=)
              Prelude.<$> snapshotScheduleState,
            ("VpcId" Data..=) Prelude.<$> vpcId,
            ("VpcSecurityGroups" Data..=)
              Prelude.<$> vpcSecurityGroups
          ]
      )
