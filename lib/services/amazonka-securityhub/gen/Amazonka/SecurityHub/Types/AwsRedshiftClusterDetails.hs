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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The unique identifier of the cluster.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The public key for the cluster.
    clusterPublicKey :: Prelude.Maybe Prelude.Text,
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
    -- | The list of cluster parameter groups that are associated with this
    -- cluster.
    clusterParameterGroups :: Prelude.Maybe [AwsRedshiftClusterClusterParameterGroup],
    -- | Indicates whether major version upgrades are applied automatically to
    -- the cluster during the maintenance window.
    allowVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The status of the next expected snapshot.
    --
    -- Valid values: @OnTrack@ | @Pending@
    expectedNextSnapshotScheduleTimeStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the subnet group that is associated with the cluster. This
    -- parameter is valid only when the cluster is in a VPC.
    clusterSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the cluster snapshot schedule.
    snapshotScheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Indicates the start of the next maintenance window.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    nextMaintenanceWindowStartTime :: Prelude.Maybe Prelude.Text,
    -- | Information about the status of the Elastic IP (EIP) address.
    elasticIpStatus :: Prelude.Maybe AwsRedshiftClusterElasticIpStatus,
    -- | The nodes in the cluster.
    clusterNodes :: Prelude.Maybe [AwsRedshiftClusterClusterNode],
    -- | The version ID of the Amazon Redshift engine that runs on the cluster.
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | Information about the logging status of the cluster.
    loggingStatus :: Prelude.Maybe AwsRedshiftClusterLoggingStatus,
    -- | A list of cluster operations that are waiting to start.
    pendingActions :: Prelude.Maybe [Prelude.Text],
    -- | The specific revision number of the database in the cluster.
    clusterRevisionNumber :: Prelude.Maybe Prelude.Text,
    -- | The name of the maintenance track for the cluster.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | A list of IAM roles that the cluster can use to access other Amazon Web
    -- Services services.
    iamRoles :: Prelude.Maybe [AwsRedshiftClusterIamRole],
    -- | The name of the Availability Zone in which the cluster is located.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The node type for the nodes in the cluster.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | Whether the cluster can be accessed from a public network.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Information about the destination Region and retention period for the
    -- cross-Region snapshot copy.
    clusterSnapshotCopyStatus :: Prelude.Maybe AwsRedshiftClusterClusterSnapshotCopyStatus,
    -- | The current state of the cluster snapshot schedule.
    --
    -- Valid values: @MODIFYING@ | @ACTIVE@ | @FAILED@
    snapshotScheduleState :: Prelude.Maybe Prelude.Text,
    -- | Information about whether the Amazon Redshift cluster finished applying
    -- any changes to hardware security module (HSM) settings that were
    -- specified in a modify cluster command.
    hsmStatus :: Prelude.Maybe AwsRedshiftClusterHsmStatus,
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
    -- | Information about the resize operation for the cluster.
    resizeInfo :: Prelude.Maybe AwsRedshiftClusterResizeInfo,
    -- | List of time windows during which maintenance was deferred.
    deferredMaintenanceWindows :: Prelude.Maybe [AwsRedshiftClusterDeferredMaintenanceWindow],
    -- | Indicates whether the data in the cluster is encrypted at rest.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The number of compute nodes in the cluster.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the KMS encryption key that is used to encrypt data in
    -- the cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the next snapshot is expected to be taken. The cluster
    -- must have a valid snapshot schedule and have backups enabled.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    expectedNextSnapshotScheduleTime :: Prelude.Maybe Prelude.Text,
    -- | The number of nodes that you can use the elastic resize method to resize
    -- the cluster to.
    elasticResizeNumberOfNodeOptions :: Prelude.Maybe Prelude.Text,
    -- | A list of changes to the cluster that are currently pending.
    pendingModifiedValues :: Prelude.Maybe AwsRedshiftClusterPendingModifiedValues,
    -- | Indicates whether to create the cluster with enhanced VPC routing
    -- enabled.
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | The weekly time range, in Universal Coordinated Time (UTC), during which
    -- system maintenance can occur.
    --
    -- Format: @ \<day>:HH:MM-\<day>:HH:MM@
    --
    -- For the day values, use @mon@ | @tue@ | @wed@ | @thu@ | @fri@ | @sat@ |
    -- @sun@
    --
    -- For example, @sun:09:32-sun:10:02@
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A list of cluster security groups that are associated with the cluster.
    clusterSecurityGroups :: Prelude.Maybe [AwsRedshiftClusterClusterSecurityGroup],
    -- | The connection endpoint.
    endpoint :: Prelude.Maybe AwsRedshiftClusterEndpoint,
    -- | The identifier of the VPC that the cluster is in, if the cluster is in a
    -- VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The number of days that automatic cluster snapshots are retained.
    automatedSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Indicates when the cluster was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    clusterCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the initial database that was created when the cluster was
    -- created.
    --
    -- The same name is returned for the life of the cluster.
    --
    -- If an initial database is not specified, a database named @devdev@ is
    -- created by default.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | Information about the status of a cluster restore action. Only applies
    -- to a cluster that was created by restoring a snapshot.
    restoreStatus :: Prelude.Maybe AwsRedshiftClusterRestoreStatus,
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
-- 'clusterIdentifier', 'awsRedshiftClusterDetails_clusterIdentifier' - The unique identifier of the cluster.
--
-- 'clusterPublicKey', 'awsRedshiftClusterDetails_clusterPublicKey' - The public key for the cluster.
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
-- 'clusterParameterGroups', 'awsRedshiftClusterDetails_clusterParameterGroups' - The list of cluster parameter groups that are associated with this
-- cluster.
--
-- 'allowVersionUpgrade', 'awsRedshiftClusterDetails_allowVersionUpgrade' - Indicates whether major version upgrades are applied automatically to
-- the cluster during the maintenance window.
--
-- 'expectedNextSnapshotScheduleTimeStatus', 'awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus' - The status of the next expected snapshot.
--
-- Valid values: @OnTrack@ | @Pending@
--
-- 'clusterSubnetGroupName', 'awsRedshiftClusterDetails_clusterSubnetGroupName' - The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
--
-- 'snapshotScheduleIdentifier', 'awsRedshiftClusterDetails_snapshotScheduleIdentifier' - A unique identifier for the cluster snapshot schedule.
--
-- 'nextMaintenanceWindowStartTime', 'awsRedshiftClusterDetails_nextMaintenanceWindowStartTime' - Indicates the start of the next maintenance window.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'elasticIpStatus', 'awsRedshiftClusterDetails_elasticIpStatus' - Information about the status of the Elastic IP (EIP) address.
--
-- 'clusterNodes', 'awsRedshiftClusterDetails_clusterNodes' - The nodes in the cluster.
--
-- 'clusterVersion', 'awsRedshiftClusterDetails_clusterVersion' - The version ID of the Amazon Redshift engine that runs on the cluster.
--
-- 'loggingStatus', 'awsRedshiftClusterDetails_loggingStatus' - Information about the logging status of the cluster.
--
-- 'pendingActions', 'awsRedshiftClusterDetails_pendingActions' - A list of cluster operations that are waiting to start.
--
-- 'clusterRevisionNumber', 'awsRedshiftClusterDetails_clusterRevisionNumber' - The specific revision number of the database in the cluster.
--
-- 'maintenanceTrackName', 'awsRedshiftClusterDetails_maintenanceTrackName' - The name of the maintenance track for the cluster.
--
-- 'iamRoles', 'awsRedshiftClusterDetails_iamRoles' - A list of IAM roles that the cluster can use to access other Amazon Web
-- Services services.
--
-- 'availabilityZone', 'awsRedshiftClusterDetails_availabilityZone' - The name of the Availability Zone in which the cluster is located.
--
-- 'nodeType', 'awsRedshiftClusterDetails_nodeType' - The node type for the nodes in the cluster.
--
-- 'publiclyAccessible', 'awsRedshiftClusterDetails_publiclyAccessible' - Whether the cluster can be accessed from a public network.
--
-- 'clusterSnapshotCopyStatus', 'awsRedshiftClusterDetails_clusterSnapshotCopyStatus' - Information about the destination Region and retention period for the
-- cross-Region snapshot copy.
--
-- 'snapshotScheduleState', 'awsRedshiftClusterDetails_snapshotScheduleState' - The current state of the cluster snapshot schedule.
--
-- Valid values: @MODIFYING@ | @ACTIVE@ | @FAILED@
--
-- 'hsmStatus', 'awsRedshiftClusterDetails_hsmStatus' - Information about whether the Amazon Redshift cluster finished applying
-- any changes to hardware security module (HSM) settings that were
-- specified in a modify cluster command.
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
-- 'resizeInfo', 'awsRedshiftClusterDetails_resizeInfo' - Information about the resize operation for the cluster.
--
-- 'deferredMaintenanceWindows', 'awsRedshiftClusterDetails_deferredMaintenanceWindows' - List of time windows during which maintenance was deferred.
--
-- 'encrypted', 'awsRedshiftClusterDetails_encrypted' - Indicates whether the data in the cluster is encrypted at rest.
--
-- 'numberOfNodes', 'awsRedshiftClusterDetails_numberOfNodes' - The number of compute nodes in the cluster.
--
-- 'kmsKeyId', 'awsRedshiftClusterDetails_kmsKeyId' - The identifier of the KMS encryption key that is used to encrypt data in
-- the cluster.
--
-- 'expectedNextSnapshotScheduleTime', 'awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime' - Indicates when the next snapshot is expected to be taken. The cluster
-- must have a valid snapshot schedule and have backups enabled.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'elasticResizeNumberOfNodeOptions', 'awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions' - The number of nodes that you can use the elastic resize method to resize
-- the cluster to.
--
-- 'pendingModifiedValues', 'awsRedshiftClusterDetails_pendingModifiedValues' - A list of changes to the cluster that are currently pending.
--
-- 'enhancedVpcRouting', 'awsRedshiftClusterDetails_enhancedVpcRouting' - Indicates whether to create the cluster with enhanced VPC routing
-- enabled.
--
-- 'preferredMaintenanceWindow', 'awsRedshiftClusterDetails_preferredMaintenanceWindow' - The weekly time range, in Universal Coordinated Time (UTC), during which
-- system maintenance can occur.
--
-- Format: @ \<day>:HH:MM-\<day>:HH:MM@
--
-- For the day values, use @mon@ | @tue@ | @wed@ | @thu@ | @fri@ | @sat@ |
-- @sun@
--
-- For example, @sun:09:32-sun:10:02@
--
-- 'clusterSecurityGroups', 'awsRedshiftClusterDetails_clusterSecurityGroups' - A list of cluster security groups that are associated with the cluster.
--
-- 'endpoint', 'awsRedshiftClusterDetails_endpoint' - The connection endpoint.
--
-- 'vpcId', 'awsRedshiftClusterDetails_vpcId' - The identifier of the VPC that the cluster is in, if the cluster is in a
-- VPC.
--
-- 'automatedSnapshotRetentionPeriod', 'awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod' - The number of days that automatic cluster snapshots are retained.
--
-- 'clusterCreateTime', 'awsRedshiftClusterDetails_clusterCreateTime' - Indicates when the cluster was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'dbName', 'awsRedshiftClusterDetails_dbName' - The name of the initial database that was created when the cluster was
-- created.
--
-- The same name is returned for the life of the cluster.
--
-- If an initial database is not specified, a database named @devdev@ is
-- created by default.
--
-- 'restoreStatus', 'awsRedshiftClusterDetails_restoreStatus' - Information about the status of a cluster restore action. Only applies
-- to a cluster that was created by restoring a snapshot.
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
-- 'vpcSecurityGroups', 'awsRedshiftClusterDetails_vpcSecurityGroups' - The list of VPC security groups that the cluster belongs to, if the
-- cluster is in a VPC.
newAwsRedshiftClusterDetails ::
  AwsRedshiftClusterDetails
newAwsRedshiftClusterDetails =
  AwsRedshiftClusterDetails'
    { clusterIdentifier =
        Prelude.Nothing,
      clusterPublicKey = Prelude.Nothing,
      manualSnapshotRetentionPeriod = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      clusterParameterGroups = Prelude.Nothing,
      allowVersionUpgrade = Prelude.Nothing,
      expectedNextSnapshotScheduleTimeStatus =
        Prelude.Nothing,
      clusterSubnetGroupName = Prelude.Nothing,
      snapshotScheduleIdentifier = Prelude.Nothing,
      nextMaintenanceWindowStartTime = Prelude.Nothing,
      elasticIpStatus = Prelude.Nothing,
      clusterNodes = Prelude.Nothing,
      clusterVersion = Prelude.Nothing,
      loggingStatus = Prelude.Nothing,
      pendingActions = Prelude.Nothing,
      clusterRevisionNumber = Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing,
      iamRoles = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      clusterSnapshotCopyStatus = Prelude.Nothing,
      snapshotScheduleState = Prelude.Nothing,
      hsmStatus = Prelude.Nothing,
      clusterStatus = Prelude.Nothing,
      resizeInfo = Prelude.Nothing,
      deferredMaintenanceWindows = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      expectedNextSnapshotScheduleTime =
        Prelude.Nothing,
      elasticResizeNumberOfNodeOptions =
        Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      enhancedVpcRouting = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      clusterSecurityGroups = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      automatedSnapshotRetentionPeriod =
        Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      dbName = Prelude.Nothing,
      restoreStatus = Prelude.Nothing,
      clusterAvailabilityStatus = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing
    }

-- | The unique identifier of the cluster.
awsRedshiftClusterDetails_clusterIdentifier :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterIdentifier = Lens.lens (\AwsRedshiftClusterDetails' {clusterIdentifier} -> clusterIdentifier) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterIdentifier = a} :: AwsRedshiftClusterDetails)

-- | The public key for the cluster.
awsRedshiftClusterDetails_clusterPublicKey :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterPublicKey = Lens.lens (\AwsRedshiftClusterDetails' {clusterPublicKey} -> clusterPublicKey) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterPublicKey = a} :: AwsRedshiftClusterDetails)

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

-- | The list of cluster parameter groups that are associated with this
-- cluster.
awsRedshiftClusterDetails_clusterParameterGroups :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterClusterParameterGroup])
awsRedshiftClusterDetails_clusterParameterGroups = Lens.lens (\AwsRedshiftClusterDetails' {clusterParameterGroups} -> clusterParameterGroups) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterParameterGroups = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether major version upgrades are applied automatically to
-- the cluster during the maintenance window.
awsRedshiftClusterDetails_allowVersionUpgrade :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_allowVersionUpgrade = Lens.lens (\AwsRedshiftClusterDetails' {allowVersionUpgrade} -> allowVersionUpgrade) (\s@AwsRedshiftClusterDetails' {} a -> s {allowVersionUpgrade = a} :: AwsRedshiftClusterDetails)

-- | The status of the next expected snapshot.
--
-- Valid values: @OnTrack@ | @Pending@
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus = Lens.lens (\AwsRedshiftClusterDetails' {expectedNextSnapshotScheduleTimeStatus} -> expectedNextSnapshotScheduleTimeStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {expectedNextSnapshotScheduleTimeStatus = a} :: AwsRedshiftClusterDetails)

-- | The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
awsRedshiftClusterDetails_clusterSubnetGroupName :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterSubnetGroupName = Lens.lens (\AwsRedshiftClusterDetails' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterSubnetGroupName = a} :: AwsRedshiftClusterDetails)

-- | A unique identifier for the cluster snapshot schedule.
awsRedshiftClusterDetails_snapshotScheduleIdentifier :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_snapshotScheduleIdentifier = Lens.lens (\AwsRedshiftClusterDetails' {snapshotScheduleIdentifier} -> snapshotScheduleIdentifier) (\s@AwsRedshiftClusterDetails' {} a -> s {snapshotScheduleIdentifier = a} :: AwsRedshiftClusterDetails)

-- | Indicates the start of the next maintenance window.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDetails_nextMaintenanceWindowStartTime :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_nextMaintenanceWindowStartTime = Lens.lens (\AwsRedshiftClusterDetails' {nextMaintenanceWindowStartTime} -> nextMaintenanceWindowStartTime) (\s@AwsRedshiftClusterDetails' {} a -> s {nextMaintenanceWindowStartTime = a} :: AwsRedshiftClusterDetails)

-- | Information about the status of the Elastic IP (EIP) address.
awsRedshiftClusterDetails_elasticIpStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterElasticIpStatus)
awsRedshiftClusterDetails_elasticIpStatus = Lens.lens (\AwsRedshiftClusterDetails' {elasticIpStatus} -> elasticIpStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {elasticIpStatus = a} :: AwsRedshiftClusterDetails)

-- | The nodes in the cluster.
awsRedshiftClusterDetails_clusterNodes :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterClusterNode])
awsRedshiftClusterDetails_clusterNodes = Lens.lens (\AwsRedshiftClusterDetails' {clusterNodes} -> clusterNodes) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterNodes = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The version ID of the Amazon Redshift engine that runs on the cluster.
awsRedshiftClusterDetails_clusterVersion :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterVersion = Lens.lens (\AwsRedshiftClusterDetails' {clusterVersion} -> clusterVersion) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterVersion = a} :: AwsRedshiftClusterDetails)

-- | Information about the logging status of the cluster.
awsRedshiftClusterDetails_loggingStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterLoggingStatus)
awsRedshiftClusterDetails_loggingStatus = Lens.lens (\AwsRedshiftClusterDetails' {loggingStatus} -> loggingStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {loggingStatus = a} :: AwsRedshiftClusterDetails)

-- | A list of cluster operations that are waiting to start.
awsRedshiftClusterDetails_pendingActions :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [Prelude.Text])
awsRedshiftClusterDetails_pendingActions = Lens.lens (\AwsRedshiftClusterDetails' {pendingActions} -> pendingActions) (\s@AwsRedshiftClusterDetails' {} a -> s {pendingActions = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The specific revision number of the database in the cluster.
awsRedshiftClusterDetails_clusterRevisionNumber :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterRevisionNumber = Lens.lens (\AwsRedshiftClusterDetails' {clusterRevisionNumber} -> clusterRevisionNumber) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterRevisionNumber = a} :: AwsRedshiftClusterDetails)

-- | The name of the maintenance track for the cluster.
awsRedshiftClusterDetails_maintenanceTrackName :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_maintenanceTrackName = Lens.lens (\AwsRedshiftClusterDetails' {maintenanceTrackName} -> maintenanceTrackName) (\s@AwsRedshiftClusterDetails' {} a -> s {maintenanceTrackName = a} :: AwsRedshiftClusterDetails)

-- | A list of IAM roles that the cluster can use to access other Amazon Web
-- Services services.
awsRedshiftClusterDetails_iamRoles :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterIamRole])
awsRedshiftClusterDetails_iamRoles = Lens.lens (\AwsRedshiftClusterDetails' {iamRoles} -> iamRoles) (\s@AwsRedshiftClusterDetails' {} a -> s {iamRoles = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Availability Zone in which the cluster is located.
awsRedshiftClusterDetails_availabilityZone :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_availabilityZone = Lens.lens (\AwsRedshiftClusterDetails' {availabilityZone} -> availabilityZone) (\s@AwsRedshiftClusterDetails' {} a -> s {availabilityZone = a} :: AwsRedshiftClusterDetails)

-- | The node type for the nodes in the cluster.
awsRedshiftClusterDetails_nodeType :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_nodeType = Lens.lens (\AwsRedshiftClusterDetails' {nodeType} -> nodeType) (\s@AwsRedshiftClusterDetails' {} a -> s {nodeType = a} :: AwsRedshiftClusterDetails)

-- | Whether the cluster can be accessed from a public network.
awsRedshiftClusterDetails_publiclyAccessible :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_publiclyAccessible = Lens.lens (\AwsRedshiftClusterDetails' {publiclyAccessible} -> publiclyAccessible) (\s@AwsRedshiftClusterDetails' {} a -> s {publiclyAccessible = a} :: AwsRedshiftClusterDetails)

-- | Information about the destination Region and retention period for the
-- cross-Region snapshot copy.
awsRedshiftClusterDetails_clusterSnapshotCopyStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterClusterSnapshotCopyStatus)
awsRedshiftClusterDetails_clusterSnapshotCopyStatus = Lens.lens (\AwsRedshiftClusterDetails' {clusterSnapshotCopyStatus} -> clusterSnapshotCopyStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterSnapshotCopyStatus = a} :: AwsRedshiftClusterDetails)

-- | The current state of the cluster snapshot schedule.
--
-- Valid values: @MODIFYING@ | @ACTIVE@ | @FAILED@
awsRedshiftClusterDetails_snapshotScheduleState :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_snapshotScheduleState = Lens.lens (\AwsRedshiftClusterDetails' {snapshotScheduleState} -> snapshotScheduleState) (\s@AwsRedshiftClusterDetails' {} a -> s {snapshotScheduleState = a} :: AwsRedshiftClusterDetails)

-- | Information about whether the Amazon Redshift cluster finished applying
-- any changes to hardware security module (HSM) settings that were
-- specified in a modify cluster command.
awsRedshiftClusterDetails_hsmStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterHsmStatus)
awsRedshiftClusterDetails_hsmStatus = Lens.lens (\AwsRedshiftClusterDetails' {hsmStatus} -> hsmStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {hsmStatus = a} :: AwsRedshiftClusterDetails)

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

-- | Information about the resize operation for the cluster.
awsRedshiftClusterDetails_resizeInfo :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterResizeInfo)
awsRedshiftClusterDetails_resizeInfo = Lens.lens (\AwsRedshiftClusterDetails' {resizeInfo} -> resizeInfo) (\s@AwsRedshiftClusterDetails' {} a -> s {resizeInfo = a} :: AwsRedshiftClusterDetails)

-- | List of time windows during which maintenance was deferred.
awsRedshiftClusterDetails_deferredMaintenanceWindows :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterDeferredMaintenanceWindow])
awsRedshiftClusterDetails_deferredMaintenanceWindows = Lens.lens (\AwsRedshiftClusterDetails' {deferredMaintenanceWindows} -> deferredMaintenanceWindows) (\s@AwsRedshiftClusterDetails' {} a -> s {deferredMaintenanceWindows = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the data in the cluster is encrypted at rest.
awsRedshiftClusterDetails_encrypted :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_encrypted = Lens.lens (\AwsRedshiftClusterDetails' {encrypted} -> encrypted) (\s@AwsRedshiftClusterDetails' {} a -> s {encrypted = a} :: AwsRedshiftClusterDetails)

-- | The number of compute nodes in the cluster.
awsRedshiftClusterDetails_numberOfNodes :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Int)
awsRedshiftClusterDetails_numberOfNodes = Lens.lens (\AwsRedshiftClusterDetails' {numberOfNodes} -> numberOfNodes) (\s@AwsRedshiftClusterDetails' {} a -> s {numberOfNodes = a} :: AwsRedshiftClusterDetails)

-- | The identifier of the KMS encryption key that is used to encrypt data in
-- the cluster.
awsRedshiftClusterDetails_kmsKeyId :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_kmsKeyId = Lens.lens (\AwsRedshiftClusterDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsRedshiftClusterDetails' {} a -> s {kmsKeyId = a} :: AwsRedshiftClusterDetails)

-- | Indicates when the next snapshot is expected to be taken. The cluster
-- must have a valid snapshot schedule and have backups enabled.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime = Lens.lens (\AwsRedshiftClusterDetails' {expectedNextSnapshotScheduleTime} -> expectedNextSnapshotScheduleTime) (\s@AwsRedshiftClusterDetails' {} a -> s {expectedNextSnapshotScheduleTime = a} :: AwsRedshiftClusterDetails)

-- | The number of nodes that you can use the elastic resize method to resize
-- the cluster to.
awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions = Lens.lens (\AwsRedshiftClusterDetails' {elasticResizeNumberOfNodeOptions} -> elasticResizeNumberOfNodeOptions) (\s@AwsRedshiftClusterDetails' {} a -> s {elasticResizeNumberOfNodeOptions = a} :: AwsRedshiftClusterDetails)

-- | A list of changes to the cluster that are currently pending.
awsRedshiftClusterDetails_pendingModifiedValues :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterPendingModifiedValues)
awsRedshiftClusterDetails_pendingModifiedValues = Lens.lens (\AwsRedshiftClusterDetails' {pendingModifiedValues} -> pendingModifiedValues) (\s@AwsRedshiftClusterDetails' {} a -> s {pendingModifiedValues = a} :: AwsRedshiftClusterDetails)

-- | Indicates whether to create the cluster with enhanced VPC routing
-- enabled.
awsRedshiftClusterDetails_enhancedVpcRouting :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_enhancedVpcRouting = Lens.lens (\AwsRedshiftClusterDetails' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@AwsRedshiftClusterDetails' {} a -> s {enhancedVpcRouting = a} :: AwsRedshiftClusterDetails)

-- | The weekly time range, in Universal Coordinated Time (UTC), during which
-- system maintenance can occur.
--
-- Format: @ \<day>:HH:MM-\<day>:HH:MM@
--
-- For the day values, use @mon@ | @tue@ | @wed@ | @thu@ | @fri@ | @sat@ |
-- @sun@
--
-- For example, @sun:09:32-sun:10:02@
awsRedshiftClusterDetails_preferredMaintenanceWindow :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_preferredMaintenanceWindow = Lens.lens (\AwsRedshiftClusterDetails' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@AwsRedshiftClusterDetails' {} a -> s {preferredMaintenanceWindow = a} :: AwsRedshiftClusterDetails)

-- | A list of cluster security groups that are associated with the cluster.
awsRedshiftClusterDetails_clusterSecurityGroups :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterClusterSecurityGroup])
awsRedshiftClusterDetails_clusterSecurityGroups = Lens.lens (\AwsRedshiftClusterDetails' {clusterSecurityGroups} -> clusterSecurityGroups) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterSecurityGroups = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The connection endpoint.
awsRedshiftClusterDetails_endpoint :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterEndpoint)
awsRedshiftClusterDetails_endpoint = Lens.lens (\AwsRedshiftClusterDetails' {endpoint} -> endpoint) (\s@AwsRedshiftClusterDetails' {} a -> s {endpoint = a} :: AwsRedshiftClusterDetails)

-- | The identifier of the VPC that the cluster is in, if the cluster is in a
-- VPC.
awsRedshiftClusterDetails_vpcId :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_vpcId = Lens.lens (\AwsRedshiftClusterDetails' {vpcId} -> vpcId) (\s@AwsRedshiftClusterDetails' {} a -> s {vpcId = a} :: AwsRedshiftClusterDetails)

-- | The number of days that automatic cluster snapshots are retained.
awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Int)
awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod = Lens.lens (\AwsRedshiftClusterDetails' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@AwsRedshiftClusterDetails' {} a -> s {automatedSnapshotRetentionPeriod = a} :: AwsRedshiftClusterDetails)

-- | Indicates when the cluster was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDetails_clusterCreateTime :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterCreateTime = Lens.lens (\AwsRedshiftClusterDetails' {clusterCreateTime} -> clusterCreateTime) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterCreateTime = a} :: AwsRedshiftClusterDetails)

-- | The name of the initial database that was created when the cluster was
-- created.
--
-- The same name is returned for the life of the cluster.
--
-- If an initial database is not specified, a database named @devdev@ is
-- created by default.
awsRedshiftClusterDetails_dbName :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_dbName = Lens.lens (\AwsRedshiftClusterDetails' {dbName} -> dbName) (\s@AwsRedshiftClusterDetails' {} a -> s {dbName = a} :: AwsRedshiftClusterDetails)

-- | Information about the status of a cluster restore action. Only applies
-- to a cluster that was created by restoring a snapshot.
awsRedshiftClusterDetails_restoreStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterRestoreStatus)
awsRedshiftClusterDetails_restoreStatus = Lens.lens (\AwsRedshiftClusterDetails' {restoreStatus} -> restoreStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {restoreStatus = a} :: AwsRedshiftClusterDetails)

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
            Prelude.<$> (x Data..:? "ClusterIdentifier")
            Prelude.<*> (x Data..:? "ClusterPublicKey")
            Prelude.<*> (x Data..:? "ManualSnapshotRetentionPeriod")
            Prelude.<*> (x Data..:? "MasterUsername")
            Prelude.<*> ( x Data..:? "ClusterParameterGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AllowVersionUpgrade")
            Prelude.<*> (x Data..:? "ExpectedNextSnapshotScheduleTimeStatus")
            Prelude.<*> (x Data..:? "ClusterSubnetGroupName")
            Prelude.<*> (x Data..:? "SnapshotScheduleIdentifier")
            Prelude.<*> (x Data..:? "NextMaintenanceWindowStartTime")
            Prelude.<*> (x Data..:? "ElasticIpStatus")
            Prelude.<*> (x Data..:? "ClusterNodes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ClusterVersion")
            Prelude.<*> (x Data..:? "LoggingStatus")
            Prelude.<*> (x Data..:? "PendingActions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ClusterRevisionNumber")
            Prelude.<*> (x Data..:? "MaintenanceTrackName")
            Prelude.<*> (x Data..:? "IamRoles" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "PubliclyAccessible")
            Prelude.<*> (x Data..:? "ClusterSnapshotCopyStatus")
            Prelude.<*> (x Data..:? "SnapshotScheduleState")
            Prelude.<*> (x Data..:? "HsmStatus")
            Prelude.<*> (x Data..:? "ClusterStatus")
            Prelude.<*> (x Data..:? "ResizeInfo")
            Prelude.<*> ( x Data..:? "DeferredMaintenanceWindows"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Encrypted")
            Prelude.<*> (x Data..:? "NumberOfNodes")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "ExpectedNextSnapshotScheduleTime")
            Prelude.<*> (x Data..:? "ElasticResizeNumberOfNodeOptions")
            Prelude.<*> (x Data..:? "PendingModifiedValues")
            Prelude.<*> (x Data..:? "EnhancedVpcRouting")
            Prelude.<*> (x Data..:? "PreferredMaintenanceWindow")
            Prelude.<*> ( x Data..:? "ClusterSecurityGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Endpoint")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..:? "AutomatedSnapshotRetentionPeriod")
            Prelude.<*> (x Data..:? "ClusterCreateTime")
            Prelude.<*> (x Data..:? "DBName")
            Prelude.<*> (x Data..:? "RestoreStatus")
            Prelude.<*> (x Data..:? "ClusterAvailabilityStatus")
            Prelude.<*> ( x Data..:? "VpcSecurityGroups"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsRedshiftClusterDetails where
  hashWithSalt _salt AwsRedshiftClusterDetails' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` clusterPublicKey
      `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` clusterParameterGroups
      `Prelude.hashWithSalt` allowVersionUpgrade
      `Prelude.hashWithSalt` expectedNextSnapshotScheduleTimeStatus
      `Prelude.hashWithSalt` clusterSubnetGroupName
      `Prelude.hashWithSalt` snapshotScheduleIdentifier
      `Prelude.hashWithSalt` nextMaintenanceWindowStartTime
      `Prelude.hashWithSalt` elasticIpStatus
      `Prelude.hashWithSalt` clusterNodes
      `Prelude.hashWithSalt` clusterVersion
      `Prelude.hashWithSalt` loggingStatus
      `Prelude.hashWithSalt` pendingActions
      `Prelude.hashWithSalt` clusterRevisionNumber
      `Prelude.hashWithSalt` maintenanceTrackName
      `Prelude.hashWithSalt` iamRoles
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` clusterSnapshotCopyStatus
      `Prelude.hashWithSalt` snapshotScheduleState
      `Prelude.hashWithSalt` hsmStatus
      `Prelude.hashWithSalt` clusterStatus
      `Prelude.hashWithSalt` resizeInfo
      `Prelude.hashWithSalt` deferredMaintenanceWindows
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` expectedNextSnapshotScheduleTime
      `Prelude.hashWithSalt` elasticResizeNumberOfNodeOptions
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` enhancedVpcRouting
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` clusterSecurityGroups
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` automatedSnapshotRetentionPeriod
      `Prelude.hashWithSalt` clusterCreateTime
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` restoreStatus
      `Prelude.hashWithSalt` clusterAvailabilityStatus
      `Prelude.hashWithSalt` vpcSecurityGroups

instance Prelude.NFData AwsRedshiftClusterDetails where
  rnf AwsRedshiftClusterDetails' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf clusterPublicKey
      `Prelude.seq` Prelude.rnf manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf clusterParameterGroups
      `Prelude.seq` Prelude.rnf allowVersionUpgrade
      `Prelude.seq` Prelude.rnf expectedNextSnapshotScheduleTimeStatus
      `Prelude.seq` Prelude.rnf clusterSubnetGroupName
      `Prelude.seq` Prelude.rnf snapshotScheduleIdentifier
      `Prelude.seq` Prelude.rnf nextMaintenanceWindowStartTime
      `Prelude.seq` Prelude.rnf elasticIpStatus
      `Prelude.seq` Prelude.rnf clusterNodes
      `Prelude.seq` Prelude.rnf clusterVersion
      `Prelude.seq` Prelude.rnf loggingStatus
      `Prelude.seq` Prelude.rnf pendingActions
      `Prelude.seq` Prelude.rnf clusterRevisionNumber
      `Prelude.seq` Prelude.rnf maintenanceTrackName
      `Prelude.seq` Prelude.rnf iamRoles
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        clusterSnapshotCopyStatus
      `Prelude.seq` Prelude.rnf
        snapshotScheduleState
      `Prelude.seq` Prelude.rnf hsmStatus
      `Prelude.seq` Prelude.rnf
        clusterStatus
      `Prelude.seq` Prelude.rnf
        resizeInfo
      `Prelude.seq` Prelude.rnf
        deferredMaintenanceWindows
      `Prelude.seq` Prelude.rnf
        encrypted
      `Prelude.seq` Prelude.rnf
        numberOfNodes
      `Prelude.seq` Prelude.rnf
        kmsKeyId
      `Prelude.seq` Prelude.rnf
        expectedNextSnapshotScheduleTime
      `Prelude.seq` Prelude.rnf
        elasticResizeNumberOfNodeOptions
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
      `Prelude.seq` Prelude.rnf
        enhancedVpcRouting
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        clusterSecurityGroups
      `Prelude.seq` Prelude.rnf
        endpoint
      `Prelude.seq` Prelude.rnf
        vpcId
      `Prelude.seq` Prelude.rnf
        automatedSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf
        clusterCreateTime
      `Prelude.seq` Prelude.rnf
        dbName
      `Prelude.seq` Prelude.rnf
        restoreStatus
      `Prelude.seq` Prelude.rnf
        clusterAvailabilityStatus
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroups

instance Data.ToJSON AwsRedshiftClusterDetails where
  toJSON AwsRedshiftClusterDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterIdentifier" Data..=)
              Prelude.<$> clusterIdentifier,
            ("ClusterPublicKey" Data..=)
              Prelude.<$> clusterPublicKey,
            ("ManualSnapshotRetentionPeriod" Data..=)
              Prelude.<$> manualSnapshotRetentionPeriod,
            ("MasterUsername" Data..=)
              Prelude.<$> masterUsername,
            ("ClusterParameterGroups" Data..=)
              Prelude.<$> clusterParameterGroups,
            ("AllowVersionUpgrade" Data..=)
              Prelude.<$> allowVersionUpgrade,
            ("ExpectedNextSnapshotScheduleTimeStatus" Data..=)
              Prelude.<$> expectedNextSnapshotScheduleTimeStatus,
            ("ClusterSubnetGroupName" Data..=)
              Prelude.<$> clusterSubnetGroupName,
            ("SnapshotScheduleIdentifier" Data..=)
              Prelude.<$> snapshotScheduleIdentifier,
            ("NextMaintenanceWindowStartTime" Data..=)
              Prelude.<$> nextMaintenanceWindowStartTime,
            ("ElasticIpStatus" Data..=)
              Prelude.<$> elasticIpStatus,
            ("ClusterNodes" Data..=) Prelude.<$> clusterNodes,
            ("ClusterVersion" Data..=)
              Prelude.<$> clusterVersion,
            ("LoggingStatus" Data..=) Prelude.<$> loggingStatus,
            ("PendingActions" Data..=)
              Prelude.<$> pendingActions,
            ("ClusterRevisionNumber" Data..=)
              Prelude.<$> clusterRevisionNumber,
            ("MaintenanceTrackName" Data..=)
              Prelude.<$> maintenanceTrackName,
            ("IamRoles" Data..=) Prelude.<$> iamRoles,
            ("AvailabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("NodeType" Data..=) Prelude.<$> nodeType,
            ("PubliclyAccessible" Data..=)
              Prelude.<$> publiclyAccessible,
            ("ClusterSnapshotCopyStatus" Data..=)
              Prelude.<$> clusterSnapshotCopyStatus,
            ("SnapshotScheduleState" Data..=)
              Prelude.<$> snapshotScheduleState,
            ("HsmStatus" Data..=) Prelude.<$> hsmStatus,
            ("ClusterStatus" Data..=) Prelude.<$> clusterStatus,
            ("ResizeInfo" Data..=) Prelude.<$> resizeInfo,
            ("DeferredMaintenanceWindows" Data..=)
              Prelude.<$> deferredMaintenanceWindows,
            ("Encrypted" Data..=) Prelude.<$> encrypted,
            ("NumberOfNodes" Data..=) Prelude.<$> numberOfNodes,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("ExpectedNextSnapshotScheduleTime" Data..=)
              Prelude.<$> expectedNextSnapshotScheduleTime,
            ("ElasticResizeNumberOfNodeOptions" Data..=)
              Prelude.<$> elasticResizeNumberOfNodeOptions,
            ("PendingModifiedValues" Data..=)
              Prelude.<$> pendingModifiedValues,
            ("EnhancedVpcRouting" Data..=)
              Prelude.<$> enhancedVpcRouting,
            ("PreferredMaintenanceWindow" Data..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("ClusterSecurityGroups" Data..=)
              Prelude.<$> clusterSecurityGroups,
            ("Endpoint" Data..=) Prelude.<$> endpoint,
            ("VpcId" Data..=) Prelude.<$> vpcId,
            ("AutomatedSnapshotRetentionPeriod" Data..=)
              Prelude.<$> automatedSnapshotRetentionPeriod,
            ("ClusterCreateTime" Data..=)
              Prelude.<$> clusterCreateTime,
            ("DBName" Data..=) Prelude.<$> dbName,
            ("RestoreStatus" Data..=) Prelude.<$> restoreStatus,
            ("ClusterAvailabilityStatus" Data..=)
              Prelude.<$> clusterAvailabilityStatus,
            ("VpcSecurityGroups" Data..=)
              Prelude.<$> vpcSecurityGroups
          ]
      )
