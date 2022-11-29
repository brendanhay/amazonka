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
-- Module      : Amazonka.DocumentDB.Types.DBCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.DBCluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DocumentDB.Types.DBClusterMember
import Amazonka.DocumentDB.Types.DBClusterRole
import Amazonka.DocumentDB.Types.VpcSecurityGroupMembership
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about a cluster.
--
-- /See:/ 'newDBCluster' smart constructor.
data DBCluster = DBCluster'
  { -- | Specifies the port that the database engine is listening on.
    port :: Prelude.Maybe Prelude.Int,
    -- | Identifies the clone group to which the DB cluster is associated.
    cloneGroupId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the cluster.
    dbClusterArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
    -- zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the progress of the operation as a percentage.
    percentProgress :: Prelude.Maybe Prelude.Text,
    -- | Specifies the daily time range during which automated backups are
    -- created if automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of days for which automatic snapshots are retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Contains the master user name for the cluster.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of instances that make up the cluster.
    dbClusterMembers :: Prelude.Maybe [DBClusterMember],
    -- | Specifies the name of the cluster parameter group for the cluster.
    dbClusterParameterGroup :: Prelude.Maybe Prelude.Text,
    -- | Specifies the latest time to which a database can be restored with
    -- point-in-time restore.
    latestRestorableTime :: Prelude.Maybe Core.ISO8601,
    -- | Contains a user-supplied cluster identifier. This identifier is the
    -- unique key that identifies a cluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of Amazon EC2 Availability Zones that instances in the
    -- cluster can be created in.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Specifies information on the subnet group that is associated with the
    -- cluster, including the name, description, and subnets in the subnet
    -- group.
    dbSubnetGroup :: Prelude.Maybe Prelude.Text,
    -- | Specifies the current state of this cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | Contains the identifier of the source cluster if this cluster is a
    -- secondary cluster.
    replicationSourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | If @StorageEncrypted@ is @true@, the KMS key identifier for the
    -- encrypted cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Provides the name of the database engine to be used for this cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The reader endpoint for the cluster. The reader endpoint for a cluster
    -- load balances connections across the Amazon DocumentDB replicas that are
    -- available in a cluster. As clients request new connections to the reader
    -- endpoint, Amazon DocumentDB distributes the connection requests among
    -- the Amazon DocumentDB replicas in the cluster. This functionality can
    -- help balance your read workload across multiple Amazon DocumentDB
    -- replicas in your cluster.
    --
    -- If a failover occurs, and the Amazon DocumentDB replica that you are
    -- connected to is promoted to be the primary instance, your connection is
    -- dropped. To continue sending your read workload to other Amazon
    -- DocumentDB replicas in the cluster, you can then reconnect to the reader
    -- endpoint.
    readerEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The earliest time to which a database can be restored with point-in-time
    -- restore.
    earliestRestorableTime :: Prelude.Maybe Core.ISO8601,
    -- | Specifies whether this cluster can be deleted. If @DeletionProtection@
    -- is enabled, the cluster cannot be deleted unless it is modified and
    -- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
    -- from being accidentally deleted.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the weekly time range during which system maintenance can
    -- occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the connection endpoint for the primary instance of the
    -- cluster.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time when the cluster was created, in Universal
    -- Coordinated Time (UTC).
    clusterCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | Contains one or more identifiers of the secondary clusters that are
    -- associated with this cluster.
    readReplicaIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | A list of log types that this cluster is configured to export to Amazon
    -- CloudWatch Logs.
    enabledCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services Region-unique, immutable identifier for the
    -- cluster. This identifier is found in CloudTrail log entries whenever the
    -- KMS key for the cluster is accessed.
    dbClusterResourceId :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of the Identity and Access Management (IAM) roles that
    -- are associated with the cluster. (IAM) roles that are associated with a
    -- cluster grant permission for the cluster to access other Amazon Web
    -- Services services on your behalf.
    associatedRoles :: Prelude.Maybe [DBClusterRole],
    -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the cluster has instances in multiple Availability
    -- Zones.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | Provides a list of virtual private cloud (VPC) security groups that the
    -- cluster belongs to.
    vpcSecurityGroups :: Prelude.Maybe [VpcSecurityGroupMembership]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'dbCluster_port' - Specifies the port that the database engine is listening on.
--
-- 'cloneGroupId', 'dbCluster_cloneGroupId' - Identifies the clone group to which the DB cluster is associated.
--
-- 'dbClusterArn', 'dbCluster_dbClusterArn' - The Amazon Resource Name (ARN) for the cluster.
--
-- 'hostedZoneId', 'dbCluster_hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
--
-- 'percentProgress', 'dbCluster_percentProgress' - Specifies the progress of the operation as a percentage.
--
-- 'preferredBackupWindow', 'dbCluster_preferredBackupWindow' - Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
--
-- 'backupRetentionPeriod', 'dbCluster_backupRetentionPeriod' - Specifies the number of days for which automatic snapshots are retained.
--
-- 'masterUsername', 'dbCluster_masterUsername' - Contains the master user name for the cluster.
--
-- 'dbClusterMembers', 'dbCluster_dbClusterMembers' - Provides the list of instances that make up the cluster.
--
-- 'dbClusterParameterGroup', 'dbCluster_dbClusterParameterGroup' - Specifies the name of the cluster parameter group for the cluster.
--
-- 'latestRestorableTime', 'dbCluster_latestRestorableTime' - Specifies the latest time to which a database can be restored with
-- point-in-time restore.
--
-- 'dbClusterIdentifier', 'dbCluster_dbClusterIdentifier' - Contains a user-supplied cluster identifier. This identifier is the
-- unique key that identifies a cluster.
--
-- 'availabilityZones', 'dbCluster_availabilityZones' - Provides the list of Amazon EC2 Availability Zones that instances in the
-- cluster can be created in.
--
-- 'dbSubnetGroup', 'dbCluster_dbSubnetGroup' - Specifies information on the subnet group that is associated with the
-- cluster, including the name, description, and subnets in the subnet
-- group.
--
-- 'status', 'dbCluster_status' - Specifies the current state of this cluster.
--
-- 'replicationSourceIdentifier', 'dbCluster_replicationSourceIdentifier' - Contains the identifier of the source cluster if this cluster is a
-- secondary cluster.
--
-- 'storageEncrypted', 'dbCluster_storageEncrypted' - Specifies whether the cluster is encrypted.
--
-- 'kmsKeyId', 'dbCluster_kmsKeyId' - If @StorageEncrypted@ is @true@, the KMS key identifier for the
-- encrypted cluster.
--
-- 'engine', 'dbCluster_engine' - Provides the name of the database engine to be used for this cluster.
--
-- 'readerEndpoint', 'dbCluster_readerEndpoint' - The reader endpoint for the cluster. The reader endpoint for a cluster
-- load balances connections across the Amazon DocumentDB replicas that are
-- available in a cluster. As clients request new connections to the reader
-- endpoint, Amazon DocumentDB distributes the connection requests among
-- the Amazon DocumentDB replicas in the cluster. This functionality can
-- help balance your read workload across multiple Amazon DocumentDB
-- replicas in your cluster.
--
-- If a failover occurs, and the Amazon DocumentDB replica that you are
-- connected to is promoted to be the primary instance, your connection is
-- dropped. To continue sending your read workload to other Amazon
-- DocumentDB replicas in the cluster, you can then reconnect to the reader
-- endpoint.
--
-- 'earliestRestorableTime', 'dbCluster_earliestRestorableTime' - The earliest time to which a database can be restored with point-in-time
-- restore.
--
-- 'deletionProtection', 'dbCluster_deletionProtection' - Specifies whether this cluster can be deleted. If @DeletionProtection@
-- is enabled, the cluster cannot be deleted unless it is modified and
-- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
-- from being accidentally deleted.
--
-- 'preferredMaintenanceWindow', 'dbCluster_preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
--
-- 'endpoint', 'dbCluster_endpoint' - Specifies the connection endpoint for the primary instance of the
-- cluster.
--
-- 'clusterCreateTime', 'dbCluster_clusterCreateTime' - Specifies the time when the cluster was created, in Universal
-- Coordinated Time (UTC).
--
-- 'readReplicaIdentifiers', 'dbCluster_readReplicaIdentifiers' - Contains one or more identifiers of the secondary clusters that are
-- associated with this cluster.
--
-- 'enabledCloudwatchLogsExports', 'dbCluster_enabledCloudwatchLogsExports' - A list of log types that this cluster is configured to export to Amazon
-- CloudWatch Logs.
--
-- 'dbClusterResourceId', 'dbCluster_dbClusterResourceId' - The Amazon Web Services Region-unique, immutable identifier for the
-- cluster. This identifier is found in CloudTrail log entries whenever the
-- KMS key for the cluster is accessed.
--
-- 'associatedRoles', 'dbCluster_associatedRoles' - Provides a list of the Identity and Access Management (IAM) roles that
-- are associated with the cluster. (IAM) roles that are associated with a
-- cluster grant permission for the cluster to access other Amazon Web
-- Services services on your behalf.
--
-- 'engineVersion', 'dbCluster_engineVersion' - Indicates the database engine version.
--
-- 'multiAZ', 'dbCluster_multiAZ' - Specifies whether the cluster has instances in multiple Availability
-- Zones.
--
-- 'vpcSecurityGroups', 'dbCluster_vpcSecurityGroups' - Provides a list of virtual private cloud (VPC) security groups that the
-- cluster belongs to.
newDBCluster ::
  DBCluster
newDBCluster =
  DBCluster'
    { port = Prelude.Nothing,
      cloneGroupId = Prelude.Nothing,
      dbClusterArn = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      dbClusterMembers = Prelude.Nothing,
      dbClusterParameterGroup = Prelude.Nothing,
      latestRestorableTime = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      dbSubnetGroup = Prelude.Nothing,
      status = Prelude.Nothing,
      replicationSourceIdentifier = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      engine = Prelude.Nothing,
      readerEndpoint = Prelude.Nothing,
      earliestRestorableTime = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      readReplicaIdentifiers = Prelude.Nothing,
      enabledCloudwatchLogsExports = Prelude.Nothing,
      dbClusterResourceId = Prelude.Nothing,
      associatedRoles = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing
    }

-- | Specifies the port that the database engine is listening on.
dbCluster_port :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_port = Lens.lens (\DBCluster' {port} -> port) (\s@DBCluster' {} a -> s {port = a} :: DBCluster)

-- | Identifies the clone group to which the DB cluster is associated.
dbCluster_cloneGroupId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_cloneGroupId = Lens.lens (\DBCluster' {cloneGroupId} -> cloneGroupId) (\s@DBCluster' {} a -> s {cloneGroupId = a} :: DBCluster)

-- | The Amazon Resource Name (ARN) for the cluster.
dbCluster_dbClusterArn :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterArn = Lens.lens (\DBCluster' {dbClusterArn} -> dbClusterArn) (\s@DBCluster' {} a -> s {dbClusterArn = a} :: DBCluster)

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
dbCluster_hostedZoneId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_hostedZoneId = Lens.lens (\DBCluster' {hostedZoneId} -> hostedZoneId) (\s@DBCluster' {} a -> s {hostedZoneId = a} :: DBCluster)

-- | Specifies the progress of the operation as a percentage.
dbCluster_percentProgress :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_percentProgress = Lens.lens (\DBCluster' {percentProgress} -> percentProgress) (\s@DBCluster' {} a -> s {percentProgress = a} :: DBCluster)

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
dbCluster_preferredBackupWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_preferredBackupWindow = Lens.lens (\DBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@DBCluster' {} a -> s {preferredBackupWindow = a} :: DBCluster)

-- | Specifies the number of days for which automatic snapshots are retained.
dbCluster_backupRetentionPeriod :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_backupRetentionPeriod = Lens.lens (\DBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBCluster' {} a -> s {backupRetentionPeriod = a} :: DBCluster)

-- | Contains the master user name for the cluster.
dbCluster_masterUsername :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_masterUsername = Lens.lens (\DBCluster' {masterUsername} -> masterUsername) (\s@DBCluster' {} a -> s {masterUsername = a} :: DBCluster)

-- | Provides the list of instances that make up the cluster.
dbCluster_dbClusterMembers :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterMember])
dbCluster_dbClusterMembers = Lens.lens (\DBCluster' {dbClusterMembers} -> dbClusterMembers) (\s@DBCluster' {} a -> s {dbClusterMembers = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the cluster parameter group for the cluster.
dbCluster_dbClusterParameterGroup :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterParameterGroup = Lens.lens (\DBCluster' {dbClusterParameterGroup} -> dbClusterParameterGroup) (\s@DBCluster' {} a -> s {dbClusterParameterGroup = a} :: DBCluster)

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbCluster_latestRestorableTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_latestRestorableTime = Lens.lens (\DBCluster' {latestRestorableTime} -> latestRestorableTime) (\s@DBCluster' {} a -> s {latestRestorableTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | Contains a user-supplied cluster identifier. This identifier is the
-- unique key that identifies a cluster.
dbCluster_dbClusterIdentifier :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterIdentifier = Lens.lens (\DBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBCluster' {} a -> s {dbClusterIdentifier = a} :: DBCluster)

-- | Provides the list of Amazon EC2 Availability Zones that instances in the
-- cluster can be created in.
dbCluster_availabilityZones :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_availabilityZones = Lens.lens (\DBCluster' {availabilityZones} -> availabilityZones) (\s@DBCluster' {} a -> s {availabilityZones = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Specifies information on the subnet group that is associated with the
-- cluster, including the name, description, and subnets in the subnet
-- group.
dbCluster_dbSubnetGroup :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbSubnetGroup = Lens.lens (\DBCluster' {dbSubnetGroup} -> dbSubnetGroup) (\s@DBCluster' {} a -> s {dbSubnetGroup = a} :: DBCluster)

-- | Specifies the current state of this cluster.
dbCluster_status :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_status = Lens.lens (\DBCluster' {status} -> status) (\s@DBCluster' {} a -> s {status = a} :: DBCluster)

-- | Contains the identifier of the source cluster if this cluster is a
-- secondary cluster.
dbCluster_replicationSourceIdentifier :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_replicationSourceIdentifier = Lens.lens (\DBCluster' {replicationSourceIdentifier} -> replicationSourceIdentifier) (\s@DBCluster' {} a -> s {replicationSourceIdentifier = a} :: DBCluster)

-- | Specifies whether the cluster is encrypted.
dbCluster_storageEncrypted :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_storageEncrypted = Lens.lens (\DBCluster' {storageEncrypted} -> storageEncrypted) (\s@DBCluster' {} a -> s {storageEncrypted = a} :: DBCluster)

-- | If @StorageEncrypted@ is @true@, the KMS key identifier for the
-- encrypted cluster.
dbCluster_kmsKeyId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_kmsKeyId = Lens.lens (\DBCluster' {kmsKeyId} -> kmsKeyId) (\s@DBCluster' {} a -> s {kmsKeyId = a} :: DBCluster)

-- | Provides the name of the database engine to be used for this cluster.
dbCluster_engine :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engine = Lens.lens (\DBCluster' {engine} -> engine) (\s@DBCluster' {} a -> s {engine = a} :: DBCluster)

-- | The reader endpoint for the cluster. The reader endpoint for a cluster
-- load balances connections across the Amazon DocumentDB replicas that are
-- available in a cluster. As clients request new connections to the reader
-- endpoint, Amazon DocumentDB distributes the connection requests among
-- the Amazon DocumentDB replicas in the cluster. This functionality can
-- help balance your read workload across multiple Amazon DocumentDB
-- replicas in your cluster.
--
-- If a failover occurs, and the Amazon DocumentDB replica that you are
-- connected to is promoted to be the primary instance, your connection is
-- dropped. To continue sending your read workload to other Amazon
-- DocumentDB replicas in the cluster, you can then reconnect to the reader
-- endpoint.
dbCluster_readerEndpoint :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_readerEndpoint = Lens.lens (\DBCluster' {readerEndpoint} -> readerEndpoint) (\s@DBCluster' {} a -> s {readerEndpoint = a} :: DBCluster)

-- | The earliest time to which a database can be restored with point-in-time
-- restore.
dbCluster_earliestRestorableTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_earliestRestorableTime = Lens.lens (\DBCluster' {earliestRestorableTime} -> earliestRestorableTime) (\s@DBCluster' {} a -> s {earliestRestorableTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | Specifies whether this cluster can be deleted. If @DeletionProtection@
-- is enabled, the cluster cannot be deleted unless it is modified and
-- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
-- from being accidentally deleted.
dbCluster_deletionProtection :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_deletionProtection = Lens.lens (\DBCluster' {deletionProtection} -> deletionProtection) (\s@DBCluster' {} a -> s {deletionProtection = a} :: DBCluster)

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
dbCluster_preferredMaintenanceWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_preferredMaintenanceWindow = Lens.lens (\DBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@DBCluster' {} a -> s {preferredMaintenanceWindow = a} :: DBCluster)

-- | Specifies the connection endpoint for the primary instance of the
-- cluster.
dbCluster_endpoint :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_endpoint = Lens.lens (\DBCluster' {endpoint} -> endpoint) (\s@DBCluster' {} a -> s {endpoint = a} :: DBCluster)

-- | Specifies the time when the cluster was created, in Universal
-- Coordinated Time (UTC).
dbCluster_clusterCreateTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_clusterCreateTime = Lens.lens (\DBCluster' {clusterCreateTime} -> clusterCreateTime) (\s@DBCluster' {} a -> s {clusterCreateTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | Contains one or more identifiers of the secondary clusters that are
-- associated with this cluster.
dbCluster_readReplicaIdentifiers :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_readReplicaIdentifiers = Lens.lens (\DBCluster' {readReplicaIdentifiers} -> readReplicaIdentifiers) (\s@DBCluster' {} a -> s {readReplicaIdentifiers = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | A list of log types that this cluster is configured to export to Amazon
-- CloudWatch Logs.
dbCluster_enabledCloudwatchLogsExports :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_enabledCloudwatchLogsExports = Lens.lens (\DBCluster' {enabledCloudwatchLogsExports} -> enabledCloudwatchLogsExports) (\s@DBCluster' {} a -> s {enabledCloudwatchLogsExports = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services Region-unique, immutable identifier for the
-- cluster. This identifier is found in CloudTrail log entries whenever the
-- KMS key for the cluster is accessed.
dbCluster_dbClusterResourceId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterResourceId = Lens.lens (\DBCluster' {dbClusterResourceId} -> dbClusterResourceId) (\s@DBCluster' {} a -> s {dbClusterResourceId = a} :: DBCluster)

-- | Provides a list of the Identity and Access Management (IAM) roles that
-- are associated with the cluster. (IAM) roles that are associated with a
-- cluster grant permission for the cluster to access other Amazon Web
-- Services services on your behalf.
dbCluster_associatedRoles :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterRole])
dbCluster_associatedRoles = Lens.lens (\DBCluster' {associatedRoles} -> associatedRoles) (\s@DBCluster' {} a -> s {associatedRoles = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the database engine version.
dbCluster_engineVersion :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engineVersion = Lens.lens (\DBCluster' {engineVersion} -> engineVersion) (\s@DBCluster' {} a -> s {engineVersion = a} :: DBCluster)

-- | Specifies whether the cluster has instances in multiple Availability
-- Zones.
dbCluster_multiAZ :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_multiAZ = Lens.lens (\DBCluster' {multiAZ} -> multiAZ) (\s@DBCluster' {} a -> s {multiAZ = a} :: DBCluster)

-- | Provides a list of virtual private cloud (VPC) security groups that the
-- cluster belongs to.
dbCluster_vpcSecurityGroups :: Lens.Lens' DBCluster (Prelude.Maybe [VpcSecurityGroupMembership])
dbCluster_vpcSecurityGroups = Lens.lens (\DBCluster' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@DBCluster' {} a -> s {vpcSecurityGroups = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML DBCluster where
  parseXML x =
    DBCluster'
      Prelude.<$> (x Core..@? "Port")
      Prelude.<*> (x Core..@? "CloneGroupId")
      Prelude.<*> (x Core..@? "DBClusterArn")
      Prelude.<*> (x Core..@? "HostedZoneId")
      Prelude.<*> (x Core..@? "PercentProgress")
      Prelude.<*> (x Core..@? "PreferredBackupWindow")
      Prelude.<*> (x Core..@? "BackupRetentionPeriod")
      Prelude.<*> (x Core..@? "MasterUsername")
      Prelude.<*> ( x Core..@? "DBClusterMembers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBClusterMember")
                  )
      Prelude.<*> (x Core..@? "DBClusterParameterGroup")
      Prelude.<*> (x Core..@? "LatestRestorableTime")
      Prelude.<*> (x Core..@? "DBClusterIdentifier")
      Prelude.<*> ( x Core..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Core..@? "DBSubnetGroup")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "ReplicationSourceIdentifier")
      Prelude.<*> (x Core..@? "StorageEncrypted")
      Prelude.<*> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "ReaderEndpoint")
      Prelude.<*> (x Core..@? "EarliestRestorableTime")
      Prelude.<*> (x Core..@? "DeletionProtection")
      Prelude.<*> (x Core..@? "PreferredMaintenanceWindow")
      Prelude.<*> (x Core..@? "Endpoint")
      Prelude.<*> (x Core..@? "ClusterCreateTime")
      Prelude.<*> ( x Core..@? "ReadReplicaIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ReadReplicaIdentifier")
                  )
      Prelude.<*> ( x Core..@? "EnabledCloudwatchLogsExports"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "DbClusterResourceId")
      Prelude.<*> ( x Core..@? "AssociatedRoles" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBClusterRole")
                  )
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "MultiAZ")
      Prelude.<*> ( x Core..@? "VpcSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "VpcSecurityGroupMembership")
                  )

instance Prelude.Hashable DBCluster where
  hashWithSalt _salt DBCluster' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` cloneGroupId
      `Prelude.hashWithSalt` dbClusterArn
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` percentProgress
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` dbClusterMembers
      `Prelude.hashWithSalt` dbClusterParameterGroup
      `Prelude.hashWithSalt` latestRestorableTime
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` dbSubnetGroup
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` replicationSourceIdentifier
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` readerEndpoint
      `Prelude.hashWithSalt` earliestRestorableTime
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` clusterCreateTime
      `Prelude.hashWithSalt` readReplicaIdentifiers
      `Prelude.hashWithSalt` enabledCloudwatchLogsExports
      `Prelude.hashWithSalt` dbClusterResourceId
      `Prelude.hashWithSalt` associatedRoles
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` vpcSecurityGroups

instance Prelude.NFData DBCluster where
  rnf DBCluster' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf cloneGroupId
      `Prelude.seq` Prelude.rnf dbClusterArn
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf percentProgress
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf dbClusterMembers
      `Prelude.seq` Prelude.rnf dbClusterParameterGroup
      `Prelude.seq` Prelude.rnf latestRestorableTime
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf dbSubnetGroup
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf
        replicationSourceIdentifier
      `Prelude.seq` Prelude.rnf storageEncrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf readerEndpoint
      `Prelude.seq` Prelude.rnf
        earliestRestorableTime
      `Prelude.seq` Prelude.rnf
        deletionProtection
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf
        clusterCreateTime
      `Prelude.seq` Prelude.rnf
        readReplicaIdentifiers
      `Prelude.seq` Prelude.rnf
        enabledCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf
        dbClusterResourceId
      `Prelude.seq` Prelude.rnf
        associatedRoles
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        multiAZ
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroups
