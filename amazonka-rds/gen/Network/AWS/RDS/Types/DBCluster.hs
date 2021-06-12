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
-- Module      : Network.AWS.RDS.Types.DBCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBCluster where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.ActivityStreamMode
import Network.AWS.RDS.Types.ActivityStreamStatus
import Network.AWS.RDS.Types.ClusterPendingModifiedValues
import Network.AWS.RDS.Types.DBClusterMember
import Network.AWS.RDS.Types.DBClusterOptionGroupStatus
import Network.AWS.RDS.Types.DBClusterRole
import Network.AWS.RDS.Types.DomainMembership
import Network.AWS.RDS.Types.ScalingConfigurationInfo
import Network.AWS.RDS.Types.Tag
import Network.AWS.RDS.Types.VpcSecurityGroupMembership
import Network.AWS.RDS.Types.WriteForwardingStatus

-- | Contains the details of an Amazon Aurora DB cluster.
--
-- This data type is used as a response element in the
-- @DescribeDBClusters@, @StopDBCluster@, and @StartDBCluster@ actions.
--
-- /See:/ 'newDBCluster' smart constructor.
data DBCluster = DBCluster'
  { -- | Specifies the number of days for which automatic DB snapshots are
    -- retained.
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | Indicates if the DB cluster has deletion protection enabled. The
    -- database can\'t be deleted when deletion protection is enabled.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | Specifies whether the DB cluster is encrypted.
    storageEncrypted :: Core.Maybe Core.Bool,
    -- | Provides a list of the AWS Identity and Access Management (IAM) roles
    -- that are associated with the DB cluster. IAM roles that are associated
    -- with a DB cluster grant permission for the DB cluster to access other
    -- AWS services on your behalf.
    associatedRoles :: Core.Maybe [DBClusterRole],
    -- | Provides a list of VPC security groups that the DB cluster belongs to.
    vpcSecurityGroups :: Core.Maybe [VpcSecurityGroupMembership],
    -- | Specifies the daily time range during which automated backups are
    -- created if automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@.
    preferredBackupWindow :: Core.Maybe Core.Text,
    -- | Provides the list of instances that make up the DB cluster.
    dbClusterMembers :: Core.Maybe [DBClusterMember],
    -- | Specifies the current state of this DB cluster.
    status :: Core.Maybe Core.Text,
    -- | The number of change records stored for Backtrack.
    backtrackConsumedChangeRecords :: Core.Maybe Core.Integer,
    -- | Specifies whether the DB cluster is a clone of a DB cluster owned by a
    -- different AWS account.
    crossAccountClone :: Core.Maybe Core.Bool,
    -- | Provides the list of Availability Zones (AZs) where instances in the DB
    -- cluster can be created.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | Specifies whether a secondary cluster in an Aurora global database has
    -- write forwarding enabled, not enabled, or is in the process of enabling
    -- it.
    globalWriteForwardingStatus :: Core.Maybe WriteForwardingStatus,
    -- | Provides the list of option group memberships for this DB cluster.
    dbClusterOptionGroupMemberships :: Core.Maybe [DBClusterOptionGroupStatus],
    -- | Identifies the clone group to which the DB cluster is associated.
    cloneGroupId :: Core.Maybe Core.Text,
    -- | Specifies the latest time to which a database can be restored with
    -- point-in-time restore.
    latestRestorableTime :: Core.Maybe Core.ISO8601,
    -- | The Amazon Resource Name (ARN) for the DB cluster.
    dbClusterArn :: Core.Maybe Core.Text,
    -- | The Active Directory Domain membership records associated with the DB
    -- cluster.
    domainMemberships :: Core.Maybe [DomainMembership],
    -- | The AWS KMS key identifier used for encrypting messages in the database
    -- activity stream.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    activityStreamKmsKeyId :: Core.Maybe Core.Text,
    -- | The reader endpoint for the DB cluster. The reader endpoint for a DB
    -- cluster load-balances connections across the Aurora Replicas that are
    -- available in a DB cluster. As clients request new connections to the
    -- reader endpoint, Aurora distributes the connection requests among the
    -- Aurora Replicas in the DB cluster. This functionality can help balance
    -- your read workload across multiple Aurora Replicas in your DB cluster.
    --
    -- If a failover occurs, and the Aurora Replica that you are connected to
    -- is promoted to be the primary instance, your connection is dropped. To
    -- continue sending your read workload to other Aurora Replicas in the
    -- cluster, you can then reconnect to the reader endpoint.
    readerEndpoint :: Core.Maybe Core.Text,
    -- | A value that indicates whether the HTTP endpoint for an Aurora
    -- Serverless DB cluster is enabled.
    --
    -- When enabled, the HTTP endpoint provides a connectionless web service
    -- API for running SQL queries on the Aurora Serverless DB cluster. You can
    -- also query your database from inside the RDS console with the query
    -- editor.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless>
    -- in the /Amazon Aurora User Guide/.
    httpEndpointEnabled :: Core.Maybe Core.Bool,
    -- | Specifies the time when the DB cluster was created, in Universal
    -- Coordinated Time (UTC).
    clusterCreateTime :: Core.Maybe Core.ISO8601,
    -- | The earliest time to which a database can be restored with point-in-time
    -- restore.
    earliestRestorableTime :: Core.Maybe Core.ISO8601,
    -- | The DB engine mode of the DB cluster, either @provisioned@,
    -- @serverless@, @parallelquery@, @global@, or @multimaster@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
    engineMode :: Core.Maybe Core.Text,
    -- | The status of the database activity stream.
    activityStreamStatus :: Core.Maybe ActivityStreamStatus,
    -- | A list of log types that this DB cluster is configured to export to
    -- CloudWatch Logs.
    --
    -- Log types vary by DB engine. For information about the log types for
    -- each DB engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
    -- in the /Amazon Aurora User Guide./
    enabledCloudwatchLogsExports :: Core.Maybe [Core.Text],
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
    -- zone.
    hostedZoneId :: Core.Maybe Core.Text,
    -- | Specifies information on the subnet group associated with the DB
    -- cluster, including the name, description, and subnets in the subnet
    -- group.
    dbSubnetGroup :: Core.Maybe Core.Text,
    -- | Contains one or more identifiers of the read replicas associated with
    -- this DB cluster.
    readReplicaIdentifiers :: Core.Maybe [Core.Text],
    -- | Contains the master username for the DB cluster.
    masterUsername :: Core.Maybe Core.Text,
    -- | Specifies whether the DB cluster has instances in multiple Availability
    -- Zones.
    multiAZ :: Core.Maybe Core.Bool,
    -- | If @StorageEncrypted@ is enabled, the AWS KMS key identifier for the
    -- encrypted DB cluster.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Contains a user-supplied DB cluster identifier. This identifier is the
    -- unique key that identifies a DB cluster.
    dbClusterIdentifier :: Core.Maybe Core.Text,
    -- | The current capacity of an Aurora Serverless DB cluster. The capacity is
    -- 0 (zero) when the cluster is paused.
    --
    -- For more information about Aurora Serverless, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless>
    -- in the /Amazon Aurora User Guide/.
    capacity :: Core.Maybe Core.Int,
    -- | Indicates the database engine version.
    engineVersion :: Core.Maybe Core.Text,
    -- | Specifies the weekly time range during which system maintenance can
    -- occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | If present, specifies the name of the character set that this cluster is
    -- associated with.
    characterSetName :: Core.Maybe Core.Text,
    -- | Specifies the port that the database engine is listening on.
    port :: Core.Maybe Core.Int,
    -- | Specifies the progress of the operation as a percentage.
    percentProgress :: Core.Maybe Core.Text,
    -- | The name of the database engine to be used for this DB cluster.
    engine :: Core.Maybe Core.Text,
    -- | A value that specifies that changes to the DB cluster are pending. This
    -- element is only included when changes are pending. Specific changes are
    -- identified by subelements.
    pendingModifiedValues :: Core.Maybe ClusterPendingModifiedValues,
    -- | The AWS Region-unique, immutable identifier for the DB cluster. This
    -- identifier is found in AWS CloudTrail log entries whenever the AWS KMS
    -- CMK for the DB cluster is accessed.
    dbClusterResourceId :: Core.Maybe Core.Text,
    -- | Specifies whether tags are copied from the DB cluster to snapshots of
    -- the DB cluster.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
    -- | Identifies all custom endpoints associated with the cluster.
    customEndpoints :: Core.Maybe [Core.Text],
    -- | Specifies the connection endpoint for the primary instance of the DB
    -- cluster.
    endpoint :: Core.Maybe Core.Text,
    scalingConfigurationInfo :: Core.Maybe ScalingConfigurationInfo,
    -- | The earliest time to which a DB cluster can be backtracked.
    earliestBacktrackTime :: Core.Maybe Core.ISO8601,
    tagList :: Core.Maybe [Tag],
    -- | For all database engines except Amazon Aurora, @AllocatedStorage@
    -- specifies the allocated storage size in gibibytes (GiB). For Aurora,
    -- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
    -- size isn\'t fixed, but instead automatically adjusts as needed.
    allocatedStorage :: Core.Maybe Core.Int,
    -- | The target backtrack window, in seconds. If this value is set to 0,
    -- backtracking is disabled for the DB cluster. Otherwise, backtracking is
    -- enabled.
    backtrackWindow :: Core.Maybe Core.Integer,
    -- | A value that indicates whether the mapping of AWS Identity and Access
    -- Management (IAM) accounts to database accounts is enabled.
    iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool,
    -- | Specifies whether you have requested to enable write forwarding for a
    -- secondary cluster in an Aurora global database. Because write forwarding
    -- takes time to enable, check the value of @GlobalWriteForwardingStatus@
    -- to confirm that the request has completed before using the write
    -- forwarding feature for this cluster.
    globalWriteForwardingRequested :: Core.Maybe Core.Bool,
    -- | Specifies the name of the DB cluster parameter group for the DB cluster.
    dbClusterParameterGroup :: Core.Maybe Core.Text,
    -- | Contains the identifier of the source DB cluster if this DB cluster is a
    -- read replica.
    replicationSourceIdentifier :: Core.Maybe Core.Text,
    -- | Contains the name of the initial database of this DB cluster that was
    -- provided at create time, if one was specified when the DB cluster was
    -- created. This same name is returned for the life of the DB cluster.
    databaseName :: Core.Maybe Core.Text,
    -- | The name of the Amazon Kinesis data stream used for the database
    -- activity stream.
    activityStreamKinesisStreamName :: Core.Maybe Core.Text,
    -- | The mode of the database activity stream. Database events such as a
    -- change or access generate an activity stream event. The database session
    -- can handle these events either synchronously or asynchronously.
    activityStreamMode :: Core.Maybe ActivityStreamMode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPeriod', 'dbCluster_backupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are
-- retained.
--
-- 'deletionProtection', 'dbCluster_deletionProtection' - Indicates if the DB cluster has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled.
--
-- 'storageEncrypted', 'dbCluster_storageEncrypted' - Specifies whether the DB cluster is encrypted.
--
-- 'associatedRoles', 'dbCluster_associatedRoles' - Provides a list of the AWS Identity and Access Management (IAM) roles
-- that are associated with the DB cluster. IAM roles that are associated
-- with a DB cluster grant permission for the DB cluster to access other
-- AWS services on your behalf.
--
-- 'vpcSecurityGroups', 'dbCluster_vpcSecurityGroups' - Provides a list of VPC security groups that the DB cluster belongs to.
--
-- 'preferredBackupWindow', 'dbCluster_preferredBackupWindow' - Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
--
-- 'dbClusterMembers', 'dbCluster_dbClusterMembers' - Provides the list of instances that make up the DB cluster.
--
-- 'status', 'dbCluster_status' - Specifies the current state of this DB cluster.
--
-- 'backtrackConsumedChangeRecords', 'dbCluster_backtrackConsumedChangeRecords' - The number of change records stored for Backtrack.
--
-- 'crossAccountClone', 'dbCluster_crossAccountClone' - Specifies whether the DB cluster is a clone of a DB cluster owned by a
-- different AWS account.
--
-- 'availabilityZones', 'dbCluster_availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster can be created.
--
-- 'globalWriteForwardingStatus', 'dbCluster_globalWriteForwardingStatus' - Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
--
-- 'dbClusterOptionGroupMemberships', 'dbCluster_dbClusterOptionGroupMemberships' - Provides the list of option group memberships for this DB cluster.
--
-- 'cloneGroupId', 'dbCluster_cloneGroupId' - Identifies the clone group to which the DB cluster is associated.
--
-- 'latestRestorableTime', 'dbCluster_latestRestorableTime' - Specifies the latest time to which a database can be restored with
-- point-in-time restore.
--
-- 'dbClusterArn', 'dbCluster_dbClusterArn' - The Amazon Resource Name (ARN) for the DB cluster.
--
-- 'domainMemberships', 'dbCluster_domainMemberships' - The Active Directory Domain membership records associated with the DB
-- cluster.
--
-- 'activityStreamKmsKeyId', 'dbCluster_activityStreamKmsKeyId' - The AWS KMS key identifier used for encrypting messages in the database
-- activity stream.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- 'readerEndpoint', 'dbCluster_readerEndpoint' - The reader endpoint for the DB cluster. The reader endpoint for a DB
-- cluster load-balances connections across the Aurora Replicas that are
-- available in a DB cluster. As clients request new connections to the
-- reader endpoint, Aurora distributes the connection requests among the
-- Aurora Replicas in the DB cluster. This functionality can help balance
-- your read workload across multiple Aurora Replicas in your DB cluster.
--
-- If a failover occurs, and the Aurora Replica that you are connected to
-- is promoted to be the primary instance, your connection is dropped. To
-- continue sending your read workload to other Aurora Replicas in the
-- cluster, you can then reconnect to the reader endpoint.
--
-- 'httpEndpointEnabled', 'dbCluster_httpEndpointEnabled' - A value that indicates whether the HTTP endpoint for an Aurora
-- Serverless DB cluster is enabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service
-- API for running SQL queries on the Aurora Serverless DB cluster. You can
-- also query your database from inside the RDS console with the query
-- editor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
--
-- 'clusterCreateTime', 'dbCluster_clusterCreateTime' - Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
--
-- 'earliestRestorableTime', 'dbCluster_earliestRestorableTime' - The earliest time to which a database can be restored with point-in-time
-- restore.
--
-- 'engineMode', 'dbCluster_engineMode' - The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
--
-- 'activityStreamStatus', 'dbCluster_activityStreamStatus' - The status of the database activity stream.
--
-- 'enabledCloudwatchLogsExports', 'dbCluster_enabledCloudwatchLogsExports' - A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon Aurora User Guide./
--
-- 'hostedZoneId', 'dbCluster_hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
--
-- 'dbSubnetGroup', 'dbCluster_dbSubnetGroup' - Specifies information on the subnet group associated with the DB
-- cluster, including the name, description, and subnets in the subnet
-- group.
--
-- 'readReplicaIdentifiers', 'dbCluster_readReplicaIdentifiers' - Contains one or more identifiers of the read replicas associated with
-- this DB cluster.
--
-- 'masterUsername', 'dbCluster_masterUsername' - Contains the master username for the DB cluster.
--
-- 'multiAZ', 'dbCluster_multiAZ' - Specifies whether the DB cluster has instances in multiple Availability
-- Zones.
--
-- 'kmsKeyId', 'dbCluster_kmsKeyId' - If @StorageEncrypted@ is enabled, the AWS KMS key identifier for the
-- encrypted DB cluster.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- 'dbClusterIdentifier', 'dbCluster_dbClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
--
-- 'capacity', 'dbCluster_capacity' - The current capacity of an Aurora Serverless DB cluster. The capacity is
-- 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
--
-- 'engineVersion', 'dbCluster_engineVersion' - Indicates the database engine version.
--
-- 'preferredMaintenanceWindow', 'dbCluster_preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
--
-- 'characterSetName', 'dbCluster_characterSetName' - If present, specifies the name of the character set that this cluster is
-- associated with.
--
-- 'port', 'dbCluster_port' - Specifies the port that the database engine is listening on.
--
-- 'percentProgress', 'dbCluster_percentProgress' - Specifies the progress of the operation as a percentage.
--
-- 'engine', 'dbCluster_engine' - The name of the database engine to be used for this DB cluster.
--
-- 'pendingModifiedValues', 'dbCluster_pendingModifiedValues' - A value that specifies that changes to the DB cluster are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
--
-- 'dbClusterResourceId', 'dbCluster_dbClusterResourceId' - The AWS Region-unique, immutable identifier for the DB cluster. This
-- identifier is found in AWS CloudTrail log entries whenever the AWS KMS
-- CMK for the DB cluster is accessed.
--
-- 'copyTagsToSnapshot', 'dbCluster_copyTagsToSnapshot' - Specifies whether tags are copied from the DB cluster to snapshots of
-- the DB cluster.
--
-- 'customEndpoints', 'dbCluster_customEndpoints' - Identifies all custom endpoints associated with the cluster.
--
-- 'endpoint', 'dbCluster_endpoint' - Specifies the connection endpoint for the primary instance of the DB
-- cluster.
--
-- 'scalingConfigurationInfo', 'dbCluster_scalingConfigurationInfo' - Undocumented member.
--
-- 'earliestBacktrackTime', 'dbCluster_earliestBacktrackTime' - The earliest time to which a DB cluster can be backtracked.
--
-- 'tagList', 'dbCluster_tagList' - Undocumented member.
--
-- 'allocatedStorage', 'dbCluster_allocatedStorage' - For all database engines except Amazon Aurora, @AllocatedStorage@
-- specifies the allocated storage size in gibibytes (GiB). For Aurora,
-- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
-- size isn\'t fixed, but instead automatically adjusts as needed.
--
-- 'backtrackWindow', 'dbCluster_backtrackWindow' - The target backtrack window, in seconds. If this value is set to 0,
-- backtracking is disabled for the DB cluster. Otherwise, backtracking is
-- enabled.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbCluster_iAMDatabaseAuthenticationEnabled' - A value that indicates whether the mapping of AWS Identity and Access
-- Management (IAM) accounts to database accounts is enabled.
--
-- 'globalWriteForwardingRequested', 'dbCluster_globalWriteForwardingRequested' - Specifies whether you have requested to enable write forwarding for a
-- secondary cluster in an Aurora global database. Because write forwarding
-- takes time to enable, check the value of @GlobalWriteForwardingStatus@
-- to confirm that the request has completed before using the write
-- forwarding feature for this cluster.
--
-- 'dbClusterParameterGroup', 'dbCluster_dbClusterParameterGroup' - Specifies the name of the DB cluster parameter group for the DB cluster.
--
-- 'replicationSourceIdentifier', 'dbCluster_replicationSourceIdentifier' - Contains the identifier of the source DB cluster if this DB cluster is a
-- read replica.
--
-- 'databaseName', 'dbCluster_databaseName' - Contains the name of the initial database of this DB cluster that was
-- provided at create time, if one was specified when the DB cluster was
-- created. This same name is returned for the life of the DB cluster.
--
-- 'activityStreamKinesisStreamName', 'dbCluster_activityStreamKinesisStreamName' - The name of the Amazon Kinesis data stream used for the database
-- activity stream.
--
-- 'activityStreamMode', 'dbCluster_activityStreamMode' - The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. The database session
-- can handle these events either synchronously or asynchronously.
newDBCluster ::
  DBCluster
newDBCluster =
  DBCluster'
    { backupRetentionPeriod = Core.Nothing,
      deletionProtection = Core.Nothing,
      storageEncrypted = Core.Nothing,
      associatedRoles = Core.Nothing,
      vpcSecurityGroups = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      dbClusterMembers = Core.Nothing,
      status = Core.Nothing,
      backtrackConsumedChangeRecords = Core.Nothing,
      crossAccountClone = Core.Nothing,
      availabilityZones = Core.Nothing,
      globalWriteForwardingStatus = Core.Nothing,
      dbClusterOptionGroupMemberships = Core.Nothing,
      cloneGroupId = Core.Nothing,
      latestRestorableTime = Core.Nothing,
      dbClusterArn = Core.Nothing,
      domainMemberships = Core.Nothing,
      activityStreamKmsKeyId = Core.Nothing,
      readerEndpoint = Core.Nothing,
      httpEndpointEnabled = Core.Nothing,
      clusterCreateTime = Core.Nothing,
      earliestRestorableTime = Core.Nothing,
      engineMode = Core.Nothing,
      activityStreamStatus = Core.Nothing,
      enabledCloudwatchLogsExports = Core.Nothing,
      hostedZoneId = Core.Nothing,
      dbSubnetGroup = Core.Nothing,
      readReplicaIdentifiers = Core.Nothing,
      masterUsername = Core.Nothing,
      multiAZ = Core.Nothing,
      kmsKeyId = Core.Nothing,
      dbClusterIdentifier = Core.Nothing,
      capacity = Core.Nothing,
      engineVersion = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      characterSetName = Core.Nothing,
      port = Core.Nothing,
      percentProgress = Core.Nothing,
      engine = Core.Nothing,
      pendingModifiedValues = Core.Nothing,
      dbClusterResourceId = Core.Nothing,
      copyTagsToSnapshot = Core.Nothing,
      customEndpoints = Core.Nothing,
      endpoint = Core.Nothing,
      scalingConfigurationInfo = Core.Nothing,
      earliestBacktrackTime = Core.Nothing,
      tagList = Core.Nothing,
      allocatedStorage = Core.Nothing,
      backtrackWindow = Core.Nothing,
      iAMDatabaseAuthenticationEnabled = Core.Nothing,
      globalWriteForwardingRequested = Core.Nothing,
      dbClusterParameterGroup = Core.Nothing,
      replicationSourceIdentifier = Core.Nothing,
      databaseName = Core.Nothing,
      activityStreamKinesisStreamName = Core.Nothing,
      activityStreamMode = Core.Nothing
    }

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
dbCluster_backupRetentionPeriod :: Lens.Lens' DBCluster (Core.Maybe Core.Int)
dbCluster_backupRetentionPeriod = Lens.lens (\DBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBCluster' {} a -> s {backupRetentionPeriod = a} :: DBCluster)

-- | Indicates if the DB cluster has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled.
dbCluster_deletionProtection :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbCluster_deletionProtection = Lens.lens (\DBCluster' {deletionProtection} -> deletionProtection) (\s@DBCluster' {} a -> s {deletionProtection = a} :: DBCluster)

-- | Specifies whether the DB cluster is encrypted.
dbCluster_storageEncrypted :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbCluster_storageEncrypted = Lens.lens (\DBCluster' {storageEncrypted} -> storageEncrypted) (\s@DBCluster' {} a -> s {storageEncrypted = a} :: DBCluster)

-- | Provides a list of the AWS Identity and Access Management (IAM) roles
-- that are associated with the DB cluster. IAM roles that are associated
-- with a DB cluster grant permission for the DB cluster to access other
-- AWS services on your behalf.
dbCluster_associatedRoles :: Lens.Lens' DBCluster (Core.Maybe [DBClusterRole])
dbCluster_associatedRoles = Lens.lens (\DBCluster' {associatedRoles} -> associatedRoles) (\s@DBCluster' {} a -> s {associatedRoles = a} :: DBCluster) Core.. Lens.mapping Lens._Coerce

-- | Provides a list of VPC security groups that the DB cluster belongs to.
dbCluster_vpcSecurityGroups :: Lens.Lens' DBCluster (Core.Maybe [VpcSecurityGroupMembership])
dbCluster_vpcSecurityGroups = Lens.lens (\DBCluster' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@DBCluster' {} a -> s {vpcSecurityGroups = a} :: DBCluster) Core.. Lens.mapping Lens._Coerce

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
dbCluster_preferredBackupWindow :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_preferredBackupWindow = Lens.lens (\DBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@DBCluster' {} a -> s {preferredBackupWindow = a} :: DBCluster)

-- | Provides the list of instances that make up the DB cluster.
dbCluster_dbClusterMembers :: Lens.Lens' DBCluster (Core.Maybe [DBClusterMember])
dbCluster_dbClusterMembers = Lens.lens (\DBCluster' {dbClusterMembers} -> dbClusterMembers) (\s@DBCluster' {} a -> s {dbClusterMembers = a} :: DBCluster) Core.. Lens.mapping Lens._Coerce

-- | Specifies the current state of this DB cluster.
dbCluster_status :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_status = Lens.lens (\DBCluster' {status} -> status) (\s@DBCluster' {} a -> s {status = a} :: DBCluster)

-- | The number of change records stored for Backtrack.
dbCluster_backtrackConsumedChangeRecords :: Lens.Lens' DBCluster (Core.Maybe Core.Integer)
dbCluster_backtrackConsumedChangeRecords = Lens.lens (\DBCluster' {backtrackConsumedChangeRecords} -> backtrackConsumedChangeRecords) (\s@DBCluster' {} a -> s {backtrackConsumedChangeRecords = a} :: DBCluster)

-- | Specifies whether the DB cluster is a clone of a DB cluster owned by a
-- different AWS account.
dbCluster_crossAccountClone :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbCluster_crossAccountClone = Lens.lens (\DBCluster' {crossAccountClone} -> crossAccountClone) (\s@DBCluster' {} a -> s {crossAccountClone = a} :: DBCluster)

-- | Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster can be created.
dbCluster_availabilityZones :: Lens.Lens' DBCluster (Core.Maybe [Core.Text])
dbCluster_availabilityZones = Lens.lens (\DBCluster' {availabilityZones} -> availabilityZones) (\s@DBCluster' {} a -> s {availabilityZones = a} :: DBCluster) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
dbCluster_globalWriteForwardingStatus :: Lens.Lens' DBCluster (Core.Maybe WriteForwardingStatus)
dbCluster_globalWriteForwardingStatus = Lens.lens (\DBCluster' {globalWriteForwardingStatus} -> globalWriteForwardingStatus) (\s@DBCluster' {} a -> s {globalWriteForwardingStatus = a} :: DBCluster)

-- | Provides the list of option group memberships for this DB cluster.
dbCluster_dbClusterOptionGroupMemberships :: Lens.Lens' DBCluster (Core.Maybe [DBClusterOptionGroupStatus])
dbCluster_dbClusterOptionGroupMemberships = Lens.lens (\DBCluster' {dbClusterOptionGroupMemberships} -> dbClusterOptionGroupMemberships) (\s@DBCluster' {} a -> s {dbClusterOptionGroupMemberships = a} :: DBCluster) Core.. Lens.mapping Lens._Coerce

-- | Identifies the clone group to which the DB cluster is associated.
dbCluster_cloneGroupId :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_cloneGroupId = Lens.lens (\DBCluster' {cloneGroupId} -> cloneGroupId) (\s@DBCluster' {} a -> s {cloneGroupId = a} :: DBCluster)

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbCluster_latestRestorableTime :: Lens.Lens' DBCluster (Core.Maybe Core.UTCTime)
dbCluster_latestRestorableTime = Lens.lens (\DBCluster' {latestRestorableTime} -> latestRestorableTime) (\s@DBCluster' {} a -> s {latestRestorableTime = a} :: DBCluster) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) for the DB cluster.
dbCluster_dbClusterArn :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_dbClusterArn = Lens.lens (\DBCluster' {dbClusterArn} -> dbClusterArn) (\s@DBCluster' {} a -> s {dbClusterArn = a} :: DBCluster)

-- | The Active Directory Domain membership records associated with the DB
-- cluster.
dbCluster_domainMemberships :: Lens.Lens' DBCluster (Core.Maybe [DomainMembership])
dbCluster_domainMemberships = Lens.lens (\DBCluster' {domainMemberships} -> domainMemberships) (\s@DBCluster' {} a -> s {domainMemberships = a} :: DBCluster) Core.. Lens.mapping Lens._Coerce

-- | The AWS KMS key identifier used for encrypting messages in the database
-- activity stream.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
dbCluster_activityStreamKmsKeyId :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_activityStreamKmsKeyId = Lens.lens (\DBCluster' {activityStreamKmsKeyId} -> activityStreamKmsKeyId) (\s@DBCluster' {} a -> s {activityStreamKmsKeyId = a} :: DBCluster)

-- | The reader endpoint for the DB cluster. The reader endpoint for a DB
-- cluster load-balances connections across the Aurora Replicas that are
-- available in a DB cluster. As clients request new connections to the
-- reader endpoint, Aurora distributes the connection requests among the
-- Aurora Replicas in the DB cluster. This functionality can help balance
-- your read workload across multiple Aurora Replicas in your DB cluster.
--
-- If a failover occurs, and the Aurora Replica that you are connected to
-- is promoted to be the primary instance, your connection is dropped. To
-- continue sending your read workload to other Aurora Replicas in the
-- cluster, you can then reconnect to the reader endpoint.
dbCluster_readerEndpoint :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_readerEndpoint = Lens.lens (\DBCluster' {readerEndpoint} -> readerEndpoint) (\s@DBCluster' {} a -> s {readerEndpoint = a} :: DBCluster)

-- | A value that indicates whether the HTTP endpoint for an Aurora
-- Serverless DB cluster is enabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service
-- API for running SQL queries on the Aurora Serverless DB cluster. You can
-- also query your database from inside the RDS console with the query
-- editor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
dbCluster_httpEndpointEnabled :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbCluster_httpEndpointEnabled = Lens.lens (\DBCluster' {httpEndpointEnabled} -> httpEndpointEnabled) (\s@DBCluster' {} a -> s {httpEndpointEnabled = a} :: DBCluster)

-- | Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
dbCluster_clusterCreateTime :: Lens.Lens' DBCluster (Core.Maybe Core.UTCTime)
dbCluster_clusterCreateTime = Lens.lens (\DBCluster' {clusterCreateTime} -> clusterCreateTime) (\s@DBCluster' {} a -> s {clusterCreateTime = a} :: DBCluster) Core.. Lens.mapping Core._Time

-- | The earliest time to which a database can be restored with point-in-time
-- restore.
dbCluster_earliestRestorableTime :: Lens.Lens' DBCluster (Core.Maybe Core.UTCTime)
dbCluster_earliestRestorableTime = Lens.lens (\DBCluster' {earliestRestorableTime} -> earliestRestorableTime) (\s@DBCluster' {} a -> s {earliestRestorableTime = a} :: DBCluster) Core.. Lens.mapping Core._Time

-- | The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
dbCluster_engineMode :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_engineMode = Lens.lens (\DBCluster' {engineMode} -> engineMode) (\s@DBCluster' {} a -> s {engineMode = a} :: DBCluster)

-- | The status of the database activity stream.
dbCluster_activityStreamStatus :: Lens.Lens' DBCluster (Core.Maybe ActivityStreamStatus)
dbCluster_activityStreamStatus = Lens.lens (\DBCluster' {activityStreamStatus} -> activityStreamStatus) (\s@DBCluster' {} a -> s {activityStreamStatus = a} :: DBCluster)

-- | A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon Aurora User Guide./
dbCluster_enabledCloudwatchLogsExports :: Lens.Lens' DBCluster (Core.Maybe [Core.Text])
dbCluster_enabledCloudwatchLogsExports = Lens.lens (\DBCluster' {enabledCloudwatchLogsExports} -> enabledCloudwatchLogsExports) (\s@DBCluster' {} a -> s {enabledCloudwatchLogsExports = a} :: DBCluster) Core.. Lens.mapping Lens._Coerce

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
dbCluster_hostedZoneId :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_hostedZoneId = Lens.lens (\DBCluster' {hostedZoneId} -> hostedZoneId) (\s@DBCluster' {} a -> s {hostedZoneId = a} :: DBCluster)

-- | Specifies information on the subnet group associated with the DB
-- cluster, including the name, description, and subnets in the subnet
-- group.
dbCluster_dbSubnetGroup :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_dbSubnetGroup = Lens.lens (\DBCluster' {dbSubnetGroup} -> dbSubnetGroup) (\s@DBCluster' {} a -> s {dbSubnetGroup = a} :: DBCluster)

-- | Contains one or more identifiers of the read replicas associated with
-- this DB cluster.
dbCluster_readReplicaIdentifiers :: Lens.Lens' DBCluster (Core.Maybe [Core.Text])
dbCluster_readReplicaIdentifiers = Lens.lens (\DBCluster' {readReplicaIdentifiers} -> readReplicaIdentifiers) (\s@DBCluster' {} a -> s {readReplicaIdentifiers = a} :: DBCluster) Core.. Lens.mapping Lens._Coerce

-- | Contains the master username for the DB cluster.
dbCluster_masterUsername :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_masterUsername = Lens.lens (\DBCluster' {masterUsername} -> masterUsername) (\s@DBCluster' {} a -> s {masterUsername = a} :: DBCluster)

-- | Specifies whether the DB cluster has instances in multiple Availability
-- Zones.
dbCluster_multiAZ :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbCluster_multiAZ = Lens.lens (\DBCluster' {multiAZ} -> multiAZ) (\s@DBCluster' {} a -> s {multiAZ = a} :: DBCluster)

-- | If @StorageEncrypted@ is enabled, the AWS KMS key identifier for the
-- encrypted DB cluster.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
dbCluster_kmsKeyId :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_kmsKeyId = Lens.lens (\DBCluster' {kmsKeyId} -> kmsKeyId) (\s@DBCluster' {} a -> s {kmsKeyId = a} :: DBCluster)

-- | Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
dbCluster_dbClusterIdentifier :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_dbClusterIdentifier = Lens.lens (\DBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBCluster' {} a -> s {dbClusterIdentifier = a} :: DBCluster)

-- | The current capacity of an Aurora Serverless DB cluster. The capacity is
-- 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
dbCluster_capacity :: Lens.Lens' DBCluster (Core.Maybe Core.Int)
dbCluster_capacity = Lens.lens (\DBCluster' {capacity} -> capacity) (\s@DBCluster' {} a -> s {capacity = a} :: DBCluster)

-- | Indicates the database engine version.
dbCluster_engineVersion :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_engineVersion = Lens.lens (\DBCluster' {engineVersion} -> engineVersion) (\s@DBCluster' {} a -> s {engineVersion = a} :: DBCluster)

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
dbCluster_preferredMaintenanceWindow :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_preferredMaintenanceWindow = Lens.lens (\DBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@DBCluster' {} a -> s {preferredMaintenanceWindow = a} :: DBCluster)

-- | If present, specifies the name of the character set that this cluster is
-- associated with.
dbCluster_characterSetName :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_characterSetName = Lens.lens (\DBCluster' {characterSetName} -> characterSetName) (\s@DBCluster' {} a -> s {characterSetName = a} :: DBCluster)

-- | Specifies the port that the database engine is listening on.
dbCluster_port :: Lens.Lens' DBCluster (Core.Maybe Core.Int)
dbCluster_port = Lens.lens (\DBCluster' {port} -> port) (\s@DBCluster' {} a -> s {port = a} :: DBCluster)

-- | Specifies the progress of the operation as a percentage.
dbCluster_percentProgress :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_percentProgress = Lens.lens (\DBCluster' {percentProgress} -> percentProgress) (\s@DBCluster' {} a -> s {percentProgress = a} :: DBCluster)

-- | The name of the database engine to be used for this DB cluster.
dbCluster_engine :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_engine = Lens.lens (\DBCluster' {engine} -> engine) (\s@DBCluster' {} a -> s {engine = a} :: DBCluster)

-- | A value that specifies that changes to the DB cluster are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
dbCluster_pendingModifiedValues :: Lens.Lens' DBCluster (Core.Maybe ClusterPendingModifiedValues)
dbCluster_pendingModifiedValues = Lens.lens (\DBCluster' {pendingModifiedValues} -> pendingModifiedValues) (\s@DBCluster' {} a -> s {pendingModifiedValues = a} :: DBCluster)

-- | The AWS Region-unique, immutable identifier for the DB cluster. This
-- identifier is found in AWS CloudTrail log entries whenever the AWS KMS
-- CMK for the DB cluster is accessed.
dbCluster_dbClusterResourceId :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_dbClusterResourceId = Lens.lens (\DBCluster' {dbClusterResourceId} -> dbClusterResourceId) (\s@DBCluster' {} a -> s {dbClusterResourceId = a} :: DBCluster)

-- | Specifies whether tags are copied from the DB cluster to snapshots of
-- the DB cluster.
dbCluster_copyTagsToSnapshot :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbCluster_copyTagsToSnapshot = Lens.lens (\DBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@DBCluster' {} a -> s {copyTagsToSnapshot = a} :: DBCluster)

-- | Identifies all custom endpoints associated with the cluster.
dbCluster_customEndpoints :: Lens.Lens' DBCluster (Core.Maybe [Core.Text])
dbCluster_customEndpoints = Lens.lens (\DBCluster' {customEndpoints} -> customEndpoints) (\s@DBCluster' {} a -> s {customEndpoints = a} :: DBCluster) Core.. Lens.mapping Lens._Coerce

-- | Specifies the connection endpoint for the primary instance of the DB
-- cluster.
dbCluster_endpoint :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_endpoint = Lens.lens (\DBCluster' {endpoint} -> endpoint) (\s@DBCluster' {} a -> s {endpoint = a} :: DBCluster)

-- | Undocumented member.
dbCluster_scalingConfigurationInfo :: Lens.Lens' DBCluster (Core.Maybe ScalingConfigurationInfo)
dbCluster_scalingConfigurationInfo = Lens.lens (\DBCluster' {scalingConfigurationInfo} -> scalingConfigurationInfo) (\s@DBCluster' {} a -> s {scalingConfigurationInfo = a} :: DBCluster)

-- | The earliest time to which a DB cluster can be backtracked.
dbCluster_earliestBacktrackTime :: Lens.Lens' DBCluster (Core.Maybe Core.UTCTime)
dbCluster_earliestBacktrackTime = Lens.lens (\DBCluster' {earliestBacktrackTime} -> earliestBacktrackTime) (\s@DBCluster' {} a -> s {earliestBacktrackTime = a} :: DBCluster) Core.. Lens.mapping Core._Time

-- | Undocumented member.
dbCluster_tagList :: Lens.Lens' DBCluster (Core.Maybe [Tag])
dbCluster_tagList = Lens.lens (\DBCluster' {tagList} -> tagList) (\s@DBCluster' {} a -> s {tagList = a} :: DBCluster) Core.. Lens.mapping Lens._Coerce

-- | For all database engines except Amazon Aurora, @AllocatedStorage@
-- specifies the allocated storage size in gibibytes (GiB). For Aurora,
-- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
-- size isn\'t fixed, but instead automatically adjusts as needed.
dbCluster_allocatedStorage :: Lens.Lens' DBCluster (Core.Maybe Core.Int)
dbCluster_allocatedStorage = Lens.lens (\DBCluster' {allocatedStorage} -> allocatedStorage) (\s@DBCluster' {} a -> s {allocatedStorage = a} :: DBCluster)

-- | The target backtrack window, in seconds. If this value is set to 0,
-- backtracking is disabled for the DB cluster. Otherwise, backtracking is
-- enabled.
dbCluster_backtrackWindow :: Lens.Lens' DBCluster (Core.Maybe Core.Integer)
dbCluster_backtrackWindow = Lens.lens (\DBCluster' {backtrackWindow} -> backtrackWindow) (\s@DBCluster' {} a -> s {backtrackWindow = a} :: DBCluster)

-- | A value that indicates whether the mapping of AWS Identity and Access
-- Management (IAM) accounts to database accounts is enabled.
dbCluster_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbCluster_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBCluster' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBCluster' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBCluster)

-- | Specifies whether you have requested to enable write forwarding for a
-- secondary cluster in an Aurora global database. Because write forwarding
-- takes time to enable, check the value of @GlobalWriteForwardingStatus@
-- to confirm that the request has completed before using the write
-- forwarding feature for this cluster.
dbCluster_globalWriteForwardingRequested :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbCluster_globalWriteForwardingRequested = Lens.lens (\DBCluster' {globalWriteForwardingRequested} -> globalWriteForwardingRequested) (\s@DBCluster' {} a -> s {globalWriteForwardingRequested = a} :: DBCluster)

-- | Specifies the name of the DB cluster parameter group for the DB cluster.
dbCluster_dbClusterParameterGroup :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_dbClusterParameterGroup = Lens.lens (\DBCluster' {dbClusterParameterGroup} -> dbClusterParameterGroup) (\s@DBCluster' {} a -> s {dbClusterParameterGroup = a} :: DBCluster)

-- | Contains the identifier of the source DB cluster if this DB cluster is a
-- read replica.
dbCluster_replicationSourceIdentifier :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_replicationSourceIdentifier = Lens.lens (\DBCluster' {replicationSourceIdentifier} -> replicationSourceIdentifier) (\s@DBCluster' {} a -> s {replicationSourceIdentifier = a} :: DBCluster)

-- | Contains the name of the initial database of this DB cluster that was
-- provided at create time, if one was specified when the DB cluster was
-- created. This same name is returned for the life of the DB cluster.
dbCluster_databaseName :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_databaseName = Lens.lens (\DBCluster' {databaseName} -> databaseName) (\s@DBCluster' {} a -> s {databaseName = a} :: DBCluster)

-- | The name of the Amazon Kinesis data stream used for the database
-- activity stream.
dbCluster_activityStreamKinesisStreamName :: Lens.Lens' DBCluster (Core.Maybe Core.Text)
dbCluster_activityStreamKinesisStreamName = Lens.lens (\DBCluster' {activityStreamKinesisStreamName} -> activityStreamKinesisStreamName) (\s@DBCluster' {} a -> s {activityStreamKinesisStreamName = a} :: DBCluster)

-- | The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. The database session
-- can handle these events either synchronously or asynchronously.
dbCluster_activityStreamMode :: Lens.Lens' DBCluster (Core.Maybe ActivityStreamMode)
dbCluster_activityStreamMode = Lens.lens (\DBCluster' {activityStreamMode} -> activityStreamMode) (\s@DBCluster' {} a -> s {activityStreamMode = a} :: DBCluster)

instance Core.FromXML DBCluster where
  parseXML x =
    DBCluster'
      Core.<$> (x Core..@? "BackupRetentionPeriod")
      Core.<*> (x Core..@? "DeletionProtection")
      Core.<*> (x Core..@? "StorageEncrypted")
      Core.<*> ( x Core..@? "AssociatedRoles" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DBClusterRole")
               )
      Core.<*> ( x Core..@? "VpcSecurityGroups" Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "VpcSecurityGroupMembership")
               )
      Core.<*> (x Core..@? "PreferredBackupWindow")
      Core.<*> ( x Core..@? "DBClusterMembers" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DBClusterMember")
               )
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "BacktrackConsumedChangeRecords")
      Core.<*> (x Core..@? "CrossAccountClone")
      Core.<*> ( x Core..@? "AvailabilityZones" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "AvailabilityZone")
               )
      Core.<*> (x Core..@? "GlobalWriteForwardingStatus")
      Core.<*> ( x Core..@? "DBClusterOptionGroupMemberships"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DBClusterOptionGroup")
               )
      Core.<*> (x Core..@? "CloneGroupId")
      Core.<*> (x Core..@? "LatestRestorableTime")
      Core.<*> (x Core..@? "DBClusterArn")
      Core.<*> ( x Core..@? "DomainMemberships" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DomainMembership")
               )
      Core.<*> (x Core..@? "ActivityStreamKmsKeyId")
      Core.<*> (x Core..@? "ReaderEndpoint")
      Core.<*> (x Core..@? "HttpEndpointEnabled")
      Core.<*> (x Core..@? "ClusterCreateTime")
      Core.<*> (x Core..@? "EarliestRestorableTime")
      Core.<*> (x Core..@? "EngineMode")
      Core.<*> (x Core..@? "ActivityStreamStatus")
      Core.<*> ( x Core..@? "EnabledCloudwatchLogsExports"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "HostedZoneId")
      Core.<*> (x Core..@? "DBSubnetGroup")
      Core.<*> ( x Core..@? "ReadReplicaIdentifiers"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "ReadReplicaIdentifier")
               )
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "MultiAZ")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "DBClusterIdentifier")
      Core.<*> (x Core..@? "Capacity")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "PreferredMaintenanceWindow")
      Core.<*> (x Core..@? "CharacterSetName")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "PercentProgress")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "PendingModifiedValues")
      Core.<*> (x Core..@? "DbClusterResourceId")
      Core.<*> (x Core..@? "CopyTagsToSnapshot")
      Core.<*> ( x Core..@? "CustomEndpoints" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Endpoint")
      Core.<*> (x Core..@? "ScalingConfigurationInfo")
      Core.<*> (x Core..@? "EarliestBacktrackTime")
      Core.<*> ( x Core..@? "TagList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> (x Core..@? "AllocatedStorage")
      Core.<*> (x Core..@? "BacktrackWindow")
      Core.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Core.<*> (x Core..@? "GlobalWriteForwardingRequested")
      Core.<*> (x Core..@? "DBClusterParameterGroup")
      Core.<*> (x Core..@? "ReplicationSourceIdentifier")
      Core.<*> (x Core..@? "DatabaseName")
      Core.<*> (x Core..@? "ActivityStreamKinesisStreamName")
      Core.<*> (x Core..@? "ActivityStreamMode")

instance Core.Hashable DBCluster

instance Core.NFData DBCluster
