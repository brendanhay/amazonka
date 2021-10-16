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
import qualified Network.AWS.Prelude as Prelude
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
  { -- | Provides a list of the Amazon Web Services Identity and Access
    -- Management (IAM) roles that are associated with the DB cluster. IAM
    -- roles that are associated with a DB cluster grant permission for the DB
    -- cluster to access other Amazon Web Services on your behalf.
    associatedRoles :: Prelude.Maybe [DBClusterRole],
    -- | Provides a list of VPC security groups that the DB cluster belongs to.
    vpcSecurityGroups :: Prelude.Maybe [VpcSecurityGroupMembership],
    -- | The time when a stopped DB cluster is restarted automatically.
    automaticRestartTime :: Prelude.Maybe Core.ISO8601,
    -- | Indicates if the DB cluster has deletion protection enabled. The
    -- database can\'t be deleted when deletion protection is enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the daily time range during which automated backups are
    -- created if automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of instances that make up the DB cluster.
    dbClusterMembers :: Prelude.Maybe [DBClusterMember],
    -- | Specifies the number of days for which automatic DB snapshots are
    -- retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | Provides the list of Availability Zones (AZs) where instances in the DB
    -- cluster can be created.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the current state of this DB cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | The number of change records stored for Backtrack.
    backtrackConsumedChangeRecords :: Prelude.Maybe Prelude.Integer,
    -- | Specifies whether the DB cluster is a clone of a DB cluster owned by a
    -- different Amazon Web Services account.
    crossAccountClone :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a secondary cluster in an Aurora global database has
    -- write forwarding enabled, not enabled, or is in the process of enabling
    -- it.
    globalWriteForwardingStatus :: Prelude.Maybe WriteForwardingStatus,
    -- | The Active Directory Domain membership records associated with the DB
    -- cluster.
    domainMemberships :: Prelude.Maybe [DomainMembership],
    -- | The Amazon Resource Name (ARN) for the DB cluster.
    dbClusterArn :: Prelude.Maybe Prelude.Text,
    -- | Identifies the clone group to which the DB cluster is associated.
    cloneGroupId :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of option group memberships for this DB cluster.
    dbClusterOptionGroupMemberships :: Prelude.Maybe [DBClusterOptionGroupStatus],
    -- | Specifies the latest time to which a database can be restored with
    -- point-in-time restore.
    latestRestorableTime :: Prelude.Maybe Core.ISO8601,
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
    readerEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier used for encrypting messages
    -- in the database activity stream.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK).
    activityStreamKmsKeyId :: Prelude.Maybe Prelude.Text,
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
    httpEndpointEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the time when the DB cluster was created, in Universal
    -- Coordinated Time (UTC).
    clusterCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | The earliest time to which a database can be restored with point-in-time
    -- restore.
    earliestRestorableTime :: Prelude.Maybe Core.ISO8601,
    -- | A list of log types that this DB cluster is configured to export to
    -- CloudWatch Logs.
    --
    -- Log types vary by DB engine. For information about the log types for
    -- each DB engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
    -- in the /Amazon Aurora User Guide./
    enabledCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The status of the database activity stream.
    activityStreamStatus :: Prelude.Maybe ActivityStreamStatus,
    -- | The DB engine mode of the DB cluster, either @provisioned@,
    -- @serverless@, @parallelquery@, @global@, or @multimaster@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the DB cluster has instances in multiple Availability
    -- Zones.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | Specifies information on the subnet group associated with the DB
    -- cluster, including the name, description, and subnets in the subnet
    -- group.
    dbSubnetGroup :: Prelude.Maybe Prelude.Text,
    -- | Contains one or more identifiers of the read replicas associated with
    -- this DB cluster.
    readReplicaIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Contains the master username for the DB cluster.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
    -- zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | If @StorageEncrypted@ is enabled, the Amazon Web Services KMS key
    -- identifier for the encrypted DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK).
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Contains a user-supplied DB cluster identifier. This identifier is the
    -- unique key that identifies a DB cluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The current capacity of an Aurora Serverless DB cluster. The capacity is
    -- 0 (zero) when the cluster is paused.
    --
    -- For more information about Aurora Serverless, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless>
    -- in the /Amazon Aurora User Guide/.
    capacity :: Prelude.Maybe Prelude.Int,
    -- | If present, specifies the name of the character set that this cluster is
    -- associated with.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which system maintenance can
    -- occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the port that the database engine is listening on.
    port :: Prelude.Maybe Prelude.Int,
    -- | Specifies the progress of the operation as a percentage.
    percentProgress :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine to be used for this DB cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies that changes to the DB cluster are pending. This
    -- element is only included when changes are pending. Specific changes are
    -- identified by subelements.
    pendingModifiedValues :: Prelude.Maybe ClusterPendingModifiedValues,
    -- | Specifies the connection endpoint for the primary instance of the DB
    -- cluster.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether tags are copied from the DB cluster to snapshots of
    -- the DB cluster.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    scalingConfigurationInfo :: Prelude.Maybe ScalingConfigurationInfo,
    -- | The Amazon Web Services Region-unique, immutable identifier for the DB
    -- cluster. This identifier is found in Amazon Web Services CloudTrail log
    -- entries whenever the Amazon Web Services KMS CMK for the DB cluster is
    -- accessed.
    dbClusterResourceId :: Prelude.Maybe Prelude.Text,
    -- | Identifies all custom endpoints associated with the cluster.
    customEndpoints :: Prelude.Maybe [Prelude.Text],
    tagList :: Prelude.Maybe [Tag],
    -- | The earliest time to which a DB cluster can be backtracked.
    earliestBacktrackTime :: Prelude.Maybe Core.ISO8601,
    -- | The target backtrack window, in seconds. If this value is set to 0,
    -- backtracking is disabled for the DB cluster. Otherwise, backtracking is
    -- enabled.
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
    -- | For all database engines except Amazon Aurora, @AllocatedStorage@
    -- specifies the allocated storage size in gibibytes (GiB). For Aurora,
    -- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
    -- size isn\'t fixed, but instead automatically adjusts as needed.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether you have requested to enable write forwarding for a
    -- secondary cluster in an Aurora global database. Because write forwarding
    -- takes time to enable, check the value of @GlobalWriteForwardingStatus@
    -- to confirm that the request has completed before using the write
    -- forwarding feature for this cluster.
    globalWriteForwardingRequested :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts is
    -- enabled.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the DB cluster parameter group for the DB cluster.
    dbClusterParameterGroup :: Prelude.Maybe Prelude.Text,
    -- | Contains the identifier of the source DB cluster if this DB cluster is a
    -- read replica.
    replicationSourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Kinesis data stream used for the database
    -- activity stream.
    activityStreamKinesisStreamName :: Prelude.Maybe Prelude.Text,
    -- | Contains the name of the initial database of this DB cluster that was
    -- provided at create time, if one was specified when the DB cluster was
    -- created. This same name is returned for the life of the DB cluster.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The mode of the database activity stream. Database events such as a
    -- change or access generate an activity stream event. The database session
    -- can handle these events either synchronously or asynchronously.
    activityStreamMode :: Prelude.Maybe ActivityStreamMode
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
-- 'associatedRoles', 'dbCluster_associatedRoles' - Provides a list of the Amazon Web Services Identity and Access
-- Management (IAM) roles that are associated with the DB cluster. IAM
-- roles that are associated with a DB cluster grant permission for the DB
-- cluster to access other Amazon Web Services on your behalf.
--
-- 'vpcSecurityGroups', 'dbCluster_vpcSecurityGroups' - Provides a list of VPC security groups that the DB cluster belongs to.
--
-- 'automaticRestartTime', 'dbCluster_automaticRestartTime' - The time when a stopped DB cluster is restarted automatically.
--
-- 'deletionProtection', 'dbCluster_deletionProtection' - Indicates if the DB cluster has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled.
--
-- 'preferredBackupWindow', 'dbCluster_preferredBackupWindow' - Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
--
-- 'dbClusterMembers', 'dbCluster_dbClusterMembers' - Provides the list of instances that make up the DB cluster.
--
-- 'backupRetentionPeriod', 'dbCluster_backupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are
-- retained.
--
-- 'storageEncrypted', 'dbCluster_storageEncrypted' - Specifies whether the DB cluster is encrypted.
--
-- 'availabilityZones', 'dbCluster_availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster can be created.
--
-- 'status', 'dbCluster_status' - Specifies the current state of this DB cluster.
--
-- 'backtrackConsumedChangeRecords', 'dbCluster_backtrackConsumedChangeRecords' - The number of change records stored for Backtrack.
--
-- 'crossAccountClone', 'dbCluster_crossAccountClone' - Specifies whether the DB cluster is a clone of a DB cluster owned by a
-- different Amazon Web Services account.
--
-- 'globalWriteForwardingStatus', 'dbCluster_globalWriteForwardingStatus' - Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
--
-- 'domainMemberships', 'dbCluster_domainMemberships' - The Active Directory Domain membership records associated with the DB
-- cluster.
--
-- 'dbClusterArn', 'dbCluster_dbClusterArn' - The Amazon Resource Name (ARN) for the DB cluster.
--
-- 'cloneGroupId', 'dbCluster_cloneGroupId' - Identifies the clone group to which the DB cluster is associated.
--
-- 'dbClusterOptionGroupMemberships', 'dbCluster_dbClusterOptionGroupMemberships' - Provides the list of option group memberships for this DB cluster.
--
-- 'latestRestorableTime', 'dbCluster_latestRestorableTime' - Specifies the latest time to which a database can be restored with
-- point-in-time restore.
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
-- 'activityStreamKmsKeyId', 'dbCluster_activityStreamKmsKeyId' - The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
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
-- 'enabledCloudwatchLogsExports', 'dbCluster_enabledCloudwatchLogsExports' - A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon Aurora User Guide./
--
-- 'activityStreamStatus', 'dbCluster_activityStreamStatus' - The status of the database activity stream.
--
-- 'engineMode', 'dbCluster_engineMode' - The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
--
-- 'multiAZ', 'dbCluster_multiAZ' - Specifies whether the DB cluster has instances in multiple Availability
-- Zones.
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
-- 'hostedZoneId', 'dbCluster_hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
--
-- 'kmsKeyId', 'dbCluster_kmsKeyId' - If @StorageEncrypted@ is enabled, the Amazon Web Services KMS key
-- identifier for the encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
--
-- 'dbClusterIdentifier', 'dbCluster_dbClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
--
-- 'engineVersion', 'dbCluster_engineVersion' - Indicates the database engine version.
--
-- 'capacity', 'dbCluster_capacity' - The current capacity of an Aurora Serverless DB cluster. The capacity is
-- 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
--
-- 'characterSetName', 'dbCluster_characterSetName' - If present, specifies the name of the character set that this cluster is
-- associated with.
--
-- 'preferredMaintenanceWindow', 'dbCluster_preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
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
-- 'endpoint', 'dbCluster_endpoint' - Specifies the connection endpoint for the primary instance of the DB
-- cluster.
--
-- 'copyTagsToSnapshot', 'dbCluster_copyTagsToSnapshot' - Specifies whether tags are copied from the DB cluster to snapshots of
-- the DB cluster.
--
-- 'scalingConfigurationInfo', 'dbCluster_scalingConfigurationInfo' - Undocumented member.
--
-- 'dbClusterResourceId', 'dbCluster_dbClusterResourceId' - The Amazon Web Services Region-unique, immutable identifier for the DB
-- cluster. This identifier is found in Amazon Web Services CloudTrail log
-- entries whenever the Amazon Web Services KMS CMK for the DB cluster is
-- accessed.
--
-- 'customEndpoints', 'dbCluster_customEndpoints' - Identifies all custom endpoints associated with the cluster.
--
-- 'tagList', 'dbCluster_tagList' - Undocumented member.
--
-- 'earliestBacktrackTime', 'dbCluster_earliestBacktrackTime' - The earliest time to which a DB cluster can be backtracked.
--
-- 'backtrackWindow', 'dbCluster_backtrackWindow' - The target backtrack window, in seconds. If this value is set to 0,
-- backtracking is disabled for the DB cluster. Otherwise, backtracking is
-- enabled.
--
-- 'allocatedStorage', 'dbCluster_allocatedStorage' - For all database engines except Amazon Aurora, @AllocatedStorage@
-- specifies the allocated storage size in gibibytes (GiB). For Aurora,
-- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
-- size isn\'t fixed, but instead automatically adjusts as needed.
--
-- 'globalWriteForwardingRequested', 'dbCluster_globalWriteForwardingRequested' - Specifies whether you have requested to enable write forwarding for a
-- secondary cluster in an Aurora global database. Because write forwarding
-- takes time to enable, check the value of @GlobalWriteForwardingStatus@
-- to confirm that the request has completed before using the write
-- forwarding feature for this cluster.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbCluster_iAMDatabaseAuthenticationEnabled' - A value that indicates whether the mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts is
-- enabled.
--
-- 'dbClusterParameterGroup', 'dbCluster_dbClusterParameterGroup' - Specifies the name of the DB cluster parameter group for the DB cluster.
--
-- 'replicationSourceIdentifier', 'dbCluster_replicationSourceIdentifier' - Contains the identifier of the source DB cluster if this DB cluster is a
-- read replica.
--
-- 'activityStreamKinesisStreamName', 'dbCluster_activityStreamKinesisStreamName' - The name of the Amazon Kinesis data stream used for the database
-- activity stream.
--
-- 'databaseName', 'dbCluster_databaseName' - Contains the name of the initial database of this DB cluster that was
-- provided at create time, if one was specified when the DB cluster was
-- created. This same name is returned for the life of the DB cluster.
--
-- 'activityStreamMode', 'dbCluster_activityStreamMode' - The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. The database session
-- can handle these events either synchronously or asynchronously.
newDBCluster ::
  DBCluster
newDBCluster =
  DBCluster'
    { associatedRoles = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing,
      automaticRestartTime = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      dbClusterMembers = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      status = Prelude.Nothing,
      backtrackConsumedChangeRecords = Prelude.Nothing,
      crossAccountClone = Prelude.Nothing,
      globalWriteForwardingStatus = Prelude.Nothing,
      domainMemberships = Prelude.Nothing,
      dbClusterArn = Prelude.Nothing,
      cloneGroupId = Prelude.Nothing,
      dbClusterOptionGroupMemberships = Prelude.Nothing,
      latestRestorableTime = Prelude.Nothing,
      readerEndpoint = Prelude.Nothing,
      activityStreamKmsKeyId = Prelude.Nothing,
      httpEndpointEnabled = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      earliestRestorableTime = Prelude.Nothing,
      enabledCloudwatchLogsExports = Prelude.Nothing,
      activityStreamStatus = Prelude.Nothing,
      engineMode = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      dbSubnetGroup = Prelude.Nothing,
      readReplicaIdentifiers = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      capacity = Prelude.Nothing,
      characterSetName = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      port = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      engine = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      scalingConfigurationInfo = Prelude.Nothing,
      dbClusterResourceId = Prelude.Nothing,
      customEndpoints = Prelude.Nothing,
      tagList = Prelude.Nothing,
      earliestBacktrackTime = Prelude.Nothing,
      backtrackWindow = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      globalWriteForwardingRequested = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled = Prelude.Nothing,
      dbClusterParameterGroup = Prelude.Nothing,
      replicationSourceIdentifier = Prelude.Nothing,
      activityStreamKinesisStreamName = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      activityStreamMode = Prelude.Nothing
    }

-- | Provides a list of the Amazon Web Services Identity and Access
-- Management (IAM) roles that are associated with the DB cluster. IAM
-- roles that are associated with a DB cluster grant permission for the DB
-- cluster to access other Amazon Web Services on your behalf.
dbCluster_associatedRoles :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterRole])
dbCluster_associatedRoles = Lens.lens (\DBCluster' {associatedRoles} -> associatedRoles) (\s@DBCluster' {} a -> s {associatedRoles = a} :: DBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | Provides a list of VPC security groups that the DB cluster belongs to.
dbCluster_vpcSecurityGroups :: Lens.Lens' DBCluster (Prelude.Maybe [VpcSecurityGroupMembership])
dbCluster_vpcSecurityGroups = Lens.lens (\DBCluster' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@DBCluster' {} a -> s {vpcSecurityGroups = a} :: DBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The time when a stopped DB cluster is restarted automatically.
dbCluster_automaticRestartTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_automaticRestartTime = Lens.lens (\DBCluster' {automaticRestartTime} -> automaticRestartTime) (\s@DBCluster' {} a -> s {automaticRestartTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | Indicates if the DB cluster has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled.
dbCluster_deletionProtection :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_deletionProtection = Lens.lens (\DBCluster' {deletionProtection} -> deletionProtection) (\s@DBCluster' {} a -> s {deletionProtection = a} :: DBCluster)

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
dbCluster_preferredBackupWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_preferredBackupWindow = Lens.lens (\DBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@DBCluster' {} a -> s {preferredBackupWindow = a} :: DBCluster)

-- | Provides the list of instances that make up the DB cluster.
dbCluster_dbClusterMembers :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterMember])
dbCluster_dbClusterMembers = Lens.lens (\DBCluster' {dbClusterMembers} -> dbClusterMembers) (\s@DBCluster' {} a -> s {dbClusterMembers = a} :: DBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
dbCluster_backupRetentionPeriod :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_backupRetentionPeriod = Lens.lens (\DBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBCluster' {} a -> s {backupRetentionPeriod = a} :: DBCluster)

-- | Specifies whether the DB cluster is encrypted.
dbCluster_storageEncrypted :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_storageEncrypted = Lens.lens (\DBCluster' {storageEncrypted} -> storageEncrypted) (\s@DBCluster' {} a -> s {storageEncrypted = a} :: DBCluster)

-- | Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster can be created.
dbCluster_availabilityZones :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_availabilityZones = Lens.lens (\DBCluster' {availabilityZones} -> availabilityZones) (\s@DBCluster' {} a -> s {availabilityZones = a} :: DBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the current state of this DB cluster.
dbCluster_status :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_status = Lens.lens (\DBCluster' {status} -> status) (\s@DBCluster' {} a -> s {status = a} :: DBCluster)

-- | The number of change records stored for Backtrack.
dbCluster_backtrackConsumedChangeRecords :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Integer)
dbCluster_backtrackConsumedChangeRecords = Lens.lens (\DBCluster' {backtrackConsumedChangeRecords} -> backtrackConsumedChangeRecords) (\s@DBCluster' {} a -> s {backtrackConsumedChangeRecords = a} :: DBCluster)

-- | Specifies whether the DB cluster is a clone of a DB cluster owned by a
-- different Amazon Web Services account.
dbCluster_crossAccountClone :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_crossAccountClone = Lens.lens (\DBCluster' {crossAccountClone} -> crossAccountClone) (\s@DBCluster' {} a -> s {crossAccountClone = a} :: DBCluster)

-- | Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
dbCluster_globalWriteForwardingStatus :: Lens.Lens' DBCluster (Prelude.Maybe WriteForwardingStatus)
dbCluster_globalWriteForwardingStatus = Lens.lens (\DBCluster' {globalWriteForwardingStatus} -> globalWriteForwardingStatus) (\s@DBCluster' {} a -> s {globalWriteForwardingStatus = a} :: DBCluster)

-- | The Active Directory Domain membership records associated with the DB
-- cluster.
dbCluster_domainMemberships :: Lens.Lens' DBCluster (Prelude.Maybe [DomainMembership])
dbCluster_domainMemberships = Lens.lens (\DBCluster' {domainMemberships} -> domainMemberships) (\s@DBCluster' {} a -> s {domainMemberships = a} :: DBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) for the DB cluster.
dbCluster_dbClusterArn :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterArn = Lens.lens (\DBCluster' {dbClusterArn} -> dbClusterArn) (\s@DBCluster' {} a -> s {dbClusterArn = a} :: DBCluster)

-- | Identifies the clone group to which the DB cluster is associated.
dbCluster_cloneGroupId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_cloneGroupId = Lens.lens (\DBCluster' {cloneGroupId} -> cloneGroupId) (\s@DBCluster' {} a -> s {cloneGroupId = a} :: DBCluster)

-- | Provides the list of option group memberships for this DB cluster.
dbCluster_dbClusterOptionGroupMemberships :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterOptionGroupStatus])
dbCluster_dbClusterOptionGroupMemberships = Lens.lens (\DBCluster' {dbClusterOptionGroupMemberships} -> dbClusterOptionGroupMemberships) (\s@DBCluster' {} a -> s {dbClusterOptionGroupMemberships = a} :: DBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbCluster_latestRestorableTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_latestRestorableTime = Lens.lens (\DBCluster' {latestRestorableTime} -> latestRestorableTime) (\s@DBCluster' {} a -> s {latestRestorableTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

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
dbCluster_readerEndpoint :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_readerEndpoint = Lens.lens (\DBCluster' {readerEndpoint} -> readerEndpoint) (\s@DBCluster' {} a -> s {readerEndpoint = a} :: DBCluster)

-- | The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
dbCluster_activityStreamKmsKeyId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_activityStreamKmsKeyId = Lens.lens (\DBCluster' {activityStreamKmsKeyId} -> activityStreamKmsKeyId) (\s@DBCluster' {} a -> s {activityStreamKmsKeyId = a} :: DBCluster)

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
dbCluster_httpEndpointEnabled :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_httpEndpointEnabled = Lens.lens (\DBCluster' {httpEndpointEnabled} -> httpEndpointEnabled) (\s@DBCluster' {} a -> s {httpEndpointEnabled = a} :: DBCluster)

-- | Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
dbCluster_clusterCreateTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_clusterCreateTime = Lens.lens (\DBCluster' {clusterCreateTime} -> clusterCreateTime) (\s@DBCluster' {} a -> s {clusterCreateTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | The earliest time to which a database can be restored with point-in-time
-- restore.
dbCluster_earliestRestorableTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_earliestRestorableTime = Lens.lens (\DBCluster' {earliestRestorableTime} -> earliestRestorableTime) (\s@DBCluster' {} a -> s {earliestRestorableTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon Aurora User Guide./
dbCluster_enabledCloudwatchLogsExports :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_enabledCloudwatchLogsExports = Lens.lens (\DBCluster' {enabledCloudwatchLogsExports} -> enabledCloudwatchLogsExports) (\s@DBCluster' {} a -> s {enabledCloudwatchLogsExports = a} :: DBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The status of the database activity stream.
dbCluster_activityStreamStatus :: Lens.Lens' DBCluster (Prelude.Maybe ActivityStreamStatus)
dbCluster_activityStreamStatus = Lens.lens (\DBCluster' {activityStreamStatus} -> activityStreamStatus) (\s@DBCluster' {} a -> s {activityStreamStatus = a} :: DBCluster)

-- | The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
dbCluster_engineMode :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engineMode = Lens.lens (\DBCluster' {engineMode} -> engineMode) (\s@DBCluster' {} a -> s {engineMode = a} :: DBCluster)

-- | Specifies whether the DB cluster has instances in multiple Availability
-- Zones.
dbCluster_multiAZ :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_multiAZ = Lens.lens (\DBCluster' {multiAZ} -> multiAZ) (\s@DBCluster' {} a -> s {multiAZ = a} :: DBCluster)

-- | Specifies information on the subnet group associated with the DB
-- cluster, including the name, description, and subnets in the subnet
-- group.
dbCluster_dbSubnetGroup :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbSubnetGroup = Lens.lens (\DBCluster' {dbSubnetGroup} -> dbSubnetGroup) (\s@DBCluster' {} a -> s {dbSubnetGroup = a} :: DBCluster)

-- | Contains one or more identifiers of the read replicas associated with
-- this DB cluster.
dbCluster_readReplicaIdentifiers :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_readReplicaIdentifiers = Lens.lens (\DBCluster' {readReplicaIdentifiers} -> readReplicaIdentifiers) (\s@DBCluster' {} a -> s {readReplicaIdentifiers = a} :: DBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | Contains the master username for the DB cluster.
dbCluster_masterUsername :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_masterUsername = Lens.lens (\DBCluster' {masterUsername} -> masterUsername) (\s@DBCluster' {} a -> s {masterUsername = a} :: DBCluster)

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
dbCluster_hostedZoneId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_hostedZoneId = Lens.lens (\DBCluster' {hostedZoneId} -> hostedZoneId) (\s@DBCluster' {} a -> s {hostedZoneId = a} :: DBCluster)

-- | If @StorageEncrypted@ is enabled, the Amazon Web Services KMS key
-- identifier for the encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
dbCluster_kmsKeyId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_kmsKeyId = Lens.lens (\DBCluster' {kmsKeyId} -> kmsKeyId) (\s@DBCluster' {} a -> s {kmsKeyId = a} :: DBCluster)

-- | Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
dbCluster_dbClusterIdentifier :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterIdentifier = Lens.lens (\DBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBCluster' {} a -> s {dbClusterIdentifier = a} :: DBCluster)

-- | Indicates the database engine version.
dbCluster_engineVersion :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engineVersion = Lens.lens (\DBCluster' {engineVersion} -> engineVersion) (\s@DBCluster' {} a -> s {engineVersion = a} :: DBCluster)

-- | The current capacity of an Aurora Serverless DB cluster. The capacity is
-- 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
dbCluster_capacity :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_capacity = Lens.lens (\DBCluster' {capacity} -> capacity) (\s@DBCluster' {} a -> s {capacity = a} :: DBCluster)

-- | If present, specifies the name of the character set that this cluster is
-- associated with.
dbCluster_characterSetName :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_characterSetName = Lens.lens (\DBCluster' {characterSetName} -> characterSetName) (\s@DBCluster' {} a -> s {characterSetName = a} :: DBCluster)

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
dbCluster_preferredMaintenanceWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_preferredMaintenanceWindow = Lens.lens (\DBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@DBCluster' {} a -> s {preferredMaintenanceWindow = a} :: DBCluster)

-- | Specifies the port that the database engine is listening on.
dbCluster_port :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_port = Lens.lens (\DBCluster' {port} -> port) (\s@DBCluster' {} a -> s {port = a} :: DBCluster)

-- | Specifies the progress of the operation as a percentage.
dbCluster_percentProgress :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_percentProgress = Lens.lens (\DBCluster' {percentProgress} -> percentProgress) (\s@DBCluster' {} a -> s {percentProgress = a} :: DBCluster)

-- | The name of the database engine to be used for this DB cluster.
dbCluster_engine :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engine = Lens.lens (\DBCluster' {engine} -> engine) (\s@DBCluster' {} a -> s {engine = a} :: DBCluster)

-- | A value that specifies that changes to the DB cluster are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
dbCluster_pendingModifiedValues :: Lens.Lens' DBCluster (Prelude.Maybe ClusterPendingModifiedValues)
dbCluster_pendingModifiedValues = Lens.lens (\DBCluster' {pendingModifiedValues} -> pendingModifiedValues) (\s@DBCluster' {} a -> s {pendingModifiedValues = a} :: DBCluster)

-- | Specifies the connection endpoint for the primary instance of the DB
-- cluster.
dbCluster_endpoint :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_endpoint = Lens.lens (\DBCluster' {endpoint} -> endpoint) (\s@DBCluster' {} a -> s {endpoint = a} :: DBCluster)

-- | Specifies whether tags are copied from the DB cluster to snapshots of
-- the DB cluster.
dbCluster_copyTagsToSnapshot :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_copyTagsToSnapshot = Lens.lens (\DBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@DBCluster' {} a -> s {copyTagsToSnapshot = a} :: DBCluster)

-- | Undocumented member.
dbCluster_scalingConfigurationInfo :: Lens.Lens' DBCluster (Prelude.Maybe ScalingConfigurationInfo)
dbCluster_scalingConfigurationInfo = Lens.lens (\DBCluster' {scalingConfigurationInfo} -> scalingConfigurationInfo) (\s@DBCluster' {} a -> s {scalingConfigurationInfo = a} :: DBCluster)

-- | The Amazon Web Services Region-unique, immutable identifier for the DB
-- cluster. This identifier is found in Amazon Web Services CloudTrail log
-- entries whenever the Amazon Web Services KMS CMK for the DB cluster is
-- accessed.
dbCluster_dbClusterResourceId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterResourceId = Lens.lens (\DBCluster' {dbClusterResourceId} -> dbClusterResourceId) (\s@DBCluster' {} a -> s {dbClusterResourceId = a} :: DBCluster)

-- | Identifies all custom endpoints associated with the cluster.
dbCluster_customEndpoints :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_customEndpoints = Lens.lens (\DBCluster' {customEndpoints} -> customEndpoints) (\s@DBCluster' {} a -> s {customEndpoints = a} :: DBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
dbCluster_tagList :: Lens.Lens' DBCluster (Prelude.Maybe [Tag])
dbCluster_tagList = Lens.lens (\DBCluster' {tagList} -> tagList) (\s@DBCluster' {} a -> s {tagList = a} :: DBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The earliest time to which a DB cluster can be backtracked.
dbCluster_earliestBacktrackTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_earliestBacktrackTime = Lens.lens (\DBCluster' {earliestBacktrackTime} -> earliestBacktrackTime) (\s@DBCluster' {} a -> s {earliestBacktrackTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | The target backtrack window, in seconds. If this value is set to 0,
-- backtracking is disabled for the DB cluster. Otherwise, backtracking is
-- enabled.
dbCluster_backtrackWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Integer)
dbCluster_backtrackWindow = Lens.lens (\DBCluster' {backtrackWindow} -> backtrackWindow) (\s@DBCluster' {} a -> s {backtrackWindow = a} :: DBCluster)

-- | For all database engines except Amazon Aurora, @AllocatedStorage@
-- specifies the allocated storage size in gibibytes (GiB). For Aurora,
-- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
-- size isn\'t fixed, but instead automatically adjusts as needed.
dbCluster_allocatedStorage :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_allocatedStorage = Lens.lens (\DBCluster' {allocatedStorage} -> allocatedStorage) (\s@DBCluster' {} a -> s {allocatedStorage = a} :: DBCluster)

-- | Specifies whether you have requested to enable write forwarding for a
-- secondary cluster in an Aurora global database. Because write forwarding
-- takes time to enable, check the value of @GlobalWriteForwardingStatus@
-- to confirm that the request has completed before using the write
-- forwarding feature for this cluster.
dbCluster_globalWriteForwardingRequested :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_globalWriteForwardingRequested = Lens.lens (\DBCluster' {globalWriteForwardingRequested} -> globalWriteForwardingRequested) (\s@DBCluster' {} a -> s {globalWriteForwardingRequested = a} :: DBCluster)

-- | A value that indicates whether the mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts is
-- enabled.
dbCluster_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBCluster' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBCluster' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBCluster)

-- | Specifies the name of the DB cluster parameter group for the DB cluster.
dbCluster_dbClusterParameterGroup :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterParameterGroup = Lens.lens (\DBCluster' {dbClusterParameterGroup} -> dbClusterParameterGroup) (\s@DBCluster' {} a -> s {dbClusterParameterGroup = a} :: DBCluster)

-- | Contains the identifier of the source DB cluster if this DB cluster is a
-- read replica.
dbCluster_replicationSourceIdentifier :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_replicationSourceIdentifier = Lens.lens (\DBCluster' {replicationSourceIdentifier} -> replicationSourceIdentifier) (\s@DBCluster' {} a -> s {replicationSourceIdentifier = a} :: DBCluster)

-- | The name of the Amazon Kinesis data stream used for the database
-- activity stream.
dbCluster_activityStreamKinesisStreamName :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_activityStreamKinesisStreamName = Lens.lens (\DBCluster' {activityStreamKinesisStreamName} -> activityStreamKinesisStreamName) (\s@DBCluster' {} a -> s {activityStreamKinesisStreamName = a} :: DBCluster)

-- | Contains the name of the initial database of this DB cluster that was
-- provided at create time, if one was specified when the DB cluster was
-- created. This same name is returned for the life of the DB cluster.
dbCluster_databaseName :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_databaseName = Lens.lens (\DBCluster' {databaseName} -> databaseName) (\s@DBCluster' {} a -> s {databaseName = a} :: DBCluster)

-- | The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. The database session
-- can handle these events either synchronously or asynchronously.
dbCluster_activityStreamMode :: Lens.Lens' DBCluster (Prelude.Maybe ActivityStreamMode)
dbCluster_activityStreamMode = Lens.lens (\DBCluster' {activityStreamMode} -> activityStreamMode) (\s@DBCluster' {} a -> s {activityStreamMode = a} :: DBCluster)

instance Core.FromXML DBCluster where
  parseXML x =
    DBCluster'
      Prelude.<$> ( x Core..@? "AssociatedRoles" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBClusterRole")
                  )
      Prelude.<*> ( x Core..@? "VpcSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "VpcSecurityGroupMembership")
                  )
      Prelude.<*> (x Core..@? "AutomaticRestartTime")
      Prelude.<*> (x Core..@? "DeletionProtection")
      Prelude.<*> (x Core..@? "PreferredBackupWindow")
      Prelude.<*> ( x Core..@? "DBClusterMembers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBClusterMember")
                  )
      Prelude.<*> (x Core..@? "BackupRetentionPeriod")
      Prelude.<*> (x Core..@? "StorageEncrypted")
      Prelude.<*> ( x Core..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "BacktrackConsumedChangeRecords")
      Prelude.<*> (x Core..@? "CrossAccountClone")
      Prelude.<*> (x Core..@? "GlobalWriteForwardingStatus")
      Prelude.<*> ( x Core..@? "DomainMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DomainMembership")
                  )
      Prelude.<*> (x Core..@? "DBClusterArn")
      Prelude.<*> (x Core..@? "CloneGroupId")
      Prelude.<*> ( x Core..@? "DBClusterOptionGroupMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBClusterOptionGroup")
                  )
      Prelude.<*> (x Core..@? "LatestRestorableTime")
      Prelude.<*> (x Core..@? "ReaderEndpoint")
      Prelude.<*> (x Core..@? "ActivityStreamKmsKeyId")
      Prelude.<*> (x Core..@? "HttpEndpointEnabled")
      Prelude.<*> (x Core..@? "ClusterCreateTime")
      Prelude.<*> (x Core..@? "EarliestRestorableTime")
      Prelude.<*> ( x Core..@? "EnabledCloudwatchLogsExports"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "ActivityStreamStatus")
      Prelude.<*> (x Core..@? "EngineMode")
      Prelude.<*> (x Core..@? "MultiAZ")
      Prelude.<*> (x Core..@? "DBSubnetGroup")
      Prelude.<*> ( x Core..@? "ReadReplicaIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ReadReplicaIdentifier")
                  )
      Prelude.<*> (x Core..@? "MasterUsername")
      Prelude.<*> (x Core..@? "HostedZoneId")
      Prelude.<*> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "DBClusterIdentifier")
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "Capacity")
      Prelude.<*> (x Core..@? "CharacterSetName")
      Prelude.<*> (x Core..@? "PreferredMaintenanceWindow")
      Prelude.<*> (x Core..@? "Port")
      Prelude.<*> (x Core..@? "PercentProgress")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "PendingModifiedValues")
      Prelude.<*> (x Core..@? "Endpoint")
      Prelude.<*> (x Core..@? "CopyTagsToSnapshot")
      Prelude.<*> (x Core..@? "ScalingConfigurationInfo")
      Prelude.<*> (x Core..@? "DbClusterResourceId")
      Prelude.<*> ( x Core..@? "CustomEndpoints" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "EarliestBacktrackTime")
      Prelude.<*> (x Core..@? "BacktrackWindow")
      Prelude.<*> (x Core..@? "AllocatedStorage")
      Prelude.<*> (x Core..@? "GlobalWriteForwardingRequested")
      Prelude.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Core..@? "DBClusterParameterGroup")
      Prelude.<*> (x Core..@? "ReplicationSourceIdentifier")
      Prelude.<*> (x Core..@? "ActivityStreamKinesisStreamName")
      Prelude.<*> (x Core..@? "DatabaseName")
      Prelude.<*> (x Core..@? "ActivityStreamMode")

instance Prelude.Hashable DBCluster

instance Prelude.NFData DBCluster
