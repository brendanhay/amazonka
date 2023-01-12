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
-- Module      : Amazonka.RDS.Types.DBCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBCluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.ActivityStreamMode
import Amazonka.RDS.Types.ActivityStreamStatus
import Amazonka.RDS.Types.ClusterPendingModifiedValues
import Amazonka.RDS.Types.DBClusterMember
import Amazonka.RDS.Types.DBClusterOptionGroupStatus
import Amazonka.RDS.Types.DBClusterRole
import Amazonka.RDS.Types.DomainMembership
import Amazonka.RDS.Types.MasterUserSecret
import Amazonka.RDS.Types.ScalingConfigurationInfo
import Amazonka.RDS.Types.ServerlessV2ScalingConfigurationInfo
import Amazonka.RDS.Types.Tag
import Amazonka.RDS.Types.VpcSecurityGroupMembership
import Amazonka.RDS.Types.WriteForwardingStatus

-- | Contains the details of an Amazon Aurora DB cluster or Multi-AZ DB
-- cluster.
--
-- For an Amazon Aurora DB cluster, this data type is used as a response
-- element in the operations @CreateDBCluster@, @DeleteDBCluster@,
-- @DescribeDBClusters@, @FailoverDBCluster@, @ModifyDBCluster@,
-- @PromoteReadReplicaDBCluster@, @RestoreDBClusterFromS3@,
-- @RestoreDBClusterFromSnapshot@, @RestoreDBClusterToPointInTime@,
-- @StartDBCluster@, and @StopDBCluster@.
--
-- For a Multi-AZ DB cluster, this data type is used as a response element
-- in the operations @CreateDBCluster@, @DeleteDBCluster@,
-- @DescribeDBClusters@, @FailoverDBCluster@, @ModifyDBCluster@,
-- @RebootDBCluster@, @RestoreDBClusterFromSnapshot@, and
-- @RestoreDBClusterToPointInTime@.
--
-- For more information on Amazon Aurora DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide./
--
-- /See:/ 'newDBCluster' smart constructor.
data DBCluster = DBCluster'
  { -- | The name of the Amazon Kinesis data stream used for the database
    -- activity stream.
    activityStreamKinesisStreamName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier used for encrypting messages
    -- in the database activity stream.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    activityStreamKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The mode of the database activity stream. Database events such as a
    -- change or access generate an activity stream event. The database session
    -- can handle these events either synchronously or asynchronously.
    activityStreamMode :: Prelude.Maybe ActivityStreamMode,
    -- | The status of the database activity stream.
    activityStreamStatus :: Prelude.Maybe ActivityStreamStatus,
    -- | For all database engines except Amazon Aurora, @AllocatedStorage@
    -- specifies the allocated storage size in gibibytes (GiB). For Aurora,
    -- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
    -- size isn\'t fixed, but instead automatically adjusts as needed.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Provides a list of the Amazon Web Services Identity and Access
    -- Management (IAM) roles that are associated with the DB cluster. IAM
    -- roles that are associated with a DB cluster grant permission for the DB
    -- cluster to access other Amazon Web Services on your behalf.
    associatedRoles :: Prelude.Maybe [DBClusterRole],
    -- | A value that indicates that minor version patches are applied
    -- automatically.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The time when a stopped DB cluster is restarted automatically.
    automaticRestartTime :: Prelude.Maybe Data.ISO8601,
    -- | Provides the list of Availability Zones (AZs) where instances in the DB
    -- cluster can be created.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The number of change records stored for Backtrack.
    backtrackConsumedChangeRecords :: Prelude.Maybe Prelude.Integer,
    -- | The target backtrack window, in seconds. If this value is set to 0,
    -- backtracking is disabled for the DB cluster. Otherwise, backtracking is
    -- enabled.
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the number of days for which automatic DB snapshots are
    -- retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The current capacity of an Aurora Serverless v1 DB cluster. The capacity
    -- is 0 (zero) when the cluster is paused.
    --
    -- For more information about Aurora Serverless v1, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless v1>
    -- in the /Amazon Aurora User Guide/.
    capacity :: Prelude.Maybe Prelude.Int,
    -- | If present, specifies the name of the character set that this cluster is
    -- associated with.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | Identifies the clone group to which the DB cluster is associated.
    cloneGroupId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time when the DB cluster was created, in Universal
    -- Coordinated Time (UTC).
    clusterCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | Specifies whether tags are copied from the DB cluster to snapshots of
    -- the DB cluster.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the DB cluster is a clone of a DB cluster owned by a
    -- different Amazon Web Services account.
    crossAccountClone :: Prelude.Maybe Prelude.Bool,
    -- | Identifies all custom endpoints associated with the cluster.
    customEndpoints :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) for the DB cluster.
    dbClusterArn :: Prelude.Maybe Prelude.Text,
    -- | Contains a user-supplied DB cluster identifier. This identifier is the
    -- unique key that identifies a DB cluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute and memory capacity class of the DB instance.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    dbClusterInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of instances that make up the DB cluster.
    dbClusterMembers :: Prelude.Maybe [DBClusterMember],
    -- | Provides the list of option group memberships for this DB cluster.
    dbClusterOptionGroupMemberships :: Prelude.Maybe [DBClusterOptionGroupStatus],
    -- | Specifies the name of the DB cluster parameter group for the DB cluster.
    dbClusterParameterGroup :: Prelude.Maybe Prelude.Text,
    -- | Specifies information on the subnet group associated with the DB
    -- cluster, including the name, description, and subnets in the subnet
    -- group.
    dbSubnetGroup :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    dbSystemId :: Prelude.Maybe Prelude.Text,
    -- | Contains the name of the initial database of this DB cluster that was
    -- provided at create time, if one was specified when the DB cluster was
    -- created. This same name is returned for the life of the DB cluster.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region-unique, immutable identifier for the DB
    -- cluster. This identifier is found in Amazon Web Services CloudTrail log
    -- entries whenever the KMS key for the DB cluster is accessed.
    dbClusterResourceId :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the DB cluster has deletion protection enabled. The
    -- database can\'t be deleted when deletion protection is enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The Active Directory Domain membership records associated with the DB
    -- cluster.
    domainMemberships :: Prelude.Maybe [DomainMembership],
    -- | The earliest time to which a DB cluster can be backtracked.
    earliestBacktrackTime :: Prelude.Maybe Data.ISO8601,
    -- | The earliest time to which a database can be restored with point-in-time
    -- restore.
    earliestRestorableTime :: Prelude.Maybe Data.ISO8601,
    -- | A list of log types that this DB cluster is configured to export to
    -- CloudWatch Logs.
    --
    -- Log types vary by DB engine. For information about the log types for
    -- each DB engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
    -- in the /Amazon Aurora User Guide./
    enabledCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the connection endpoint for the primary instance of the DB
    -- cluster.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine to be used for this DB cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The DB engine mode of the DB cluster, either @provisioned@,
    -- @serverless@, @parallelquery@, @global@, or @multimaster@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether you have requested to enable write forwarding for a
    -- secondary cluster in an Aurora global database. Because write forwarding
    -- takes time to enable, check the value of @GlobalWriteForwardingStatus@
    -- to confirm that the request has completed before using the write
    -- forwarding feature for this cluster.
    globalWriteForwardingRequested :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a secondary cluster in an Aurora global database has
    -- write forwarding enabled, not enabled, or is in the process of enabling
    -- it.
    globalWriteForwardingStatus :: Prelude.Maybe WriteForwardingStatus,
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
    -- zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the HTTP endpoint for an Aurora
    -- Serverless v1 DB cluster is enabled.
    --
    -- When enabled, the HTTP endpoint provides a connectionless web service
    -- API for running SQL queries on the Aurora Serverless v1 DB cluster. You
    -- can also query your database from inside the RDS console with the query
    -- editor.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless v1>
    -- in the /Amazon Aurora User Guide/.
    httpEndpointEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts is
    -- enabled.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Provisioned IOPS (I\/O operations per second) value.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    iops :: Prelude.Maybe Prelude.Int,
    -- | If @StorageEncrypted@ is enabled, the Amazon Web Services KMS key
    -- identifier for the encrypted DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the latest time to which a database can be restored with
    -- point-in-time restore.
    latestRestorableTime :: Prelude.Maybe Data.ISO8601,
    -- | Contains the secret managed by RDS in Amazon Web Services Secrets
    -- Manager for the master user password.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon RDS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon Aurora User Guide./
    masterUserSecret :: Prelude.Maybe MasterUserSecret,
    -- | Contains the master username for the DB cluster.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB cluster.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring
    -- metrics to Amazon CloudWatch Logs.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the DB cluster has instances in multiple Availability
    -- Zones.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The network type of the DB instance.
    --
    -- Valid values:
    --
    -- -   @IPV4@
    --
    -- -   @DUAL@
    --
    -- The network type is determined by the @DBSubnetGroup@ specified for the
    -- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
    -- IPv4 and the IPv6 protocols (@DUAL@).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
    -- in the /Amazon Aurora User Guide./
    --
    -- This setting is only for Aurora DB clusters.
    networkType :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies that changes to the DB cluster are pending. This
    -- element is only included when changes are pending. Specific changes are
    -- identified by subelements.
    pendingModifiedValues :: Prelude.Maybe ClusterPendingModifiedValues,
    -- | Specifies the progress of the operation as a percentage.
    percentProgress :: Prelude.Maybe Prelude.Text,
    -- | True if Performance Insights is enabled for the DB cluster, and
    -- otherwise false.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    performanceInsightsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services KMS key identifier for encryption of Performance
    -- Insights data.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain Performance Insights data. The default is 7
    -- days. The following values are valid:
    --
    -- -   7
    --
    -- -   /month/ * 31, where /month/ is a number of months from 1-23
    --
    -- -   731
    --
    -- For example, the following values are valid:
    --
    -- -   93 (3 months * 31)
    --
    -- -   341 (11 months * 31)
    --
    -- -   589 (19 months * 31)
    --
    -- -   731
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Specifies the port that the database engine is listening on.
    port :: Prelude.Maybe Prelude.Int,
    -- | Specifies the daily time range during which automated backups are
    -- created if automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which system maintenance can
    -- occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the accessibility options for the DB instance.
    --
    -- When the DB instance is publicly accessible, its Domain Name System
    -- (DNS) endpoint resolves to the private IP address from within the DB
    -- instance\'s virtual private cloud (VPC). It resolves to the public IP
    -- address from outside of the DB instance\'s VPC. Access to the DB
    -- instance is ultimately controlled by the security group it uses. That
    -- public access is not permitted if the security group assigned to the DB
    -- instance doesn\'t permit it.
    --
    -- When the DB instance isn\'t publicly accessible, it is an internal DB
    -- instance with a DNS name that resolves to a private IP address.
    --
    -- For more information, see CreateDBInstance.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Contains one or more identifiers of the read replicas associated with
    -- this DB cluster.
    readReplicaIdentifiers :: Prelude.Maybe [Prelude.Text],
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
    -- | Contains the identifier of the source DB cluster if this DB cluster is a
    -- read replica.
    replicationSourceIdentifier :: Prelude.Maybe Prelude.Text,
    scalingConfigurationInfo :: Prelude.Maybe ScalingConfigurationInfo,
    serverlessV2ScalingConfiguration :: Prelude.Maybe ServerlessV2ScalingConfigurationInfo,
    -- | Specifies the current state of this DB cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The storage type associated with the DB cluster.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    storageType :: Prelude.Maybe Prelude.Text,
    tagList :: Prelude.Maybe [Tag],
    -- | Provides a list of VPC security groups that the DB cluster belongs to.
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
-- 'activityStreamKinesisStreamName', 'dbCluster_activityStreamKinesisStreamName' - The name of the Amazon Kinesis data stream used for the database
-- activity stream.
--
-- 'activityStreamKmsKeyId', 'dbCluster_activityStreamKmsKeyId' - The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- 'activityStreamMode', 'dbCluster_activityStreamMode' - The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. The database session
-- can handle these events either synchronously or asynchronously.
--
-- 'activityStreamStatus', 'dbCluster_activityStreamStatus' - The status of the database activity stream.
--
-- 'allocatedStorage', 'dbCluster_allocatedStorage' - For all database engines except Amazon Aurora, @AllocatedStorage@
-- specifies the allocated storage size in gibibytes (GiB). For Aurora,
-- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
-- size isn\'t fixed, but instead automatically adjusts as needed.
--
-- 'associatedRoles', 'dbCluster_associatedRoles' - Provides a list of the Amazon Web Services Identity and Access
-- Management (IAM) roles that are associated with the DB cluster. IAM
-- roles that are associated with a DB cluster grant permission for the DB
-- cluster to access other Amazon Web Services on your behalf.
--
-- 'autoMinorVersionUpgrade', 'dbCluster_autoMinorVersionUpgrade' - A value that indicates that minor version patches are applied
-- automatically.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'automaticRestartTime', 'dbCluster_automaticRestartTime' - The time when a stopped DB cluster is restarted automatically.
--
-- 'availabilityZones', 'dbCluster_availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster can be created.
--
-- 'backtrackConsumedChangeRecords', 'dbCluster_backtrackConsumedChangeRecords' - The number of change records stored for Backtrack.
--
-- 'backtrackWindow', 'dbCluster_backtrackWindow' - The target backtrack window, in seconds. If this value is set to 0,
-- backtracking is disabled for the DB cluster. Otherwise, backtracking is
-- enabled.
--
-- 'backupRetentionPeriod', 'dbCluster_backupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are
-- retained.
--
-- 'capacity', 'dbCluster_capacity' - The current capacity of an Aurora Serverless v1 DB cluster. The capacity
-- is 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless v1, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
--
-- 'characterSetName', 'dbCluster_characterSetName' - If present, specifies the name of the character set that this cluster is
-- associated with.
--
-- 'cloneGroupId', 'dbCluster_cloneGroupId' - Identifies the clone group to which the DB cluster is associated.
--
-- 'clusterCreateTime', 'dbCluster_clusterCreateTime' - Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
--
-- 'copyTagsToSnapshot', 'dbCluster_copyTagsToSnapshot' - Specifies whether tags are copied from the DB cluster to snapshots of
-- the DB cluster.
--
-- 'crossAccountClone', 'dbCluster_crossAccountClone' - Specifies whether the DB cluster is a clone of a DB cluster owned by a
-- different Amazon Web Services account.
--
-- 'customEndpoints', 'dbCluster_customEndpoints' - Identifies all custom endpoints associated with the cluster.
--
-- 'dbClusterArn', 'dbCluster_dbClusterArn' - The Amazon Resource Name (ARN) for the DB cluster.
--
-- 'dbClusterIdentifier', 'dbCluster_dbClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
--
-- 'dbClusterInstanceClass', 'dbCluster_dbClusterInstanceClass' - The name of the compute and memory capacity class of the DB instance.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'dbClusterMembers', 'dbCluster_dbClusterMembers' - Provides the list of instances that make up the DB cluster.
--
-- 'dbClusterOptionGroupMemberships', 'dbCluster_dbClusterOptionGroupMemberships' - Provides the list of option group memberships for this DB cluster.
--
-- 'dbClusterParameterGroup', 'dbCluster_dbClusterParameterGroup' - Specifies the name of the DB cluster parameter group for the DB cluster.
--
-- 'dbSubnetGroup', 'dbCluster_dbSubnetGroup' - Specifies information on the subnet group associated with the DB
-- cluster, including the name, description, and subnets in the subnet
-- group.
--
-- 'dbSystemId', 'dbCluster_dbSystemId' - Reserved for future use.
--
-- 'databaseName', 'dbCluster_databaseName' - Contains the name of the initial database of this DB cluster that was
-- provided at create time, if one was specified when the DB cluster was
-- created. This same name is returned for the life of the DB cluster.
--
-- 'dbClusterResourceId', 'dbCluster_dbClusterResourceId' - The Amazon Web Services Region-unique, immutable identifier for the DB
-- cluster. This identifier is found in Amazon Web Services CloudTrail log
-- entries whenever the KMS key for the DB cluster is accessed.
--
-- 'deletionProtection', 'dbCluster_deletionProtection' - Indicates if the DB cluster has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled.
--
-- 'domainMemberships', 'dbCluster_domainMemberships' - The Active Directory Domain membership records associated with the DB
-- cluster.
--
-- 'earliestBacktrackTime', 'dbCluster_earliestBacktrackTime' - The earliest time to which a DB cluster can be backtracked.
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
-- 'endpoint', 'dbCluster_endpoint' - Specifies the connection endpoint for the primary instance of the DB
-- cluster.
--
-- 'engine', 'dbCluster_engine' - The name of the database engine to be used for this DB cluster.
--
-- 'engineMode', 'dbCluster_engineMode' - The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
--
-- 'engineVersion', 'dbCluster_engineVersion' - Indicates the database engine version.
--
-- 'globalWriteForwardingRequested', 'dbCluster_globalWriteForwardingRequested' - Specifies whether you have requested to enable write forwarding for a
-- secondary cluster in an Aurora global database. Because write forwarding
-- takes time to enable, check the value of @GlobalWriteForwardingStatus@
-- to confirm that the request has completed before using the write
-- forwarding feature for this cluster.
--
-- 'globalWriteForwardingStatus', 'dbCluster_globalWriteForwardingStatus' - Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
--
-- 'hostedZoneId', 'dbCluster_hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
--
-- 'httpEndpointEnabled', 'dbCluster_httpEndpointEnabled' - A value that indicates whether the HTTP endpoint for an Aurora
-- Serverless v1 DB cluster is enabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service
-- API for running SQL queries on the Aurora Serverless v1 DB cluster. You
-- can also query your database from inside the RDS console with the query
-- editor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbCluster_iAMDatabaseAuthenticationEnabled' - A value that indicates whether the mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts is
-- enabled.
--
-- 'iops', 'dbCluster_iops' - The Provisioned IOPS (I\/O operations per second) value.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'kmsKeyId', 'dbCluster_kmsKeyId' - If @StorageEncrypted@ is enabled, the Amazon Web Services KMS key
-- identifier for the encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- 'latestRestorableTime', 'dbCluster_latestRestorableTime' - Specifies the latest time to which a database can be restored with
-- point-in-time restore.
--
-- 'masterUserSecret', 'dbCluster_masterUserSecret' - Contains the secret managed by RDS in Amazon Web Services Secrets
-- Manager for the master user password.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
--
-- 'masterUsername', 'dbCluster_masterUsername' - Contains the master username for the DB cluster.
--
-- 'monitoringInterval', 'dbCluster_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB cluster.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'monitoringRoleArn', 'dbCluster_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send Enhanced Monitoring
-- metrics to Amazon CloudWatch Logs.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'multiAZ', 'dbCluster_multiAZ' - Specifies whether the DB cluster has instances in multiple Availability
-- Zones.
--
-- 'networkType', 'dbCluster_networkType' - The network type of the DB instance.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon Aurora User Guide./
--
-- This setting is only for Aurora DB clusters.
--
-- 'pendingModifiedValues', 'dbCluster_pendingModifiedValues' - A value that specifies that changes to the DB cluster are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
--
-- 'percentProgress', 'dbCluster_percentProgress' - Specifies the progress of the operation as a percentage.
--
-- 'performanceInsightsEnabled', 'dbCluster_performanceInsightsEnabled' - True if Performance Insights is enabled for the DB cluster, and
-- otherwise false.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'performanceInsightsKMSKeyId', 'dbCluster_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'performanceInsightsRetentionPeriod', 'dbCluster_performanceInsightsRetentionPeriod' - The number of days to retain Performance Insights data. The default is 7
-- days. The following values are valid:
--
-- -   7
--
-- -   /month/ * 31, where /month/ is a number of months from 1-23
--
-- -   731
--
-- For example, the following values are valid:
--
-- -   93 (3 months * 31)
--
-- -   341 (11 months * 31)
--
-- -   589 (19 months * 31)
--
-- -   731
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'port', 'dbCluster_port' - Specifies the port that the database engine is listening on.
--
-- 'preferredBackupWindow', 'dbCluster_preferredBackupWindow' - Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
--
-- 'preferredMaintenanceWindow', 'dbCluster_preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
--
-- 'publiclyAccessible', 'dbCluster_publiclyAccessible' - Specifies the accessibility options for the DB instance.
--
-- When the DB instance is publicly accessible, its Domain Name System
-- (DNS) endpoint resolves to the private IP address from within the DB
-- instance\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB instance\'s VPC. Access to the DB
-- instance is ultimately controlled by the security group it uses. That
-- public access is not permitted if the security group assigned to the DB
-- instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'readReplicaIdentifiers', 'dbCluster_readReplicaIdentifiers' - Contains one or more identifiers of the read replicas associated with
-- this DB cluster.
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
-- 'replicationSourceIdentifier', 'dbCluster_replicationSourceIdentifier' - Contains the identifier of the source DB cluster if this DB cluster is a
-- read replica.
--
-- 'scalingConfigurationInfo', 'dbCluster_scalingConfigurationInfo' - Undocumented member.
--
-- 'serverlessV2ScalingConfiguration', 'dbCluster_serverlessV2ScalingConfiguration' - Undocumented member.
--
-- 'status', 'dbCluster_status' - Specifies the current state of this DB cluster.
--
-- 'storageEncrypted', 'dbCluster_storageEncrypted' - Specifies whether the DB cluster is encrypted.
--
-- 'storageType', 'dbCluster_storageType' - The storage type associated with the DB cluster.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'tagList', 'dbCluster_tagList' - Undocumented member.
--
-- 'vpcSecurityGroups', 'dbCluster_vpcSecurityGroups' - Provides a list of VPC security groups that the DB cluster belongs to.
newDBCluster ::
  DBCluster
newDBCluster =
  DBCluster'
    { activityStreamKinesisStreamName =
        Prelude.Nothing,
      activityStreamKmsKeyId = Prelude.Nothing,
      activityStreamMode = Prelude.Nothing,
      activityStreamStatus = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      associatedRoles = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      automaticRestartTime = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      backtrackConsumedChangeRecords = Prelude.Nothing,
      backtrackWindow = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      capacity = Prelude.Nothing,
      characterSetName = Prelude.Nothing,
      cloneGroupId = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      crossAccountClone = Prelude.Nothing,
      customEndpoints = Prelude.Nothing,
      dbClusterArn = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      dbClusterInstanceClass = Prelude.Nothing,
      dbClusterMembers = Prelude.Nothing,
      dbClusterOptionGroupMemberships = Prelude.Nothing,
      dbClusterParameterGroup = Prelude.Nothing,
      dbSubnetGroup = Prelude.Nothing,
      dbSystemId = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      dbClusterResourceId = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      domainMemberships = Prelude.Nothing,
      earliestBacktrackTime = Prelude.Nothing,
      earliestRestorableTime = Prelude.Nothing,
      enabledCloudwatchLogsExports = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineMode = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      globalWriteForwardingRequested = Prelude.Nothing,
      globalWriteForwardingStatus = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      httpEndpointEnabled = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled = Prelude.Nothing,
      iops = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      latestRestorableTime = Prelude.Nothing,
      masterUserSecret = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      monitoringInterval = Prelude.Nothing,
      monitoringRoleArn = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      networkType = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      performanceInsightsEnabled = Prelude.Nothing,
      performanceInsightsKMSKeyId = Prelude.Nothing,
      performanceInsightsRetentionPeriod = Prelude.Nothing,
      port = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      readReplicaIdentifiers = Prelude.Nothing,
      readerEndpoint = Prelude.Nothing,
      replicationSourceIdentifier = Prelude.Nothing,
      scalingConfigurationInfo = Prelude.Nothing,
      serverlessV2ScalingConfiguration = Prelude.Nothing,
      status = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      storageType = Prelude.Nothing,
      tagList = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing
    }

-- | The name of the Amazon Kinesis data stream used for the database
-- activity stream.
dbCluster_activityStreamKinesisStreamName :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_activityStreamKinesisStreamName = Lens.lens (\DBCluster' {activityStreamKinesisStreamName} -> activityStreamKinesisStreamName) (\s@DBCluster' {} a -> s {activityStreamKinesisStreamName = a} :: DBCluster)

-- | The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
dbCluster_activityStreamKmsKeyId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_activityStreamKmsKeyId = Lens.lens (\DBCluster' {activityStreamKmsKeyId} -> activityStreamKmsKeyId) (\s@DBCluster' {} a -> s {activityStreamKmsKeyId = a} :: DBCluster)

-- | The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. The database session
-- can handle these events either synchronously or asynchronously.
dbCluster_activityStreamMode :: Lens.Lens' DBCluster (Prelude.Maybe ActivityStreamMode)
dbCluster_activityStreamMode = Lens.lens (\DBCluster' {activityStreamMode} -> activityStreamMode) (\s@DBCluster' {} a -> s {activityStreamMode = a} :: DBCluster)

-- | The status of the database activity stream.
dbCluster_activityStreamStatus :: Lens.Lens' DBCluster (Prelude.Maybe ActivityStreamStatus)
dbCluster_activityStreamStatus = Lens.lens (\DBCluster' {activityStreamStatus} -> activityStreamStatus) (\s@DBCluster' {} a -> s {activityStreamStatus = a} :: DBCluster)

-- | For all database engines except Amazon Aurora, @AllocatedStorage@
-- specifies the allocated storage size in gibibytes (GiB). For Aurora,
-- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
-- size isn\'t fixed, but instead automatically adjusts as needed.
dbCluster_allocatedStorage :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_allocatedStorage = Lens.lens (\DBCluster' {allocatedStorage} -> allocatedStorage) (\s@DBCluster' {} a -> s {allocatedStorage = a} :: DBCluster)

-- | Provides a list of the Amazon Web Services Identity and Access
-- Management (IAM) roles that are associated with the DB cluster. IAM
-- roles that are associated with a DB cluster grant permission for the DB
-- cluster to access other Amazon Web Services on your behalf.
dbCluster_associatedRoles :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterRole])
dbCluster_associatedRoles = Lens.lens (\DBCluster' {associatedRoles} -> associatedRoles) (\s@DBCluster' {} a -> s {associatedRoles = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates that minor version patches are applied
-- automatically.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_autoMinorVersionUpgrade :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_autoMinorVersionUpgrade = Lens.lens (\DBCluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@DBCluster' {} a -> s {autoMinorVersionUpgrade = a} :: DBCluster)

-- | The time when a stopped DB cluster is restarted automatically.
dbCluster_automaticRestartTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_automaticRestartTime = Lens.lens (\DBCluster' {automaticRestartTime} -> automaticRestartTime) (\s@DBCluster' {} a -> s {automaticRestartTime = a} :: DBCluster) Prelude.. Lens.mapping Data._Time

-- | Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster can be created.
dbCluster_availabilityZones :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_availabilityZones = Lens.lens (\DBCluster' {availabilityZones} -> availabilityZones) (\s@DBCluster' {} a -> s {availabilityZones = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The number of change records stored for Backtrack.
dbCluster_backtrackConsumedChangeRecords :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Integer)
dbCluster_backtrackConsumedChangeRecords = Lens.lens (\DBCluster' {backtrackConsumedChangeRecords} -> backtrackConsumedChangeRecords) (\s@DBCluster' {} a -> s {backtrackConsumedChangeRecords = a} :: DBCluster)

-- | The target backtrack window, in seconds. If this value is set to 0,
-- backtracking is disabled for the DB cluster. Otherwise, backtracking is
-- enabled.
dbCluster_backtrackWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Integer)
dbCluster_backtrackWindow = Lens.lens (\DBCluster' {backtrackWindow} -> backtrackWindow) (\s@DBCluster' {} a -> s {backtrackWindow = a} :: DBCluster)

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
dbCluster_backupRetentionPeriod :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_backupRetentionPeriod = Lens.lens (\DBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBCluster' {} a -> s {backupRetentionPeriod = a} :: DBCluster)

-- | The current capacity of an Aurora Serverless v1 DB cluster. The capacity
-- is 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless v1, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
dbCluster_capacity :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_capacity = Lens.lens (\DBCluster' {capacity} -> capacity) (\s@DBCluster' {} a -> s {capacity = a} :: DBCluster)

-- | If present, specifies the name of the character set that this cluster is
-- associated with.
dbCluster_characterSetName :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_characterSetName = Lens.lens (\DBCluster' {characterSetName} -> characterSetName) (\s@DBCluster' {} a -> s {characterSetName = a} :: DBCluster)

-- | Identifies the clone group to which the DB cluster is associated.
dbCluster_cloneGroupId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_cloneGroupId = Lens.lens (\DBCluster' {cloneGroupId} -> cloneGroupId) (\s@DBCluster' {} a -> s {cloneGroupId = a} :: DBCluster)

-- | Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
dbCluster_clusterCreateTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_clusterCreateTime = Lens.lens (\DBCluster' {clusterCreateTime} -> clusterCreateTime) (\s@DBCluster' {} a -> s {clusterCreateTime = a} :: DBCluster) Prelude.. Lens.mapping Data._Time

-- | Specifies whether tags are copied from the DB cluster to snapshots of
-- the DB cluster.
dbCluster_copyTagsToSnapshot :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_copyTagsToSnapshot = Lens.lens (\DBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@DBCluster' {} a -> s {copyTagsToSnapshot = a} :: DBCluster)

-- | Specifies whether the DB cluster is a clone of a DB cluster owned by a
-- different Amazon Web Services account.
dbCluster_crossAccountClone :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_crossAccountClone = Lens.lens (\DBCluster' {crossAccountClone} -> crossAccountClone) (\s@DBCluster' {} a -> s {crossAccountClone = a} :: DBCluster)

-- | Identifies all custom endpoints associated with the cluster.
dbCluster_customEndpoints :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_customEndpoints = Lens.lens (\DBCluster' {customEndpoints} -> customEndpoints) (\s@DBCluster' {} a -> s {customEndpoints = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the DB cluster.
dbCluster_dbClusterArn :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterArn = Lens.lens (\DBCluster' {dbClusterArn} -> dbClusterArn) (\s@DBCluster' {} a -> s {dbClusterArn = a} :: DBCluster)

-- | Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
dbCluster_dbClusterIdentifier :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterIdentifier = Lens.lens (\DBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBCluster' {} a -> s {dbClusterIdentifier = a} :: DBCluster)

-- | The name of the compute and memory capacity class of the DB instance.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_dbClusterInstanceClass :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterInstanceClass = Lens.lens (\DBCluster' {dbClusterInstanceClass} -> dbClusterInstanceClass) (\s@DBCluster' {} a -> s {dbClusterInstanceClass = a} :: DBCluster)

-- | Provides the list of instances that make up the DB cluster.
dbCluster_dbClusterMembers :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterMember])
dbCluster_dbClusterMembers = Lens.lens (\DBCluster' {dbClusterMembers} -> dbClusterMembers) (\s@DBCluster' {} a -> s {dbClusterMembers = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Provides the list of option group memberships for this DB cluster.
dbCluster_dbClusterOptionGroupMemberships :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterOptionGroupStatus])
dbCluster_dbClusterOptionGroupMemberships = Lens.lens (\DBCluster' {dbClusterOptionGroupMemberships} -> dbClusterOptionGroupMemberships) (\s@DBCluster' {} a -> s {dbClusterOptionGroupMemberships = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the DB cluster parameter group for the DB cluster.
dbCluster_dbClusterParameterGroup :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterParameterGroup = Lens.lens (\DBCluster' {dbClusterParameterGroup} -> dbClusterParameterGroup) (\s@DBCluster' {} a -> s {dbClusterParameterGroup = a} :: DBCluster)

-- | Specifies information on the subnet group associated with the DB
-- cluster, including the name, description, and subnets in the subnet
-- group.
dbCluster_dbSubnetGroup :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbSubnetGroup = Lens.lens (\DBCluster' {dbSubnetGroup} -> dbSubnetGroup) (\s@DBCluster' {} a -> s {dbSubnetGroup = a} :: DBCluster)

-- | Reserved for future use.
dbCluster_dbSystemId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbSystemId = Lens.lens (\DBCluster' {dbSystemId} -> dbSystemId) (\s@DBCluster' {} a -> s {dbSystemId = a} :: DBCluster)

-- | Contains the name of the initial database of this DB cluster that was
-- provided at create time, if one was specified when the DB cluster was
-- created. This same name is returned for the life of the DB cluster.
dbCluster_databaseName :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_databaseName = Lens.lens (\DBCluster' {databaseName} -> databaseName) (\s@DBCluster' {} a -> s {databaseName = a} :: DBCluster)

-- | The Amazon Web Services Region-unique, immutable identifier for the DB
-- cluster. This identifier is found in Amazon Web Services CloudTrail log
-- entries whenever the KMS key for the DB cluster is accessed.
dbCluster_dbClusterResourceId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterResourceId = Lens.lens (\DBCluster' {dbClusterResourceId} -> dbClusterResourceId) (\s@DBCluster' {} a -> s {dbClusterResourceId = a} :: DBCluster)

-- | Indicates if the DB cluster has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled.
dbCluster_deletionProtection :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_deletionProtection = Lens.lens (\DBCluster' {deletionProtection} -> deletionProtection) (\s@DBCluster' {} a -> s {deletionProtection = a} :: DBCluster)

-- | The Active Directory Domain membership records associated with the DB
-- cluster.
dbCluster_domainMemberships :: Lens.Lens' DBCluster (Prelude.Maybe [DomainMembership])
dbCluster_domainMemberships = Lens.lens (\DBCluster' {domainMemberships} -> domainMemberships) (\s@DBCluster' {} a -> s {domainMemberships = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The earliest time to which a DB cluster can be backtracked.
dbCluster_earliestBacktrackTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_earliestBacktrackTime = Lens.lens (\DBCluster' {earliestBacktrackTime} -> earliestBacktrackTime) (\s@DBCluster' {} a -> s {earliestBacktrackTime = a} :: DBCluster) Prelude.. Lens.mapping Data._Time

-- | The earliest time to which a database can be restored with point-in-time
-- restore.
dbCluster_earliestRestorableTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_earliestRestorableTime = Lens.lens (\DBCluster' {earliestRestorableTime} -> earliestRestorableTime) (\s@DBCluster' {} a -> s {earliestRestorableTime = a} :: DBCluster) Prelude.. Lens.mapping Data._Time

-- | A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon Aurora User Guide./
dbCluster_enabledCloudwatchLogsExports :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_enabledCloudwatchLogsExports = Lens.lens (\DBCluster' {enabledCloudwatchLogsExports} -> enabledCloudwatchLogsExports) (\s@DBCluster' {} a -> s {enabledCloudwatchLogsExports = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the connection endpoint for the primary instance of the DB
-- cluster.
dbCluster_endpoint :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_endpoint = Lens.lens (\DBCluster' {endpoint} -> endpoint) (\s@DBCluster' {} a -> s {endpoint = a} :: DBCluster)

-- | The name of the database engine to be used for this DB cluster.
dbCluster_engine :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engine = Lens.lens (\DBCluster' {engine} -> engine) (\s@DBCluster' {} a -> s {engine = a} :: DBCluster)

-- | The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
dbCluster_engineMode :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engineMode = Lens.lens (\DBCluster' {engineMode} -> engineMode) (\s@DBCluster' {} a -> s {engineMode = a} :: DBCluster)

-- | Indicates the database engine version.
dbCluster_engineVersion :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engineVersion = Lens.lens (\DBCluster' {engineVersion} -> engineVersion) (\s@DBCluster' {} a -> s {engineVersion = a} :: DBCluster)

-- | Specifies whether you have requested to enable write forwarding for a
-- secondary cluster in an Aurora global database. Because write forwarding
-- takes time to enable, check the value of @GlobalWriteForwardingStatus@
-- to confirm that the request has completed before using the write
-- forwarding feature for this cluster.
dbCluster_globalWriteForwardingRequested :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_globalWriteForwardingRequested = Lens.lens (\DBCluster' {globalWriteForwardingRequested} -> globalWriteForwardingRequested) (\s@DBCluster' {} a -> s {globalWriteForwardingRequested = a} :: DBCluster)

-- | Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
dbCluster_globalWriteForwardingStatus :: Lens.Lens' DBCluster (Prelude.Maybe WriteForwardingStatus)
dbCluster_globalWriteForwardingStatus = Lens.lens (\DBCluster' {globalWriteForwardingStatus} -> globalWriteForwardingStatus) (\s@DBCluster' {} a -> s {globalWriteForwardingStatus = a} :: DBCluster)

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
dbCluster_hostedZoneId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_hostedZoneId = Lens.lens (\DBCluster' {hostedZoneId} -> hostedZoneId) (\s@DBCluster' {} a -> s {hostedZoneId = a} :: DBCluster)

-- | A value that indicates whether the HTTP endpoint for an Aurora
-- Serverless v1 DB cluster is enabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service
-- API for running SQL queries on the Aurora Serverless v1 DB cluster. You
-- can also query your database from inside the RDS console with the query
-- editor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
dbCluster_httpEndpointEnabled :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_httpEndpointEnabled = Lens.lens (\DBCluster' {httpEndpointEnabled} -> httpEndpointEnabled) (\s@DBCluster' {} a -> s {httpEndpointEnabled = a} :: DBCluster)

-- | A value that indicates whether the mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts is
-- enabled.
dbCluster_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBCluster' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBCluster' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBCluster)

-- | The Provisioned IOPS (I\/O operations per second) value.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_iops :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_iops = Lens.lens (\DBCluster' {iops} -> iops) (\s@DBCluster' {} a -> s {iops = a} :: DBCluster)

-- | If @StorageEncrypted@ is enabled, the Amazon Web Services KMS key
-- identifier for the encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
dbCluster_kmsKeyId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_kmsKeyId = Lens.lens (\DBCluster' {kmsKeyId} -> kmsKeyId) (\s@DBCluster' {} a -> s {kmsKeyId = a} :: DBCluster)

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbCluster_latestRestorableTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_latestRestorableTime = Lens.lens (\DBCluster' {latestRestorableTime} -> latestRestorableTime) (\s@DBCluster' {} a -> s {latestRestorableTime = a} :: DBCluster) Prelude.. Lens.mapping Data._Time

-- | Contains the secret managed by RDS in Amazon Web Services Secrets
-- Manager for the master user password.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
dbCluster_masterUserSecret :: Lens.Lens' DBCluster (Prelude.Maybe MasterUserSecret)
dbCluster_masterUserSecret = Lens.lens (\DBCluster' {masterUserSecret} -> masterUserSecret) (\s@DBCluster' {} a -> s {masterUserSecret = a} :: DBCluster)

-- | Contains the master username for the DB cluster.
dbCluster_masterUsername :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_masterUsername = Lens.lens (\DBCluster' {masterUsername} -> masterUsername) (\s@DBCluster' {} a -> s {masterUsername = a} :: DBCluster)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB cluster.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_monitoringInterval :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_monitoringInterval = Lens.lens (\DBCluster' {monitoringInterval} -> monitoringInterval) (\s@DBCluster' {} a -> s {monitoringInterval = a} :: DBCluster)

-- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring
-- metrics to Amazon CloudWatch Logs.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_monitoringRoleArn :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_monitoringRoleArn = Lens.lens (\DBCluster' {monitoringRoleArn} -> monitoringRoleArn) (\s@DBCluster' {} a -> s {monitoringRoleArn = a} :: DBCluster)

-- | Specifies whether the DB cluster has instances in multiple Availability
-- Zones.
dbCluster_multiAZ :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_multiAZ = Lens.lens (\DBCluster' {multiAZ} -> multiAZ) (\s@DBCluster' {} a -> s {multiAZ = a} :: DBCluster)

-- | The network type of the DB instance.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon Aurora User Guide./
--
-- This setting is only for Aurora DB clusters.
dbCluster_networkType :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_networkType = Lens.lens (\DBCluster' {networkType} -> networkType) (\s@DBCluster' {} a -> s {networkType = a} :: DBCluster)

-- | A value that specifies that changes to the DB cluster are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
dbCluster_pendingModifiedValues :: Lens.Lens' DBCluster (Prelude.Maybe ClusterPendingModifiedValues)
dbCluster_pendingModifiedValues = Lens.lens (\DBCluster' {pendingModifiedValues} -> pendingModifiedValues) (\s@DBCluster' {} a -> s {pendingModifiedValues = a} :: DBCluster)

-- | Specifies the progress of the operation as a percentage.
dbCluster_percentProgress :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_percentProgress = Lens.lens (\DBCluster' {percentProgress} -> percentProgress) (\s@DBCluster' {} a -> s {percentProgress = a} :: DBCluster)

-- | True if Performance Insights is enabled for the DB cluster, and
-- otherwise false.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_performanceInsightsEnabled :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_performanceInsightsEnabled = Lens.lens (\DBCluster' {performanceInsightsEnabled} -> performanceInsightsEnabled) (\s@DBCluster' {} a -> s {performanceInsightsEnabled = a} :: DBCluster)

-- | The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_performanceInsightsKMSKeyId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_performanceInsightsKMSKeyId = Lens.lens (\DBCluster' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@DBCluster' {} a -> s {performanceInsightsKMSKeyId = a} :: DBCluster)

-- | The number of days to retain Performance Insights data. The default is 7
-- days. The following values are valid:
--
-- -   7
--
-- -   /month/ * 31, where /month/ is a number of months from 1-23
--
-- -   731
--
-- For example, the following values are valid:
--
-- -   93 (3 months * 31)
--
-- -   341 (11 months * 31)
--
-- -   589 (19 months * 31)
--
-- -   731
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_performanceInsightsRetentionPeriod :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_performanceInsightsRetentionPeriod = Lens.lens (\DBCluster' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@DBCluster' {} a -> s {performanceInsightsRetentionPeriod = a} :: DBCluster)

-- | Specifies the port that the database engine is listening on.
dbCluster_port :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_port = Lens.lens (\DBCluster' {port} -> port) (\s@DBCluster' {} a -> s {port = a} :: DBCluster)

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
dbCluster_preferredBackupWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_preferredBackupWindow = Lens.lens (\DBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@DBCluster' {} a -> s {preferredBackupWindow = a} :: DBCluster)

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
dbCluster_preferredMaintenanceWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_preferredMaintenanceWindow = Lens.lens (\DBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@DBCluster' {} a -> s {preferredMaintenanceWindow = a} :: DBCluster)

-- | Specifies the accessibility options for the DB instance.
--
-- When the DB instance is publicly accessible, its Domain Name System
-- (DNS) endpoint resolves to the private IP address from within the DB
-- instance\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB instance\'s VPC. Access to the DB
-- instance is ultimately controlled by the security group it uses. That
-- public access is not permitted if the security group assigned to the DB
-- instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_publiclyAccessible :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_publiclyAccessible = Lens.lens (\DBCluster' {publiclyAccessible} -> publiclyAccessible) (\s@DBCluster' {} a -> s {publiclyAccessible = a} :: DBCluster)

-- | Contains one or more identifiers of the read replicas associated with
-- this DB cluster.
dbCluster_readReplicaIdentifiers :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_readReplicaIdentifiers = Lens.lens (\DBCluster' {readReplicaIdentifiers} -> readReplicaIdentifiers) (\s@DBCluster' {} a -> s {readReplicaIdentifiers = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

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

-- | Contains the identifier of the source DB cluster if this DB cluster is a
-- read replica.
dbCluster_replicationSourceIdentifier :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_replicationSourceIdentifier = Lens.lens (\DBCluster' {replicationSourceIdentifier} -> replicationSourceIdentifier) (\s@DBCluster' {} a -> s {replicationSourceIdentifier = a} :: DBCluster)

-- | Undocumented member.
dbCluster_scalingConfigurationInfo :: Lens.Lens' DBCluster (Prelude.Maybe ScalingConfigurationInfo)
dbCluster_scalingConfigurationInfo = Lens.lens (\DBCluster' {scalingConfigurationInfo} -> scalingConfigurationInfo) (\s@DBCluster' {} a -> s {scalingConfigurationInfo = a} :: DBCluster)

-- | Undocumented member.
dbCluster_serverlessV2ScalingConfiguration :: Lens.Lens' DBCluster (Prelude.Maybe ServerlessV2ScalingConfigurationInfo)
dbCluster_serverlessV2ScalingConfiguration = Lens.lens (\DBCluster' {serverlessV2ScalingConfiguration} -> serverlessV2ScalingConfiguration) (\s@DBCluster' {} a -> s {serverlessV2ScalingConfiguration = a} :: DBCluster)

-- | Specifies the current state of this DB cluster.
dbCluster_status :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_status = Lens.lens (\DBCluster' {status} -> status) (\s@DBCluster' {} a -> s {status = a} :: DBCluster)

-- | Specifies whether the DB cluster is encrypted.
dbCluster_storageEncrypted :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_storageEncrypted = Lens.lens (\DBCluster' {storageEncrypted} -> storageEncrypted) (\s@DBCluster' {} a -> s {storageEncrypted = a} :: DBCluster)

-- | The storage type associated with the DB cluster.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_storageType :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_storageType = Lens.lens (\DBCluster' {storageType} -> storageType) (\s@DBCluster' {} a -> s {storageType = a} :: DBCluster)

-- | Undocumented member.
dbCluster_tagList :: Lens.Lens' DBCluster (Prelude.Maybe [Tag])
dbCluster_tagList = Lens.lens (\DBCluster' {tagList} -> tagList) (\s@DBCluster' {} a -> s {tagList = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Provides a list of VPC security groups that the DB cluster belongs to.
dbCluster_vpcSecurityGroups :: Lens.Lens' DBCluster (Prelude.Maybe [VpcSecurityGroupMembership])
dbCluster_vpcSecurityGroups = Lens.lens (\DBCluster' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@DBCluster' {} a -> s {vpcSecurityGroups = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML DBCluster where
  parseXML x =
    DBCluster'
      Prelude.<$> (x Data..@? "ActivityStreamKinesisStreamName")
      Prelude.<*> (x Data..@? "ActivityStreamKmsKeyId")
      Prelude.<*> (x Data..@? "ActivityStreamMode")
      Prelude.<*> (x Data..@? "ActivityStreamStatus")
      Prelude.<*> (x Data..@? "AllocatedStorage")
      Prelude.<*> ( x Data..@? "AssociatedRoles" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBClusterRole")
                  )
      Prelude.<*> (x Data..@? "AutoMinorVersionUpgrade")
      Prelude.<*> (x Data..@? "AutomaticRestartTime")
      Prelude.<*> ( x Data..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Data..@? "BacktrackConsumedChangeRecords")
      Prelude.<*> (x Data..@? "BacktrackWindow")
      Prelude.<*> (x Data..@? "BackupRetentionPeriod")
      Prelude.<*> (x Data..@? "Capacity")
      Prelude.<*> (x Data..@? "CharacterSetName")
      Prelude.<*> (x Data..@? "CloneGroupId")
      Prelude.<*> (x Data..@? "ClusterCreateTime")
      Prelude.<*> (x Data..@? "CopyTagsToSnapshot")
      Prelude.<*> (x Data..@? "CrossAccountClone")
      Prelude.<*> ( x Data..@? "CustomEndpoints" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "DBClusterArn")
      Prelude.<*> (x Data..@? "DBClusterIdentifier")
      Prelude.<*> (x Data..@? "DBClusterInstanceClass")
      Prelude.<*> ( x Data..@? "DBClusterMembers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBClusterMember")
                  )
      Prelude.<*> ( x Data..@? "DBClusterOptionGroupMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBClusterOptionGroup")
                  )
      Prelude.<*> (x Data..@? "DBClusterParameterGroup")
      Prelude.<*> (x Data..@? "DBSubnetGroup")
      Prelude.<*> (x Data..@? "DBSystemId")
      Prelude.<*> (x Data..@? "DatabaseName")
      Prelude.<*> (x Data..@? "DbClusterResourceId")
      Prelude.<*> (x Data..@? "DeletionProtection")
      Prelude.<*> ( x Data..@? "DomainMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DomainMembership")
                  )
      Prelude.<*> (x Data..@? "EarliestBacktrackTime")
      Prelude.<*> (x Data..@? "EarliestRestorableTime")
      Prelude.<*> ( x Data..@? "EnabledCloudwatchLogsExports"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Endpoint")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineMode")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "GlobalWriteForwardingRequested")
      Prelude.<*> (x Data..@? "GlobalWriteForwardingStatus")
      Prelude.<*> (x Data..@? "HostedZoneId")
      Prelude.<*> (x Data..@? "HttpEndpointEnabled")
      Prelude.<*> (x Data..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Data..@? "Iops")
      Prelude.<*> (x Data..@? "KmsKeyId")
      Prelude.<*> (x Data..@? "LatestRestorableTime")
      Prelude.<*> (x Data..@? "MasterUserSecret")
      Prelude.<*> (x Data..@? "MasterUsername")
      Prelude.<*> (x Data..@? "MonitoringInterval")
      Prelude.<*> (x Data..@? "MonitoringRoleArn")
      Prelude.<*> (x Data..@? "MultiAZ")
      Prelude.<*> (x Data..@? "NetworkType")
      Prelude.<*> (x Data..@? "PendingModifiedValues")
      Prelude.<*> (x Data..@? "PercentProgress")
      Prelude.<*> (x Data..@? "PerformanceInsightsEnabled")
      Prelude.<*> (x Data..@? "PerformanceInsightsKMSKeyId")
      Prelude.<*> (x Data..@? "PerformanceInsightsRetentionPeriod")
      Prelude.<*> (x Data..@? "Port")
      Prelude.<*> (x Data..@? "PreferredBackupWindow")
      Prelude.<*> (x Data..@? "PreferredMaintenanceWindow")
      Prelude.<*> (x Data..@? "PubliclyAccessible")
      Prelude.<*> ( x Data..@? "ReadReplicaIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "ReadReplicaIdentifier")
                  )
      Prelude.<*> (x Data..@? "ReaderEndpoint")
      Prelude.<*> (x Data..@? "ReplicationSourceIdentifier")
      Prelude.<*> (x Data..@? "ScalingConfigurationInfo")
      Prelude.<*> (x Data..@? "ServerlessV2ScalingConfiguration")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "StorageEncrypted")
      Prelude.<*> (x Data..@? "StorageType")
      Prelude.<*> ( x Data..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )
      Prelude.<*> ( x Data..@? "VpcSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "VpcSecurityGroupMembership")
                  )

instance Prelude.Hashable DBCluster where
  hashWithSalt _salt DBCluster' {..} =
    _salt
      `Prelude.hashWithSalt` activityStreamKinesisStreamName
      `Prelude.hashWithSalt` activityStreamKmsKeyId
      `Prelude.hashWithSalt` activityStreamMode
      `Prelude.hashWithSalt` activityStreamStatus
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` associatedRoles
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` automaticRestartTime
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` backtrackConsumedChangeRecords
      `Prelude.hashWithSalt` backtrackWindow
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` cloneGroupId
      `Prelude.hashWithSalt` clusterCreateTime
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` crossAccountClone
      `Prelude.hashWithSalt` customEndpoints
      `Prelude.hashWithSalt` dbClusterArn
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` dbClusterInstanceClass
      `Prelude.hashWithSalt` dbClusterMembers
      `Prelude.hashWithSalt` dbClusterOptionGroupMemberships
      `Prelude.hashWithSalt` dbClusterParameterGroup
      `Prelude.hashWithSalt` dbSubnetGroup
      `Prelude.hashWithSalt` dbSystemId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` dbClusterResourceId
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domainMemberships
      `Prelude.hashWithSalt` earliestBacktrackTime
      `Prelude.hashWithSalt` earliestRestorableTime
      `Prelude.hashWithSalt` enabledCloudwatchLogsExports
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineMode
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` globalWriteForwardingRequested
      `Prelude.hashWithSalt` globalWriteForwardingStatus
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` httpEndpointEnabled
      `Prelude.hashWithSalt` iAMDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` latestRestorableTime
      `Prelude.hashWithSalt` masterUserSecret
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` percentProgress
      `Prelude.hashWithSalt` performanceInsightsEnabled
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` performanceInsightsRetentionPeriod
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` readReplicaIdentifiers
      `Prelude.hashWithSalt` readerEndpoint
      `Prelude.hashWithSalt` replicationSourceIdentifier
      `Prelude.hashWithSalt` scalingConfigurationInfo
      `Prelude.hashWithSalt` serverlessV2ScalingConfiguration
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` vpcSecurityGroups

instance Prelude.NFData DBCluster where
  rnf DBCluster' {..} =
    Prelude.rnf activityStreamKinesisStreamName
      `Prelude.seq` Prelude.rnf activityStreamKmsKeyId
      `Prelude.seq` Prelude.rnf activityStreamMode
      `Prelude.seq` Prelude.rnf activityStreamStatus
      `Prelude.seq` Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf associatedRoles
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf automaticRestartTime
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf backtrackConsumedChangeRecords
      `Prelude.seq` Prelude.rnf backtrackWindow
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf cloneGroupId
      `Prelude.seq` Prelude.rnf clusterCreateTime
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf crossAccountClone
      `Prelude.seq` Prelude.rnf customEndpoints
      `Prelude.seq` Prelude.rnf dbClusterArn
      `Prelude.seq` Prelude.rnf
        dbClusterIdentifier
      `Prelude.seq` Prelude.rnf
        dbClusterInstanceClass
      `Prelude.seq` Prelude.rnf
        dbClusterMembers
      `Prelude.seq` Prelude.rnf
        dbClusterOptionGroupMemberships
      `Prelude.seq` Prelude.rnf
        dbClusterParameterGroup
      `Prelude.seq` Prelude.rnf
        dbSubnetGroup
      `Prelude.seq` Prelude.rnf
        dbSystemId
      `Prelude.seq` Prelude.rnf
        databaseName
      `Prelude.seq` Prelude.rnf
        dbClusterResourceId
      `Prelude.seq` Prelude.rnf
        deletionProtection
      `Prelude.seq` Prelude.rnf
        domainMemberships
      `Prelude.seq` Prelude.rnf
        earliestBacktrackTime
      `Prelude.seq` Prelude.rnf
        earliestRestorableTime
      `Prelude.seq` Prelude.rnf
        enabledCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf
        endpoint
      `Prelude.seq` Prelude.rnf
        engine
      `Prelude.seq` Prelude.rnf
        engineMode
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        globalWriteForwardingRequested
      `Prelude.seq` Prelude.rnf
        globalWriteForwardingStatus
      `Prelude.seq` Prelude.rnf
        hostedZoneId
      `Prelude.seq` Prelude.rnf
        httpEndpointEnabled
      `Prelude.seq` Prelude.rnf
        iAMDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf
        iops
      `Prelude.seq` Prelude.rnf
        kmsKeyId
      `Prelude.seq` Prelude.rnf
        latestRestorableTime
      `Prelude.seq` Prelude.rnf
        masterUserSecret
      `Prelude.seq` Prelude.rnf
        masterUsername
      `Prelude.seq` Prelude.rnf
        monitoringInterval
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf
        multiAZ
      `Prelude.seq` Prelude.rnf
        networkType
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
      `Prelude.seq` Prelude.rnf
        percentProgress
      `Prelude.seq` Prelude.rnf
        performanceInsightsEnabled
      `Prelude.seq` Prelude.rnf
        performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        performanceInsightsRetentionPeriod
      `Prelude.seq` Prelude.rnf
        port
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        readReplicaIdentifiers
      `Prelude.seq` Prelude.rnf
        readerEndpoint
      `Prelude.seq` Prelude.rnf
        replicationSourceIdentifier
      `Prelude.seq` Prelude.rnf
        scalingConfigurationInfo
      `Prelude.seq` Prelude.rnf
        serverlessV2ScalingConfiguration
      `Prelude.seq` Prelude.rnf
        status
      `Prelude.seq` Prelude.rnf
        storageEncrypted
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tagList
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroups
