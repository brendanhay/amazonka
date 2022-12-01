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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBCluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.ActivityStreamMode
import Amazonka.RDS.Types.ActivityStreamStatus
import Amazonka.RDS.Types.ClusterPendingModifiedValues
import Amazonka.RDS.Types.DBClusterMember
import Amazonka.RDS.Types.DBClusterOptionGroupStatus
import Amazonka.RDS.Types.DBClusterRole
import Amazonka.RDS.Types.DomainMembership
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
  { -- | Specifies the port that the database engine is listening on.
    port :: Prelude.Maybe Prelude.Int,
    serverlessV2ScalingConfiguration :: Prelude.Maybe ServerlessV2ScalingConfigurationInfo,
    -- | Identifies the clone group to which the DB cluster is associated.
    cloneGroupId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the DB cluster.
    dbClusterArn :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
    -- zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the progress of the operation as a percentage.
    percentProgress :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether you have requested to enable write forwarding for a
    -- secondary cluster in an Aurora global database. Because write forwarding
    -- takes time to enable, check the value of @GlobalWriteForwardingStatus@
    -- to confirm that the request has completed before using the write
    -- forwarding feature for this cluster.
    globalWriteForwardingRequested :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the daily time range during which automated backups are
    -- created if automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of days for which automatic DB snapshots are
    -- retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | If present, specifies the name of the character set that this cluster is
    -- associated with.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | Contains the master username for the DB cluster.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether tags are copied from the DB cluster to snapshots of
    -- the DB cluster.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The status of the database activity stream.
    activityStreamStatus :: Prelude.Maybe ActivityStreamStatus,
    -- | Provides the list of instances that make up the DB cluster.
    dbClusterMembers :: Prelude.Maybe [DBClusterMember],
    -- | Specifies the name of the DB cluster parameter group for the DB cluster.
    dbClusterParameterGroup :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates that minor version patches are applied
    -- automatically.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The mode of the database activity stream. Database events such as a
    -- change or access generate an activity stream event. The database session
    -- can handle these events either synchronously or asynchronously.
    activityStreamMode :: Prelude.Maybe ActivityStreamMode,
    tagList :: Prelude.Maybe [Tag],
    -- | Specifies the latest time to which a database can be restored with
    -- point-in-time restore.
    latestRestorableTime :: Prelude.Maybe Core.ISO8601,
    -- | The name of the compute and memory capacity class of the DB instance.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    dbClusterInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Contains the name of the initial database of this DB cluster that was
    -- provided at create time, if one was specified when the DB cluster was
    -- created. This same name is returned for the life of the DB cluster.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Contains a user-supplied DB cluster identifier. This identifier is the
    -- unique key that identifies a DB cluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Active Directory Domain membership records associated with the DB
    -- cluster.
    domainMemberships :: Prelude.Maybe [DomainMembership],
    scalingConfigurationInfo :: Prelude.Maybe ScalingConfigurationInfo,
    -- | Provides the list of Availability Zones (AZs) where instances in the DB
    -- cluster can be created.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The earliest time to which a DB cluster can be backtracked.
    earliestBacktrackTime :: Prelude.Maybe Core.ISO8601,
    -- | The time when a stopped DB cluster is restarted automatically.
    automaticRestartTime :: Prelude.Maybe Core.ISO8601,
    -- | The Amazon Web Services KMS key identifier for encryption of Performance
    -- Insights data.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the DB cluster is a clone of a DB cluster owned by a
    -- different Amazon Web Services account.
    crossAccountClone :: Prelude.Maybe Prelude.Bool,
    -- | Provides the list of option group memberships for this DB cluster.
    dbClusterOptionGroupMemberships :: Prelude.Maybe [DBClusterOptionGroupStatus],
    -- | Specifies information on the subnet group associated with the DB
    -- cluster, including the name, description, and subnets in the subnet
    -- group.
    dbSubnetGroup :: Prelude.Maybe Prelude.Text,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB cluster.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | True if Performance Insights is enabled for the DB cluster, and
    -- otherwise false.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    performanceInsightsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the current state of this DB cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Kinesis data stream used for the database
    -- activity stream.
    activityStreamKinesisStreamName :: Prelude.Maybe Prelude.Text,
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
    -- | The storage type associated with the DB cluster.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The target backtrack window, in seconds. If this value is set to 0,
    -- backtracking is disabled for the DB cluster. Otherwise, backtracking is
    -- enabled.
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
    -- | Identifies all custom endpoints associated with the cluster.
    customEndpoints :: Prelude.Maybe [Prelude.Text],
    -- | Contains the identifier of the source DB cluster if this DB cluster is a
    -- read replica.
    replicationSourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring
    -- metrics to Amazon CloudWatch Logs.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The number of change records stored for Backtrack.
    backtrackConsumedChangeRecords :: Prelude.Maybe Prelude.Integer,
    -- | The DB engine mode of the DB cluster, either @provisioned@,
    -- @serverless@, @parallelquery@, @global@, or @multimaster@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | If @StorageEncrypted@ is enabled, the Amazon Web Services KMS key
    -- identifier for the encrypted DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine to be used for this DB cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | For all database engines except Amazon Aurora, @AllocatedStorage@
    -- specifies the allocated storage size in gibibytes (GiB). For Aurora,
    -- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
    -- size isn\'t fixed, but instead automatically adjusts as needed.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
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
    -- | The earliest time to which a database can be restored with point-in-time
    -- restore.
    earliestRestorableTime :: Prelude.Maybe Core.ISO8601,
    -- | A value that indicates whether the mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts is
    -- enabled.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates if the DB cluster has deletion protection enabled. The
    -- database can\'t be deleted when deletion protection is enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that specifies that changes to the DB cluster are pending. This
    -- element is only included when changes are pending. Specific changes are
    -- identified by subelements.
    pendingModifiedValues :: Prelude.Maybe ClusterPendingModifiedValues,
    -- | Specifies the weekly time range during which system maintenance can
    -- occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the connection endpoint for the primary instance of the DB
    -- cluster.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The current capacity of an Aurora Serverless v1 DB cluster. The capacity
    -- is 0 (zero) when the cluster is paused.
    --
    -- For more information about Aurora Serverless v1, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless v1>
    -- in the /Amazon Aurora User Guide/.
    capacity :: Prelude.Maybe Prelude.Int,
    -- | Specifies the time when the DB cluster was created, in Universal
    -- Coordinated Time (UTC).
    clusterCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | Contains one or more identifiers of the read replicas associated with
    -- this DB cluster.
    readReplicaIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Reserved for future use.
    dbSystemId :: Prelude.Maybe Prelude.Text,
    -- | A list of log types that this DB cluster is configured to export to
    -- CloudWatch Logs.
    --
    -- Log types vary by DB engine. For information about the log types for
    -- each DB engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
    -- in the /Amazon Aurora User Guide./
    enabledCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The Provisioned IOPS (I\/O operations per second) value.
    --
    -- This setting is only for non-Aurora Multi-AZ DB clusters.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services Region-unique, immutable identifier for the DB
    -- cluster. This identifier is found in Amazon Web Services CloudTrail log
    -- entries whenever the KMS key for the DB cluster is accessed.
    dbClusterResourceId :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of the Amazon Web Services Identity and Access
    -- Management (IAM) roles that are associated with the DB cluster. IAM
    -- roles that are associated with a DB cluster grant permission for the DB
    -- cluster to access other Amazon Web Services on your behalf.
    associatedRoles :: Prelude.Maybe [DBClusterRole],
    -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies whether the DB cluster has instances in multiple Availability
    -- Zones.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services KMS key identifier used for encrypting messages
    -- in the database activity stream.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    activityStreamKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a secondary cluster in an Aurora global database has
    -- write forwarding enabled, not enabled, or is in the process of enabling
    -- it.
    globalWriteForwardingStatus :: Prelude.Maybe WriteForwardingStatus,
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
-- 'port', 'dbCluster_port' - Specifies the port that the database engine is listening on.
--
-- 'serverlessV2ScalingConfiguration', 'dbCluster_serverlessV2ScalingConfiguration' - Undocumented member.
--
-- 'cloneGroupId', 'dbCluster_cloneGroupId' - Identifies the clone group to which the DB cluster is associated.
--
-- 'dbClusterArn', 'dbCluster_dbClusterArn' - The Amazon Resource Name (ARN) for the DB cluster.
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
-- 'hostedZoneId', 'dbCluster_hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
--
-- 'percentProgress', 'dbCluster_percentProgress' - Specifies the progress of the operation as a percentage.
--
-- 'globalWriteForwardingRequested', 'dbCluster_globalWriteForwardingRequested' - Specifies whether you have requested to enable write forwarding for a
-- secondary cluster in an Aurora global database. Because write forwarding
-- takes time to enable, check the value of @GlobalWriteForwardingStatus@
-- to confirm that the request has completed before using the write
-- forwarding feature for this cluster.
--
-- 'preferredBackupWindow', 'dbCluster_preferredBackupWindow' - Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
--
-- 'backupRetentionPeriod', 'dbCluster_backupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are
-- retained.
--
-- 'characterSetName', 'dbCluster_characterSetName' - If present, specifies the name of the character set that this cluster is
-- associated with.
--
-- 'masterUsername', 'dbCluster_masterUsername' - Contains the master username for the DB cluster.
--
-- 'copyTagsToSnapshot', 'dbCluster_copyTagsToSnapshot' - Specifies whether tags are copied from the DB cluster to snapshots of
-- the DB cluster.
--
-- 'activityStreamStatus', 'dbCluster_activityStreamStatus' - The status of the database activity stream.
--
-- 'dbClusterMembers', 'dbCluster_dbClusterMembers' - Provides the list of instances that make up the DB cluster.
--
-- 'dbClusterParameterGroup', 'dbCluster_dbClusterParameterGroup' - Specifies the name of the DB cluster parameter group for the DB cluster.
--
-- 'autoMinorVersionUpgrade', 'dbCluster_autoMinorVersionUpgrade' - A value that indicates that minor version patches are applied
-- automatically.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'activityStreamMode', 'dbCluster_activityStreamMode' - The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. The database session
-- can handle these events either synchronously or asynchronously.
--
-- 'tagList', 'dbCluster_tagList' - Undocumented member.
--
-- 'latestRestorableTime', 'dbCluster_latestRestorableTime' - Specifies the latest time to which a database can be restored with
-- point-in-time restore.
--
-- 'dbClusterInstanceClass', 'dbCluster_dbClusterInstanceClass' - The name of the compute and memory capacity class of the DB instance.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'databaseName', 'dbCluster_databaseName' - Contains the name of the initial database of this DB cluster that was
-- provided at create time, if one was specified when the DB cluster was
-- created. This same name is returned for the life of the DB cluster.
--
-- 'dbClusterIdentifier', 'dbCluster_dbClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
--
-- 'domainMemberships', 'dbCluster_domainMemberships' - The Active Directory Domain membership records associated with the DB
-- cluster.
--
-- 'scalingConfigurationInfo', 'dbCluster_scalingConfigurationInfo' - Undocumented member.
--
-- 'availabilityZones', 'dbCluster_availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster can be created.
--
-- 'earliestBacktrackTime', 'dbCluster_earliestBacktrackTime' - The earliest time to which a DB cluster can be backtracked.
--
-- 'automaticRestartTime', 'dbCluster_automaticRestartTime' - The time when a stopped DB cluster is restarted automatically.
--
-- 'performanceInsightsKMSKeyId', 'dbCluster_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'crossAccountClone', 'dbCluster_crossAccountClone' - Specifies whether the DB cluster is a clone of a DB cluster owned by a
-- different Amazon Web Services account.
--
-- 'dbClusterOptionGroupMemberships', 'dbCluster_dbClusterOptionGroupMemberships' - Provides the list of option group memberships for this DB cluster.
--
-- 'dbSubnetGroup', 'dbCluster_dbSubnetGroup' - Specifies information on the subnet group associated with the DB
-- cluster, including the name, description, and subnets in the subnet
-- group.
--
-- 'monitoringInterval', 'dbCluster_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB cluster.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'performanceInsightsEnabled', 'dbCluster_performanceInsightsEnabled' - True if Performance Insights is enabled for the DB cluster, and
-- otherwise false.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'status', 'dbCluster_status' - Specifies the current state of this DB cluster.
--
-- 'activityStreamKinesisStreamName', 'dbCluster_activityStreamKinesisStreamName' - The name of the Amazon Kinesis data stream used for the database
-- activity stream.
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
-- 'storageType', 'dbCluster_storageType' - The storage type associated with the DB cluster.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'backtrackWindow', 'dbCluster_backtrackWindow' - The target backtrack window, in seconds. If this value is set to 0,
-- backtracking is disabled for the DB cluster. Otherwise, backtracking is
-- enabled.
--
-- 'customEndpoints', 'dbCluster_customEndpoints' - Identifies all custom endpoints associated with the cluster.
--
-- 'replicationSourceIdentifier', 'dbCluster_replicationSourceIdentifier' - Contains the identifier of the source DB cluster if this DB cluster is a
-- read replica.
--
-- 'monitoringRoleArn', 'dbCluster_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send Enhanced Monitoring
-- metrics to Amazon CloudWatch Logs.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'backtrackConsumedChangeRecords', 'dbCluster_backtrackConsumedChangeRecords' - The number of change records stored for Backtrack.
--
-- 'engineMode', 'dbCluster_engineMode' - The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
--
-- 'storageEncrypted', 'dbCluster_storageEncrypted' - Specifies whether the DB cluster is encrypted.
--
-- 'kmsKeyId', 'dbCluster_kmsKeyId' - If @StorageEncrypted@ is enabled, the Amazon Web Services KMS key
-- identifier for the encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- 'engine', 'dbCluster_engine' - The name of the database engine to be used for this DB cluster.
--
-- 'allocatedStorage', 'dbCluster_allocatedStorage' - For all database engines except Amazon Aurora, @AllocatedStorage@
-- specifies the allocated storage size in gibibytes (GiB). For Aurora,
-- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
-- size isn\'t fixed, but instead automatically adjusts as needed.
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
-- 'earliestRestorableTime', 'dbCluster_earliestRestorableTime' - The earliest time to which a database can be restored with point-in-time
-- restore.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbCluster_iAMDatabaseAuthenticationEnabled' - A value that indicates whether the mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts is
-- enabled.
--
-- 'deletionProtection', 'dbCluster_deletionProtection' - Indicates if the DB cluster has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled.
--
-- 'pendingModifiedValues', 'dbCluster_pendingModifiedValues' - A value that specifies that changes to the DB cluster are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
--
-- 'preferredMaintenanceWindow', 'dbCluster_preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
--
-- 'endpoint', 'dbCluster_endpoint' - Specifies the connection endpoint for the primary instance of the DB
-- cluster.
--
-- 'capacity', 'dbCluster_capacity' - The current capacity of an Aurora Serverless v1 DB cluster. The capacity
-- is 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless v1, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
--
-- 'clusterCreateTime', 'dbCluster_clusterCreateTime' - Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
--
-- 'readReplicaIdentifiers', 'dbCluster_readReplicaIdentifiers' - Contains one or more identifiers of the read replicas associated with
-- this DB cluster.
--
-- 'dbSystemId', 'dbCluster_dbSystemId' - Reserved for future use.
--
-- 'enabledCloudwatchLogsExports', 'dbCluster_enabledCloudwatchLogsExports' - A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon Aurora User Guide./
--
-- 'iops', 'dbCluster_iops' - The Provisioned IOPS (I\/O operations per second) value.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
--
-- 'dbClusterResourceId', 'dbCluster_dbClusterResourceId' - The Amazon Web Services Region-unique, immutable identifier for the DB
-- cluster. This identifier is found in Amazon Web Services CloudTrail log
-- entries whenever the KMS key for the DB cluster is accessed.
--
-- 'associatedRoles', 'dbCluster_associatedRoles' - Provides a list of the Amazon Web Services Identity and Access
-- Management (IAM) roles that are associated with the DB cluster. IAM
-- roles that are associated with a DB cluster grant permission for the DB
-- cluster to access other Amazon Web Services on your behalf.
--
-- 'engineVersion', 'dbCluster_engineVersion' - Indicates the database engine version.
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
-- 'multiAZ', 'dbCluster_multiAZ' - Specifies whether the DB cluster has instances in multiple Availability
-- Zones.
--
-- 'activityStreamKmsKeyId', 'dbCluster_activityStreamKmsKeyId' - The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- 'globalWriteForwardingStatus', 'dbCluster_globalWriteForwardingStatus' - Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
--
-- 'vpcSecurityGroups', 'dbCluster_vpcSecurityGroups' - Provides a list of VPC security groups that the DB cluster belongs to.
newDBCluster ::
  DBCluster
newDBCluster =
  DBCluster'
    { port = Prelude.Nothing,
      serverlessV2ScalingConfiguration = Prelude.Nothing,
      cloneGroupId = Prelude.Nothing,
      dbClusterArn = Prelude.Nothing,
      performanceInsightsRetentionPeriod = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      globalWriteForwardingRequested = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      characterSetName = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      activityStreamStatus = Prelude.Nothing,
      dbClusterMembers = Prelude.Nothing,
      dbClusterParameterGroup = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      activityStreamMode = Prelude.Nothing,
      tagList = Prelude.Nothing,
      latestRestorableTime = Prelude.Nothing,
      dbClusterInstanceClass = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      domainMemberships = Prelude.Nothing,
      scalingConfigurationInfo = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      earliestBacktrackTime = Prelude.Nothing,
      automaticRestartTime = Prelude.Nothing,
      performanceInsightsKMSKeyId = Prelude.Nothing,
      crossAccountClone = Prelude.Nothing,
      dbClusterOptionGroupMemberships = Prelude.Nothing,
      dbSubnetGroup = Prelude.Nothing,
      monitoringInterval = Prelude.Nothing,
      performanceInsightsEnabled = Prelude.Nothing,
      status = Prelude.Nothing,
      activityStreamKinesisStreamName = Prelude.Nothing,
      httpEndpointEnabled = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      storageType = Prelude.Nothing,
      backtrackWindow = Prelude.Nothing,
      customEndpoints = Prelude.Nothing,
      replicationSourceIdentifier = Prelude.Nothing,
      monitoringRoleArn = Prelude.Nothing,
      backtrackConsumedChangeRecords = Prelude.Nothing,
      engineMode = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      engine = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      readerEndpoint = Prelude.Nothing,
      earliestRestorableTime = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      capacity = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      readReplicaIdentifiers = Prelude.Nothing,
      dbSystemId = Prelude.Nothing,
      enabledCloudwatchLogsExports = Prelude.Nothing,
      iops = Prelude.Nothing,
      dbClusterResourceId = Prelude.Nothing,
      associatedRoles = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      networkType = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      activityStreamKmsKeyId = Prelude.Nothing,
      globalWriteForwardingStatus = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing
    }

-- | Specifies the port that the database engine is listening on.
dbCluster_port :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_port = Lens.lens (\DBCluster' {port} -> port) (\s@DBCluster' {} a -> s {port = a} :: DBCluster)

-- | Undocumented member.
dbCluster_serverlessV2ScalingConfiguration :: Lens.Lens' DBCluster (Prelude.Maybe ServerlessV2ScalingConfigurationInfo)
dbCluster_serverlessV2ScalingConfiguration = Lens.lens (\DBCluster' {serverlessV2ScalingConfiguration} -> serverlessV2ScalingConfiguration) (\s@DBCluster' {} a -> s {serverlessV2ScalingConfiguration = a} :: DBCluster)

-- | Identifies the clone group to which the DB cluster is associated.
dbCluster_cloneGroupId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_cloneGroupId = Lens.lens (\DBCluster' {cloneGroupId} -> cloneGroupId) (\s@DBCluster' {} a -> s {cloneGroupId = a} :: DBCluster)

-- | The Amazon Resource Name (ARN) for the DB cluster.
dbCluster_dbClusterArn :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterArn = Lens.lens (\DBCluster' {dbClusterArn} -> dbClusterArn) (\s@DBCluster' {} a -> s {dbClusterArn = a} :: DBCluster)

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

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
dbCluster_hostedZoneId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_hostedZoneId = Lens.lens (\DBCluster' {hostedZoneId} -> hostedZoneId) (\s@DBCluster' {} a -> s {hostedZoneId = a} :: DBCluster)

-- | Specifies the progress of the operation as a percentage.
dbCluster_percentProgress :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_percentProgress = Lens.lens (\DBCluster' {percentProgress} -> percentProgress) (\s@DBCluster' {} a -> s {percentProgress = a} :: DBCluster)

-- | Specifies whether you have requested to enable write forwarding for a
-- secondary cluster in an Aurora global database. Because write forwarding
-- takes time to enable, check the value of @GlobalWriteForwardingStatus@
-- to confirm that the request has completed before using the write
-- forwarding feature for this cluster.
dbCluster_globalWriteForwardingRequested :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_globalWriteForwardingRequested = Lens.lens (\DBCluster' {globalWriteForwardingRequested} -> globalWriteForwardingRequested) (\s@DBCluster' {} a -> s {globalWriteForwardingRequested = a} :: DBCluster)

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
dbCluster_preferredBackupWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_preferredBackupWindow = Lens.lens (\DBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@DBCluster' {} a -> s {preferredBackupWindow = a} :: DBCluster)

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
dbCluster_backupRetentionPeriod :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_backupRetentionPeriod = Lens.lens (\DBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBCluster' {} a -> s {backupRetentionPeriod = a} :: DBCluster)

-- | If present, specifies the name of the character set that this cluster is
-- associated with.
dbCluster_characterSetName :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_characterSetName = Lens.lens (\DBCluster' {characterSetName} -> characterSetName) (\s@DBCluster' {} a -> s {characterSetName = a} :: DBCluster)

-- | Contains the master username for the DB cluster.
dbCluster_masterUsername :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_masterUsername = Lens.lens (\DBCluster' {masterUsername} -> masterUsername) (\s@DBCluster' {} a -> s {masterUsername = a} :: DBCluster)

-- | Specifies whether tags are copied from the DB cluster to snapshots of
-- the DB cluster.
dbCluster_copyTagsToSnapshot :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_copyTagsToSnapshot = Lens.lens (\DBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@DBCluster' {} a -> s {copyTagsToSnapshot = a} :: DBCluster)

-- | The status of the database activity stream.
dbCluster_activityStreamStatus :: Lens.Lens' DBCluster (Prelude.Maybe ActivityStreamStatus)
dbCluster_activityStreamStatus = Lens.lens (\DBCluster' {activityStreamStatus} -> activityStreamStatus) (\s@DBCluster' {} a -> s {activityStreamStatus = a} :: DBCluster)

-- | Provides the list of instances that make up the DB cluster.
dbCluster_dbClusterMembers :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterMember])
dbCluster_dbClusterMembers = Lens.lens (\DBCluster' {dbClusterMembers} -> dbClusterMembers) (\s@DBCluster' {} a -> s {dbClusterMembers = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the DB cluster parameter group for the DB cluster.
dbCluster_dbClusterParameterGroup :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterParameterGroup = Lens.lens (\DBCluster' {dbClusterParameterGroup} -> dbClusterParameterGroup) (\s@DBCluster' {} a -> s {dbClusterParameterGroup = a} :: DBCluster)

-- | A value that indicates that minor version patches are applied
-- automatically.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_autoMinorVersionUpgrade :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_autoMinorVersionUpgrade = Lens.lens (\DBCluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@DBCluster' {} a -> s {autoMinorVersionUpgrade = a} :: DBCluster)

-- | The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. The database session
-- can handle these events either synchronously or asynchronously.
dbCluster_activityStreamMode :: Lens.Lens' DBCluster (Prelude.Maybe ActivityStreamMode)
dbCluster_activityStreamMode = Lens.lens (\DBCluster' {activityStreamMode} -> activityStreamMode) (\s@DBCluster' {} a -> s {activityStreamMode = a} :: DBCluster)

-- | Undocumented member.
dbCluster_tagList :: Lens.Lens' DBCluster (Prelude.Maybe [Tag])
dbCluster_tagList = Lens.lens (\DBCluster' {tagList} -> tagList) (\s@DBCluster' {} a -> s {tagList = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbCluster_latestRestorableTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_latestRestorableTime = Lens.lens (\DBCluster' {latestRestorableTime} -> latestRestorableTime) (\s@DBCluster' {} a -> s {latestRestorableTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | The name of the compute and memory capacity class of the DB instance.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_dbClusterInstanceClass :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterInstanceClass = Lens.lens (\DBCluster' {dbClusterInstanceClass} -> dbClusterInstanceClass) (\s@DBCluster' {} a -> s {dbClusterInstanceClass = a} :: DBCluster)

-- | Contains the name of the initial database of this DB cluster that was
-- provided at create time, if one was specified when the DB cluster was
-- created. This same name is returned for the life of the DB cluster.
dbCluster_databaseName :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_databaseName = Lens.lens (\DBCluster' {databaseName} -> databaseName) (\s@DBCluster' {} a -> s {databaseName = a} :: DBCluster)

-- | Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
dbCluster_dbClusterIdentifier :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterIdentifier = Lens.lens (\DBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBCluster' {} a -> s {dbClusterIdentifier = a} :: DBCluster)

-- | The Active Directory Domain membership records associated with the DB
-- cluster.
dbCluster_domainMemberships :: Lens.Lens' DBCluster (Prelude.Maybe [DomainMembership])
dbCluster_domainMemberships = Lens.lens (\DBCluster' {domainMemberships} -> domainMemberships) (\s@DBCluster' {} a -> s {domainMemberships = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
dbCluster_scalingConfigurationInfo :: Lens.Lens' DBCluster (Prelude.Maybe ScalingConfigurationInfo)
dbCluster_scalingConfigurationInfo = Lens.lens (\DBCluster' {scalingConfigurationInfo} -> scalingConfigurationInfo) (\s@DBCluster' {} a -> s {scalingConfigurationInfo = a} :: DBCluster)

-- | Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster can be created.
dbCluster_availabilityZones :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_availabilityZones = Lens.lens (\DBCluster' {availabilityZones} -> availabilityZones) (\s@DBCluster' {} a -> s {availabilityZones = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The earliest time to which a DB cluster can be backtracked.
dbCluster_earliestBacktrackTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_earliestBacktrackTime = Lens.lens (\DBCluster' {earliestBacktrackTime} -> earliestBacktrackTime) (\s@DBCluster' {} a -> s {earliestBacktrackTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | The time when a stopped DB cluster is restarted automatically.
dbCluster_automaticRestartTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_automaticRestartTime = Lens.lens (\DBCluster' {automaticRestartTime} -> automaticRestartTime) (\s@DBCluster' {} a -> s {automaticRestartTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_performanceInsightsKMSKeyId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_performanceInsightsKMSKeyId = Lens.lens (\DBCluster' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@DBCluster' {} a -> s {performanceInsightsKMSKeyId = a} :: DBCluster)

-- | Specifies whether the DB cluster is a clone of a DB cluster owned by a
-- different Amazon Web Services account.
dbCluster_crossAccountClone :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_crossAccountClone = Lens.lens (\DBCluster' {crossAccountClone} -> crossAccountClone) (\s@DBCluster' {} a -> s {crossAccountClone = a} :: DBCluster)

-- | Provides the list of option group memberships for this DB cluster.
dbCluster_dbClusterOptionGroupMemberships :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterOptionGroupStatus])
dbCluster_dbClusterOptionGroupMemberships = Lens.lens (\DBCluster' {dbClusterOptionGroupMemberships} -> dbClusterOptionGroupMemberships) (\s@DBCluster' {} a -> s {dbClusterOptionGroupMemberships = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Specifies information on the subnet group associated with the DB
-- cluster, including the name, description, and subnets in the subnet
-- group.
dbCluster_dbSubnetGroup :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbSubnetGroup = Lens.lens (\DBCluster' {dbSubnetGroup} -> dbSubnetGroup) (\s@DBCluster' {} a -> s {dbSubnetGroup = a} :: DBCluster)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB cluster.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_monitoringInterval :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_monitoringInterval = Lens.lens (\DBCluster' {monitoringInterval} -> monitoringInterval) (\s@DBCluster' {} a -> s {monitoringInterval = a} :: DBCluster)

-- | True if Performance Insights is enabled for the DB cluster, and
-- otherwise false.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_performanceInsightsEnabled :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_performanceInsightsEnabled = Lens.lens (\DBCluster' {performanceInsightsEnabled} -> performanceInsightsEnabled) (\s@DBCluster' {} a -> s {performanceInsightsEnabled = a} :: DBCluster)

-- | Specifies the current state of this DB cluster.
dbCluster_status :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_status = Lens.lens (\DBCluster' {status} -> status) (\s@DBCluster' {} a -> s {status = a} :: DBCluster)

-- | The name of the Amazon Kinesis data stream used for the database
-- activity stream.
dbCluster_activityStreamKinesisStreamName :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_activityStreamKinesisStreamName = Lens.lens (\DBCluster' {activityStreamKinesisStreamName} -> activityStreamKinesisStreamName) (\s@DBCluster' {} a -> s {activityStreamKinesisStreamName = a} :: DBCluster)

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

-- | The storage type associated with the DB cluster.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_storageType :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_storageType = Lens.lens (\DBCluster' {storageType} -> storageType) (\s@DBCluster' {} a -> s {storageType = a} :: DBCluster)

-- | The target backtrack window, in seconds. If this value is set to 0,
-- backtracking is disabled for the DB cluster. Otherwise, backtracking is
-- enabled.
dbCluster_backtrackWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Integer)
dbCluster_backtrackWindow = Lens.lens (\DBCluster' {backtrackWindow} -> backtrackWindow) (\s@DBCluster' {} a -> s {backtrackWindow = a} :: DBCluster)

-- | Identifies all custom endpoints associated with the cluster.
dbCluster_customEndpoints :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_customEndpoints = Lens.lens (\DBCluster' {customEndpoints} -> customEndpoints) (\s@DBCluster' {} a -> s {customEndpoints = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Contains the identifier of the source DB cluster if this DB cluster is a
-- read replica.
dbCluster_replicationSourceIdentifier :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_replicationSourceIdentifier = Lens.lens (\DBCluster' {replicationSourceIdentifier} -> replicationSourceIdentifier) (\s@DBCluster' {} a -> s {replicationSourceIdentifier = a} :: DBCluster)

-- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring
-- metrics to Amazon CloudWatch Logs.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_monitoringRoleArn :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_monitoringRoleArn = Lens.lens (\DBCluster' {monitoringRoleArn} -> monitoringRoleArn) (\s@DBCluster' {} a -> s {monitoringRoleArn = a} :: DBCluster)

-- | The number of change records stored for Backtrack.
dbCluster_backtrackConsumedChangeRecords :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Integer)
dbCluster_backtrackConsumedChangeRecords = Lens.lens (\DBCluster' {backtrackConsumedChangeRecords} -> backtrackConsumedChangeRecords) (\s@DBCluster' {} a -> s {backtrackConsumedChangeRecords = a} :: DBCluster)

-- | The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster>.
dbCluster_engineMode :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engineMode = Lens.lens (\DBCluster' {engineMode} -> engineMode) (\s@DBCluster' {} a -> s {engineMode = a} :: DBCluster)

-- | Specifies whether the DB cluster is encrypted.
dbCluster_storageEncrypted :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_storageEncrypted = Lens.lens (\DBCluster' {storageEncrypted} -> storageEncrypted) (\s@DBCluster' {} a -> s {storageEncrypted = a} :: DBCluster)

-- | If @StorageEncrypted@ is enabled, the Amazon Web Services KMS key
-- identifier for the encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
dbCluster_kmsKeyId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_kmsKeyId = Lens.lens (\DBCluster' {kmsKeyId} -> kmsKeyId) (\s@DBCluster' {} a -> s {kmsKeyId = a} :: DBCluster)

-- | The name of the database engine to be used for this DB cluster.
dbCluster_engine :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engine = Lens.lens (\DBCluster' {engine} -> engine) (\s@DBCluster' {} a -> s {engine = a} :: DBCluster)

-- | For all database engines except Amazon Aurora, @AllocatedStorage@
-- specifies the allocated storage size in gibibytes (GiB). For Aurora,
-- @AllocatedStorage@ always returns 1, because Aurora DB cluster storage
-- size isn\'t fixed, but instead automatically adjusts as needed.
dbCluster_allocatedStorage :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_allocatedStorage = Lens.lens (\DBCluster' {allocatedStorage} -> allocatedStorage) (\s@DBCluster' {} a -> s {allocatedStorage = a} :: DBCluster)

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

-- | The earliest time to which a database can be restored with point-in-time
-- restore.
dbCluster_earliestRestorableTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_earliestRestorableTime = Lens.lens (\DBCluster' {earliestRestorableTime} -> earliestRestorableTime) (\s@DBCluster' {} a -> s {earliestRestorableTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | A value that indicates whether the mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts is
-- enabled.
dbCluster_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBCluster' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBCluster' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBCluster)

-- | Indicates if the DB cluster has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled.
dbCluster_deletionProtection :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_deletionProtection = Lens.lens (\DBCluster' {deletionProtection} -> deletionProtection) (\s@DBCluster' {} a -> s {deletionProtection = a} :: DBCluster)

-- | A value that specifies that changes to the DB cluster are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
dbCluster_pendingModifiedValues :: Lens.Lens' DBCluster (Prelude.Maybe ClusterPendingModifiedValues)
dbCluster_pendingModifiedValues = Lens.lens (\DBCluster' {pendingModifiedValues} -> pendingModifiedValues) (\s@DBCluster' {} a -> s {pendingModifiedValues = a} :: DBCluster)

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
dbCluster_preferredMaintenanceWindow :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_preferredMaintenanceWindow = Lens.lens (\DBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@DBCluster' {} a -> s {preferredMaintenanceWindow = a} :: DBCluster)

-- | Specifies the connection endpoint for the primary instance of the DB
-- cluster.
dbCluster_endpoint :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_endpoint = Lens.lens (\DBCluster' {endpoint} -> endpoint) (\s@DBCluster' {} a -> s {endpoint = a} :: DBCluster)

-- | The current capacity of an Aurora Serverless v1 DB cluster. The capacity
-- is 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless v1, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
dbCluster_capacity :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_capacity = Lens.lens (\DBCluster' {capacity} -> capacity) (\s@DBCluster' {} a -> s {capacity = a} :: DBCluster)

-- | Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
dbCluster_clusterCreateTime :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.UTCTime)
dbCluster_clusterCreateTime = Lens.lens (\DBCluster' {clusterCreateTime} -> clusterCreateTime) (\s@DBCluster' {} a -> s {clusterCreateTime = a} :: DBCluster) Prelude.. Lens.mapping Core._Time

-- | Contains one or more identifiers of the read replicas associated with
-- this DB cluster.
dbCluster_readReplicaIdentifiers :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_readReplicaIdentifiers = Lens.lens (\DBCluster' {readReplicaIdentifiers} -> readReplicaIdentifiers) (\s@DBCluster' {} a -> s {readReplicaIdentifiers = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Reserved for future use.
dbCluster_dbSystemId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbSystemId = Lens.lens (\DBCluster' {dbSystemId} -> dbSystemId) (\s@DBCluster' {} a -> s {dbSystemId = a} :: DBCluster)

-- | A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon Aurora User Guide./
dbCluster_enabledCloudwatchLogsExports :: Lens.Lens' DBCluster (Prelude.Maybe [Prelude.Text])
dbCluster_enabledCloudwatchLogsExports = Lens.lens (\DBCluster' {enabledCloudwatchLogsExports} -> enabledCloudwatchLogsExports) (\s@DBCluster' {} a -> s {enabledCloudwatchLogsExports = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The Provisioned IOPS (I\/O operations per second) value.
--
-- This setting is only for non-Aurora Multi-AZ DB clusters.
dbCluster_iops :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Int)
dbCluster_iops = Lens.lens (\DBCluster' {iops} -> iops) (\s@DBCluster' {} a -> s {iops = a} :: DBCluster)

-- | The Amazon Web Services Region-unique, immutable identifier for the DB
-- cluster. This identifier is found in Amazon Web Services CloudTrail log
-- entries whenever the KMS key for the DB cluster is accessed.
dbCluster_dbClusterResourceId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_dbClusterResourceId = Lens.lens (\DBCluster' {dbClusterResourceId} -> dbClusterResourceId) (\s@DBCluster' {} a -> s {dbClusterResourceId = a} :: DBCluster)

-- | Provides a list of the Amazon Web Services Identity and Access
-- Management (IAM) roles that are associated with the DB cluster. IAM
-- roles that are associated with a DB cluster grant permission for the DB
-- cluster to access other Amazon Web Services on your behalf.
dbCluster_associatedRoles :: Lens.Lens' DBCluster (Prelude.Maybe [DBClusterRole])
dbCluster_associatedRoles = Lens.lens (\DBCluster' {associatedRoles} -> associatedRoles) (\s@DBCluster' {} a -> s {associatedRoles = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the database engine version.
dbCluster_engineVersion :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_engineVersion = Lens.lens (\DBCluster' {engineVersion} -> engineVersion) (\s@DBCluster' {} a -> s {engineVersion = a} :: DBCluster)

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

-- | Specifies whether the DB cluster has instances in multiple Availability
-- Zones.
dbCluster_multiAZ :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Bool)
dbCluster_multiAZ = Lens.lens (\DBCluster' {multiAZ} -> multiAZ) (\s@DBCluster' {} a -> s {multiAZ = a} :: DBCluster)

-- | The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
dbCluster_activityStreamKmsKeyId :: Lens.Lens' DBCluster (Prelude.Maybe Prelude.Text)
dbCluster_activityStreamKmsKeyId = Lens.lens (\DBCluster' {activityStreamKmsKeyId} -> activityStreamKmsKeyId) (\s@DBCluster' {} a -> s {activityStreamKmsKeyId = a} :: DBCluster)

-- | Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
dbCluster_globalWriteForwardingStatus :: Lens.Lens' DBCluster (Prelude.Maybe WriteForwardingStatus)
dbCluster_globalWriteForwardingStatus = Lens.lens (\DBCluster' {globalWriteForwardingStatus} -> globalWriteForwardingStatus) (\s@DBCluster' {} a -> s {globalWriteForwardingStatus = a} :: DBCluster)

-- | Provides a list of VPC security groups that the DB cluster belongs to.
dbCluster_vpcSecurityGroups :: Lens.Lens' DBCluster (Prelude.Maybe [VpcSecurityGroupMembership])
dbCluster_vpcSecurityGroups = Lens.lens (\DBCluster' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@DBCluster' {} a -> s {vpcSecurityGroups = a} :: DBCluster) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML DBCluster where
  parseXML x =
    DBCluster'
      Prelude.<$> (x Core..@? "Port")
      Prelude.<*> (x Core..@? "ServerlessV2ScalingConfiguration")
      Prelude.<*> (x Core..@? "CloneGroupId")
      Prelude.<*> (x Core..@? "DBClusterArn")
      Prelude.<*> (x Core..@? "PerformanceInsightsRetentionPeriod")
      Prelude.<*> (x Core..@? "HostedZoneId")
      Prelude.<*> (x Core..@? "PercentProgress")
      Prelude.<*> (x Core..@? "GlobalWriteForwardingRequested")
      Prelude.<*> (x Core..@? "PreferredBackupWindow")
      Prelude.<*> (x Core..@? "BackupRetentionPeriod")
      Prelude.<*> (x Core..@? "CharacterSetName")
      Prelude.<*> (x Core..@? "MasterUsername")
      Prelude.<*> (x Core..@? "CopyTagsToSnapshot")
      Prelude.<*> (x Core..@? "ActivityStreamStatus")
      Prelude.<*> ( x Core..@? "DBClusterMembers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBClusterMember")
                  )
      Prelude.<*> (x Core..@? "DBClusterParameterGroup")
      Prelude.<*> (x Core..@? "AutoMinorVersionUpgrade")
      Prelude.<*> (x Core..@? "ActivityStreamMode")
      Prelude.<*> ( x Core..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "LatestRestorableTime")
      Prelude.<*> (x Core..@? "DBClusterInstanceClass")
      Prelude.<*> (x Core..@? "DatabaseName")
      Prelude.<*> (x Core..@? "DBClusterIdentifier")
      Prelude.<*> ( x Core..@? "DomainMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DomainMembership")
                  )
      Prelude.<*> (x Core..@? "ScalingConfigurationInfo")
      Prelude.<*> ( x Core..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Core..@? "EarliestBacktrackTime")
      Prelude.<*> (x Core..@? "AutomaticRestartTime")
      Prelude.<*> (x Core..@? "PerformanceInsightsKMSKeyId")
      Prelude.<*> (x Core..@? "CrossAccountClone")
      Prelude.<*> ( x Core..@? "DBClusterOptionGroupMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBClusterOptionGroup")
                  )
      Prelude.<*> (x Core..@? "DBSubnetGroup")
      Prelude.<*> (x Core..@? "MonitoringInterval")
      Prelude.<*> (x Core..@? "PerformanceInsightsEnabled")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "ActivityStreamKinesisStreamName")
      Prelude.<*> (x Core..@? "HttpEndpointEnabled")
      Prelude.<*> (x Core..@? "PubliclyAccessible")
      Prelude.<*> (x Core..@? "StorageType")
      Prelude.<*> (x Core..@? "BacktrackWindow")
      Prelude.<*> ( x Core..@? "CustomEndpoints" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "ReplicationSourceIdentifier")
      Prelude.<*> (x Core..@? "MonitoringRoleArn")
      Prelude.<*> (x Core..@? "BacktrackConsumedChangeRecords")
      Prelude.<*> (x Core..@? "EngineMode")
      Prelude.<*> (x Core..@? "StorageEncrypted")
      Prelude.<*> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "AllocatedStorage")
      Prelude.<*> (x Core..@? "ReaderEndpoint")
      Prelude.<*> (x Core..@? "EarliestRestorableTime")
      Prelude.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Core..@? "DeletionProtection")
      Prelude.<*> (x Core..@? "PendingModifiedValues")
      Prelude.<*> (x Core..@? "PreferredMaintenanceWindow")
      Prelude.<*> (x Core..@? "Endpoint")
      Prelude.<*> (x Core..@? "Capacity")
      Prelude.<*> (x Core..@? "ClusterCreateTime")
      Prelude.<*> ( x Core..@? "ReadReplicaIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ReadReplicaIdentifier")
                  )
      Prelude.<*> (x Core..@? "DBSystemId")
      Prelude.<*> ( x Core..@? "EnabledCloudwatchLogsExports"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Iops")
      Prelude.<*> (x Core..@? "DbClusterResourceId")
      Prelude.<*> ( x Core..@? "AssociatedRoles" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBClusterRole")
                  )
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "NetworkType")
      Prelude.<*> (x Core..@? "MultiAZ")
      Prelude.<*> (x Core..@? "ActivityStreamKmsKeyId")
      Prelude.<*> (x Core..@? "GlobalWriteForwardingStatus")
      Prelude.<*> ( x Core..@? "VpcSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "VpcSecurityGroupMembership")
                  )

instance Prelude.Hashable DBCluster where
  hashWithSalt _salt DBCluster' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` serverlessV2ScalingConfiguration
      `Prelude.hashWithSalt` cloneGroupId
      `Prelude.hashWithSalt` dbClusterArn
      `Prelude.hashWithSalt` performanceInsightsRetentionPeriod
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` percentProgress
      `Prelude.hashWithSalt` globalWriteForwardingRequested
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` activityStreamStatus
      `Prelude.hashWithSalt` dbClusterMembers
      `Prelude.hashWithSalt` dbClusterParameterGroup
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` activityStreamMode
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` latestRestorableTime
      `Prelude.hashWithSalt` dbClusterInstanceClass
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` domainMemberships
      `Prelude.hashWithSalt` scalingConfigurationInfo
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` earliestBacktrackTime
      `Prelude.hashWithSalt` automaticRestartTime
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` crossAccountClone
      `Prelude.hashWithSalt` dbClusterOptionGroupMemberships
      `Prelude.hashWithSalt` dbSubnetGroup
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` performanceInsightsEnabled
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` activityStreamKinesisStreamName
      `Prelude.hashWithSalt` httpEndpointEnabled
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` backtrackWindow
      `Prelude.hashWithSalt` customEndpoints
      `Prelude.hashWithSalt` replicationSourceIdentifier
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` backtrackConsumedChangeRecords
      `Prelude.hashWithSalt` engineMode
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` readerEndpoint
      `Prelude.hashWithSalt` earliestRestorableTime
      `Prelude.hashWithSalt` iAMDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` clusterCreateTime
      `Prelude.hashWithSalt` readReplicaIdentifiers
      `Prelude.hashWithSalt` dbSystemId
      `Prelude.hashWithSalt` enabledCloudwatchLogsExports
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` dbClusterResourceId
      `Prelude.hashWithSalt` associatedRoles
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` activityStreamKmsKeyId
      `Prelude.hashWithSalt` globalWriteForwardingStatus
      `Prelude.hashWithSalt` vpcSecurityGroups

instance Prelude.NFData DBCluster where
  rnf DBCluster' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf serverlessV2ScalingConfiguration
      `Prelude.seq` Prelude.rnf cloneGroupId
      `Prelude.seq` Prelude.rnf dbClusterArn
      `Prelude.seq` Prelude.rnf performanceInsightsRetentionPeriod
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf percentProgress
      `Prelude.seq` Prelude.rnf globalWriteForwardingRequested
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf activityStreamStatus
      `Prelude.seq` Prelude.rnf dbClusterMembers
      `Prelude.seq` Prelude.rnf dbClusterParameterGroup
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf activityStreamMode
      `Prelude.seq` Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf
        latestRestorableTime
      `Prelude.seq` Prelude.rnf
        dbClusterInstanceClass
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf
        dbClusterIdentifier
      `Prelude.seq` Prelude.rnf
        domainMemberships
      `Prelude.seq` Prelude.rnf
        scalingConfigurationInfo
      `Prelude.seq` Prelude.rnf
        availabilityZones
      `Prelude.seq` Prelude.rnf
        earliestBacktrackTime
      `Prelude.seq` Prelude.rnf
        automaticRestartTime
      `Prelude.seq` Prelude.rnf
        performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        crossAccountClone
      `Prelude.seq` Prelude.rnf
        dbClusterOptionGroupMemberships
      `Prelude.seq` Prelude.rnf
        dbSubnetGroup
      `Prelude.seq` Prelude.rnf
        monitoringInterval
      `Prelude.seq` Prelude.rnf
        performanceInsightsEnabled
      `Prelude.seq` Prelude.rnf
        status
      `Prelude.seq` Prelude.rnf
        activityStreamKinesisStreamName
      `Prelude.seq` Prelude.rnf
        httpEndpointEnabled
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        backtrackWindow
      `Prelude.seq` Prelude.rnf
        customEndpoints
      `Prelude.seq` Prelude.rnf
        replicationSourceIdentifier
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf
        backtrackConsumedChangeRecords
      `Prelude.seq` Prelude.rnf
        engineMode
      `Prelude.seq` Prelude.rnf
        storageEncrypted
      `Prelude.seq` Prelude.rnf
        kmsKeyId
      `Prelude.seq` Prelude.rnf
        engine
      `Prelude.seq` Prelude.rnf
        allocatedStorage
      `Prelude.seq` Prelude.rnf
        readerEndpoint
      `Prelude.seq` Prelude.rnf
        earliestRestorableTime
      `Prelude.seq` Prelude.rnf
        iAMDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf
        deletionProtection
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        endpoint
      `Prelude.seq` Prelude.rnf
        capacity
      `Prelude.seq` Prelude.rnf
        clusterCreateTime
      `Prelude.seq` Prelude.rnf
        readReplicaIdentifiers
      `Prelude.seq` Prelude.rnf
        dbSystemId
      `Prelude.seq` Prelude.rnf
        enabledCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf
        iops
      `Prelude.seq` Prelude.rnf
        dbClusterResourceId
      `Prelude.seq` Prelude.rnf
        associatedRoles
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        networkType
      `Prelude.seq` Prelude.rnf
        multiAZ
      `Prelude.seq` Prelude.rnf
        activityStreamKmsKeyId
      `Prelude.seq` Prelude.rnf
        globalWriteForwardingStatus
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroups
