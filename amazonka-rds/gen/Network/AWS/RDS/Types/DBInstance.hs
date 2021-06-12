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
-- Module      : Network.AWS.RDS.Types.DBInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBInstance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.DBInstanceAutomatedBackupsReplication
import Network.AWS.RDS.Types.DBInstanceRole
import Network.AWS.RDS.Types.DBInstanceStatusInfo
import Network.AWS.RDS.Types.DBParameterGroupStatus
import Network.AWS.RDS.Types.DBSecurityGroupMembership
import Network.AWS.RDS.Types.DBSubnetGroup
import Network.AWS.RDS.Types.DomainMembership
import Network.AWS.RDS.Types.Endpoint
import Network.AWS.RDS.Types.OptionGroupMembership
import Network.AWS.RDS.Types.PendingModifiedValues
import Network.AWS.RDS.Types.ProcessorFeature
import Network.AWS.RDS.Types.ReplicaMode
import Network.AWS.RDS.Types.Tag
import Network.AWS.RDS.Types.VpcSecurityGroupMembership

-- | Contains the details of an Amazon RDS DB instance.
--
-- This data type is used as a response element in the
-- @DescribeDBInstances@ action.
--
-- /See:/ 'newDBInstance' smart constructor.
data DBInstance = DBInstance'
  { -- | Specifies the number of days for which automatic DB snapshots are
    -- retained.
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | Indicates if the DB instance has deletion protection enabled. The
    -- database can\'t be deleted when deletion protection is enabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | Specifies whether the DB instance is encrypted.
    storageEncrypted :: Core.Maybe Core.Bool,
    -- | The AWS Identity and Access Management (IAM) roles associated with the
    -- DB instance.
    associatedRoles :: Core.Maybe [DBInstanceRole],
    -- | Provides a list of VPC security group elements that the DB instance
    -- belongs to.
    vpcSecurityGroups :: Core.Maybe [VpcSecurityGroupMembership],
    -- | Specifies the daily time range during which automated backups are
    -- created if automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@.
    preferredBackupWindow :: Core.Maybe Core.Text,
    -- | The identifier of the CA certificate for this DB instance.
    cACertificateIdentifier :: Core.Maybe Core.Text,
    -- | A list of DB security group elements containing @DBSecurityGroup.Name@
    -- and @DBSecurityGroup.Status@ subelements.
    dbSecurityGroups :: Core.Maybe [DBSecurityGroupMembership],
    -- | The upper limit to which Amazon RDS can automatically scale the storage
    -- of the DB instance.
    maxAllocatedStorage :: Core.Maybe Core.Int,
    -- | The status of a read replica. If the instance isn\'t a read replica,
    -- this is blank.
    statusInfos :: Core.Maybe [DBInstanceStatusInfo],
    -- | Specifies the storage type associated with DB instance.
    storageType :: Core.Maybe Core.Text,
    -- | Specifies the latest time to which a database can be restored with
    -- point-in-time restore.
    latestRestorableTime :: Core.Maybe Core.ISO8601,
    -- | The Active Directory Domain membership records associated with the DB
    -- instance.
    domainMemberships :: Core.Maybe [DomainMembership],
    -- | Specifies the port that the DB instance listens on. If the DB instance
    -- is part of a DB cluster, this can be a different port than the DB
    -- cluster port.
    dbInstancePort :: Core.Maybe Core.Int,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance.
    monitoringInterval :: Core.Maybe Core.Int,
    -- | Provides the list of DB parameter groups applied to this DB instance.
    dbParameterGroups :: Core.Maybe [DBParameterGroupStatus],
    -- | Provides the date and time the DB instance was created.
    instanceCreateTime :: Core.Maybe Core.ISO8601,
    -- | The AWS Region-unique, immutable identifier for the DB instance. This
    -- identifier is found in AWS CloudTrail log entries whenever the AWS KMS
    -- customer master key (CMK) for the DB instance is accessed.
    dbiResourceId :: Core.Maybe Core.Text,
    -- | Provides the list of option group memberships for this DB instance.
    optionGroupMemberships :: Core.Maybe [OptionGroupMembership],
    -- | Specifies the listener connection endpoint for SQL Server Always On.
    listenerEndpoint :: Core.Maybe Endpoint,
    -- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring
    -- metrics to Amazon CloudWatch Logs.
    monitoringRoleArn :: Core.Maybe Core.Text,
    -- | A list of log types that this DB instance is configured to export to
    -- CloudWatch Logs.
    --
    -- Log types vary by DB engine. For information about the log types for
    -- each DB engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
    -- in the /Amazon RDS User Guide./
    enabledCloudwatchLogsExports :: Core.Maybe [Core.Text],
    -- | Specifies information on the subnet group associated with the DB
    -- instance, including the name, description, and subnets in the subnet
    -- group.
    dbSubnetGroup :: Core.Maybe DBSubnetGroup,
    -- | The name of the NCHAR character set for the Oracle DB instance. This
    -- character set specifies the Unicode encoding for data stored in table
    -- columns of type NCHAR, NCLOB, or NVARCHAR2.
    ncharCharacterSetName :: Core.Maybe Core.Text,
    -- | Contains the master username for the DB instance.
    masterUsername :: Core.Maybe Core.Text,
    -- | Specifies if the DB instance is a Multi-AZ deployment.
    multiAZ :: Core.Maybe Core.Bool,
    -- | Specifies the accessibility options for the DB instance.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves
    -- to the private IP address from within the DB instance\'s VPC, and to the
    -- public IP address from outside of the DB instance\'s VPC. Access to the
    -- DB instance is ultimately controlled by the security group it uses, and
    -- that public access is not permitted if the security group assigned to
    -- the DB instance doesn\'t permit it.
    --
    -- When the DB instance isn\'t publicly accessible, it is an internal DB
    -- instance with a DNS name that resolves to a private IP address.
    --
    -- For more information, see CreateDBInstance.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | The AWS KMS key identifier for encryption of Performance Insights data.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    performanceInsightsKMSKeyId :: Core.Maybe Core.Text,
    -- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the
    -- encrypted DB instance.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Specifies the name of the Availability Zone the DB instance is located
    -- in.
    availabilityZone :: Core.Maybe Core.Text,
    -- | If the DB instance is a member of a DB cluster, contains the name of the
    -- DB cluster that the DB instance is a member of.
    dbClusterIdentifier :: Core.Maybe Core.Text,
    -- | Specifies whether a customer-owned IP address (CoIP) is enabled for an
    -- RDS on Outposts DB instance.
    --
    -- A /CoIP/ provides local or external connectivity to resources in your
    -- Outpost subnets through your on-premises network. For some use cases, a
    -- CoIP can provide lower latency for connections to the DB instance from
    -- outside of its virtual private cloud (VPC) on your local network.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about CoIPs, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
    -- in the /AWS Outposts User Guide/.
    customerOwnedIpEnabled :: Core.Maybe Core.Bool,
    -- | Indicates the database engine version.
    engineVersion :: Core.Maybe Core.Text,
    -- | Specifies the weekly time range during which system maintenance can
    -- occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | If present, specifies the name of the character set that this instance
    -- is associated with.
    characterSetName :: Core.Maybe Core.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid
    -- values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Core.Maybe Core.Int,
    -- | License model information for this DB instance.
    licenseModel :: Core.Maybe Core.Text,
    -- | Contains a user-supplied database identifier. This identifier is the
    -- unique key that identifies a DB instance.
    dbInstanceIdentifier :: Core.Maybe Core.Text,
    -- | A value that specifies the order in which an Aurora Replica is promoted
    -- to the primary instance after a failure of the existing primary
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
    -- in the /Amazon Aurora User Guide/.
    promotionTier :: Core.Maybe Core.Int,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Core.Maybe [ProcessorFeature],
    -- | The Amazon Resource Name (ARN) of the recovery point in AWS Backup.
    awsBackupRecoveryPointArn :: Core.Maybe Core.Text,
    -- | Contains the name of the compute and memory capacity class of the DB
    -- instance.
    dbInstanceClass :: Core.Maybe Core.Text,
    -- | The meaning of this parameter differs according to the database engine
    -- you use.
    --
    -- __MySQL, MariaDB, SQL Server, PostgreSQL__
    --
    -- Contains the name of the initial database of this instance that was
    -- provided at create time, if one was specified when the DB instance was
    -- created. This same name is returned for the life of the DB instance.
    --
    -- Type: String
    --
    -- __Oracle__
    --
    -- Contains the Oracle System ID (SID) of the created DB instance. Not
    -- shown when the returned parameters do not apply to an Oracle DB
    -- instance.
    dbName :: Core.Maybe Core.Text,
    -- | The name of the database engine to be used for this DB instance.
    engine :: Core.Maybe Core.Text,
    -- | Contains one or more identifiers of Aurora DB clusters to which the RDS
    -- DB instance is replicated as a read replica. For example, when you
    -- create an Aurora read replica of an RDS MySQL DB instance, the Aurora
    -- MySQL DB cluster for the Aurora read replica is shown. This output does
    -- not contain information about cross region Aurora read replicas.
    --
    -- Currently, each RDS DB instance can have only one Aurora read replica.
    readReplicaDBClusterIdentifiers :: Core.Maybe [Core.Text],
    -- | A value that specifies that changes to the DB instance are pending. This
    -- element is only included when changes are pending. Specific changes are
    -- identified by subelements.
    pendingModifiedValues :: Core.Maybe PendingModifiedValues,
    -- | The ARN from the key store with which the instance is associated for TDE
    -- encryption.
    tdeCredentialArn :: Core.Maybe Core.Text,
    -- | The time zone of the DB instance. In most cases, the @Timezone@ element
    -- is empty. @Timezone@ content appears only for Microsoft SQL Server DB
    -- instances that were created with a time zone specified.
    timezone :: Core.Maybe Core.Text,
    -- | Specifies the current state of this database.
    --
    -- For information about DB instance statuses, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Status.html DB Instance Status>
    -- in the /Amazon RDS User Guide./
    dbInstanceStatus :: Core.Maybe Core.Text,
    -- | The list of replicated automated backups associated with the DB
    -- instance.
    dbInstanceAutomatedBackupsReplications :: Core.Maybe [DBInstanceAutomatedBackupsReplication],
    -- | Specifies whether tags are copied from the DB instance to snapshots of
    -- the DB instance.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. Copying tags to snapshots is managed by the DB cluster.
    -- Setting this value for an Aurora DB instance has no effect on the DB
    -- cluster setting. For more information, see @DBCluster@.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
    -- | Contains the identifier of the source DB instance if this DB instance is
    -- a read replica.
    readReplicaSourceDBInstanceIdentifier :: Core.Maybe Core.Text,
    -- | The open mode of an Oracle read replica. The default is
    -- @open-read-only@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
    -- in the /Amazon RDS User Guide/.
    --
    -- This attribute is only supported in RDS for Oracle.
    replicaMode :: Core.Maybe ReplicaMode,
    -- | Specifies the connection endpoint.
    endpoint :: Core.Maybe Endpoint,
    tagList :: Core.Maybe [Tag],
    -- | True if Performance Insights is enabled for the DB instance, and
    -- otherwise false.
    performanceInsightsEnabled :: Core.Maybe Core.Bool,
    -- | Specifies the allocated storage size specified in gibibytes.
    allocatedStorage :: Core.Maybe Core.Int,
    -- | True if mapping of AWS Identity and Access Management (IAM) accounts to
    -- database accounts is enabled, and otherwise false.
    --
    -- IAM database authentication can be enabled for the following database
    -- engines
    --
    -- -   For MySQL 5.6, minor version 5.6.34 or higher
    --
    -- -   For MySQL 5.7, minor version 5.7.16 or higher
    --
    -- -   Aurora 5.6 or higher. To enable IAM database authentication for
    --     Aurora, see DBCluster Type.
    iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool,
    -- | If present, specifies the name of the secondary Availability Zone for a
    -- DB instance with multi-AZ support.
    secondaryAvailabilityZone :: Core.Maybe Core.Text,
    -- | Specifies the Provisioned IOPS (I\/O operations per second) value.
    iops :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
    -- that receives the Enhanced Monitoring metrics data for the DB instance.
    enhancedMonitoringResourceArn :: Core.Maybe Core.Text,
    -- | Contains one or more identifiers of the read replicas associated with
    -- this DB instance.
    readReplicaDBInstanceIdentifiers :: Core.Maybe [Core.Text],
    -- | A value that indicates that minor version patches are applied
    -- automatically.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) for the DB instance.
    dbInstanceArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPeriod', 'dbInstance_backupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are
-- retained.
--
-- 'deletionProtection', 'dbInstance_deletionProtection' - Indicates if the DB instance has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'storageEncrypted', 'dbInstance_storageEncrypted' - Specifies whether the DB instance is encrypted.
--
-- 'associatedRoles', 'dbInstance_associatedRoles' - The AWS Identity and Access Management (IAM) roles associated with the
-- DB instance.
--
-- 'vpcSecurityGroups', 'dbInstance_vpcSecurityGroups' - Provides a list of VPC security group elements that the DB instance
-- belongs to.
--
-- 'preferredBackupWindow', 'dbInstance_preferredBackupWindow' - Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
--
-- 'cACertificateIdentifier', 'dbInstance_cACertificateIdentifier' - The identifier of the CA certificate for this DB instance.
--
-- 'dbSecurityGroups', 'dbInstance_dbSecurityGroups' - A list of DB security group elements containing @DBSecurityGroup.Name@
-- and @DBSecurityGroup.Status@ subelements.
--
-- 'maxAllocatedStorage', 'dbInstance_maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
--
-- 'statusInfos', 'dbInstance_statusInfos' - The status of a read replica. If the instance isn\'t a read replica,
-- this is blank.
--
-- 'storageType', 'dbInstance_storageType' - Specifies the storage type associated with DB instance.
--
-- 'latestRestorableTime', 'dbInstance_latestRestorableTime' - Specifies the latest time to which a database can be restored with
-- point-in-time restore.
--
-- 'domainMemberships', 'dbInstance_domainMemberships' - The Active Directory Domain membership records associated with the DB
-- instance.
--
-- 'dbInstancePort', 'dbInstance_dbInstancePort' - Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
--
-- 'monitoringInterval', 'dbInstance_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance.
--
-- 'dbParameterGroups', 'dbInstance_dbParameterGroups' - Provides the list of DB parameter groups applied to this DB instance.
--
-- 'instanceCreateTime', 'dbInstance_instanceCreateTime' - Provides the date and time the DB instance was created.
--
-- 'dbiResourceId', 'dbInstance_dbiResourceId' - The AWS Region-unique, immutable identifier for the DB instance. This
-- identifier is found in AWS CloudTrail log entries whenever the AWS KMS
-- customer master key (CMK) for the DB instance is accessed.
--
-- 'optionGroupMemberships', 'dbInstance_optionGroupMemberships' - Provides the list of option group memberships for this DB instance.
--
-- 'listenerEndpoint', 'dbInstance_listenerEndpoint' - Specifies the listener connection endpoint for SQL Server Always On.
--
-- 'monitoringRoleArn', 'dbInstance_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send Enhanced Monitoring
-- metrics to Amazon CloudWatch Logs.
--
-- 'enabledCloudwatchLogsExports', 'dbInstance_enabledCloudwatchLogsExports' - A list of log types that this DB instance is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon RDS User Guide./
--
-- 'dbSubnetGroup', 'dbInstance_dbSubnetGroup' - Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
--
-- 'ncharCharacterSetName', 'dbInstance_ncharCharacterSetName' - The name of the NCHAR character set for the Oracle DB instance. This
-- character set specifies the Unicode encoding for data stored in table
-- columns of type NCHAR, NCLOB, or NVARCHAR2.
--
-- 'masterUsername', 'dbInstance_masterUsername' - Contains the master username for the DB instance.
--
-- 'multiAZ', 'dbInstance_multiAZ' - Specifies if the DB instance is a Multi-AZ deployment.
--
-- 'publiclyAccessible', 'dbInstance_publiclyAccessible' - Specifies the accessibility options for the DB instance.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves
-- to the private IP address from within the DB instance\'s VPC, and to the
-- public IP address from outside of the DB instance\'s VPC. Access to the
-- DB instance is ultimately controlled by the security group it uses, and
-- that public access is not permitted if the security group assigned to
-- the DB instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
--
-- 'performanceInsightsKMSKeyId', 'dbInstance_performanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- 'kmsKeyId', 'dbInstance_kmsKeyId' - If @StorageEncrypted@ is true, the AWS KMS key identifier for the
-- encrypted DB instance.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- 'availabilityZone', 'dbInstance_availabilityZone' - Specifies the name of the Availability Zone the DB instance is located
-- in.
--
-- 'dbClusterIdentifier', 'dbInstance_dbClusterIdentifier' - If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
--
-- 'customerOwnedIpEnabled', 'dbInstance_customerOwnedIpEnabled' - Specifies whether a customer-owned IP address (CoIP) is enabled for an
-- RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /AWS Outposts User Guide/.
--
-- 'engineVersion', 'dbInstance_engineVersion' - Indicates the database engine version.
--
-- 'preferredMaintenanceWindow', 'dbInstance_preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
--
-- 'characterSetName', 'dbInstance_characterSetName' - If present, specifies the name of the character set that this instance
-- is associated with.
--
-- 'performanceInsightsRetentionPeriod', 'dbInstance_performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
--
-- 'licenseModel', 'dbInstance_licenseModel' - License model information for this DB instance.
--
-- 'dbInstanceIdentifier', 'dbInstance_dbInstanceIdentifier' - Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
--
-- 'promotionTier', 'dbInstance_promotionTier' - A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- 'processorFeatures', 'dbInstance_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'awsBackupRecoveryPointArn', 'dbInstance_awsBackupRecoveryPointArn' - The Amazon Resource Name (ARN) of the recovery point in AWS Backup.
--
-- 'dbInstanceClass', 'dbInstance_dbInstanceClass' - Contains the name of the compute and memory capacity class of the DB
-- instance.
--
-- 'dbName', 'dbInstance_dbName' - The meaning of this parameter differs according to the database engine
-- you use.
--
-- __MySQL, MariaDB, SQL Server, PostgreSQL__
--
-- Contains the name of the initial database of this instance that was
-- provided at create time, if one was specified when the DB instance was
-- created. This same name is returned for the life of the DB instance.
--
-- Type: String
--
-- __Oracle__
--
-- Contains the Oracle System ID (SID) of the created DB instance. Not
-- shown when the returned parameters do not apply to an Oracle DB
-- instance.
--
-- 'engine', 'dbInstance_engine' - The name of the database engine to be used for this DB instance.
--
-- 'readReplicaDBClusterIdentifiers', 'dbInstance_readReplicaDBClusterIdentifiers' - Contains one or more identifiers of Aurora DB clusters to which the RDS
-- DB instance is replicated as a read replica. For example, when you
-- create an Aurora read replica of an RDS MySQL DB instance, the Aurora
-- MySQL DB cluster for the Aurora read replica is shown. This output does
-- not contain information about cross region Aurora read replicas.
--
-- Currently, each RDS DB instance can have only one Aurora read replica.
--
-- 'pendingModifiedValues', 'dbInstance_pendingModifiedValues' - A value that specifies that changes to the DB instance are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
--
-- 'tdeCredentialArn', 'dbInstance_tdeCredentialArn' - The ARN from the key store with which the instance is associated for TDE
-- encryption.
--
-- 'timezone', 'dbInstance_timezone' - The time zone of the DB instance. In most cases, the @Timezone@ element
-- is empty. @Timezone@ content appears only for Microsoft SQL Server DB
-- instances that were created with a time zone specified.
--
-- 'dbInstanceStatus', 'dbInstance_dbInstanceStatus' - Specifies the current state of this database.
--
-- For information about DB instance statuses, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Status.html DB Instance Status>
-- in the /Amazon RDS User Guide./
--
-- 'dbInstanceAutomatedBackupsReplications', 'dbInstance_dbInstanceAutomatedBackupsReplications' - The list of replicated automated backups associated with the DB
-- instance.
--
-- 'copyTagsToSnapshot', 'dbInstance_copyTagsToSnapshot' - Specifies whether tags are copied from the DB instance to snapshots of
-- the DB instance.
--
-- __Amazon Aurora__
--
-- Not applicable. Copying tags to snapshots is managed by the DB cluster.
-- Setting this value for an Aurora DB instance has no effect on the DB
-- cluster setting. For more information, see @DBCluster@.
--
-- 'readReplicaSourceDBInstanceIdentifier', 'dbInstance_readReplicaSourceDBInstanceIdentifier' - Contains the identifier of the source DB instance if this DB instance is
-- a read replica.
--
-- 'replicaMode', 'dbInstance_replicaMode' - The open mode of an Oracle read replica. The default is
-- @open-read-only@. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
-- in the /Amazon RDS User Guide/.
--
-- This attribute is only supported in RDS for Oracle.
--
-- 'endpoint', 'dbInstance_endpoint' - Specifies the connection endpoint.
--
-- 'tagList', 'dbInstance_tagList' - Undocumented member.
--
-- 'performanceInsightsEnabled', 'dbInstance_performanceInsightsEnabled' - True if Performance Insights is enabled for the DB instance, and
-- otherwise false.
--
-- 'allocatedStorage', 'dbInstance_allocatedStorage' - Specifies the allocated storage size specified in gibibytes.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbInstance_iAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to
-- database accounts is enabled, and otherwise false.
--
-- IAM database authentication can be enabled for the following database
-- engines
--
-- -   For MySQL 5.6, minor version 5.6.34 or higher
--
-- -   For MySQL 5.7, minor version 5.7.16 or higher
--
-- -   Aurora 5.6 or higher. To enable IAM database authentication for
--     Aurora, see DBCluster Type.
--
-- 'secondaryAvailabilityZone', 'dbInstance_secondaryAvailabilityZone' - If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
--
-- 'iops', 'dbInstance_iops' - Specifies the Provisioned IOPS (I\/O operations per second) value.
--
-- 'enhancedMonitoringResourceArn', 'dbInstance_enhancedMonitoringResourceArn' - The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
-- that receives the Enhanced Monitoring metrics data for the DB instance.
--
-- 'readReplicaDBInstanceIdentifiers', 'dbInstance_readReplicaDBInstanceIdentifiers' - Contains one or more identifiers of the read replicas associated with
-- this DB instance.
--
-- 'autoMinorVersionUpgrade', 'dbInstance_autoMinorVersionUpgrade' - A value that indicates that minor version patches are applied
-- automatically.
--
-- 'dbInstanceArn', 'dbInstance_dbInstanceArn' - The Amazon Resource Name (ARN) for the DB instance.
newDBInstance ::
  DBInstance
newDBInstance =
  DBInstance'
    { backupRetentionPeriod = Core.Nothing,
      deletionProtection = Core.Nothing,
      storageEncrypted = Core.Nothing,
      associatedRoles = Core.Nothing,
      vpcSecurityGroups = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      cACertificateIdentifier = Core.Nothing,
      dbSecurityGroups = Core.Nothing,
      maxAllocatedStorage = Core.Nothing,
      statusInfos = Core.Nothing,
      storageType = Core.Nothing,
      latestRestorableTime = Core.Nothing,
      domainMemberships = Core.Nothing,
      dbInstancePort = Core.Nothing,
      monitoringInterval = Core.Nothing,
      dbParameterGroups = Core.Nothing,
      instanceCreateTime = Core.Nothing,
      dbiResourceId = Core.Nothing,
      optionGroupMemberships = Core.Nothing,
      listenerEndpoint = Core.Nothing,
      monitoringRoleArn = Core.Nothing,
      enabledCloudwatchLogsExports = Core.Nothing,
      dbSubnetGroup = Core.Nothing,
      ncharCharacterSetName = Core.Nothing,
      masterUsername = Core.Nothing,
      multiAZ = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      performanceInsightsKMSKeyId = Core.Nothing,
      kmsKeyId = Core.Nothing,
      availabilityZone = Core.Nothing,
      dbClusterIdentifier = Core.Nothing,
      customerOwnedIpEnabled = Core.Nothing,
      engineVersion = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      characterSetName = Core.Nothing,
      performanceInsightsRetentionPeriod = Core.Nothing,
      licenseModel = Core.Nothing,
      dbInstanceIdentifier = Core.Nothing,
      promotionTier = Core.Nothing,
      processorFeatures = Core.Nothing,
      awsBackupRecoveryPointArn = Core.Nothing,
      dbInstanceClass = Core.Nothing,
      dbName = Core.Nothing,
      engine = Core.Nothing,
      readReplicaDBClusterIdentifiers = Core.Nothing,
      pendingModifiedValues = Core.Nothing,
      tdeCredentialArn = Core.Nothing,
      timezone = Core.Nothing,
      dbInstanceStatus = Core.Nothing,
      dbInstanceAutomatedBackupsReplications =
        Core.Nothing,
      copyTagsToSnapshot = Core.Nothing,
      readReplicaSourceDBInstanceIdentifier = Core.Nothing,
      replicaMode = Core.Nothing,
      endpoint = Core.Nothing,
      tagList = Core.Nothing,
      performanceInsightsEnabled = Core.Nothing,
      allocatedStorage = Core.Nothing,
      iAMDatabaseAuthenticationEnabled = Core.Nothing,
      secondaryAvailabilityZone = Core.Nothing,
      iops = Core.Nothing,
      enhancedMonitoringResourceArn = Core.Nothing,
      readReplicaDBInstanceIdentifiers = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      dbInstanceArn = Core.Nothing
    }

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
dbInstance_backupRetentionPeriod :: Lens.Lens' DBInstance (Core.Maybe Core.Int)
dbInstance_backupRetentionPeriod = Lens.lens (\DBInstance' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBInstance' {} a -> s {backupRetentionPeriod = a} :: DBInstance)

-- | Indicates if the DB instance has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
dbInstance_deletionProtection :: Lens.Lens' DBInstance (Core.Maybe Core.Bool)
dbInstance_deletionProtection = Lens.lens (\DBInstance' {deletionProtection} -> deletionProtection) (\s@DBInstance' {} a -> s {deletionProtection = a} :: DBInstance)

-- | Specifies whether the DB instance is encrypted.
dbInstance_storageEncrypted :: Lens.Lens' DBInstance (Core.Maybe Core.Bool)
dbInstance_storageEncrypted = Lens.lens (\DBInstance' {storageEncrypted} -> storageEncrypted) (\s@DBInstance' {} a -> s {storageEncrypted = a} :: DBInstance)

-- | The AWS Identity and Access Management (IAM) roles associated with the
-- DB instance.
dbInstance_associatedRoles :: Lens.Lens' DBInstance (Core.Maybe [DBInstanceRole])
dbInstance_associatedRoles = Lens.lens (\DBInstance' {associatedRoles} -> associatedRoles) (\s@DBInstance' {} a -> s {associatedRoles = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | Provides a list of VPC security group elements that the DB instance
-- belongs to.
dbInstance_vpcSecurityGroups :: Lens.Lens' DBInstance (Core.Maybe [VpcSecurityGroupMembership])
dbInstance_vpcSecurityGroups = Lens.lens (\DBInstance' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@DBInstance' {} a -> s {vpcSecurityGroups = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
dbInstance_preferredBackupWindow :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_preferredBackupWindow = Lens.lens (\DBInstance' {preferredBackupWindow} -> preferredBackupWindow) (\s@DBInstance' {} a -> s {preferredBackupWindow = a} :: DBInstance)

-- | The identifier of the CA certificate for this DB instance.
dbInstance_cACertificateIdentifier :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_cACertificateIdentifier = Lens.lens (\DBInstance' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@DBInstance' {} a -> s {cACertificateIdentifier = a} :: DBInstance)

-- | A list of DB security group elements containing @DBSecurityGroup.Name@
-- and @DBSecurityGroup.Status@ subelements.
dbInstance_dbSecurityGroups :: Lens.Lens' DBInstance (Core.Maybe [DBSecurityGroupMembership])
dbInstance_dbSecurityGroups = Lens.lens (\DBInstance' {dbSecurityGroups} -> dbSecurityGroups) (\s@DBInstance' {} a -> s {dbSecurityGroups = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
dbInstance_maxAllocatedStorage :: Lens.Lens' DBInstance (Core.Maybe Core.Int)
dbInstance_maxAllocatedStorage = Lens.lens (\DBInstance' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@DBInstance' {} a -> s {maxAllocatedStorage = a} :: DBInstance)

-- | The status of a read replica. If the instance isn\'t a read replica,
-- this is blank.
dbInstance_statusInfos :: Lens.Lens' DBInstance (Core.Maybe [DBInstanceStatusInfo])
dbInstance_statusInfos = Lens.lens (\DBInstance' {statusInfos} -> statusInfos) (\s@DBInstance' {} a -> s {statusInfos = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | Specifies the storage type associated with DB instance.
dbInstance_storageType :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_storageType = Lens.lens (\DBInstance' {storageType} -> storageType) (\s@DBInstance' {} a -> s {storageType = a} :: DBInstance)

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbInstance_latestRestorableTime :: Lens.Lens' DBInstance (Core.Maybe Core.UTCTime)
dbInstance_latestRestorableTime = Lens.lens (\DBInstance' {latestRestorableTime} -> latestRestorableTime) (\s@DBInstance' {} a -> s {latestRestorableTime = a} :: DBInstance) Core.. Lens.mapping Core._Time

-- | The Active Directory Domain membership records associated with the DB
-- instance.
dbInstance_domainMemberships :: Lens.Lens' DBInstance (Core.Maybe [DomainMembership])
dbInstance_domainMemberships = Lens.lens (\DBInstance' {domainMemberships} -> domainMemberships) (\s@DBInstance' {} a -> s {domainMemberships = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
dbInstance_dbInstancePort :: Lens.Lens' DBInstance (Core.Maybe Core.Int)
dbInstance_dbInstancePort = Lens.lens (\DBInstance' {dbInstancePort} -> dbInstancePort) (\s@DBInstance' {} a -> s {dbInstancePort = a} :: DBInstance)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance.
dbInstance_monitoringInterval :: Lens.Lens' DBInstance (Core.Maybe Core.Int)
dbInstance_monitoringInterval = Lens.lens (\DBInstance' {monitoringInterval} -> monitoringInterval) (\s@DBInstance' {} a -> s {monitoringInterval = a} :: DBInstance)

-- | Provides the list of DB parameter groups applied to this DB instance.
dbInstance_dbParameterGroups :: Lens.Lens' DBInstance (Core.Maybe [DBParameterGroupStatus])
dbInstance_dbParameterGroups = Lens.lens (\DBInstance' {dbParameterGroups} -> dbParameterGroups) (\s@DBInstance' {} a -> s {dbParameterGroups = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | Provides the date and time the DB instance was created.
dbInstance_instanceCreateTime :: Lens.Lens' DBInstance (Core.Maybe Core.UTCTime)
dbInstance_instanceCreateTime = Lens.lens (\DBInstance' {instanceCreateTime} -> instanceCreateTime) (\s@DBInstance' {} a -> s {instanceCreateTime = a} :: DBInstance) Core.. Lens.mapping Core._Time

-- | The AWS Region-unique, immutable identifier for the DB instance. This
-- identifier is found in AWS CloudTrail log entries whenever the AWS KMS
-- customer master key (CMK) for the DB instance is accessed.
dbInstance_dbiResourceId :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_dbiResourceId = Lens.lens (\DBInstance' {dbiResourceId} -> dbiResourceId) (\s@DBInstance' {} a -> s {dbiResourceId = a} :: DBInstance)

-- | Provides the list of option group memberships for this DB instance.
dbInstance_optionGroupMemberships :: Lens.Lens' DBInstance (Core.Maybe [OptionGroupMembership])
dbInstance_optionGroupMemberships = Lens.lens (\DBInstance' {optionGroupMemberships} -> optionGroupMemberships) (\s@DBInstance' {} a -> s {optionGroupMemberships = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | Specifies the listener connection endpoint for SQL Server Always On.
dbInstance_listenerEndpoint :: Lens.Lens' DBInstance (Core.Maybe Endpoint)
dbInstance_listenerEndpoint = Lens.lens (\DBInstance' {listenerEndpoint} -> listenerEndpoint) (\s@DBInstance' {} a -> s {listenerEndpoint = a} :: DBInstance)

-- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring
-- metrics to Amazon CloudWatch Logs.
dbInstance_monitoringRoleArn :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_monitoringRoleArn = Lens.lens (\DBInstance' {monitoringRoleArn} -> monitoringRoleArn) (\s@DBInstance' {} a -> s {monitoringRoleArn = a} :: DBInstance)

-- | A list of log types that this DB instance is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon RDS User Guide./
dbInstance_enabledCloudwatchLogsExports :: Lens.Lens' DBInstance (Core.Maybe [Core.Text])
dbInstance_enabledCloudwatchLogsExports = Lens.lens (\DBInstance' {enabledCloudwatchLogsExports} -> enabledCloudwatchLogsExports) (\s@DBInstance' {} a -> s {enabledCloudwatchLogsExports = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
dbInstance_dbSubnetGroup :: Lens.Lens' DBInstance (Core.Maybe DBSubnetGroup)
dbInstance_dbSubnetGroup = Lens.lens (\DBInstance' {dbSubnetGroup} -> dbSubnetGroup) (\s@DBInstance' {} a -> s {dbSubnetGroup = a} :: DBInstance)

-- | The name of the NCHAR character set for the Oracle DB instance. This
-- character set specifies the Unicode encoding for data stored in table
-- columns of type NCHAR, NCLOB, or NVARCHAR2.
dbInstance_ncharCharacterSetName :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_ncharCharacterSetName = Lens.lens (\DBInstance' {ncharCharacterSetName} -> ncharCharacterSetName) (\s@DBInstance' {} a -> s {ncharCharacterSetName = a} :: DBInstance)

-- | Contains the master username for the DB instance.
dbInstance_masterUsername :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_masterUsername = Lens.lens (\DBInstance' {masterUsername} -> masterUsername) (\s@DBInstance' {} a -> s {masterUsername = a} :: DBInstance)

-- | Specifies if the DB instance is a Multi-AZ deployment.
dbInstance_multiAZ :: Lens.Lens' DBInstance (Core.Maybe Core.Bool)
dbInstance_multiAZ = Lens.lens (\DBInstance' {multiAZ} -> multiAZ) (\s@DBInstance' {} a -> s {multiAZ = a} :: DBInstance)

-- | Specifies the accessibility options for the DB instance.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves
-- to the private IP address from within the DB instance\'s VPC, and to the
-- public IP address from outside of the DB instance\'s VPC. Access to the
-- DB instance is ultimately controlled by the security group it uses, and
-- that public access is not permitted if the security group assigned to
-- the DB instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
dbInstance_publiclyAccessible :: Lens.Lens' DBInstance (Core.Maybe Core.Bool)
dbInstance_publiclyAccessible = Lens.lens (\DBInstance' {publiclyAccessible} -> publiclyAccessible) (\s@DBInstance' {} a -> s {publiclyAccessible = a} :: DBInstance)

-- | The AWS KMS key identifier for encryption of Performance Insights data.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
dbInstance_performanceInsightsKMSKeyId :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_performanceInsightsKMSKeyId = Lens.lens (\DBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@DBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: DBInstance)

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the
-- encrypted DB instance.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
dbInstance_kmsKeyId :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_kmsKeyId = Lens.lens (\DBInstance' {kmsKeyId} -> kmsKeyId) (\s@DBInstance' {} a -> s {kmsKeyId = a} :: DBInstance)

-- | Specifies the name of the Availability Zone the DB instance is located
-- in.
dbInstance_availabilityZone :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_availabilityZone = Lens.lens (\DBInstance' {availabilityZone} -> availabilityZone) (\s@DBInstance' {} a -> s {availabilityZone = a} :: DBInstance)

-- | If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
dbInstance_dbClusterIdentifier :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_dbClusterIdentifier = Lens.lens (\DBInstance' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBInstance' {} a -> s {dbClusterIdentifier = a} :: DBInstance)

-- | Specifies whether a customer-owned IP address (CoIP) is enabled for an
-- RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /AWS Outposts User Guide/.
dbInstance_customerOwnedIpEnabled :: Lens.Lens' DBInstance (Core.Maybe Core.Bool)
dbInstance_customerOwnedIpEnabled = Lens.lens (\DBInstance' {customerOwnedIpEnabled} -> customerOwnedIpEnabled) (\s@DBInstance' {} a -> s {customerOwnedIpEnabled = a} :: DBInstance)

-- | Indicates the database engine version.
dbInstance_engineVersion :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_engineVersion = Lens.lens (\DBInstance' {engineVersion} -> engineVersion) (\s@DBInstance' {} a -> s {engineVersion = a} :: DBInstance)

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
dbInstance_preferredMaintenanceWindow :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_preferredMaintenanceWindow = Lens.lens (\DBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@DBInstance' {} a -> s {preferredMaintenanceWindow = a} :: DBInstance)

-- | If present, specifies the name of the character set that this instance
-- is associated with.
dbInstance_characterSetName :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_characterSetName = Lens.lens (\DBInstance' {characterSetName} -> characterSetName) (\s@DBInstance' {} a -> s {characterSetName = a} :: DBInstance)

-- | The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
dbInstance_performanceInsightsRetentionPeriod :: Lens.Lens' DBInstance (Core.Maybe Core.Int)
dbInstance_performanceInsightsRetentionPeriod = Lens.lens (\DBInstance' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@DBInstance' {} a -> s {performanceInsightsRetentionPeriod = a} :: DBInstance)

-- | License model information for this DB instance.
dbInstance_licenseModel :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_licenseModel = Lens.lens (\DBInstance' {licenseModel} -> licenseModel) (\s@DBInstance' {} a -> s {licenseModel = a} :: DBInstance)

-- | Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
dbInstance_dbInstanceIdentifier :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_dbInstanceIdentifier = Lens.lens (\DBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBInstance' {} a -> s {dbInstanceIdentifier = a} :: DBInstance)

-- | A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
dbInstance_promotionTier :: Lens.Lens' DBInstance (Core.Maybe Core.Int)
dbInstance_promotionTier = Lens.lens (\DBInstance' {promotionTier} -> promotionTier) (\s@DBInstance' {} a -> s {promotionTier = a} :: DBInstance)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
dbInstance_processorFeatures :: Lens.Lens' DBInstance (Core.Maybe [ProcessorFeature])
dbInstance_processorFeatures = Lens.lens (\DBInstance' {processorFeatures} -> processorFeatures) (\s@DBInstance' {} a -> s {processorFeatures = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the recovery point in AWS Backup.
dbInstance_awsBackupRecoveryPointArn :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_awsBackupRecoveryPointArn = Lens.lens (\DBInstance' {awsBackupRecoveryPointArn} -> awsBackupRecoveryPointArn) (\s@DBInstance' {} a -> s {awsBackupRecoveryPointArn = a} :: DBInstance)

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
dbInstance_dbInstanceClass :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_dbInstanceClass = Lens.lens (\DBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@DBInstance' {} a -> s {dbInstanceClass = a} :: DBInstance)

-- | The meaning of this parameter differs according to the database engine
-- you use.
--
-- __MySQL, MariaDB, SQL Server, PostgreSQL__
--
-- Contains the name of the initial database of this instance that was
-- provided at create time, if one was specified when the DB instance was
-- created. This same name is returned for the life of the DB instance.
--
-- Type: String
--
-- __Oracle__
--
-- Contains the Oracle System ID (SID) of the created DB instance. Not
-- shown when the returned parameters do not apply to an Oracle DB
-- instance.
dbInstance_dbName :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_dbName = Lens.lens (\DBInstance' {dbName} -> dbName) (\s@DBInstance' {} a -> s {dbName = a} :: DBInstance)

-- | The name of the database engine to be used for this DB instance.
dbInstance_engine :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_engine = Lens.lens (\DBInstance' {engine} -> engine) (\s@DBInstance' {} a -> s {engine = a} :: DBInstance)

-- | Contains one or more identifiers of Aurora DB clusters to which the RDS
-- DB instance is replicated as a read replica. For example, when you
-- create an Aurora read replica of an RDS MySQL DB instance, the Aurora
-- MySQL DB cluster for the Aurora read replica is shown. This output does
-- not contain information about cross region Aurora read replicas.
--
-- Currently, each RDS DB instance can have only one Aurora read replica.
dbInstance_readReplicaDBClusterIdentifiers :: Lens.Lens' DBInstance (Core.Maybe [Core.Text])
dbInstance_readReplicaDBClusterIdentifiers = Lens.lens (\DBInstance' {readReplicaDBClusterIdentifiers} -> readReplicaDBClusterIdentifiers) (\s@DBInstance' {} a -> s {readReplicaDBClusterIdentifiers = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | A value that specifies that changes to the DB instance are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
dbInstance_pendingModifiedValues :: Lens.Lens' DBInstance (Core.Maybe PendingModifiedValues)
dbInstance_pendingModifiedValues = Lens.lens (\DBInstance' {pendingModifiedValues} -> pendingModifiedValues) (\s@DBInstance' {} a -> s {pendingModifiedValues = a} :: DBInstance)

-- | The ARN from the key store with which the instance is associated for TDE
-- encryption.
dbInstance_tdeCredentialArn :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_tdeCredentialArn = Lens.lens (\DBInstance' {tdeCredentialArn} -> tdeCredentialArn) (\s@DBInstance' {} a -> s {tdeCredentialArn = a} :: DBInstance)

-- | The time zone of the DB instance. In most cases, the @Timezone@ element
-- is empty. @Timezone@ content appears only for Microsoft SQL Server DB
-- instances that were created with a time zone specified.
dbInstance_timezone :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_timezone = Lens.lens (\DBInstance' {timezone} -> timezone) (\s@DBInstance' {} a -> s {timezone = a} :: DBInstance)

-- | Specifies the current state of this database.
--
-- For information about DB instance statuses, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Status.html DB Instance Status>
-- in the /Amazon RDS User Guide./
dbInstance_dbInstanceStatus :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_dbInstanceStatus = Lens.lens (\DBInstance' {dbInstanceStatus} -> dbInstanceStatus) (\s@DBInstance' {} a -> s {dbInstanceStatus = a} :: DBInstance)

-- | The list of replicated automated backups associated with the DB
-- instance.
dbInstance_dbInstanceAutomatedBackupsReplications :: Lens.Lens' DBInstance (Core.Maybe [DBInstanceAutomatedBackupsReplication])
dbInstance_dbInstanceAutomatedBackupsReplications = Lens.lens (\DBInstance' {dbInstanceAutomatedBackupsReplications} -> dbInstanceAutomatedBackupsReplications) (\s@DBInstance' {} a -> s {dbInstanceAutomatedBackupsReplications = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether tags are copied from the DB instance to snapshots of
-- the DB instance.
--
-- __Amazon Aurora__
--
-- Not applicable. Copying tags to snapshots is managed by the DB cluster.
-- Setting this value for an Aurora DB instance has no effect on the DB
-- cluster setting. For more information, see @DBCluster@.
dbInstance_copyTagsToSnapshot :: Lens.Lens' DBInstance (Core.Maybe Core.Bool)
dbInstance_copyTagsToSnapshot = Lens.lens (\DBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@DBInstance' {} a -> s {copyTagsToSnapshot = a} :: DBInstance)

-- | Contains the identifier of the source DB instance if this DB instance is
-- a read replica.
dbInstance_readReplicaSourceDBInstanceIdentifier :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_readReplicaSourceDBInstanceIdentifier = Lens.lens (\DBInstance' {readReplicaSourceDBInstanceIdentifier} -> readReplicaSourceDBInstanceIdentifier) (\s@DBInstance' {} a -> s {readReplicaSourceDBInstanceIdentifier = a} :: DBInstance)

-- | The open mode of an Oracle read replica. The default is
-- @open-read-only@. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
-- in the /Amazon RDS User Guide/.
--
-- This attribute is only supported in RDS for Oracle.
dbInstance_replicaMode :: Lens.Lens' DBInstance (Core.Maybe ReplicaMode)
dbInstance_replicaMode = Lens.lens (\DBInstance' {replicaMode} -> replicaMode) (\s@DBInstance' {} a -> s {replicaMode = a} :: DBInstance)

-- | Specifies the connection endpoint.
dbInstance_endpoint :: Lens.Lens' DBInstance (Core.Maybe Endpoint)
dbInstance_endpoint = Lens.lens (\DBInstance' {endpoint} -> endpoint) (\s@DBInstance' {} a -> s {endpoint = a} :: DBInstance)

-- | Undocumented member.
dbInstance_tagList :: Lens.Lens' DBInstance (Core.Maybe [Tag])
dbInstance_tagList = Lens.lens (\DBInstance' {tagList} -> tagList) (\s@DBInstance' {} a -> s {tagList = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | True if Performance Insights is enabled for the DB instance, and
-- otherwise false.
dbInstance_performanceInsightsEnabled :: Lens.Lens' DBInstance (Core.Maybe Core.Bool)
dbInstance_performanceInsightsEnabled = Lens.lens (\DBInstance' {performanceInsightsEnabled} -> performanceInsightsEnabled) (\s@DBInstance' {} a -> s {performanceInsightsEnabled = a} :: DBInstance)

-- | Specifies the allocated storage size specified in gibibytes.
dbInstance_allocatedStorage :: Lens.Lens' DBInstance (Core.Maybe Core.Int)
dbInstance_allocatedStorage = Lens.lens (\DBInstance' {allocatedStorage} -> allocatedStorage) (\s@DBInstance' {} a -> s {allocatedStorage = a} :: DBInstance)

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to
-- database accounts is enabled, and otherwise false.
--
-- IAM database authentication can be enabled for the following database
-- engines
--
-- -   For MySQL 5.6, minor version 5.6.34 or higher
--
-- -   For MySQL 5.7, minor version 5.7.16 or higher
--
-- -   Aurora 5.6 or higher. To enable IAM database authentication for
--     Aurora, see DBCluster Type.
dbInstance_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBInstance (Core.Maybe Core.Bool)
dbInstance_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBInstance' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBInstance' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBInstance)

-- | If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
dbInstance_secondaryAvailabilityZone :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_secondaryAvailabilityZone = Lens.lens (\DBInstance' {secondaryAvailabilityZone} -> secondaryAvailabilityZone) (\s@DBInstance' {} a -> s {secondaryAvailabilityZone = a} :: DBInstance)

-- | Specifies the Provisioned IOPS (I\/O operations per second) value.
dbInstance_iops :: Lens.Lens' DBInstance (Core.Maybe Core.Int)
dbInstance_iops = Lens.lens (\DBInstance' {iops} -> iops) (\s@DBInstance' {} a -> s {iops = a} :: DBInstance)

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
-- that receives the Enhanced Monitoring metrics data for the DB instance.
dbInstance_enhancedMonitoringResourceArn :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_enhancedMonitoringResourceArn = Lens.lens (\DBInstance' {enhancedMonitoringResourceArn} -> enhancedMonitoringResourceArn) (\s@DBInstance' {} a -> s {enhancedMonitoringResourceArn = a} :: DBInstance)

-- | Contains one or more identifiers of the read replicas associated with
-- this DB instance.
dbInstance_readReplicaDBInstanceIdentifiers :: Lens.Lens' DBInstance (Core.Maybe [Core.Text])
dbInstance_readReplicaDBInstanceIdentifiers = Lens.lens (\DBInstance' {readReplicaDBInstanceIdentifiers} -> readReplicaDBInstanceIdentifiers) (\s@DBInstance' {} a -> s {readReplicaDBInstanceIdentifiers = a} :: DBInstance) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates that minor version patches are applied
-- automatically.
dbInstance_autoMinorVersionUpgrade :: Lens.Lens' DBInstance (Core.Maybe Core.Bool)
dbInstance_autoMinorVersionUpgrade = Lens.lens (\DBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@DBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: DBInstance)

-- | The Amazon Resource Name (ARN) for the DB instance.
dbInstance_dbInstanceArn :: Lens.Lens' DBInstance (Core.Maybe Core.Text)
dbInstance_dbInstanceArn = Lens.lens (\DBInstance' {dbInstanceArn} -> dbInstanceArn) (\s@DBInstance' {} a -> s {dbInstanceArn = a} :: DBInstance)

instance Core.FromXML DBInstance where
  parseXML x =
    DBInstance'
      Core.<$> (x Core..@? "BackupRetentionPeriod")
      Core.<*> (x Core..@? "DeletionProtection")
      Core.<*> (x Core..@? "StorageEncrypted")
      Core.<*> ( x Core..@? "AssociatedRoles" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DBInstanceRole")
               )
      Core.<*> ( x Core..@? "VpcSecurityGroups" Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "VpcSecurityGroupMembership")
               )
      Core.<*> (x Core..@? "PreferredBackupWindow")
      Core.<*> (x Core..@? "CACertificateIdentifier")
      Core.<*> ( x Core..@? "DBSecurityGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DBSecurityGroup")
               )
      Core.<*> (x Core..@? "MaxAllocatedStorage")
      Core.<*> ( x Core..@? "StatusInfos" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DBInstanceStatusInfo")
               )
      Core.<*> (x Core..@? "StorageType")
      Core.<*> (x Core..@? "LatestRestorableTime")
      Core.<*> ( x Core..@? "DomainMemberships" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DomainMembership")
               )
      Core.<*> (x Core..@? "DbInstancePort")
      Core.<*> (x Core..@? "MonitoringInterval")
      Core.<*> ( x Core..@? "DBParameterGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "DBParameterGroup")
               )
      Core.<*> (x Core..@? "InstanceCreateTime")
      Core.<*> (x Core..@? "DbiResourceId")
      Core.<*> ( x Core..@? "OptionGroupMemberships"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "OptionGroupMembership")
               )
      Core.<*> (x Core..@? "ListenerEndpoint")
      Core.<*> (x Core..@? "MonitoringRoleArn")
      Core.<*> ( x Core..@? "EnabledCloudwatchLogsExports"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "DBSubnetGroup")
      Core.<*> (x Core..@? "NcharCharacterSetName")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "MultiAZ")
      Core.<*> (x Core..@? "PubliclyAccessible")
      Core.<*> (x Core..@? "PerformanceInsightsKMSKeyId")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> (x Core..@? "DBClusterIdentifier")
      Core.<*> (x Core..@? "CustomerOwnedIpEnabled")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "PreferredMaintenanceWindow")
      Core.<*> (x Core..@? "CharacterSetName")
      Core.<*> (x Core..@? "PerformanceInsightsRetentionPeriod")
      Core.<*> (x Core..@? "LicenseModel")
      Core.<*> (x Core..@? "DBInstanceIdentifier")
      Core.<*> (x Core..@? "PromotionTier")
      Core.<*> ( x Core..@? "ProcessorFeatures" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "ProcessorFeature")
               )
      Core.<*> (x Core..@? "AwsBackupRecoveryPointArn")
      Core.<*> (x Core..@? "DBInstanceClass")
      Core.<*> (x Core..@? "DBName")
      Core.<*> (x Core..@? "Engine")
      Core.<*> ( x Core..@? "ReadReplicaDBClusterIdentifiers"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "ReadReplicaDBClusterIdentifier")
               )
      Core.<*> (x Core..@? "PendingModifiedValues")
      Core.<*> (x Core..@? "TdeCredentialArn")
      Core.<*> (x Core..@? "Timezone")
      Core.<*> (x Core..@? "DBInstanceStatus")
      Core.<*> ( x Core..@? "DBInstanceAutomatedBackupsReplications"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     ( Core.parseXMLList
                         "DBInstanceAutomatedBackupsReplication"
                     )
               )
      Core.<*> (x Core..@? "CopyTagsToSnapshot")
      Core.<*> (x Core..@? "ReadReplicaSourceDBInstanceIdentifier")
      Core.<*> (x Core..@? "ReplicaMode")
      Core.<*> (x Core..@? "Endpoint")
      Core.<*> ( x Core..@? "TagList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> (x Core..@? "PerformanceInsightsEnabled")
      Core.<*> (x Core..@? "AllocatedStorage")
      Core.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Core.<*> (x Core..@? "SecondaryAvailabilityZone")
      Core.<*> (x Core..@? "Iops")
      Core.<*> (x Core..@? "EnhancedMonitoringResourceArn")
      Core.<*> ( x Core..@? "ReadReplicaDBInstanceIdentifiers"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     ( Core.parseXMLList
                         "ReadReplicaDBInstanceIdentifier"
                     )
               )
      Core.<*> (x Core..@? "AutoMinorVersionUpgrade")
      Core.<*> (x Core..@? "DBInstanceArn")

instance Core.Hashable DBInstance

instance Core.NFData DBInstance
