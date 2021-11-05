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
-- Module      : Amazonka.RDS.Types.DBInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.ActivityStreamMode
import Amazonka.RDS.Types.ActivityStreamStatus
import Amazonka.RDS.Types.DBInstanceAutomatedBackupsReplication
import Amazonka.RDS.Types.DBInstanceRole
import Amazonka.RDS.Types.DBInstanceStatusInfo
import Amazonka.RDS.Types.DBParameterGroupStatus
import Amazonka.RDS.Types.DBSecurityGroupMembership
import Amazonka.RDS.Types.DBSubnetGroup
import Amazonka.RDS.Types.DomainMembership
import Amazonka.RDS.Types.Endpoint
import Amazonka.RDS.Types.OptionGroupMembership
import Amazonka.RDS.Types.PendingModifiedValues
import Amazonka.RDS.Types.ProcessorFeature
import Amazonka.RDS.Types.ReplicaMode
import Amazonka.RDS.Types.Tag
import Amazonka.RDS.Types.VpcSecurityGroupMembership

-- | Contains the details of an Amazon RDS DB instance.
--
-- This data type is used as a response element in the
-- @DescribeDBInstances@ action.
--
-- /See:/ 'newDBInstance' smart constructor.
data DBInstance = DBInstance'
  { -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of DB security group elements containing @DBSecurityGroup.Name@
    -- and @DBSecurityGroup.Status@ subelements.
    dbSecurityGroups :: Prelude.Maybe [DBSecurityGroupMembership],
    -- | Indicates if the DB instance has deletion protection enabled. The
    -- database can\'t be deleted when deletion protection is enabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The time when a stopped DB instance is restarted automatically.
    automaticRestartTime :: Prelude.Maybe Core.ISO8601,
    -- | Specifies whether the DB instance is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | If the DB instance is a member of a DB cluster, contains the name of the
    -- DB cluster that the DB instance is a member of.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
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
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates that minor version patches are applied
    -- automatically.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the DB instance.
    dbInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Kinesis data stream used for the database
    -- activity stream.
    activityStreamKinesisStreamName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether engine-native audit fields are included in the
    -- database activity stream.
    activityStreamEngineNativeAuditFieldsIncluded :: Prelude.Maybe Prelude.Bool,
    -- | Contains the master username for the DB instance.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | Contains one or more identifiers of the read replicas associated with
    -- this DB instance.
    readReplicaDBInstanceIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | True if mapping of Amazon Web Services Identity and Access Management
    -- (IAM) accounts to database accounts is enabled, and otherwise false.
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
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring
    -- metrics to Amazon CloudWatch Logs.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Provisioned IOPS (I\/O operations per second) value.
    iops :: Prelude.Maybe Prelude.Int,
    -- | Provides the date and time the DB instance was created.
    instanceCreateTime :: Prelude.Maybe Core.ISO8601,
    tagList :: Prelude.Maybe [Tag],
    -- | Contains the identifier of the source DB instance if this DB instance is
    -- a read replica.
    readReplicaSourceDBInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The open mode of an Oracle read replica. The default is
    -- @open-read-only@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
    -- in the /Amazon RDS User Guide/.
    --
    -- This attribute is only supported in RDS for Oracle.
    replicaMode :: Prelude.Maybe ReplicaMode,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance.
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The name of the database engine to be used for this DB instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | Specifies the latest time to which a database can be restored with
    -- point-in-time restore.
    latestRestorableTime :: Prelude.Maybe Core.ISO8601,
    -- | Contains the name of the compute and memory capacity class of the DB
    -- instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the order in which an Aurora Replica is promoted
    -- to the primary instance after a failure of the existing primary
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
    -- in the /Amazon Aurora User Guide/.
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the recovery point in Amazon Web
    -- Services Backup.
    awsBackupRecoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | License model information for this DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which system maintenance can
    -- occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid
    -- values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the CA certificate for this DB instance.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Contains a user-supplied database identifier. This identifier is the
    -- unique key that identifies a DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the name of the character set that this instance
    -- is associated with.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
    -- scale the storage of the DB instance.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether a customer-owned IP address (CoIP) is enabled for an
    -- RDS on Outposts DB instance.
    --
    -- A /CoIP/ provides local or external connectivity to resources in your
    -- Outpost subnets through your on-premises network. For some use cases, a
    -- CoIP can provide lower latency for connections to the DB instance from
    -- outside of its virtual private cloud (VPC) on your local network.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about CoIPs, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
    -- in the /Amazon Web Services Outposts User Guide/.
    customerOwnedIpEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If @StorageEncrypted@ is true, the Amazon Web Services KMS key
    -- identifier for the encrypted DB instance.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK).
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the daily time range during which automated backups are
    -- created if automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Identity and Access Management (IAM) roles
    -- associated with the DB instance.
    associatedRoles :: Prelude.Maybe [DBInstanceRole],
    -- | Specifies the name of the Availability Zone the DB instance is located
    -- in.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of VPC security group elements that the DB instance
    -- belongs to.
    vpcSecurityGroups :: Prelude.Maybe [VpcSecurityGroupMembership],
    -- | Specifies the number of days for which automatic DB snapshots are
    -- retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The name of the NCHAR character set for the Oracle DB instance. This
    -- character set specifies the Unicode encoding for data stored in table
    -- columns of type NCHAR, NCLOB, or NVARCHAR2.
    ncharCharacterSetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for encryption of Performance
    -- Insights data.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK).
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies information on the subnet group associated with the DB
    -- instance, including the name, description, and subnets in the subnet
    -- group.
    dbSubnetGroup :: Prelude.Maybe DBSubnetGroup,
    -- | The mode of the database activity stream. Database events such as a
    -- change or access generate an activity stream event. RDS for Oracle
    -- always handles these events asynchronously.
    activityStreamMode :: Prelude.Maybe ActivityStreamMode,
    -- | Specifies if the DB instance is a Multi-AZ deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the listener connection endpoint for SQL Server Always On.
    listenerEndpoint :: Prelude.Maybe Endpoint,
    -- | Provides the list of option group memberships for this DB instance.
    optionGroupMemberships :: Prelude.Maybe [OptionGroupMembership],
    -- | A list of log types that this DB instance is configured to export to
    -- CloudWatch Logs.
    --
    -- Log types vary by DB engine. For information about the log types for
    -- each DB engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
    -- in the /Amazon RDS User Guide./
    enabledCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
    -- that receives the Enhanced Monitoring metrics data for the DB instance.
    enhancedMonitoringResourceArn :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the name of the secondary Availability Zone for a
    -- DB instance with multi-AZ support.
    secondaryAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The status of the database activity stream.
    activityStreamStatus :: Prelude.Maybe ActivityStreamStatus,
    -- | True if Performance Insights is enabled for the DB instance, and
    -- otherwise false.
    performanceInsightsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the allocated storage size specified in gibibytes (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services Region-unique, immutable identifier for the DB
    -- instance. This identifier is found in Amazon Web Services CloudTrail log
    -- entries whenever the Amazon Web Services KMS customer master key (CMK)
    -- for the DB instance is accessed.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of DB parameter groups applied to this DB instance.
    dbParameterGroups :: Prelude.Maybe [DBParameterGroupStatus],
    -- | Specifies whether tags are copied from the DB instance to snapshots of
    -- the DB instance.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. Copying tags to snapshots is managed by the DB cluster.
    -- Setting this value for an Aurora DB instance has no effect on the DB
    -- cluster setting. For more information, see @DBCluster@.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The time zone of the DB instance. In most cases, the @Timezone@ element
    -- is empty. @Timezone@ content appears only for Microsoft SQL Server DB
    -- instances that were created with a time zone specified.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The ARN from the key store with which the instance is associated for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The list of replicated automated backups associated with the DB
    -- instance.
    dbInstanceAutomatedBackupsReplications :: Prelude.Maybe [DBInstanceAutomatedBackupsReplication],
    -- | Specifies the connection endpoint.
    --
    -- The endpoint might not be shown for instances whose status is
    -- @creating@.
    endpoint :: Prelude.Maybe Endpoint,
    -- | Specifies the current state of this database.
    --
    -- For information about DB instance statuses, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/accessing-monitoring.html#Overview.DBInstance.Status Viewing DB instance status>
    -- in the /Amazon RDS User Guide./
    dbInstanceStatus :: Prelude.Maybe Prelude.Text,
    -- | Specifies the port that the DB instance listens on. If the DB instance
    -- is part of a DB cluster, this can be a different port than the DB
    -- cluster port.
    dbInstancePort :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services KMS key identifier used for encrypting messages
    -- in the database activity stream. The Amazon Web Services KMS key
    -- identifier is the key ARN, key ID, alias ARN, or alias name for the
    -- Amazon Web Services KMS customer master key (CMK).
    activityStreamKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies that changes to the DB instance are pending. This
    -- element is only included when changes are pending. Specific changes are
    -- identified by subelements.
    pendingModifiedValues :: Prelude.Maybe PendingModifiedValues,
    -- | Contains one or more identifiers of Aurora DB clusters to which the RDS
    -- DB instance is replicated as a read replica. For example, when you
    -- create an Aurora read replica of an RDS MySQL DB instance, the Aurora
    -- MySQL DB cluster for the Aurora read replica is shown. This output does
    -- not contain information about cross region Aurora read replicas.
    --
    -- Currently, each RDS DB instance can have only one Aurora read replica.
    readReplicaDBClusterIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the storage type associated with DB instance.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The status of a read replica. If the instance isn\'t a read replica,
    -- this is blank.
    statusInfos :: Prelude.Maybe [DBInstanceStatusInfo],
    -- | The Active Directory Domain membership records associated with the DB
    -- instance.
    domainMemberships :: Prelude.Maybe [DomainMembership],
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
    dbName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'dbInstance_engineVersion' - Indicates the database engine version.
--
-- 'dbSecurityGroups', 'dbInstance_dbSecurityGroups' - A list of DB security group elements containing @DBSecurityGroup.Name@
-- and @DBSecurityGroup.Status@ subelements.
--
-- 'deletionProtection', 'dbInstance_deletionProtection' - Indicates if the DB instance has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'automaticRestartTime', 'dbInstance_automaticRestartTime' - The time when a stopped DB instance is restarted automatically.
--
-- 'storageEncrypted', 'dbInstance_storageEncrypted' - Specifies whether the DB instance is encrypted.
--
-- 'dbClusterIdentifier', 'dbInstance_dbClusterIdentifier' - If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
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
-- 'autoMinorVersionUpgrade', 'dbInstance_autoMinorVersionUpgrade' - A value that indicates that minor version patches are applied
-- automatically.
--
-- 'dbInstanceArn', 'dbInstance_dbInstanceArn' - The Amazon Resource Name (ARN) for the DB instance.
--
-- 'activityStreamKinesisStreamName', 'dbInstance_activityStreamKinesisStreamName' - The name of the Amazon Kinesis data stream used for the database
-- activity stream.
--
-- 'activityStreamEngineNativeAuditFieldsIncluded', 'dbInstance_activityStreamEngineNativeAuditFieldsIncluded' - Indicates whether engine-native audit fields are included in the
-- database activity stream.
--
-- 'masterUsername', 'dbInstance_masterUsername' - Contains the master username for the DB instance.
--
-- 'readReplicaDBInstanceIdentifiers', 'dbInstance_readReplicaDBInstanceIdentifiers' - Contains one or more identifiers of the read replicas associated with
-- this DB instance.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbInstance_iAMDatabaseAuthenticationEnabled' - True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
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
-- 'monitoringRoleArn', 'dbInstance_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send Enhanced Monitoring
-- metrics to Amazon CloudWatch Logs.
--
-- 'iops', 'dbInstance_iops' - Specifies the Provisioned IOPS (I\/O operations per second) value.
--
-- 'instanceCreateTime', 'dbInstance_instanceCreateTime' - Provides the date and time the DB instance was created.
--
-- 'tagList', 'dbInstance_tagList' - Undocumented member.
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
-- 'monitoringInterval', 'dbInstance_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance.
--
-- 'engine', 'dbInstance_engine' - The name of the database engine to be used for this DB instance.
--
-- 'processorFeatures', 'dbInstance_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'latestRestorableTime', 'dbInstance_latestRestorableTime' - Specifies the latest time to which a database can be restored with
-- point-in-time restore.
--
-- 'dbInstanceClass', 'dbInstance_dbInstanceClass' - Contains the name of the compute and memory capacity class of the DB
-- instance.
--
-- 'promotionTier', 'dbInstance_promotionTier' - A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- 'awsBackupRecoveryPointArn', 'dbInstance_awsBackupRecoveryPointArn' - The Amazon Resource Name (ARN) of the recovery point in Amazon Web
-- Services Backup.
--
-- 'licenseModel', 'dbInstance_licenseModel' - License model information for this DB instance.
--
-- 'preferredMaintenanceWindow', 'dbInstance_preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
--
-- 'performanceInsightsRetentionPeriod', 'dbInstance_performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
--
-- 'cACertificateIdentifier', 'dbInstance_cACertificateIdentifier' - The identifier of the CA certificate for this DB instance.
--
-- 'dbInstanceIdentifier', 'dbInstance_dbInstanceIdentifier' - Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
--
-- 'characterSetName', 'dbInstance_characterSetName' - If present, specifies the name of the character set that this instance
-- is associated with.
--
-- 'maxAllocatedStorage', 'dbInstance_maxAllocatedStorage' - The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
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
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
--
-- 'kmsKeyId', 'dbInstance_kmsKeyId' - If @StorageEncrypted@ is true, the Amazon Web Services KMS key
-- identifier for the encrypted DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
--
-- 'preferredBackupWindow', 'dbInstance_preferredBackupWindow' - Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
--
-- 'associatedRoles', 'dbInstance_associatedRoles' - The Amazon Web Services Identity and Access Management (IAM) roles
-- associated with the DB instance.
--
-- 'availabilityZone', 'dbInstance_availabilityZone' - Specifies the name of the Availability Zone the DB instance is located
-- in.
--
-- 'vpcSecurityGroups', 'dbInstance_vpcSecurityGroups' - Provides a list of VPC security group elements that the DB instance
-- belongs to.
--
-- 'backupRetentionPeriod', 'dbInstance_backupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are
-- retained.
--
-- 'ncharCharacterSetName', 'dbInstance_ncharCharacterSetName' - The name of the NCHAR character set for the Oracle DB instance. This
-- character set specifies the Unicode encoding for data stored in table
-- columns of type NCHAR, NCLOB, or NVARCHAR2.
--
-- 'performanceInsightsKMSKeyId', 'dbInstance_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
--
-- 'dbSubnetGroup', 'dbInstance_dbSubnetGroup' - Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
--
-- 'activityStreamMode', 'dbInstance_activityStreamMode' - The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. RDS for Oracle
-- always handles these events asynchronously.
--
-- 'multiAZ', 'dbInstance_multiAZ' - Specifies if the DB instance is a Multi-AZ deployment.
--
-- 'listenerEndpoint', 'dbInstance_listenerEndpoint' - Specifies the listener connection endpoint for SQL Server Always On.
--
-- 'optionGroupMemberships', 'dbInstance_optionGroupMemberships' - Provides the list of option group memberships for this DB instance.
--
-- 'enabledCloudwatchLogsExports', 'dbInstance_enabledCloudwatchLogsExports' - A list of log types that this DB instance is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon RDS User Guide./
--
-- 'enhancedMonitoringResourceArn', 'dbInstance_enhancedMonitoringResourceArn' - The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
-- that receives the Enhanced Monitoring metrics data for the DB instance.
--
-- 'secondaryAvailabilityZone', 'dbInstance_secondaryAvailabilityZone' - If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
--
-- 'activityStreamStatus', 'dbInstance_activityStreamStatus' - The status of the database activity stream.
--
-- 'performanceInsightsEnabled', 'dbInstance_performanceInsightsEnabled' - True if Performance Insights is enabled for the DB instance, and
-- otherwise false.
--
-- 'allocatedStorage', 'dbInstance_allocatedStorage' - Specifies the allocated storage size specified in gibibytes (GiB).
--
-- 'dbiResourceId', 'dbInstance_dbiResourceId' - The Amazon Web Services Region-unique, immutable identifier for the DB
-- instance. This identifier is found in Amazon Web Services CloudTrail log
-- entries whenever the Amazon Web Services KMS customer master key (CMK)
-- for the DB instance is accessed.
--
-- 'dbParameterGroups', 'dbInstance_dbParameterGroups' - Provides the list of DB parameter groups applied to this DB instance.
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
-- 'timezone', 'dbInstance_timezone' - The time zone of the DB instance. In most cases, the @Timezone@ element
-- is empty. @Timezone@ content appears only for Microsoft SQL Server DB
-- instances that were created with a time zone specified.
--
-- 'tdeCredentialArn', 'dbInstance_tdeCredentialArn' - The ARN from the key store with which the instance is associated for TDE
-- encryption.
--
-- 'dbInstanceAutomatedBackupsReplications', 'dbInstance_dbInstanceAutomatedBackupsReplications' - The list of replicated automated backups associated with the DB
-- instance.
--
-- 'endpoint', 'dbInstance_endpoint' - Specifies the connection endpoint.
--
-- The endpoint might not be shown for instances whose status is
-- @creating@.
--
-- 'dbInstanceStatus', 'dbInstance_dbInstanceStatus' - Specifies the current state of this database.
--
-- For information about DB instance statuses, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/accessing-monitoring.html#Overview.DBInstance.Status Viewing DB instance status>
-- in the /Amazon RDS User Guide./
--
-- 'dbInstancePort', 'dbInstance_dbInstancePort' - Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
--
-- 'activityStreamKmsKeyId', 'dbInstance_activityStreamKmsKeyId' - The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream. The Amazon Web Services KMS key
-- identifier is the key ARN, key ID, alias ARN, or alias name for the
-- Amazon Web Services KMS customer master key (CMK).
--
-- 'pendingModifiedValues', 'dbInstance_pendingModifiedValues' - A value that specifies that changes to the DB instance are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
--
-- 'readReplicaDBClusterIdentifiers', 'dbInstance_readReplicaDBClusterIdentifiers' - Contains one or more identifiers of Aurora DB clusters to which the RDS
-- DB instance is replicated as a read replica. For example, when you
-- create an Aurora read replica of an RDS MySQL DB instance, the Aurora
-- MySQL DB cluster for the Aurora read replica is shown. This output does
-- not contain information about cross region Aurora read replicas.
--
-- Currently, each RDS DB instance can have only one Aurora read replica.
--
-- 'storageType', 'dbInstance_storageType' - Specifies the storage type associated with DB instance.
--
-- 'statusInfos', 'dbInstance_statusInfos' - The status of a read replica. If the instance isn\'t a read replica,
-- this is blank.
--
-- 'domainMemberships', 'dbInstance_domainMemberships' - The Active Directory Domain membership records associated with the DB
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
newDBInstance ::
  DBInstance
newDBInstance =
  DBInstance'
    { engineVersion = Prelude.Nothing,
      dbSecurityGroups = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      automaticRestartTime = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      dbInstanceArn = Prelude.Nothing,
      activityStreamKinesisStreamName = Prelude.Nothing,
      activityStreamEngineNativeAuditFieldsIncluded =
        Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      readReplicaDBInstanceIdentifiers = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled = Prelude.Nothing,
      monitoringRoleArn = Prelude.Nothing,
      iops = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      tagList = Prelude.Nothing,
      readReplicaSourceDBInstanceIdentifier =
        Prelude.Nothing,
      replicaMode = Prelude.Nothing,
      monitoringInterval = Prelude.Nothing,
      engine = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      latestRestorableTime = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      promotionTier = Prelude.Nothing,
      awsBackupRecoveryPointArn = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      performanceInsightsRetentionPeriod = Prelude.Nothing,
      cACertificateIdentifier = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      characterSetName = Prelude.Nothing,
      maxAllocatedStorage = Prelude.Nothing,
      customerOwnedIpEnabled = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      associatedRoles = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      ncharCharacterSetName = Prelude.Nothing,
      performanceInsightsKMSKeyId = Prelude.Nothing,
      dbSubnetGroup = Prelude.Nothing,
      activityStreamMode = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      listenerEndpoint = Prelude.Nothing,
      optionGroupMemberships = Prelude.Nothing,
      enabledCloudwatchLogsExports = Prelude.Nothing,
      enhancedMonitoringResourceArn = Prelude.Nothing,
      secondaryAvailabilityZone = Prelude.Nothing,
      activityStreamStatus = Prelude.Nothing,
      performanceInsightsEnabled = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      dbParameterGroups = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      timezone = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      dbInstanceAutomatedBackupsReplications =
        Prelude.Nothing,
      endpoint = Prelude.Nothing,
      dbInstanceStatus = Prelude.Nothing,
      dbInstancePort = Prelude.Nothing,
      activityStreamKmsKeyId = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      readReplicaDBClusterIdentifiers = Prelude.Nothing,
      storageType = Prelude.Nothing,
      statusInfos = Prelude.Nothing,
      domainMemberships = Prelude.Nothing,
      dbName = Prelude.Nothing
    }

-- | Indicates the database engine version.
dbInstance_engineVersion :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_engineVersion = Lens.lens (\DBInstance' {engineVersion} -> engineVersion) (\s@DBInstance' {} a -> s {engineVersion = a} :: DBInstance)

-- | A list of DB security group elements containing @DBSecurityGroup.Name@
-- and @DBSecurityGroup.Status@ subelements.
dbInstance_dbSecurityGroups :: Lens.Lens' DBInstance (Prelude.Maybe [DBSecurityGroupMembership])
dbInstance_dbSecurityGroups = Lens.lens (\DBInstance' {dbSecurityGroups} -> dbSecurityGroups) (\s@DBInstance' {} a -> s {dbSecurityGroups = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Indicates if the DB instance has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
dbInstance_deletionProtection :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_deletionProtection = Lens.lens (\DBInstance' {deletionProtection} -> deletionProtection) (\s@DBInstance' {} a -> s {deletionProtection = a} :: DBInstance)

-- | The time when a stopped DB instance is restarted automatically.
dbInstance_automaticRestartTime :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.UTCTime)
dbInstance_automaticRestartTime = Lens.lens (\DBInstance' {automaticRestartTime} -> automaticRestartTime) (\s@DBInstance' {} a -> s {automaticRestartTime = a} :: DBInstance) Prelude.. Lens.mapping Core._Time

-- | Specifies whether the DB instance is encrypted.
dbInstance_storageEncrypted :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_storageEncrypted = Lens.lens (\DBInstance' {storageEncrypted} -> storageEncrypted) (\s@DBInstance' {} a -> s {storageEncrypted = a} :: DBInstance)

-- | If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
dbInstance_dbClusterIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbClusterIdentifier = Lens.lens (\DBInstance' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBInstance' {} a -> s {dbClusterIdentifier = a} :: DBInstance)

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
dbInstance_publiclyAccessible :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_publiclyAccessible = Lens.lens (\DBInstance' {publiclyAccessible} -> publiclyAccessible) (\s@DBInstance' {} a -> s {publiclyAccessible = a} :: DBInstance)

-- | A value that indicates that minor version patches are applied
-- automatically.
dbInstance_autoMinorVersionUpgrade :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_autoMinorVersionUpgrade = Lens.lens (\DBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@DBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: DBInstance)

-- | The Amazon Resource Name (ARN) for the DB instance.
dbInstance_dbInstanceArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceArn = Lens.lens (\DBInstance' {dbInstanceArn} -> dbInstanceArn) (\s@DBInstance' {} a -> s {dbInstanceArn = a} :: DBInstance)

-- | The name of the Amazon Kinesis data stream used for the database
-- activity stream.
dbInstance_activityStreamKinesisStreamName :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_activityStreamKinesisStreamName = Lens.lens (\DBInstance' {activityStreamKinesisStreamName} -> activityStreamKinesisStreamName) (\s@DBInstance' {} a -> s {activityStreamKinesisStreamName = a} :: DBInstance)

-- | Indicates whether engine-native audit fields are included in the
-- database activity stream.
dbInstance_activityStreamEngineNativeAuditFieldsIncluded :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_activityStreamEngineNativeAuditFieldsIncluded = Lens.lens (\DBInstance' {activityStreamEngineNativeAuditFieldsIncluded} -> activityStreamEngineNativeAuditFieldsIncluded) (\s@DBInstance' {} a -> s {activityStreamEngineNativeAuditFieldsIncluded = a} :: DBInstance)

-- | Contains the master username for the DB instance.
dbInstance_masterUsername :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_masterUsername = Lens.lens (\DBInstance' {masterUsername} -> masterUsername) (\s@DBInstance' {} a -> s {masterUsername = a} :: DBInstance)

-- | Contains one or more identifiers of the read replicas associated with
-- this DB instance.
dbInstance_readReplicaDBInstanceIdentifiers :: Lens.Lens' DBInstance (Prelude.Maybe [Prelude.Text])
dbInstance_readReplicaDBInstanceIdentifiers = Lens.lens (\DBInstance' {readReplicaDBInstanceIdentifiers} -> readReplicaDBInstanceIdentifiers) (\s@DBInstance' {} a -> s {readReplicaDBInstanceIdentifiers = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
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
dbInstance_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBInstance' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBInstance' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBInstance)

-- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring
-- metrics to Amazon CloudWatch Logs.
dbInstance_monitoringRoleArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_monitoringRoleArn = Lens.lens (\DBInstance' {monitoringRoleArn} -> monitoringRoleArn) (\s@DBInstance' {} a -> s {monitoringRoleArn = a} :: DBInstance)

-- | Specifies the Provisioned IOPS (I\/O operations per second) value.
dbInstance_iops :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_iops = Lens.lens (\DBInstance' {iops} -> iops) (\s@DBInstance' {} a -> s {iops = a} :: DBInstance)

-- | Provides the date and time the DB instance was created.
dbInstance_instanceCreateTime :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.UTCTime)
dbInstance_instanceCreateTime = Lens.lens (\DBInstance' {instanceCreateTime} -> instanceCreateTime) (\s@DBInstance' {} a -> s {instanceCreateTime = a} :: DBInstance) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
dbInstance_tagList :: Lens.Lens' DBInstance (Prelude.Maybe [Tag])
dbInstance_tagList = Lens.lens (\DBInstance' {tagList} -> tagList) (\s@DBInstance' {} a -> s {tagList = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Contains the identifier of the source DB instance if this DB instance is
-- a read replica.
dbInstance_readReplicaSourceDBInstanceIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_readReplicaSourceDBInstanceIdentifier = Lens.lens (\DBInstance' {readReplicaSourceDBInstanceIdentifier} -> readReplicaSourceDBInstanceIdentifier) (\s@DBInstance' {} a -> s {readReplicaSourceDBInstanceIdentifier = a} :: DBInstance)

-- | The open mode of an Oracle read replica. The default is
-- @open-read-only@. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
-- in the /Amazon RDS User Guide/.
--
-- This attribute is only supported in RDS for Oracle.
dbInstance_replicaMode :: Lens.Lens' DBInstance (Prelude.Maybe ReplicaMode)
dbInstance_replicaMode = Lens.lens (\DBInstance' {replicaMode} -> replicaMode) (\s@DBInstance' {} a -> s {replicaMode = a} :: DBInstance)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance.
dbInstance_monitoringInterval :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_monitoringInterval = Lens.lens (\DBInstance' {monitoringInterval} -> monitoringInterval) (\s@DBInstance' {} a -> s {monitoringInterval = a} :: DBInstance)

-- | The name of the database engine to be used for this DB instance.
dbInstance_engine :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_engine = Lens.lens (\DBInstance' {engine} -> engine) (\s@DBInstance' {} a -> s {engine = a} :: DBInstance)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
dbInstance_processorFeatures :: Lens.Lens' DBInstance (Prelude.Maybe [ProcessorFeature])
dbInstance_processorFeatures = Lens.lens (\DBInstance' {processorFeatures} -> processorFeatures) (\s@DBInstance' {} a -> s {processorFeatures = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbInstance_latestRestorableTime :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.UTCTime)
dbInstance_latestRestorableTime = Lens.lens (\DBInstance' {latestRestorableTime} -> latestRestorableTime) (\s@DBInstance' {} a -> s {latestRestorableTime = a} :: DBInstance) Prelude.. Lens.mapping Core._Time

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
dbInstance_dbInstanceClass :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceClass = Lens.lens (\DBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@DBInstance' {} a -> s {dbInstanceClass = a} :: DBInstance)

-- | A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
dbInstance_promotionTier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_promotionTier = Lens.lens (\DBInstance' {promotionTier} -> promotionTier) (\s@DBInstance' {} a -> s {promotionTier = a} :: DBInstance)

-- | The Amazon Resource Name (ARN) of the recovery point in Amazon Web
-- Services Backup.
dbInstance_awsBackupRecoveryPointArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_awsBackupRecoveryPointArn = Lens.lens (\DBInstance' {awsBackupRecoveryPointArn} -> awsBackupRecoveryPointArn) (\s@DBInstance' {} a -> s {awsBackupRecoveryPointArn = a} :: DBInstance)

-- | License model information for this DB instance.
dbInstance_licenseModel :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_licenseModel = Lens.lens (\DBInstance' {licenseModel} -> licenseModel) (\s@DBInstance' {} a -> s {licenseModel = a} :: DBInstance)

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
dbInstance_preferredMaintenanceWindow :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_preferredMaintenanceWindow = Lens.lens (\DBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@DBInstance' {} a -> s {preferredMaintenanceWindow = a} :: DBInstance)

-- | The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
dbInstance_performanceInsightsRetentionPeriod :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_performanceInsightsRetentionPeriod = Lens.lens (\DBInstance' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@DBInstance' {} a -> s {performanceInsightsRetentionPeriod = a} :: DBInstance)

-- | The identifier of the CA certificate for this DB instance.
dbInstance_cACertificateIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_cACertificateIdentifier = Lens.lens (\DBInstance' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@DBInstance' {} a -> s {cACertificateIdentifier = a} :: DBInstance)

-- | Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
dbInstance_dbInstanceIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceIdentifier = Lens.lens (\DBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBInstance' {} a -> s {dbInstanceIdentifier = a} :: DBInstance)

-- | If present, specifies the name of the character set that this instance
-- is associated with.
dbInstance_characterSetName :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_characterSetName = Lens.lens (\DBInstance' {characterSetName} -> characterSetName) (\s@DBInstance' {} a -> s {characterSetName = a} :: DBInstance)

-- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
dbInstance_maxAllocatedStorage :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_maxAllocatedStorage = Lens.lens (\DBInstance' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@DBInstance' {} a -> s {maxAllocatedStorage = a} :: DBInstance)

-- | Specifies whether a customer-owned IP address (CoIP) is enabled for an
-- RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
dbInstance_customerOwnedIpEnabled :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_customerOwnedIpEnabled = Lens.lens (\DBInstance' {customerOwnedIpEnabled} -> customerOwnedIpEnabled) (\s@DBInstance' {} a -> s {customerOwnedIpEnabled = a} :: DBInstance)

-- | If @StorageEncrypted@ is true, the Amazon Web Services KMS key
-- identifier for the encrypted DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
dbInstance_kmsKeyId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_kmsKeyId = Lens.lens (\DBInstance' {kmsKeyId} -> kmsKeyId) (\s@DBInstance' {} a -> s {kmsKeyId = a} :: DBInstance)

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
dbInstance_preferredBackupWindow :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_preferredBackupWindow = Lens.lens (\DBInstance' {preferredBackupWindow} -> preferredBackupWindow) (\s@DBInstance' {} a -> s {preferredBackupWindow = a} :: DBInstance)

-- | The Amazon Web Services Identity and Access Management (IAM) roles
-- associated with the DB instance.
dbInstance_associatedRoles :: Lens.Lens' DBInstance (Prelude.Maybe [DBInstanceRole])
dbInstance_associatedRoles = Lens.lens (\DBInstance' {associatedRoles} -> associatedRoles) (\s@DBInstance' {} a -> s {associatedRoles = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the Availability Zone the DB instance is located
-- in.
dbInstance_availabilityZone :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_availabilityZone = Lens.lens (\DBInstance' {availabilityZone} -> availabilityZone) (\s@DBInstance' {} a -> s {availabilityZone = a} :: DBInstance)

-- | Provides a list of VPC security group elements that the DB instance
-- belongs to.
dbInstance_vpcSecurityGroups :: Lens.Lens' DBInstance (Prelude.Maybe [VpcSecurityGroupMembership])
dbInstance_vpcSecurityGroups = Lens.lens (\DBInstance' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@DBInstance' {} a -> s {vpcSecurityGroups = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
dbInstance_backupRetentionPeriod :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_backupRetentionPeriod = Lens.lens (\DBInstance' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBInstance' {} a -> s {backupRetentionPeriod = a} :: DBInstance)

-- | The name of the NCHAR character set for the Oracle DB instance. This
-- character set specifies the Unicode encoding for data stored in table
-- columns of type NCHAR, NCLOB, or NVARCHAR2.
dbInstance_ncharCharacterSetName :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_ncharCharacterSetName = Lens.lens (\DBInstance' {ncharCharacterSetName} -> ncharCharacterSetName) (\s@DBInstance' {} a -> s {ncharCharacterSetName = a} :: DBInstance)

-- | The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
dbInstance_performanceInsightsKMSKeyId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_performanceInsightsKMSKeyId = Lens.lens (\DBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@DBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: DBInstance)

-- | Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
dbInstance_dbSubnetGroup :: Lens.Lens' DBInstance (Prelude.Maybe DBSubnetGroup)
dbInstance_dbSubnetGroup = Lens.lens (\DBInstance' {dbSubnetGroup} -> dbSubnetGroup) (\s@DBInstance' {} a -> s {dbSubnetGroup = a} :: DBInstance)

-- | The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. RDS for Oracle
-- always handles these events asynchronously.
dbInstance_activityStreamMode :: Lens.Lens' DBInstance (Prelude.Maybe ActivityStreamMode)
dbInstance_activityStreamMode = Lens.lens (\DBInstance' {activityStreamMode} -> activityStreamMode) (\s@DBInstance' {} a -> s {activityStreamMode = a} :: DBInstance)

-- | Specifies if the DB instance is a Multi-AZ deployment.
dbInstance_multiAZ :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_multiAZ = Lens.lens (\DBInstance' {multiAZ} -> multiAZ) (\s@DBInstance' {} a -> s {multiAZ = a} :: DBInstance)

-- | Specifies the listener connection endpoint for SQL Server Always On.
dbInstance_listenerEndpoint :: Lens.Lens' DBInstance (Prelude.Maybe Endpoint)
dbInstance_listenerEndpoint = Lens.lens (\DBInstance' {listenerEndpoint} -> listenerEndpoint) (\s@DBInstance' {} a -> s {listenerEndpoint = a} :: DBInstance)

-- | Provides the list of option group memberships for this DB instance.
dbInstance_optionGroupMemberships :: Lens.Lens' DBInstance (Prelude.Maybe [OptionGroupMembership])
dbInstance_optionGroupMemberships = Lens.lens (\DBInstance' {optionGroupMemberships} -> optionGroupMemberships) (\s@DBInstance' {} a -> s {optionGroupMemberships = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | A list of log types that this DB instance is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon RDS User Guide./
dbInstance_enabledCloudwatchLogsExports :: Lens.Lens' DBInstance (Prelude.Maybe [Prelude.Text])
dbInstance_enabledCloudwatchLogsExports = Lens.lens (\DBInstance' {enabledCloudwatchLogsExports} -> enabledCloudwatchLogsExports) (\s@DBInstance' {} a -> s {enabledCloudwatchLogsExports = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
-- that receives the Enhanced Monitoring metrics data for the DB instance.
dbInstance_enhancedMonitoringResourceArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_enhancedMonitoringResourceArn = Lens.lens (\DBInstance' {enhancedMonitoringResourceArn} -> enhancedMonitoringResourceArn) (\s@DBInstance' {} a -> s {enhancedMonitoringResourceArn = a} :: DBInstance)

-- | If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
dbInstance_secondaryAvailabilityZone :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_secondaryAvailabilityZone = Lens.lens (\DBInstance' {secondaryAvailabilityZone} -> secondaryAvailabilityZone) (\s@DBInstance' {} a -> s {secondaryAvailabilityZone = a} :: DBInstance)

-- | The status of the database activity stream.
dbInstance_activityStreamStatus :: Lens.Lens' DBInstance (Prelude.Maybe ActivityStreamStatus)
dbInstance_activityStreamStatus = Lens.lens (\DBInstance' {activityStreamStatus} -> activityStreamStatus) (\s@DBInstance' {} a -> s {activityStreamStatus = a} :: DBInstance)

-- | True if Performance Insights is enabled for the DB instance, and
-- otherwise false.
dbInstance_performanceInsightsEnabled :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_performanceInsightsEnabled = Lens.lens (\DBInstance' {performanceInsightsEnabled} -> performanceInsightsEnabled) (\s@DBInstance' {} a -> s {performanceInsightsEnabled = a} :: DBInstance)

-- | Specifies the allocated storage size specified in gibibytes (GiB).
dbInstance_allocatedStorage :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_allocatedStorage = Lens.lens (\DBInstance' {allocatedStorage} -> allocatedStorage) (\s@DBInstance' {} a -> s {allocatedStorage = a} :: DBInstance)

-- | The Amazon Web Services Region-unique, immutable identifier for the DB
-- instance. This identifier is found in Amazon Web Services CloudTrail log
-- entries whenever the Amazon Web Services KMS customer master key (CMK)
-- for the DB instance is accessed.
dbInstance_dbiResourceId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbiResourceId = Lens.lens (\DBInstance' {dbiResourceId} -> dbiResourceId) (\s@DBInstance' {} a -> s {dbiResourceId = a} :: DBInstance)

-- | Provides the list of DB parameter groups applied to this DB instance.
dbInstance_dbParameterGroups :: Lens.Lens' DBInstance (Prelude.Maybe [DBParameterGroupStatus])
dbInstance_dbParameterGroups = Lens.lens (\DBInstance' {dbParameterGroups} -> dbParameterGroups) (\s@DBInstance' {} a -> s {dbParameterGroups = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether tags are copied from the DB instance to snapshots of
-- the DB instance.
--
-- __Amazon Aurora__
--
-- Not applicable. Copying tags to snapshots is managed by the DB cluster.
-- Setting this value for an Aurora DB instance has no effect on the DB
-- cluster setting. For more information, see @DBCluster@.
dbInstance_copyTagsToSnapshot :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_copyTagsToSnapshot = Lens.lens (\DBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@DBInstance' {} a -> s {copyTagsToSnapshot = a} :: DBInstance)

-- | The time zone of the DB instance. In most cases, the @Timezone@ element
-- is empty. @Timezone@ content appears only for Microsoft SQL Server DB
-- instances that were created with a time zone specified.
dbInstance_timezone :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_timezone = Lens.lens (\DBInstance' {timezone} -> timezone) (\s@DBInstance' {} a -> s {timezone = a} :: DBInstance)

-- | The ARN from the key store with which the instance is associated for TDE
-- encryption.
dbInstance_tdeCredentialArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_tdeCredentialArn = Lens.lens (\DBInstance' {tdeCredentialArn} -> tdeCredentialArn) (\s@DBInstance' {} a -> s {tdeCredentialArn = a} :: DBInstance)

-- | The list of replicated automated backups associated with the DB
-- instance.
dbInstance_dbInstanceAutomatedBackupsReplications :: Lens.Lens' DBInstance (Prelude.Maybe [DBInstanceAutomatedBackupsReplication])
dbInstance_dbInstanceAutomatedBackupsReplications = Lens.lens (\DBInstance' {dbInstanceAutomatedBackupsReplications} -> dbInstanceAutomatedBackupsReplications) (\s@DBInstance' {} a -> s {dbInstanceAutomatedBackupsReplications = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the connection endpoint.
--
-- The endpoint might not be shown for instances whose status is
-- @creating@.
dbInstance_endpoint :: Lens.Lens' DBInstance (Prelude.Maybe Endpoint)
dbInstance_endpoint = Lens.lens (\DBInstance' {endpoint} -> endpoint) (\s@DBInstance' {} a -> s {endpoint = a} :: DBInstance)

-- | Specifies the current state of this database.
--
-- For information about DB instance statuses, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/accessing-monitoring.html#Overview.DBInstance.Status Viewing DB instance status>
-- in the /Amazon RDS User Guide./
dbInstance_dbInstanceStatus :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceStatus = Lens.lens (\DBInstance' {dbInstanceStatus} -> dbInstanceStatus) (\s@DBInstance' {} a -> s {dbInstanceStatus = a} :: DBInstance)

-- | Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
dbInstance_dbInstancePort :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_dbInstancePort = Lens.lens (\DBInstance' {dbInstancePort} -> dbInstancePort) (\s@DBInstance' {} a -> s {dbInstancePort = a} :: DBInstance)

-- | The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream. The Amazon Web Services KMS key
-- identifier is the key ARN, key ID, alias ARN, or alias name for the
-- Amazon Web Services KMS customer master key (CMK).
dbInstance_activityStreamKmsKeyId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_activityStreamKmsKeyId = Lens.lens (\DBInstance' {activityStreamKmsKeyId} -> activityStreamKmsKeyId) (\s@DBInstance' {} a -> s {activityStreamKmsKeyId = a} :: DBInstance)

-- | A value that specifies that changes to the DB instance are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
dbInstance_pendingModifiedValues :: Lens.Lens' DBInstance (Prelude.Maybe PendingModifiedValues)
dbInstance_pendingModifiedValues = Lens.lens (\DBInstance' {pendingModifiedValues} -> pendingModifiedValues) (\s@DBInstance' {} a -> s {pendingModifiedValues = a} :: DBInstance)

-- | Contains one or more identifiers of Aurora DB clusters to which the RDS
-- DB instance is replicated as a read replica. For example, when you
-- create an Aurora read replica of an RDS MySQL DB instance, the Aurora
-- MySQL DB cluster for the Aurora read replica is shown. This output does
-- not contain information about cross region Aurora read replicas.
--
-- Currently, each RDS DB instance can have only one Aurora read replica.
dbInstance_readReplicaDBClusterIdentifiers :: Lens.Lens' DBInstance (Prelude.Maybe [Prelude.Text])
dbInstance_readReplicaDBClusterIdentifiers = Lens.lens (\DBInstance' {readReplicaDBClusterIdentifiers} -> readReplicaDBClusterIdentifiers) (\s@DBInstance' {} a -> s {readReplicaDBClusterIdentifiers = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the storage type associated with DB instance.
dbInstance_storageType :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_storageType = Lens.lens (\DBInstance' {storageType} -> storageType) (\s@DBInstance' {} a -> s {storageType = a} :: DBInstance)

-- | The status of a read replica. If the instance isn\'t a read replica,
-- this is blank.
dbInstance_statusInfos :: Lens.Lens' DBInstance (Prelude.Maybe [DBInstanceStatusInfo])
dbInstance_statusInfos = Lens.lens (\DBInstance' {statusInfos} -> statusInfos) (\s@DBInstance' {} a -> s {statusInfos = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The Active Directory Domain membership records associated with the DB
-- instance.
dbInstance_domainMemberships :: Lens.Lens' DBInstance (Prelude.Maybe [DomainMembership])
dbInstance_domainMemberships = Lens.lens (\DBInstance' {domainMemberships} -> domainMemberships) (\s@DBInstance' {} a -> s {domainMemberships = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

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
dbInstance_dbName :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbName = Lens.lens (\DBInstance' {dbName} -> dbName) (\s@DBInstance' {} a -> s {dbName = a} :: DBInstance)

instance Core.FromXML DBInstance where
  parseXML x =
    DBInstance'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> ( x Core..@? "DBSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBSecurityGroup")
                  )
      Prelude.<*> (x Core..@? "DeletionProtection")
      Prelude.<*> (x Core..@? "AutomaticRestartTime")
      Prelude.<*> (x Core..@? "StorageEncrypted")
      Prelude.<*> (x Core..@? "DBClusterIdentifier")
      Prelude.<*> (x Core..@? "PubliclyAccessible")
      Prelude.<*> (x Core..@? "AutoMinorVersionUpgrade")
      Prelude.<*> (x Core..@? "DBInstanceArn")
      Prelude.<*> (x Core..@? "ActivityStreamKinesisStreamName")
      Prelude.<*> ( x
                      Core..@? "ActivityStreamEngineNativeAuditFieldsIncluded"
                  )
      Prelude.<*> (x Core..@? "MasterUsername")
      Prelude.<*> ( x Core..@? "ReadReplicaDBInstanceIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Core.parseXMLList
                            "ReadReplicaDBInstanceIdentifier"
                        )
                  )
      Prelude.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Core..@? "MonitoringRoleArn")
      Prelude.<*> (x Core..@? "Iops")
      Prelude.<*> (x Core..@? "InstanceCreateTime")
      Prelude.<*> ( x Core..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "ReadReplicaSourceDBInstanceIdentifier")
      Prelude.<*> (x Core..@? "ReplicaMode")
      Prelude.<*> (x Core..@? "MonitoringInterval")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> ( x Core..@? "ProcessorFeatures"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ProcessorFeature")
                  )
      Prelude.<*> (x Core..@? "LatestRestorableTime")
      Prelude.<*> (x Core..@? "DBInstanceClass")
      Prelude.<*> (x Core..@? "PromotionTier")
      Prelude.<*> (x Core..@? "AwsBackupRecoveryPointArn")
      Prelude.<*> (x Core..@? "LicenseModel")
      Prelude.<*> (x Core..@? "PreferredMaintenanceWindow")
      Prelude.<*> (x Core..@? "PerformanceInsightsRetentionPeriod")
      Prelude.<*> (x Core..@? "CACertificateIdentifier")
      Prelude.<*> (x Core..@? "DBInstanceIdentifier")
      Prelude.<*> (x Core..@? "CharacterSetName")
      Prelude.<*> (x Core..@? "MaxAllocatedStorage")
      Prelude.<*> (x Core..@? "CustomerOwnedIpEnabled")
      Prelude.<*> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "PreferredBackupWindow")
      Prelude.<*> ( x Core..@? "AssociatedRoles" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBInstanceRole")
                  )
      Prelude.<*> (x Core..@? "AvailabilityZone")
      Prelude.<*> ( x Core..@? "VpcSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "VpcSecurityGroupMembership")
                  )
      Prelude.<*> (x Core..@? "BackupRetentionPeriod")
      Prelude.<*> (x Core..@? "NcharCharacterSetName")
      Prelude.<*> (x Core..@? "PerformanceInsightsKMSKeyId")
      Prelude.<*> (x Core..@? "DBSubnetGroup")
      Prelude.<*> (x Core..@? "ActivityStreamMode")
      Prelude.<*> (x Core..@? "MultiAZ")
      Prelude.<*> (x Core..@? "ListenerEndpoint")
      Prelude.<*> ( x Core..@? "OptionGroupMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "OptionGroupMembership")
                  )
      Prelude.<*> ( x Core..@? "EnabledCloudwatchLogsExports"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "EnhancedMonitoringResourceArn")
      Prelude.<*> (x Core..@? "SecondaryAvailabilityZone")
      Prelude.<*> (x Core..@? "ActivityStreamStatus")
      Prelude.<*> (x Core..@? "PerformanceInsightsEnabled")
      Prelude.<*> (x Core..@? "AllocatedStorage")
      Prelude.<*> (x Core..@? "DbiResourceId")
      Prelude.<*> ( x Core..@? "DBParameterGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBParameterGroup")
                  )
      Prelude.<*> (x Core..@? "CopyTagsToSnapshot")
      Prelude.<*> (x Core..@? "Timezone")
      Prelude.<*> (x Core..@? "TdeCredentialArn")
      Prelude.<*> ( x Core..@? "DBInstanceAutomatedBackupsReplications"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Core.parseXMLList
                            "DBInstanceAutomatedBackupsReplication"
                        )
                  )
      Prelude.<*> (x Core..@? "Endpoint")
      Prelude.<*> (x Core..@? "DBInstanceStatus")
      Prelude.<*> (x Core..@? "DbInstancePort")
      Prelude.<*> (x Core..@? "ActivityStreamKmsKeyId")
      Prelude.<*> (x Core..@? "PendingModifiedValues")
      Prelude.<*> ( x Core..@? "ReadReplicaDBClusterIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "ReadReplicaDBClusterIdentifier")
                  )
      Prelude.<*> (x Core..@? "StorageType")
      Prelude.<*> ( x Core..@? "StatusInfos" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DBInstanceStatusInfo")
                  )
      Prelude.<*> ( x Core..@? "DomainMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DomainMembership")
                  )
      Prelude.<*> (x Core..@? "DBName")

instance Prelude.Hashable DBInstance

instance Prelude.NFData DBInstance
