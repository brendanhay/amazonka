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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.ActivityStreamMode
import Amazonka.RDS.Types.ActivityStreamPolicyStatus
import Amazonka.RDS.Types.ActivityStreamStatus
import Amazonka.RDS.Types.AutomationMode
import Amazonka.RDS.Types.CertificateDetails
import Amazonka.RDS.Types.DBInstanceAutomatedBackupsReplication
import Amazonka.RDS.Types.DBInstanceRole
import Amazonka.RDS.Types.DBInstanceStatusInfo
import Amazonka.RDS.Types.DBParameterGroupStatus
import Amazonka.RDS.Types.DBSecurityGroupMembership
import Amazonka.RDS.Types.DBSubnetGroup
import Amazonka.RDS.Types.DomainMembership
import Amazonka.RDS.Types.Endpoint
import Amazonka.RDS.Types.MasterUserSecret
import Amazonka.RDS.Types.OptionGroupMembership
import Amazonka.RDS.Types.PendingModifiedValues
import Amazonka.RDS.Types.ProcessorFeature
import Amazonka.RDS.Types.ReplicaMode
import Amazonka.RDS.Types.Tag
import Amazonka.RDS.Types.VpcSecurityGroupMembership

-- | Contains the details of an Amazon RDS DB instance.
--
-- This data type is used as a response element in the operations
-- @CreateDBInstance@, @CreateDBInstanceReadReplica@, @DeleteDBInstance@,
-- @DescribeDBInstances@, @ModifyDBInstance@, @PromoteReadReplica@,
-- @RebootDBInstance@, @RestoreDBInstanceFromDBSnapshot@,
-- @RestoreDBInstanceFromS3@, @RestoreDBInstanceToPointInTime@,
-- @StartDBInstance@, and @StopDBInstance@.
--
-- /See:/ 'newDBInstance' smart constructor.
data DBInstance = DBInstance'
  { -- | Indicates whether engine-native audit fields are included in the
    -- database activity stream.
    activityStreamEngineNativeAuditFieldsIncluded :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Amazon Kinesis data stream used for the database
    -- activity stream.
    activityStreamKinesisStreamName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier used for encrypting messages
    -- in the database activity stream. The Amazon Web Services KMS key
    -- identifier is the key ARN, key ID, alias ARN, or alias name for the KMS
    -- key.
    activityStreamKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The mode of the database activity stream. Database events such as a
    -- change or access generate an activity stream event. RDS for Oracle
    -- always handles these events asynchronously.
    activityStreamMode :: Prelude.Maybe ActivityStreamMode,
    -- | The status of the policy state of the activity stream.
    activityStreamPolicyStatus :: Prelude.Maybe ActivityStreamPolicyStatus,
    -- | The status of the database activity stream.
    activityStreamStatus :: Prelude.Maybe ActivityStreamStatus,
    -- | Specifies the allocated storage size specified in gibibytes (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services Identity and Access Management (IAM) roles
    -- associated with the DB instance.
    associatedRoles :: Prelude.Maybe [DBInstanceRole],
    -- | A value that indicates that minor version patches are applied
    -- automatically.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The time when a stopped DB instance is restarted automatically.
    automaticRestartTime :: Prelude.Maybe Data.ISO8601,
    -- | The automation mode of the RDS Custom DB instance: @full@ or
    -- @all paused@. If @full@, the DB instance automates monitoring and
    -- instance recovery. If @all paused@, the instance pauses automation for
    -- the duration set by @--resume-full-automation-mode-minutes@.
    automationMode :: Prelude.Maybe AutomationMode,
    -- | Specifies the name of the Availability Zone the DB instance is located
    -- in.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the recovery point in Amazon Web
    -- Services Backup.
    awsBackupRecoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of days for which automatic DB snapshots are
    -- retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Specifies where automated backups and manual snapshots are stored:
    -- Amazon Web Services Outposts or the Amazon Web Services Region.
    backupTarget :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the CA certificate for this DB instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
    -- in the /Amazon RDS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
    -- in the /Amazon Aurora User Guide/.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The details of the DB instance\'s server certificate.
    certificateDetails :: Prelude.Maybe CertificateDetails,
    -- | If present, specifies the name of the character set that this instance
    -- is associated with.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether tags are copied from the DB instance to snapshots of
    -- the DB instance.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. Copying tags to snapshots is managed by the DB cluster.
    -- Setting this value for an Aurora DB instance has no effect on the DB
    -- cluster setting. For more information, see @DBCluster@.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The instance profile associated with the underlying Amazon EC2 instance
    -- of an RDS Custom DB instance. The instance profile must meet the
    -- following requirements:
    --
    -- -   The profile must exist in your account.
    --
    -- -   The profile must have an IAM role that Amazon EC2 has permissions to
    --     assume.
    --
    -- -   The instance profile name and the associated IAM role name must
    --     start with the prefix @AWSRDSCustom@.
    --
    -- For the list of permissions required for the IAM role, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
    -- in the /Amazon RDS User Guide/.
    customIamInstanceProfile :: Prelude.Maybe Prelude.Text,
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
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/routing.html#ip-addressing Customer-owned IP addresses>
    -- in the /Amazon Web Services Outposts User Guide/.
    customerOwnedIpEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If the DB instance is a member of a DB cluster, contains the name of the
    -- DB cluster that the DB instance is a member of.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the DB instance.
    dbInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The list of replicated automated backups associated with the DB
    -- instance.
    dbInstanceAutomatedBackupsReplications :: Prelude.Maybe [DBInstanceAutomatedBackupsReplication],
    -- | Contains the name of the compute and memory capacity class of the DB
    -- instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Contains a user-supplied database identifier. This identifier is the
    -- unique key that identifies a DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the current state of this database.
    --
    -- For information about DB instance statuses, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/accessing-monitoring.html#Overview.DBInstance.Status Viewing DB instance status>
    -- in the /Amazon RDS User Guide./
    dbInstanceStatus :: Prelude.Maybe Prelude.Text,
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
    dbName :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of DB parameter groups applied to this DB instance.
    dbParameterGroups :: Prelude.Maybe [DBParameterGroupStatus],
    -- | A list of DB security group elements containing @DBSecurityGroup.Name@
    -- and @DBSecurityGroup.Status@ subelements.
    dbSecurityGroups :: Prelude.Maybe [DBSecurityGroupMembership],
    -- | Specifies information on the subnet group associated with the DB
    -- instance, including the name, description, and subnets in the subnet
    -- group.
    dbSubnetGroup :: Prelude.Maybe DBSubnetGroup,
    -- | The Oracle system ID (Oracle SID) for a container database (CDB). The
    -- Oracle SID is also the name of the CDB. This setting is valid for RDS
    -- Custom only.
    dbSystemId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the port that the DB instance listens on. If the DB instance
    -- is part of a DB cluster, this can be a different port than the DB
    -- cluster port.
    dbInstancePort :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services Region-unique, immutable identifier for the DB
    -- instance. This identifier is found in Amazon Web Services CloudTrail log
    -- entries whenever the Amazon Web Services KMS key for the DB instance is
    -- accessed.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the DB instance has deletion protection enabled. The
    -- database can\'t be deleted when deletion protection is enabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The Active Directory Domain membership records associated with the DB
    -- instance.
    domainMemberships :: Prelude.Maybe [DomainMembership],
    -- | A list of log types that this DB instance is configured to export to
    -- CloudWatch Logs.
    --
    -- Log types vary by DB engine. For information about the log types for
    -- each DB engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
    -- in the /Amazon RDS User Guide./
    enabledCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the connection endpoint.
    --
    -- The endpoint might not be shown for instances whose status is
    -- @creating@.
    endpoint :: Prelude.Maybe Endpoint,
    -- | The name of the database engine to be used for this DB instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
    -- that receives the Enhanced Monitoring metrics data for the DB instance.
    enhancedMonitoringResourceArn :: Prelude.Maybe Prelude.Text,
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
    -- | Provides the date and time the DB instance was created.
    instanceCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | Specifies the Provisioned IOPS (I\/O operations per second) value.
    iops :: Prelude.Maybe Prelude.Int,
    -- | If @StorageEncrypted@ is true, the Amazon Web Services KMS key
    -- identifier for the encrypted DB instance.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the latest time to which a database can be restored with
    -- point-in-time restore.
    latestRestorableTime :: Prelude.Maybe Data.ISO8601,
    -- | License model information for this DB instance. This setting doesn\'t
    -- apply to RDS Custom.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | Specifies the listener connection endpoint for SQL Server Always On.
    listenerEndpoint :: Prelude.Maybe Endpoint,
    -- | Contains the secret managed by RDS in Amazon Web Services Secrets
    -- Manager for the master user password.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon RDS User Guide./
    masterUserSecret :: Prelude.Maybe MasterUserSecret,
    -- | Contains the master username for the DB instance.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
    -- scale the storage of the DB instance.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance.
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring
    -- metrics to Amazon CloudWatch Logs.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies if the DB instance is a Multi-AZ deployment. This setting
    -- doesn\'t apply to RDS Custom.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The name of the NCHAR character set for the Oracle DB instance. This
    -- character set specifies the Unicode encoding for data stored in table
    -- columns of type NCHAR, NCLOB, or NVARCHAR2.
    ncharCharacterSetName :: Prelude.Maybe Prelude.Text,
    -- | The network type of the DB instance.
    --
    -- Valid values:
    --
    -- -   @IPV4@
    --
    -- -   @DUAL@
    --
    -- The network type is determined by the @DBSubnetGroup@ specified for the
    -- DB instance. A @DBSubnetGroup@ can support only the IPv4 protocol or the
    -- IPv4 and the IPv6 protocols (@DUAL@).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
    -- in the /Amazon RDS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
    -- in the /Amazon Aurora User Guide./
    networkType :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of option group memberships for this DB instance.
    optionGroupMemberships :: Prelude.Maybe [OptionGroupMembership],
    -- | A value that specifies that changes to the DB instance are pending. This
    -- element is only included when changes are pending. Specific changes are
    -- identified by subelements.
    pendingModifiedValues :: Prelude.Maybe PendingModifiedValues,
    -- | True if Performance Insights is enabled for the DB instance, and
    -- otherwise false.
    performanceInsightsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services KMS key identifier for encryption of Performance
    -- Insights data.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
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
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Specifies the daily time range during which automated backups are
    -- created if automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which system maintenance can
    -- occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | A value that specifies the order in which an Aurora Replica is promoted
    -- to the primary instance after a failure of the existing primary
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
    -- in the /Amazon Aurora User Guide/.
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | Specifies the accessibility options for the DB instance.
    --
    -- When the DB cluster is publicly accessible, its Domain Name System (DNS)
    -- endpoint resolves to the private IP address from within the DB
    -- cluster\'s virtual private cloud (VPC). It resolves to the public IP
    -- address from outside of the DB cluster\'s VPC. Access to the DB cluster
    -- is ultimately controlled by the security group it uses. That public
    -- access isn\'t permitted if the security group assigned to the DB cluster
    -- doesn\'t permit it.
    --
    -- When the DB instance isn\'t publicly accessible, it is an internal DB
    -- instance with a DNS name that resolves to a private IP address.
    --
    -- For more information, see CreateDBInstance.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Contains one or more identifiers of Aurora DB clusters to which the RDS
    -- DB instance is replicated as a read replica. For example, when you
    -- create an Aurora read replica of an RDS for MySQL DB instance, the
    -- Aurora MySQL DB cluster for the Aurora read replica is shown. This
    -- output doesn\'t contain information about cross-Region Aurora read
    -- replicas.
    --
    -- Currently, each RDS DB instance can have only one Aurora read replica.
    readReplicaDBClusterIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Contains one or more identifiers of the read replicas associated with
    -- this DB instance.
    readReplicaDBInstanceIdentifiers :: Prelude.Maybe [Prelude.Text],
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
    -- | The number of minutes to pause the automation. When the time period
    -- ends, RDS Custom resumes full automation. The minimum value is 60
    -- (default). The maximum value is 1,440.
    resumeFullAutomationModeTime :: Prelude.Maybe Data.ISO8601,
    -- | If present, specifies the name of the secondary Availability Zone for a
    -- DB instance with multi-AZ support.
    secondaryAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The status of a read replica. If the instance isn\'t a read replica,
    -- this is blank.
    statusInfos :: Prelude.Maybe [DBInstanceStatusInfo],
    -- | Specifies whether the DB instance is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage throughput for the DB instance.
    --
    -- This setting applies only to the @gp3@ storage type.
    storageThroughput :: Prelude.Maybe Prelude.Int,
    -- | Specifies the storage type associated with the DB instance.
    storageType :: Prelude.Maybe Prelude.Text,
    tagList :: Prelude.Maybe [Tag],
    -- | The ARN from the key store with which the instance is associated for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the DB instance. In most cases, the @Timezone@ element
    -- is empty. @Timezone@ content appears only for Microsoft SQL Server DB
    -- instances that were created with a time zone specified.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of VPC security group elements that the DB instance
    -- belongs to.
    vpcSecurityGroups :: Prelude.Maybe [VpcSecurityGroupMembership]
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
-- 'activityStreamEngineNativeAuditFieldsIncluded', 'dbInstance_activityStreamEngineNativeAuditFieldsIncluded' - Indicates whether engine-native audit fields are included in the
-- database activity stream.
--
-- 'activityStreamKinesisStreamName', 'dbInstance_activityStreamKinesisStreamName' - The name of the Amazon Kinesis data stream used for the database
-- activity stream.
--
-- 'activityStreamKmsKeyId', 'dbInstance_activityStreamKmsKeyId' - The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream. The Amazon Web Services KMS key
-- identifier is the key ARN, key ID, alias ARN, or alias name for the KMS
-- key.
--
-- 'activityStreamMode', 'dbInstance_activityStreamMode' - The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. RDS for Oracle
-- always handles these events asynchronously.
--
-- 'activityStreamPolicyStatus', 'dbInstance_activityStreamPolicyStatus' - The status of the policy state of the activity stream.
--
-- 'activityStreamStatus', 'dbInstance_activityStreamStatus' - The status of the database activity stream.
--
-- 'allocatedStorage', 'dbInstance_allocatedStorage' - Specifies the allocated storage size specified in gibibytes (GiB).
--
-- 'associatedRoles', 'dbInstance_associatedRoles' - The Amazon Web Services Identity and Access Management (IAM) roles
-- associated with the DB instance.
--
-- 'autoMinorVersionUpgrade', 'dbInstance_autoMinorVersionUpgrade' - A value that indicates that minor version patches are applied
-- automatically.
--
-- 'automaticRestartTime', 'dbInstance_automaticRestartTime' - The time when a stopped DB instance is restarted automatically.
--
-- 'automationMode', 'dbInstance_automationMode' - The automation mode of the RDS Custom DB instance: @full@ or
-- @all paused@. If @full@, the DB instance automates monitoring and
-- instance recovery. If @all paused@, the instance pauses automation for
-- the duration set by @--resume-full-automation-mode-minutes@.
--
-- 'availabilityZone', 'dbInstance_availabilityZone' - Specifies the name of the Availability Zone the DB instance is located
-- in.
--
-- 'awsBackupRecoveryPointArn', 'dbInstance_awsBackupRecoveryPointArn' - The Amazon Resource Name (ARN) of the recovery point in Amazon Web
-- Services Backup.
--
-- 'backupRetentionPeriod', 'dbInstance_backupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are
-- retained.
--
-- 'backupTarget', 'dbInstance_backupTarget' - Specifies where automated backups and manual snapshots are stored:
-- Amazon Web Services Outposts or the Amazon Web Services Region.
--
-- 'cACertificateIdentifier', 'dbInstance_cACertificateIdentifier' - The identifier of the CA certificate for this DB instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
-- in the /Amazon Aurora User Guide/.
--
-- 'certificateDetails', 'dbInstance_certificateDetails' - The details of the DB instance\'s server certificate.
--
-- 'characterSetName', 'dbInstance_characterSetName' - If present, specifies the name of the character set that this instance
-- is associated with.
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
-- 'customIamInstanceProfile', 'dbInstance_customIamInstanceProfile' - The instance profile associated with the underlying Amazon EC2 instance
-- of an RDS Custom DB instance. The instance profile must meet the
-- following requirements:
--
-- -   The profile must exist in your account.
--
-- -   The profile must have an IAM role that Amazon EC2 has permissions to
--     assume.
--
-- -   The instance profile name and the associated IAM role name must
--     start with the prefix @AWSRDSCustom@.
--
-- For the list of permissions required for the IAM role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
-- in the /Amazon RDS User Guide/.
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
-- <https://docs.aws.amazon.com/outposts/latest/userguide/routing.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
--
-- 'dbClusterIdentifier', 'dbInstance_dbClusterIdentifier' - If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
--
-- 'dbInstanceArn', 'dbInstance_dbInstanceArn' - The Amazon Resource Name (ARN) for the DB instance.
--
-- 'dbInstanceAutomatedBackupsReplications', 'dbInstance_dbInstanceAutomatedBackupsReplications' - The list of replicated automated backups associated with the DB
-- instance.
--
-- 'dbInstanceClass', 'dbInstance_dbInstanceClass' - Contains the name of the compute and memory capacity class of the DB
-- instance.
--
-- 'dbInstanceIdentifier', 'dbInstance_dbInstanceIdentifier' - Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
--
-- 'dbInstanceStatus', 'dbInstance_dbInstanceStatus' - Specifies the current state of this database.
--
-- For information about DB instance statuses, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/accessing-monitoring.html#Overview.DBInstance.Status Viewing DB instance status>
-- in the /Amazon RDS User Guide./
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
-- 'dbParameterGroups', 'dbInstance_dbParameterGroups' - Provides the list of DB parameter groups applied to this DB instance.
--
-- 'dbSecurityGroups', 'dbInstance_dbSecurityGroups' - A list of DB security group elements containing @DBSecurityGroup.Name@
-- and @DBSecurityGroup.Status@ subelements.
--
-- 'dbSubnetGroup', 'dbInstance_dbSubnetGroup' - Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
--
-- 'dbSystemId', 'dbInstance_dbSystemId' - The Oracle system ID (Oracle SID) for a container database (CDB). The
-- Oracle SID is also the name of the CDB. This setting is valid for RDS
-- Custom only.
--
-- 'dbInstancePort', 'dbInstance_dbInstancePort' - Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
--
-- 'dbiResourceId', 'dbInstance_dbiResourceId' - The Amazon Web Services Region-unique, immutable identifier for the DB
-- instance. This identifier is found in Amazon Web Services CloudTrail log
-- entries whenever the Amazon Web Services KMS key for the DB instance is
-- accessed.
--
-- 'deletionProtection', 'dbInstance_deletionProtection' - Indicates if the DB instance has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'domainMemberships', 'dbInstance_domainMemberships' - The Active Directory Domain membership records associated with the DB
-- instance.
--
-- 'enabledCloudwatchLogsExports', 'dbInstance_enabledCloudwatchLogsExports' - A list of log types that this DB instance is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon RDS User Guide./
--
-- 'endpoint', 'dbInstance_endpoint' - Specifies the connection endpoint.
--
-- The endpoint might not be shown for instances whose status is
-- @creating@.
--
-- 'engine', 'dbInstance_engine' - The name of the database engine to be used for this DB instance.
--
-- 'engineVersion', 'dbInstance_engineVersion' - Indicates the database engine version.
--
-- 'enhancedMonitoringResourceArn', 'dbInstance_enhancedMonitoringResourceArn' - The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
-- that receives the Enhanced Monitoring metrics data for the DB instance.
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
-- 'instanceCreateTime', 'dbInstance_instanceCreateTime' - Provides the date and time the DB instance was created.
--
-- 'iops', 'dbInstance_iops' - Specifies the Provisioned IOPS (I\/O operations per second) value.
--
-- 'kmsKeyId', 'dbInstance_kmsKeyId' - If @StorageEncrypted@ is true, the Amazon Web Services KMS key
-- identifier for the encrypted DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- 'latestRestorableTime', 'dbInstance_latestRestorableTime' - Specifies the latest time to which a database can be restored with
-- point-in-time restore.
--
-- 'licenseModel', 'dbInstance_licenseModel' - License model information for this DB instance. This setting doesn\'t
-- apply to RDS Custom.
--
-- 'listenerEndpoint', 'dbInstance_listenerEndpoint' - Specifies the listener connection endpoint for SQL Server Always On.
--
-- 'masterUserSecret', 'dbInstance_masterUserSecret' - Contains the secret managed by RDS in Amazon Web Services Secrets
-- Manager for the master user password.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide./
--
-- 'masterUsername', 'dbInstance_masterUsername' - Contains the master username for the DB instance.
--
-- 'maxAllocatedStorage', 'dbInstance_maxAllocatedStorage' - The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- 'monitoringInterval', 'dbInstance_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance.
--
-- 'monitoringRoleArn', 'dbInstance_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send Enhanced Monitoring
-- metrics to Amazon CloudWatch Logs.
--
-- 'multiAZ', 'dbInstance_multiAZ' - Specifies if the DB instance is a Multi-AZ deployment. This setting
-- doesn\'t apply to RDS Custom.
--
-- 'ncharCharacterSetName', 'dbInstance_ncharCharacterSetName' - The name of the NCHAR character set for the Oracle DB instance. This
-- character set specifies the Unicode encoding for data stored in table
-- columns of type NCHAR, NCLOB, or NVARCHAR2.
--
-- 'networkType', 'dbInstance_networkType' - The network type of the DB instance.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB instance. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon Aurora User Guide./
--
-- 'optionGroupMemberships', 'dbInstance_optionGroupMemberships' - Provides the list of option group memberships for this DB instance.
--
-- 'pendingModifiedValues', 'dbInstance_pendingModifiedValues' - A value that specifies that changes to the DB instance are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
--
-- 'performanceInsightsEnabled', 'dbInstance_performanceInsightsEnabled' - True if Performance Insights is enabled for the DB instance, and
-- otherwise false.
--
-- 'performanceInsightsKMSKeyId', 'dbInstance_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- 'performanceInsightsRetentionPeriod', 'dbInstance_performanceInsightsRetentionPeriod' - The number of days to retain Performance Insights data. The default is 7
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
-- 'preferredBackupWindow', 'dbInstance_preferredBackupWindow' - Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
--
-- 'preferredMaintenanceWindow', 'dbInstance_preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
--
-- 'processorFeatures', 'dbInstance_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'promotionTier', 'dbInstance_promotionTier' - A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- 'publiclyAccessible', 'dbInstance_publiclyAccessible' - Specifies the accessibility options for the DB instance.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access isn\'t permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
--
-- 'readReplicaDBClusterIdentifiers', 'dbInstance_readReplicaDBClusterIdentifiers' - Contains one or more identifiers of Aurora DB clusters to which the RDS
-- DB instance is replicated as a read replica. For example, when you
-- create an Aurora read replica of an RDS for MySQL DB instance, the
-- Aurora MySQL DB cluster for the Aurora read replica is shown. This
-- output doesn\'t contain information about cross-Region Aurora read
-- replicas.
--
-- Currently, each RDS DB instance can have only one Aurora read replica.
--
-- 'readReplicaDBInstanceIdentifiers', 'dbInstance_readReplicaDBInstanceIdentifiers' - Contains one or more identifiers of the read replicas associated with
-- this DB instance.
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
-- 'resumeFullAutomationModeTime', 'dbInstance_resumeFullAutomationModeTime' - The number of minutes to pause the automation. When the time period
-- ends, RDS Custom resumes full automation. The minimum value is 60
-- (default). The maximum value is 1,440.
--
-- 'secondaryAvailabilityZone', 'dbInstance_secondaryAvailabilityZone' - If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
--
-- 'statusInfos', 'dbInstance_statusInfos' - The status of a read replica. If the instance isn\'t a read replica,
-- this is blank.
--
-- 'storageEncrypted', 'dbInstance_storageEncrypted' - Specifies whether the DB instance is encrypted.
--
-- 'storageThroughput', 'dbInstance_storageThroughput' - Specifies the storage throughput for the DB instance.
--
-- This setting applies only to the @gp3@ storage type.
--
-- 'storageType', 'dbInstance_storageType' - Specifies the storage type associated with the DB instance.
--
-- 'tagList', 'dbInstance_tagList' - Undocumented member.
--
-- 'tdeCredentialArn', 'dbInstance_tdeCredentialArn' - The ARN from the key store with which the instance is associated for TDE
-- encryption.
--
-- 'timezone', 'dbInstance_timezone' - The time zone of the DB instance. In most cases, the @Timezone@ element
-- is empty. @Timezone@ content appears only for Microsoft SQL Server DB
-- instances that were created with a time zone specified.
--
-- 'vpcSecurityGroups', 'dbInstance_vpcSecurityGroups' - Provides a list of VPC security group elements that the DB instance
-- belongs to.
newDBInstance ::
  DBInstance
newDBInstance =
  DBInstance'
    { activityStreamEngineNativeAuditFieldsIncluded =
        Prelude.Nothing,
      activityStreamKinesisStreamName = Prelude.Nothing,
      activityStreamKmsKeyId = Prelude.Nothing,
      activityStreamMode = Prelude.Nothing,
      activityStreamPolicyStatus = Prelude.Nothing,
      activityStreamStatus = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      associatedRoles = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      automaticRestartTime = Prelude.Nothing,
      automationMode = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      awsBackupRecoveryPointArn = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      backupTarget = Prelude.Nothing,
      cACertificateIdentifier = Prelude.Nothing,
      certificateDetails = Prelude.Nothing,
      characterSetName = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      customIamInstanceProfile = Prelude.Nothing,
      customerOwnedIpEnabled = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      dbInstanceArn = Prelude.Nothing,
      dbInstanceAutomatedBackupsReplications =
        Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbInstanceStatus = Prelude.Nothing,
      dbName = Prelude.Nothing,
      dbParameterGroups = Prelude.Nothing,
      dbSecurityGroups = Prelude.Nothing,
      dbSubnetGroup = Prelude.Nothing,
      dbSystemId = Prelude.Nothing,
      dbInstancePort = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      domainMemberships = Prelude.Nothing,
      enabledCloudwatchLogsExports = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      enhancedMonitoringResourceArn = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      iops = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      latestRestorableTime = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      listenerEndpoint = Prelude.Nothing,
      masterUserSecret = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      maxAllocatedStorage = Prelude.Nothing,
      monitoringInterval = Prelude.Nothing,
      monitoringRoleArn = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      ncharCharacterSetName = Prelude.Nothing,
      networkType = Prelude.Nothing,
      optionGroupMemberships = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      performanceInsightsEnabled = Prelude.Nothing,
      performanceInsightsKMSKeyId = Prelude.Nothing,
      performanceInsightsRetentionPeriod = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      promotionTier = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      readReplicaDBClusterIdentifiers = Prelude.Nothing,
      readReplicaDBInstanceIdentifiers = Prelude.Nothing,
      readReplicaSourceDBInstanceIdentifier =
        Prelude.Nothing,
      replicaMode = Prelude.Nothing,
      resumeFullAutomationModeTime = Prelude.Nothing,
      secondaryAvailabilityZone = Prelude.Nothing,
      statusInfos = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      storageThroughput = Prelude.Nothing,
      storageType = Prelude.Nothing,
      tagList = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      timezone = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing
    }

-- | Indicates whether engine-native audit fields are included in the
-- database activity stream.
dbInstance_activityStreamEngineNativeAuditFieldsIncluded :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_activityStreamEngineNativeAuditFieldsIncluded = Lens.lens (\DBInstance' {activityStreamEngineNativeAuditFieldsIncluded} -> activityStreamEngineNativeAuditFieldsIncluded) (\s@DBInstance' {} a -> s {activityStreamEngineNativeAuditFieldsIncluded = a} :: DBInstance)

-- | The name of the Amazon Kinesis data stream used for the database
-- activity stream.
dbInstance_activityStreamKinesisStreamName :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_activityStreamKinesisStreamName = Lens.lens (\DBInstance' {activityStreamKinesisStreamName} -> activityStreamKinesisStreamName) (\s@DBInstance' {} a -> s {activityStreamKinesisStreamName = a} :: DBInstance)

-- | The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream. The Amazon Web Services KMS key
-- identifier is the key ARN, key ID, alias ARN, or alias name for the KMS
-- key.
dbInstance_activityStreamKmsKeyId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_activityStreamKmsKeyId = Lens.lens (\DBInstance' {activityStreamKmsKeyId} -> activityStreamKmsKeyId) (\s@DBInstance' {} a -> s {activityStreamKmsKeyId = a} :: DBInstance)

-- | The mode of the database activity stream. Database events such as a
-- change or access generate an activity stream event. RDS for Oracle
-- always handles these events asynchronously.
dbInstance_activityStreamMode :: Lens.Lens' DBInstance (Prelude.Maybe ActivityStreamMode)
dbInstance_activityStreamMode = Lens.lens (\DBInstance' {activityStreamMode} -> activityStreamMode) (\s@DBInstance' {} a -> s {activityStreamMode = a} :: DBInstance)

-- | The status of the policy state of the activity stream.
dbInstance_activityStreamPolicyStatus :: Lens.Lens' DBInstance (Prelude.Maybe ActivityStreamPolicyStatus)
dbInstance_activityStreamPolicyStatus = Lens.lens (\DBInstance' {activityStreamPolicyStatus} -> activityStreamPolicyStatus) (\s@DBInstance' {} a -> s {activityStreamPolicyStatus = a} :: DBInstance)

-- | The status of the database activity stream.
dbInstance_activityStreamStatus :: Lens.Lens' DBInstance (Prelude.Maybe ActivityStreamStatus)
dbInstance_activityStreamStatus = Lens.lens (\DBInstance' {activityStreamStatus} -> activityStreamStatus) (\s@DBInstance' {} a -> s {activityStreamStatus = a} :: DBInstance)

-- | Specifies the allocated storage size specified in gibibytes (GiB).
dbInstance_allocatedStorage :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_allocatedStorage = Lens.lens (\DBInstance' {allocatedStorage} -> allocatedStorage) (\s@DBInstance' {} a -> s {allocatedStorage = a} :: DBInstance)

-- | The Amazon Web Services Identity and Access Management (IAM) roles
-- associated with the DB instance.
dbInstance_associatedRoles :: Lens.Lens' DBInstance (Prelude.Maybe [DBInstanceRole])
dbInstance_associatedRoles = Lens.lens (\DBInstance' {associatedRoles} -> associatedRoles) (\s@DBInstance' {} a -> s {associatedRoles = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates that minor version patches are applied
-- automatically.
dbInstance_autoMinorVersionUpgrade :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_autoMinorVersionUpgrade = Lens.lens (\DBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@DBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: DBInstance)

-- | The time when a stopped DB instance is restarted automatically.
dbInstance_automaticRestartTime :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.UTCTime)
dbInstance_automaticRestartTime = Lens.lens (\DBInstance' {automaticRestartTime} -> automaticRestartTime) (\s@DBInstance' {} a -> s {automaticRestartTime = a} :: DBInstance) Prelude.. Lens.mapping Data._Time

-- | The automation mode of the RDS Custom DB instance: @full@ or
-- @all paused@. If @full@, the DB instance automates monitoring and
-- instance recovery. If @all paused@, the instance pauses automation for
-- the duration set by @--resume-full-automation-mode-minutes@.
dbInstance_automationMode :: Lens.Lens' DBInstance (Prelude.Maybe AutomationMode)
dbInstance_automationMode = Lens.lens (\DBInstance' {automationMode} -> automationMode) (\s@DBInstance' {} a -> s {automationMode = a} :: DBInstance)

-- | Specifies the name of the Availability Zone the DB instance is located
-- in.
dbInstance_availabilityZone :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_availabilityZone = Lens.lens (\DBInstance' {availabilityZone} -> availabilityZone) (\s@DBInstance' {} a -> s {availabilityZone = a} :: DBInstance)

-- | The Amazon Resource Name (ARN) of the recovery point in Amazon Web
-- Services Backup.
dbInstance_awsBackupRecoveryPointArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_awsBackupRecoveryPointArn = Lens.lens (\DBInstance' {awsBackupRecoveryPointArn} -> awsBackupRecoveryPointArn) (\s@DBInstance' {} a -> s {awsBackupRecoveryPointArn = a} :: DBInstance)

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
dbInstance_backupRetentionPeriod :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_backupRetentionPeriod = Lens.lens (\DBInstance' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBInstance' {} a -> s {backupRetentionPeriod = a} :: DBInstance)

-- | Specifies where automated backups and manual snapshots are stored:
-- Amazon Web Services Outposts or the Amazon Web Services Region.
dbInstance_backupTarget :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_backupTarget = Lens.lens (\DBInstance' {backupTarget} -> backupTarget) (\s@DBInstance' {} a -> s {backupTarget = a} :: DBInstance)

-- | The identifier of the CA certificate for this DB instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
-- in the /Amazon Aurora User Guide/.
dbInstance_cACertificateIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_cACertificateIdentifier = Lens.lens (\DBInstance' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@DBInstance' {} a -> s {cACertificateIdentifier = a} :: DBInstance)

-- | The details of the DB instance\'s server certificate.
dbInstance_certificateDetails :: Lens.Lens' DBInstance (Prelude.Maybe CertificateDetails)
dbInstance_certificateDetails = Lens.lens (\DBInstance' {certificateDetails} -> certificateDetails) (\s@DBInstance' {} a -> s {certificateDetails = a} :: DBInstance)

-- | If present, specifies the name of the character set that this instance
-- is associated with.
dbInstance_characterSetName :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_characterSetName = Lens.lens (\DBInstance' {characterSetName} -> characterSetName) (\s@DBInstance' {} a -> s {characterSetName = a} :: DBInstance)

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

-- | The instance profile associated with the underlying Amazon EC2 instance
-- of an RDS Custom DB instance. The instance profile must meet the
-- following requirements:
--
-- -   The profile must exist in your account.
--
-- -   The profile must have an IAM role that Amazon EC2 has permissions to
--     assume.
--
-- -   The instance profile name and the associated IAM role name must
--     start with the prefix @AWSRDSCustom@.
--
-- For the list of permissions required for the IAM role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
-- in the /Amazon RDS User Guide/.
dbInstance_customIamInstanceProfile :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_customIamInstanceProfile = Lens.lens (\DBInstance' {customIamInstanceProfile} -> customIamInstanceProfile) (\s@DBInstance' {} a -> s {customIamInstanceProfile = a} :: DBInstance)

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
-- <https://docs.aws.amazon.com/outposts/latest/userguide/routing.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
dbInstance_customerOwnedIpEnabled :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_customerOwnedIpEnabled = Lens.lens (\DBInstance' {customerOwnedIpEnabled} -> customerOwnedIpEnabled) (\s@DBInstance' {} a -> s {customerOwnedIpEnabled = a} :: DBInstance)

-- | If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
dbInstance_dbClusterIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbClusterIdentifier = Lens.lens (\DBInstance' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBInstance' {} a -> s {dbClusterIdentifier = a} :: DBInstance)

-- | The Amazon Resource Name (ARN) for the DB instance.
dbInstance_dbInstanceArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceArn = Lens.lens (\DBInstance' {dbInstanceArn} -> dbInstanceArn) (\s@DBInstance' {} a -> s {dbInstanceArn = a} :: DBInstance)

-- | The list of replicated automated backups associated with the DB
-- instance.
dbInstance_dbInstanceAutomatedBackupsReplications :: Lens.Lens' DBInstance (Prelude.Maybe [DBInstanceAutomatedBackupsReplication])
dbInstance_dbInstanceAutomatedBackupsReplications = Lens.lens (\DBInstance' {dbInstanceAutomatedBackupsReplications} -> dbInstanceAutomatedBackupsReplications) (\s@DBInstance' {} a -> s {dbInstanceAutomatedBackupsReplications = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
dbInstance_dbInstanceClass :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceClass = Lens.lens (\DBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@DBInstance' {} a -> s {dbInstanceClass = a} :: DBInstance)

-- | Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
dbInstance_dbInstanceIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceIdentifier = Lens.lens (\DBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBInstance' {} a -> s {dbInstanceIdentifier = a} :: DBInstance)

-- | Specifies the current state of this database.
--
-- For information about DB instance statuses, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/accessing-monitoring.html#Overview.DBInstance.Status Viewing DB instance status>
-- in the /Amazon RDS User Guide./
dbInstance_dbInstanceStatus :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceStatus = Lens.lens (\DBInstance' {dbInstanceStatus} -> dbInstanceStatus) (\s@DBInstance' {} a -> s {dbInstanceStatus = a} :: DBInstance)

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

-- | Provides the list of DB parameter groups applied to this DB instance.
dbInstance_dbParameterGroups :: Lens.Lens' DBInstance (Prelude.Maybe [DBParameterGroupStatus])
dbInstance_dbParameterGroups = Lens.lens (\DBInstance' {dbParameterGroups} -> dbParameterGroups) (\s@DBInstance' {} a -> s {dbParameterGroups = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | A list of DB security group elements containing @DBSecurityGroup.Name@
-- and @DBSecurityGroup.Status@ subelements.
dbInstance_dbSecurityGroups :: Lens.Lens' DBInstance (Prelude.Maybe [DBSecurityGroupMembership])
dbInstance_dbSecurityGroups = Lens.lens (\DBInstance' {dbSecurityGroups} -> dbSecurityGroups) (\s@DBInstance' {} a -> s {dbSecurityGroups = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
dbInstance_dbSubnetGroup :: Lens.Lens' DBInstance (Prelude.Maybe DBSubnetGroup)
dbInstance_dbSubnetGroup = Lens.lens (\DBInstance' {dbSubnetGroup} -> dbSubnetGroup) (\s@DBInstance' {} a -> s {dbSubnetGroup = a} :: DBInstance)

-- | The Oracle system ID (Oracle SID) for a container database (CDB). The
-- Oracle SID is also the name of the CDB. This setting is valid for RDS
-- Custom only.
dbInstance_dbSystemId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbSystemId = Lens.lens (\DBInstance' {dbSystemId} -> dbSystemId) (\s@DBInstance' {} a -> s {dbSystemId = a} :: DBInstance)

-- | Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
dbInstance_dbInstancePort :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_dbInstancePort = Lens.lens (\DBInstance' {dbInstancePort} -> dbInstancePort) (\s@DBInstance' {} a -> s {dbInstancePort = a} :: DBInstance)

-- | The Amazon Web Services Region-unique, immutable identifier for the DB
-- instance. This identifier is found in Amazon Web Services CloudTrail log
-- entries whenever the Amazon Web Services KMS key for the DB instance is
-- accessed.
dbInstance_dbiResourceId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbiResourceId = Lens.lens (\DBInstance' {dbiResourceId} -> dbiResourceId) (\s@DBInstance' {} a -> s {dbiResourceId = a} :: DBInstance)

-- | Indicates if the DB instance has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
dbInstance_deletionProtection :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_deletionProtection = Lens.lens (\DBInstance' {deletionProtection} -> deletionProtection) (\s@DBInstance' {} a -> s {deletionProtection = a} :: DBInstance)

-- | The Active Directory Domain membership records associated with the DB
-- instance.
dbInstance_domainMemberships :: Lens.Lens' DBInstance (Prelude.Maybe [DomainMembership])
dbInstance_domainMemberships = Lens.lens (\DBInstance' {domainMemberships} -> domainMemberships) (\s@DBInstance' {} a -> s {domainMemberships = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | A list of log types that this DB instance is configured to export to
-- CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for
-- each DB engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html Amazon RDS Database Log Files>
-- in the /Amazon RDS User Guide./
dbInstance_enabledCloudwatchLogsExports :: Lens.Lens' DBInstance (Prelude.Maybe [Prelude.Text])
dbInstance_enabledCloudwatchLogsExports = Lens.lens (\DBInstance' {enabledCloudwatchLogsExports} -> enabledCloudwatchLogsExports) (\s@DBInstance' {} a -> s {enabledCloudwatchLogsExports = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the connection endpoint.
--
-- The endpoint might not be shown for instances whose status is
-- @creating@.
dbInstance_endpoint :: Lens.Lens' DBInstance (Prelude.Maybe Endpoint)
dbInstance_endpoint = Lens.lens (\DBInstance' {endpoint} -> endpoint) (\s@DBInstance' {} a -> s {endpoint = a} :: DBInstance)

-- | The name of the database engine to be used for this DB instance.
dbInstance_engine :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_engine = Lens.lens (\DBInstance' {engine} -> engine) (\s@DBInstance' {} a -> s {engine = a} :: DBInstance)

-- | Indicates the database engine version.
dbInstance_engineVersion :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_engineVersion = Lens.lens (\DBInstance' {engineVersion} -> engineVersion) (\s@DBInstance' {} a -> s {engineVersion = a} :: DBInstance)

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
-- that receives the Enhanced Monitoring metrics data for the DB instance.
dbInstance_enhancedMonitoringResourceArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_enhancedMonitoringResourceArn = Lens.lens (\DBInstance' {enhancedMonitoringResourceArn} -> enhancedMonitoringResourceArn) (\s@DBInstance' {} a -> s {enhancedMonitoringResourceArn = a} :: DBInstance)

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

-- | Provides the date and time the DB instance was created.
dbInstance_instanceCreateTime :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.UTCTime)
dbInstance_instanceCreateTime = Lens.lens (\DBInstance' {instanceCreateTime} -> instanceCreateTime) (\s@DBInstance' {} a -> s {instanceCreateTime = a} :: DBInstance) Prelude.. Lens.mapping Data._Time

-- | Specifies the Provisioned IOPS (I\/O operations per second) value.
dbInstance_iops :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_iops = Lens.lens (\DBInstance' {iops} -> iops) (\s@DBInstance' {} a -> s {iops = a} :: DBInstance)

-- | If @StorageEncrypted@ is true, the Amazon Web Services KMS key
-- identifier for the encrypted DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
dbInstance_kmsKeyId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_kmsKeyId = Lens.lens (\DBInstance' {kmsKeyId} -> kmsKeyId) (\s@DBInstance' {} a -> s {kmsKeyId = a} :: DBInstance)

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbInstance_latestRestorableTime :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.UTCTime)
dbInstance_latestRestorableTime = Lens.lens (\DBInstance' {latestRestorableTime} -> latestRestorableTime) (\s@DBInstance' {} a -> s {latestRestorableTime = a} :: DBInstance) Prelude.. Lens.mapping Data._Time

-- | License model information for this DB instance. This setting doesn\'t
-- apply to RDS Custom.
dbInstance_licenseModel :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_licenseModel = Lens.lens (\DBInstance' {licenseModel} -> licenseModel) (\s@DBInstance' {} a -> s {licenseModel = a} :: DBInstance)

-- | Specifies the listener connection endpoint for SQL Server Always On.
dbInstance_listenerEndpoint :: Lens.Lens' DBInstance (Prelude.Maybe Endpoint)
dbInstance_listenerEndpoint = Lens.lens (\DBInstance' {listenerEndpoint} -> listenerEndpoint) (\s@DBInstance' {} a -> s {listenerEndpoint = a} :: DBInstance)

-- | Contains the secret managed by RDS in Amazon Web Services Secrets
-- Manager for the master user password.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide./
dbInstance_masterUserSecret :: Lens.Lens' DBInstance (Prelude.Maybe MasterUserSecret)
dbInstance_masterUserSecret = Lens.lens (\DBInstance' {masterUserSecret} -> masterUserSecret) (\s@DBInstance' {} a -> s {masterUserSecret = a} :: DBInstance)

-- | Contains the master username for the DB instance.
dbInstance_masterUsername :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_masterUsername = Lens.lens (\DBInstance' {masterUsername} -> masterUsername) (\s@DBInstance' {} a -> s {masterUsername = a} :: DBInstance)

-- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
dbInstance_maxAllocatedStorage :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_maxAllocatedStorage = Lens.lens (\DBInstance' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@DBInstance' {} a -> s {maxAllocatedStorage = a} :: DBInstance)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance.
dbInstance_monitoringInterval :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_monitoringInterval = Lens.lens (\DBInstance' {monitoringInterval} -> monitoringInterval) (\s@DBInstance' {} a -> s {monitoringInterval = a} :: DBInstance)

-- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring
-- metrics to Amazon CloudWatch Logs.
dbInstance_monitoringRoleArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_monitoringRoleArn = Lens.lens (\DBInstance' {monitoringRoleArn} -> monitoringRoleArn) (\s@DBInstance' {} a -> s {monitoringRoleArn = a} :: DBInstance)

-- | Specifies if the DB instance is a Multi-AZ deployment. This setting
-- doesn\'t apply to RDS Custom.
dbInstance_multiAZ :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_multiAZ = Lens.lens (\DBInstance' {multiAZ} -> multiAZ) (\s@DBInstance' {} a -> s {multiAZ = a} :: DBInstance)

-- | The name of the NCHAR character set for the Oracle DB instance. This
-- character set specifies the Unicode encoding for data stored in table
-- columns of type NCHAR, NCLOB, or NVARCHAR2.
dbInstance_ncharCharacterSetName :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_ncharCharacterSetName = Lens.lens (\DBInstance' {ncharCharacterSetName} -> ncharCharacterSetName) (\s@DBInstance' {} a -> s {ncharCharacterSetName = a} :: DBInstance)

-- | The network type of the DB instance.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB instance. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon Aurora User Guide./
dbInstance_networkType :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_networkType = Lens.lens (\DBInstance' {networkType} -> networkType) (\s@DBInstance' {} a -> s {networkType = a} :: DBInstance)

-- | Provides the list of option group memberships for this DB instance.
dbInstance_optionGroupMemberships :: Lens.Lens' DBInstance (Prelude.Maybe [OptionGroupMembership])
dbInstance_optionGroupMemberships = Lens.lens (\DBInstance' {optionGroupMemberships} -> optionGroupMemberships) (\s@DBInstance' {} a -> s {optionGroupMemberships = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | A value that specifies that changes to the DB instance are pending. This
-- element is only included when changes are pending. Specific changes are
-- identified by subelements.
dbInstance_pendingModifiedValues :: Lens.Lens' DBInstance (Prelude.Maybe PendingModifiedValues)
dbInstance_pendingModifiedValues = Lens.lens (\DBInstance' {pendingModifiedValues} -> pendingModifiedValues) (\s@DBInstance' {} a -> s {pendingModifiedValues = a} :: DBInstance)

-- | True if Performance Insights is enabled for the DB instance, and
-- otherwise false.
dbInstance_performanceInsightsEnabled :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_performanceInsightsEnabled = Lens.lens (\DBInstance' {performanceInsightsEnabled} -> performanceInsightsEnabled) (\s@DBInstance' {} a -> s {performanceInsightsEnabled = a} :: DBInstance)

-- | The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
dbInstance_performanceInsightsKMSKeyId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_performanceInsightsKMSKeyId = Lens.lens (\DBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@DBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: DBInstance)

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
dbInstance_performanceInsightsRetentionPeriod :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_performanceInsightsRetentionPeriod = Lens.lens (\DBInstance' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@DBInstance' {} a -> s {performanceInsightsRetentionPeriod = a} :: DBInstance)

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
dbInstance_preferredBackupWindow :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_preferredBackupWindow = Lens.lens (\DBInstance' {preferredBackupWindow} -> preferredBackupWindow) (\s@DBInstance' {} a -> s {preferredBackupWindow = a} :: DBInstance)

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
dbInstance_preferredMaintenanceWindow :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_preferredMaintenanceWindow = Lens.lens (\DBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@DBInstance' {} a -> s {preferredMaintenanceWindow = a} :: DBInstance)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
dbInstance_processorFeatures :: Lens.Lens' DBInstance (Prelude.Maybe [ProcessorFeature])
dbInstance_processorFeatures = Lens.lens (\DBInstance' {processorFeatures} -> processorFeatures) (\s@DBInstance' {} a -> s {processorFeatures = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
dbInstance_promotionTier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_promotionTier = Lens.lens (\DBInstance' {promotionTier} -> promotionTier) (\s@DBInstance' {} a -> s {promotionTier = a} :: DBInstance)

-- | Specifies the accessibility options for the DB instance.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access isn\'t permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
dbInstance_publiclyAccessible :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_publiclyAccessible = Lens.lens (\DBInstance' {publiclyAccessible} -> publiclyAccessible) (\s@DBInstance' {} a -> s {publiclyAccessible = a} :: DBInstance)

-- | Contains one or more identifiers of Aurora DB clusters to which the RDS
-- DB instance is replicated as a read replica. For example, when you
-- create an Aurora read replica of an RDS for MySQL DB instance, the
-- Aurora MySQL DB cluster for the Aurora read replica is shown. This
-- output doesn\'t contain information about cross-Region Aurora read
-- replicas.
--
-- Currently, each RDS DB instance can have only one Aurora read replica.
dbInstance_readReplicaDBClusterIdentifiers :: Lens.Lens' DBInstance (Prelude.Maybe [Prelude.Text])
dbInstance_readReplicaDBClusterIdentifiers = Lens.lens (\DBInstance' {readReplicaDBClusterIdentifiers} -> readReplicaDBClusterIdentifiers) (\s@DBInstance' {} a -> s {readReplicaDBClusterIdentifiers = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Contains one or more identifiers of the read replicas associated with
-- this DB instance.
dbInstance_readReplicaDBInstanceIdentifiers :: Lens.Lens' DBInstance (Prelude.Maybe [Prelude.Text])
dbInstance_readReplicaDBInstanceIdentifiers = Lens.lens (\DBInstance' {readReplicaDBInstanceIdentifiers} -> readReplicaDBInstanceIdentifiers) (\s@DBInstance' {} a -> s {readReplicaDBInstanceIdentifiers = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

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

-- | The number of minutes to pause the automation. When the time period
-- ends, RDS Custom resumes full automation. The minimum value is 60
-- (default). The maximum value is 1,440.
dbInstance_resumeFullAutomationModeTime :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.UTCTime)
dbInstance_resumeFullAutomationModeTime = Lens.lens (\DBInstance' {resumeFullAutomationModeTime} -> resumeFullAutomationModeTime) (\s@DBInstance' {} a -> s {resumeFullAutomationModeTime = a} :: DBInstance) Prelude.. Lens.mapping Data._Time

-- | If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
dbInstance_secondaryAvailabilityZone :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_secondaryAvailabilityZone = Lens.lens (\DBInstance' {secondaryAvailabilityZone} -> secondaryAvailabilityZone) (\s@DBInstance' {} a -> s {secondaryAvailabilityZone = a} :: DBInstance)

-- | The status of a read replica. If the instance isn\'t a read replica,
-- this is blank.
dbInstance_statusInfos :: Lens.Lens' DBInstance (Prelude.Maybe [DBInstanceStatusInfo])
dbInstance_statusInfos = Lens.lens (\DBInstance' {statusInfos} -> statusInfos) (\s@DBInstance' {} a -> s {statusInfos = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the DB instance is encrypted.
dbInstance_storageEncrypted :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_storageEncrypted = Lens.lens (\DBInstance' {storageEncrypted} -> storageEncrypted) (\s@DBInstance' {} a -> s {storageEncrypted = a} :: DBInstance)

-- | Specifies the storage throughput for the DB instance.
--
-- This setting applies only to the @gp3@ storage type.
dbInstance_storageThroughput :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_storageThroughput = Lens.lens (\DBInstance' {storageThroughput} -> storageThroughput) (\s@DBInstance' {} a -> s {storageThroughput = a} :: DBInstance)

-- | Specifies the storage type associated with the DB instance.
dbInstance_storageType :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_storageType = Lens.lens (\DBInstance' {storageType} -> storageType) (\s@DBInstance' {} a -> s {storageType = a} :: DBInstance)

-- | Undocumented member.
dbInstance_tagList :: Lens.Lens' DBInstance (Prelude.Maybe [Tag])
dbInstance_tagList = Lens.lens (\DBInstance' {tagList} -> tagList) (\s@DBInstance' {} a -> s {tagList = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The ARN from the key store with which the instance is associated for TDE
-- encryption.
dbInstance_tdeCredentialArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_tdeCredentialArn = Lens.lens (\DBInstance' {tdeCredentialArn} -> tdeCredentialArn) (\s@DBInstance' {} a -> s {tdeCredentialArn = a} :: DBInstance)

-- | The time zone of the DB instance. In most cases, the @Timezone@ element
-- is empty. @Timezone@ content appears only for Microsoft SQL Server DB
-- instances that were created with a time zone specified.
dbInstance_timezone :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_timezone = Lens.lens (\DBInstance' {timezone} -> timezone) (\s@DBInstance' {} a -> s {timezone = a} :: DBInstance)

-- | Provides a list of VPC security group elements that the DB instance
-- belongs to.
dbInstance_vpcSecurityGroups :: Lens.Lens' DBInstance (Prelude.Maybe [VpcSecurityGroupMembership])
dbInstance_vpcSecurityGroups = Lens.lens (\DBInstance' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@DBInstance' {} a -> s {vpcSecurityGroups = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML DBInstance where
  parseXML x =
    DBInstance'
      Prelude.<$> ( x
                      Data..@? "ActivityStreamEngineNativeAuditFieldsIncluded"
                  )
      Prelude.<*> (x Data..@? "ActivityStreamKinesisStreamName")
      Prelude.<*> (x Data..@? "ActivityStreamKmsKeyId")
      Prelude.<*> (x Data..@? "ActivityStreamMode")
      Prelude.<*> (x Data..@? "ActivityStreamPolicyStatus")
      Prelude.<*> (x Data..@? "ActivityStreamStatus")
      Prelude.<*> (x Data..@? "AllocatedStorage")
      Prelude.<*> ( x Data..@? "AssociatedRoles" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBInstanceRole")
                  )
      Prelude.<*> (x Data..@? "AutoMinorVersionUpgrade")
      Prelude.<*> (x Data..@? "AutomaticRestartTime")
      Prelude.<*> (x Data..@? "AutomationMode")
      Prelude.<*> (x Data..@? "AvailabilityZone")
      Prelude.<*> (x Data..@? "AwsBackupRecoveryPointArn")
      Prelude.<*> (x Data..@? "BackupRetentionPeriod")
      Prelude.<*> (x Data..@? "BackupTarget")
      Prelude.<*> (x Data..@? "CACertificateIdentifier")
      Prelude.<*> (x Data..@? "CertificateDetails")
      Prelude.<*> (x Data..@? "CharacterSetName")
      Prelude.<*> (x Data..@? "CopyTagsToSnapshot")
      Prelude.<*> (x Data..@? "CustomIamInstanceProfile")
      Prelude.<*> (x Data..@? "CustomerOwnedIpEnabled")
      Prelude.<*> (x Data..@? "DBClusterIdentifier")
      Prelude.<*> (x Data..@? "DBInstanceArn")
      Prelude.<*> ( x Data..@? "DBInstanceAutomatedBackupsReplications"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Data.parseXMLList
                            "DBInstanceAutomatedBackupsReplication"
                        )
                  )
      Prelude.<*> (x Data..@? "DBInstanceClass")
      Prelude.<*> (x Data..@? "DBInstanceIdentifier")
      Prelude.<*> (x Data..@? "DBInstanceStatus")
      Prelude.<*> (x Data..@? "DBName")
      Prelude.<*> ( x Data..@? "DBParameterGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBParameterGroup")
                  )
      Prelude.<*> ( x Data..@? "DBSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBSecurityGroup")
                  )
      Prelude.<*> (x Data..@? "DBSubnetGroup")
      Prelude.<*> (x Data..@? "DBSystemId")
      Prelude.<*> (x Data..@? "DbInstancePort")
      Prelude.<*> (x Data..@? "DbiResourceId")
      Prelude.<*> (x Data..@? "DeletionProtection")
      Prelude.<*> ( x Data..@? "DomainMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DomainMembership")
                  )
      Prelude.<*> ( x Data..@? "EnabledCloudwatchLogsExports"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Endpoint")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "EnhancedMonitoringResourceArn")
      Prelude.<*> (x Data..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Data..@? "InstanceCreateTime")
      Prelude.<*> (x Data..@? "Iops")
      Prelude.<*> (x Data..@? "KmsKeyId")
      Prelude.<*> (x Data..@? "LatestRestorableTime")
      Prelude.<*> (x Data..@? "LicenseModel")
      Prelude.<*> (x Data..@? "ListenerEndpoint")
      Prelude.<*> (x Data..@? "MasterUserSecret")
      Prelude.<*> (x Data..@? "MasterUsername")
      Prelude.<*> (x Data..@? "MaxAllocatedStorage")
      Prelude.<*> (x Data..@? "MonitoringInterval")
      Prelude.<*> (x Data..@? "MonitoringRoleArn")
      Prelude.<*> (x Data..@? "MultiAZ")
      Prelude.<*> (x Data..@? "NcharCharacterSetName")
      Prelude.<*> (x Data..@? "NetworkType")
      Prelude.<*> ( x Data..@? "OptionGroupMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "OptionGroupMembership")
                  )
      Prelude.<*> (x Data..@? "PendingModifiedValues")
      Prelude.<*> (x Data..@? "PerformanceInsightsEnabled")
      Prelude.<*> (x Data..@? "PerformanceInsightsKMSKeyId")
      Prelude.<*> (x Data..@? "PerformanceInsightsRetentionPeriod")
      Prelude.<*> (x Data..@? "PreferredBackupWindow")
      Prelude.<*> (x Data..@? "PreferredMaintenanceWindow")
      Prelude.<*> ( x Data..@? "ProcessorFeatures"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "ProcessorFeature")
                  )
      Prelude.<*> (x Data..@? "PromotionTier")
      Prelude.<*> (x Data..@? "PubliclyAccessible")
      Prelude.<*> ( x Data..@? "ReadReplicaDBClusterIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "ReadReplicaDBClusterIdentifier")
                  )
      Prelude.<*> ( x Data..@? "ReadReplicaDBInstanceIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Data.parseXMLList
                            "ReadReplicaDBInstanceIdentifier"
                        )
                  )
      Prelude.<*> (x Data..@? "ReadReplicaSourceDBInstanceIdentifier")
      Prelude.<*> (x Data..@? "ReplicaMode")
      Prelude.<*> (x Data..@? "ResumeFullAutomationModeTime")
      Prelude.<*> (x Data..@? "SecondaryAvailabilityZone")
      Prelude.<*> ( x Data..@? "StatusInfos" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBInstanceStatusInfo")
                  )
      Prelude.<*> (x Data..@? "StorageEncrypted")
      Prelude.<*> (x Data..@? "StorageThroughput")
      Prelude.<*> (x Data..@? "StorageType")
      Prelude.<*> ( x Data..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )
      Prelude.<*> (x Data..@? "TdeCredentialArn")
      Prelude.<*> (x Data..@? "Timezone")
      Prelude.<*> ( x Data..@? "VpcSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "VpcSecurityGroupMembership")
                  )

instance Prelude.Hashable DBInstance where
  hashWithSalt _salt DBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` activityStreamEngineNativeAuditFieldsIncluded
      `Prelude.hashWithSalt` activityStreamKinesisStreamName
      `Prelude.hashWithSalt` activityStreamKmsKeyId
      `Prelude.hashWithSalt` activityStreamMode
      `Prelude.hashWithSalt` activityStreamPolicyStatus
      `Prelude.hashWithSalt` activityStreamStatus
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` associatedRoles
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` automaticRestartTime
      `Prelude.hashWithSalt` automationMode
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` awsBackupRecoveryPointArn
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` backupTarget
      `Prelude.hashWithSalt` cACertificateIdentifier
      `Prelude.hashWithSalt` certificateDetails
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` customIamInstanceProfile
      `Prelude.hashWithSalt` customerOwnedIpEnabled
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` dbInstanceArn
      `Prelude.hashWithSalt` dbInstanceAutomatedBackupsReplications
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbInstanceStatus
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` dbParameterGroups
      `Prelude.hashWithSalt` dbSecurityGroups
      `Prelude.hashWithSalt` dbSubnetGroup
      `Prelude.hashWithSalt` dbSystemId
      `Prelude.hashWithSalt` dbInstancePort
      `Prelude.hashWithSalt` dbiResourceId
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domainMemberships
      `Prelude.hashWithSalt` enabledCloudwatchLogsExports
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` enhancedMonitoringResourceArn
      `Prelude.hashWithSalt` iAMDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` instanceCreateTime
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` latestRestorableTime
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` listenerEndpoint
      `Prelude.hashWithSalt` masterUserSecret
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` maxAllocatedStorage
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` ncharCharacterSetName
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` optionGroupMemberships
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` performanceInsightsEnabled
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` performanceInsightsRetentionPeriod
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` promotionTier
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` readReplicaDBClusterIdentifiers
      `Prelude.hashWithSalt` readReplicaDBInstanceIdentifiers
      `Prelude.hashWithSalt` readReplicaSourceDBInstanceIdentifier
      `Prelude.hashWithSalt` replicaMode
      `Prelude.hashWithSalt` resumeFullAutomationModeTime
      `Prelude.hashWithSalt` secondaryAvailabilityZone
      `Prelude.hashWithSalt` statusInfos
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` storageThroughput
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` tdeCredentialArn
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` vpcSecurityGroups

instance Prelude.NFData DBInstance where
  rnf DBInstance' {..} =
    Prelude.rnf
      activityStreamEngineNativeAuditFieldsIncluded
      `Prelude.seq` Prelude.rnf activityStreamKinesisStreamName
      `Prelude.seq` Prelude.rnf activityStreamKmsKeyId
      `Prelude.seq` Prelude.rnf activityStreamMode
      `Prelude.seq` Prelude.rnf activityStreamPolicyStatus
      `Prelude.seq` Prelude.rnf activityStreamStatus
      `Prelude.seq` Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf associatedRoles
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf automaticRestartTime
      `Prelude.seq` Prelude.rnf automationMode
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf awsBackupRecoveryPointArn
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf backupTarget
      `Prelude.seq` Prelude.rnf cACertificateIdentifier
      `Prelude.seq` Prelude.rnf certificateDetails
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf
        customIamInstanceProfile
      `Prelude.seq` Prelude.rnf
        customerOwnedIpEnabled
      `Prelude.seq` Prelude.rnf
        dbClusterIdentifier
      `Prelude.seq` Prelude.rnf
        dbInstanceArn
      `Prelude.seq` Prelude.rnf
        dbInstanceAutomatedBackupsReplications
      `Prelude.seq` Prelude.rnf
        dbInstanceClass
      `Prelude.seq` Prelude.rnf
        dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf
        dbInstanceStatus
      `Prelude.seq` Prelude.rnf
        dbName
      `Prelude.seq` Prelude.rnf
        dbParameterGroups
      `Prelude.seq` Prelude.rnf
        dbSecurityGroups
      `Prelude.seq` Prelude.rnf
        dbSubnetGroup
      `Prelude.seq` Prelude.rnf
        dbSystemId
      `Prelude.seq` Prelude.rnf
        dbInstancePort
      `Prelude.seq` Prelude.rnf
        dbiResourceId
      `Prelude.seq` Prelude.rnf
        deletionProtection
      `Prelude.seq` Prelude.rnf
        domainMemberships
      `Prelude.seq` Prelude.rnf
        enabledCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf
        endpoint
      `Prelude.seq` Prelude.rnf
        engine
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        enhancedMonitoringResourceArn
      `Prelude.seq` Prelude.rnf
        iAMDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf
        instanceCreateTime
      `Prelude.seq` Prelude.rnf
        iops
      `Prelude.seq` Prelude.rnf
        kmsKeyId
      `Prelude.seq` Prelude.rnf
        latestRestorableTime
      `Prelude.seq` Prelude.rnf
        licenseModel
      `Prelude.seq` Prelude.rnf
        listenerEndpoint
      `Prelude.seq` Prelude.rnf
        masterUserSecret
      `Prelude.seq` Prelude.rnf
        masterUsername
      `Prelude.seq` Prelude.rnf
        maxAllocatedStorage
      `Prelude.seq` Prelude.rnf
        monitoringInterval
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf
        multiAZ
      `Prelude.seq` Prelude.rnf
        ncharCharacterSetName
      `Prelude.seq` Prelude.rnf
        networkType
      `Prelude.seq` Prelude.rnf
        optionGroupMemberships
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
      `Prelude.seq` Prelude.rnf
        performanceInsightsEnabled
      `Prelude.seq` Prelude.rnf
        performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        performanceInsightsRetentionPeriod
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        processorFeatures
      `Prelude.seq` Prelude.rnf
        promotionTier
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        readReplicaDBClusterIdentifiers
      `Prelude.seq` Prelude.rnf
        readReplicaDBInstanceIdentifiers
      `Prelude.seq` Prelude.rnf
        readReplicaSourceDBInstanceIdentifier
      `Prelude.seq` Prelude.rnf
        replicaMode
      `Prelude.seq` Prelude.rnf
        resumeFullAutomationModeTime
      `Prelude.seq` Prelude.rnf
        secondaryAvailabilityZone
      `Prelude.seq` Prelude.rnf
        statusInfos
      `Prelude.seq` Prelude.rnf
        storageEncrypted
      `Prelude.seq` Prelude.rnf
        storageThroughput
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tagList
      `Prelude.seq` Prelude.rnf
        tdeCredentialArn
      `Prelude.seq` Prelude.rnf
        timezone
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroups
