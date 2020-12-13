{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance.
module Network.AWS.RDS.CreateDBInstance
  ( -- * Creating a request
    CreateDBInstance (..),
    mkCreateDBInstance,

    -- ** Request lenses
    cdiEngineVersion,
    cdiDBSecurityGroups,
    cdiDeletionProtection,
    cdiStorageEncrypted,
    cdiDBClusterIdentifier,
    cdiMasterUserPassword,
    cdiPubliclyAccessible,
    cdiAutoMinorVersionUpgrade,
    cdiMasterUsername,
    cdiDBSubnetGroupName,
    cdiMonitoringRoleARN,
    cdiIOPS,
    cdiDomain,
    cdiMonitoringInterval,
    cdiEngine,
    cdiTDECredentialPassword,
    cdiProcessorFeatures,
    cdiDBInstanceClass,
    cdiPromotionTier,
    cdiLicenseModel,
    cdiPreferredMaintenanceWindow,
    cdiPerformanceInsightsRetentionPeriod,
    cdiDBInstanceIdentifier,
    cdiCharacterSetName,
    cdiMaxAllocatedStorage,
    cdiEnablePerformanceInsights,
    cdiKMSKeyId,
    cdiDBParameterGroupName,
    cdiPreferredBackupWindow,
    cdiAvailabilityZone,
    cdiBackupRetentionPeriod,
    cdiNcharCharacterSetName,
    cdiPerformanceInsightsKMSKeyId,
    cdiVPCSecurityGroupIds,
    cdiMultiAZ,
    cdiAllocatedStorage,
    cdiOptionGroupName,
    cdiCopyTagsToSnapshot,
    cdiTimezone,
    cdiTDECredentialARN,
    cdiDomainIAMRoleName,
    cdiTags,
    cdiPort,
    cdiEnableIAMDatabaseAuthentication,
    cdiStorageType,
    cdiEnableCloudwatchLogsExports,
    cdiDBName,

    -- * Destructuring the response
    CreateDBInstanceResponse (..),
    mkCreateDBInstanceResponse,

    -- ** Response lenses
    cdirsDBInstance,
    cdirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateDBInstance' smart constructor.
data CreateDBInstance = CreateDBInstance'
  { -- | The version number of the database engine to use.
    --
    -- For a list of valid engine versions, use the @DescribeDBEngineVersions@ action.
    -- The following are the database engines and links to information about the major and minor versions that are available with Amazon RDS. Not every database engine is available for every AWS Region.
    -- __Amazon Aurora__
    -- Not applicable. The version number of the database engine to be used by the DB instance is managed by the DB cluster.
    -- __MariaDB__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS Versions> in the /Amazon RDS User Guide./
    -- __Microsoft SQL Server__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
    -- __MySQL__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions> in the /Amazon RDS User Guide./
    -- __Oracle__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine Release Notes> in the /Amazon RDS User Guide./
    -- __PostgreSQL__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts.General.DBVersions Supported PostgreSQL Database Versions> in the /Amazon RDS User Guide./
    engineVersion :: Lude.Maybe Lude.Text,
    -- | A list of DB security groups to associate with this DB instance.
    --
    -- Default: The default DB security group for the database engine.
    dbSecurityGroups :: Lude.Maybe [Lude.Text],
    -- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
    --
    -- __Amazon Aurora__
    -- Not applicable. You can enable or disable deletion protection for the DB cluster. For more information, see @CreateDBCluster@ . DB instances in a DB cluster can be deleted even when deletion protection is enabled for the DB cluster.
    deletionProtection :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether the DB instance is encrypted. By default, it isn't encrypted.
    --
    -- __Amazon Aurora__
    -- Not applicable. The encryption for DB instances is managed by the DB cluster.
    storageEncrypted :: Lude.Maybe Lude.Bool,
    -- | The identifier of the DB cluster that the instance will belong to.
    dbClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | The password for the master user. The password can include any printable ASCII character except "/", """, or "@".
    --
    -- __Amazon Aurora__
    -- Not applicable. The password for the master user is managed by the DB cluster.
    -- __MariaDB__
    -- Constraints: Must contain from 8 to 41 characters.
    -- __Microsoft SQL Server__
    -- Constraints: Must contain from 8 to 128 characters.
    -- __MySQL__
    -- Constraints: Must contain from 8 to 41 characters.
    -- __Oracle__
    -- Constraints: Must contain from 8 to 30 characters.
    -- __PostgreSQL__
    -- Constraints: Must contain from 8 to 128 characters.
    masterUserPassword :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
    -- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
    -- Default: The default behavior varies depending on whether @DBSubnetGroupName@ is specified.
    -- If @DBSubnetGroupName@ isn't specified, and @PubliclyAccessible@ isn't specified, the following applies:
    --
    --     * If the default VPC in the target region doesn’t have an Internet gateway attached to it, the DB instance is private.
    --
    --
    --     * If the default VPC in the target region has an Internet gateway attached to it, the DB instance is public.
    --
    --
    -- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn't specified, the following applies:
    --
    --     * If the subnets are part of a VPC that doesn’t have an Internet gateway attached to it, the DB instance is private.
    --
    --
    --     * If the subnets are part of a VPC that has an Internet gateway attached to it, the DB instance is public.
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are applied automatically.
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    -- | The name for the master user.
    --
    -- __Amazon Aurora__
    -- Not applicable. The name for the master user is managed by the DB cluster.
    -- __MariaDB__
    -- Constraints:
    --
    --     * Required for MariaDB.
    --
    --
    --     * Must be 1 to 16 letters or numbers.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    --
    --
    -- __Microsoft SQL Server__
    -- Constraints:
    --
    --     * Required for SQL Server.
    --
    --
    --     * Must be 1 to 128 letters or numbers.
    --
    --
    --     * The first character must be a letter.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    --
    --
    -- __MySQL__
    -- Constraints:
    --
    --     * Required for MySQL.
    --
    --
    --     * Must be 1 to 16 letters or numbers.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    --
    --
    -- __Oracle__
    -- Constraints:
    --
    --     * Required for Oracle.
    --
    --
    --     * Must be 1 to 30 letters or numbers.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    --
    --
    -- __PostgreSQL__
    -- Constraints:
    --
    --     * Required for PostgreSQL.
    --
    --
    --     * Must be 1 to 63 letters or numbers.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    masterUsername :: Lude.Maybe Lude.Text,
    -- | A DB subnet group to associate with this DB instance.
    --
    -- If there is no DB subnet group, then it is a non-VPC DB instance.
    dbSubnetGroupName :: Lude.Maybe Lude.Text,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide/ .
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
    monitoringRoleARN :: Lude.Maybe Lude.Text,
    -- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide/ .
    --
    -- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL DB instances, must be a multiple between .5 and 50 of the storage amount for the DB instance. For SQL Server DB instances, must be a multiple between 1 and 50 of the storage amount for the DB instance.
    iops :: Lude.Maybe Lude.Int,
    -- | The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
    domain :: Lude.Maybe Lude.Text,
    -- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Lude.Maybe Lude.Int,
    -- | The name of the database engine to be used for this instance.
    --
    -- Not every database engine is available for every AWS Region.
    -- Valid Values:
    --
    --     * @aurora@ (for MySQL 5.6-compatible Aurora)
    --
    --
    --     * @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
    --
    --
    --     * @aurora-postgresql@
    --
    --
    --     * @mariadb@
    --
    --
    --     * @mysql@
    --
    --
    --     * @oracle-ee@
    --
    --
    --     * @oracle-se2@
    --
    --
    --     * @oracle-se1@
    --
    --
    --     * @oracle-se@
    --
    --
    --     * @postgres@
    --
    --
    --     * @sqlserver-ee@
    --
    --
    --     * @sqlserver-se@
    --
    --
    --     * @sqlserver-ex@
    --
    --
    --     * @sqlserver-web@
    engine :: Lude.Text,
    -- | The password for the given ARN from the key store in order to access the device.
    tdeCredentialPassword :: Lude.Maybe Lude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
    processorFeatures :: Lude.Maybe [ProcessorFeature],
    -- | The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
    dbInstanceClass :: Lude.Text,
    -- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
    --
    -- Default: 1
    -- Valid Values: 0 - 15
    promotionTier :: Lude.Maybe Lude.Int,
    -- | License model information for this DB instance.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
    licenseModel :: Lude.Maybe Lude.Text,
    -- | The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> .
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week.
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | The DB instance identifier. This parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens.
    --
    --
    -- Example: @mydbinstance@
    dbInstanceIdentifier :: Lude.Text,
    -- | For supported engines, indicates that the DB instance should be associated with the specified CharacterSet.
    --
    -- __Amazon Aurora__
    -- Not applicable. The character set is managed by the DB cluster. For more information, see @CreateDBCluster@ .
    characterSetName :: Lude.Maybe Lude.Text,
    -- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
    maxAllocatedStorage :: Lude.Maybe Lude.Int,
    -- | A value that indicates whether to enable Performance Insights for the DB instance.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
    enablePerformanceInsights :: Lude.Maybe Lude.Bool,
    -- | The AWS KMS key identifier for an encrypted DB instance.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key.
    -- __Amazon Aurora__
    -- Not applicable. The KMS key identifier is managed by the DB cluster. For more information, see @CreateDBCluster@ .
    -- If @StorageEncrypted@ is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The name of the DB parameter group to associate with this DB instance. If you do not specify a value, then the default DB parameter group for the specified DB engine and version is used.
    --
    -- Constraints:
    --
    --     * Must be 1 to 255 letters, numbers, or hyphens.
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    dbParameterGroupName :: Lude.Maybe Lude.Text,
    -- | The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide/ .
    --
    -- __Amazon Aurora__
    -- Not applicable. The daily time range for creating automated backups is managed by the DB cluster.
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow Adjusting the Preferred DB Instance Maintenance Window> in the /Amazon RDS User Guide/ .
    -- Constraints:
    --
    --     * Must be in the format @hh24:mi-hh24:mi@ .
    --
    --
    --     * Must be in Universal Coordinated Time (UTC).
    --
    --
    --     * Must not conflict with the preferred maintenance window.
    --
    --
    --     * Must be at least 30 minutes.
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    -- | The Availability Zone (AZ) where the database will be created. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
    -- Example: @us-east-1d@
    -- Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
    --
    -- __Amazon Aurora__
    -- Not applicable. The retention period for automated backups is managed by the DB cluster.
    -- Default: 1
    -- Constraints:
    --
    --     * Must be a value from 0 to 35
    --
    --
    --     * Can't be set to 0 if the DB instance is a source to read replicas
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | The name of the NCHAR character set for the Oracle DB instance.
    ncharCharacterSetName :: Lude.Maybe Lude.Text,
    -- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    performanceInsightsKMSKeyId :: Lude.Maybe Lude.Text,
    -- | A list of Amazon EC2 VPC security groups to associate with this DB instance.
    --
    -- __Amazon Aurora__
    -- Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster.
    -- Default: The default EC2 VPC security group for the DB subnet group's VPC.
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
    multiAZ :: Lude.Maybe Lude.Bool,
    -- | The amount of storage (in gibibytes) to allocate for the DB instance.
    --
    -- Type: Integer
    -- __Amazon Aurora__
    -- Not applicable. Aurora cluster volumes automatically grow as the amount of data in your database increases, though you are only charged for the space that you use in an Aurora cluster volume.
    -- __MySQL__
    -- Constraints to the amount of storage for each storage type are the following:
    --
    --     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
    --
    --
    --     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
    --
    --
    --     * Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    --
    -- __MariaDB__
    -- Constraints to the amount of storage for each storage type are the following:
    --
    --     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
    --
    --
    --     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
    --
    --
    --     * Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    --
    -- __PostgreSQL__
    -- Constraints to the amount of storage for each storage type are the following:
    --
    --     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
    --
    --
    --     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
    --
    --
    --     * Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    --
    -- __Oracle__
    -- Constraints to the amount of storage for each storage type are the following:
    --
    --     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
    --
    --
    --     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
    --
    --
    --     * Magnetic storage (standard): Must be an integer from 10 to 3072.
    --
    --
    -- __SQL Server__
    -- Constraints to the amount of storage for each storage type are the following:
    --
    --     * General Purpose (SSD) storage (gp2):
    --
    --     * Enterprise and Standard editions: Must be an integer from 200 to 16384.
    --
    --
    --     * Web and Express editions: Must be an integer from 20 to 16384.
    --
    --
    --
    --
    --     * Provisioned IOPS storage (io1):
    --
    --     * Enterprise and Standard editions: Must be an integer from 200 to 16384.
    --
    --
    --     * Web and Express editions: Must be an integer from 100 to 16384.
    --
    --
    --
    --
    --     * Magnetic storage (standard):
    --
    --     * Enterprise and Standard editions: Must be an integer from 200 to 1024.
    --
    --
    --     * Web and Express editions: Must be an integer from 20 to 1024.
    allocatedStorage :: Lude.Maybe Lude.Int,
    -- | Indicates that the DB instance should be associated with the specified option group.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group. Also, that option group can't be removed from a DB instance once it is associated with a DB instance
    optionGroupName :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to copy tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
    --
    -- __Amazon Aurora__
    -- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting.
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    -- | The time zone of the DB instance. The time zone parameter is currently supported only by <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server> .
    timezone :: Lude.Maybe Lude.Text,
    -- | The ARN from the key store with which to associate the instance for TDE encryption.
    tdeCredentialARN :: Lude.Maybe Lude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
    domainIAMRoleName :: Lude.Maybe Lude.Text,
    -- | Tags to assign to the DB instance.
    tags :: Lude.Maybe [Tag],
    -- | The port number on which the database accepts connections.
    --
    -- __MySQL__
    -- Default: @3306@
    -- Valid values: @1150-65535@
    -- Type: Integer
    -- __MariaDB__
    -- Default: @3306@
    -- Valid values: @1150-65535@
    -- Type: Integer
    -- __PostgreSQL__
    -- Default: @5432@
    -- Valid values: @1150-65535@
    -- Type: Integer
    -- __Oracle__
    -- Default: @1521@
    -- Valid values: @1150-65535@
    -- __SQL Server__
    -- Default: @1433@
    -- Valid values: @1150-65535@ except @1234@ , @1434@ , @3260@ , @3343@ , @3389@ , @47001@ , and @49152-49156@ .
    -- __Amazon Aurora__
    -- Default: @3306@
    -- Valid values: @1150-65535@
    -- Type: Integer
    port :: Lude.Maybe Lude.Int,
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- This setting doesn't apply to Amazon Aurora. Mapping AWS IAM accounts to database accounts is managed by the DB cluster.
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Lude.Maybe Lude.Bool,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @standard | gp2 | io1@
    -- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Lude.Maybe Lude.Text,
    -- | The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Relational Database Service User Guide/ .
    --
    -- __Amazon Aurora__
    -- Not applicable. CloudWatch Logs exports are managed by the DB cluster.
    -- __MariaDB__
    -- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
    -- __Microsoft SQL Server__
    -- Possible values are @agent@ and @error@ .
    -- __MySQL__
    -- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
    -- __Oracle__
    -- Possible values are @alert@ , @audit@ , @listener@ , and @trace@ .
    -- __PostgreSQL__
    -- Possible values are @postgresql@ and @upgrade@ .
    enableCloudwatchLogsExports :: Lude.Maybe [Lude.Text],
    -- | The meaning of this parameter differs according to the database engine you use.
    --
    -- __MySQL__
    -- The name of the database to create when the DB instance is created. If this parameter isn't specified, no database is created in the DB instance.
    -- Constraints:
    --
    --     * Must contain 1 to 64 letters or numbers.
    --
    --
    --     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
    --
    --
    --     * Can't be a word reserved by the specified database engine
    --
    --
    -- __MariaDB__
    -- The name of the database to create when the DB instance is created. If this parameter isn't specified, no database is created in the DB instance.
    -- Constraints:
    --
    --     * Must contain 1 to 64 letters or numbers.
    --
    --
    --     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
    --
    --
    --     * Can't be a word reserved by the specified database engine
    --
    --
    -- __PostgreSQL__
    -- The name of the database to create when the DB instance is created. If this parameter isn't specified, the default "postgres" database is created in the DB instance.
    -- Constraints:
    --
    --     * Must contain 1 to 63 letters, numbers, or underscores.
    --
    --
    --     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
    --
    --
    --     * Can't be a word reserved by the specified database engine
    --
    --
    -- __Oracle__
    -- The Oracle System ID (SID) of the created DB instance. If you specify @null@ , the default value @ORCL@ is used. You can't specify the string NULL, or any other reserved word, for @DBName@ .
    -- Default: @ORCL@
    -- Constraints:
    --
    --     * Can't be longer than 8 characters
    --
    --
    -- __SQL Server__
    -- Not applicable. Must be null.
    -- __Amazon Aurora__
    -- The name of the database to create when the primary instance of the DB cluster is created. If this parameter isn't specified, no database is created in the DB instance.
    -- Constraints:
    --
    --     * Must contain 1 to 64 letters or numbers.
    --
    --
    --     * Can't be a word reserved by the specified database engine
    dbName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBInstance' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The version number of the database engine to use.
--
-- For a list of valid engine versions, use the @DescribeDBEngineVersions@ action.
-- The following are the database engines and links to information about the major and minor versions that are available with Amazon RDS. Not every database engine is available for every AWS Region.
-- __Amazon Aurora__
-- Not applicable. The version number of the database engine to be used by the DB instance is managed by the DB cluster.
-- __MariaDB__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS Versions> in the /Amazon RDS User Guide./
-- __Microsoft SQL Server__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
-- __MySQL__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions> in the /Amazon RDS User Guide./
-- __Oracle__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine Release Notes> in the /Amazon RDS User Guide./
-- __PostgreSQL__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts.General.DBVersions Supported PostgreSQL Database Versions> in the /Amazon RDS User Guide./
-- * 'dbSecurityGroups' - A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
-- * 'deletionProtection' - A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- __Amazon Aurora__
-- Not applicable. You can enable or disable deletion protection for the DB cluster. For more information, see @CreateDBCluster@ . DB instances in a DB cluster can be deleted even when deletion protection is enabled for the DB cluster.
-- * 'storageEncrypted' - A value that indicates whether the DB instance is encrypted. By default, it isn't encrypted.
--
-- __Amazon Aurora__
-- Not applicable. The encryption for DB instances is managed by the DB cluster.
-- * 'dbClusterIdentifier' - The identifier of the DB cluster that the instance will belong to.
-- * 'masterUserPassword' - The password for the master user. The password can include any printable ASCII character except "/", """, or "@".
--
-- __Amazon Aurora__
-- Not applicable. The password for the master user is managed by the DB cluster.
-- __MariaDB__
-- Constraints: Must contain from 8 to 41 characters.
-- __Microsoft SQL Server__
-- Constraints: Must contain from 8 to 128 characters.
-- __MySQL__
-- Constraints: Must contain from 8 to 41 characters.
-- __Oracle__
-- Constraints: Must contain from 8 to 30 characters.
-- __PostgreSQL__
-- Constraints: Must contain from 8 to 128 characters.
-- * 'publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- Default: The default behavior varies depending on whether @DBSubnetGroupName@ is specified.
-- If @DBSubnetGroupName@ isn't specified, and @PubliclyAccessible@ isn't specified, the following applies:
--
--     * If the default VPC in the target region doesn’t have an Internet gateway attached to it, the DB instance is private.
--
--
--     * If the default VPC in the target region has an Internet gateway attached to it, the DB instance is public.
--
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn't specified, the following applies:
--
--     * If the subnets are part of a VPC that doesn’t have an Internet gateway attached to it, the DB instance is private.
--
--
--     * If the subnets are part of a VPC that has an Internet gateway attached to it, the DB instance is public.
--
--
-- * 'autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are applied automatically.
-- * 'masterUsername' - The name for the master user.
--
-- __Amazon Aurora__
-- Not applicable. The name for the master user is managed by the DB cluster.
-- __MariaDB__
-- Constraints:
--
--     * Required for MariaDB.
--
--
--     * Must be 1 to 16 letters or numbers.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __Microsoft SQL Server__
-- Constraints:
--
--     * Required for SQL Server.
--
--
--     * Must be 1 to 128 letters or numbers.
--
--
--     * The first character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __MySQL__
-- Constraints:
--
--     * Required for MySQL.
--
--
--     * Must be 1 to 16 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __Oracle__
-- Constraints:
--
--     * Required for Oracle.
--
--
--     * Must be 1 to 30 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __PostgreSQL__
-- Constraints:
--
--     * Required for PostgreSQL.
--
--
--     * Must be 1 to 63 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- * 'dbSubnetGroupName' - A DB subnet group to associate with this DB instance.
--
-- If there is no DB subnet group, then it is a non-VPC DB instance.
-- * 'monitoringRoleARN' - The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide/ .
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
-- * 'iops' - The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide/ .
--
-- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL DB instances, must be a multiple between .5 and 50 of the storage amount for the DB instance. For SQL Server DB instances, must be a multiple between 1 and 50 of the storage amount for the DB instance.
-- * 'domain' - The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
-- * 'monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
-- * 'engine' - The name of the database engine to be used for this instance.
--
-- Not every database engine is available for every AWS Region.
-- Valid Values:
--
--     * @aurora@ (for MySQL 5.6-compatible Aurora)
--
--
--     * @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
--
--
--     * @aurora-postgresql@
--
--
--     * @mariadb@
--
--
--     * @mysql@
--
--
--     * @oracle-ee@
--
--
--     * @oracle-se2@
--
--
--     * @oracle-se1@
--
--
--     * @oracle-se@
--
--
--     * @postgres@
--
--
--     * @sqlserver-ee@
--
--
--     * @sqlserver-se@
--
--
--     * @sqlserver-ex@
--
--
--     * @sqlserver-web@
--
--
-- * 'tdeCredentialPassword' - The password for the given ARN from the key store in order to access the device.
-- * 'processorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
-- * 'dbInstanceClass' - The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
-- * 'promotionTier' - A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- Default: 1
-- Valid Values: 0 - 15
-- * 'licenseModel' - License model information for this DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
-- * 'preferredMaintenanceWindow' - The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> .
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week.
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
-- * 'performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
-- * 'dbInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @mydbinstance@
-- * 'characterSetName' - For supported engines, indicates that the DB instance should be associated with the specified CharacterSet.
--
-- __Amazon Aurora__
-- Not applicable. The character set is managed by the DB cluster. For more information, see @CreateDBCluster@ .
-- * 'maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
-- * 'enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB instance.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
-- * 'kmsKeyId' - The AWS KMS key identifier for an encrypted DB instance.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key.
-- __Amazon Aurora__
-- Not applicable. The KMS key identifier is managed by the DB cluster. For more information, see @CreateDBCluster@ .
-- If @StorageEncrypted@ is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance. If you do not specify a value, then the default DB parameter group for the specified DB engine and version is used.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- * 'preferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide/ .
--
-- __Amazon Aurora__
-- Not applicable. The daily time range for creating automated backups is managed by the DB cluster.
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow Adjusting the Preferred DB Instance Maintenance Window> in the /Amazon RDS User Guide/ .
-- Constraints:
--
--     * Must be in the format @hh24:mi-hh24:mi@ .
--
--
--     * Must be in Universal Coordinated Time (UTC).
--
--
--     * Must not conflict with the preferred maintenance window.
--
--
--     * Must be at least 30 minutes.
--
--
-- * 'availabilityZone' - The Availability Zone (AZ) where the database will be created. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
-- Example: @us-east-1d@
-- Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint.
-- * 'backupRetentionPeriod' - The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
--
-- __Amazon Aurora__
-- Not applicable. The retention period for automated backups is managed by the DB cluster.
-- Default: 1
-- Constraints:
--
--     * Must be a value from 0 to 35
--
--
--     * Can't be set to 0 if the DB instance is a source to read replicas
--
--
-- * 'ncharCharacterSetName' - The name of the NCHAR character set for the Oracle DB instance.
-- * 'performanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'vpcSecurityGroupIds' - A list of Amazon EC2 VPC security groups to associate with this DB instance.
--
-- __Amazon Aurora__
-- Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster.
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
-- * 'multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
-- * 'allocatedStorage' - The amount of storage (in gibibytes) to allocate for the DB instance.
--
-- Type: Integer
-- __Amazon Aurora__
-- Not applicable. Aurora cluster volumes automatically grow as the amount of data in your database increases, though you are only charged for the space that you use in an Aurora cluster volume.
-- __MySQL__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--
-- __MariaDB__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--
-- __PostgreSQL__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--
-- __Oracle__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 10 to 3072.
--
--
-- __SQL Server__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2):
--
--     * Enterprise and Standard editions: Must be an integer from 200 to 16384.
--
--
--     * Web and Express editions: Must be an integer from 20 to 16384.
--
--
--
--
--     * Provisioned IOPS storage (io1):
--
--     * Enterprise and Standard editions: Must be an integer from 200 to 16384.
--
--
--     * Web and Express editions: Must be an integer from 100 to 16384.
--
--
--
--
--     * Magnetic storage (standard):
--
--     * Enterprise and Standard editions: Must be an integer from 200 to 1024.
--
--
--     * Web and Express editions: Must be an integer from 20 to 1024.
--
--
--
--
-- * 'optionGroupName' - Indicates that the DB instance should be associated with the specified option group.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group. Also, that option group can't be removed from a DB instance once it is associated with a DB instance
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
-- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting.
-- * 'timezone' - The time zone of the DB instance. The time zone parameter is currently supported only by <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server> .
-- * 'tdeCredentialARN' - The ARN from the key store with which to associate the instance for TDE encryption.
-- * 'domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
-- * 'tags' - Tags to assign to the DB instance.
-- * 'port' - The port number on which the database accepts connections.
--
-- __MySQL__
-- Default: @3306@
-- Valid values: @1150-65535@
-- Type: Integer
-- __MariaDB__
-- Default: @3306@
-- Valid values: @1150-65535@
-- Type: Integer
-- __PostgreSQL__
-- Default: @5432@
-- Valid values: @1150-65535@
-- Type: Integer
-- __Oracle__
-- Default: @1521@
-- Valid values: @1150-65535@
-- __SQL Server__
-- Default: @1433@
-- Valid values: @1150-65535@ except @1234@ , @1434@ , @3260@ , @3343@ , @3389@ , @47001@ , and @49152-49156@ .
-- __Amazon Aurora__
-- Default: @3306@
-- Valid values: @1150-65535@
-- Type: Integer
-- * 'enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- This setting doesn't apply to Amazon Aurora. Mapping AWS IAM accounts to database accounts is managed by the DB cluster.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
-- * 'storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
-- * 'enableCloudwatchLogsExports' - The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Relational Database Service User Guide/ .
--
-- __Amazon Aurora__
-- Not applicable. CloudWatch Logs exports are managed by the DB cluster.
-- __MariaDB__
-- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
-- __Microsoft SQL Server__
-- Possible values are @agent@ and @error@ .
-- __MySQL__
-- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
-- __Oracle__
-- Possible values are @alert@ , @audit@ , @listener@ , and @trace@ .
-- __PostgreSQL__
-- Possible values are @postgresql@ and @upgrade@ .
-- * 'dbName' - The meaning of this parameter differs according to the database engine you use.
--
-- __MySQL__
-- The name of the database to create when the DB instance is created. If this parameter isn't specified, no database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 64 letters or numbers.
--
--
--     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
--
--
--     * Can't be a word reserved by the specified database engine
--
--
-- __MariaDB__
-- The name of the database to create when the DB instance is created. If this parameter isn't specified, no database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 64 letters or numbers.
--
--
--     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
--
--
--     * Can't be a word reserved by the specified database engine
--
--
-- __PostgreSQL__
-- The name of the database to create when the DB instance is created. If this parameter isn't specified, the default "postgres" database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 63 letters, numbers, or underscores.
--
--
--     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
--
--
--     * Can't be a word reserved by the specified database engine
--
--
-- __Oracle__
-- The Oracle System ID (SID) of the created DB instance. If you specify @null@ , the default value @ORCL@ is used. You can't specify the string NULL, or any other reserved word, for @DBName@ .
-- Default: @ORCL@
-- Constraints:
--
--     * Can't be longer than 8 characters
--
--
-- __SQL Server__
-- Not applicable. Must be null.
-- __Amazon Aurora__
-- The name of the database to create when the primary instance of the DB cluster is created. If this parameter isn't specified, no database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 64 letters or numbers.
--
--
--     * Can't be a word reserved by the specified database engine
mkCreateDBInstance ::
  -- | 'engine'
  Lude.Text ->
  -- | 'dbInstanceClass'
  Lude.Text ->
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  CreateDBInstance
mkCreateDBInstance
  pEngine_
  pDBInstanceClass_
  pDBInstanceIdentifier_ =
    CreateDBInstance'
      { engineVersion = Lude.Nothing,
        dbSecurityGroups = Lude.Nothing,
        deletionProtection = Lude.Nothing,
        storageEncrypted = Lude.Nothing,
        dbClusterIdentifier = Lude.Nothing,
        masterUserPassword = Lude.Nothing,
        publiclyAccessible = Lude.Nothing,
        autoMinorVersionUpgrade = Lude.Nothing,
        masterUsername = Lude.Nothing,
        dbSubnetGroupName = Lude.Nothing,
        monitoringRoleARN = Lude.Nothing,
        iops = Lude.Nothing,
        domain = Lude.Nothing,
        monitoringInterval = Lude.Nothing,
        engine = pEngine_,
        tdeCredentialPassword = Lude.Nothing,
        processorFeatures = Lude.Nothing,
        dbInstanceClass = pDBInstanceClass_,
        promotionTier = Lude.Nothing,
        licenseModel = Lude.Nothing,
        preferredMaintenanceWindow = Lude.Nothing,
        performanceInsightsRetentionPeriod = Lude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        characterSetName = Lude.Nothing,
        maxAllocatedStorage = Lude.Nothing,
        enablePerformanceInsights = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        dbParameterGroupName = Lude.Nothing,
        preferredBackupWindow = Lude.Nothing,
        availabilityZone = Lude.Nothing,
        backupRetentionPeriod = Lude.Nothing,
        ncharCharacterSetName = Lude.Nothing,
        performanceInsightsKMSKeyId = Lude.Nothing,
        vpcSecurityGroupIds = Lude.Nothing,
        multiAZ = Lude.Nothing,
        allocatedStorage = Lude.Nothing,
        optionGroupName = Lude.Nothing,
        copyTagsToSnapshot = Lude.Nothing,
        timezone = Lude.Nothing,
        tdeCredentialARN = Lude.Nothing,
        domainIAMRoleName = Lude.Nothing,
        tags = Lude.Nothing,
        port = Lude.Nothing,
        enableIAMDatabaseAuthentication = Lude.Nothing,
        storageType = Lude.Nothing,
        enableCloudwatchLogsExports = Lude.Nothing,
        dbName = Lude.Nothing
      }

-- | The version number of the database engine to use.
--
-- For a list of valid engine versions, use the @DescribeDBEngineVersions@ action.
-- The following are the database engines and links to information about the major and minor versions that are available with Amazon RDS. Not every database engine is available for every AWS Region.
-- __Amazon Aurora__
-- Not applicable. The version number of the database engine to be used by the DB instance is managed by the DB cluster.
-- __MariaDB__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS Versions> in the /Amazon RDS User Guide./
-- __Microsoft SQL Server__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
-- __MySQL__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions> in the /Amazon RDS User Guide./
-- __Oracle__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine Release Notes> in the /Amazon RDS User Guide./
-- __PostgreSQL__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts.General.DBVersions Supported PostgreSQL Database Versions> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiEngineVersion :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiEngineVersion = Lens.lens (engineVersion :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CreateDBInstance)
{-# DEPRECATED cdiEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
--
-- /Note:/ Consider using 'dbSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiDBSecurityGroups :: Lens.Lens' CreateDBInstance (Lude.Maybe [Lude.Text])
cdiDBSecurityGroups = Lens.lens (dbSecurityGroups :: CreateDBInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {dbSecurityGroups = a} :: CreateDBInstance)
{-# DEPRECATED cdiDBSecurityGroups "Use generic-lens or generic-optics with 'dbSecurityGroups' instead." #-}

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- __Amazon Aurora__
-- Not applicable. You can enable or disable deletion protection for the DB cluster. For more information, see @CreateDBCluster@ . DB instances in a DB cluster can be deleted even when deletion protection is enabled for the DB cluster.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiDeletionProtection :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Bool)
cdiDeletionProtection = Lens.lens (deletionProtection :: CreateDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: CreateDBInstance)
{-# DEPRECATED cdiDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | A value that indicates whether the DB instance is encrypted. By default, it isn't encrypted.
--
-- __Amazon Aurora__
-- Not applicable. The encryption for DB instances is managed by the DB cluster.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiStorageEncrypted :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Bool)
cdiStorageEncrypted = Lens.lens (storageEncrypted :: CreateDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {storageEncrypted = a} :: CreateDBInstance)
{-# DEPRECATED cdiStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | The identifier of the DB cluster that the instance will belong to.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiDBClusterIdentifier :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: CreateDBInstance)
{-# DEPRECATED cdiDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The password for the master user. The password can include any printable ASCII character except "/", """, or "@".
--
-- __Amazon Aurora__
-- Not applicable. The password for the master user is managed by the DB cluster.
-- __MariaDB__
-- Constraints: Must contain from 8 to 41 characters.
-- __Microsoft SQL Server__
-- Constraints: Must contain from 8 to 128 characters.
-- __MySQL__
-- Constraints: Must contain from 8 to 41 characters.
-- __Oracle__
-- Constraints: Must contain from 8 to 30 characters.
-- __PostgreSQL__
-- Constraints: Must contain from 8 to 128 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiMasterUserPassword :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiMasterUserPassword = Lens.lens (masterUserPassword :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {masterUserPassword = a} :: CreateDBInstance)
{-# DEPRECATED cdiMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- Default: The default behavior varies depending on whether @DBSubnetGroupName@ is specified.
-- If @DBSubnetGroupName@ isn't specified, and @PubliclyAccessible@ isn't specified, the following applies:
--
--     * If the default VPC in the target region doesn’t have an Internet gateway attached to it, the DB instance is private.
--
--
--     * If the default VPC in the target region has an Internet gateway attached to it, the DB instance is public.
--
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn't specified, the following applies:
--
--     * If the subnets are part of a VPC that doesn’t have an Internet gateway attached to it, the DB instance is private.
--
--
--     * If the subnets are part of a VPC that has an Internet gateway attached to it, the DB instance is public.
--
--
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiPubliclyAccessible :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Bool)
cdiPubliclyAccessible = Lens.lens (publiclyAccessible :: CreateDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: CreateDBInstance)
{-# DEPRECATED cdiPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are applied automatically.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiAutoMinorVersionUpgrade :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Bool)
cdiAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: CreateDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: CreateDBInstance)
{-# DEPRECATED cdiAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The name for the master user.
--
-- __Amazon Aurora__
-- Not applicable. The name for the master user is managed by the DB cluster.
-- __MariaDB__
-- Constraints:
--
--     * Required for MariaDB.
--
--
--     * Must be 1 to 16 letters or numbers.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __Microsoft SQL Server__
-- Constraints:
--
--     * Required for SQL Server.
--
--
--     * Must be 1 to 128 letters or numbers.
--
--
--     * The first character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __MySQL__
-- Constraints:
--
--     * Required for MySQL.
--
--
--     * Must be 1 to 16 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __Oracle__
-- Constraints:
--
--     * Required for Oracle.
--
--
--     * Must be 1 to 30 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __PostgreSQL__
-- Constraints:
--
--     * Required for PostgreSQL.
--
--
--     * Must be 1 to 63 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiMasterUsername :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiMasterUsername = Lens.lens (masterUsername :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: CreateDBInstance)
{-# DEPRECATED cdiMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | A DB subnet group to associate with this DB instance.
--
-- If there is no DB subnet group, then it is a non-VPC DB instance.
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiDBSubnetGroupName :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: CreateDBInstance)
{-# DEPRECATED cdiDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide/ .
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- /Note:/ Consider using 'monitoringRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiMonitoringRoleARN :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiMonitoringRoleARN = Lens.lens (monitoringRoleARN :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {monitoringRoleARN = a} :: CreateDBInstance)
{-# DEPRECATED cdiMonitoringRoleARN "Use generic-lens or generic-optics with 'monitoringRoleARN' instead." #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide/ .
--
-- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL DB instances, must be a multiple between .5 and 50 of the storage amount for the DB instance. For SQL Server DB instances, must be a multiple between 1 and 50 of the storage amount for the DB instance.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiIOPS :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Int)
cdiIOPS = Lens.lens (iops :: CreateDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: CreateDBInstance)
{-# DEPRECATED cdiIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiDomain :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiDomain = Lens.lens (domain :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: CreateDBInstance)
{-# DEPRECATED cdiDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- /Note:/ Consider using 'monitoringInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiMonitoringInterval :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Int)
cdiMonitoringInterval = Lens.lens (monitoringInterval :: CreateDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {monitoringInterval = a} :: CreateDBInstance)
{-# DEPRECATED cdiMonitoringInterval "Use generic-lens or generic-optics with 'monitoringInterval' instead." #-}

-- | The name of the database engine to be used for this instance.
--
-- Not every database engine is available for every AWS Region.
-- Valid Values:
--
--     * @aurora@ (for MySQL 5.6-compatible Aurora)
--
--
--     * @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
--
--
--     * @aurora-postgresql@
--
--
--     * @mariadb@
--
--
--     * @mysql@
--
--
--     * @oracle-ee@
--
--
--     * @oracle-se2@
--
--
--     * @oracle-se1@
--
--
--     * @oracle-se@
--
--
--     * @postgres@
--
--
--     * @sqlserver-ee@
--
--
--     * @sqlserver-se@
--
--
--     * @sqlserver-ex@
--
--
--     * @sqlserver-web@
--
--
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiEngine :: Lens.Lens' CreateDBInstance Lude.Text
cdiEngine = Lens.lens (engine :: CreateDBInstance -> Lude.Text) (\s a -> s {engine = a} :: CreateDBInstance)
{-# DEPRECATED cdiEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The password for the given ARN from the key store in order to access the device.
--
-- /Note:/ Consider using 'tdeCredentialPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiTDECredentialPassword :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiTDECredentialPassword = Lens.lens (tdeCredentialPassword :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialPassword = a} :: CreateDBInstance)
{-# DEPRECATED cdiTDECredentialPassword "Use generic-lens or generic-optics with 'tdeCredentialPassword' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiProcessorFeatures :: Lens.Lens' CreateDBInstance (Lude.Maybe [ProcessorFeature])
cdiProcessorFeatures = Lens.lens (processorFeatures :: CreateDBInstance -> Lude.Maybe [ProcessorFeature]) (\s a -> s {processorFeatures = a} :: CreateDBInstance)
{-# DEPRECATED cdiProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiDBInstanceClass :: Lens.Lens' CreateDBInstance Lude.Text
cdiDBInstanceClass = Lens.lens (dbInstanceClass :: CreateDBInstance -> Lude.Text) (\s a -> s {dbInstanceClass = a} :: CreateDBInstance)
{-# DEPRECATED cdiDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- Default: 1
-- Valid Values: 0 - 15
--
-- /Note:/ Consider using 'promotionTier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiPromotionTier :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Int)
cdiPromotionTier = Lens.lens (promotionTier :: CreateDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {promotionTier = a} :: CreateDBInstance)
{-# DEPRECATED cdiPromotionTier "Use generic-lens or generic-optics with 'promotionTier' instead." #-}

-- | License model information for this DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiLicenseModel :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiLicenseModel = Lens.lens (licenseModel :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: CreateDBInstance)
{-# DEPRECATED cdiLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> .
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week.
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiPreferredMaintenanceWindow :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: CreateDBInstance)
{-# DEPRECATED cdiPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
--
-- /Note:/ Consider using 'performanceInsightsRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiPerformanceInsightsRetentionPeriod :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Int)
cdiPerformanceInsightsRetentionPeriod = Lens.lens (performanceInsightsRetentionPeriod :: CreateDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {performanceInsightsRetentionPeriod = a} :: CreateDBInstance)
{-# DEPRECATED cdiPerformanceInsightsRetentionPeriod "Use generic-lens or generic-optics with 'performanceInsightsRetentionPeriod' instead." #-}

-- | The DB instance identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @mydbinstance@
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiDBInstanceIdentifier :: Lens.Lens' CreateDBInstance Lude.Text
cdiDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: CreateDBInstance -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: CreateDBInstance)
{-# DEPRECATED cdiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | For supported engines, indicates that the DB instance should be associated with the specified CharacterSet.
--
-- __Amazon Aurora__
-- Not applicable. The character set is managed by the DB cluster. For more information, see @CreateDBCluster@ .
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiCharacterSetName :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiCharacterSetName = Lens.lens (characterSetName :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {characterSetName = a} :: CreateDBInstance)
{-# DEPRECATED cdiCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiMaxAllocatedStorage :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Int)
cdiMaxAllocatedStorage = Lens.lens (maxAllocatedStorage :: CreateDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {maxAllocatedStorage = a} :: CreateDBInstance)
{-# DEPRECATED cdiMaxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead." #-}

-- | A value that indicates whether to enable Performance Insights for the DB instance.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
--
-- /Note:/ Consider using 'enablePerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiEnablePerformanceInsights :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Bool)
cdiEnablePerformanceInsights = Lens.lens (enablePerformanceInsights :: CreateDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {enablePerformanceInsights = a} :: CreateDBInstance)
{-# DEPRECATED cdiEnablePerformanceInsights "Use generic-lens or generic-optics with 'enablePerformanceInsights' instead." #-}

-- | The AWS KMS key identifier for an encrypted DB instance.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key.
-- __Amazon Aurora__
-- Not applicable. The KMS key identifier is managed by the DB cluster. For more information, see @CreateDBCluster@ .
-- If @StorageEncrypted@ is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiKMSKeyId :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiKMSKeyId = Lens.lens (kmsKeyId :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateDBInstance)
{-# DEPRECATED cdiKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the DB parameter group to associate with this DB instance. If you do not specify a value, then the default DB parameter group for the specified DB engine and version is used.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiDBParameterGroupName :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiDBParameterGroupName = Lens.lens (dbParameterGroupName :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupName = a} :: CreateDBInstance)
{-# DEPRECATED cdiDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide/ .
--
-- __Amazon Aurora__
-- Not applicable. The daily time range for creating automated backups is managed by the DB cluster.
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow Adjusting the Preferred DB Instance Maintenance Window> in the /Amazon RDS User Guide/ .
-- Constraints:
--
--     * Must be in the format @hh24:mi-hh24:mi@ .
--
--
--     * Must be in Universal Coordinated Time (UTC).
--
--
--     * Must not conflict with the preferred maintenance window.
--
--
--     * Must be at least 30 minutes.
--
--
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiPreferredBackupWindow :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiPreferredBackupWindow = Lens.lens (preferredBackupWindow :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: CreateDBInstance)
{-# DEPRECATED cdiPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The Availability Zone (AZ) where the database will be created. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
-- Example: @us-east-1d@
-- Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiAvailabilityZone :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiAvailabilityZone = Lens.lens (availabilityZone :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateDBInstance)
{-# DEPRECATED cdiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
--
-- __Amazon Aurora__
-- Not applicable. The retention period for automated backups is managed by the DB cluster.
-- Default: 1
-- Constraints:
--
--     * Must be a value from 0 to 35
--
--
--     * Can't be set to 0 if the DB instance is a source to read replicas
--
--
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiBackupRetentionPeriod :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Int)
cdiBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: CreateDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: CreateDBInstance)
{-# DEPRECATED cdiBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | The name of the NCHAR character set for the Oracle DB instance.
--
-- /Note:/ Consider using 'ncharCharacterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiNcharCharacterSetName :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiNcharCharacterSetName = Lens.lens (ncharCharacterSetName :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {ncharCharacterSetName = a} :: CreateDBInstance)
{-# DEPRECATED cdiNcharCharacterSetName "Use generic-lens or generic-optics with 'ncharCharacterSetName' instead." #-}

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'performanceInsightsKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiPerformanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiPerformanceInsightsKMSKeyId = Lens.lens (performanceInsightsKMSKeyId :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {performanceInsightsKMSKeyId = a} :: CreateDBInstance)
{-# DEPRECATED cdiPerformanceInsightsKMSKeyId "Use generic-lens or generic-optics with 'performanceInsightsKMSKeyId' instead." #-}

-- | A list of Amazon EC2 VPC security groups to associate with this DB instance.
--
-- __Amazon Aurora__
-- Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster.
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiVPCSecurityGroupIds :: Lens.Lens' CreateDBInstance (Lude.Maybe [Lude.Text])
cdiVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: CreateDBInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: CreateDBInstance)
{-# DEPRECATED cdiVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiMultiAZ :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Bool)
cdiMultiAZ = Lens.lens (multiAZ :: CreateDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: CreateDBInstance)
{-# DEPRECATED cdiMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The amount of storage (in gibibytes) to allocate for the DB instance.
--
-- Type: Integer
-- __Amazon Aurora__
-- Not applicable. Aurora cluster volumes automatically grow as the amount of data in your database increases, though you are only charged for the space that you use in an Aurora cluster volume.
-- __MySQL__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--
-- __MariaDB__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--
-- __PostgreSQL__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--
-- __Oracle__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 10 to 3072.
--
--
-- __SQL Server__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2):
--
--     * Enterprise and Standard editions: Must be an integer from 200 to 16384.
--
--
--     * Web and Express editions: Must be an integer from 20 to 16384.
--
--
--
--
--     * Provisioned IOPS storage (io1):
--
--     * Enterprise and Standard editions: Must be an integer from 200 to 16384.
--
--
--     * Web and Express editions: Must be an integer from 100 to 16384.
--
--
--
--
--     * Magnetic storage (standard):
--
--     * Enterprise and Standard editions: Must be an integer from 200 to 1024.
--
--
--     * Web and Express editions: Must be an integer from 20 to 1024.
--
--
--
--
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiAllocatedStorage :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Int)
cdiAllocatedStorage = Lens.lens (allocatedStorage :: CreateDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: CreateDBInstance)
{-# DEPRECATED cdiAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | Indicates that the DB instance should be associated with the specified option group.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group. Also, that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiOptionGroupName :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiOptionGroupName = Lens.lens (optionGroupName :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: CreateDBInstance)
{-# DEPRECATED cdiOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
-- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiCopyTagsToSnapshot :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Bool)
cdiCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: CreateDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: CreateDBInstance)
{-# DEPRECATED cdiCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The time zone of the DB instance. The time zone parameter is currently supported only by <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server> .
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiTimezone :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiTimezone = Lens.lens (timezone :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: CreateDBInstance)
{-# DEPRECATED cdiTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiTDECredentialARN :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiTDECredentialARN = Lens.lens (tdeCredentialARN :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialARN = a} :: CreateDBInstance)
{-# DEPRECATED cdiTDECredentialARN "Use generic-lens or generic-optics with 'tdeCredentialARN' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiDomainIAMRoleName :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiDomainIAMRoleName = Lens.lens (domainIAMRoleName :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: CreateDBInstance)
{-# DEPRECATED cdiDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | Tags to assign to the DB instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiTags :: Lens.Lens' CreateDBInstance (Lude.Maybe [Tag])
cdiTags = Lens.lens (tags :: CreateDBInstance -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBInstance)
{-# DEPRECATED cdiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which the database accepts connections.
--
-- __MySQL__
-- Default: @3306@
-- Valid values: @1150-65535@
-- Type: Integer
-- __MariaDB__
-- Default: @3306@
-- Valid values: @1150-65535@
-- Type: Integer
-- __PostgreSQL__
-- Default: @5432@
-- Valid values: @1150-65535@
-- Type: Integer
-- __Oracle__
-- Default: @1521@
-- Valid values: @1150-65535@
-- __SQL Server__
-- Default: @1433@
-- Valid values: @1150-65535@ except @1234@ , @1434@ , @3260@ , @3343@ , @3389@ , @47001@ , and @49152-49156@ .
-- __Amazon Aurora__
-- Default: @3306@
-- Valid values: @1150-65535@
-- Type: Integer
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiPort :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Int)
cdiPort = Lens.lens (port :: CreateDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: CreateDBInstance)
{-# DEPRECATED cdiPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- This setting doesn't apply to Amazon Aurora. Mapping AWS IAM accounts to database accounts is managed by the DB cluster.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiEnableIAMDatabaseAuthentication :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Bool)
cdiEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: CreateDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBInstance)
{-# DEPRECATED cdiEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiStorageType :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiStorageType = Lens.lens (storageType :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: CreateDBInstance)
{-# DEPRECATED cdiStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Relational Database Service User Guide/ .
--
-- __Amazon Aurora__
-- Not applicable. CloudWatch Logs exports are managed by the DB cluster.
-- __MariaDB__
-- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
-- __Microsoft SQL Server__
-- Possible values are @agent@ and @error@ .
-- __MySQL__
-- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
-- __Oracle__
-- Possible values are @alert@ , @audit@ , @listener@ , and @trace@ .
-- __PostgreSQL__
-- Possible values are @postgresql@ and @upgrade@ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiEnableCloudwatchLogsExports :: Lens.Lens' CreateDBInstance (Lude.Maybe [Lude.Text])
cdiEnableCloudwatchLogsExports = Lens.lens (enableCloudwatchLogsExports :: CreateDBInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {enableCloudwatchLogsExports = a} :: CreateDBInstance)
{-# DEPRECATED cdiEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

-- | The meaning of this parameter differs according to the database engine you use.
--
-- __MySQL__
-- The name of the database to create when the DB instance is created. If this parameter isn't specified, no database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 64 letters or numbers.
--
--
--     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
--
--
--     * Can't be a word reserved by the specified database engine
--
--
-- __MariaDB__
-- The name of the database to create when the DB instance is created. If this parameter isn't specified, no database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 64 letters or numbers.
--
--
--     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
--
--
--     * Can't be a word reserved by the specified database engine
--
--
-- __PostgreSQL__
-- The name of the database to create when the DB instance is created. If this parameter isn't specified, the default "postgres" database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 63 letters, numbers, or underscores.
--
--
--     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
--
--
--     * Can't be a word reserved by the specified database engine
--
--
-- __Oracle__
-- The Oracle System ID (SID) of the created DB instance. If you specify @null@ , the default value @ORCL@ is used. You can't specify the string NULL, or any other reserved word, for @DBName@ .
-- Default: @ORCL@
-- Constraints:
--
--     * Can't be longer than 8 characters
--
--
-- __SQL Server__
-- Not applicable. Must be null.
-- __Amazon Aurora__
-- The name of the database to create when the primary instance of the DB cluster is created. If this parameter isn't specified, no database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 64 letters or numbers.
--
--
--     * Can't be a word reserved by the specified database engine
--
--
--
-- /Note:/ Consider using 'dbName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdiDBName :: Lens.Lens' CreateDBInstance (Lude.Maybe Lude.Text)
cdiDBName = Lens.lens (dbName :: CreateDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbName = a} :: CreateDBInstance)
{-# DEPRECATED cdiDBName "Use generic-lens or generic-optics with 'dbName' instead." #-}

instance Lude.AWSRequest CreateDBInstance where
  type Rs CreateDBInstance = CreateDBInstanceResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBInstanceResult"
      ( \s h x ->
          CreateDBInstanceResponse'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDBInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBInstance where
  toQuery CreateDBInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDBInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "DBSecurityGroups"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "DBSecurityGroupName" Lude.<$> dbSecurityGroups),
        "DeletionProtection" Lude.=: deletionProtection,
        "StorageEncrypted" Lude.=: storageEncrypted,
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "MasterUserPassword" Lude.=: masterUserPassword,
        "PubliclyAccessible" Lude.=: publiclyAccessible,
        "AutoMinorVersionUpgrade" Lude.=: autoMinorVersionUpgrade,
        "MasterUsername" Lude.=: masterUsername,
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "MonitoringRoleArn" Lude.=: monitoringRoleARN,
        "Iops" Lude.=: iops,
        "Domain" Lude.=: domain,
        "MonitoringInterval" Lude.=: monitoringInterval,
        "Engine" Lude.=: engine,
        "TdeCredentialPassword" Lude.=: tdeCredentialPassword,
        "ProcessorFeatures"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "ProcessorFeature" Lude.<$> processorFeatures),
        "DBInstanceClass" Lude.=: dbInstanceClass,
        "PromotionTier" Lude.=: promotionTier,
        "LicenseModel" Lude.=: licenseModel,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "PerformanceInsightsRetentionPeriod"
          Lude.=: performanceInsightsRetentionPeriod,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "CharacterSetName" Lude.=: characterSetName,
        "MaxAllocatedStorage" Lude.=: maxAllocatedStorage,
        "EnablePerformanceInsights" Lude.=: enablePerformanceInsights,
        "KmsKeyId" Lude.=: kmsKeyId,
        "DBParameterGroupName" Lude.=: dbParameterGroupName,
        "PreferredBackupWindow" Lude.=: preferredBackupWindow,
        "AvailabilityZone" Lude.=: availabilityZone,
        "BackupRetentionPeriod" Lude.=: backupRetentionPeriod,
        "NcharCharacterSetName" Lude.=: ncharCharacterSetName,
        "PerformanceInsightsKMSKeyId" Lude.=: performanceInsightsKMSKeyId,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "MultiAZ" Lude.=: multiAZ,
        "AllocatedStorage" Lude.=: allocatedStorage,
        "OptionGroupName" Lude.=: optionGroupName,
        "CopyTagsToSnapshot" Lude.=: copyTagsToSnapshot,
        "Timezone" Lude.=: timezone,
        "TdeCredentialArn" Lude.=: tdeCredentialARN,
        "DomainIAMRoleName" Lude.=: domainIAMRoleName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "EnableIAMDatabaseAuthentication"
          Lude.=: enableIAMDatabaseAuthentication,
        "StorageType" Lude.=: storageType,
        "EnableCloudwatchLogsExports"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> enableCloudwatchLogsExports),
        "DBName" Lude.=: dbName
      ]

-- | /See:/ 'mkCreateDBInstanceResponse' smart constructor.
data CreateDBInstanceResponse = CreateDBInstanceResponse'
  { dbInstance :: Lude.Maybe DBInstance,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBInstanceResponse' with the minimum fields required to make a request.
--
-- * 'dbInstance' -
-- * 'responseStatus' - The response status code.
mkCreateDBInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDBInstanceResponse
mkCreateDBInstanceResponse pResponseStatus_ =
  CreateDBInstanceResponse'
    { dbInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirsDBInstance :: Lens.Lens' CreateDBInstanceResponse (Lude.Maybe DBInstance)
cdirsDBInstance = Lens.lens (dbInstance :: CreateDBInstanceResponse -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: CreateDBInstanceResponse)
{-# DEPRECATED cdirsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirsResponseStatus :: Lens.Lens' CreateDBInstanceResponse Lude.Int
cdirsResponseStatus = Lens.lens (responseStatus :: CreateDBInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDBInstanceResponse)
{-# DEPRECATED cdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
