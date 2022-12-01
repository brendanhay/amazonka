{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.RestoreDBInstanceFromS3
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Relational Database Service (Amazon RDS) supports importing MySQL
-- databases by using backup files. You can create a backup of your
-- on-premises database, store it on Amazon Simple Storage Service (Amazon
-- S3), and then restore the backup file onto a new Amazon RDS DB instance
-- running MySQL. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/MySQL.Procedural.Importing.html Importing Data into an Amazon RDS MySQL DB Instance>
-- in the /Amazon RDS User Guide./
--
-- This command doesn\'t apply to RDS Custom.
module Amazonka.RDS.RestoreDBInstanceFromS3
  ( -- * Creating a Request
    RestoreDBInstanceFromS3 (..),
    newRestoreDBInstanceFromS3,

    -- * Request Lenses
    restoreDBInstanceFromS3_tags,
    restoreDBInstanceFromS3_maxAllocatedStorage,
    restoreDBInstanceFromS3_port,
    restoreDBInstanceFromS3_performanceInsightsRetentionPeriod,
    restoreDBInstanceFromS3_vpcSecurityGroupIds,
    restoreDBInstanceFromS3_dbParameterGroupName,
    restoreDBInstanceFromS3_preferredBackupWindow,
    restoreDBInstanceFromS3_storageThroughput,
    restoreDBInstanceFromS3_backupRetentionPeriod,
    restoreDBInstanceFromS3_masterUsername,
    restoreDBInstanceFromS3_copyTagsToSnapshot,
    restoreDBInstanceFromS3_dbSubnetGroupName,
    restoreDBInstanceFromS3_autoMinorVersionUpgrade,
    restoreDBInstanceFromS3_optionGroupName,
    restoreDBInstanceFromS3_performanceInsightsKMSKeyId,
    restoreDBInstanceFromS3_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromS3_dbSecurityGroups,
    restoreDBInstanceFromS3_monitoringInterval,
    restoreDBInstanceFromS3_availabilityZone,
    restoreDBInstanceFromS3_masterUserPassword,
    restoreDBInstanceFromS3_publiclyAccessible,
    restoreDBInstanceFromS3_storageType,
    restoreDBInstanceFromS3_enableCloudwatchLogsExports,
    restoreDBInstanceFromS3_processorFeatures,
    restoreDBInstanceFromS3_enablePerformanceInsights,
    restoreDBInstanceFromS3_monitoringRoleArn,
    restoreDBInstanceFromS3_storageEncrypted,
    restoreDBInstanceFromS3_kmsKeyId,
    restoreDBInstanceFromS3_allocatedStorage,
    restoreDBInstanceFromS3_deletionProtection,
    restoreDBInstanceFromS3_preferredMaintenanceWindow,
    restoreDBInstanceFromS3_iops,
    restoreDBInstanceFromS3_engineVersion,
    restoreDBInstanceFromS3_dbName,
    restoreDBInstanceFromS3_networkType,
    restoreDBInstanceFromS3_multiAZ,
    restoreDBInstanceFromS3_s3Prefix,
    restoreDBInstanceFromS3_licenseModel,
    restoreDBInstanceFromS3_useDefaultProcessorFeatures,
    restoreDBInstanceFromS3_dbInstanceIdentifier,
    restoreDBInstanceFromS3_dbInstanceClass,
    restoreDBInstanceFromS3_engine,
    restoreDBInstanceFromS3_sourceEngine,
    restoreDBInstanceFromS3_sourceEngineVersion,
    restoreDBInstanceFromS3_s3BucketName,
    restoreDBInstanceFromS3_s3IngestionRoleArn,

    -- * Destructuring the Response
    RestoreDBInstanceFromS3Response (..),
    newRestoreDBInstanceFromS3Response,

    -- * Response Lenses
    restoreDBInstanceFromS3Response_dbInstance,
    restoreDBInstanceFromS3Response_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreDBInstanceFromS3' smart constructor.
data RestoreDBInstanceFromS3 = RestoreDBInstanceFromS3'
  { -- | A list of tags to associate with this DB instance. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources>
    -- in the /Amazon RDS User Guide./
    tags :: Prelude.Maybe [Tag],
    -- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
    -- scale the storage of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The port number on which the database accepts connections.
    --
    -- Type: Integer
    --
    -- Valid Values: @1150@-@65535@
    --
    -- Default: @3306@
    port :: Prelude.Maybe Prelude.Int,
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
    -- If you specify a retention period such as 94, which isn\'t a valid
    -- value, RDS issues an error.
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A list of VPC security groups to associate with this DB instance.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the DB parameter group to associate with this DB instance.
    --
    -- If you do not specify a value for @DBParameterGroupName@, then the
    -- default @DBParameterGroup@ for the specified DB engine is used.
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The time range each day during which automated backups are created if
    -- automated backups are enabled. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Backup window>
    -- in the /Amazon RDS User Guide./
    --
    -- Constraints:
    --
    -- -   Must be in the format @hh24:mi-hh24:mi@.
    --
    -- -   Must be in Universal Coordinated Time (UTC).
    --
    -- -   Must not conflict with the preferred maintenance window.
    --
    -- -   Must be at least 30 minutes.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the storage throughput value for the DB instance.
    --
    -- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
    storageThroughput :: Prelude.Maybe Prelude.Int,
    -- | The number of days for which automated backups are retained. Setting
    -- this parameter to a positive number enables backups. For more
    -- information, see @CreateDBInstance@.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The name for the master user.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 16 letters or numbers.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the DB instance to
    -- snapshots of the DB instance. By default, tags are not copied.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | A DB subnet group to associate with this DB instance.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mydbsubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the DB instance during the maintenance window. By
    -- default, minor engine upgrades are not applied automatically.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The name of the option group to associate with this DB instance. If this
    -- argument is omitted, the default option group for the specified engine
    -- is used.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for encryption of Performance
    -- Insights data.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
    -- Amazon RDS uses your default KMS key. There is a default KMS key for
    -- your Amazon Web Services account. Your Amazon Web Services account has a
    -- different default KMS key for each Amazon Web Services Region.
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping isn\'t enabled.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | A list of DB security groups to associate with this DB instance.
    --
    -- Default: The default DB security group for the database engine.
    dbSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance. To disable collecting
    -- Enhanced Monitoring metrics, specify 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set
    -- @MonitoringInterval@ to a value other than 0.
    --
    -- Valid Values: 0, 1, 5, 10, 15, 30, 60
    --
    -- Default: @0@
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The Availability Zone that the DB instance is created in. For
    -- information about Amazon Web Services Regions and Availability Zones,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>
    -- in the /Amazon RDS User Guide./
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint\'s
    -- Amazon Web Services Region.
    --
    -- Example: @us-east-1d@
    --
    -- Constraint: The @AvailabilityZone@ parameter can\'t be specified if the
    -- DB instance is a Multi-AZ deployment. The specified Availability Zone
    -- must be in the same Amazon Web Services Region as the current endpoint.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The password for the master user. The password can include any printable
    -- ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance is publicly accessible.
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
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @gp2 | gp3 | io1 | standard@
    --
    -- If you specify @io1@ or @gp3@, you must also include a value for the
    -- @Iops@ parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The list of logs that the restored DB instance is to export to
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | A value that indicates whether to enable Performance Insights for the DB
    -- instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon RDS User Guide/.
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring
    -- metrics to Amazon CloudWatch Logs. For example,
    -- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
    -- monitoring role, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
    -- in the /Amazon RDS User Guide./
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must
    -- supply a @MonitoringRoleArn@ value.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the new DB instance is encrypted or not.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services KMS key identifier for an encrypted DB instance.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key. To use a KMS key in a different
    -- Amazon Web Services account, specify the key ARN or alias ARN.
    --
    -- If the @StorageEncrypted@ parameter is enabled, and you do not specify a
    -- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
    -- default KMS key. There is a default KMS key for your Amazon Web Services
    -- account. Your Amazon Web Services account has a different default KMS
    -- key for each Amazon Web Services Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The amount of storage (in gigabytes) to allocate initially for the DB
    -- instance. Follow the allocation rules specified in @CreateDBInstance@.
    --
    -- Be sure to allocate enough memory for your new DB instance so that the
    -- restore operation can succeed. You can also allocate additional memory
    -- for future growth.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection isn\'t enabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The time range each week during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC). For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>
    -- in the /Amazon RDS User Guide./
    --
    -- Constraints:
    --
    -- -   Must be in the format @ddd:hh24:mi-ddd:hh24:mi@.
    --
    -- -   Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- -   Must be in Universal Coordinated Time (UTC).
    --
    -- -   Must not conflict with the preferred backup window.
    --
    -- -   Must be at least 30 minutes.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- allocate initially for the DB instance. For information about valid IOPS
    -- values, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
    -- in the /Amazon RDS User Guide./
    iops :: Prelude.Maybe Prelude.Int,
    -- | The version number of the database engine to use. Choose the latest
    -- minor version of your database engine. For information about engine
    -- versions, see @CreateDBInstance@, or call @DescribeDBEngineVersions@.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the database to create when the DB instance is created.
    -- Follow the naming rules specified in @CreateDBInstance@.
    dbName :: Prelude.Maybe Prelude.Text,
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
    -- in the /Amazon RDS User Guide./
    networkType :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    -- If the DB instance is a Multi-AZ deployment, you can\'t set the
    -- @AvailabilityZone@ parameter.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The prefix of your Amazon S3 bucket.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The license model for this DB instance. Use @general-public-license@.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    useDefaultProcessorFeatures :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance identifier. This parameter is stored as a lowercase
    -- string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @mydbinstance@
    dbInstanceIdentifier :: Prelude.Text,
    -- | The compute and memory capacity of the DB instance, for example
    -- db.m4.large. Not all DB instance classes are available in all Amazon Web
    -- Services Regions, or for all database engines. For the full list of DB
    -- instance classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    --
    -- Importing from Amazon S3 isn\'t supported on the db.t2.micro DB instance
    -- class.
    dbInstanceClass :: Prelude.Text,
    -- | The name of the database engine to be used for this instance.
    --
    -- Valid Values: @mysql@
    engine :: Prelude.Text,
    -- | The name of the engine of your source database.
    --
    -- Valid Values: @mysql@
    sourceEngine :: Prelude.Text,
    -- | The version of the database that the backup files were created from.
    --
    -- MySQL versions 5.6 and 5.7 are supported.
    --
    -- Example: @5.6.40@
    sourceEngineVersion :: Prelude.Text,
    -- | The name of your Amazon S3 bucket that contains your database backup
    -- file.
    s3BucketName :: Prelude.Text,
    -- | An Amazon Web Services Identity and Access Management (IAM) role to
    -- allow Amazon RDS to access your Amazon S3 bucket.
    s3IngestionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBInstanceFromS3' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'restoreDBInstanceFromS3_tags' - A list of tags to associate with this DB instance. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources>
-- in the /Amazon RDS User Guide./
--
-- 'maxAllocatedStorage', 'restoreDBInstanceFromS3_maxAllocatedStorage' - The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- 'port', 'restoreDBInstanceFromS3_port' - The port number on which the database accepts connections.
--
-- Type: Integer
--
-- Valid Values: @1150@-@65535@
--
-- Default: @3306@
--
-- 'performanceInsightsRetentionPeriod', 'restoreDBInstanceFromS3_performanceInsightsRetentionPeriod' - The number of days to retain Performance Insights data. The default is 7
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
-- If you specify a retention period such as 94, which isn\'t a valid
-- value, RDS issues an error.
--
-- 'vpcSecurityGroupIds', 'restoreDBInstanceFromS3_vpcSecurityGroupIds' - A list of VPC security groups to associate with this DB instance.
--
-- 'dbParameterGroupName', 'restoreDBInstanceFromS3_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then the
-- default @DBParameterGroup@ for the specified DB engine is used.
--
-- 'preferredBackupWindow', 'restoreDBInstanceFromS3_preferredBackupWindow' - The time range each day during which automated backups are created if
-- automated backups are enabled. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Backup window>
-- in the /Amazon RDS User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
--
-- 'storageThroughput', 'restoreDBInstanceFromS3_storageThroughput' - Specifies the storage throughput value for the DB instance.
--
-- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
--
-- 'backupRetentionPeriod', 'restoreDBInstanceFromS3_backupRetentionPeriod' - The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. For more
-- information, see @CreateDBInstance@.
--
-- 'masterUsername', 'restoreDBInstanceFromS3_masterUsername' - The name for the master user.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- 'copyTagsToSnapshot', 'restoreDBInstanceFromS3_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
--
-- 'dbSubnetGroupName', 'restoreDBInstanceFromS3_dbSubnetGroupName' - A DB subnet group to associate with this DB instance.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mydbsubnetgroup@
--
-- 'autoMinorVersionUpgrade', 'restoreDBInstanceFromS3_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the DB instance during the maintenance window. By
-- default, minor engine upgrades are not applied automatically.
--
-- 'optionGroupName', 'restoreDBInstanceFromS3_optionGroupName' - The name of the option group to associate with this DB instance. If this
-- argument is omitted, the default option group for the specified engine
-- is used.
--
-- 'performanceInsightsKMSKeyId', 'restoreDBInstanceFromS3_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default KMS key. There is a default KMS key for
-- your Amazon Web Services account. Your Amazon Web Services account has a
-- different default KMS key for each Amazon Web Services Region.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBInstanceFromS3_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- 'dbSecurityGroups', 'restoreDBInstanceFromS3_dbSecurityGroups' - A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
--
-- 'monitoringInterval', 'restoreDBInstanceFromS3_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: 0, 1, 5, 10, 15, 30, 60
--
-- Default: @0@
--
-- 'availabilityZone', 'restoreDBInstanceFromS3_availabilityZone' - The Availability Zone that the DB instance is created in. For
-- information about Amazon Web Services Regions and Availability Zones,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>
-- in the /Amazon RDS User Guide./
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- Amazon Web Services Region.
--
-- Example: @us-east-1d@
--
-- Constraint: The @AvailabilityZone@ parameter can\'t be specified if the
-- DB instance is a Multi-AZ deployment. The specified Availability Zone
-- must be in the same Amazon Web Services Region as the current endpoint.
--
-- 'masterUserPassword', 'restoreDBInstanceFromS3_masterUserPassword' - The password for the master user. The password can include any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- 'publiclyAccessible', 'restoreDBInstanceFromS3_publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
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
-- 'storageType', 'restoreDBInstanceFromS3_storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @gp2 | gp3 | io1 | standard@
--
-- If you specify @io1@ or @gp3@, you must also include a value for the
-- @Iops@ parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
--
-- 'enableCloudwatchLogsExports', 'restoreDBInstanceFromS3_enableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- 'processorFeatures', 'restoreDBInstanceFromS3_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'enablePerformanceInsights', 'restoreDBInstanceFromS3_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- 'monitoringRoleArn', 'restoreDBInstanceFromS3_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
--
-- 'storageEncrypted', 'restoreDBInstanceFromS3_storageEncrypted' - A value that indicates whether the new DB instance is encrypted or not.
--
-- 'kmsKeyId', 'restoreDBInstanceFromS3_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- If the @StorageEncrypted@ parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default KMS key. There is a default KMS key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default KMS
-- key for each Amazon Web Services Region.
--
-- 'allocatedStorage', 'restoreDBInstanceFromS3_allocatedStorage' - The amount of storage (in gigabytes) to allocate initially for the DB
-- instance. Follow the allocation rules specified in @CreateDBInstance@.
--
-- Be sure to allocate enough memory for your new DB instance so that the
-- restore operation can succeed. You can also allocate additional memory
-- for future growth.
--
-- 'deletionProtection', 'restoreDBInstanceFromS3_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'preferredMaintenanceWindow', 'restoreDBInstanceFromS3_preferredMaintenanceWindow' - The time range each week during which system maintenance can occur, in
-- Universal Coordinated Time (UTC). For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- Constraints:
--
-- -   Must be in the format @ddd:hh24:mi-ddd:hh24:mi@.
--
-- -   Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred backup window.
--
-- -   Must be at least 30 minutes.
--
-- 'iops', 'restoreDBInstanceFromS3_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- allocate initially for the DB instance. For information about valid IOPS
-- values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide./
--
-- 'engineVersion', 'restoreDBInstanceFromS3_engineVersion' - The version number of the database engine to use. Choose the latest
-- minor version of your database engine. For information about engine
-- versions, see @CreateDBInstance@, or call @DescribeDBEngineVersions@.
--
-- 'dbName', 'restoreDBInstanceFromS3_dbName' - The name of the database to create when the DB instance is created.
-- Follow the naming rules specified in @CreateDBInstance@.
--
-- 'networkType', 'restoreDBInstanceFromS3_networkType' - The network type of the DB instance.
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
-- in the /Amazon RDS User Guide./
--
-- 'multiAZ', 'restoreDBInstanceFromS3_multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
-- If the DB instance is a Multi-AZ deployment, you can\'t set the
-- @AvailabilityZone@ parameter.
--
-- 's3Prefix', 'restoreDBInstanceFromS3_s3Prefix' - The prefix of your Amazon S3 bucket.
--
-- 'licenseModel', 'restoreDBInstanceFromS3_licenseModel' - The license model for this DB instance. Use @general-public-license@.
--
-- 'useDefaultProcessorFeatures', 'restoreDBInstanceFromS3_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- 'dbInstanceIdentifier', 'restoreDBInstanceFromS3_dbInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
--
-- 'dbInstanceClass', 'restoreDBInstanceFromS3_dbInstanceClass' - The compute and memory capacity of the DB instance, for example
-- db.m4.large. Not all DB instance classes are available in all Amazon Web
-- Services Regions, or for all database engines. For the full list of DB
-- instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Importing from Amazon S3 isn\'t supported on the db.t2.micro DB instance
-- class.
--
-- 'engine', 'restoreDBInstanceFromS3_engine' - The name of the database engine to be used for this instance.
--
-- Valid Values: @mysql@
--
-- 'sourceEngine', 'restoreDBInstanceFromS3_sourceEngine' - The name of the engine of your source database.
--
-- Valid Values: @mysql@
--
-- 'sourceEngineVersion', 'restoreDBInstanceFromS3_sourceEngineVersion' - The version of the database that the backup files were created from.
--
-- MySQL versions 5.6 and 5.7 are supported.
--
-- Example: @5.6.40@
--
-- 's3BucketName', 'restoreDBInstanceFromS3_s3BucketName' - The name of your Amazon S3 bucket that contains your database backup
-- file.
--
-- 's3IngestionRoleArn', 'restoreDBInstanceFromS3_s3IngestionRoleArn' - An Amazon Web Services Identity and Access Management (IAM) role to
-- allow Amazon RDS to access your Amazon S3 bucket.
newRestoreDBInstanceFromS3 ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  -- | 'dbInstanceClass'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  -- | 'sourceEngine'
  Prelude.Text ->
  -- | 'sourceEngineVersion'
  Prelude.Text ->
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 's3IngestionRoleArn'
  Prelude.Text ->
  RestoreDBInstanceFromS3
newRestoreDBInstanceFromS3
  pDBInstanceIdentifier_
  pDBInstanceClass_
  pEngine_
  pSourceEngine_
  pSourceEngineVersion_
  pS3BucketName_
  pS3IngestionRoleArn_ =
    RestoreDBInstanceFromS3'
      { tags = Prelude.Nothing,
        maxAllocatedStorage = Prelude.Nothing,
        port = Prelude.Nothing,
        performanceInsightsRetentionPeriod =
          Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        preferredBackupWindow = Prelude.Nothing,
        storageThroughput = Prelude.Nothing,
        backupRetentionPeriod = Prelude.Nothing,
        masterUsername = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        performanceInsightsKMSKeyId = Prelude.Nothing,
        enableIAMDatabaseAuthentication = Prelude.Nothing,
        dbSecurityGroups = Prelude.Nothing,
        monitoringInterval = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        masterUserPassword = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        storageType = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        enablePerformanceInsights = Prelude.Nothing,
        monitoringRoleArn = Prelude.Nothing,
        storageEncrypted = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        allocatedStorage = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        iops = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        dbName = Prelude.Nothing,
        networkType = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        s3Prefix = Prelude.Nothing,
        licenseModel = Prelude.Nothing,
        useDefaultProcessorFeatures = Prelude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        dbInstanceClass = pDBInstanceClass_,
        engine = pEngine_,
        sourceEngine = pSourceEngine_,
        sourceEngineVersion = pSourceEngineVersion_,
        s3BucketName = pS3BucketName_,
        s3IngestionRoleArn = pS3IngestionRoleArn_
      }

-- | A list of tags to associate with this DB instance. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromS3_tags :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe [Tag])
restoreDBInstanceFromS3_tags = Lens.lens (\RestoreDBInstanceFromS3' {tags} -> tags) (\s@RestoreDBInstanceFromS3' {} a -> s {tags = a} :: RestoreDBInstanceFromS3) Prelude.. Lens.mapping Lens.coerced

-- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromS3_maxAllocatedStorage :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS3_maxAllocatedStorage = Lens.lens (\RestoreDBInstanceFromS3' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@RestoreDBInstanceFromS3' {} a -> s {maxAllocatedStorage = a} :: RestoreDBInstanceFromS3)

-- | The port number on which the database accepts connections.
--
-- Type: Integer
--
-- Valid Values: @1150@-@65535@
--
-- Default: @3306@
restoreDBInstanceFromS3_port :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS3_port = Lens.lens (\RestoreDBInstanceFromS3' {port} -> port) (\s@RestoreDBInstanceFromS3' {} a -> s {port = a} :: RestoreDBInstanceFromS3)

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
-- If you specify a retention period such as 94, which isn\'t a valid
-- value, RDS issues an error.
restoreDBInstanceFromS3_performanceInsightsRetentionPeriod :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS3_performanceInsightsRetentionPeriod = Lens.lens (\RestoreDBInstanceFromS3' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@RestoreDBInstanceFromS3' {} a -> s {performanceInsightsRetentionPeriod = a} :: RestoreDBInstanceFromS3)

-- | A list of VPC security groups to associate with this DB instance.
restoreDBInstanceFromS3_vpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe [Prelude.Text])
restoreDBInstanceFromS3_vpcSecurityGroupIds = Lens.lens (\RestoreDBInstanceFromS3' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBInstanceFromS3' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBInstanceFromS3) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then the
-- default @DBParameterGroup@ for the specified DB engine is used.
restoreDBInstanceFromS3_dbParameterGroupName :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_dbParameterGroupName = Lens.lens (\RestoreDBInstanceFromS3' {dbParameterGroupName} -> dbParameterGroupName) (\s@RestoreDBInstanceFromS3' {} a -> s {dbParameterGroupName = a} :: RestoreDBInstanceFromS3)

-- | The time range each day during which automated backups are created if
-- automated backups are enabled. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Backup window>
-- in the /Amazon RDS User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
restoreDBInstanceFromS3_preferredBackupWindow :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_preferredBackupWindow = Lens.lens (\RestoreDBInstanceFromS3' {preferredBackupWindow} -> preferredBackupWindow) (\s@RestoreDBInstanceFromS3' {} a -> s {preferredBackupWindow = a} :: RestoreDBInstanceFromS3)

-- | Specifies the storage throughput value for the DB instance.
--
-- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
restoreDBInstanceFromS3_storageThroughput :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS3_storageThroughput = Lens.lens (\RestoreDBInstanceFromS3' {storageThroughput} -> storageThroughput) (\s@RestoreDBInstanceFromS3' {} a -> s {storageThroughput = a} :: RestoreDBInstanceFromS3)

-- | The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. For more
-- information, see @CreateDBInstance@.
restoreDBInstanceFromS3_backupRetentionPeriod :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS3_backupRetentionPeriod = Lens.lens (\RestoreDBInstanceFromS3' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@RestoreDBInstanceFromS3' {} a -> s {backupRetentionPeriod = a} :: RestoreDBInstanceFromS3)

-- | The name for the master user.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
restoreDBInstanceFromS3_masterUsername :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_masterUsername = Lens.lens (\RestoreDBInstanceFromS3' {masterUsername} -> masterUsername) (\s@RestoreDBInstanceFromS3' {} a -> s {masterUsername = a} :: RestoreDBInstanceFromS3)

-- | A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
restoreDBInstanceFromS3_copyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS3_copyTagsToSnapshot = Lens.lens (\RestoreDBInstanceFromS3' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBInstanceFromS3' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBInstanceFromS3)

-- | A DB subnet group to associate with this DB instance.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mydbsubnetgroup@
restoreDBInstanceFromS3_dbSubnetGroupName :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_dbSubnetGroupName = Lens.lens (\RestoreDBInstanceFromS3' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBInstanceFromS3' {} a -> s {dbSubnetGroupName = a} :: RestoreDBInstanceFromS3)

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the DB instance during the maintenance window. By
-- default, minor engine upgrades are not applied automatically.
restoreDBInstanceFromS3_autoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS3_autoMinorVersionUpgrade = Lens.lens (\RestoreDBInstanceFromS3' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@RestoreDBInstanceFromS3' {} a -> s {autoMinorVersionUpgrade = a} :: RestoreDBInstanceFromS3)

-- | The name of the option group to associate with this DB instance. If this
-- argument is omitted, the default option group for the specified engine
-- is used.
restoreDBInstanceFromS3_optionGroupName :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_optionGroupName = Lens.lens (\RestoreDBInstanceFromS3' {optionGroupName} -> optionGroupName) (\s@RestoreDBInstanceFromS3' {} a -> s {optionGroupName = a} :: RestoreDBInstanceFromS3)

-- | The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default KMS key. There is a default KMS key for
-- your Amazon Web Services account. Your Amazon Web Services account has a
-- different default KMS key for each Amazon Web Services Region.
restoreDBInstanceFromS3_performanceInsightsKMSKeyId :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_performanceInsightsKMSKeyId = Lens.lens (\RestoreDBInstanceFromS3' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@RestoreDBInstanceFromS3' {} a -> s {performanceInsightsKMSKeyId = a} :: RestoreDBInstanceFromS3)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromS3_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS3_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBInstanceFromS3' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBInstanceFromS3' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBInstanceFromS3)

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
restoreDBInstanceFromS3_dbSecurityGroups :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe [Prelude.Text])
restoreDBInstanceFromS3_dbSecurityGroups = Lens.lens (\RestoreDBInstanceFromS3' {dbSecurityGroups} -> dbSecurityGroups) (\s@RestoreDBInstanceFromS3' {} a -> s {dbSecurityGroups = a} :: RestoreDBInstanceFromS3) Prelude.. Lens.mapping Lens.coerced

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: 0, 1, 5, 10, 15, 30, 60
--
-- Default: @0@
restoreDBInstanceFromS3_monitoringInterval :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS3_monitoringInterval = Lens.lens (\RestoreDBInstanceFromS3' {monitoringInterval} -> monitoringInterval) (\s@RestoreDBInstanceFromS3' {} a -> s {monitoringInterval = a} :: RestoreDBInstanceFromS3)

-- | The Availability Zone that the DB instance is created in. For
-- information about Amazon Web Services Regions and Availability Zones,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>
-- in the /Amazon RDS User Guide./
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- Amazon Web Services Region.
--
-- Example: @us-east-1d@
--
-- Constraint: The @AvailabilityZone@ parameter can\'t be specified if the
-- DB instance is a Multi-AZ deployment. The specified Availability Zone
-- must be in the same Amazon Web Services Region as the current endpoint.
restoreDBInstanceFromS3_availabilityZone :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_availabilityZone = Lens.lens (\RestoreDBInstanceFromS3' {availabilityZone} -> availabilityZone) (\s@RestoreDBInstanceFromS3' {} a -> s {availabilityZone = a} :: RestoreDBInstanceFromS3)

-- | The password for the master user. The password can include any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
restoreDBInstanceFromS3_masterUserPassword :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_masterUserPassword = Lens.lens (\RestoreDBInstanceFromS3' {masterUserPassword} -> masterUserPassword) (\s@RestoreDBInstanceFromS3' {} a -> s {masterUserPassword = a} :: RestoreDBInstanceFromS3)

-- | A value that indicates whether the DB instance is publicly accessible.
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
restoreDBInstanceFromS3_publiclyAccessible :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS3_publiclyAccessible = Lens.lens (\RestoreDBInstanceFromS3' {publiclyAccessible} -> publiclyAccessible) (\s@RestoreDBInstanceFromS3' {} a -> s {publiclyAccessible = a} :: RestoreDBInstanceFromS3)

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @gp2 | gp3 | io1 | standard@
--
-- If you specify @io1@ or @gp3@, you must also include a value for the
-- @Iops@ parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
restoreDBInstanceFromS3_storageType :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_storageType = Lens.lens (\RestoreDBInstanceFromS3' {storageType} -> storageType) (\s@RestoreDBInstanceFromS3' {} a -> s {storageType = a} :: RestoreDBInstanceFromS3)

-- | The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromS3_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe [Prelude.Text])
restoreDBInstanceFromS3_enableCloudwatchLogsExports = Lens.lens (\RestoreDBInstanceFromS3' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBInstanceFromS3' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBInstanceFromS3) Prelude.. Lens.mapping Lens.coerced

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
restoreDBInstanceFromS3_processorFeatures :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe [ProcessorFeature])
restoreDBInstanceFromS3_processorFeatures = Lens.lens (\RestoreDBInstanceFromS3' {processorFeatures} -> processorFeatures) (\s@RestoreDBInstanceFromS3' {} a -> s {processorFeatures = a} :: RestoreDBInstanceFromS3) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromS3_enablePerformanceInsights :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS3_enablePerformanceInsights = Lens.lens (\RestoreDBInstanceFromS3' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@RestoreDBInstanceFromS3' {} a -> s {enablePerformanceInsights = a} :: RestoreDBInstanceFromS3)

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
restoreDBInstanceFromS3_monitoringRoleArn :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_monitoringRoleArn = Lens.lens (\RestoreDBInstanceFromS3' {monitoringRoleArn} -> monitoringRoleArn) (\s@RestoreDBInstanceFromS3' {} a -> s {monitoringRoleArn = a} :: RestoreDBInstanceFromS3)

-- | A value that indicates whether the new DB instance is encrypted or not.
restoreDBInstanceFromS3_storageEncrypted :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS3_storageEncrypted = Lens.lens (\RestoreDBInstanceFromS3' {storageEncrypted} -> storageEncrypted) (\s@RestoreDBInstanceFromS3' {} a -> s {storageEncrypted = a} :: RestoreDBInstanceFromS3)

-- | The Amazon Web Services KMS key identifier for an encrypted DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- If the @StorageEncrypted@ parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default KMS key. There is a default KMS key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default KMS
-- key for each Amazon Web Services Region.
restoreDBInstanceFromS3_kmsKeyId :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_kmsKeyId = Lens.lens (\RestoreDBInstanceFromS3' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBInstanceFromS3' {} a -> s {kmsKeyId = a} :: RestoreDBInstanceFromS3)

-- | The amount of storage (in gigabytes) to allocate initially for the DB
-- instance. Follow the allocation rules specified in @CreateDBInstance@.
--
-- Be sure to allocate enough memory for your new DB instance so that the
-- restore operation can succeed. You can also allocate additional memory
-- for future growth.
restoreDBInstanceFromS3_allocatedStorage :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS3_allocatedStorage = Lens.lens (\RestoreDBInstanceFromS3' {allocatedStorage} -> allocatedStorage) (\s@RestoreDBInstanceFromS3' {} a -> s {allocatedStorage = a} :: RestoreDBInstanceFromS3)

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
restoreDBInstanceFromS3_deletionProtection :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS3_deletionProtection = Lens.lens (\RestoreDBInstanceFromS3' {deletionProtection} -> deletionProtection) (\s@RestoreDBInstanceFromS3' {} a -> s {deletionProtection = a} :: RestoreDBInstanceFromS3)

-- | The time range each week during which system maintenance can occur, in
-- Universal Coordinated Time (UTC). For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- Constraints:
--
-- -   Must be in the format @ddd:hh24:mi-ddd:hh24:mi@.
--
-- -   Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred backup window.
--
-- -   Must be at least 30 minutes.
restoreDBInstanceFromS3_preferredMaintenanceWindow :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_preferredMaintenanceWindow = Lens.lens (\RestoreDBInstanceFromS3' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@RestoreDBInstanceFromS3' {} a -> s {preferredMaintenanceWindow = a} :: RestoreDBInstanceFromS3)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- allocate initially for the DB instance. For information about valid IOPS
-- values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromS3_iops :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS3_iops = Lens.lens (\RestoreDBInstanceFromS3' {iops} -> iops) (\s@RestoreDBInstanceFromS3' {} a -> s {iops = a} :: RestoreDBInstanceFromS3)

-- | The version number of the database engine to use. Choose the latest
-- minor version of your database engine. For information about engine
-- versions, see @CreateDBInstance@, or call @DescribeDBEngineVersions@.
restoreDBInstanceFromS3_engineVersion :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_engineVersion = Lens.lens (\RestoreDBInstanceFromS3' {engineVersion} -> engineVersion) (\s@RestoreDBInstanceFromS3' {} a -> s {engineVersion = a} :: RestoreDBInstanceFromS3)

-- | The name of the database to create when the DB instance is created.
-- Follow the naming rules specified in @CreateDBInstance@.
restoreDBInstanceFromS3_dbName :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_dbName = Lens.lens (\RestoreDBInstanceFromS3' {dbName} -> dbName) (\s@RestoreDBInstanceFromS3' {} a -> s {dbName = a} :: RestoreDBInstanceFromS3)

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
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromS3_networkType :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_networkType = Lens.lens (\RestoreDBInstanceFromS3' {networkType} -> networkType) (\s@RestoreDBInstanceFromS3' {} a -> s {networkType = a} :: RestoreDBInstanceFromS3)

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
-- If the DB instance is a Multi-AZ deployment, you can\'t set the
-- @AvailabilityZone@ parameter.
restoreDBInstanceFromS3_multiAZ :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS3_multiAZ = Lens.lens (\RestoreDBInstanceFromS3' {multiAZ} -> multiAZ) (\s@RestoreDBInstanceFromS3' {} a -> s {multiAZ = a} :: RestoreDBInstanceFromS3)

-- | The prefix of your Amazon S3 bucket.
restoreDBInstanceFromS3_s3Prefix :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_s3Prefix = Lens.lens (\RestoreDBInstanceFromS3' {s3Prefix} -> s3Prefix) (\s@RestoreDBInstanceFromS3' {} a -> s {s3Prefix = a} :: RestoreDBInstanceFromS3)

-- | The license model for this DB instance. Use @general-public-license@.
restoreDBInstanceFromS3_licenseModel :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS3_licenseModel = Lens.lens (\RestoreDBInstanceFromS3' {licenseModel} -> licenseModel) (\s@RestoreDBInstanceFromS3' {} a -> s {licenseModel = a} :: RestoreDBInstanceFromS3)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
restoreDBInstanceFromS3_useDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS3_useDefaultProcessorFeatures = Lens.lens (\RestoreDBInstanceFromS3' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@RestoreDBInstanceFromS3' {} a -> s {useDefaultProcessorFeatures = a} :: RestoreDBInstanceFromS3)

-- | The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
restoreDBInstanceFromS3_dbInstanceIdentifier :: Lens.Lens' RestoreDBInstanceFromS3 Prelude.Text
restoreDBInstanceFromS3_dbInstanceIdentifier = Lens.lens (\RestoreDBInstanceFromS3' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RestoreDBInstanceFromS3' {} a -> s {dbInstanceIdentifier = a} :: RestoreDBInstanceFromS3)

-- | The compute and memory capacity of the DB instance, for example
-- db.m4.large. Not all DB instance classes are available in all Amazon Web
-- Services Regions, or for all database engines. For the full list of DB
-- instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Importing from Amazon S3 isn\'t supported on the db.t2.micro DB instance
-- class.
restoreDBInstanceFromS3_dbInstanceClass :: Lens.Lens' RestoreDBInstanceFromS3 Prelude.Text
restoreDBInstanceFromS3_dbInstanceClass = Lens.lens (\RestoreDBInstanceFromS3' {dbInstanceClass} -> dbInstanceClass) (\s@RestoreDBInstanceFromS3' {} a -> s {dbInstanceClass = a} :: RestoreDBInstanceFromS3)

-- | The name of the database engine to be used for this instance.
--
-- Valid Values: @mysql@
restoreDBInstanceFromS3_engine :: Lens.Lens' RestoreDBInstanceFromS3 Prelude.Text
restoreDBInstanceFromS3_engine = Lens.lens (\RestoreDBInstanceFromS3' {engine} -> engine) (\s@RestoreDBInstanceFromS3' {} a -> s {engine = a} :: RestoreDBInstanceFromS3)

-- | The name of the engine of your source database.
--
-- Valid Values: @mysql@
restoreDBInstanceFromS3_sourceEngine :: Lens.Lens' RestoreDBInstanceFromS3 Prelude.Text
restoreDBInstanceFromS3_sourceEngine = Lens.lens (\RestoreDBInstanceFromS3' {sourceEngine} -> sourceEngine) (\s@RestoreDBInstanceFromS3' {} a -> s {sourceEngine = a} :: RestoreDBInstanceFromS3)

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.6 and 5.7 are supported.
--
-- Example: @5.6.40@
restoreDBInstanceFromS3_sourceEngineVersion :: Lens.Lens' RestoreDBInstanceFromS3 Prelude.Text
restoreDBInstanceFromS3_sourceEngineVersion = Lens.lens (\RestoreDBInstanceFromS3' {sourceEngineVersion} -> sourceEngineVersion) (\s@RestoreDBInstanceFromS3' {} a -> s {sourceEngineVersion = a} :: RestoreDBInstanceFromS3)

-- | The name of your Amazon S3 bucket that contains your database backup
-- file.
restoreDBInstanceFromS3_s3BucketName :: Lens.Lens' RestoreDBInstanceFromS3 Prelude.Text
restoreDBInstanceFromS3_s3BucketName = Lens.lens (\RestoreDBInstanceFromS3' {s3BucketName} -> s3BucketName) (\s@RestoreDBInstanceFromS3' {} a -> s {s3BucketName = a} :: RestoreDBInstanceFromS3)

-- | An Amazon Web Services Identity and Access Management (IAM) role to
-- allow Amazon RDS to access your Amazon S3 bucket.
restoreDBInstanceFromS3_s3IngestionRoleArn :: Lens.Lens' RestoreDBInstanceFromS3 Prelude.Text
restoreDBInstanceFromS3_s3IngestionRoleArn = Lens.lens (\RestoreDBInstanceFromS3' {s3IngestionRoleArn} -> s3IngestionRoleArn) (\s@RestoreDBInstanceFromS3' {} a -> s {s3IngestionRoleArn = a} :: RestoreDBInstanceFromS3)

instance Core.AWSRequest RestoreDBInstanceFromS3 where
  type
    AWSResponse RestoreDBInstanceFromS3 =
      RestoreDBInstanceFromS3Response
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RestoreDBInstanceFromS3Result"
      ( \s h x ->
          RestoreDBInstanceFromS3Response'
            Prelude.<$> (x Core..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreDBInstanceFromS3 where
  hashWithSalt _salt RestoreDBInstanceFromS3' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` maxAllocatedStorage
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` performanceInsightsRetentionPeriod
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` storageThroughput
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` dbSecurityGroups
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` enablePerformanceInsights
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` s3Prefix
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` useDefaultProcessorFeatures
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` sourceEngine
      `Prelude.hashWithSalt` sourceEngineVersion
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3IngestionRoleArn

instance Prelude.NFData RestoreDBInstanceFromS3 where
  rnf RestoreDBInstanceFromS3' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf maxAllocatedStorage
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf performanceInsightsRetentionPeriod
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf storageThroughput
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf dbSecurityGroups
      `Prelude.seq` Prelude.rnf monitoringInterval
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf
        enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf
        processorFeatures
      `Prelude.seq` Prelude.rnf
        enablePerformanceInsights
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf
        storageEncrypted
      `Prelude.seq` Prelude.rnf
        kmsKeyId
      `Prelude.seq` Prelude.rnf
        allocatedStorage
      `Prelude.seq` Prelude.rnf
        deletionProtection
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        iops
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        dbName
      `Prelude.seq` Prelude.rnf
        networkType
      `Prelude.seq` Prelude.rnf
        multiAZ
      `Prelude.seq` Prelude.rnf
        s3Prefix
      `Prelude.seq` Prelude.rnf
        licenseModel
      `Prelude.seq` Prelude.rnf
        useDefaultProcessorFeatures
      `Prelude.seq` Prelude.rnf
        dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf
        dbInstanceClass
      `Prelude.seq` Prelude.rnf
        engine
      `Prelude.seq` Prelude.rnf
        sourceEngine
      `Prelude.seq` Prelude.rnf
        sourceEngineVersion
      `Prelude.seq` Prelude.rnf
        s3BucketName
      `Prelude.seq` Prelude.rnf
        s3IngestionRoleArn

instance Core.ToHeaders RestoreDBInstanceFromS3 where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RestoreDBInstanceFromS3 where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreDBInstanceFromS3 where
  toQuery RestoreDBInstanceFromS3' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RestoreDBInstanceFromS3" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "MaxAllocatedStorage" Core.=: maxAllocatedStorage,
        "Port" Core.=: port,
        "PerformanceInsightsRetentionPeriod"
          Core.=: performanceInsightsRetentionPeriod,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "StorageThroughput" Core.=: storageThroughput,
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "MasterUsername" Core.=: masterUsername,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "OptionGroupName" Core.=: optionGroupName,
        "PerformanceInsightsKMSKeyId"
          Core.=: performanceInsightsKMSKeyId,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "DBSecurityGroups"
          Core.=: Core.toQuery
            ( Core.toQueryList "DBSecurityGroupName"
                Prelude.<$> dbSecurityGroups
            ),
        "MonitoringInterval" Core.=: monitoringInterval,
        "AvailabilityZone" Core.=: availabilityZone,
        "MasterUserPassword" Core.=: masterUserPassword,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "StorageType" Core.=: storageType,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "ProcessorFeatures"
          Core.=: Core.toQuery
            ( Core.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "EnablePerformanceInsights"
          Core.=: enablePerformanceInsights,
        "MonitoringRoleArn" Core.=: monitoringRoleArn,
        "StorageEncrypted" Core.=: storageEncrypted,
        "KmsKeyId" Core.=: kmsKeyId,
        "AllocatedStorage" Core.=: allocatedStorage,
        "DeletionProtection" Core.=: deletionProtection,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "Iops" Core.=: iops,
        "EngineVersion" Core.=: engineVersion,
        "DBName" Core.=: dbName,
        "NetworkType" Core.=: networkType,
        "MultiAZ" Core.=: multiAZ,
        "S3Prefix" Core.=: s3Prefix,
        "LicenseModel" Core.=: licenseModel,
        "UseDefaultProcessorFeatures"
          Core.=: useDefaultProcessorFeatures,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier,
        "DBInstanceClass" Core.=: dbInstanceClass,
        "Engine" Core.=: engine,
        "SourceEngine" Core.=: sourceEngine,
        "SourceEngineVersion" Core.=: sourceEngineVersion,
        "S3BucketName" Core.=: s3BucketName,
        "S3IngestionRoleArn" Core.=: s3IngestionRoleArn
      ]

-- | /See:/ 'newRestoreDBInstanceFromS3Response' smart constructor.
data RestoreDBInstanceFromS3Response = RestoreDBInstanceFromS3Response'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBInstanceFromS3Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'restoreDBInstanceFromS3Response_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'restoreDBInstanceFromS3Response_httpStatus' - The response's http status code.
newRestoreDBInstanceFromS3Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreDBInstanceFromS3Response
newRestoreDBInstanceFromS3Response pHttpStatus_ =
  RestoreDBInstanceFromS3Response'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreDBInstanceFromS3Response_dbInstance :: Lens.Lens' RestoreDBInstanceFromS3Response (Prelude.Maybe DBInstance)
restoreDBInstanceFromS3Response_dbInstance = Lens.lens (\RestoreDBInstanceFromS3Response' {dbInstance} -> dbInstance) (\s@RestoreDBInstanceFromS3Response' {} a -> s {dbInstance = a} :: RestoreDBInstanceFromS3Response)

-- | The response's http status code.
restoreDBInstanceFromS3Response_httpStatus :: Lens.Lens' RestoreDBInstanceFromS3Response Prelude.Int
restoreDBInstanceFromS3Response_httpStatus = Lens.lens (\RestoreDBInstanceFromS3Response' {httpStatus} -> httpStatus) (\s@RestoreDBInstanceFromS3Response' {} a -> s {httpStatus = a} :: RestoreDBInstanceFromS3Response)

instance
  Prelude.NFData
    RestoreDBInstanceFromS3Response
  where
  rnf RestoreDBInstanceFromS3Response' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
