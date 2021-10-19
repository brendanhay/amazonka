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
-- Module      : Network.AWS.RDS.RestoreDBInstanceFromS
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.RDS.RestoreDBInstanceFromS
  ( -- * Creating a Request
    RestoreDBInstanceFromS (..),
    newRestoreDBInstanceFromS,

    -- * Request Lenses
    restoreDBInstanceFromS_engineVersion,
    restoreDBInstanceFromS_dbSecurityGroups,
    restoreDBInstanceFromS_deletionProtection,
    restoreDBInstanceFromS_storageEncrypted,
    restoreDBInstanceFromS_masterUserPassword,
    restoreDBInstanceFromS_publiclyAccessible,
    restoreDBInstanceFromS_autoMinorVersionUpgrade,
    restoreDBInstanceFromS_masterUsername,
    restoreDBInstanceFromS_dbSubnetGroupName,
    restoreDBInstanceFromS_monitoringRoleArn,
    restoreDBInstanceFromS_iops,
    restoreDBInstanceFromS_monitoringInterval,
    restoreDBInstanceFromS_processorFeatures,
    restoreDBInstanceFromS_licenseModel,
    restoreDBInstanceFromS_preferredMaintenanceWindow,
    restoreDBInstanceFromS_performanceInsightsRetentionPeriod,
    restoreDBInstanceFromS_maxAllocatedStorage,
    restoreDBInstanceFromS_enablePerformanceInsights,
    restoreDBInstanceFromS_kmsKeyId,
    restoreDBInstanceFromS_dbParameterGroupName,
    restoreDBInstanceFromS_preferredBackupWindow,
    restoreDBInstanceFromS_availabilityZone,
    restoreDBInstanceFromS_backupRetentionPeriod,
    restoreDBInstanceFromS_performanceInsightsKMSKeyId,
    restoreDBInstanceFromS_vpcSecurityGroupIds,
    restoreDBInstanceFromS_multiAZ,
    restoreDBInstanceFromS_s3Prefix,
    restoreDBInstanceFromS_allocatedStorage,
    restoreDBInstanceFromS_optionGroupName,
    restoreDBInstanceFromS_copyTagsToSnapshot,
    restoreDBInstanceFromS_tags,
    restoreDBInstanceFromS_port,
    restoreDBInstanceFromS_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromS_useDefaultProcessorFeatures,
    restoreDBInstanceFromS_storageType,
    restoreDBInstanceFromS_enableCloudwatchLogsExports,
    restoreDBInstanceFromS_dbName,
    restoreDBInstanceFromS_dbInstanceIdentifier,
    restoreDBInstanceFromS_dbInstanceClass,
    restoreDBInstanceFromS_engine,
    restoreDBInstanceFromS_sourceEngine,
    restoreDBInstanceFromS_sourceEngineVersion,
    restoreDBInstanceFromS_s3BucketName,
    restoreDBInstanceFromS_s3IngestionRoleArn,

    -- * Destructuring the Response
    RestoreDBInstanceFromSResponse (..),
    newRestoreDBInstanceFromSResponse,

    -- * Response Lenses
    restoreDBInstanceFromSResponse_dbInstance,
    restoreDBInstanceFromSResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRestoreDBInstanceFromS' smart constructor.
data RestoreDBInstanceFromS = RestoreDBInstanceFromS'
  { -- | The version number of the database engine to use. Choose the latest
    -- minor version of your database engine. For information about engine
    -- versions, see @CreateDBInstance@, or call @DescribeDBEngineVersions@.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of DB security groups to associate with this DB instance.
    --
    -- Default: The default DB security group for the database engine.
    dbSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the new DB instance is encrypted or not.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The password for the master user. The password can include any printable
    -- ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance is publicly accessible.
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
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the DB instance during the maintenance window. By
    -- default, minor engine upgrades are not applied automatically.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
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
    -- | A DB subnet group to associate with this DB instance.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
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
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- allocate initially for the DB instance. For information about valid Iops
    -- values, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
    -- in the /Amazon RDS User Guide./
    iops :: Prelude.Maybe Prelude.Int,
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
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | The license model for this DB instance. Use @general-public-license@.
    licenseModel :: Prelude.Maybe Prelude.Text,
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
    -- | The amount of time, in days, to retain Performance Insights data. Valid
    -- values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
    -- scale the storage of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to enable Performance Insights for the DB
    -- instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon Relational Database Service User Guide/.
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services KMS key identifier for an encrypted DB instance.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK). To use a CMK in a different Amazon Web Services account, specify
    -- the key ARN or alias ARN.
    --
    -- If the @StorageEncrypted@ parameter is enabled, and you do not specify a
    -- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
    -- default CMK. There is a default CMK for your Amazon Web Services
    -- account. Your Amazon Web Services account has a different default CMK
    -- for each Amazon Web Services Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
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
    -- | The number of days for which automated backups are retained. Setting
    -- this parameter to a positive number enables backups. For more
    -- information, see @CreateDBInstance@.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services KMS key identifier for encryption of Performance
    -- Insights data.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK).
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
    -- Amazon RDS uses your default CMK. There is a default CMK for your Amazon
    -- Web Services account. Your Amazon Web Services account has a different
    -- default CMK for each Amazon Web Services Region.
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of VPC security groups to associate with this DB instance.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    -- If the DB instance is a Multi-AZ deployment, you can\'t set the
    -- @AvailabilityZone@ parameter.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The prefix of your Amazon S3 bucket.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The amount of storage (in gigabytes) to allocate initially for the DB
    -- instance. Follow the allocation rules specified in @CreateDBInstance@.
    --
    -- Be sure to allocate enough memory for your new DB instance so that the
    -- restore operation can succeed. You can also allocate additional memory
    -- for future growth.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The name of the option group to associate with this DB instance. If this
    -- argument is omitted, the default option group for the specified engine
    -- is used.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the DB instance to
    -- snapshots of the DB instance. By default, tags are not copied.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | A list of tags to associate with this DB instance. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources>
    -- in the /Amazon RDS User Guide./
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which the database accepts connections.
    --
    -- Type: Integer
    --
    -- Valid Values: @1150@-@65535@
    --
    -- Default: @3306@
    port :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping is disabled.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    useDefaultProcessorFeatures :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @standard@ | @gp2@ | @io1@
    --
    -- If you specify @io1@, you must also include a value for the @Iops@
    -- parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The list of logs that the restored DB instance is to export to
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The name of the database to create when the DB instance is created.
    -- Follow the naming rules specified in @CreateDBInstance@.
    dbName :: Prelude.Maybe Prelude.Text,
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
    -- | The compute and memory capacity of the DB instance, for example,
    -- @db.m4.large@. Not all DB instance classes are available in all Amazon
    -- Web Services Regions, or for all database engines. For the full list of
    -- DB instance classes, and availability for your engine, see
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
-- Create a value of 'RestoreDBInstanceFromS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'restoreDBInstanceFromS_engineVersion' - The version number of the database engine to use. Choose the latest
-- minor version of your database engine. For information about engine
-- versions, see @CreateDBInstance@, or call @DescribeDBEngineVersions@.
--
-- 'dbSecurityGroups', 'restoreDBInstanceFromS_dbSecurityGroups' - A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
--
-- 'deletionProtection', 'restoreDBInstanceFromS_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'storageEncrypted', 'restoreDBInstanceFromS_storageEncrypted' - A value that indicates whether the new DB instance is encrypted or not.
--
-- 'masterUserPassword', 'restoreDBInstanceFromS_masterUserPassword' - The password for the master user. The password can include any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- 'publiclyAccessible', 'restoreDBInstanceFromS_publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
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
-- 'autoMinorVersionUpgrade', 'restoreDBInstanceFromS_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the DB instance during the maintenance window. By
-- default, minor engine upgrades are not applied automatically.
--
-- 'masterUsername', 'restoreDBInstanceFromS_masterUsername' - The name for the master user.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- 'dbSubnetGroupName', 'restoreDBInstanceFromS_dbSubnetGroupName' - A DB subnet group to associate with this DB instance.
--
-- 'monitoringRoleArn', 'restoreDBInstanceFromS_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
--
-- 'iops', 'restoreDBInstanceFromS_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- allocate initially for the DB instance. For information about valid Iops
-- values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
-- in the /Amazon RDS User Guide./
--
-- 'monitoringInterval', 'restoreDBInstanceFromS_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
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
-- 'processorFeatures', 'restoreDBInstanceFromS_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'licenseModel', 'restoreDBInstanceFromS_licenseModel' - The license model for this DB instance. Use @general-public-license@.
--
-- 'preferredMaintenanceWindow', 'restoreDBInstanceFromS_preferredMaintenanceWindow' - The time range each week during which system maintenance can occur, in
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
-- 'performanceInsightsRetentionPeriod', 'restoreDBInstanceFromS_performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
--
-- 'maxAllocatedStorage', 'restoreDBInstanceFromS_maxAllocatedStorage' - The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- 'enablePerformanceInsights', 'restoreDBInstanceFromS_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon Relational Database Service User Guide/.
--
-- 'kmsKeyId', 'restoreDBInstanceFromS_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK). To use a CMK in a different Amazon Web Services account, specify
-- the key ARN or alias ARN.
--
-- If the @StorageEncrypted@ parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default CMK. There is a default CMK for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default CMK
-- for each Amazon Web Services Region.
--
-- 'dbParameterGroupName', 'restoreDBInstanceFromS_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then the
-- default @DBParameterGroup@ for the specified DB engine is used.
--
-- 'preferredBackupWindow', 'restoreDBInstanceFromS_preferredBackupWindow' - The time range each day during which automated backups are created if
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
-- 'availabilityZone', 'restoreDBInstanceFromS_availabilityZone' - The Availability Zone that the DB instance is created in. For
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
-- 'backupRetentionPeriod', 'restoreDBInstanceFromS_backupRetentionPeriod' - The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. For more
-- information, see @CreateDBInstance@.
--
-- 'performanceInsightsKMSKeyId', 'restoreDBInstanceFromS_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your Amazon
-- Web Services account. Your Amazon Web Services account has a different
-- default CMK for each Amazon Web Services Region.
--
-- 'vpcSecurityGroupIds', 'restoreDBInstanceFromS_vpcSecurityGroupIds' - A list of VPC security groups to associate with this DB instance.
--
-- 'multiAZ', 'restoreDBInstanceFromS_multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
-- If the DB instance is a Multi-AZ deployment, you can\'t set the
-- @AvailabilityZone@ parameter.
--
-- 's3Prefix', 'restoreDBInstanceFromS_s3Prefix' - The prefix of your Amazon S3 bucket.
--
-- 'allocatedStorage', 'restoreDBInstanceFromS_allocatedStorage' - The amount of storage (in gigabytes) to allocate initially for the DB
-- instance. Follow the allocation rules specified in @CreateDBInstance@.
--
-- Be sure to allocate enough memory for your new DB instance so that the
-- restore operation can succeed. You can also allocate additional memory
-- for future growth.
--
-- 'optionGroupName', 'restoreDBInstanceFromS_optionGroupName' - The name of the option group to associate with this DB instance. If this
-- argument is omitted, the default option group for the specified engine
-- is used.
--
-- 'copyTagsToSnapshot', 'restoreDBInstanceFromS_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
--
-- 'tags', 'restoreDBInstanceFromS_tags' - A list of tags to associate with this DB instance. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources>
-- in the /Amazon RDS User Guide./
--
-- 'port', 'restoreDBInstanceFromS_port' - The port number on which the database accepts connections.
--
-- Type: Integer
--
-- Valid Values: @1150@-@65535@
--
-- Default: @3306@
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBInstanceFromS_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- 'useDefaultProcessorFeatures', 'restoreDBInstanceFromS_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- 'storageType', 'restoreDBInstanceFromS_storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard@ | @gp2@ | @io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
--
-- 'enableCloudwatchLogsExports', 'restoreDBInstanceFromS_enableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- 'dbName', 'restoreDBInstanceFromS_dbName' - The name of the database to create when the DB instance is created.
-- Follow the naming rules specified in @CreateDBInstance@.
--
-- 'dbInstanceIdentifier', 'restoreDBInstanceFromS_dbInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase
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
-- 'dbInstanceClass', 'restoreDBInstanceFromS_dbInstanceClass' - The compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all Amazon
-- Web Services Regions, or for all database engines. For the full list of
-- DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Importing from Amazon S3 isn\'t supported on the db.t2.micro DB instance
-- class.
--
-- 'engine', 'restoreDBInstanceFromS_engine' - The name of the database engine to be used for this instance.
--
-- Valid Values: @mysql@
--
-- 'sourceEngine', 'restoreDBInstanceFromS_sourceEngine' - The name of the engine of your source database.
--
-- Valid Values: @mysql@
--
-- 'sourceEngineVersion', 'restoreDBInstanceFromS_sourceEngineVersion' - The version of the database that the backup files were created from.
--
-- MySQL versions 5.6 and 5.7 are supported.
--
-- Example: @5.6.40@
--
-- 's3BucketName', 'restoreDBInstanceFromS_s3BucketName' - The name of your Amazon S3 bucket that contains your database backup
-- file.
--
-- 's3IngestionRoleArn', 'restoreDBInstanceFromS_s3IngestionRoleArn' - An Amazon Web Services Identity and Access Management (IAM) role to
-- allow Amazon RDS to access your Amazon S3 bucket.
newRestoreDBInstanceFromS ::
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
  RestoreDBInstanceFromS
newRestoreDBInstanceFromS
  pDBInstanceIdentifier_
  pDBInstanceClass_
  pEngine_
  pSourceEngine_
  pSourceEngineVersion_
  pS3BucketName_
  pS3IngestionRoleArn_ =
    RestoreDBInstanceFromS'
      { engineVersion =
          Prelude.Nothing,
        dbSecurityGroups = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        storageEncrypted = Prelude.Nothing,
        masterUserPassword = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        masterUsername = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        monitoringRoleArn = Prelude.Nothing,
        iops = Prelude.Nothing,
        monitoringInterval = Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        licenseModel = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        performanceInsightsRetentionPeriod =
          Prelude.Nothing,
        maxAllocatedStorage = Prelude.Nothing,
        enablePerformanceInsights = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        preferredBackupWindow = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        backupRetentionPeriod = Prelude.Nothing,
        performanceInsightsKMSKeyId = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        s3Prefix = Prelude.Nothing,
        allocatedStorage = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        tags = Prelude.Nothing,
        port = Prelude.Nothing,
        enableIAMDatabaseAuthentication = Prelude.Nothing,
        useDefaultProcessorFeatures = Prelude.Nothing,
        storageType = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        dbName = Prelude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        dbInstanceClass = pDBInstanceClass_,
        engine = pEngine_,
        sourceEngine = pSourceEngine_,
        sourceEngineVersion = pSourceEngineVersion_,
        s3BucketName = pS3BucketName_,
        s3IngestionRoleArn = pS3IngestionRoleArn_
      }

-- | The version number of the database engine to use. Choose the latest
-- minor version of your database engine. For information about engine
-- versions, see @CreateDBInstance@, or call @DescribeDBEngineVersions@.
restoreDBInstanceFromS_engineVersion :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_engineVersion = Lens.lens (\RestoreDBInstanceFromS' {engineVersion} -> engineVersion) (\s@RestoreDBInstanceFromS' {} a -> s {engineVersion = a} :: RestoreDBInstanceFromS)

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
restoreDBInstanceFromS_dbSecurityGroups :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe [Prelude.Text])
restoreDBInstanceFromS_dbSecurityGroups = Lens.lens (\RestoreDBInstanceFromS' {dbSecurityGroups} -> dbSecurityGroups) (\s@RestoreDBInstanceFromS' {} a -> s {dbSecurityGroups = a} :: RestoreDBInstanceFromS) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
restoreDBInstanceFromS_deletionProtection :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS_deletionProtection = Lens.lens (\RestoreDBInstanceFromS' {deletionProtection} -> deletionProtection) (\s@RestoreDBInstanceFromS' {} a -> s {deletionProtection = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether the new DB instance is encrypted or not.
restoreDBInstanceFromS_storageEncrypted :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS_storageEncrypted = Lens.lens (\RestoreDBInstanceFromS' {storageEncrypted} -> storageEncrypted) (\s@RestoreDBInstanceFromS' {} a -> s {storageEncrypted = a} :: RestoreDBInstanceFromS)

-- | The password for the master user. The password can include any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
restoreDBInstanceFromS_masterUserPassword :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_masterUserPassword = Lens.lens (\RestoreDBInstanceFromS' {masterUserPassword} -> masterUserPassword) (\s@RestoreDBInstanceFromS' {} a -> s {masterUserPassword = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether the DB instance is publicly accessible.
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
restoreDBInstanceFromS_publiclyAccessible :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS_publiclyAccessible = Lens.lens (\RestoreDBInstanceFromS' {publiclyAccessible} -> publiclyAccessible) (\s@RestoreDBInstanceFromS' {} a -> s {publiclyAccessible = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the DB instance during the maintenance window. By
-- default, minor engine upgrades are not applied automatically.
restoreDBInstanceFromS_autoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS_autoMinorVersionUpgrade = Lens.lens (\RestoreDBInstanceFromS' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@RestoreDBInstanceFromS' {} a -> s {autoMinorVersionUpgrade = a} :: RestoreDBInstanceFromS)

-- | The name for the master user.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
restoreDBInstanceFromS_masterUsername :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_masterUsername = Lens.lens (\RestoreDBInstanceFromS' {masterUsername} -> masterUsername) (\s@RestoreDBInstanceFromS' {} a -> s {masterUsername = a} :: RestoreDBInstanceFromS)

-- | A DB subnet group to associate with this DB instance.
restoreDBInstanceFromS_dbSubnetGroupName :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_dbSubnetGroupName = Lens.lens (\RestoreDBInstanceFromS' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBInstanceFromS' {} a -> s {dbSubnetGroupName = a} :: RestoreDBInstanceFromS)

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
restoreDBInstanceFromS_monitoringRoleArn :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_monitoringRoleArn = Lens.lens (\RestoreDBInstanceFromS' {monitoringRoleArn} -> monitoringRoleArn) (\s@RestoreDBInstanceFromS' {} a -> s {monitoringRoleArn = a} :: RestoreDBInstanceFromS)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- allocate initially for the DB instance. For information about valid Iops
-- values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromS_iops :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS_iops = Lens.lens (\RestoreDBInstanceFromS' {iops} -> iops) (\s@RestoreDBInstanceFromS' {} a -> s {iops = a} :: RestoreDBInstanceFromS)

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
restoreDBInstanceFromS_monitoringInterval :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS_monitoringInterval = Lens.lens (\RestoreDBInstanceFromS' {monitoringInterval} -> monitoringInterval) (\s@RestoreDBInstanceFromS' {} a -> s {monitoringInterval = a} :: RestoreDBInstanceFromS)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
restoreDBInstanceFromS_processorFeatures :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe [ProcessorFeature])
restoreDBInstanceFromS_processorFeatures = Lens.lens (\RestoreDBInstanceFromS' {processorFeatures} -> processorFeatures) (\s@RestoreDBInstanceFromS' {} a -> s {processorFeatures = a} :: RestoreDBInstanceFromS) Prelude.. Lens.mapping Lens.coerced

-- | The license model for this DB instance. Use @general-public-license@.
restoreDBInstanceFromS_licenseModel :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_licenseModel = Lens.lens (\RestoreDBInstanceFromS' {licenseModel} -> licenseModel) (\s@RestoreDBInstanceFromS' {} a -> s {licenseModel = a} :: RestoreDBInstanceFromS)

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
restoreDBInstanceFromS_preferredMaintenanceWindow :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_preferredMaintenanceWindow = Lens.lens (\RestoreDBInstanceFromS' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@RestoreDBInstanceFromS' {} a -> s {preferredMaintenanceWindow = a} :: RestoreDBInstanceFromS)

-- | The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
restoreDBInstanceFromS_performanceInsightsRetentionPeriod :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS_performanceInsightsRetentionPeriod = Lens.lens (\RestoreDBInstanceFromS' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@RestoreDBInstanceFromS' {} a -> s {performanceInsightsRetentionPeriod = a} :: RestoreDBInstanceFromS)

-- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromS_maxAllocatedStorage :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS_maxAllocatedStorage = Lens.lens (\RestoreDBInstanceFromS' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@RestoreDBInstanceFromS' {} a -> s {maxAllocatedStorage = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon Relational Database Service User Guide/.
restoreDBInstanceFromS_enablePerformanceInsights :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS_enablePerformanceInsights = Lens.lens (\RestoreDBInstanceFromS' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@RestoreDBInstanceFromS' {} a -> s {enablePerformanceInsights = a} :: RestoreDBInstanceFromS)

-- | The Amazon Web Services KMS key identifier for an encrypted DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK). To use a CMK in a different Amazon Web Services account, specify
-- the key ARN or alias ARN.
--
-- If the @StorageEncrypted@ parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default CMK. There is a default CMK for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default CMK
-- for each Amazon Web Services Region.
restoreDBInstanceFromS_kmsKeyId :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_kmsKeyId = Lens.lens (\RestoreDBInstanceFromS' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBInstanceFromS' {} a -> s {kmsKeyId = a} :: RestoreDBInstanceFromS)

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then the
-- default @DBParameterGroup@ for the specified DB engine is used.
restoreDBInstanceFromS_dbParameterGroupName :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_dbParameterGroupName = Lens.lens (\RestoreDBInstanceFromS' {dbParameterGroupName} -> dbParameterGroupName) (\s@RestoreDBInstanceFromS' {} a -> s {dbParameterGroupName = a} :: RestoreDBInstanceFromS)

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
restoreDBInstanceFromS_preferredBackupWindow :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_preferredBackupWindow = Lens.lens (\RestoreDBInstanceFromS' {preferredBackupWindow} -> preferredBackupWindow) (\s@RestoreDBInstanceFromS' {} a -> s {preferredBackupWindow = a} :: RestoreDBInstanceFromS)

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
restoreDBInstanceFromS_availabilityZone :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_availabilityZone = Lens.lens (\RestoreDBInstanceFromS' {availabilityZone} -> availabilityZone) (\s@RestoreDBInstanceFromS' {} a -> s {availabilityZone = a} :: RestoreDBInstanceFromS)

-- | The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. For more
-- information, see @CreateDBInstance@.
restoreDBInstanceFromS_backupRetentionPeriod :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS_backupRetentionPeriod = Lens.lens (\RestoreDBInstanceFromS' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@RestoreDBInstanceFromS' {} a -> s {backupRetentionPeriod = a} :: RestoreDBInstanceFromS)

-- | The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your Amazon
-- Web Services account. Your Amazon Web Services account has a different
-- default CMK for each Amazon Web Services Region.
restoreDBInstanceFromS_performanceInsightsKMSKeyId :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_performanceInsightsKMSKeyId = Lens.lens (\RestoreDBInstanceFromS' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@RestoreDBInstanceFromS' {} a -> s {performanceInsightsKMSKeyId = a} :: RestoreDBInstanceFromS)

-- | A list of VPC security groups to associate with this DB instance.
restoreDBInstanceFromS_vpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe [Prelude.Text])
restoreDBInstanceFromS_vpcSecurityGroupIds = Lens.lens (\RestoreDBInstanceFromS' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBInstanceFromS' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBInstanceFromS) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
-- If the DB instance is a Multi-AZ deployment, you can\'t set the
-- @AvailabilityZone@ parameter.
restoreDBInstanceFromS_multiAZ :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS_multiAZ = Lens.lens (\RestoreDBInstanceFromS' {multiAZ} -> multiAZ) (\s@RestoreDBInstanceFromS' {} a -> s {multiAZ = a} :: RestoreDBInstanceFromS)

-- | The prefix of your Amazon S3 bucket.
restoreDBInstanceFromS_s3Prefix :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_s3Prefix = Lens.lens (\RestoreDBInstanceFromS' {s3Prefix} -> s3Prefix) (\s@RestoreDBInstanceFromS' {} a -> s {s3Prefix = a} :: RestoreDBInstanceFromS)

-- | The amount of storage (in gigabytes) to allocate initially for the DB
-- instance. Follow the allocation rules specified in @CreateDBInstance@.
--
-- Be sure to allocate enough memory for your new DB instance so that the
-- restore operation can succeed. You can also allocate additional memory
-- for future growth.
restoreDBInstanceFromS_allocatedStorage :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS_allocatedStorage = Lens.lens (\RestoreDBInstanceFromS' {allocatedStorage} -> allocatedStorage) (\s@RestoreDBInstanceFromS' {} a -> s {allocatedStorage = a} :: RestoreDBInstanceFromS)

-- | The name of the option group to associate with this DB instance. If this
-- argument is omitted, the default option group for the specified engine
-- is used.
restoreDBInstanceFromS_optionGroupName :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_optionGroupName = Lens.lens (\RestoreDBInstanceFromS' {optionGroupName} -> optionGroupName) (\s@RestoreDBInstanceFromS' {} a -> s {optionGroupName = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
restoreDBInstanceFromS_copyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS_copyTagsToSnapshot = Lens.lens (\RestoreDBInstanceFromS' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBInstanceFromS' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBInstanceFromS)

-- | A list of tags to associate with this DB instance. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromS_tags :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe [Tag])
restoreDBInstanceFromS_tags = Lens.lens (\RestoreDBInstanceFromS' {tags} -> tags) (\s@RestoreDBInstanceFromS' {} a -> s {tags = a} :: RestoreDBInstanceFromS) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the database accepts connections.
--
-- Type: Integer
--
-- Valid Values: @1150@-@65535@
--
-- Default: @3306@
restoreDBInstanceFromS_port :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Int)
restoreDBInstanceFromS_port = Lens.lens (\RestoreDBInstanceFromS' {port} -> port) (\s@RestoreDBInstanceFromS' {} a -> s {port = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromS_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBInstanceFromS' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBInstanceFromS' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
restoreDBInstanceFromS_useDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Bool)
restoreDBInstanceFromS_useDefaultProcessorFeatures = Lens.lens (\RestoreDBInstanceFromS' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@RestoreDBInstanceFromS' {} a -> s {useDefaultProcessorFeatures = a} :: RestoreDBInstanceFromS)

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard@ | @gp2@ | @io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
restoreDBInstanceFromS_storageType :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_storageType = Lens.lens (\RestoreDBInstanceFromS' {storageType} -> storageType) (\s@RestoreDBInstanceFromS' {} a -> s {storageType = a} :: RestoreDBInstanceFromS)

-- | The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromS_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe [Prelude.Text])
restoreDBInstanceFromS_enableCloudwatchLogsExports = Lens.lens (\RestoreDBInstanceFromS' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBInstanceFromS' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBInstanceFromS) Prelude.. Lens.mapping Lens.coerced

-- | The name of the database to create when the DB instance is created.
-- Follow the naming rules specified in @CreateDBInstance@.
restoreDBInstanceFromS_dbName :: Lens.Lens' RestoreDBInstanceFromS (Prelude.Maybe Prelude.Text)
restoreDBInstanceFromS_dbName = Lens.lens (\RestoreDBInstanceFromS' {dbName} -> dbName) (\s@RestoreDBInstanceFromS' {} a -> s {dbName = a} :: RestoreDBInstanceFromS)

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
restoreDBInstanceFromS_dbInstanceIdentifier :: Lens.Lens' RestoreDBInstanceFromS Prelude.Text
restoreDBInstanceFromS_dbInstanceIdentifier = Lens.lens (\RestoreDBInstanceFromS' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RestoreDBInstanceFromS' {} a -> s {dbInstanceIdentifier = a} :: RestoreDBInstanceFromS)

-- | The compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all Amazon
-- Web Services Regions, or for all database engines. For the full list of
-- DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Importing from Amazon S3 isn\'t supported on the db.t2.micro DB instance
-- class.
restoreDBInstanceFromS_dbInstanceClass :: Lens.Lens' RestoreDBInstanceFromS Prelude.Text
restoreDBInstanceFromS_dbInstanceClass = Lens.lens (\RestoreDBInstanceFromS' {dbInstanceClass} -> dbInstanceClass) (\s@RestoreDBInstanceFromS' {} a -> s {dbInstanceClass = a} :: RestoreDBInstanceFromS)

-- | The name of the database engine to be used for this instance.
--
-- Valid Values: @mysql@
restoreDBInstanceFromS_engine :: Lens.Lens' RestoreDBInstanceFromS Prelude.Text
restoreDBInstanceFromS_engine = Lens.lens (\RestoreDBInstanceFromS' {engine} -> engine) (\s@RestoreDBInstanceFromS' {} a -> s {engine = a} :: RestoreDBInstanceFromS)

-- | The name of the engine of your source database.
--
-- Valid Values: @mysql@
restoreDBInstanceFromS_sourceEngine :: Lens.Lens' RestoreDBInstanceFromS Prelude.Text
restoreDBInstanceFromS_sourceEngine = Lens.lens (\RestoreDBInstanceFromS' {sourceEngine} -> sourceEngine) (\s@RestoreDBInstanceFromS' {} a -> s {sourceEngine = a} :: RestoreDBInstanceFromS)

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.6 and 5.7 are supported.
--
-- Example: @5.6.40@
restoreDBInstanceFromS_sourceEngineVersion :: Lens.Lens' RestoreDBInstanceFromS Prelude.Text
restoreDBInstanceFromS_sourceEngineVersion = Lens.lens (\RestoreDBInstanceFromS' {sourceEngineVersion} -> sourceEngineVersion) (\s@RestoreDBInstanceFromS' {} a -> s {sourceEngineVersion = a} :: RestoreDBInstanceFromS)

-- | The name of your Amazon S3 bucket that contains your database backup
-- file.
restoreDBInstanceFromS_s3BucketName :: Lens.Lens' RestoreDBInstanceFromS Prelude.Text
restoreDBInstanceFromS_s3BucketName = Lens.lens (\RestoreDBInstanceFromS' {s3BucketName} -> s3BucketName) (\s@RestoreDBInstanceFromS' {} a -> s {s3BucketName = a} :: RestoreDBInstanceFromS)

-- | An Amazon Web Services Identity and Access Management (IAM) role to
-- allow Amazon RDS to access your Amazon S3 bucket.
restoreDBInstanceFromS_s3IngestionRoleArn :: Lens.Lens' RestoreDBInstanceFromS Prelude.Text
restoreDBInstanceFromS_s3IngestionRoleArn = Lens.lens (\RestoreDBInstanceFromS' {s3IngestionRoleArn} -> s3IngestionRoleArn) (\s@RestoreDBInstanceFromS' {} a -> s {s3IngestionRoleArn = a} :: RestoreDBInstanceFromS)

instance Core.AWSRequest RestoreDBInstanceFromS where
  type
    AWSResponse RestoreDBInstanceFromS =
      RestoreDBInstanceFromSResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RestoreDBInstanceFromS3Result"
      ( \s h x ->
          RestoreDBInstanceFromSResponse'
            Prelude.<$> (x Core..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreDBInstanceFromS

instance Prelude.NFData RestoreDBInstanceFromS

instance Core.ToHeaders RestoreDBInstanceFromS where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RestoreDBInstanceFromS where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreDBInstanceFromS where
  toQuery RestoreDBInstanceFromS' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RestoreDBInstanceFromS" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "EngineVersion" Core.=: engineVersion,
        "DBSecurityGroups"
          Core.=: Core.toQuery
            ( Core.toQueryList "DBSecurityGroupName"
                Prelude.<$> dbSecurityGroups
            ),
        "DeletionProtection" Core.=: deletionProtection,
        "StorageEncrypted" Core.=: storageEncrypted,
        "MasterUserPassword" Core.=: masterUserPassword,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "MasterUsername" Core.=: masterUsername,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "MonitoringRoleArn" Core.=: monitoringRoleArn,
        "Iops" Core.=: iops,
        "MonitoringInterval" Core.=: monitoringInterval,
        "ProcessorFeatures"
          Core.=: Core.toQuery
            ( Core.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "LicenseModel" Core.=: licenseModel,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "PerformanceInsightsRetentionPeriod"
          Core.=: performanceInsightsRetentionPeriod,
        "MaxAllocatedStorage" Core.=: maxAllocatedStorage,
        "EnablePerformanceInsights"
          Core.=: enablePerformanceInsights,
        "KmsKeyId" Core.=: kmsKeyId,
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "AvailabilityZone" Core.=: availabilityZone,
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "PerformanceInsightsKMSKeyId"
          Core.=: performanceInsightsKMSKeyId,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "MultiAZ" Core.=: multiAZ,
        "S3Prefix" Core.=: s3Prefix,
        "AllocatedStorage" Core.=: allocatedStorage,
        "OptionGroupName" Core.=: optionGroupName,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures"
          Core.=: useDefaultProcessorFeatures,
        "StorageType" Core.=: storageType,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "DBName" Core.=: dbName,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier,
        "DBInstanceClass" Core.=: dbInstanceClass,
        "Engine" Core.=: engine,
        "SourceEngine" Core.=: sourceEngine,
        "SourceEngineVersion" Core.=: sourceEngineVersion,
        "S3BucketName" Core.=: s3BucketName,
        "S3IngestionRoleArn" Core.=: s3IngestionRoleArn
      ]

-- | /See:/ 'newRestoreDBInstanceFromSResponse' smart constructor.
data RestoreDBInstanceFromSResponse = RestoreDBInstanceFromSResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBInstanceFromSResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'restoreDBInstanceFromSResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'restoreDBInstanceFromSResponse_httpStatus' - The response's http status code.
newRestoreDBInstanceFromSResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreDBInstanceFromSResponse
newRestoreDBInstanceFromSResponse pHttpStatus_ =
  RestoreDBInstanceFromSResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreDBInstanceFromSResponse_dbInstance :: Lens.Lens' RestoreDBInstanceFromSResponse (Prelude.Maybe DBInstance)
restoreDBInstanceFromSResponse_dbInstance = Lens.lens (\RestoreDBInstanceFromSResponse' {dbInstance} -> dbInstance) (\s@RestoreDBInstanceFromSResponse' {} a -> s {dbInstance = a} :: RestoreDBInstanceFromSResponse)

-- | The response's http status code.
restoreDBInstanceFromSResponse_httpStatus :: Lens.Lens' RestoreDBInstanceFromSResponse Prelude.Int
restoreDBInstanceFromSResponse_httpStatus = Lens.lens (\RestoreDBInstanceFromSResponse' {httpStatus} -> httpStatus) (\s@RestoreDBInstanceFromSResponse' {} a -> s {httpStatus = a} :: RestoreDBInstanceFromSResponse)

instance
  Prelude.NFData
    RestoreDBInstanceFromSResponse
