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
    restoreDBInstanceFromS_backupRetentionPeriod,
    restoreDBInstanceFromS_deletionProtection,
    restoreDBInstanceFromS_storageEncrypted,
    restoreDBInstanceFromS_preferredBackupWindow,
    restoreDBInstanceFromS_enablePerformanceInsights,
    restoreDBInstanceFromS_dbSecurityGroups,
    restoreDBInstanceFromS_maxAllocatedStorage,
    restoreDBInstanceFromS_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromS_enableCloudwatchLogsExports,
    restoreDBInstanceFromS_storageType,
    restoreDBInstanceFromS_useDefaultProcessorFeatures,
    restoreDBInstanceFromS_monitoringInterval,
    restoreDBInstanceFromS_optionGroupName,
    restoreDBInstanceFromS_monitoringRoleArn,
    restoreDBInstanceFromS_dbSubnetGroupName,
    restoreDBInstanceFromS_masterUserPassword,
    restoreDBInstanceFromS_masterUsername,
    restoreDBInstanceFromS_multiAZ,
    restoreDBInstanceFromS_publiclyAccessible,
    restoreDBInstanceFromS_vpcSecurityGroupIds,
    restoreDBInstanceFromS_performanceInsightsKMSKeyId,
    restoreDBInstanceFromS_kmsKeyId,
    restoreDBInstanceFromS_dbParameterGroupName,
    restoreDBInstanceFromS_availabilityZone,
    restoreDBInstanceFromS_engineVersion,
    restoreDBInstanceFromS_preferredMaintenanceWindow,
    restoreDBInstanceFromS_performanceInsightsRetentionPeriod,
    restoreDBInstanceFromS_licenseModel,
    restoreDBInstanceFromS_tags,
    restoreDBInstanceFromS_processorFeatures,
    restoreDBInstanceFromS_port,
    restoreDBInstanceFromS_dbName,
    restoreDBInstanceFromS_copyTagsToSnapshot,
    restoreDBInstanceFromS_s3Prefix,
    restoreDBInstanceFromS_allocatedStorage,
    restoreDBInstanceFromS_iops,
    restoreDBInstanceFromS_autoMinorVersionUpgrade,
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
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRestoreDBInstanceFromS' smart constructor.
data RestoreDBInstanceFromS = RestoreDBInstanceFromS'
  { -- | The number of days for which automated backups are retained. Setting
    -- this parameter to a positive number enables backups. For more
    -- information, see @CreateDBInstance@.
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | A value that indicates whether the new DB instance is encrypted or not.
    storageEncrypted :: Core.Maybe Core.Bool,
    -- | The time range each day during which automated backups are created if
    -- automated backups are enabled. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window>
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
    preferredBackupWindow :: Core.Maybe Core.Text,
    -- | A value that indicates whether to enable Performance Insights for the DB
    -- instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon Relational Database Service User Guide/.
    enablePerformanceInsights :: Core.Maybe Core.Bool,
    -- | A list of DB security groups to associate with this DB instance.
    --
    -- Default: The default DB security group for the database engine.
    dbSecurityGroups :: Core.Maybe [Core.Text],
    -- | The upper limit to which Amazon RDS can automatically scale the storage
    -- of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    maxAllocatedStorage :: Core.Maybe Core.Int,
    -- | A value that indicates whether to enable mapping of AWS Identity and
    -- Access Management (IAM) accounts to database accounts. By default,
    -- mapping is disabled.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
    -- | The list of logs that the restored DB instance is to export to
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    enableCloudwatchLogsExports :: Core.Maybe [Core.Text],
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @standard@ | @gp2@ | @io1@
    --
    -- If you specify @io1@, you must also include a value for the @Iops@
    -- parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
    storageType :: Core.Maybe Core.Text,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    useDefaultProcessorFeatures :: Core.Maybe Core.Bool,
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
    monitoringInterval :: Core.Maybe Core.Int,
    -- | The name of the option group to associate with this DB instance. If this
    -- argument is omitted, the default option group for the specified engine
    -- is used.
    optionGroupName :: Core.Maybe Core.Text,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring
    -- metrics to Amazon CloudWatch Logs. For example,
    -- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
    -- monitoring role, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
    -- in the /Amazon RDS User Guide./
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must
    -- supply a @MonitoringRoleArn@ value.
    monitoringRoleArn :: Core.Maybe Core.Text,
    -- | A DB subnet group to associate with this DB instance.
    dbSubnetGroupName :: Core.Maybe Core.Text,
    -- | The password for the master user. The password can include any printable
    -- ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Core.Maybe Core.Text,
    -- | The name for the master user.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 16 letters or numbers.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    masterUsername :: Core.Maybe Core.Text,
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    -- If the DB instance is a Multi-AZ deployment, you can\'t set the
    -- @AvailabilityZone@ parameter.
    multiAZ :: Core.Maybe Core.Bool,
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
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | A list of VPC security groups to associate with this DB instance.
    vpcSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | The AWS KMS key identifier for encryption of Performance Insights data.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
    -- Amazon RDS uses your default CMK. There is a default CMK for your AWS
    -- account. Your AWS account has a different default CMK for each AWS
    -- Region.
    performanceInsightsKMSKeyId :: Core.Maybe Core.Text,
    -- | The AWS KMS key identifier for an encrypted DB instance.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK). To use a CMK in a
    -- different AWS account, specify the key ARN or alias ARN.
    --
    -- If the @StorageEncrypted@ parameter is enabled, and you do not specify a
    -- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
    -- default CMK. There is a default CMK for your AWS account. Your AWS
    -- account has a different default CMK for each AWS Region.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The name of the DB parameter group to associate with this DB instance.
    --
    -- If you do not specify a value for @DBParameterGroupName@, then the
    -- default @DBParameterGroup@ for the specified DB engine is used.
    dbParameterGroupName :: Core.Maybe Core.Text,
    -- | The Availability Zone that the DB instance is created in. For
    -- information about AWS Regions and Availability Zones, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>
    -- in the /Amazon RDS User Guide./
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint\'s
    -- AWS Region.
    --
    -- Example: @us-east-1d@
    --
    -- Constraint: The @AvailabilityZone@ parameter can\'t be specified if the
    -- DB instance is a Multi-AZ deployment. The specified Availability Zone
    -- must be in the same AWS Region as the current endpoint.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The version number of the database engine to use. Choose the latest
    -- minor version of your database engine. For information about engine
    -- versions, see @CreateDBInstance@, or call @DescribeDBEngineVersions@.
    engineVersion :: Core.Maybe Core.Text,
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
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid
    -- values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Core.Maybe Core.Int,
    -- | The license model for this DB instance. Use @general-public-license@.
    licenseModel :: Core.Maybe Core.Text,
    -- | A list of tags to associate with this DB instance. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources>
    -- in the /Amazon RDS User Guide./
    tags :: Core.Maybe [Tag],
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Core.Maybe [ProcessorFeature],
    -- | The port number on which the database accepts connections.
    --
    -- Type: Integer
    --
    -- Valid Values: @1150@-@65535@
    --
    -- Default: @3306@
    port :: Core.Maybe Core.Int,
    -- | The name of the database to create when the DB instance is created.
    -- Follow the naming rules specified in @CreateDBInstance@.
    dbName :: Core.Maybe Core.Text,
    -- | A value that indicates whether to copy all tags from the DB instance to
    -- snapshots of the DB instance. By default, tags are not copied.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
    -- | The prefix of your Amazon S3 bucket.
    s3Prefix :: Core.Maybe Core.Text,
    -- | The amount of storage (in gigabytes) to allocate initially for the DB
    -- instance. Follow the allocation rules specified in @CreateDBInstance@.
    --
    -- Be sure to allocate enough memory for your new DB instance so that the
    -- restore operation can succeed. You can also allocate additional memory
    -- for future growth.
    allocatedStorage :: Core.Maybe Core.Int,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- allocate initially for the DB instance. For information about valid Iops
    -- values, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
    -- in the /Amazon RDS User Guide./
    iops :: Core.Maybe Core.Int,
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the DB instance during the maintenance window. By
    -- default, minor engine upgrades are not applied automatically.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
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
    dbInstanceIdentifier :: Core.Text,
    -- | The compute and memory capacity of the DB instance, for example,
    -- @db.m4.large@. Not all DB instance classes are available in all AWS
    -- Regions, or for all database engines. For the full list of DB instance
    -- classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    --
    -- Importing from Amazon S3 isn\'t supported on the db.t2.micro DB instance
    -- class.
    dbInstanceClass :: Core.Text,
    -- | The name of the database engine to be used for this instance.
    --
    -- Valid Values: @mysql@
    engine :: Core.Text,
    -- | The name of the engine of your source database.
    --
    -- Valid Values: @mysql@
    sourceEngine :: Core.Text,
    -- | The version of the database that the backup files were created from.
    --
    -- MySQL versions 5.6 and 5.7 are supported.
    --
    -- Example: @5.6.40@
    sourceEngineVersion :: Core.Text,
    -- | The name of your Amazon S3 bucket that contains your database backup
    -- file.
    s3BucketName :: Core.Text,
    -- | An AWS Identity and Access Management (IAM) role to allow Amazon RDS to
    -- access your Amazon S3 bucket.
    s3IngestionRoleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreDBInstanceFromS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPeriod', 'restoreDBInstanceFromS_backupRetentionPeriod' - The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. For more
-- information, see @CreateDBInstance@.
--
-- 'deletionProtection', 'restoreDBInstanceFromS_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'storageEncrypted', 'restoreDBInstanceFromS_storageEncrypted' - A value that indicates whether the new DB instance is encrypted or not.
--
-- 'preferredBackupWindow', 'restoreDBInstanceFromS_preferredBackupWindow' - The time range each day during which automated backups are created if
-- automated backups are enabled. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window>
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
-- 'enablePerformanceInsights', 'restoreDBInstanceFromS_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon Relational Database Service User Guide/.
--
-- 'dbSecurityGroups', 'restoreDBInstanceFromS_dbSecurityGroups' - A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
--
-- 'maxAllocatedStorage', 'restoreDBInstanceFromS_maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBInstanceFromS_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- 'enableCloudwatchLogsExports', 'restoreDBInstanceFromS_enableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
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
-- 'useDefaultProcessorFeatures', 'restoreDBInstanceFromS_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
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
-- 'optionGroupName', 'restoreDBInstanceFromS_optionGroupName' - The name of the option group to associate with this DB instance. If this
-- argument is omitted, the default option group for the specified engine
-- is used.
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
-- 'dbSubnetGroupName', 'restoreDBInstanceFromS_dbSubnetGroupName' - A DB subnet group to associate with this DB instance.
--
-- 'masterUserPassword', 'restoreDBInstanceFromS_masterUserPassword' - The password for the master user. The password can include any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
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
-- 'multiAZ', 'restoreDBInstanceFromS_multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
-- If the DB instance is a Multi-AZ deployment, you can\'t set the
-- @AvailabilityZone@ parameter.
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
-- 'vpcSecurityGroupIds', 'restoreDBInstanceFromS_vpcSecurityGroupIds' - A list of VPC security groups to associate with this DB instance.
--
-- 'performanceInsightsKMSKeyId', 'restoreDBInstanceFromS_performanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your AWS
-- account. Your AWS account has a different default CMK for each AWS
-- Region.
--
-- 'kmsKeyId', 'restoreDBInstanceFromS_kmsKeyId' - The AWS KMS key identifier for an encrypted DB instance.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK). To use a CMK in a
-- different AWS account, specify the key ARN or alias ARN.
--
-- If the @StorageEncrypted@ parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default CMK. There is a default CMK for your AWS account. Your AWS
-- account has a different default CMK for each AWS Region.
--
-- 'dbParameterGroupName', 'restoreDBInstanceFromS_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then the
-- default @DBParameterGroup@ for the specified DB engine is used.
--
-- 'availabilityZone', 'restoreDBInstanceFromS_availabilityZone' - The Availability Zone that the DB instance is created in. For
-- information about AWS Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>
-- in the /Amazon RDS User Guide./
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- AWS Region.
--
-- Example: @us-east-1d@
--
-- Constraint: The @AvailabilityZone@ parameter can\'t be specified if the
-- DB instance is a Multi-AZ deployment. The specified Availability Zone
-- must be in the same AWS Region as the current endpoint.
--
-- 'engineVersion', 'restoreDBInstanceFromS_engineVersion' - The version number of the database engine to use. Choose the latest
-- minor version of your database engine. For information about engine
-- versions, see @CreateDBInstance@, or call @DescribeDBEngineVersions@.
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
-- 'licenseModel', 'restoreDBInstanceFromS_licenseModel' - The license model for this DB instance. Use @general-public-license@.
--
-- 'tags', 'restoreDBInstanceFromS_tags' - A list of tags to associate with this DB instance. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources>
-- in the /Amazon RDS User Guide./
--
-- 'processorFeatures', 'restoreDBInstanceFromS_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'port', 'restoreDBInstanceFromS_port' - The port number on which the database accepts connections.
--
-- Type: Integer
--
-- Valid Values: @1150@-@65535@
--
-- Default: @3306@
--
-- 'dbName', 'restoreDBInstanceFromS_dbName' - The name of the database to create when the DB instance is created.
-- Follow the naming rules specified in @CreateDBInstance@.
--
-- 'copyTagsToSnapshot', 'restoreDBInstanceFromS_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
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
-- 'iops', 'restoreDBInstanceFromS_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- allocate initially for the DB instance. For information about valid Iops
-- values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
-- in the /Amazon RDS User Guide./
--
-- 'autoMinorVersionUpgrade', 'restoreDBInstanceFromS_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the DB instance during the maintenance window. By
-- default, minor engine upgrades are not applied automatically.
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
-- @db.m4.large@. Not all DB instance classes are available in all AWS
-- Regions, or for all database engines. For the full list of DB instance
-- classes, and availability for your engine, see
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
-- 's3IngestionRoleArn', 'restoreDBInstanceFromS_s3IngestionRoleArn' - An AWS Identity and Access Management (IAM) role to allow Amazon RDS to
-- access your Amazon S3 bucket.
newRestoreDBInstanceFromS ::
  -- | 'dbInstanceIdentifier'
  Core.Text ->
  -- | 'dbInstanceClass'
  Core.Text ->
  -- | 'engine'
  Core.Text ->
  -- | 'sourceEngine'
  Core.Text ->
  -- | 'sourceEngineVersion'
  Core.Text ->
  -- | 's3BucketName'
  Core.Text ->
  -- | 's3IngestionRoleArn'
  Core.Text ->
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
      { backupRetentionPeriod =
          Core.Nothing,
        deletionProtection = Core.Nothing,
        storageEncrypted = Core.Nothing,
        preferredBackupWindow = Core.Nothing,
        enablePerformanceInsights = Core.Nothing,
        dbSecurityGroups = Core.Nothing,
        maxAllocatedStorage = Core.Nothing,
        enableIAMDatabaseAuthentication = Core.Nothing,
        enableCloudwatchLogsExports = Core.Nothing,
        storageType = Core.Nothing,
        useDefaultProcessorFeatures = Core.Nothing,
        monitoringInterval = Core.Nothing,
        optionGroupName = Core.Nothing,
        monitoringRoleArn = Core.Nothing,
        dbSubnetGroupName = Core.Nothing,
        masterUserPassword = Core.Nothing,
        masterUsername = Core.Nothing,
        multiAZ = Core.Nothing,
        publiclyAccessible = Core.Nothing,
        vpcSecurityGroupIds = Core.Nothing,
        performanceInsightsKMSKeyId = Core.Nothing,
        kmsKeyId = Core.Nothing,
        dbParameterGroupName = Core.Nothing,
        availabilityZone = Core.Nothing,
        engineVersion = Core.Nothing,
        preferredMaintenanceWindow = Core.Nothing,
        performanceInsightsRetentionPeriod = Core.Nothing,
        licenseModel = Core.Nothing,
        tags = Core.Nothing,
        processorFeatures = Core.Nothing,
        port = Core.Nothing,
        dbName = Core.Nothing,
        copyTagsToSnapshot = Core.Nothing,
        s3Prefix = Core.Nothing,
        allocatedStorage = Core.Nothing,
        iops = Core.Nothing,
        autoMinorVersionUpgrade = Core.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        dbInstanceClass = pDBInstanceClass_,
        engine = pEngine_,
        sourceEngine = pSourceEngine_,
        sourceEngineVersion = pSourceEngineVersion_,
        s3BucketName = pS3BucketName_,
        s3IngestionRoleArn = pS3IngestionRoleArn_
      }

-- | The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. For more
-- information, see @CreateDBInstance@.
restoreDBInstanceFromS_backupRetentionPeriod :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Int)
restoreDBInstanceFromS_backupRetentionPeriod = Lens.lens (\RestoreDBInstanceFromS' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@RestoreDBInstanceFromS' {} a -> s {backupRetentionPeriod = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
restoreDBInstanceFromS_deletionProtection :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Bool)
restoreDBInstanceFromS_deletionProtection = Lens.lens (\RestoreDBInstanceFromS' {deletionProtection} -> deletionProtection) (\s@RestoreDBInstanceFromS' {} a -> s {deletionProtection = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether the new DB instance is encrypted or not.
restoreDBInstanceFromS_storageEncrypted :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Bool)
restoreDBInstanceFromS_storageEncrypted = Lens.lens (\RestoreDBInstanceFromS' {storageEncrypted} -> storageEncrypted) (\s@RestoreDBInstanceFromS' {} a -> s {storageEncrypted = a} :: RestoreDBInstanceFromS)

-- | The time range each day during which automated backups are created if
-- automated backups are enabled. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window>
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
restoreDBInstanceFromS_preferredBackupWindow :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_preferredBackupWindow = Lens.lens (\RestoreDBInstanceFromS' {preferredBackupWindow} -> preferredBackupWindow) (\s@RestoreDBInstanceFromS' {} a -> s {preferredBackupWindow = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon Relational Database Service User Guide/.
restoreDBInstanceFromS_enablePerformanceInsights :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Bool)
restoreDBInstanceFromS_enablePerformanceInsights = Lens.lens (\RestoreDBInstanceFromS' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@RestoreDBInstanceFromS' {} a -> s {enablePerformanceInsights = a} :: RestoreDBInstanceFromS)

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
restoreDBInstanceFromS_dbSecurityGroups :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe [Core.Text])
restoreDBInstanceFromS_dbSecurityGroups = Lens.lens (\RestoreDBInstanceFromS' {dbSecurityGroups} -> dbSecurityGroups) (\s@RestoreDBInstanceFromS' {} a -> s {dbSecurityGroups = a} :: RestoreDBInstanceFromS) Core.. Lens.mapping Lens._Coerce

-- | The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromS_maxAllocatedStorage :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Int)
restoreDBInstanceFromS_maxAllocatedStorage = Lens.lens (\RestoreDBInstanceFromS' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@RestoreDBInstanceFromS' {} a -> s {maxAllocatedStorage = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromS_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Bool)
restoreDBInstanceFromS_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBInstanceFromS' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBInstanceFromS' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBInstanceFromS)

-- | The list of logs that the restored DB instance is to export to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
restoreDBInstanceFromS_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe [Core.Text])
restoreDBInstanceFromS_enableCloudwatchLogsExports = Lens.lens (\RestoreDBInstanceFromS' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBInstanceFromS' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBInstanceFromS) Core.. Lens.mapping Lens._Coerce

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard@ | @gp2@ | @io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
restoreDBInstanceFromS_storageType :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_storageType = Lens.lens (\RestoreDBInstanceFromS' {storageType} -> storageType) (\s@RestoreDBInstanceFromS' {} a -> s {storageType = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
restoreDBInstanceFromS_useDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Bool)
restoreDBInstanceFromS_useDefaultProcessorFeatures = Lens.lens (\RestoreDBInstanceFromS' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@RestoreDBInstanceFromS' {} a -> s {useDefaultProcessorFeatures = a} :: RestoreDBInstanceFromS)

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
restoreDBInstanceFromS_monitoringInterval :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Int)
restoreDBInstanceFromS_monitoringInterval = Lens.lens (\RestoreDBInstanceFromS' {monitoringInterval} -> monitoringInterval) (\s@RestoreDBInstanceFromS' {} a -> s {monitoringInterval = a} :: RestoreDBInstanceFromS)

-- | The name of the option group to associate with this DB instance. If this
-- argument is omitted, the default option group for the specified engine
-- is used.
restoreDBInstanceFromS_optionGroupName :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_optionGroupName = Lens.lens (\RestoreDBInstanceFromS' {optionGroupName} -> optionGroupName) (\s@RestoreDBInstanceFromS' {} a -> s {optionGroupName = a} :: RestoreDBInstanceFromS)

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
restoreDBInstanceFromS_monitoringRoleArn :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_monitoringRoleArn = Lens.lens (\RestoreDBInstanceFromS' {monitoringRoleArn} -> monitoringRoleArn) (\s@RestoreDBInstanceFromS' {} a -> s {monitoringRoleArn = a} :: RestoreDBInstanceFromS)

-- | A DB subnet group to associate with this DB instance.
restoreDBInstanceFromS_dbSubnetGroupName :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_dbSubnetGroupName = Lens.lens (\RestoreDBInstanceFromS' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBInstanceFromS' {} a -> s {dbSubnetGroupName = a} :: RestoreDBInstanceFromS)

-- | The password for the master user. The password can include any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
restoreDBInstanceFromS_masterUserPassword :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_masterUserPassword = Lens.lens (\RestoreDBInstanceFromS' {masterUserPassword} -> masterUserPassword) (\s@RestoreDBInstanceFromS' {} a -> s {masterUserPassword = a} :: RestoreDBInstanceFromS)

-- | The name for the master user.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
restoreDBInstanceFromS_masterUsername :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_masterUsername = Lens.lens (\RestoreDBInstanceFromS' {masterUsername} -> masterUsername) (\s@RestoreDBInstanceFromS' {} a -> s {masterUsername = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
-- If the DB instance is a Multi-AZ deployment, you can\'t set the
-- @AvailabilityZone@ parameter.
restoreDBInstanceFromS_multiAZ :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Bool)
restoreDBInstanceFromS_multiAZ = Lens.lens (\RestoreDBInstanceFromS' {multiAZ} -> multiAZ) (\s@RestoreDBInstanceFromS' {} a -> s {multiAZ = a} :: RestoreDBInstanceFromS)

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
restoreDBInstanceFromS_publiclyAccessible :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Bool)
restoreDBInstanceFromS_publiclyAccessible = Lens.lens (\RestoreDBInstanceFromS' {publiclyAccessible} -> publiclyAccessible) (\s@RestoreDBInstanceFromS' {} a -> s {publiclyAccessible = a} :: RestoreDBInstanceFromS)

-- | A list of VPC security groups to associate with this DB instance.
restoreDBInstanceFromS_vpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe [Core.Text])
restoreDBInstanceFromS_vpcSecurityGroupIds = Lens.lens (\RestoreDBInstanceFromS' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBInstanceFromS' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBInstanceFromS) Core.. Lens.mapping Lens._Coerce

-- | The AWS KMS key identifier for encryption of Performance Insights data.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your AWS
-- account. Your AWS account has a different default CMK for each AWS
-- Region.
restoreDBInstanceFromS_performanceInsightsKMSKeyId :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_performanceInsightsKMSKeyId = Lens.lens (\RestoreDBInstanceFromS' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@RestoreDBInstanceFromS' {} a -> s {performanceInsightsKMSKeyId = a} :: RestoreDBInstanceFromS)

-- | The AWS KMS key identifier for an encrypted DB instance.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK). To use a CMK in a
-- different AWS account, specify the key ARN or alias ARN.
--
-- If the @StorageEncrypted@ parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default CMK. There is a default CMK for your AWS account. Your AWS
-- account has a different default CMK for each AWS Region.
restoreDBInstanceFromS_kmsKeyId :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_kmsKeyId = Lens.lens (\RestoreDBInstanceFromS' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBInstanceFromS' {} a -> s {kmsKeyId = a} :: RestoreDBInstanceFromS)

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then the
-- default @DBParameterGroup@ for the specified DB engine is used.
restoreDBInstanceFromS_dbParameterGroupName :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_dbParameterGroupName = Lens.lens (\RestoreDBInstanceFromS' {dbParameterGroupName} -> dbParameterGroupName) (\s@RestoreDBInstanceFromS' {} a -> s {dbParameterGroupName = a} :: RestoreDBInstanceFromS)

-- | The Availability Zone that the DB instance is created in. For
-- information about AWS Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>
-- in the /Amazon RDS User Guide./
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- AWS Region.
--
-- Example: @us-east-1d@
--
-- Constraint: The @AvailabilityZone@ parameter can\'t be specified if the
-- DB instance is a Multi-AZ deployment. The specified Availability Zone
-- must be in the same AWS Region as the current endpoint.
restoreDBInstanceFromS_availabilityZone :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_availabilityZone = Lens.lens (\RestoreDBInstanceFromS' {availabilityZone} -> availabilityZone) (\s@RestoreDBInstanceFromS' {} a -> s {availabilityZone = a} :: RestoreDBInstanceFromS)

-- | The version number of the database engine to use. Choose the latest
-- minor version of your database engine. For information about engine
-- versions, see @CreateDBInstance@, or call @DescribeDBEngineVersions@.
restoreDBInstanceFromS_engineVersion :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_engineVersion = Lens.lens (\RestoreDBInstanceFromS' {engineVersion} -> engineVersion) (\s@RestoreDBInstanceFromS' {} a -> s {engineVersion = a} :: RestoreDBInstanceFromS)

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
restoreDBInstanceFromS_preferredMaintenanceWindow :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_preferredMaintenanceWindow = Lens.lens (\RestoreDBInstanceFromS' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@RestoreDBInstanceFromS' {} a -> s {preferredMaintenanceWindow = a} :: RestoreDBInstanceFromS)

-- | The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
restoreDBInstanceFromS_performanceInsightsRetentionPeriod :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Int)
restoreDBInstanceFromS_performanceInsightsRetentionPeriod = Lens.lens (\RestoreDBInstanceFromS' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@RestoreDBInstanceFromS' {} a -> s {performanceInsightsRetentionPeriod = a} :: RestoreDBInstanceFromS)

-- | The license model for this DB instance. Use @general-public-license@.
restoreDBInstanceFromS_licenseModel :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_licenseModel = Lens.lens (\RestoreDBInstanceFromS' {licenseModel} -> licenseModel) (\s@RestoreDBInstanceFromS' {} a -> s {licenseModel = a} :: RestoreDBInstanceFromS)

-- | A list of tags to associate with this DB instance. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromS_tags :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe [Tag])
restoreDBInstanceFromS_tags = Lens.lens (\RestoreDBInstanceFromS' {tags} -> tags) (\s@RestoreDBInstanceFromS' {} a -> s {tags = a} :: RestoreDBInstanceFromS) Core.. Lens.mapping Lens._Coerce

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
restoreDBInstanceFromS_processorFeatures :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe [ProcessorFeature])
restoreDBInstanceFromS_processorFeatures = Lens.lens (\RestoreDBInstanceFromS' {processorFeatures} -> processorFeatures) (\s@RestoreDBInstanceFromS' {} a -> s {processorFeatures = a} :: RestoreDBInstanceFromS) Core.. Lens.mapping Lens._Coerce

-- | The port number on which the database accepts connections.
--
-- Type: Integer
--
-- Valid Values: @1150@-@65535@
--
-- Default: @3306@
restoreDBInstanceFromS_port :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Int)
restoreDBInstanceFromS_port = Lens.lens (\RestoreDBInstanceFromS' {port} -> port) (\s@RestoreDBInstanceFromS' {} a -> s {port = a} :: RestoreDBInstanceFromS)

-- | The name of the database to create when the DB instance is created.
-- Follow the naming rules specified in @CreateDBInstance@.
restoreDBInstanceFromS_dbName :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_dbName = Lens.lens (\RestoreDBInstanceFromS' {dbName} -> dbName) (\s@RestoreDBInstanceFromS' {} a -> s {dbName = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
restoreDBInstanceFromS_copyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Bool)
restoreDBInstanceFromS_copyTagsToSnapshot = Lens.lens (\RestoreDBInstanceFromS' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBInstanceFromS' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBInstanceFromS)

-- | The prefix of your Amazon S3 bucket.
restoreDBInstanceFromS_s3Prefix :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Text)
restoreDBInstanceFromS_s3Prefix = Lens.lens (\RestoreDBInstanceFromS' {s3Prefix} -> s3Prefix) (\s@RestoreDBInstanceFromS' {} a -> s {s3Prefix = a} :: RestoreDBInstanceFromS)

-- | The amount of storage (in gigabytes) to allocate initially for the DB
-- instance. Follow the allocation rules specified in @CreateDBInstance@.
--
-- Be sure to allocate enough memory for your new DB instance so that the
-- restore operation can succeed. You can also allocate additional memory
-- for future growth.
restoreDBInstanceFromS_allocatedStorage :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Int)
restoreDBInstanceFromS_allocatedStorage = Lens.lens (\RestoreDBInstanceFromS' {allocatedStorage} -> allocatedStorage) (\s@RestoreDBInstanceFromS' {} a -> s {allocatedStorage = a} :: RestoreDBInstanceFromS)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- allocate initially for the DB instance. For information about valid Iops
-- values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
-- in the /Amazon RDS User Guide./
restoreDBInstanceFromS_iops :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Int)
restoreDBInstanceFromS_iops = Lens.lens (\RestoreDBInstanceFromS' {iops} -> iops) (\s@RestoreDBInstanceFromS' {} a -> s {iops = a} :: RestoreDBInstanceFromS)

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the DB instance during the maintenance window. By
-- default, minor engine upgrades are not applied automatically.
restoreDBInstanceFromS_autoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceFromS (Core.Maybe Core.Bool)
restoreDBInstanceFromS_autoMinorVersionUpgrade = Lens.lens (\RestoreDBInstanceFromS' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@RestoreDBInstanceFromS' {} a -> s {autoMinorVersionUpgrade = a} :: RestoreDBInstanceFromS)

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
restoreDBInstanceFromS_dbInstanceIdentifier :: Lens.Lens' RestoreDBInstanceFromS Core.Text
restoreDBInstanceFromS_dbInstanceIdentifier = Lens.lens (\RestoreDBInstanceFromS' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RestoreDBInstanceFromS' {} a -> s {dbInstanceIdentifier = a} :: RestoreDBInstanceFromS)

-- | The compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all AWS
-- Regions, or for all database engines. For the full list of DB instance
-- classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Importing from Amazon S3 isn\'t supported on the db.t2.micro DB instance
-- class.
restoreDBInstanceFromS_dbInstanceClass :: Lens.Lens' RestoreDBInstanceFromS Core.Text
restoreDBInstanceFromS_dbInstanceClass = Lens.lens (\RestoreDBInstanceFromS' {dbInstanceClass} -> dbInstanceClass) (\s@RestoreDBInstanceFromS' {} a -> s {dbInstanceClass = a} :: RestoreDBInstanceFromS)

-- | The name of the database engine to be used for this instance.
--
-- Valid Values: @mysql@
restoreDBInstanceFromS_engine :: Lens.Lens' RestoreDBInstanceFromS Core.Text
restoreDBInstanceFromS_engine = Lens.lens (\RestoreDBInstanceFromS' {engine} -> engine) (\s@RestoreDBInstanceFromS' {} a -> s {engine = a} :: RestoreDBInstanceFromS)

-- | The name of the engine of your source database.
--
-- Valid Values: @mysql@
restoreDBInstanceFromS_sourceEngine :: Lens.Lens' RestoreDBInstanceFromS Core.Text
restoreDBInstanceFromS_sourceEngine = Lens.lens (\RestoreDBInstanceFromS' {sourceEngine} -> sourceEngine) (\s@RestoreDBInstanceFromS' {} a -> s {sourceEngine = a} :: RestoreDBInstanceFromS)

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.6 and 5.7 are supported.
--
-- Example: @5.6.40@
restoreDBInstanceFromS_sourceEngineVersion :: Lens.Lens' RestoreDBInstanceFromS Core.Text
restoreDBInstanceFromS_sourceEngineVersion = Lens.lens (\RestoreDBInstanceFromS' {sourceEngineVersion} -> sourceEngineVersion) (\s@RestoreDBInstanceFromS' {} a -> s {sourceEngineVersion = a} :: RestoreDBInstanceFromS)

-- | The name of your Amazon S3 bucket that contains your database backup
-- file.
restoreDBInstanceFromS_s3BucketName :: Lens.Lens' RestoreDBInstanceFromS Core.Text
restoreDBInstanceFromS_s3BucketName = Lens.lens (\RestoreDBInstanceFromS' {s3BucketName} -> s3BucketName) (\s@RestoreDBInstanceFromS' {} a -> s {s3BucketName = a} :: RestoreDBInstanceFromS)

-- | An AWS Identity and Access Management (IAM) role to allow Amazon RDS to
-- access your Amazon S3 bucket.
restoreDBInstanceFromS_s3IngestionRoleArn :: Lens.Lens' RestoreDBInstanceFromS Core.Text
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
            Core.<$> (x Core..@? "DBInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RestoreDBInstanceFromS

instance Core.NFData RestoreDBInstanceFromS

instance Core.ToHeaders RestoreDBInstanceFromS where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RestoreDBInstanceFromS where
  toPath = Core.const "/"

instance Core.ToQuery RestoreDBInstanceFromS where
  toQuery RestoreDBInstanceFromS' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RestoreDBInstanceFromS" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "DeletionProtection" Core.=: deletionProtection,
        "StorageEncrypted" Core.=: storageEncrypted,
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "EnablePerformanceInsights"
          Core.=: enablePerformanceInsights,
        "DBSecurityGroups"
          Core.=: Core.toQuery
            ( Core.toQueryList "DBSecurityGroupName"
                Core.<$> dbSecurityGroups
            ),
        "MaxAllocatedStorage" Core.=: maxAllocatedStorage,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> enableCloudwatchLogsExports
            ),
        "StorageType" Core.=: storageType,
        "UseDefaultProcessorFeatures"
          Core.=: useDefaultProcessorFeatures,
        "MonitoringInterval" Core.=: monitoringInterval,
        "OptionGroupName" Core.=: optionGroupName,
        "MonitoringRoleArn" Core.=: monitoringRoleArn,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "MasterUserPassword" Core.=: masterUserPassword,
        "MasterUsername" Core.=: masterUsername,
        "MultiAZ" Core.=: multiAZ,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Core.<$> vpcSecurityGroupIds
            ),
        "PerformanceInsightsKMSKeyId"
          Core.=: performanceInsightsKMSKeyId,
        "KmsKeyId" Core.=: kmsKeyId,
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "AvailabilityZone" Core.=: availabilityZone,
        "EngineVersion" Core.=: engineVersion,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "PerformanceInsightsRetentionPeriod"
          Core.=: performanceInsightsRetentionPeriod,
        "LicenseModel" Core.=: licenseModel,
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "ProcessorFeatures"
          Core.=: Core.toQuery
            ( Core.toQueryList "ProcessorFeature"
                Core.<$> processorFeatures
            ),
        "Port" Core.=: port,
        "DBName" Core.=: dbName,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "S3Prefix" Core.=: s3Prefix,
        "AllocatedStorage" Core.=: allocatedStorage,
        "Iops" Core.=: iops,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
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
  { dbInstance :: Core.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RestoreDBInstanceFromSResponse
newRestoreDBInstanceFromSResponse pHttpStatus_ =
  RestoreDBInstanceFromSResponse'
    { dbInstance =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreDBInstanceFromSResponse_dbInstance :: Lens.Lens' RestoreDBInstanceFromSResponse (Core.Maybe DBInstance)
restoreDBInstanceFromSResponse_dbInstance = Lens.lens (\RestoreDBInstanceFromSResponse' {dbInstance} -> dbInstance) (\s@RestoreDBInstanceFromSResponse' {} a -> s {dbInstance = a} :: RestoreDBInstanceFromSResponse)

-- | The response's http status code.
restoreDBInstanceFromSResponse_httpStatus :: Lens.Lens' RestoreDBInstanceFromSResponse Core.Int
restoreDBInstanceFromSResponse_httpStatus = Lens.lens (\RestoreDBInstanceFromSResponse' {httpStatus} -> httpStatus) (\s@RestoreDBInstanceFromSResponse' {} a -> s {httpStatus = a} :: RestoreDBInstanceFromSResponse)

instance Core.NFData RestoreDBInstanceFromSResponse
