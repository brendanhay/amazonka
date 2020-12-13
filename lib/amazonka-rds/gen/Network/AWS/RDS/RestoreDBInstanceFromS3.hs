{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RestoreDBInstanceFromS3
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Relational Database Service (Amazon RDS) supports importing MySQL databases by using backup files. You can create a backup of your on-premises database, store it on Amazon Simple Storage Service (Amazon S3), and then restore the backup file onto a new Amazon RDS DB instance running MySQL. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/MySQL.Procedural.Importing.html Importing Data into an Amazon RDS MySQL DB Instance> in the /Amazon RDS User Guide./
module Network.AWS.RDS.RestoreDBInstanceFromS3
  ( -- * Creating a request
    RestoreDBInstanceFromS3 (..),
    mkRestoreDBInstanceFromS3,

    -- ** Request lenses
    rdifsEngineVersion,
    rdifsDBSecurityGroups,
    rdifsDeletionProtection,
    rdifsStorageEncrypted,
    rdifsMasterUserPassword,
    rdifsPubliclyAccessible,
    rdifsAutoMinorVersionUpgrade,
    rdifsMasterUsername,
    rdifsDBSubnetGroupName,
    rdifsMonitoringRoleARN,
    rdifsIOPS,
    rdifsS3IngestionRoleARN,
    rdifsMonitoringInterval,
    rdifsSourceEngine,
    rdifsEngine,
    rdifsSourceEngineVersion,
    rdifsProcessorFeatures,
    rdifsDBInstanceClass,
    rdifsLicenseModel,
    rdifsPreferredMaintenanceWindow,
    rdifsPerformanceInsightsRetentionPeriod,
    rdifsDBInstanceIdentifier,
    rdifsMaxAllocatedStorage,
    rdifsEnablePerformanceInsights,
    rdifsKMSKeyId,
    rdifsDBParameterGroupName,
    rdifsPreferredBackupWindow,
    rdifsAvailabilityZone,
    rdifsBackupRetentionPeriod,
    rdifsPerformanceInsightsKMSKeyId,
    rdifsVPCSecurityGroupIds,
    rdifsMultiAZ,
    rdifsS3Prefix,
    rdifsAllocatedStorage,
    rdifsOptionGroupName,
    rdifsCopyTagsToSnapshot,
    rdifsS3BucketName,
    rdifsTags,
    rdifsPort,
    rdifsEnableIAMDatabaseAuthentication,
    rdifsUseDefaultProcessorFeatures,
    rdifsStorageType,
    rdifsEnableCloudwatchLogsExports,
    rdifsDBName,

    -- * Destructuring the response
    RestoreDBInstanceFromS3Response (..),
    mkRestoreDBInstanceFromS3Response,

    -- ** Response lenses
    rdifsrsDBInstance,
    rdifsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRestoreDBInstanceFromS3' smart constructor.
data RestoreDBInstanceFromS3 = RestoreDBInstanceFromS3'
  { -- | The version number of the database engine to use. Choose the latest minor version of your database engine. For information about engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
    engineVersion :: Lude.Maybe Lude.Text,
    -- | A list of DB security groups to associate with this DB instance.
    --
    -- Default: The default DB security group for the database engine.
    dbSecurityGroups :: Lude.Maybe [Lude.Text],
    -- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
    deletionProtection :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether the new DB instance is encrypted or not.
    storageEncrypted :: Lude.Maybe Lude.Bool,
    -- | The password for the master user. The password can include any printable ASCII character except "/", """, or "@".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
    -- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
    -- For more information, see 'CreateDBInstance' .
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are not applied automatically.
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    -- | The name for the master user.
    --
    -- Constraints:
    --
    --     * Must be 1 to 16 letters or numbers.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    masterUsername :: Lude.Maybe Lude.Text,
    -- | A DB subnet group to associate with this DB instance.
    dbSubnetGroupName :: Lude.Maybe Lude.Text,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide./
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
    monitoringRoleARN :: Lude.Maybe Lude.Text,
    -- | The amount of Provisioned IOPS (input/output operations per second) to allocate initially for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./
    iops :: Lude.Maybe Lude.Int,
    -- | An AWS Identity and Access Management (IAM) role to allow Amazon RDS to access your Amazon S3 bucket.
    s3IngestionRoleARN :: Lude.Text,
    -- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
    -- Valid Values: 0, 1, 5, 10, 15, 30, 60
    -- Default: @0@
    monitoringInterval :: Lude.Maybe Lude.Int,
    -- | The name of the engine of your source database.
    --
    -- Valid Values: @mysql@
    sourceEngine :: Lude.Text,
    -- | The name of the database engine to be used for this instance.
    --
    -- Valid Values: @mysql@
    engine :: Lude.Text,
    -- | The version of the database that the backup files were created from.
    --
    -- MySQL versions 5.6 and 5.7 are supported.
    -- Example: @5.6.40@
    sourceEngineVersion :: Lude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
    processorFeatures :: Lude.Maybe [ProcessorFeature],
    -- | The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
    --
    -- Importing from Amazon S3 isn't supported on the db.t2.micro DB instance class.
    dbInstanceClass :: Lude.Text,
    -- | The license model for this DB instance. Use @general-public-license@ .
    licenseModel :: Lude.Maybe Lude.Text,
    -- | The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> in the /Amazon RDS User Guide./
    --
    -- Constraints:
    --
    --     * Must be in the format @ddd:hh24:mi-ddd:hh24:mi@ .
    --
    --
    --     * Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    --
    --     * Must be in Universal Coordinated Time (UTC).
    --
    --
    --     * Must not conflict with the preferred backup window.
    --
    --
    --     * Must be at least 30 minutes.
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
    -- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
    maxAllocatedStorage :: Lude.Maybe Lude.Int,
    -- | A value that indicates whether to enable Performance Insights for the DB instance.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
    enablePerformanceInsights :: Lude.Maybe Lude.Bool,
    -- | The AWS KMS key identifier for an encrypted DB instance.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key.
    -- If the @StorageEncrypted@ parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The name of the DB parameter group to associate with this DB instance.
    --
    -- If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used.
    dbParameterGroupName :: Lude.Maybe Lude.Text,
    -- | The time range each day during which automated backups are created if automated backups are enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide./
    --
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
    -- | The Availability Zone that the DB instance is created in. For information about AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> in the /Amazon RDS User Guide./
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
    -- Example: @us-east-1d@
    -- Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. For more information, see @CreateDBInstance@ .
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key.
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    performanceInsightsKMSKeyId :: Lude.Maybe Lude.Text,
    -- | A list of VPC security groups to associate with this DB instance.
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment. If the DB instance is a Multi-AZ deployment, you can't set the @AvailabilityZone@ parameter.
    multiAZ :: Lude.Maybe Lude.Bool,
    -- | The prefix of your Amazon S3 bucket.
    s3Prefix :: Lude.Maybe Lude.Text,
    -- | The amount of storage (in gigabytes) to allocate initially for the DB instance. Follow the allocation rules specified in @CreateDBInstance@ .
    allocatedStorage :: Lude.Maybe Lude.Int,
    -- | The name of the option group to associate with this DB instance. If this argument is omitted, the default option group for the specified engine is used.
    optionGroupName :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    -- | The name of your Amazon S3 bucket that contains your database backup file.
    s3BucketName :: Lude.Text,
    -- | A list of tags to associate with this DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide./
    tags :: Lude.Maybe [Tag],
    -- | The port number on which the database accepts connections.
    --
    -- Type: Integer
    -- Valid Values: @1150@ -@65535@
    -- Default: @3306@
    port :: Lude.Maybe Lude.Int,
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
    useDefaultProcessorFeatures :: Lude.Maybe Lude.Bool,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @standard@ | @gp2@ | @io1@
    -- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
    -- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
    storageType :: Lude.Maybe Lude.Text,
    -- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
    enableCloudwatchLogsExports :: Lude.Maybe [Lude.Text],
    -- | The name of the database to create when the DB instance is created. Follow the naming rules specified in @CreateDBInstance@ .
    dbName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBInstanceFromS3' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The version number of the database engine to use. Choose the latest minor version of your database engine. For information about engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
-- * 'dbSecurityGroups' - A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
-- * 'deletionProtection' - A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
-- * 'storageEncrypted' - A value that indicates whether the new DB instance is encrypted or not.
-- * 'masterUserPassword' - The password for the master user. The password can include any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
-- * 'publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
-- * 'autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are not applied automatically.
-- * 'masterUsername' - The name for the master user.
--
-- Constraints:
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
-- * 'dbSubnetGroupName' - A DB subnet group to associate with this DB instance.
-- * 'monitoringRoleARN' - The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
-- * 'iops' - The amount of Provisioned IOPS (input/output operations per second) to allocate initially for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./
-- * 's3IngestionRoleARN' - An AWS Identity and Access Management (IAM) role to allow Amazon RDS to access your Amazon S3 bucket.
-- * 'monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: 0, 1, 5, 10, 15, 30, 60
-- Default: @0@
-- * 'sourceEngine' - The name of the engine of your source database.
--
-- Valid Values: @mysql@
-- * 'engine' - The name of the database engine to be used for this instance.
--
-- Valid Values: @mysql@
-- * 'sourceEngineVersion' - The version of the database that the backup files were created from.
--
-- MySQL versions 5.6 and 5.7 are supported.
-- Example: @5.6.40@
-- * 'processorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
-- * 'dbInstanceClass' - The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- Importing from Amazon S3 isn't supported on the db.t2.micro DB instance class.
-- * 'licenseModel' - The license model for this DB instance. Use @general-public-license@ .
-- * 'preferredMaintenanceWindow' - The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> in the /Amazon RDS User Guide./
--
-- Constraints:
--
--     * Must be in the format @ddd:hh24:mi-ddd:hh24:mi@ .
--
--
--     * Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
--
--     * Must be in Universal Coordinated Time (UTC).
--
--
--     * Must not conflict with the preferred backup window.
--
--
--     * Must be at least 30 minutes.
--
--
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
-- * 'maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
-- * 'enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB instance.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
-- * 'kmsKeyId' - The AWS KMS key identifier for an encrypted DB instance.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key.
-- If the @StorageEncrypted@ parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used.
-- * 'preferredBackupWindow' - The time range each day during which automated backups are created if automated backups are enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide./
--
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
-- * 'availabilityZone' - The Availability Zone that the DB instance is created in. For information about AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> in the /Amazon RDS User Guide./
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
-- Example: @us-east-1d@
-- Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint.
-- * 'backupRetentionPeriod' - The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. For more information, see @CreateDBInstance@ .
-- * 'performanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'vpcSecurityGroupIds' - A list of VPC security groups to associate with this DB instance.
-- * 'multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment. If the DB instance is a Multi-AZ deployment, you can't set the @AvailabilityZone@ parameter.
-- * 's3Prefix' - The prefix of your Amazon S3 bucket.
-- * 'allocatedStorage' - The amount of storage (in gigabytes) to allocate initially for the DB instance. Follow the allocation rules specified in @CreateDBInstance@ .
-- * 'optionGroupName' - The name of the option group to associate with this DB instance. If this argument is omitted, the default option group for the specified engine is used.
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
-- * 's3BucketName' - The name of your Amazon S3 bucket that contains your database backup file.
-- * 'tags' - A list of tags to associate with this DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide./
-- * 'port' - The port number on which the database accepts connections.
--
-- Type: Integer
-- Valid Values: @1150@ -@65535@
-- Default: @3306@
-- * 'enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
-- * 'useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance uses its default processor features.
-- * 'storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard@ | @gp2@ | @io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
-- * 'enableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
-- * 'dbName' - The name of the database to create when the DB instance is created. Follow the naming rules specified in @CreateDBInstance@ .
mkRestoreDBInstanceFromS3 ::
  -- | 's3IngestionRoleARN'
  Lude.Text ->
  -- | 'sourceEngine'
  Lude.Text ->
  -- | 'engine'
  Lude.Text ->
  -- | 'sourceEngineVersion'
  Lude.Text ->
  -- | 'dbInstanceClass'
  Lude.Text ->
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  -- | 's3BucketName'
  Lude.Text ->
  RestoreDBInstanceFromS3
mkRestoreDBInstanceFromS3
  pS3IngestionRoleARN_
  pSourceEngine_
  pEngine_
  pSourceEngineVersion_
  pDBInstanceClass_
  pDBInstanceIdentifier_
  pS3BucketName_ =
    RestoreDBInstanceFromS3'
      { engineVersion = Lude.Nothing,
        dbSecurityGroups = Lude.Nothing,
        deletionProtection = Lude.Nothing,
        storageEncrypted = Lude.Nothing,
        masterUserPassword = Lude.Nothing,
        publiclyAccessible = Lude.Nothing,
        autoMinorVersionUpgrade = Lude.Nothing,
        masterUsername = Lude.Nothing,
        dbSubnetGroupName = Lude.Nothing,
        monitoringRoleARN = Lude.Nothing,
        iops = Lude.Nothing,
        s3IngestionRoleARN = pS3IngestionRoleARN_,
        monitoringInterval = Lude.Nothing,
        sourceEngine = pSourceEngine_,
        engine = pEngine_,
        sourceEngineVersion = pSourceEngineVersion_,
        processorFeatures = Lude.Nothing,
        dbInstanceClass = pDBInstanceClass_,
        licenseModel = Lude.Nothing,
        preferredMaintenanceWindow = Lude.Nothing,
        performanceInsightsRetentionPeriod = Lude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        maxAllocatedStorage = Lude.Nothing,
        enablePerformanceInsights = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        dbParameterGroupName = Lude.Nothing,
        preferredBackupWindow = Lude.Nothing,
        availabilityZone = Lude.Nothing,
        backupRetentionPeriod = Lude.Nothing,
        performanceInsightsKMSKeyId = Lude.Nothing,
        vpcSecurityGroupIds = Lude.Nothing,
        multiAZ = Lude.Nothing,
        s3Prefix = Lude.Nothing,
        allocatedStorage = Lude.Nothing,
        optionGroupName = Lude.Nothing,
        copyTagsToSnapshot = Lude.Nothing,
        s3BucketName = pS3BucketName_,
        tags = Lude.Nothing,
        port = Lude.Nothing,
        enableIAMDatabaseAuthentication = Lude.Nothing,
        useDefaultProcessorFeatures = Lude.Nothing,
        storageType = Lude.Nothing,
        enableCloudwatchLogsExports = Lude.Nothing,
        dbName = Lude.Nothing
      }

-- | The version number of the database engine to use. Choose the latest minor version of your database engine. For information about engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsEngineVersion :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsEngineVersion = Lens.lens (engineVersion :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
--
-- /Note:/ Consider using 'dbSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsDBSecurityGroups :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe [Lude.Text])
rdifsDBSecurityGroups = Lens.lens (dbSecurityGroups :: RestoreDBInstanceFromS3 -> Lude.Maybe [Lude.Text]) (\s a -> s {dbSecurityGroups = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsDBSecurityGroups "Use generic-lens or generic-optics with 'dbSecurityGroups' instead." #-}

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsDeletionProtection :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Bool)
rdifsDeletionProtection = Lens.lens (deletionProtection :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | A value that indicates whether the new DB instance is encrypted or not.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsStorageEncrypted :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Bool)
rdifsStorageEncrypted = Lens.lens (storageEncrypted :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {storageEncrypted = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | The password for the master user. The password can include any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsMasterUserPassword :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsMasterUserPassword = Lens.lens (masterUserPassword :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {masterUserPassword = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsPubliclyAccessible :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Bool)
rdifsPubliclyAccessible = Lens.lens (publiclyAccessible :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are not applied automatically.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsAutoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Bool)
rdifsAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The name for the master user.
--
-- Constraints:
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
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsMasterUsername :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsMasterUsername = Lens.lens (masterUsername :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | A DB subnet group to associate with this DB instance.
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsDBSubnetGroupName :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- /Note:/ Consider using 'monitoringRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsMonitoringRoleARN :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsMonitoringRoleARN = Lens.lens (monitoringRoleARN :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {monitoringRoleARN = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsMonitoringRoleARN "Use generic-lens or generic-optics with 'monitoringRoleARN' instead." #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to allocate initially for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsIOPS :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Int)
rdifsIOPS = Lens.lens (iops :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | An AWS Identity and Access Management (IAM) role to allow Amazon RDS to access your Amazon S3 bucket.
--
-- /Note:/ Consider using 's3IngestionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsS3IngestionRoleARN :: Lens.Lens' RestoreDBInstanceFromS3 Lude.Text
rdifsS3IngestionRoleARN = Lens.lens (s3IngestionRoleARN :: RestoreDBInstanceFromS3 -> Lude.Text) (\s a -> s {s3IngestionRoleARN = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsS3IngestionRoleARN "Use generic-lens or generic-optics with 's3IngestionRoleARN' instead." #-}

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: 0, 1, 5, 10, 15, 30, 60
-- Default: @0@
--
-- /Note:/ Consider using 'monitoringInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsMonitoringInterval :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Int)
rdifsMonitoringInterval = Lens.lens (monitoringInterval :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {monitoringInterval = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsMonitoringInterval "Use generic-lens or generic-optics with 'monitoringInterval' instead." #-}

-- | The name of the engine of your source database.
--
-- Valid Values: @mysql@
--
-- /Note:/ Consider using 'sourceEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsSourceEngine :: Lens.Lens' RestoreDBInstanceFromS3 Lude.Text
rdifsSourceEngine = Lens.lens (sourceEngine :: RestoreDBInstanceFromS3 -> Lude.Text) (\s a -> s {sourceEngine = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsSourceEngine "Use generic-lens or generic-optics with 'sourceEngine' instead." #-}

-- | The name of the database engine to be used for this instance.
--
-- Valid Values: @mysql@
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsEngine :: Lens.Lens' RestoreDBInstanceFromS3 Lude.Text
rdifsEngine = Lens.lens (engine :: RestoreDBInstanceFromS3 -> Lude.Text) (\s a -> s {engine = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.6 and 5.7 are supported.
-- Example: @5.6.40@
--
-- /Note:/ Consider using 'sourceEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsSourceEngineVersion :: Lens.Lens' RestoreDBInstanceFromS3 Lude.Text
rdifsSourceEngineVersion = Lens.lens (sourceEngineVersion :: RestoreDBInstanceFromS3 -> Lude.Text) (\s a -> s {sourceEngineVersion = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsSourceEngineVersion "Use generic-lens or generic-optics with 'sourceEngineVersion' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe [ProcessorFeature])
rdifsProcessorFeatures = Lens.lens (processorFeatures :: RestoreDBInstanceFromS3 -> Lude.Maybe [ProcessorFeature]) (\s a -> s {processorFeatures = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- Importing from Amazon S3 isn't supported on the db.t2.micro DB instance class.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsDBInstanceClass :: Lens.Lens' RestoreDBInstanceFromS3 Lude.Text
rdifsDBInstanceClass = Lens.lens (dbInstanceClass :: RestoreDBInstanceFromS3 -> Lude.Text) (\s a -> s {dbInstanceClass = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | The license model for this DB instance. Use @general-public-license@ .
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsLicenseModel :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsLicenseModel = Lens.lens (licenseModel :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> in the /Amazon RDS User Guide./
--
-- Constraints:
--
--     * Must be in the format @ddd:hh24:mi-ddd:hh24:mi@ .
--
--
--     * Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
--
--     * Must be in Universal Coordinated Time (UTC).
--
--
--     * Must not conflict with the preferred backup window.
--
--
--     * Must be at least 30 minutes.
--
--
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsPreferredMaintenanceWindow :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
--
-- /Note:/ Consider using 'performanceInsightsRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsPerformanceInsightsRetentionPeriod :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Int)
rdifsPerformanceInsightsRetentionPeriod = Lens.lens (performanceInsightsRetentionPeriod :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {performanceInsightsRetentionPeriod = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsPerformanceInsightsRetentionPeriod "Use generic-lens or generic-optics with 'performanceInsightsRetentionPeriod' instead." #-}

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
rdifsDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceFromS3 Lude.Text
rdifsDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: RestoreDBInstanceFromS3 -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsMaxAllocatedStorage :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Int)
rdifsMaxAllocatedStorage = Lens.lens (maxAllocatedStorage :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {maxAllocatedStorage = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsMaxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead." #-}

-- | A value that indicates whether to enable Performance Insights for the DB instance.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
--
-- /Note:/ Consider using 'enablePerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsEnablePerformanceInsights :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Bool)
rdifsEnablePerformanceInsights = Lens.lens (enablePerformanceInsights :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {enablePerformanceInsights = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsEnablePerformanceInsights "Use generic-lens or generic-optics with 'enablePerformanceInsights' instead." #-}

-- | The AWS KMS key identifier for an encrypted DB instance.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key.
-- If the @StorageEncrypted@ parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsKMSKeyId :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsKMSKeyId = Lens.lens (kmsKeyId :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used.
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsDBParameterGroupName :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsDBParameterGroupName = Lens.lens (dbParameterGroupName :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupName = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | The time range each day during which automated backups are created if automated backups are enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide./
--
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
rdifsPreferredBackupWindow :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsPreferredBackupWindow = Lens.lens (preferredBackupWindow :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The Availability Zone that the DB instance is created in. For information about AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> in the /Amazon RDS User Guide./
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
-- Example: @us-east-1d@
-- Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsAvailabilityZone :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsAvailabilityZone = Lens.lens (availabilityZone :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. For more information, see @CreateDBInstance@ .
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsBackupRetentionPeriod :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Int)
rdifsBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'performanceInsightsKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsPerformanceInsightsKMSKeyId :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsPerformanceInsightsKMSKeyId = Lens.lens (performanceInsightsKMSKeyId :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {performanceInsightsKMSKeyId = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsPerformanceInsightsKMSKeyId "Use generic-lens or generic-optics with 'performanceInsightsKMSKeyId' instead." #-}

-- | A list of VPC security groups to associate with this DB instance.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsVPCSecurityGroupIds :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe [Lude.Text])
rdifsVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: RestoreDBInstanceFromS3 -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment. If the DB instance is a Multi-AZ deployment, you can't set the @AvailabilityZone@ parameter.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsMultiAZ :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Bool)
rdifsMultiAZ = Lens.lens (multiAZ :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The prefix of your Amazon S3 bucket.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsS3Prefix :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsS3Prefix = Lens.lens (s3Prefix :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The amount of storage (in gigabytes) to allocate initially for the DB instance. Follow the allocation rules specified in @CreateDBInstance@ .
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsAllocatedStorage :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Int)
rdifsAllocatedStorage = Lens.lens (allocatedStorage :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | The name of the option group to associate with this DB instance. If this argument is omitted, the default option group for the specified engine is used.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsOptionGroupName :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsOptionGroupName = Lens.lens (optionGroupName :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsCopyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Bool)
rdifsCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The name of your Amazon S3 bucket that contains your database backup file.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsS3BucketName :: Lens.Lens' RestoreDBInstanceFromS3 Lude.Text
rdifsS3BucketName = Lens.lens (s3BucketName :: RestoreDBInstanceFromS3 -> Lude.Text) (\s a -> s {s3BucketName = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | A list of tags to associate with this DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsTags :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe [Tag])
rdifsTags = Lens.lens (tags :: RestoreDBInstanceFromS3 -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which the database accepts connections.
--
-- Type: Integer
-- Valid Values: @1150@ -@65535@
-- Default: @3306@
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsPort :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Int)
rdifsPort = Lens.lens (port :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Bool)
rdifsEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsUseDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Bool)
rdifsUseDefaultProcessorFeatures = Lens.lens (useDefaultProcessorFeatures :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {useDefaultProcessorFeatures = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsUseDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead." #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard@ | @gp2@ | @io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsStorageType :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsStorageType = Lens.lens (storageType :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe [Lude.Text])
rdifsEnableCloudwatchLogsExports = Lens.lens (enableCloudwatchLogsExports :: RestoreDBInstanceFromS3 -> Lude.Maybe [Lude.Text]) (\s a -> s {enableCloudwatchLogsExports = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

-- | The name of the database to create when the DB instance is created. Follow the naming rules specified in @CreateDBInstance@ .
--
-- /Note:/ Consider using 'dbName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsDBName :: Lens.Lens' RestoreDBInstanceFromS3 (Lude.Maybe Lude.Text)
rdifsDBName = Lens.lens (dbName :: RestoreDBInstanceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {dbName = a} :: RestoreDBInstanceFromS3)
{-# DEPRECATED rdifsDBName "Use generic-lens or generic-optics with 'dbName' instead." #-}

instance Lude.AWSRequest RestoreDBInstanceFromS3 where
  type Rs RestoreDBInstanceFromS3 = RestoreDBInstanceFromS3Response
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RestoreDBInstanceFromS3Result"
      ( \s h x ->
          RestoreDBInstanceFromS3Response'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreDBInstanceFromS3 where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestoreDBInstanceFromS3 where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreDBInstanceFromS3 where
  toQuery RestoreDBInstanceFromS3' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RestoreDBInstanceFromS3" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "DBSecurityGroups"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "DBSecurityGroupName" Lude.<$> dbSecurityGroups),
        "DeletionProtection" Lude.=: deletionProtection,
        "StorageEncrypted" Lude.=: storageEncrypted,
        "MasterUserPassword" Lude.=: masterUserPassword,
        "PubliclyAccessible" Lude.=: publiclyAccessible,
        "AutoMinorVersionUpgrade" Lude.=: autoMinorVersionUpgrade,
        "MasterUsername" Lude.=: masterUsername,
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "MonitoringRoleArn" Lude.=: monitoringRoleARN,
        "Iops" Lude.=: iops,
        "S3IngestionRoleArn" Lude.=: s3IngestionRoleARN,
        "MonitoringInterval" Lude.=: monitoringInterval,
        "SourceEngine" Lude.=: sourceEngine,
        "Engine" Lude.=: engine,
        "SourceEngineVersion" Lude.=: sourceEngineVersion,
        "ProcessorFeatures"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "ProcessorFeature" Lude.<$> processorFeatures),
        "DBInstanceClass" Lude.=: dbInstanceClass,
        "LicenseModel" Lude.=: licenseModel,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "PerformanceInsightsRetentionPeriod"
          Lude.=: performanceInsightsRetentionPeriod,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "MaxAllocatedStorage" Lude.=: maxAllocatedStorage,
        "EnablePerformanceInsights" Lude.=: enablePerformanceInsights,
        "KmsKeyId" Lude.=: kmsKeyId,
        "DBParameterGroupName" Lude.=: dbParameterGroupName,
        "PreferredBackupWindow" Lude.=: preferredBackupWindow,
        "AvailabilityZone" Lude.=: availabilityZone,
        "BackupRetentionPeriod" Lude.=: backupRetentionPeriod,
        "PerformanceInsightsKMSKeyId" Lude.=: performanceInsightsKMSKeyId,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "MultiAZ" Lude.=: multiAZ,
        "S3Prefix" Lude.=: s3Prefix,
        "AllocatedStorage" Lude.=: allocatedStorage,
        "OptionGroupName" Lude.=: optionGroupName,
        "CopyTagsToSnapshot" Lude.=: copyTagsToSnapshot,
        "S3BucketName" Lude.=: s3BucketName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "EnableIAMDatabaseAuthentication"
          Lude.=: enableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures" Lude.=: useDefaultProcessorFeatures,
        "StorageType" Lude.=: storageType,
        "EnableCloudwatchLogsExports"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> enableCloudwatchLogsExports),
        "DBName" Lude.=: dbName
      ]

-- | /See:/ 'mkRestoreDBInstanceFromS3Response' smart constructor.
data RestoreDBInstanceFromS3Response = RestoreDBInstanceFromS3Response'
  { dbInstance :: Lude.Maybe DBInstance,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBInstanceFromS3Response' with the minimum fields required to make a request.
--
-- * 'dbInstance' -
-- * 'responseStatus' - The response status code.
mkRestoreDBInstanceFromS3Response ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreDBInstanceFromS3Response
mkRestoreDBInstanceFromS3Response pResponseStatus_ =
  RestoreDBInstanceFromS3Response'
    { dbInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsrsDBInstance :: Lens.Lens' RestoreDBInstanceFromS3Response (Lude.Maybe DBInstance)
rdifsrsDBInstance = Lens.lens (dbInstance :: RestoreDBInstanceFromS3Response -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: RestoreDBInstanceFromS3Response)
{-# DEPRECATED rdifsrsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdifsrsResponseStatus :: Lens.Lens' RestoreDBInstanceFromS3Response Lude.Int
rdifsrsResponseStatus = Lens.lens (responseStatus :: RestoreDBInstanceFromS3Response -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreDBInstanceFromS3Response)
{-# DEPRECATED rdifsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
