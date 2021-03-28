{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RestoreDBInstanceFromS3 (..)
    , mkRestoreDBInstanceFromS3
    -- ** Request lenses
    , rdbifsDBInstanceIdentifier
    , rdbifsDBInstanceClass
    , rdbifsEngine
    , rdbifsSourceEngine
    , rdbifsSourceEngineVersion
    , rdbifsS3BucketName
    , rdbifsS3IngestionRoleArn
    , rdbifsAllocatedStorage
    , rdbifsAutoMinorVersionUpgrade
    , rdbifsAvailabilityZone
    , rdbifsBackupRetentionPeriod
    , rdbifsCopyTagsToSnapshot
    , rdbifsDBName
    , rdbifsDBParameterGroupName
    , rdbifsDBSecurityGroups
    , rdbifsDBSubnetGroupName
    , rdbifsDeletionProtection
    , rdbifsEnableCloudwatchLogsExports
    , rdbifsEnableIAMDatabaseAuthentication
    , rdbifsEnablePerformanceInsights
    , rdbifsEngineVersion
    , rdbifsIops
    , rdbifsKmsKeyId
    , rdbifsLicenseModel
    , rdbifsMasterUserPassword
    , rdbifsMasterUsername
    , rdbifsMaxAllocatedStorage
    , rdbifsMonitoringInterval
    , rdbifsMonitoringRoleArn
    , rdbifsMultiAZ
    , rdbifsOptionGroupName
    , rdbifsPerformanceInsightsKMSKeyId
    , rdbifsPerformanceInsightsRetentionPeriod
    , rdbifsPort
    , rdbifsPreferredBackupWindow
    , rdbifsPreferredMaintenanceWindow
    , rdbifsProcessorFeatures
    , rdbifsPubliclyAccessible
    , rdbifsS3Prefix
    , rdbifsStorageEncrypted
    , rdbifsStorageType
    , rdbifsTags
    , rdbifsUseDefaultProcessorFeatures
    , rdbifsVpcSecurityGroupIds

    -- * Destructuring the response
    , RestoreDBInstanceFromS3Response (..)
    , mkRestoreDBInstanceFromS3Response
    -- ** Response lenses
    , rdbifsrrsDBInstance
    , rdbifsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreDBInstanceFromS3' smart constructor.
data RestoreDBInstanceFromS3 = RestoreDBInstanceFromS3'
  { dBInstanceIdentifier :: Core.Text
    -- ^ The DB instance identifier. This parameter is stored as a lowercase string. 
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
  , dBInstanceClass :: Core.Text
    -- ^ The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./ 
--
-- Importing from Amazon S3 isn't supported on the db.t2.micro DB instance class. 
  , engine :: Core.Text
    -- ^ The name of the database engine to be used for this instance. 
--
-- Valid Values: @mysql@ 
  , sourceEngine :: Core.Text
    -- ^ The name of the engine of your source database. 
--
-- Valid Values: @mysql@ 
  , sourceEngineVersion :: Core.Text
    -- ^ The version of the database that the backup files were created from.
--
-- MySQL versions 5.6 and 5.7 are supported. 
-- Example: @5.6.40@ 
  , s3BucketName :: Core.Text
    -- ^ The name of your Amazon S3 bucket that contains your database backup file. 
  , s3IngestionRoleArn :: Core.Text
    -- ^ An AWS Identity and Access Management (IAM) role to allow Amazon RDS to access your Amazon S3 bucket. 
  , allocatedStorage :: Core.Maybe Core.Int
    -- ^ The amount of storage (in gigabytes) to allocate initially for the DB instance. Follow the allocation rules specified in @CreateDBInstance@ . 
  , autoMinorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are not applied automatically. 
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone that the DB instance is created in. For information about AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> in the /Amazon RDS User Guide./ 
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region. 
-- Example: @us-east-1d@ 
-- Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint. 
  , backupRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. For more information, see @CreateDBInstance@ . 
  , copyTagsToSnapshot :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied. 
  , dBName :: Core.Maybe Core.Text
    -- ^ The name of the database to create when the DB instance is created. Follow the naming rules specified in @CreateDBInstance@ . 
  , dBParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used.
  , dBSecurityGroups :: Core.Maybe [Core.Text]
    -- ^ A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
  , dBSubnetGroupName :: Core.Maybe Core.Text
    -- ^ A DB subnet group to associate with this DB instance.
  , deletionProtection :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> . 
  , enableCloudwatchLogsExports :: Core.Maybe [Core.Text]
    -- ^ The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
  , enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./ 
  , enablePerformanceInsights :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to enable Performance Insights for the DB instance. 
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ . 
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The version number of the database engine to use. Choose the latest minor version of your database engine. For information about engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ . 
  , iops :: Core.Maybe Core.Int
    -- ^ The amount of Provisioned IOPS (input/output operations per second) to allocate initially for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./ 
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS KMS key identifier for an encrypted DB instance. 
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key. 
-- If the @StorageEncrypted@ parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region. 
  , licenseModel :: Core.Maybe Core.Text
    -- ^ The license model for this DB instance. Use @general-public-license@ . 
  , masterUserPassword :: Core.Maybe Core.Text
    -- ^ The password for the master user. The password can include any printable ASCII character except "/", """, or "@". 
--
-- Constraints: Must contain from 8 to 41 characters.
  , masterUsername :: Core.Maybe Core.Text
    -- ^ The name for the master user. 
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
  , maxAllocatedStorage :: Core.Maybe Core.Int
    -- ^ The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
  , monitoringInterval :: Core.Maybe Core.Int
    -- ^ The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. 
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. 
-- Valid Values: 0, 1, 5, 10, 15, 30, 60 
-- Default: @0@ 
  , monitoringRoleArn :: Core.Maybe Core.Text
    -- ^ The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide./ 
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value. 
  , multiAZ :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance is a Multi-AZ deployment. If the DB instance is a Multi-AZ deployment, you can't set the @AvailabilityZone@ parameter. 
  , optionGroupName :: Core.Maybe Core.Text
    -- ^ The name of the option group to associate with this DB instance. If this argument is omitted, the default option group for the specified engine is used. 
  , performanceInsightsKMSKeyId :: Core.Maybe Core.Text
    -- ^ The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. 
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
  , performanceInsightsRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years). 
  , port :: Core.Maybe Core.Int
    -- ^ The port number on which the database accepts connections. 
--
-- Type: Integer 
-- Valid Values: @1150@ -@65535@ 
-- Default: @3306@ 
  , preferredBackupWindow :: Core.Maybe Core.Text
    -- ^ The time range each day during which automated backups are created if automated backups are enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide./ 
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
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> in the /Amazon RDS User Guide./ 
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
  , processorFeatures :: Core.Maybe [Types.ProcessorFeature]
    -- ^ The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
  , s3Prefix :: Core.Maybe Core.Text
    -- ^ The prefix of your Amazon S3 bucket. 
  , storageEncrypted :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the new DB instance is encrypted or not. 
  , storageType :: Core.Maybe Core.Text
    -- ^ Specifies the storage type to be associated with the DB instance. 
--
-- Valid values: @standard@ | @gp2@ | @io1@ 
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter. 
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@ 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags to associate with this DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide./ 
  , useDefaultProcessorFeatures :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance class of the DB instance uses its default processor features.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of VPC security groups to associate with this DB instance. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreDBInstanceFromS3' value with any optional fields omitted.
mkRestoreDBInstanceFromS3
    :: Core.Text -- ^ 'dBInstanceIdentifier'
    -> Core.Text -- ^ 'dBInstanceClass'
    -> Core.Text -- ^ 'engine'
    -> Core.Text -- ^ 'sourceEngine'
    -> Core.Text -- ^ 'sourceEngineVersion'
    -> Core.Text -- ^ 's3BucketName'
    -> Core.Text -- ^ 's3IngestionRoleArn'
    -> RestoreDBInstanceFromS3
mkRestoreDBInstanceFromS3 dBInstanceIdentifier dBInstanceClass
  engine sourceEngine sourceEngineVersion s3BucketName
  s3IngestionRoleArn
  = RestoreDBInstanceFromS3'{dBInstanceIdentifier, dBInstanceClass,
                             engine, sourceEngine, sourceEngineVersion, s3BucketName,
                             s3IngestionRoleArn, allocatedStorage = Core.Nothing,
                             autoMinorVersionUpgrade = Core.Nothing,
                             availabilityZone = Core.Nothing,
                             backupRetentionPeriod = Core.Nothing,
                             copyTagsToSnapshot = Core.Nothing, dBName = Core.Nothing,
                             dBParameterGroupName = Core.Nothing,
                             dBSecurityGroups = Core.Nothing, dBSubnetGroupName = Core.Nothing,
                             deletionProtection = Core.Nothing,
                             enableCloudwatchLogsExports = Core.Nothing,
                             enableIAMDatabaseAuthentication = Core.Nothing,
                             enablePerformanceInsights = Core.Nothing,
                             engineVersion = Core.Nothing, iops = Core.Nothing,
                             kmsKeyId = Core.Nothing, licenseModel = Core.Nothing,
                             masterUserPassword = Core.Nothing, masterUsername = Core.Nothing,
                             maxAllocatedStorage = Core.Nothing,
                             monitoringInterval = Core.Nothing,
                             monitoringRoleArn = Core.Nothing, multiAZ = Core.Nothing,
                             optionGroupName = Core.Nothing,
                             performanceInsightsKMSKeyId = Core.Nothing,
                             performanceInsightsRetentionPeriod = Core.Nothing,
                             port = Core.Nothing, preferredBackupWindow = Core.Nothing,
                             preferredMaintenanceWindow = Core.Nothing,
                             processorFeatures = Core.Nothing,
                             publiclyAccessible = Core.Nothing, s3Prefix = Core.Nothing,
                             storageEncrypted = Core.Nothing, storageType = Core.Nothing,
                             tags = Core.Nothing, useDefaultProcessorFeatures = Core.Nothing,
                             vpcSecurityGroupIds = Core.Nothing}

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
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsDBInstanceIdentifier :: Lens.Lens' RestoreDBInstanceFromS3 Core.Text
rdbifsDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE rdbifsDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./ 
--
-- Importing from Amazon S3 isn't supported on the db.t2.micro DB instance class. 
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsDBInstanceClass :: Lens.Lens' RestoreDBInstanceFromS3 Core.Text
rdbifsDBInstanceClass = Lens.field @"dBInstanceClass"
{-# INLINEABLE rdbifsDBInstanceClass #-}
{-# DEPRECATED dBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead"  #-}

-- | The name of the database engine to be used for this instance. 
--
-- Valid Values: @mysql@ 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsEngine :: Lens.Lens' RestoreDBInstanceFromS3 Core.Text
rdbifsEngine = Lens.field @"engine"
{-# INLINEABLE rdbifsEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The name of the engine of your source database. 
--
-- Valid Values: @mysql@ 
--
-- /Note:/ Consider using 'sourceEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsSourceEngine :: Lens.Lens' RestoreDBInstanceFromS3 Core.Text
rdbifsSourceEngine = Lens.field @"sourceEngine"
{-# INLINEABLE rdbifsSourceEngine #-}
{-# DEPRECATED sourceEngine "Use generic-lens or generic-optics with 'sourceEngine' instead"  #-}

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.6 and 5.7 are supported. 
-- Example: @5.6.40@ 
--
-- /Note:/ Consider using 'sourceEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsSourceEngineVersion :: Lens.Lens' RestoreDBInstanceFromS3 Core.Text
rdbifsSourceEngineVersion = Lens.field @"sourceEngineVersion"
{-# INLINEABLE rdbifsSourceEngineVersion #-}
{-# DEPRECATED sourceEngineVersion "Use generic-lens or generic-optics with 'sourceEngineVersion' instead"  #-}

-- | The name of your Amazon S3 bucket that contains your database backup file. 
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsS3BucketName :: Lens.Lens' RestoreDBInstanceFromS3 Core.Text
rdbifsS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE rdbifsS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | An AWS Identity and Access Management (IAM) role to allow Amazon RDS to access your Amazon S3 bucket. 
--
-- /Note:/ Consider using 's3IngestionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsS3IngestionRoleArn :: Lens.Lens' RestoreDBInstanceFromS3 Core.Text
rdbifsS3IngestionRoleArn = Lens.field @"s3IngestionRoleArn"
{-# INLINEABLE rdbifsS3IngestionRoleArn #-}
{-# DEPRECATED s3IngestionRoleArn "Use generic-lens or generic-optics with 's3IngestionRoleArn' instead"  #-}

-- | The amount of storage (in gigabytes) to allocate initially for the DB instance. Follow the allocation rules specified in @CreateDBInstance@ . 
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsAllocatedStorage :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Int)
rdbifsAllocatedStorage = Lens.field @"allocatedStorage"
{-# INLINEABLE rdbifsAllocatedStorage #-}
{-# DEPRECATED allocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead"  #-}

-- | A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are not applied automatically. 
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsAutoMinorVersionUpgrade :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Bool)
rdbifsAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# INLINEABLE rdbifsAutoMinorVersionUpgrade #-}
{-# DEPRECATED autoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead"  #-}

-- | The Availability Zone that the DB instance is created in. For information about AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> in the /Amazon RDS User Guide./ 
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region. 
-- Example: @us-east-1d@ 
-- Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint. 
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsAvailabilityZone :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE rdbifsAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. For more information, see @CreateDBInstance@ . 
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsBackupRetentionPeriod :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Int)
rdbifsBackupRetentionPeriod = Lens.field @"backupRetentionPeriod"
{-# INLINEABLE rdbifsBackupRetentionPeriod #-}
{-# DEPRECATED backupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead"  #-}

-- | A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied. 
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsCopyTagsToSnapshot :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Bool)
rdbifsCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# INLINEABLE rdbifsCopyTagsToSnapshot #-}
{-# DEPRECATED copyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead"  #-}

-- | The name of the database to create when the DB instance is created. Follow the naming rules specified in @CreateDBInstance@ . 
--
-- /Note:/ Consider using 'dBName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsDBName :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsDBName = Lens.field @"dBName"
{-# INLINEABLE rdbifsDBName #-}
{-# DEPRECATED dBName "Use generic-lens or generic-optics with 'dBName' instead"  #-}

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used.
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsDBParameterGroupName :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE rdbifsDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
--
-- /Note:/ Consider using 'dBSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsDBSecurityGroups :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe [Core.Text])
rdbifsDBSecurityGroups = Lens.field @"dBSecurityGroups"
{-# INLINEABLE rdbifsDBSecurityGroups #-}
{-# DEPRECATED dBSecurityGroups "Use generic-lens or generic-optics with 'dBSecurityGroups' instead"  #-}

-- | A DB subnet group to associate with this DB instance.
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsDBSubnetGroupName :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# INLINEABLE rdbifsDBSubnetGroupName #-}
{-# DEPRECATED dBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead"  #-}

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> . 
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsDeletionProtection :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Bool)
rdbifsDeletionProtection = Lens.field @"deletionProtection"
{-# INLINEABLE rdbifsDeletionProtection #-}
{-# DEPRECATED deletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead"  #-}

-- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe [Core.Text])
rdbifsEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# INLINEABLE rdbifsEnableCloudwatchLogsExports #-}
{-# DEPRECATED enableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead"  #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Bool)
rdbifsEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# INLINEABLE rdbifsEnableIAMDatabaseAuthentication #-}
{-# DEPRECATED enableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead"  #-}

-- | A value that indicates whether to enable Performance Insights for the DB instance. 
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ . 
--
-- /Note:/ Consider using 'enablePerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsEnablePerformanceInsights :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Bool)
rdbifsEnablePerformanceInsights = Lens.field @"enablePerformanceInsights"
{-# INLINEABLE rdbifsEnablePerformanceInsights #-}
{-# DEPRECATED enablePerformanceInsights "Use generic-lens or generic-optics with 'enablePerformanceInsights' instead"  #-}

-- | The version number of the database engine to use. Choose the latest minor version of your database engine. For information about engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ . 
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsEngineVersion :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE rdbifsEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to allocate initially for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsIops :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Int)
rdbifsIops = Lens.field @"iops"
{-# INLINEABLE rdbifsIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The AWS KMS key identifier for an encrypted DB instance. 
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key. 
-- If the @StorageEncrypted@ parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region. 
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsKmsKeyId :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE rdbifsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The license model for this DB instance. Use @general-public-license@ . 
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsLicenseModel :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsLicenseModel = Lens.field @"licenseModel"
{-# INLINEABLE rdbifsLicenseModel #-}
{-# DEPRECATED licenseModel "Use generic-lens or generic-optics with 'licenseModel' instead"  #-}

-- | The password for the master user. The password can include any printable ASCII character except "/", """, or "@". 
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsMasterUserPassword :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsMasterUserPassword = Lens.field @"masterUserPassword"
{-# INLINEABLE rdbifsMasterUserPassword #-}
{-# DEPRECATED masterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead"  #-}

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
rdbifsMasterUsername :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsMasterUsername = Lens.field @"masterUsername"
{-# INLINEABLE rdbifsMasterUsername #-}
{-# DEPRECATED masterUsername "Use generic-lens or generic-optics with 'masterUsername' instead"  #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsMaxAllocatedStorage :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Int)
rdbifsMaxAllocatedStorage = Lens.field @"maxAllocatedStorage"
{-# INLINEABLE rdbifsMaxAllocatedStorage #-}
{-# DEPRECATED maxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead"  #-}

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. 
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. 
-- Valid Values: 0, 1, 5, 10, 15, 30, 60 
-- Default: @0@ 
--
-- /Note:/ Consider using 'monitoringInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsMonitoringInterval :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Int)
rdbifsMonitoringInterval = Lens.field @"monitoringInterval"
{-# INLINEABLE rdbifsMonitoringInterval #-}
{-# DEPRECATED monitoringInterval "Use generic-lens or generic-optics with 'monitoringInterval' instead"  #-}

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide./ 
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value. 
--
-- /Note:/ Consider using 'monitoringRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsMonitoringRoleArn :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsMonitoringRoleArn = Lens.field @"monitoringRoleArn"
{-# INLINEABLE rdbifsMonitoringRoleArn #-}
{-# DEPRECATED monitoringRoleArn "Use generic-lens or generic-optics with 'monitoringRoleArn' instead"  #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment. If the DB instance is a Multi-AZ deployment, you can't set the @AvailabilityZone@ parameter. 
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsMultiAZ :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Bool)
rdbifsMultiAZ = Lens.field @"multiAZ"
{-# INLINEABLE rdbifsMultiAZ #-}
{-# DEPRECATED multiAZ "Use generic-lens or generic-optics with 'multiAZ' instead"  #-}

-- | The name of the option group to associate with this DB instance. If this argument is omitted, the default option group for the specified engine is used. 
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsOptionGroupName :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE rdbifsOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. 
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'performanceInsightsKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsPerformanceInsightsKMSKeyId :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsPerformanceInsightsKMSKeyId = Lens.field @"performanceInsightsKMSKeyId"
{-# INLINEABLE rdbifsPerformanceInsightsKMSKeyId #-}
{-# DEPRECATED performanceInsightsKMSKeyId "Use generic-lens or generic-optics with 'performanceInsightsKMSKeyId' instead"  #-}

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years). 
--
-- /Note:/ Consider using 'performanceInsightsRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsPerformanceInsightsRetentionPeriod :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Int)
rdbifsPerformanceInsightsRetentionPeriod = Lens.field @"performanceInsightsRetentionPeriod"
{-# INLINEABLE rdbifsPerformanceInsightsRetentionPeriod #-}
{-# DEPRECATED performanceInsightsRetentionPeriod "Use generic-lens or generic-optics with 'performanceInsightsRetentionPeriod' instead"  #-}

-- | The port number on which the database accepts connections. 
--
-- Type: Integer 
-- Valid Values: @1150@ -@65535@ 
-- Default: @3306@ 
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsPort :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Int)
rdbifsPort = Lens.field @"port"
{-# INLINEABLE rdbifsPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

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
rdbifsPreferredBackupWindow :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# INLINEABLE rdbifsPreferredBackupWindow #-}
{-# DEPRECATED preferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead"  #-}

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
rdbifsPreferredMaintenanceWindow :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE rdbifsPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe [Types.ProcessorFeature])
rdbifsProcessorFeatures = Lens.field @"processorFeatures"
{-# INLINEABLE rdbifsProcessorFeatures #-}
{-# DEPRECATED processorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead"  #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsPubliclyAccessible :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Bool)
rdbifsPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE rdbifsPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

-- | The prefix of your Amazon S3 bucket. 
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsS3Prefix :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsS3Prefix = Lens.field @"s3Prefix"
{-# INLINEABLE rdbifsS3Prefix #-}
{-# DEPRECATED s3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead"  #-}

-- | A value that indicates whether the new DB instance is encrypted or not. 
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsStorageEncrypted :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Bool)
rdbifsStorageEncrypted = Lens.field @"storageEncrypted"
{-# INLINEABLE rdbifsStorageEncrypted #-}
{-# DEPRECATED storageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead"  #-}

-- | Specifies the storage type to be associated with the DB instance. 
--
-- Valid values: @standard@ | @gp2@ | @io1@ 
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter. 
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@ 
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsStorageType :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Text)
rdbifsStorageType = Lens.field @"storageType"
{-# INLINEABLE rdbifsStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

-- | A list of tags to associate with this DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsTags :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe [Types.Tag])
rdbifsTags = Lens.field @"tags"
{-# INLINEABLE rdbifsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsUseDefaultProcessorFeatures :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe Core.Bool)
rdbifsUseDefaultProcessorFeatures = Lens.field @"useDefaultProcessorFeatures"
{-# INLINEABLE rdbifsUseDefaultProcessorFeatures #-}
{-# DEPRECATED useDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead"  #-}

-- | A list of VPC security groups to associate with this DB instance. 
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsVpcSecurityGroupIds :: Lens.Lens' RestoreDBInstanceFromS3 (Core.Maybe [Core.Text])
rdbifsVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE rdbifsVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery RestoreDBInstanceFromS3 where
        toQuery RestoreDBInstanceFromS3{..}
          = Core.toQueryPair "Action"
              ("RestoreDBInstanceFromS3" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier
              Core.<> Core.toQueryPair "DBInstanceClass" dBInstanceClass
              Core.<> Core.toQueryPair "Engine" engine
              Core.<> Core.toQueryPair "SourceEngine" sourceEngine
              Core.<> Core.toQueryPair "SourceEngineVersion" sourceEngineVersion
              Core.<> Core.toQueryPair "S3BucketName" s3BucketName
              Core.<> Core.toQueryPair "S3IngestionRoleArn" s3IngestionRoleArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AllocatedStorage")
                allocatedStorage
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoMinorVersionUpgrade")
                autoMinorVersionUpgrade
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZone")
                availabilityZone
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "BackupRetentionPeriod")
                backupRetentionPeriod
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CopyTagsToSnapshot")
                copyTagsToSnapshot
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DBName") dBName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBParameterGroupName")
                dBParameterGroupName
              Core.<>
              Core.toQueryPair "DBSecurityGroups"
                (Core.maybe Core.mempty (Core.toQueryList "DBSecurityGroupName")
                   dBSecurityGroups)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBSubnetGroupName")
                dBSubnetGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeletionProtection")
                deletionProtection
              Core.<>
              Core.toQueryPair "EnableCloudwatchLogsExports"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   enableCloudwatchLogsExports)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "EnableIAMDatabaseAuthentication")
                enableIAMDatabaseAuthentication
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "EnablePerformanceInsights")
                enablePerformanceInsights
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EngineVersion")
                engineVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Iops") iops
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LicenseModel")
                licenseModel
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MasterUserPassword")
                masterUserPassword
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MasterUsername")
                masterUsername
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxAllocatedStorage")
                maxAllocatedStorage
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MonitoringInterval")
                monitoringInterval
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MonitoringRoleArn")
                monitoringRoleArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "MultiAZ") multiAZ
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OptionGroupName")
                optionGroupName
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PerformanceInsightsKMSKeyId")
                performanceInsightsKMSKeyId
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PerformanceInsightsRetentionPeriod")
                performanceInsightsRetentionPeriod
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PreferredBackupWindow")
                preferredBackupWindow
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PreferredMaintenanceWindow")
                preferredMaintenanceWindow
              Core.<>
              Core.toQueryPair "ProcessorFeatures"
                (Core.maybe Core.mempty (Core.toQueryList "ProcessorFeature")
                   processorFeatures)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PubliclyAccessible")
                publiclyAccessible
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "S3Prefix") s3Prefix
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StorageEncrypted")
                storageEncrypted
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StorageType") storageType
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "UseDefaultProcessorFeatures")
                useDefaultProcessorFeatures
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupIds)

instance Core.ToHeaders RestoreDBInstanceFromS3 where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RestoreDBInstanceFromS3 where
        type Rs RestoreDBInstanceFromS3 = RestoreDBInstanceFromS3Response
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "RestoreDBInstanceFromS3Result"
              (\ s h x ->
                 RestoreDBInstanceFromS3Response' Core.<$>
                   (x Core..@? "DBInstance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreDBInstanceFromS3Response' smart constructor.
data RestoreDBInstanceFromS3Response = RestoreDBInstanceFromS3Response'
  { dBInstance :: Core.Maybe Types.DBInstance
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreDBInstanceFromS3Response' value with any optional fields omitted.
mkRestoreDBInstanceFromS3Response
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreDBInstanceFromS3Response
mkRestoreDBInstanceFromS3Response responseStatus
  = RestoreDBInstanceFromS3Response'{dBInstance = Core.Nothing,
                                     responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsrrsDBInstance :: Lens.Lens' RestoreDBInstanceFromS3Response (Core.Maybe Types.DBInstance)
rdbifsrrsDBInstance = Lens.field @"dBInstance"
{-# INLINEABLE rdbifsrrsDBInstance #-}
{-# DEPRECATED dBInstance "Use generic-lens or generic-optics with 'dBInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbifsrrsResponseStatus :: Lens.Lens' RestoreDBInstanceFromS3Response Core.Int
rdbifsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdbifsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
