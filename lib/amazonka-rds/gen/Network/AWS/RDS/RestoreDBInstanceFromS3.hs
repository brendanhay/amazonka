{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    restoreDBInstanceFromS3,
    RestoreDBInstanceFromS3,

    -- * Request Lenses
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
    rdifsMonitoringInterval,
    rdifsProcessorFeatures,
    rdifsLicenseModel,
    rdifsPreferredMaintenanceWindow,
    rdifsPerformanceInsightsRetentionPeriod,
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
    rdifsTags,
    rdifsPort,
    rdifsEnableIAMDatabaseAuthentication,
    rdifsUseDefaultProcessorFeatures,
    rdifsStorageType,
    rdifsEnableCloudwatchLogsExports,
    rdifsDBName,
    rdifsDBInstanceIdentifier,
    rdifsDBInstanceClass,
    rdifsEngine,
    rdifsSourceEngine,
    rdifsSourceEngineVersion,
    rdifsS3BucketName,
    rdifsS3IngestionRoleARN,

    -- * Destructuring the Response
    restoreDBInstanceFromS3Response,
    RestoreDBInstanceFromS3Response,

    -- * Response Lenses
    rdifsrsDBInstance,
    rdifsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'restoreDBInstanceFromS3' smart constructor.
data RestoreDBInstanceFromS3 = RestoreDBInstanceFromS3'
  { _rdifsEngineVersion ::
      !(Maybe Text),
    _rdifsDBSecurityGroups :: !(Maybe [Text]),
    _rdifsDeletionProtection :: !(Maybe Bool),
    _rdifsStorageEncrypted :: !(Maybe Bool),
    _rdifsMasterUserPassword :: !(Maybe Text),
    _rdifsPubliclyAccessible :: !(Maybe Bool),
    _rdifsAutoMinorVersionUpgrade ::
      !(Maybe Bool),
    _rdifsMasterUsername :: !(Maybe Text),
    _rdifsDBSubnetGroupName :: !(Maybe Text),
    _rdifsMonitoringRoleARN :: !(Maybe Text),
    _rdifsIOPS :: !(Maybe Int),
    _rdifsMonitoringInterval :: !(Maybe Int),
    _rdifsProcessorFeatures ::
      !(Maybe [ProcessorFeature]),
    _rdifsLicenseModel :: !(Maybe Text),
    _rdifsPreferredMaintenanceWindow ::
      !(Maybe Text),
    _rdifsPerformanceInsightsRetentionPeriod ::
      !(Maybe Int),
    _rdifsMaxAllocatedStorage :: !(Maybe Int),
    _rdifsEnablePerformanceInsights ::
      !(Maybe Bool),
    _rdifsKMSKeyId :: !(Maybe Text),
    _rdifsDBParameterGroupName :: !(Maybe Text),
    _rdifsPreferredBackupWindow ::
      !(Maybe Text),
    _rdifsAvailabilityZone :: !(Maybe Text),
    _rdifsBackupRetentionPeriod :: !(Maybe Int),
    _rdifsPerformanceInsightsKMSKeyId ::
      !(Maybe Text),
    _rdifsVPCSecurityGroupIds ::
      !(Maybe [Text]),
    _rdifsMultiAZ :: !(Maybe Bool),
    _rdifsS3Prefix :: !(Maybe Text),
    _rdifsAllocatedStorage :: !(Maybe Int),
    _rdifsOptionGroupName :: !(Maybe Text),
    _rdifsCopyTagsToSnapshot :: !(Maybe Bool),
    _rdifsTags :: !(Maybe [Tag]),
    _rdifsPort :: !(Maybe Int),
    _rdifsEnableIAMDatabaseAuthentication ::
      !(Maybe Bool),
    _rdifsUseDefaultProcessorFeatures ::
      !(Maybe Bool),
    _rdifsStorageType :: !(Maybe Text),
    _rdifsEnableCloudwatchLogsExports ::
      !(Maybe [Text]),
    _rdifsDBName :: !(Maybe Text),
    _rdifsDBInstanceIdentifier :: !Text,
    _rdifsDBInstanceClass :: !Text,
    _rdifsEngine :: !Text,
    _rdifsSourceEngine :: !Text,
    _rdifsSourceEngineVersion :: !Text,
    _rdifsS3BucketName :: !Text,
    _rdifsS3IngestionRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreDBInstanceFromS3' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdifsEngineVersion' - The version number of the database engine to use. Choose the latest minor version of your database engine. For information about engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
--
-- * 'rdifsDBSecurityGroups' - A list of DB security groups to associate with this DB instance. Default: The default DB security group for the database engine.
--
-- * 'rdifsDeletionProtection' - A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- * 'rdifsStorageEncrypted' - A value that indicates whether the new DB instance is encrypted or not.
--
-- * 'rdifsMasterUserPassword' - The password for the master user. The password can include any printable ASCII character except "/", """, or "@".  Constraints: Must contain from 8 to 41 characters.
--
-- * 'rdifsPubliclyAccessible' - A value that indicates whether the DB instance is publicly accessible. When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it. When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address. For more information, see 'CreateDBInstance' .
--
-- * 'rdifsAutoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are not applied automatically.
--
-- * 'rdifsMasterUsername' - The name for the master user.  Constraints:      * Must be 1 to 16 letters or numbers.     * First character must be a letter.     * Can't be a reserved word for the chosen database engine.
--
-- * 'rdifsDBSubnetGroupName' - A DB subnet group to associate with this DB instance.
--
-- * 'rdifsMonitoringRoleARN' - The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide./  If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- * 'rdifsIOPS' - The amount of Provisioned IOPS (input/output operations per second) to allocate initially for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./
--
-- * 'rdifsMonitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0.  If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.  Valid Values: 0, 1, 5, 10, 15, 30, 60  Default: @0@
--
-- * 'rdifsProcessorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- * 'rdifsLicenseModel' - The license model for this DB instance. Use @general-public-license@ .
--
-- * 'rdifsPreferredMaintenanceWindow' - The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> in the /Amazon RDS User Guide./  Constraints:     * Must be in the format @ddd:hh24:mi-ddd:hh24:mi@ .     * Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred backup window.     * Must be at least 30 minutes.
--
-- * 'rdifsPerformanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
--
-- * 'rdifsMaxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- * 'rdifsEnablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB instance.  For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
--
-- * 'rdifsKMSKeyId' - The AWS KMS key identifier for an encrypted DB instance.  The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key.  If the @StorageEncrypted@ parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'rdifsDBParameterGroupName' - The name of the DB parameter group to associate with this DB instance. If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used.
--
-- * 'rdifsPreferredBackupWindow' - The time range each day during which automated backups are created if automated backups are enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide./  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
--
-- * 'rdifsAvailabilityZone' - The Availability Zone that the DB instance is created in. For information about AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> in the /Amazon RDS User Guide./  Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.  Example: @us-east-1d@  Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint.
--
-- * 'rdifsBackupRetentionPeriod' - The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. For more information, see @CreateDBInstance@ .
--
-- * 'rdifsPerformanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key.  If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'rdifsVPCSecurityGroupIds' - A list of VPC security groups to associate with this DB instance.
--
-- * 'rdifsMultiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment. If the DB instance is a Multi-AZ deployment, you can't set the @AvailabilityZone@ parameter.
--
-- * 'rdifsS3Prefix' - The prefix of your Amazon S3 bucket.
--
-- * 'rdifsAllocatedStorage' - The amount of storage (in gigabytes) to allocate initially for the DB instance. Follow the allocation rules specified in @CreateDBInstance@ .
--
-- * 'rdifsOptionGroupName' - The name of the option group to associate with this DB instance. If this argument is omitted, the default option group for the specified engine is used.
--
-- * 'rdifsCopyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- * 'rdifsTags' - A list of tags to associate with this DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide./
--
-- * 'rdifsPort' - The port number on which the database accepts connections.  Type: Integer  Valid Values: @1150@ -@65535@  Default: @3306@
--
-- * 'rdifsEnableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled. For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- * 'rdifsUseDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- * 'rdifsStorageType' - Specifies the storage type to be associated with the DB instance.  Valid values: @standard@ | @gp2@ | @io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
--
-- * 'rdifsEnableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
--
-- * 'rdifsDBName' - The name of the database to create when the DB instance is created. Follow the naming rules specified in @CreateDBInstance@ .
--
-- * 'rdifsDBInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase string.  Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * First character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens. Example: @mydbinstance@
--
-- * 'rdifsDBInstanceClass' - The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./  Importing from Amazon S3 isn't supported on the db.t2.micro DB instance class.
--
-- * 'rdifsEngine' - The name of the database engine to be used for this instance.  Valid Values: @mysql@
--
-- * 'rdifsSourceEngine' - The name of the engine of your source database.  Valid Values: @mysql@
--
-- * 'rdifsSourceEngineVersion' - The version of the database that the backup files were created from. MySQL versions 5.6 and 5.7 are supported.  Example: @5.6.40@
--
-- * 'rdifsS3BucketName' - The name of your Amazon S3 bucket that contains your database backup file.
--
-- * 'rdifsS3IngestionRoleARN' - An AWS Identity and Access Management (IAM) role to allow Amazon RDS to access your Amazon S3 bucket.
restoreDBInstanceFromS3 ::
  -- | 'rdifsDBInstanceIdentifier'
  Text ->
  -- | 'rdifsDBInstanceClass'
  Text ->
  -- | 'rdifsEngine'
  Text ->
  -- | 'rdifsSourceEngine'
  Text ->
  -- | 'rdifsSourceEngineVersion'
  Text ->
  -- | 'rdifsS3BucketName'
  Text ->
  -- | 'rdifsS3IngestionRoleARN'
  Text ->
  RestoreDBInstanceFromS3
restoreDBInstanceFromS3
  pDBInstanceIdentifier_
  pDBInstanceClass_
  pEngine_
  pSourceEngine_
  pSourceEngineVersion_
  pS3BucketName_
  pS3IngestionRoleARN_ =
    RestoreDBInstanceFromS3'
      { _rdifsEngineVersion = Nothing,
        _rdifsDBSecurityGroups = Nothing,
        _rdifsDeletionProtection = Nothing,
        _rdifsStorageEncrypted = Nothing,
        _rdifsMasterUserPassword = Nothing,
        _rdifsPubliclyAccessible = Nothing,
        _rdifsAutoMinorVersionUpgrade = Nothing,
        _rdifsMasterUsername = Nothing,
        _rdifsDBSubnetGroupName = Nothing,
        _rdifsMonitoringRoleARN = Nothing,
        _rdifsIOPS = Nothing,
        _rdifsMonitoringInterval = Nothing,
        _rdifsProcessorFeatures = Nothing,
        _rdifsLicenseModel = Nothing,
        _rdifsPreferredMaintenanceWindow = Nothing,
        _rdifsPerformanceInsightsRetentionPeriod = Nothing,
        _rdifsMaxAllocatedStorage = Nothing,
        _rdifsEnablePerformanceInsights = Nothing,
        _rdifsKMSKeyId = Nothing,
        _rdifsDBParameterGroupName = Nothing,
        _rdifsPreferredBackupWindow = Nothing,
        _rdifsAvailabilityZone = Nothing,
        _rdifsBackupRetentionPeriod = Nothing,
        _rdifsPerformanceInsightsKMSKeyId = Nothing,
        _rdifsVPCSecurityGroupIds = Nothing,
        _rdifsMultiAZ = Nothing,
        _rdifsS3Prefix = Nothing,
        _rdifsAllocatedStorage = Nothing,
        _rdifsOptionGroupName = Nothing,
        _rdifsCopyTagsToSnapshot = Nothing,
        _rdifsTags = Nothing,
        _rdifsPort = Nothing,
        _rdifsEnableIAMDatabaseAuthentication = Nothing,
        _rdifsUseDefaultProcessorFeatures = Nothing,
        _rdifsStorageType = Nothing,
        _rdifsEnableCloudwatchLogsExports = Nothing,
        _rdifsDBName = Nothing,
        _rdifsDBInstanceIdentifier = pDBInstanceIdentifier_,
        _rdifsDBInstanceClass = pDBInstanceClass_,
        _rdifsEngine = pEngine_,
        _rdifsSourceEngine = pSourceEngine_,
        _rdifsSourceEngineVersion = pSourceEngineVersion_,
        _rdifsS3BucketName = pS3BucketName_,
        _rdifsS3IngestionRoleARN = pS3IngestionRoleARN_
      }

-- | The version number of the database engine to use. Choose the latest minor version of your database engine. For information about engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
rdifsEngineVersion :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsEngineVersion = lens _rdifsEngineVersion (\s a -> s {_rdifsEngineVersion = a})

-- | A list of DB security groups to associate with this DB instance. Default: The default DB security group for the database engine.
rdifsDBSecurityGroups :: Lens' RestoreDBInstanceFromS3 [Text]
rdifsDBSecurityGroups = lens _rdifsDBSecurityGroups (\s a -> s {_rdifsDBSecurityGroups = a}) . _Default . _Coerce

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
rdifsDeletionProtection :: Lens' RestoreDBInstanceFromS3 (Maybe Bool)
rdifsDeletionProtection = lens _rdifsDeletionProtection (\s a -> s {_rdifsDeletionProtection = a})

-- | A value that indicates whether the new DB instance is encrypted or not.
rdifsStorageEncrypted :: Lens' RestoreDBInstanceFromS3 (Maybe Bool)
rdifsStorageEncrypted = lens _rdifsStorageEncrypted (\s a -> s {_rdifsStorageEncrypted = a})

-- | The password for the master user. The password can include any printable ASCII character except "/", """, or "@".  Constraints: Must contain from 8 to 41 characters.
rdifsMasterUserPassword :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsMasterUserPassword = lens _rdifsMasterUserPassword (\s a -> s {_rdifsMasterUserPassword = a})

-- | A value that indicates whether the DB instance is publicly accessible. When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it. When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address. For more information, see 'CreateDBInstance' .
rdifsPubliclyAccessible :: Lens' RestoreDBInstanceFromS3 (Maybe Bool)
rdifsPubliclyAccessible = lens _rdifsPubliclyAccessible (\s a -> s {_rdifsPubliclyAccessible = a})

-- | A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are not applied automatically.
rdifsAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceFromS3 (Maybe Bool)
rdifsAutoMinorVersionUpgrade = lens _rdifsAutoMinorVersionUpgrade (\s a -> s {_rdifsAutoMinorVersionUpgrade = a})

-- | The name for the master user.  Constraints:      * Must be 1 to 16 letters or numbers.     * First character must be a letter.     * Can't be a reserved word for the chosen database engine.
rdifsMasterUsername :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsMasterUsername = lens _rdifsMasterUsername (\s a -> s {_rdifsMasterUsername = a})

-- | A DB subnet group to associate with this DB instance.
rdifsDBSubnetGroupName :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsDBSubnetGroupName = lens _rdifsDBSubnetGroupName (\s a -> s {_rdifsDBSubnetGroupName = a})

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide./  If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
rdifsMonitoringRoleARN :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsMonitoringRoleARN = lens _rdifsMonitoringRoleARN (\s a -> s {_rdifsMonitoringRoleARN = a})

-- | The amount of Provisioned IOPS (input/output operations per second) to allocate initially for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./
rdifsIOPS :: Lens' RestoreDBInstanceFromS3 (Maybe Int)
rdifsIOPS = lens _rdifsIOPS (\s a -> s {_rdifsIOPS = a})

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0.  If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.  Valid Values: 0, 1, 5, 10, 15, 30, 60  Default: @0@
rdifsMonitoringInterval :: Lens' RestoreDBInstanceFromS3 (Maybe Int)
rdifsMonitoringInterval = lens _rdifsMonitoringInterval (\s a -> s {_rdifsMonitoringInterval = a})

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
rdifsProcessorFeatures :: Lens' RestoreDBInstanceFromS3 [ProcessorFeature]
rdifsProcessorFeatures = lens _rdifsProcessorFeatures (\s a -> s {_rdifsProcessorFeatures = a}) . _Default . _Coerce

-- | The license model for this DB instance. Use @general-public-license@ .
rdifsLicenseModel :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsLicenseModel = lens _rdifsLicenseModel (\s a -> s {_rdifsLicenseModel = a})

-- | The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> in the /Amazon RDS User Guide./  Constraints:     * Must be in the format @ddd:hh24:mi-ddd:hh24:mi@ .     * Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred backup window.     * Must be at least 30 minutes.
rdifsPreferredMaintenanceWindow :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsPreferredMaintenanceWindow = lens _rdifsPreferredMaintenanceWindow (\s a -> s {_rdifsPreferredMaintenanceWindow = a})

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
rdifsPerformanceInsightsRetentionPeriod :: Lens' RestoreDBInstanceFromS3 (Maybe Int)
rdifsPerformanceInsightsRetentionPeriod = lens _rdifsPerformanceInsightsRetentionPeriod (\s a -> s {_rdifsPerformanceInsightsRetentionPeriod = a})

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
rdifsMaxAllocatedStorage :: Lens' RestoreDBInstanceFromS3 (Maybe Int)
rdifsMaxAllocatedStorage = lens _rdifsMaxAllocatedStorage (\s a -> s {_rdifsMaxAllocatedStorage = a})

-- | A value that indicates whether to enable Performance Insights for the DB instance.  For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
rdifsEnablePerformanceInsights :: Lens' RestoreDBInstanceFromS3 (Maybe Bool)
rdifsEnablePerformanceInsights = lens _rdifsEnablePerformanceInsights (\s a -> s {_rdifsEnablePerformanceInsights = a})

-- | The AWS KMS key identifier for an encrypted DB instance.  The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key.  If the @StorageEncrypted@ parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
rdifsKMSKeyId :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsKMSKeyId = lens _rdifsKMSKeyId (\s a -> s {_rdifsKMSKeyId = a})

-- | The name of the DB parameter group to associate with this DB instance. If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used.
rdifsDBParameterGroupName :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsDBParameterGroupName = lens _rdifsDBParameterGroupName (\s a -> s {_rdifsDBParameterGroupName = a})

-- | The time range each day during which automated backups are created if automated backups are enabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide./  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
rdifsPreferredBackupWindow :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsPreferredBackupWindow = lens _rdifsPreferredBackupWindow (\s a -> s {_rdifsPreferredBackupWindow = a})

-- | The Availability Zone that the DB instance is created in. For information about AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> in the /Amazon RDS User Guide./  Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.  Example: @us-east-1d@  Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint.
rdifsAvailabilityZone :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsAvailabilityZone = lens _rdifsAvailabilityZone (\s a -> s {_rdifsAvailabilityZone = a})

-- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. For more information, see @CreateDBInstance@ .
rdifsBackupRetentionPeriod :: Lens' RestoreDBInstanceFromS3 (Maybe Int)
rdifsBackupRetentionPeriod = lens _rdifsBackupRetentionPeriod (\s a -> s {_rdifsBackupRetentionPeriod = a})

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key.  If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
rdifsPerformanceInsightsKMSKeyId :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsPerformanceInsightsKMSKeyId = lens _rdifsPerformanceInsightsKMSKeyId (\s a -> s {_rdifsPerformanceInsightsKMSKeyId = a})

-- | A list of VPC security groups to associate with this DB instance.
rdifsVPCSecurityGroupIds :: Lens' RestoreDBInstanceFromS3 [Text]
rdifsVPCSecurityGroupIds = lens _rdifsVPCSecurityGroupIds (\s a -> s {_rdifsVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | A value that indicates whether the DB instance is a Multi-AZ deployment. If the DB instance is a Multi-AZ deployment, you can't set the @AvailabilityZone@ parameter.
rdifsMultiAZ :: Lens' RestoreDBInstanceFromS3 (Maybe Bool)
rdifsMultiAZ = lens _rdifsMultiAZ (\s a -> s {_rdifsMultiAZ = a})

-- | The prefix of your Amazon S3 bucket.
rdifsS3Prefix :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsS3Prefix = lens _rdifsS3Prefix (\s a -> s {_rdifsS3Prefix = a})

-- | The amount of storage (in gigabytes) to allocate initially for the DB instance. Follow the allocation rules specified in @CreateDBInstance@ .
rdifsAllocatedStorage :: Lens' RestoreDBInstanceFromS3 (Maybe Int)
rdifsAllocatedStorage = lens _rdifsAllocatedStorage (\s a -> s {_rdifsAllocatedStorage = a})

-- | The name of the option group to associate with this DB instance. If this argument is omitted, the default option group for the specified engine is used.
rdifsOptionGroupName :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsOptionGroupName = lens _rdifsOptionGroupName (\s a -> s {_rdifsOptionGroupName = a})

-- | A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
rdifsCopyTagsToSnapshot :: Lens' RestoreDBInstanceFromS3 (Maybe Bool)
rdifsCopyTagsToSnapshot = lens _rdifsCopyTagsToSnapshot (\s a -> s {_rdifsCopyTagsToSnapshot = a})

-- | A list of tags to associate with this DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide./
rdifsTags :: Lens' RestoreDBInstanceFromS3 [Tag]
rdifsTags = lens _rdifsTags (\s a -> s {_rdifsTags = a}) . _Default . _Coerce

-- | The port number on which the database accepts connections.  Type: Integer  Valid Values: @1150@ -@65535@  Default: @3306@
rdifsPort :: Lens' RestoreDBInstanceFromS3 (Maybe Int)
rdifsPort = lens _rdifsPort (\s a -> s {_rdifsPort = a})

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled. For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
rdifsEnableIAMDatabaseAuthentication :: Lens' RestoreDBInstanceFromS3 (Maybe Bool)
rdifsEnableIAMDatabaseAuthentication = lens _rdifsEnableIAMDatabaseAuthentication (\s a -> s {_rdifsEnableIAMDatabaseAuthentication = a})

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
rdifsUseDefaultProcessorFeatures :: Lens' RestoreDBInstanceFromS3 (Maybe Bool)
rdifsUseDefaultProcessorFeatures = lens _rdifsUseDefaultProcessorFeatures (\s a -> s {_rdifsUseDefaultProcessorFeatures = a})

-- | Specifies the storage type to be associated with the DB instance.  Valid values: @standard@ | @gp2@ | @io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified; otherwise @gp2@
rdifsStorageType :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsStorageType = lens _rdifsStorageType (\s a -> s {_rdifsStorageType = a})

-- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
rdifsEnableCloudwatchLogsExports :: Lens' RestoreDBInstanceFromS3 [Text]
rdifsEnableCloudwatchLogsExports = lens _rdifsEnableCloudwatchLogsExports (\s a -> s {_rdifsEnableCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The name of the database to create when the DB instance is created. Follow the naming rules specified in @CreateDBInstance@ .
rdifsDBName :: Lens' RestoreDBInstanceFromS3 (Maybe Text)
rdifsDBName = lens _rdifsDBName (\s a -> s {_rdifsDBName = a})

-- | The DB instance identifier. This parameter is stored as a lowercase string.  Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * First character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens. Example: @mydbinstance@
rdifsDBInstanceIdentifier :: Lens' RestoreDBInstanceFromS3 Text
rdifsDBInstanceIdentifier = lens _rdifsDBInstanceIdentifier (\s a -> s {_rdifsDBInstanceIdentifier = a})

-- | The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./  Importing from Amazon S3 isn't supported on the db.t2.micro DB instance class.
rdifsDBInstanceClass :: Lens' RestoreDBInstanceFromS3 Text
rdifsDBInstanceClass = lens _rdifsDBInstanceClass (\s a -> s {_rdifsDBInstanceClass = a})

-- | The name of the database engine to be used for this instance.  Valid Values: @mysql@
rdifsEngine :: Lens' RestoreDBInstanceFromS3 Text
rdifsEngine = lens _rdifsEngine (\s a -> s {_rdifsEngine = a})

-- | The name of the engine of your source database.  Valid Values: @mysql@
rdifsSourceEngine :: Lens' RestoreDBInstanceFromS3 Text
rdifsSourceEngine = lens _rdifsSourceEngine (\s a -> s {_rdifsSourceEngine = a})

-- | The version of the database that the backup files were created from. MySQL versions 5.6 and 5.7 are supported.  Example: @5.6.40@
rdifsSourceEngineVersion :: Lens' RestoreDBInstanceFromS3 Text
rdifsSourceEngineVersion = lens _rdifsSourceEngineVersion (\s a -> s {_rdifsSourceEngineVersion = a})

-- | The name of your Amazon S3 bucket that contains your database backup file.
rdifsS3BucketName :: Lens' RestoreDBInstanceFromS3 Text
rdifsS3BucketName = lens _rdifsS3BucketName (\s a -> s {_rdifsS3BucketName = a})

-- | An AWS Identity and Access Management (IAM) role to allow Amazon RDS to access your Amazon S3 bucket.
rdifsS3IngestionRoleARN :: Lens' RestoreDBInstanceFromS3 Text
rdifsS3IngestionRoleARN = lens _rdifsS3IngestionRoleARN (\s a -> s {_rdifsS3IngestionRoleARN = a})

instance AWSRequest RestoreDBInstanceFromS3 where
  type Rs RestoreDBInstanceFromS3 = RestoreDBInstanceFromS3Response
  request = postQuery rds
  response =
    receiveXMLWrapper
      "RestoreDBInstanceFromS3Result"
      ( \s h x ->
          RestoreDBInstanceFromS3Response'
            <$> (x .@? "DBInstance") <*> (pure (fromEnum s))
      )

instance Hashable RestoreDBInstanceFromS3

instance NFData RestoreDBInstanceFromS3

instance ToHeaders RestoreDBInstanceFromS3 where
  toHeaders = const mempty

instance ToPath RestoreDBInstanceFromS3 where
  toPath = const "/"

instance ToQuery RestoreDBInstanceFromS3 where
  toQuery RestoreDBInstanceFromS3' {..} =
    mconcat
      [ "Action" =: ("RestoreDBInstanceFromS3" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "EngineVersion" =: _rdifsEngineVersion,
        "DBSecurityGroups"
          =: toQuery
            (toQueryList "DBSecurityGroupName" <$> _rdifsDBSecurityGroups),
        "DeletionProtection" =: _rdifsDeletionProtection,
        "StorageEncrypted" =: _rdifsStorageEncrypted,
        "MasterUserPassword" =: _rdifsMasterUserPassword,
        "PubliclyAccessible" =: _rdifsPubliclyAccessible,
        "AutoMinorVersionUpgrade" =: _rdifsAutoMinorVersionUpgrade,
        "MasterUsername" =: _rdifsMasterUsername,
        "DBSubnetGroupName" =: _rdifsDBSubnetGroupName,
        "MonitoringRoleArn" =: _rdifsMonitoringRoleARN,
        "Iops" =: _rdifsIOPS,
        "MonitoringInterval" =: _rdifsMonitoringInterval,
        "ProcessorFeatures"
          =: toQuery
            (toQueryList "ProcessorFeature" <$> _rdifsProcessorFeatures),
        "LicenseModel" =: _rdifsLicenseModel,
        "PreferredMaintenanceWindow" =: _rdifsPreferredMaintenanceWindow,
        "PerformanceInsightsRetentionPeriod"
          =: _rdifsPerformanceInsightsRetentionPeriod,
        "MaxAllocatedStorage" =: _rdifsMaxAllocatedStorage,
        "EnablePerformanceInsights" =: _rdifsEnablePerformanceInsights,
        "KmsKeyId" =: _rdifsKMSKeyId,
        "DBParameterGroupName" =: _rdifsDBParameterGroupName,
        "PreferredBackupWindow" =: _rdifsPreferredBackupWindow,
        "AvailabilityZone" =: _rdifsAvailabilityZone,
        "BackupRetentionPeriod" =: _rdifsBackupRetentionPeriod,
        "PerformanceInsightsKMSKeyId" =: _rdifsPerformanceInsightsKMSKeyId,
        "VpcSecurityGroupIds"
          =: toQuery
            (toQueryList "VpcSecurityGroupId" <$> _rdifsVPCSecurityGroupIds),
        "MultiAZ" =: _rdifsMultiAZ,
        "S3Prefix" =: _rdifsS3Prefix,
        "AllocatedStorage" =: _rdifsAllocatedStorage,
        "OptionGroupName" =: _rdifsOptionGroupName,
        "CopyTagsToSnapshot" =: _rdifsCopyTagsToSnapshot,
        "Tags" =: toQuery (toQueryList "Tag" <$> _rdifsTags),
        "Port" =: _rdifsPort,
        "EnableIAMDatabaseAuthentication"
          =: _rdifsEnableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures" =: _rdifsUseDefaultProcessorFeatures,
        "StorageType" =: _rdifsStorageType,
        "EnableCloudwatchLogsExports"
          =: toQuery
            (toQueryList "member" <$> _rdifsEnableCloudwatchLogsExports),
        "DBName" =: _rdifsDBName,
        "DBInstanceIdentifier" =: _rdifsDBInstanceIdentifier,
        "DBInstanceClass" =: _rdifsDBInstanceClass,
        "Engine" =: _rdifsEngine,
        "SourceEngine" =: _rdifsSourceEngine,
        "SourceEngineVersion" =: _rdifsSourceEngineVersion,
        "S3BucketName" =: _rdifsS3BucketName,
        "S3IngestionRoleArn" =: _rdifsS3IngestionRoleARN
      ]

-- | /See:/ 'restoreDBInstanceFromS3Response' smart constructor.
data RestoreDBInstanceFromS3Response = RestoreDBInstanceFromS3Response'
  { _rdifsrsDBInstance ::
      !(Maybe DBInstance),
    _rdifsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreDBInstanceFromS3Response' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdifsrsDBInstance' - Undocumented member.
--
-- * 'rdifsrsResponseStatus' - -- | The response status code.
restoreDBInstanceFromS3Response ::
  -- | 'rdifsrsResponseStatus'
  Int ->
  RestoreDBInstanceFromS3Response
restoreDBInstanceFromS3Response pResponseStatus_ =
  RestoreDBInstanceFromS3Response'
    { _rdifsrsDBInstance = Nothing,
      _rdifsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
rdifsrsDBInstance :: Lens' RestoreDBInstanceFromS3Response (Maybe DBInstance)
rdifsrsDBInstance = lens _rdifsrsDBInstance (\s a -> s {_rdifsrsDBInstance = a})

-- | -- | The response status code.
rdifsrsResponseStatus :: Lens' RestoreDBInstanceFromS3Response Int
rdifsrsResponseStatus = lens _rdifsrsResponseStatus (\s a -> s {_rdifsrsResponseStatus = a})

instance NFData RestoreDBInstanceFromS3Response
