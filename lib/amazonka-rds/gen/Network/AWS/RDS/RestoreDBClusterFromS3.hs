{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RestoreDBClusterFromS3
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Aurora DB cluster from MySQL data stored in an Amazon S3 bucket. Amazon RDS must be authorized to access the Amazon S3 bucket and the data must be created using the Percona XtraBackup utility as described in <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Migrating.ExtMySQL.html#AuroraMySQL.Migrating.ExtMySQL.S3 Migrating Data from MySQL by Using an Amazon S3 Bucket> in the /Amazon Aurora User Guide/ .
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.RestoreDBClusterFromS3
  ( -- * Creating a request
    RestoreDBClusterFromS3 (..),
    mkRestoreDBClusterFromS3,

    -- ** Request lenses
    rdbcfsEngineVersion,
    rdbcfsDeletionProtection,
    rdbcfsStorageEncrypted,
    rdbcfsDBClusterIdentifier,
    rdbcfsMasterUserPassword,
    rdbcfsMasterUsername,
    rdbcfsDBSubnetGroupName,
    rdbcfsDomain,
    rdbcfsBacktrackWindow,
    rdbcfsS3IngestionRoleARN,
    rdbcfsSourceEngine,
    rdbcfsEngine,
    rdbcfsSourceEngineVersion,
    rdbcfsPreferredMaintenanceWindow,
    rdbcfsAvailabilityZones,
    rdbcfsCharacterSetName,
    rdbcfsKMSKeyId,
    rdbcfsPreferredBackupWindow,
    rdbcfsBackupRetentionPeriod,
    rdbcfsVPCSecurityGroupIds,
    rdbcfsDatabaseName,
    rdbcfsDBClusterParameterGroupName,
    rdbcfsS3Prefix,
    rdbcfsOptionGroupName,
    rdbcfsCopyTagsToSnapshot,
    rdbcfsDomainIAMRoleName,
    rdbcfsS3BucketName,
    rdbcfsTags,
    rdbcfsPort,
    rdbcfsEnableIAMDatabaseAuthentication,
    rdbcfsEnableCloudwatchLogsExports,

    -- * Destructuring the response
    RestoreDBClusterFromS3Response (..),
    mkRestoreDBClusterFromS3Response,

    -- ** Response lenses
    rdcfsrsDBCluster,
    rdcfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRestoreDBClusterFromS3' smart constructor.
data RestoreDBClusterFromS3 = RestoreDBClusterFromS3'
  { -- | The version number of the database engine to use.
    --
    -- To list all of the available engine versions for @aurora@ (for MySQL 5.6-compatible Aurora), use the following command:
    -- @aws rds describe-db-engine-versions --engine aurora --query "DBEngineVersions[].EngineVersion"@
    -- To list all of the available engine versions for @aurora-mysql@ (for MySQL 5.7-compatible Aurora), use the following command:
    -- @aws rds describe-db-engine-versions --engine aurora-mysql --query "DBEngineVersions[].EngineVersion"@
    -- To list all of the available engine versions for @aurora-postgresql@ , use the following command:
    -- @aws rds describe-db-engine-versions --engine aurora-postgresql --query "DBEngineVersions[].EngineVersion"@
    -- __Aurora MySQL__
    -- Example: @5.6.10a@ , @5.6.mysql_aurora.1.19.2@ , @5.7.12@ , @5.7.mysql_aurora.2.04.5@
    -- __Aurora PostgreSQL__
    -- Example: @9.6.3@ , @10.7@
    engineVersion :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
    deletionProtection :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether the restored DB cluster is encrypted.
    storageEncrypted :: Lude.Maybe Lude.Bool,
    -- | The name of the DB cluster to create from the source data in the Amazon S3 bucket. This parameter isn't case-sensitive.
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
    -- Example: @my-cluster1@
    dbClusterIdentifier :: Lude.Text,
    -- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Lude.Text,
    -- | The name of the master user for the restored DB cluster.
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
    masterUsername :: Lude.Text,
    -- | A DB subnet group to associate with the restored DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Lude.Maybe Lude.Text,
    -- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation.
    --
    -- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
    domain :: Lude.Maybe Lude.Text,
    -- | The target backtrack window, in seconds. To disable backtracking, set this value to 0.
    --
    -- Default: 0
    -- Constraints:
    --
    --     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
    backtrackWindow :: Lude.Maybe Lude.Integer,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon RDS to access the Amazon S3 bucket on your behalf.
    s3IngestionRoleARN :: Lude.Text,
    -- | The identifier for the database engine that was backed up to create the files stored in the Amazon S3 bucket.
    --
    -- Valid values: @mysql@
    sourceEngine :: Lude.Text,
    -- | The name of the database engine to be used for this DB cluster.
    --
    -- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
    engine :: Lude.Text,
    -- | The version of the database that the backup files were created from.
    --
    -- MySQL versions 5.5, 5.6, and 5.7 are supported.
    -- Example: @5.6.40@ , @5.7.28@
    sourceEngineVersion :: Lude.Text,
    -- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window> in the /Amazon Aurora User Guide./
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    -- | A list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
    availabilityZones :: Lude.Maybe [Lude.Text],
    -- | A value that indicates that the restored DB cluster should be associated with the specified CharacterSet.
    characterSetName :: Lude.Maybe Lude.Text,
    -- | The AWS KMS key identifier for an encrypted DB cluster.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KM encryption key.
    -- If the StorageEncrypted parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window> in the /Amazon Aurora User Guide./
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
    -- | The number of days for which automated backups of the restored DB cluster are retained. You must specify a minimum value of 1.
    --
    -- Default: 1
    -- Constraints:
    --
    --     * Must be a value from 1 to 35
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | A list of EC2 VPC security groups to associate with the restored DB cluster.
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    -- | The database name for the restored DB cluster.
    databaseName :: Lude.Maybe Lude.Text,
    -- | The name of the DB cluster parameter group to associate with the restored DB cluster. If this argument is omitted, @default.aurora5.6@ is used.
    --
    -- Constraints:
    --
    --     * If supplied, must match the name of an existing DBClusterParameterGroup.
    dbClusterParameterGroupName :: Lude.Maybe Lude.Text,
    -- | The prefix for all of the file names that contain the data used to create the Amazon Aurora DB cluster. If you do not specify a __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created by using all of the files in the Amazon S3 bucket.
    s3Prefix :: Lude.Maybe Lude.Text,
    -- | A value that indicates that the restored DB cluster should be associated with the specified option group.
    --
    -- Permanent options can't be removed from an option group. An option group can't be removed from a DB cluster once it is associated with a DB cluster.
    optionGroupName :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    -- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
    domainIAMRoleName :: Lude.Maybe Lude.Text,
    -- | The name of the Amazon S3 bucket that contains the data used to create the Amazon Aurora DB cluster.
    s3BucketName :: Lude.Text,
    tags :: Lude.Maybe [Tag],
    -- | The port number on which the instances in the restored DB cluster accept connections.
    --
    -- Default: @3306@
    port :: Lude.Maybe Lude.Int,
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Lude.Maybe Lude.Bool,
    -- | The list of logs that the restored DB cluster is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
    enableCloudwatchLogsExports :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBClusterFromS3' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The version number of the database engine to use.
--
-- To list all of the available engine versions for @aurora@ (for MySQL 5.6-compatible Aurora), use the following command:
-- @aws rds describe-db-engine-versions --engine aurora --query "DBEngineVersions[].EngineVersion"@
-- To list all of the available engine versions for @aurora-mysql@ (for MySQL 5.7-compatible Aurora), use the following command:
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query "DBEngineVersions[].EngineVersion"@
-- To list all of the available engine versions for @aurora-postgresql@ , use the following command:
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query "DBEngineVersions[].EngineVersion"@
-- __Aurora MySQL__
-- Example: @5.6.10a@ , @5.6.mysql_aurora.1.19.2@ , @5.7.12@ , @5.7.mysql_aurora.2.04.5@
-- __Aurora PostgreSQL__
-- Example: @9.6.3@ , @10.7@
-- * 'deletionProtection' - A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
-- * 'storageEncrypted' - A value that indicates whether the restored DB cluster is encrypted.
-- * 'dbClusterIdentifier' - The name of the DB cluster to create from the source data in the Amazon S3 bucket. This parameter isn't case-sensitive.
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
-- Example: @my-cluster1@
-- * 'masterUserPassword' - The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
-- * 'masterUsername' - The name of the master user for the restored DB cluster.
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
-- * 'dbSubnetGroupName' - A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@
-- * 'domain' - Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
-- * 'backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set this value to 0.
--
-- Default: 0
-- Constraints:
--
--     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
--
-- * 's3IngestionRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon RDS to access the Amazon S3 bucket on your behalf.
-- * 'sourceEngine' - The identifier for the database engine that was backed up to create the files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
-- * 'engine' - The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
-- * 'sourceEngineVersion' - The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
-- Example: @5.6.40@ , @5.7.28@
-- * 'preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window> in the /Amazon Aurora User Guide./
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
-- * 'availabilityZones' - A list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
-- * 'characterSetName' - A value that indicates that the restored DB cluster should be associated with the specified CharacterSet.
-- * 'kmsKeyId' - The AWS KMS key identifier for an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KM encryption key.
-- If the StorageEncrypted parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'preferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window> in the /Amazon Aurora User Guide./
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
-- * 'backupRetentionPeriod' - The number of days for which automated backups of the restored DB cluster are retained. You must specify a minimum value of 1.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 1 to 35
--
--
-- * 'vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the restored DB cluster.
-- * 'databaseName' - The database name for the restored DB cluster.
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with the restored DB cluster. If this argument is omitted, @default.aurora5.6@ is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
-- * 's3Prefix' - The prefix for all of the file names that contain the data used to create the Amazon Aurora DB cluster. If you do not specify a __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created by using all of the files in the Amazon S3 bucket.
-- * 'optionGroupName' - A value that indicates that the restored DB cluster should be associated with the specified option group.
--
-- Permanent options can't be removed from an option group. An option group can't be removed from a DB cluster once it is associated with a DB cluster.
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
-- * 'domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
-- * 's3BucketName' - The name of the Amazon S3 bucket that contains the data used to create the Amazon Aurora DB cluster.
-- * 'tags' -
-- * 'port' - The port number on which the instances in the restored DB cluster accept connections.
--
-- Default: @3306@
-- * 'enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
-- * 'enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
mkRestoreDBClusterFromS3 ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'masterUserPassword'
  Lude.Text ->
  -- | 'masterUsername'
  Lude.Text ->
  -- | 's3IngestionRoleARN'
  Lude.Text ->
  -- | 'sourceEngine'
  Lude.Text ->
  -- | 'engine'
  Lude.Text ->
  -- | 'sourceEngineVersion'
  Lude.Text ->
  -- | 's3BucketName'
  Lude.Text ->
  RestoreDBClusterFromS3
mkRestoreDBClusterFromS3
  pDBClusterIdentifier_
  pMasterUserPassword_
  pMasterUsername_
  pS3IngestionRoleARN_
  pSourceEngine_
  pEngine_
  pSourceEngineVersion_
  pS3BucketName_ =
    RestoreDBClusterFromS3'
      { engineVersion = Lude.Nothing,
        deletionProtection = Lude.Nothing,
        storageEncrypted = Lude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        masterUserPassword = pMasterUserPassword_,
        masterUsername = pMasterUsername_,
        dbSubnetGroupName = Lude.Nothing,
        domain = Lude.Nothing,
        backtrackWindow = Lude.Nothing,
        s3IngestionRoleARN = pS3IngestionRoleARN_,
        sourceEngine = pSourceEngine_,
        engine = pEngine_,
        sourceEngineVersion = pSourceEngineVersion_,
        preferredMaintenanceWindow = Lude.Nothing,
        availabilityZones = Lude.Nothing,
        characterSetName = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        preferredBackupWindow = Lude.Nothing,
        backupRetentionPeriod = Lude.Nothing,
        vpcSecurityGroupIds = Lude.Nothing,
        databaseName = Lude.Nothing,
        dbClusterParameterGroupName = Lude.Nothing,
        s3Prefix = Lude.Nothing,
        optionGroupName = Lude.Nothing,
        copyTagsToSnapshot = Lude.Nothing,
        domainIAMRoleName = Lude.Nothing,
        s3BucketName = pS3BucketName_,
        tags = Lude.Nothing,
        port = Lude.Nothing,
        enableIAMDatabaseAuthentication = Lude.Nothing,
        enableCloudwatchLogsExports = Lude.Nothing
      }

-- | The version number of the database engine to use.
--
-- To list all of the available engine versions for @aurora@ (for MySQL 5.6-compatible Aurora), use the following command:
-- @aws rds describe-db-engine-versions --engine aurora --query "DBEngineVersions[].EngineVersion"@
-- To list all of the available engine versions for @aurora-mysql@ (for MySQL 5.7-compatible Aurora), use the following command:
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query "DBEngineVersions[].EngineVersion"@
-- To list all of the available engine versions for @aurora-postgresql@ , use the following command:
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query "DBEngineVersions[].EngineVersion"@
-- __Aurora MySQL__
-- Example: @5.6.10a@ , @5.6.mysql_aurora.1.19.2@ , @5.7.12@ , @5.7.mysql_aurora.2.04.5@
-- __Aurora PostgreSQL__
-- Example: @9.6.3@ , @10.7@
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsEngineVersion :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsEngineVersion = Lens.lens (engineVersion :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDeletionProtection :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Bool)
rdbcfsDeletionProtection = Lens.lens (deletionProtection :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | A value that indicates whether the restored DB cluster is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsStorageEncrypted :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Bool)
rdbcfsStorageEncrypted = Lens.lens (storageEncrypted :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {storageEncrypted = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | The name of the DB cluster to create from the source data in the Amazon S3 bucket. This parameter isn't case-sensitive.
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
-- Example: @my-cluster1@
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDBClusterIdentifier :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdbcfsDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsMasterUserPassword :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdbcfsMasterUserPassword = Lens.lens (masterUserPassword :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {masterUserPassword = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The name of the master user for the restored DB cluster.
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
rdbcfsMasterUsername :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdbcfsMasterUsername = Lens.lens (masterUsername :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {masterUsername = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDBSubnetGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDomain :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsDomain = Lens.lens (domain :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The target backtrack window, in seconds. To disable backtracking, set this value to 0.
--
-- Default: 0
-- Constraints:
--
--     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
--
--
-- /Note:/ Consider using 'backtrackWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsBacktrackWindow :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Integer)
rdbcfsBacktrackWindow = Lens.lens (backtrackWindow :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Integer) (\s a -> s {backtrackWindow = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon RDS to access the Amazon S3 bucket on your behalf.
--
-- /Note:/ Consider using 's3IngestionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsS3IngestionRoleARN :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdbcfsS3IngestionRoleARN = Lens.lens (s3IngestionRoleARN :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {s3IngestionRoleARN = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsS3IngestionRoleARN "Use generic-lens or generic-optics with 's3IngestionRoleARN' instead." #-}

-- | The identifier for the database engine that was backed up to create the files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
--
-- /Note:/ Consider using 'sourceEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsSourceEngine :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdbcfsSourceEngine = Lens.lens (sourceEngine :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {sourceEngine = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsSourceEngine "Use generic-lens or generic-optics with 'sourceEngine' instead." #-}

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsEngine :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdbcfsEngine = Lens.lens (engine :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {engine = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
-- Example: @5.6.40@ , @5.7.28@
--
-- /Note:/ Consider using 'sourceEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsSourceEngineVersion :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdbcfsSourceEngineVersion = Lens.lens (sourceEngineVersion :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {sourceEngineVersion = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsSourceEngineVersion "Use generic-lens or generic-optics with 'sourceEngineVersion' instead." #-}

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window> in the /Amazon Aurora User Guide./
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsPreferredMaintenanceWindow :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | A list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsAvailabilityZones :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe [Lude.Text])
rdbcfsAvailabilityZones = Lens.lens (availabilityZones :: RestoreDBClusterFromS3 -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A value that indicates that the restored DB cluster should be associated with the specified CharacterSet.
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsCharacterSetName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsCharacterSetName = Lens.lens (characterSetName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {characterSetName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

-- | The AWS KMS key identifier for an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KM encryption key.
-- If the StorageEncrypted parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsKMSKeyId :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsKMSKeyId = Lens.lens (kmsKeyId :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window> in the /Amazon Aurora User Guide./
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
rdbcfsPreferredBackupWindow :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsPreferredBackupWindow = Lens.lens (preferredBackupWindow :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The number of days for which automated backups of the restored DB cluster are retained. You must specify a minimum value of 1.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 1 to 35
--
--
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsBackupRetentionPeriod :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Int)
rdbcfsBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | A list of EC2 VPC security groups to associate with the restored DB cluster.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsVPCSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe [Lude.Text])
rdbcfsVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: RestoreDBClusterFromS3 -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | The database name for the restored DB cluster.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDatabaseName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsDatabaseName = Lens.lens (databaseName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the DB cluster parameter group to associate with the restored DB cluster. If this argument is omitted, @default.aurora5.6@ is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDBClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

-- | The prefix for all of the file names that contain the data used to create the Amazon Aurora DB cluster. If you do not specify a __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created by using all of the files in the Amazon S3 bucket.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsS3Prefix :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsS3Prefix = Lens.lens (s3Prefix :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | A value that indicates that the restored DB cluster should be associated with the specified option group.
--
-- Permanent options can't be removed from an option group. An option group can't be removed from a DB cluster once it is associated with a DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsOptionGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsOptionGroupName = Lens.lens (optionGroupName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsCopyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Bool)
rdbcfsCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDomainIAMRoleName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdbcfsDomainIAMRoleName = Lens.lens (domainIAMRoleName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | The name of the Amazon S3 bucket that contains the data used to create the Amazon Aurora DB cluster.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsS3BucketName :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdbcfsS3BucketName = Lens.lens (s3BucketName :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {s3BucketName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsTags :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe [Tag])
rdbcfsTags = Lens.lens (tags :: RestoreDBClusterFromS3 -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which the instances in the restored DB cluster accept connections.
--
-- Default: @3306@
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsPort :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Int)
rdbcfsPort = Lens.lens (port :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Bool)
rdbcfsEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | The list of logs that the restored DB cluster is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe [Lude.Text])
rdbcfsEnableCloudwatchLogsExports = Lens.lens (enableCloudwatchLogsExports :: RestoreDBClusterFromS3 -> Lude.Maybe [Lude.Text]) (\s a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdbcfsEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

instance Lude.AWSRequest RestoreDBClusterFromS3 where
  type Rs RestoreDBClusterFromS3 = RestoreDBClusterFromS3Response
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RestoreDBClusterFromS3Result"
      ( \s h x ->
          RestoreDBClusterFromS3Response'
            Lude.<$> (x Lude..@? "DBCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreDBClusterFromS3 where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestoreDBClusterFromS3 where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreDBClusterFromS3 where
  toQuery RestoreDBClusterFromS3' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RestoreDBClusterFromS3" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "DeletionProtection" Lude.=: deletionProtection,
        "StorageEncrypted" Lude.=: storageEncrypted,
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "MasterUserPassword" Lude.=: masterUserPassword,
        "MasterUsername" Lude.=: masterUsername,
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "Domain" Lude.=: domain,
        "BacktrackWindow" Lude.=: backtrackWindow,
        "S3IngestionRoleArn" Lude.=: s3IngestionRoleARN,
        "SourceEngine" Lude.=: sourceEngine,
        "Engine" Lude.=: engine,
        "SourceEngineVersion" Lude.=: sourceEngineVersion,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "AvailabilityZones"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "AvailabilityZone" Lude.<$> availabilityZones),
        "CharacterSetName" Lude.=: characterSetName,
        "KmsKeyId" Lude.=: kmsKeyId,
        "PreferredBackupWindow" Lude.=: preferredBackupWindow,
        "BackupRetentionPeriod" Lude.=: backupRetentionPeriod,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "DatabaseName" Lude.=: databaseName,
        "DBClusterParameterGroupName" Lude.=: dbClusterParameterGroupName,
        "S3Prefix" Lude.=: s3Prefix,
        "OptionGroupName" Lude.=: optionGroupName,
        "CopyTagsToSnapshot" Lude.=: copyTagsToSnapshot,
        "DomainIAMRoleName" Lude.=: domainIAMRoleName,
        "S3BucketName" Lude.=: s3BucketName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "EnableIAMDatabaseAuthentication"
          Lude.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> enableCloudwatchLogsExports)
      ]

-- | /See:/ 'mkRestoreDBClusterFromS3Response' smart constructor.
data RestoreDBClusterFromS3Response = RestoreDBClusterFromS3Response'
  { dbCluster :: Lude.Maybe DBCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBClusterFromS3Response' with the minimum fields required to make a request.
--
-- * 'dbCluster' -
-- * 'responseStatus' - The response status code.
mkRestoreDBClusterFromS3Response ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreDBClusterFromS3Response
mkRestoreDBClusterFromS3Response pResponseStatus_ =
  RestoreDBClusterFromS3Response'
    { dbCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsrsDBCluster :: Lens.Lens' RestoreDBClusterFromS3Response (Lude.Maybe DBCluster)
rdcfsrsDBCluster = Lens.lens (dbCluster :: RestoreDBClusterFromS3Response -> Lude.Maybe DBCluster) (\s a -> s {dbCluster = a} :: RestoreDBClusterFromS3Response)
{-# DEPRECATED rdcfsrsDBCluster "Use generic-lens or generic-optics with 'dbCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsrsResponseStatus :: Lens.Lens' RestoreDBClusterFromS3Response Lude.Int
rdcfsrsResponseStatus = Lens.lens (responseStatus :: RestoreDBClusterFromS3Response -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreDBClusterFromS3Response)
{-# DEPRECATED rdcfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
