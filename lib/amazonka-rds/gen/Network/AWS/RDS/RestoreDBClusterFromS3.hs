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
    rdbcfsDBClusterIdentifier,
    rdbcfsEngine,
    rdbcfsMasterUsername,
    rdbcfsMasterUserPassword,
    rdbcfsSourceEngine,
    rdbcfsSourceEngineVersion,
    rdbcfsS3BucketName,
    rdbcfsS3IngestionRoleArn,
    rdbcfsAvailabilityZones,
    rdbcfsBacktrackWindow,
    rdbcfsBackupRetentionPeriod,
    rdbcfsCharacterSetName,
    rdbcfsCopyTagsToSnapshot,
    rdbcfsDBClusterParameterGroupName,
    rdbcfsDBSubnetGroupName,
    rdbcfsDatabaseName,
    rdbcfsDeletionProtection,
    rdbcfsDomain,
    rdbcfsDomainIAMRoleName,
    rdbcfsEnableCloudwatchLogsExports,
    rdbcfsEnableIAMDatabaseAuthentication,
    rdbcfsEngineVersion,
    rdbcfsKmsKeyId,
    rdbcfsOptionGroupName,
    rdbcfsPort,
    rdbcfsPreferredBackupWindow,
    rdbcfsPreferredMaintenanceWindow,
    rdbcfsS3Prefix,
    rdbcfsStorageEncrypted,
    rdbcfsTags,
    rdbcfsVpcSecurityGroupIds,

    -- * Destructuring the response
    RestoreDBClusterFromS3Response (..),
    mkRestoreDBClusterFromS3Response,

    -- ** Response lenses
    rdbcfsrrsDBCluster,
    rdbcfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreDBClusterFromS3' smart constructor.
data RestoreDBClusterFromS3 = RestoreDBClusterFromS3'
  { -- | The name of the DB cluster to create from the source data in the Amazon S3 bucket. This parameter isn't case-sensitive.
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
    dBClusterIdentifier :: Types.String,
    -- | The name of the database engine to be used for this DB cluster.
    --
    -- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
    engine :: Types.String,
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
    masterUsername :: Types.String,
    -- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Types.String,
    -- | The identifier for the database engine that was backed up to create the files stored in the Amazon S3 bucket.
    --
    -- Valid values: @mysql@
    sourceEngine :: Types.String,
    -- | The version of the database that the backup files were created from.
    --
    -- MySQL versions 5.5, 5.6, and 5.7 are supported.
    -- Example: @5.6.40@ , @5.7.28@
    sourceEngineVersion :: Types.String,
    -- | The name of the Amazon S3 bucket that contains the data used to create the Amazon Aurora DB cluster.
    s3BucketName :: Types.String,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon RDS to access the Amazon S3 bucket on your behalf.
    s3IngestionRoleArn :: Types.String,
    -- | A list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
    availabilityZones :: Core.Maybe [Types.String],
    -- | The target backtrack window, in seconds. To disable backtracking, set this value to 0.
    --
    -- Default: 0
    -- Constraints:
    --
    --     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
    backtrackWindow :: Core.Maybe Core.Integer,
    -- | The number of days for which automated backups of the restored DB cluster are retained. You must specify a minimum value of 1.
    --
    -- Default: 1
    -- Constraints:
    --
    --     * Must be a value from 1 to 35
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | A value that indicates that the restored DB cluster should be associated with the specified CharacterSet.
    characterSetName :: Core.Maybe Types.String,
    -- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
    -- | The name of the DB cluster parameter group to associate with the restored DB cluster. If this argument is omitted, @default.aurora5.6@ is used.
    --
    -- Constraints:
    --
    --     * If supplied, must match the name of an existing DBClusterParameterGroup.
    dBClusterParameterGroupName :: Core.Maybe Types.String,
    -- | A DB subnet group to associate with the restored DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
    -- Example: @mySubnetgroup@
    dBSubnetGroupName :: Core.Maybe Types.String,
    -- | The database name for the restored DB cluster.
    databaseName :: Core.Maybe Types.String,
    -- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation.
    --
    -- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
    domain :: Core.Maybe Types.String,
    -- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
    domainIAMRoleName :: Core.Maybe Types.String,
    -- | The list of logs that the restored DB cluster is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
    enableCloudwatchLogsExports :: Core.Maybe [Types.String],
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
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
    engineVersion :: Core.Maybe Types.String,
    -- | The AWS KMS key identifier for an encrypted DB cluster.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KM encryption key.
    -- If the StorageEncrypted parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Core.Maybe Types.String,
    -- | A value that indicates that the restored DB cluster should be associated with the specified option group.
    --
    -- Permanent options can't be removed from an option group. An option group can't be removed from a DB cluster once it is associated with a DB cluster.
    optionGroupName :: Core.Maybe Types.String,
    -- | The port number on which the instances in the restored DB cluster accept connections.
    --
    -- Default: @3306@
    port :: Core.Maybe Core.Int,
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
    preferredBackupWindow :: Core.Maybe Types.String,
    -- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window> in the /Amazon Aurora User Guide./
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Core.Maybe Types.String,
    -- | The prefix for all of the file names that contain the data used to create the Amazon Aurora DB cluster. If you do not specify a __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created by using all of the files in the Amazon S3 bucket.
    s3Prefix :: Core.Maybe Types.String,
    -- | A value that indicates whether the restored DB cluster is encrypted.
    storageEncrypted :: Core.Maybe Core.Bool,
    tags :: Core.Maybe [Types.Tag],
    -- | A list of EC2 VPC security groups to associate with the restored DB cluster.
    vpcSecurityGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreDBClusterFromS3' value with any optional fields omitted.
mkRestoreDBClusterFromS3 ::
  -- | 'dBClusterIdentifier'
  Types.String ->
  -- | 'engine'
  Types.String ->
  -- | 'masterUsername'
  Types.String ->
  -- | 'masterUserPassword'
  Types.String ->
  -- | 'sourceEngine'
  Types.String ->
  -- | 'sourceEngineVersion'
  Types.String ->
  -- | 's3BucketName'
  Types.String ->
  -- | 's3IngestionRoleArn'
  Types.String ->
  RestoreDBClusterFromS3
mkRestoreDBClusterFromS3
  dBClusterIdentifier
  engine
  masterUsername
  masterUserPassword
  sourceEngine
  sourceEngineVersion
  s3BucketName
  s3IngestionRoleArn =
    RestoreDBClusterFromS3'
      { dBClusterIdentifier,
        engine,
        masterUsername,
        masterUserPassword,
        sourceEngine,
        sourceEngineVersion,
        s3BucketName,
        s3IngestionRoleArn,
        availabilityZones = Core.Nothing,
        backtrackWindow = Core.Nothing,
        backupRetentionPeriod = Core.Nothing,
        characterSetName = Core.Nothing,
        copyTagsToSnapshot = Core.Nothing,
        dBClusterParameterGroupName = Core.Nothing,
        dBSubnetGroupName = Core.Nothing,
        databaseName = Core.Nothing,
        deletionProtection = Core.Nothing,
        domain = Core.Nothing,
        domainIAMRoleName = Core.Nothing,
        enableCloudwatchLogsExports = Core.Nothing,
        enableIAMDatabaseAuthentication = Core.Nothing,
        engineVersion = Core.Nothing,
        kmsKeyId = Core.Nothing,
        optionGroupName = Core.Nothing,
        port = Core.Nothing,
        preferredBackupWindow = Core.Nothing,
        preferredMaintenanceWindow = Core.Nothing,
        s3Prefix = Core.Nothing,
        storageEncrypted = Core.Nothing,
        tags = Core.Nothing,
        vpcSecurityGroupIds = Core.Nothing
      }

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
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDBClusterIdentifier :: Lens.Lens' RestoreDBClusterFromS3 Types.String
rdbcfsDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED rdbcfsDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsEngine :: Lens.Lens' RestoreDBClusterFromS3 Types.String
rdbcfsEngine = Lens.field @"engine"
{-# DEPRECATED rdbcfsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

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
rdbcfsMasterUsername :: Lens.Lens' RestoreDBClusterFromS3 Types.String
rdbcfsMasterUsername = Lens.field @"masterUsername"
{-# DEPRECATED rdbcfsMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsMasterUserPassword :: Lens.Lens' RestoreDBClusterFromS3 Types.String
rdbcfsMasterUserPassword = Lens.field @"masterUserPassword"
{-# DEPRECATED rdbcfsMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The identifier for the database engine that was backed up to create the files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
--
-- /Note:/ Consider using 'sourceEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsSourceEngine :: Lens.Lens' RestoreDBClusterFromS3 Types.String
rdbcfsSourceEngine = Lens.field @"sourceEngine"
{-# DEPRECATED rdbcfsSourceEngine "Use generic-lens or generic-optics with 'sourceEngine' instead." #-}

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
-- Example: @5.6.40@ , @5.7.28@
--
-- /Note:/ Consider using 'sourceEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsSourceEngineVersion :: Lens.Lens' RestoreDBClusterFromS3 Types.String
rdbcfsSourceEngineVersion = Lens.field @"sourceEngineVersion"
{-# DEPRECATED rdbcfsSourceEngineVersion "Use generic-lens or generic-optics with 'sourceEngineVersion' instead." #-}

-- | The name of the Amazon S3 bucket that contains the data used to create the Amazon Aurora DB cluster.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsS3BucketName :: Lens.Lens' RestoreDBClusterFromS3 Types.String
rdbcfsS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED rdbcfsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon RDS to access the Amazon S3 bucket on your behalf.
--
-- /Note:/ Consider using 's3IngestionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsS3IngestionRoleArn :: Lens.Lens' RestoreDBClusterFromS3 Types.String
rdbcfsS3IngestionRoleArn = Lens.field @"s3IngestionRoleArn"
{-# DEPRECATED rdbcfsS3IngestionRoleArn "Use generic-lens or generic-optics with 's3IngestionRoleArn' instead." #-}

-- | A list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsAvailabilityZones :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe [Types.String])
rdbcfsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED rdbcfsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

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
rdbcfsBacktrackWindow :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Core.Integer)
rdbcfsBacktrackWindow = Lens.field @"backtrackWindow"
{-# DEPRECATED rdbcfsBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

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
rdbcfsBackupRetentionPeriod :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Core.Int)
rdbcfsBackupRetentionPeriod = Lens.field @"backupRetentionPeriod"
{-# DEPRECATED rdbcfsBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | A value that indicates that the restored DB cluster should be associated with the specified CharacterSet.
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsCharacterSetName :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsCharacterSetName = Lens.field @"characterSetName"
{-# DEPRECATED rdbcfsCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

-- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsCopyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Core.Bool)
rdbcfsCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# DEPRECATED rdbcfsCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The name of the DB cluster parameter group to associate with the restored DB cluster. If this argument is omitted, @default.aurora5.6@ is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDBClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# DEPRECATED rdbcfsDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead." #-}

-- | A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDBSubnetGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED rdbcfsDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | The database name for the restored DB cluster.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDatabaseName :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED rdbcfsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDeletionProtection :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Core.Bool)
rdbcfsDeletionProtection = Lens.field @"deletionProtection"
{-# DEPRECATED rdbcfsDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDomain :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsDomain = Lens.field @"domain"
{-# DEPRECATED rdbcfsDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsDomainIAMRoleName :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# DEPRECATED rdbcfsDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | The list of logs that the restored DB cluster is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe [Types.String])
rdbcfsEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# DEPRECATED rdbcfsEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Core.Bool)
rdbcfsEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# DEPRECATED rdbcfsEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

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
rdbcfsEngineVersion :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED rdbcfsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The AWS KMS key identifier for an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KM encryption key.
-- If the StorageEncrypted parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsKmsKeyId :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED rdbcfsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A value that indicates that the restored DB cluster should be associated with the specified option group.
--
-- Permanent options can't be removed from an option group. An option group can't be removed from a DB cluster once it is associated with a DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsOptionGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED rdbcfsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The port number on which the instances in the restored DB cluster accept connections.
--
-- Default: @3306@
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsPort :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Core.Int)
rdbcfsPort = Lens.field @"port"
{-# DEPRECATED rdbcfsPort "Use generic-lens or generic-optics with 'port' instead." #-}

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
rdbcfsPreferredBackupWindow :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# DEPRECATED rdbcfsPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window> in the /Amazon Aurora User Guide./
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsPreferredMaintenanceWindow :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED rdbcfsPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The prefix for all of the file names that contain the data used to create the Amazon Aurora DB cluster. If you do not specify a __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created by using all of the files in the Amazon S3 bucket.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsS3Prefix :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Types.String)
rdbcfsS3Prefix = Lens.field @"s3Prefix"
{-# DEPRECATED rdbcfsS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | A value that indicates whether the restored DB cluster is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsStorageEncrypted :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe Core.Bool)
rdbcfsStorageEncrypted = Lens.field @"storageEncrypted"
{-# DEPRECATED rdbcfsStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsTags :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe [Types.Tag])
rdbcfsTags = Lens.field @"tags"
{-# DEPRECATED rdbcfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of EC2 VPC security groups to associate with the restored DB cluster.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsVpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromS3 (Core.Maybe [Types.String])
rdbcfsVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED rdbcfsVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

instance Core.AWSRequest RestoreDBClusterFromS3 where
  type Rs RestoreDBClusterFromS3 = RestoreDBClusterFromS3Response
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RestoreDBClusterFromS3")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
                Core.<> (Core.toQueryValue "Engine" engine)
                Core.<> (Core.toQueryValue "MasterUsername" masterUsername)
                Core.<> (Core.toQueryValue "MasterUserPassword" masterUserPassword)
                Core.<> (Core.toQueryValue "SourceEngine" sourceEngine)
                Core.<> (Core.toQueryValue "SourceEngineVersion" sourceEngineVersion)
                Core.<> (Core.toQueryValue "S3BucketName" s3BucketName)
                Core.<> (Core.toQueryValue "S3IngestionRoleArn" s3IngestionRoleArn)
                Core.<> ( Core.toQueryValue
                            "AvailabilityZones"
                            (Core.toQueryList "AvailabilityZone" Core.<$> availabilityZones)
                        )
                Core.<> (Core.toQueryValue "BacktrackWindow" Core.<$> backtrackWindow)
                Core.<> ( Core.toQueryValue "BackupRetentionPeriod"
                            Core.<$> backupRetentionPeriod
                        )
                Core.<> (Core.toQueryValue "CharacterSetName" Core.<$> characterSetName)
                Core.<> ( Core.toQueryValue "CopyTagsToSnapshot"
                            Core.<$> copyTagsToSnapshot
                        )
                Core.<> ( Core.toQueryValue "DBClusterParameterGroupName"
                            Core.<$> dBClusterParameterGroupName
                        )
                Core.<> (Core.toQueryValue "DBSubnetGroupName" Core.<$> dBSubnetGroupName)
                Core.<> (Core.toQueryValue "DatabaseName" Core.<$> databaseName)
                Core.<> ( Core.toQueryValue "DeletionProtection"
                            Core.<$> deletionProtection
                        )
                Core.<> (Core.toQueryValue "Domain" Core.<$> domain)
                Core.<> (Core.toQueryValue "DomainIAMRoleName" Core.<$> domainIAMRoleName)
                Core.<> ( Core.toQueryValue
                            "EnableCloudwatchLogsExports"
                            (Core.toQueryList "member" Core.<$> enableCloudwatchLogsExports)
                        )
                Core.<> ( Core.toQueryValue "EnableIAMDatabaseAuthentication"
                            Core.<$> enableIAMDatabaseAuthentication
                        )
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> (Core.toQueryValue "OptionGroupName" Core.<$> optionGroupName)
                Core.<> (Core.toQueryValue "Port" Core.<$> port)
                Core.<> ( Core.toQueryValue "PreferredBackupWindow"
                            Core.<$> preferredBackupWindow
                        )
                Core.<> ( Core.toQueryValue "PreferredMaintenanceWindow"
                            Core.<$> preferredMaintenanceWindow
                        )
                Core.<> (Core.toQueryValue "S3Prefix" Core.<$> s3Prefix)
                Core.<> (Core.toQueryValue "StorageEncrypted" Core.<$> storageEncrypted)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
                Core.<> ( Core.toQueryValue
                            "VpcSecurityGroupIds"
                            ( Core.toQueryList "VpcSecurityGroupId"
                                Core.<$> vpcSecurityGroupIds
                            )
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "RestoreDBClusterFromS3Result"
      ( \s h x ->
          RestoreDBClusterFromS3Response'
            Core.<$> (x Core..@? "DBCluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreDBClusterFromS3Response' smart constructor.
data RestoreDBClusterFromS3Response = RestoreDBClusterFromS3Response'
  { dBCluster :: Core.Maybe Types.DBCluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RestoreDBClusterFromS3Response' value with any optional fields omitted.
mkRestoreDBClusterFromS3Response ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreDBClusterFromS3Response
mkRestoreDBClusterFromS3Response responseStatus =
  RestoreDBClusterFromS3Response'
    { dBCluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsrrsDBCluster :: Lens.Lens' RestoreDBClusterFromS3Response (Core.Maybe Types.DBCluster)
rdbcfsrrsDBCluster = Lens.field @"dBCluster"
{-# DEPRECATED rdbcfsrrsDBCluster "Use generic-lens or generic-optics with 'dBCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsrrsResponseStatus :: Lens.Lens' RestoreDBClusterFromS3Response Core.Int
rdbcfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rdbcfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
