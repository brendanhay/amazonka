{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rdcfsEngineVersion,
    rdcfsDeletionProtection,
    rdcfsStorageEncrypted,
    rdcfsDBSubnetGroupName,
    rdcfsDomain,
    rdcfsBacktrackWindow,
    rdcfsPreferredMaintenanceWindow,
    rdcfsAvailabilityZones,
    rdcfsCharacterSetName,
    rdcfsKMSKeyId,
    rdcfsPreferredBackupWindow,
    rdcfsBackupRetentionPeriod,
    rdcfsVPCSecurityGroupIds,
    rdcfsDatabaseName,
    rdcfsDBClusterParameterGroupName,
    rdcfsS3Prefix,
    rdcfsOptionGroupName,
    rdcfsCopyTagsToSnapshot,
    rdcfsDomainIAMRoleName,
    rdcfsTags,
    rdcfsPort,
    rdcfsEnableIAMDatabaseAuthentication,
    rdcfsEnableCloudwatchLogsExports,
    rdcfsDBClusterIdentifier,
    rdcfsEngine,
    rdcfsMasterUsername,
    rdcfsMasterUserPassword,
    rdcfsSourceEngine,
    rdcfsSourceEngineVersion,
    rdcfsS3BucketName,
    rdcfsS3IngestionRoleARN,

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
  { engineVersion ::
      Lude.Maybe Lude.Text,
    deletionProtection :: Lude.Maybe Lude.Bool,
    storageEncrypted :: Lude.Maybe Lude.Bool,
    dbSubnetGroupName :: Lude.Maybe Lude.Text,
    domain :: Lude.Maybe Lude.Text,
    backtrackWindow :: Lude.Maybe Lude.Integer,
    preferredMaintenanceWindow ::
      Lude.Maybe Lude.Text,
    availabilityZones :: Lude.Maybe [Lude.Text],
    characterSetName :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    databaseName :: Lude.Maybe Lude.Text,
    dbClusterParameterGroupName ::
      Lude.Maybe Lude.Text,
    s3Prefix :: Lude.Maybe Lude.Text,
    optionGroupName :: Lude.Maybe Lude.Text,
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    domainIAMRoleName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    port :: Lude.Maybe Lude.Int,
    enableIAMDatabaseAuthentication ::
      Lude.Maybe Lude.Bool,
    enableCloudwatchLogsExports ::
      Lude.Maybe [Lude.Text],
    dbClusterIdentifier :: Lude.Text,
    engine :: Lude.Text,
    masterUsername :: Lude.Text,
    masterUserPassword :: Lude.Text,
    sourceEngine :: Lude.Text,
    sourceEngineVersion :: Lude.Text,
    s3BucketName :: Lude.Text,
    s3IngestionRoleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBClusterFromS3' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - A list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
-- * 'backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set this value to 0.
--
-- Default: 0
-- Constraints:
--
--     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
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
-- * 'characterSetName' - A value that indicates that the restored DB cluster should be associated with the specified CharacterSet.
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
-- * 'databaseName' - The database name for the restored DB cluster.
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
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with the restored DB cluster. If this argument is omitted, @default.aurora5.6@ is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
-- * 'dbSubnetGroupName' - A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@
-- * 'deletionProtection' - A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
-- * 'domain' - Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
-- * 'domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
-- * 'enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
-- * 'enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
-- * 'engine' - The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
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
-- * 'kmsKeyId' - The AWS KMS key identifier for an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KM encryption key.
-- If the StorageEncrypted parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
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
-- * 'optionGroupName' - A value that indicates that the restored DB cluster should be associated with the specified option group.
--
-- Permanent options can't be removed from an option group. An option group can't be removed from a DB cluster once it is associated with a DB cluster.
-- * 'port' - The port number on which the instances in the restored DB cluster accept connections.
--
-- Default: @3306@
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
-- * 'preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window> in the /Amazon Aurora User Guide./
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
-- * 's3BucketName' - The name of the Amazon S3 bucket that contains the data used to create the Amazon Aurora DB cluster.
-- * 's3IngestionRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon RDS to access the Amazon S3 bucket on your behalf.
-- * 's3Prefix' - The prefix for all of the file names that contain the data used to create the Amazon Aurora DB cluster. If you do not specify a __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created by using all of the files in the Amazon S3 bucket.
-- * 'sourceEngine' - The identifier for the database engine that was backed up to create the files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
-- * 'sourceEngineVersion' - The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
-- Example: @5.6.40@ , @5.7.28@
-- * 'storageEncrypted' - A value that indicates whether the restored DB cluster is encrypted.
-- * 'tags' - Undocumented field.
-- * 'vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the restored DB cluster.
mkRestoreDBClusterFromS3 ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'engine'
  Lude.Text ->
  -- | 'masterUsername'
  Lude.Text ->
  -- | 'masterUserPassword'
  Lude.Text ->
  -- | 'sourceEngine'
  Lude.Text ->
  -- | 'sourceEngineVersion'
  Lude.Text ->
  -- | 's3BucketName'
  Lude.Text ->
  -- | 's3IngestionRoleARN'
  Lude.Text ->
  RestoreDBClusterFromS3
mkRestoreDBClusterFromS3
  pDBClusterIdentifier_
  pEngine_
  pMasterUsername_
  pMasterUserPassword_
  pSourceEngine_
  pSourceEngineVersion_
  pS3BucketName_
  pS3IngestionRoleARN_ =
    RestoreDBClusterFromS3'
      { engineVersion = Lude.Nothing,
        deletionProtection = Lude.Nothing,
        storageEncrypted = Lude.Nothing,
        dbSubnetGroupName = Lude.Nothing,
        domain = Lude.Nothing,
        backtrackWindow = Lude.Nothing,
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
        tags = Lude.Nothing,
        port = Lude.Nothing,
        enableIAMDatabaseAuthentication = Lude.Nothing,
        enableCloudwatchLogsExports = Lude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        engine = pEngine_,
        masterUsername = pMasterUsername_,
        masterUserPassword = pMasterUserPassword_,
        sourceEngine = pSourceEngine_,
        sourceEngineVersion = pSourceEngineVersion_,
        s3BucketName = pS3BucketName_,
        s3IngestionRoleARN = pS3IngestionRoleARN_
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
rdcfsEngineVersion :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsEngineVersion = Lens.lens (engineVersion :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDeletionProtection :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Bool)
rdcfsDeletionProtection = Lens.lens (deletionProtection :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | A value that indicates whether the restored DB cluster is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsStorageEncrypted :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Bool)
rdcfsStorageEncrypted = Lens.lens (storageEncrypted :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {storageEncrypted = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDBSubnetGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDomain :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsDomain = Lens.lens (domain :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

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
rdcfsBacktrackWindow :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Integer)
rdcfsBacktrackWindow = Lens.lens (backtrackWindow :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Integer) (\s a -> s {backtrackWindow = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window> in the /Amazon Aurora User Guide./
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsPreferredMaintenanceWindow :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | A list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsAvailabilityZones :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe [Lude.Text])
rdcfsAvailabilityZones = Lens.lens (availabilityZones :: RestoreDBClusterFromS3 -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A value that indicates that the restored DB cluster should be associated with the specified CharacterSet.
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsCharacterSetName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsCharacterSetName = Lens.lens (characterSetName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {characterSetName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

-- | The AWS KMS key identifier for an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KM encryption key.
-- If the StorageEncrypted parameter is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsKMSKeyId :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsKMSKeyId = Lens.lens (kmsKeyId :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

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
rdcfsPreferredBackupWindow :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsPreferredBackupWindow = Lens.lens (preferredBackupWindow :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

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
rdcfsBackupRetentionPeriod :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Int)
rdcfsBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | A list of EC2 VPC security groups to associate with the restored DB cluster.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsVPCSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe [Lude.Text])
rdcfsVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: RestoreDBClusterFromS3 -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | The database name for the restored DB cluster.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDatabaseName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsDatabaseName = Lens.lens (databaseName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the DB cluster parameter group to associate with the restored DB cluster. If this argument is omitted, @default.aurora5.6@ is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDBClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

-- | The prefix for all of the file names that contain the data used to create the Amazon Aurora DB cluster. If you do not specify a __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created by using all of the files in the Amazon S3 bucket.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsS3Prefix :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsS3Prefix = Lens.lens (s3Prefix :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | A value that indicates that the restored DB cluster should be associated with the specified option group.
--
-- Permanent options can't be removed from an option group. An option group can't be removed from a DB cluster once it is associated with a DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsOptionGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsOptionGroupName = Lens.lens (optionGroupName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsCopyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Bool)
rdcfsCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDomainIAMRoleName :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Text)
rdcfsDomainIAMRoleName = Lens.lens (domainIAMRoleName :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsTags :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe [Tag])
rdcfsTags = Lens.lens (tags :: RestoreDBClusterFromS3 -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which the instances in the restored DB cluster accept connections.
--
-- Default: @3306@
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsPort :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Int)
rdcfsPort = Lens.lens (port :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe Lude.Bool)
rdcfsEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: RestoreDBClusterFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | The list of logs that the restored DB cluster is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromS3 (Lude.Maybe [Lude.Text])
rdcfsEnableCloudwatchLogsExports = Lens.lens (enableCloudwatchLogsExports :: RestoreDBClusterFromS3 -> Lude.Maybe [Lude.Text]) (\s a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

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
rdcfsDBClusterIdentifier :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdcfsDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsEngine :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdcfsEngine = Lens.lens (engine :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {engine = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

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
rdcfsMasterUsername :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdcfsMasterUsername = Lens.lens (masterUsername :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {masterUsername = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsMasterUserPassword :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdcfsMasterUserPassword = Lens.lens (masterUserPassword :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {masterUserPassword = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The identifier for the database engine that was backed up to create the files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
--
-- /Note:/ Consider using 'sourceEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsSourceEngine :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdcfsSourceEngine = Lens.lens (sourceEngine :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {sourceEngine = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsSourceEngine "Use generic-lens or generic-optics with 'sourceEngine' instead." #-}

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
-- Example: @5.6.40@ , @5.7.28@
--
-- /Note:/ Consider using 'sourceEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsSourceEngineVersion :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdcfsSourceEngineVersion = Lens.lens (sourceEngineVersion :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {sourceEngineVersion = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsSourceEngineVersion "Use generic-lens or generic-optics with 'sourceEngineVersion' instead." #-}

-- | The name of the Amazon S3 bucket that contains the data used to create the Amazon Aurora DB cluster.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsS3BucketName :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdcfsS3BucketName = Lens.lens (s3BucketName :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {s3BucketName = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon RDS to access the Amazon S3 bucket on your behalf.
--
-- /Note:/ Consider using 's3IngestionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsS3IngestionRoleARN :: Lens.Lens' RestoreDBClusterFromS3 Lude.Text
rdcfsS3IngestionRoleARN = Lens.lens (s3IngestionRoleARN :: RestoreDBClusterFromS3 -> Lude.Text) (\s a -> s {s3IngestionRoleARN = a} :: RestoreDBClusterFromS3)
{-# DEPRECATED rdcfsS3IngestionRoleARN "Use generic-lens or generic-optics with 's3IngestionRoleARN' instead." #-}

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
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "Domain" Lude.=: domain,
        "BacktrackWindow" Lude.=: backtrackWindow,
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
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "EnableIAMDatabaseAuthentication"
          Lude.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> enableCloudwatchLogsExports),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "Engine" Lude.=: engine,
        "MasterUsername" Lude.=: masterUsername,
        "MasterUserPassword" Lude.=: masterUserPassword,
        "SourceEngine" Lude.=: sourceEngine,
        "SourceEngineVersion" Lude.=: sourceEngineVersion,
        "S3BucketName" Lude.=: s3BucketName,
        "S3IngestionRoleArn" Lude.=: s3IngestionRoleARN
      ]

-- | /See:/ 'mkRestoreDBClusterFromS3Response' smart constructor.
data RestoreDBClusterFromS3Response = RestoreDBClusterFromS3Response'
  { dbCluster ::
      Lude.Maybe DBCluster,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBClusterFromS3Response' with the minimum fields required to make a request.
--
-- * 'dbCluster' - Undocumented field.
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
