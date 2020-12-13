{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RestoreDBClusterFromSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster from a DB snapshot or DB cluster snapshot. This action only applies to Aurora DB clusters.
--
-- The target DB cluster is created from the source snapshot with a default configuration. If you don't specify a security group, the new DB cluster is associated with the default security group.
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.RestoreDBClusterFromSnapshot
  ( -- * Creating a request
    RestoreDBClusterFromSnapshot (..),
    mkRestoreDBClusterFromSnapshot,

    -- ** Request lenses
    rdcfsEngineVersion,
    rdcfsDeletionProtection,
    rdcfsDBClusterIdentifier,
    rdcfsSnapshotIdentifier,
    rdcfsDBSubnetGroupName,
    rdcfsDomain,
    rdcfsBacktrackWindow,
    rdcfsEngine,
    rdcfsAvailabilityZones,
    rdcfsKMSKeyId,
    rdcfsVPCSecurityGroupIds,
    rdcfsDatabaseName,
    rdcfsDBClusterParameterGroupName,
    rdcfsEngineMode,
    rdcfsScalingConfiguration,
    rdcfsOptionGroupName,
    rdcfsCopyTagsToSnapshot,
    rdcfsDomainIAMRoleName,
    rdcfsTags,
    rdcfsPort,
    rdcfsEnableIAMDatabaseAuthentication,
    rdcfsEnableCloudwatchLogsExports,

    -- * Destructuring the response
    RestoreDBClusterFromSnapshotResponse (..),
    mkRestoreDBClusterFromSnapshotResponse,

    -- ** Response lenses
    rdbcfsrsDBCluster,
    rdbcfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRestoreDBClusterFromSnapshot' smart constructor.
data RestoreDBClusterFromSnapshot = RestoreDBClusterFromSnapshot'
  { -- | The version of the database engine to use for the new DB cluster.
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
    -- | The name of the DB cluster to create from the DB snapshot or DB cluster snapshot. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 letters, numbers, or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @my-snapshot-id@
    dbClusterIdentifier :: Lude.Text,
    -- | The identifier for the DB snapshot or DB cluster snapshot to restore from.
    --
    -- You can use either the name or the Amazon Resource Name (ARN) to specify a DB cluster snapshot. However, you can use only the ARN to specify a DB snapshot.
    -- Constraints:
    --
    --     * Must match the identifier of an existing Snapshot.
    snapshotIdentifier :: Lude.Text,
    -- | The name of the DB subnet group to use for the new DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing DB subnet group.
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Lude.Maybe Lude.Text,
    -- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
    domain :: Lude.Maybe Lude.Text,
    -- | The target backtrack window, in seconds. To disable backtracking, set this value to 0.
    --
    -- Default: 0
    -- Constraints:
    --
    --     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
    backtrackWindow :: Lude.Maybe Lude.Integer,
    -- | The database engine to use for the new DB cluster.
    --
    -- Default: The same as source
    -- Constraint: Must be compatible with the engine of the source
    engine :: Lude.Text,
    -- | Provides the list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
    availabilityZones :: Lude.Maybe [Lude.Text],
    -- | The AWS KMS key identifier to use when restoring an encrypted DB cluster from a DB snapshot or DB cluster snapshot.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key.
    -- If you don't specify a value for the @KmsKeyId@ parameter, then the following occurs:
    --
    --     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is encrypted, then the restored DB cluster is encrypted using the KMS key that was used to encrypt the DB snapshot or DB cluster snapshot.
    --
    --
    --     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ isn't encrypted, then the restored DB cluster isn't encrypted.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | A list of VPC security groups that the new DB cluster will belong to.
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    -- | The database name for the restored DB cluster.
    databaseName :: Lude.Maybe Lude.Text,
    -- | The name of the DB cluster parameter group to associate with this DB cluster. If this argument is omitted, the default DB cluster parameter group for the specified engine is used.
    --
    -- Constraints:
    --
    --     * If supplied, must match the name of an existing default DB cluster parameter group.
    --
    --
    --     * Must be 1 to 255 letters, numbers, or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens.
    dbClusterParameterGroupName :: Lude.Maybe Lude.Text,
    -- | The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
    engineMode :: Lude.Maybe Lude.Text,
    -- | For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
    scalingConfiguration :: Lude.Maybe ScalingConfiguration,
    -- | The name of the option group to use for the restored DB cluster.
    optionGroupName :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    -- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
    domainIAMRoleName :: Lude.Maybe Lude.Text,
    -- | The tags to be assigned to the restored DB cluster.
    tags :: Lude.Maybe [Tag],
    -- | The port number on which the new DB cluster accepts connections.
    --
    -- Constraints: This value must be @1150-65535@
    -- Default: The same port as the original DB cluster.
    port :: Lude.Maybe Lude.Int,
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Lude.Maybe Lude.Bool,
    -- | The list of logs that the restored DB cluster is to export to Amazon CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Aurora User Guide/ .
    enableCloudwatchLogsExports :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBClusterFromSnapshot' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The version of the database engine to use for the new DB cluster.
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
-- * 'dbClusterIdentifier' - The name of the DB cluster to create from the DB snapshot or DB cluster snapshot. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@
-- * 'snapshotIdentifier' - The identifier for the DB snapshot or DB cluster snapshot to restore from.
--
-- You can use either the name or the Amazon Resource Name (ARN) to specify a DB cluster snapshot. However, you can use only the ARN to specify a DB snapshot.
-- Constraints:
--
--     * Must match the identifier of an existing Snapshot.
--
--
-- * 'dbSubnetGroupName' - The name of the DB subnet group to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DB subnet group.
-- Example: @mySubnetgroup@
-- * 'domain' - Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
-- * 'backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set this value to 0.
--
-- Default: 0
-- Constraints:
--
--     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
--
-- * 'engine' - The database engine to use for the new DB cluster.
--
-- Default: The same as source
-- Constraint: Must be compatible with the engine of the source
-- * 'availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
-- * 'kmsKeyId' - The AWS KMS key identifier to use when restoring an encrypted DB cluster from a DB snapshot or DB cluster snapshot.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key.
-- If you don't specify a value for the @KmsKeyId@ parameter, then the following occurs:
--
--     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is encrypted, then the restored DB cluster is encrypted using the KMS key that was used to encrypt the DB snapshot or DB cluster snapshot.
--
--
--     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ isn't encrypted, then the restored DB cluster isn't encrypted.
--
--
-- * 'vpcSecurityGroupIds' - A list of VPC security groups that the new DB cluster will belong to.
-- * 'databaseName' - The database name for the restored DB cluster.
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB cluster. If this argument is omitted, the default DB cluster parameter group for the specified engine is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing default DB cluster parameter group.
--
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- * 'engineMode' - The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
-- * 'scalingConfiguration' - For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
-- * 'optionGroupName' - The name of the option group to use for the restored DB cluster.
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
-- * 'domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
-- * 'tags' - The tags to be assigned to the restored DB cluster.
-- * 'port' - The port number on which the new DB cluster accepts connections.
--
-- Constraints: This value must be @1150-65535@
-- Default: The same port as the original DB cluster.
-- * 'enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
-- * 'enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to Amazon CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Aurora User Guide/ .
mkRestoreDBClusterFromSnapshot ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'snapshotIdentifier'
  Lude.Text ->
  -- | 'engine'
  Lude.Text ->
  RestoreDBClusterFromSnapshot
mkRestoreDBClusterFromSnapshot
  pDBClusterIdentifier_
  pSnapshotIdentifier_
  pEngine_ =
    RestoreDBClusterFromSnapshot'
      { engineVersion = Lude.Nothing,
        deletionProtection = Lude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        snapshotIdentifier = pSnapshotIdentifier_,
        dbSubnetGroupName = Lude.Nothing,
        domain = Lude.Nothing,
        backtrackWindow = Lude.Nothing,
        engine = pEngine_,
        availabilityZones = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        vpcSecurityGroupIds = Lude.Nothing,
        databaseName = Lude.Nothing,
        dbClusterParameterGroupName = Lude.Nothing,
        engineMode = Lude.Nothing,
        scalingConfiguration = Lude.Nothing,
        optionGroupName = Lude.Nothing,
        copyTagsToSnapshot = Lude.Nothing,
        domainIAMRoleName = Lude.Nothing,
        tags = Lude.Nothing,
        port = Lude.Nothing,
        enableIAMDatabaseAuthentication = Lude.Nothing,
        enableCloudwatchLogsExports = Lude.Nothing
      }

-- | The version of the database engine to use for the new DB cluster.
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
rdcfsEngineVersion :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Text)
rdcfsEngineVersion = Lens.lens (engineVersion :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDeletionProtection :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Bool)
rdcfsDeletionProtection = Lens.lens (deletionProtection :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The name of the DB cluster to create from the DB snapshot or DB cluster snapshot. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDBClusterIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Lude.Text
rdcfsDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: RestoreDBClusterFromSnapshot -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The identifier for the DB snapshot or DB cluster snapshot to restore from.
--
-- You can use either the name or the Amazon Resource Name (ARN) to specify a DB cluster snapshot. However, you can use only the ARN to specify a DB snapshot.
-- Constraints:
--
--     * Must match the identifier of an existing Snapshot.
--
--
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsSnapshotIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Lude.Text
rdcfsSnapshotIdentifier = Lens.lens (snapshotIdentifier :: RestoreDBClusterFromSnapshot -> Lude.Text) (\s a -> s {snapshotIdentifier = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The name of the DB subnet group to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DB subnet group.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDBSubnetGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Text)
rdcfsDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDomain :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Text)
rdcfsDomain = Lens.lens (domain :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: RestoreDBClusterFromSnapshot)
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
rdcfsBacktrackWindow :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Integer)
rdcfsBacktrackWindow = Lens.lens (backtrackWindow :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Integer) (\s a -> s {backtrackWindow = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

-- | The database engine to use for the new DB cluster.
--
-- Default: The same as source
-- Constraint: Must be compatible with the engine of the source
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsEngine :: Lens.Lens' RestoreDBClusterFromSnapshot Lude.Text
rdcfsEngine = Lens.lens (engine :: RestoreDBClusterFromSnapshot -> Lude.Text) (\s a -> s {engine = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Provides the list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsAvailabilityZones :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe [Lude.Text])
rdcfsAvailabilityZones = Lens.lens (availabilityZones :: RestoreDBClusterFromSnapshot -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The AWS KMS key identifier to use when restoring an encrypted DB cluster from a DB snapshot or DB cluster snapshot.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key.
-- If you don't specify a value for the @KmsKeyId@ parameter, then the following occurs:
--
--     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is encrypted, then the restored DB cluster is encrypted using the KMS key that was used to encrypt the DB snapshot or DB cluster snapshot.
--
--
--     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ isn't encrypted, then the restored DB cluster isn't encrypted.
--
--
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsKMSKeyId :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Text)
rdcfsKMSKeyId = Lens.lens (kmsKeyId :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A list of VPC security groups that the new DB cluster will belong to.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsVPCSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe [Lude.Text])
rdcfsVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: RestoreDBClusterFromSnapshot -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | The database name for the restored DB cluster.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDatabaseName :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Text)
rdcfsDatabaseName = Lens.lens (databaseName :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the DB cluster parameter group to associate with this DB cluster. If this argument is omitted, the default DB cluster parameter group for the specified engine is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing default DB cluster parameter group.
--
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDBClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Text)
rdcfsDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

-- | The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
--
-- /Note:/ Consider using 'engineMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsEngineMode :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Text)
rdcfsEngineMode = Lens.lens (engineMode :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {engineMode = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsEngineMode "Use generic-lens or generic-optics with 'engineMode' instead." #-}

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
--
-- /Note:/ Consider using 'scalingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsScalingConfiguration :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe ScalingConfiguration)
rdcfsScalingConfiguration = Lens.lens (scalingConfiguration :: RestoreDBClusterFromSnapshot -> Lude.Maybe ScalingConfiguration) (\s a -> s {scalingConfiguration = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsScalingConfiguration "Use generic-lens or generic-optics with 'scalingConfiguration' instead." #-}

-- | The name of the option group to use for the restored DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsOptionGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Text)
rdcfsOptionGroupName = Lens.lens (optionGroupName :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsCopyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Bool)
rdcfsCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsDomainIAMRoleName :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Text)
rdcfsDomainIAMRoleName = Lens.lens (domainIAMRoleName :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | The tags to be assigned to the restored DB cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsTags :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe [Tag])
rdcfsTags = Lens.lens (tags :: RestoreDBClusterFromSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: This value must be @1150-65535@
-- Default: The same port as the original DB cluster.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsPort :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Int)
rdcfsPort = Lens.lens (port :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe Lude.Bool)
rdcfsEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: RestoreDBClusterFromSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | The list of logs that the restored DB cluster is to export to Amazon CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcfsEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromSnapshot (Lude.Maybe [Lude.Text])
rdcfsEnableCloudwatchLogsExports = Lens.lens (enableCloudwatchLogsExports :: RestoreDBClusterFromSnapshot -> Lude.Maybe [Lude.Text]) (\s a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterFromSnapshot)
{-# DEPRECATED rdcfsEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

instance Lude.AWSRequest RestoreDBClusterFromSnapshot where
  type
    Rs RestoreDBClusterFromSnapshot =
      RestoreDBClusterFromSnapshotResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RestoreDBClusterFromSnapshotResult"
      ( \s h x ->
          RestoreDBClusterFromSnapshotResponse'
            Lude.<$> (x Lude..@? "DBCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreDBClusterFromSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestoreDBClusterFromSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreDBClusterFromSnapshot where
  toQuery RestoreDBClusterFromSnapshot' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RestoreDBClusterFromSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "DeletionProtection" Lude.=: deletionProtection,
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "SnapshotIdentifier" Lude.=: snapshotIdentifier,
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "Domain" Lude.=: domain,
        "BacktrackWindow" Lude.=: backtrackWindow,
        "Engine" Lude.=: engine,
        "AvailabilityZones"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "AvailabilityZone" Lude.<$> availabilityZones),
        "KmsKeyId" Lude.=: kmsKeyId,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "DatabaseName" Lude.=: databaseName,
        "DBClusterParameterGroupName" Lude.=: dbClusterParameterGroupName,
        "EngineMode" Lude.=: engineMode,
        "ScalingConfiguration" Lude.=: scalingConfiguration,
        "OptionGroupName" Lude.=: optionGroupName,
        "CopyTagsToSnapshot" Lude.=: copyTagsToSnapshot,
        "DomainIAMRoleName" Lude.=: domainIAMRoleName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "EnableIAMDatabaseAuthentication"
          Lude.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> enableCloudwatchLogsExports)
      ]

-- | /See:/ 'mkRestoreDBClusterFromSnapshotResponse' smart constructor.
data RestoreDBClusterFromSnapshotResponse = RestoreDBClusterFromSnapshotResponse'
  { dbCluster :: Lude.Maybe DBCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBClusterFromSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'dbCluster' -
-- * 'responseStatus' - The response status code.
mkRestoreDBClusterFromSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreDBClusterFromSnapshotResponse
mkRestoreDBClusterFromSnapshotResponse pResponseStatus_ =
  RestoreDBClusterFromSnapshotResponse'
    { dbCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsrsDBCluster :: Lens.Lens' RestoreDBClusterFromSnapshotResponse (Lude.Maybe DBCluster)
rdbcfsrsDBCluster = Lens.lens (dbCluster :: RestoreDBClusterFromSnapshotResponse -> Lude.Maybe DBCluster) (\s a -> s {dbCluster = a} :: RestoreDBClusterFromSnapshotResponse)
{-# DEPRECATED rdbcfsrsDBCluster "Use generic-lens or generic-optics with 'dbCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcfsrsResponseStatus :: Lens.Lens' RestoreDBClusterFromSnapshotResponse Lude.Int
rdbcfsrsResponseStatus = Lens.lens (responseStatus :: RestoreDBClusterFromSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreDBClusterFromSnapshotResponse)
{-# DEPRECATED rdbcfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
