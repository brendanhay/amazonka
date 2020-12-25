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
    rDBClusterIdentifier,
    rSnapshotIdentifier,
    rEngine,
    rAvailabilityZones,
    rBacktrackWindow,
    rCopyTagsToSnapshot,
    rDBClusterParameterGroupName,
    rDBSubnetGroupName,
    rDatabaseName,
    rDeletionProtection,
    rDomain,
    rDomainIAMRoleName,
    rEnableCloudwatchLogsExports,
    rEnableIAMDatabaseAuthentication,
    rEngineMode,
    rEngineVersion,
    rKmsKeyId,
    rOptionGroupName,
    rPort,
    rScalingConfiguration,
    rTags,
    rVpcSecurityGroupIds,

    -- * Destructuring the response
    RestoreDBClusterFromSnapshotResponse (..),
    mkRestoreDBClusterFromSnapshotResponse,

    -- ** Response lenses
    rrsDBCluster,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkRestoreDBClusterFromSnapshot' smart constructor.
data RestoreDBClusterFromSnapshot = RestoreDBClusterFromSnapshot'
  { -- | The name of the DB cluster to create from the DB snapshot or DB cluster snapshot. This parameter isn't case-sensitive.
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
    dBClusterIdentifier :: Types.String,
    -- | The identifier for the DB snapshot or DB cluster snapshot to restore from.
    --
    -- You can use either the name or the Amazon Resource Name (ARN) to specify a DB cluster snapshot. However, you can use only the ARN to specify a DB snapshot.
    -- Constraints:
    --
    --     * Must match the identifier of an existing Snapshot.
    snapshotIdentifier :: Types.String,
    -- | The database engine to use for the new DB cluster.
    --
    -- Default: The same as source
    -- Constraint: Must be compatible with the engine of the source
    engine :: Types.String,
    -- | Provides the list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
    availabilityZones :: Core.Maybe [Types.String],
    -- | The target backtrack window, in seconds. To disable backtracking, set this value to 0.
    --
    -- Default: 0
    -- Constraints:
    --
    --     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
    backtrackWindow :: Core.Maybe Core.Integer,
    -- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
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
    dBClusterParameterGroupName :: Core.Maybe Types.String,
    -- | The name of the DB subnet group to use for the new DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing DB subnet group.
    -- Example: @mySubnetgroup@
    dBSubnetGroupName :: Core.Maybe Types.String,
    -- | The database name for the restored DB cluster.
    databaseName :: Core.Maybe Types.String,
    -- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
    domain :: Core.Maybe Types.String,
    -- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
    domainIAMRoleName :: Core.Maybe Types.String,
    -- | The list of logs that the restored DB cluster is to export to Amazon CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Aurora User Guide/ .
    enableCloudwatchLogsExports :: Core.Maybe [Types.String],
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
    -- | The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
    engineMode :: Core.Maybe Types.String,
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
    engineVersion :: Core.Maybe Types.String,
    -- | The AWS KMS key identifier to use when restoring an encrypted DB cluster from a DB snapshot or DB cluster snapshot.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key.
    -- If you don't specify a value for the @KmsKeyId@ parameter, then the following occurs:
    --
    --     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is encrypted, then the restored DB cluster is encrypted using the KMS key that was used to encrypt the DB snapshot or DB cluster snapshot.
    --
    --
    --     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ isn't encrypted, then the restored DB cluster isn't encrypted.
    kmsKeyId :: Core.Maybe Types.String,
    -- | The name of the option group to use for the restored DB cluster.
    optionGroupName :: Core.Maybe Types.String,
    -- | The port number on which the new DB cluster accepts connections.
    --
    -- Constraints: This value must be @1150-65535@
    -- Default: The same port as the original DB cluster.
    port :: Core.Maybe Core.Int,
    -- | For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
    scalingConfiguration :: Core.Maybe Types.ScalingConfiguration,
    -- | The tags to be assigned to the restored DB cluster.
    tags :: Core.Maybe [Types.Tag],
    -- | A list of VPC security groups that the new DB cluster will belong to.
    vpcSecurityGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreDBClusterFromSnapshot' value with any optional fields omitted.
mkRestoreDBClusterFromSnapshot ::
  -- | 'dBClusterIdentifier'
  Types.String ->
  -- | 'snapshotIdentifier'
  Types.String ->
  -- | 'engine'
  Types.String ->
  RestoreDBClusterFromSnapshot
mkRestoreDBClusterFromSnapshot
  dBClusterIdentifier
  snapshotIdentifier
  engine =
    RestoreDBClusterFromSnapshot'
      { dBClusterIdentifier,
        snapshotIdentifier,
        engine,
        availabilityZones = Core.Nothing,
        backtrackWindow = Core.Nothing,
        copyTagsToSnapshot = Core.Nothing,
        dBClusterParameterGroupName = Core.Nothing,
        dBSubnetGroupName = Core.Nothing,
        databaseName = Core.Nothing,
        deletionProtection = Core.Nothing,
        domain = Core.Nothing,
        domainIAMRoleName = Core.Nothing,
        enableCloudwatchLogsExports = Core.Nothing,
        enableIAMDatabaseAuthentication = Core.Nothing,
        engineMode = Core.Nothing,
        engineVersion = Core.Nothing,
        kmsKeyId = Core.Nothing,
        optionGroupName = Core.Nothing,
        port = Core.Nothing,
        scalingConfiguration = Core.Nothing,
        tags = Core.Nothing,
        vpcSecurityGroupIds = Core.Nothing
      }

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
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDBClusterIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Types.String
rDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED rDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

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
rSnapshotIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Types.String
rSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED rSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The database engine to use for the new DB cluster.
--
-- Default: The same as source
-- Constraint: Must be compatible with the engine of the source
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEngine :: Lens.Lens' RestoreDBClusterFromSnapshot Types.String
rEngine = Lens.field @"engine"
{-# DEPRECATED rEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Provides the list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAvailabilityZones :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe [Types.String])
rAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED rAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

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
rBacktrackWindow :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Integer)
rBacktrackWindow = Lens.field @"backtrackWindow"
{-# DEPRECATED rBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

-- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCopyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Bool)
rCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# DEPRECATED rCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

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
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDBClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.String)
rDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# DEPRECATED rDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead." #-}

-- | The name of the DB subnet group to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DB subnet group.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDBSubnetGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.String)
rDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED rDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | The database name for the restored DB cluster.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDatabaseName :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.String)
rDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED rDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeletionProtection :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Bool)
rDeletionProtection = Lens.field @"deletionProtection"
{-# DEPRECATED rDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDomain :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.String)
rDomain = Lens.field @"domain"
{-# DEPRECATED rDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDomainIAMRoleName :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.String)
rDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# DEPRECATED rDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | The list of logs that the restored DB cluster is to export to Amazon CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe [Types.String])
rEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# DEPRECATED rEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Bool)
rEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# DEPRECATED rEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
--
-- /Note:/ Consider using 'engineMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEngineMode :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.String)
rEngineMode = Lens.field @"engineMode"
{-# DEPRECATED rEngineMode "Use generic-lens or generic-optics with 'engineMode' instead." #-}

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
rEngineVersion :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.String)
rEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED rEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

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
rKmsKeyId :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.String)
rKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED rKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the option group to use for the restored DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOptionGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.String)
rOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED rOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: This value must be @1150-65535@
-- Default: The same port as the original DB cluster.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPort :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Int)
rPort = Lens.field @"port"
{-# DEPRECATED rPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
--
-- /Note:/ Consider using 'scalingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rScalingConfiguration :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.ScalingConfiguration)
rScalingConfiguration = Lens.field @"scalingConfiguration"
{-# DEPRECATED rScalingConfiguration "Use generic-lens or generic-optics with 'scalingConfiguration' instead." #-}

-- | The tags to be assigned to the restored DB cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTags :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe [Types.Tag])
rTags = Lens.field @"tags"
{-# DEPRECATED rTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of VPC security groups that the new DB cluster will belong to.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rVpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe [Types.String])
rVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED rVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

instance Core.AWSRequest RestoreDBClusterFromSnapshot where
  type
    Rs RestoreDBClusterFromSnapshot =
      RestoreDBClusterFromSnapshotResponse
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
            ( Core.pure ("Action", "RestoreDBClusterFromSnapshot")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
                Core.<> (Core.toQueryValue "SnapshotIdentifier" snapshotIdentifier)
                Core.<> (Core.toQueryValue "Engine" engine)
                Core.<> ( Core.toQueryValue
                            "AvailabilityZones"
                            (Core.toQueryList "AvailabilityZone" Core.<$> availabilityZones)
                        )
                Core.<> (Core.toQueryValue "BacktrackWindow" Core.<$> backtrackWindow)
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
                Core.<> (Core.toQueryValue "EngineMode" Core.<$> engineMode)
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> (Core.toQueryValue "OptionGroupName" Core.<$> optionGroupName)
                Core.<> (Core.toQueryValue "Port" Core.<$> port)
                Core.<> ( Core.toQueryValue "ScalingConfiguration"
                            Core.<$> scalingConfiguration
                        )
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
      "RestoreDBClusterFromSnapshotResult"
      ( \s h x ->
          RestoreDBClusterFromSnapshotResponse'
            Core.<$> (x Core..@? "DBCluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreDBClusterFromSnapshotResponse' smart constructor.
data RestoreDBClusterFromSnapshotResponse = RestoreDBClusterFromSnapshotResponse'
  { dBCluster :: Core.Maybe Types.DBCluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RestoreDBClusterFromSnapshotResponse' value with any optional fields omitted.
mkRestoreDBClusterFromSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreDBClusterFromSnapshotResponse
mkRestoreDBClusterFromSnapshotResponse responseStatus =
  RestoreDBClusterFromSnapshotResponse'
    { dBCluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsDBCluster :: Lens.Lens' RestoreDBClusterFromSnapshotResponse (Core.Maybe Types.DBCluster)
rrsDBCluster = Lens.field @"dBCluster"
{-# DEPRECATED rrsDBCluster "Use generic-lens or generic-optics with 'dBCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RestoreDBClusterFromSnapshotResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
