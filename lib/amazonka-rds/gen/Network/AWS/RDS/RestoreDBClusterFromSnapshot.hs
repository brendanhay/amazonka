{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RestoreDBClusterFromSnapshot (..)
    , mkRestoreDBClusterFromSnapshot
    -- ** Request lenses
    , rDBClusterIdentifier
    , rSnapshotIdentifier
    , rEngine
    , rAvailabilityZones
    , rBacktrackWindow
    , rCopyTagsToSnapshot
    , rDBClusterParameterGroupName
    , rDBSubnetGroupName
    , rDatabaseName
    , rDeletionProtection
    , rDomain
    , rDomainIAMRoleName
    , rEnableCloudwatchLogsExports
    , rEnableIAMDatabaseAuthentication
    , rEngineMode
    , rEngineVersion
    , rKmsKeyId
    , rOptionGroupName
    , rPort
    , rScalingConfiguration
    , rTags
    , rVpcSecurityGroupIds

    -- * Destructuring the response
    , RestoreDBClusterFromSnapshotResponse (..)
    , mkRestoreDBClusterFromSnapshotResponse
    -- ** Response lenses
    , rrsDBCluster
    , rrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRestoreDBClusterFromSnapshot' smart constructor.
data RestoreDBClusterFromSnapshot = RestoreDBClusterFromSnapshot'
  { dBClusterIdentifier :: Core.Text
    -- ^ The name of the DB cluster to create from the DB snapshot or DB cluster snapshot. This parameter isn't case-sensitive.
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
  , snapshotIdentifier :: Core.Text
    -- ^ The identifier for the DB snapshot or DB cluster snapshot to restore from.
--
-- You can use either the name or the Amazon Resource Name (ARN) to specify a DB cluster snapshot. However, you can use only the ARN to specify a DB snapshot.
-- Constraints:
--
--     * Must match the identifier of an existing Snapshot.
--
--
  , engine :: Core.Text
    -- ^ The database engine to use for the new DB cluster.
--
-- Default: The same as source
-- Constraint: Must be compatible with the engine of the source
  , availabilityZones :: Core.Maybe [Core.Text]
    -- ^ Provides the list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
  , backtrackWindow :: Core.Maybe Core.Integer
    -- ^ The target backtrack window, in seconds. To disable backtracking, set this value to 0.
--
-- Default: 0
-- Constraints:
--
--     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
--
  , copyTagsToSnapshot :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
  , dBClusterParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB cluster parameter group to associate with this DB cluster. If this argument is omitted, the default DB cluster parameter group for the specified engine is used.
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
  , dBSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB subnet group to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DB subnet group.
-- Example: @mySubnetgroup@ 
  , databaseName :: Core.Maybe Core.Text
    -- ^ The database name for the restored DB cluster.
  , deletionProtection :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. 
  , domain :: Core.Maybe Core.Text
    -- ^ Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ . 
  , domainIAMRoleName :: Core.Maybe Core.Text
    -- ^ Specify the name of the IAM role to be used when making API calls to the Directory Service.
  , enableCloudwatchLogsExports :: Core.Maybe [Core.Text]
    -- ^ The list of logs that the restored DB cluster is to export to Amazon CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Aurora User Guide/ .
  , enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./ 
  , engineMode :: Core.Maybe Core.Text
    -- ^ The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The version of the database engine to use for the new DB cluster.
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
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS KMS key identifier to use when restoring an encrypted DB cluster from a DB snapshot or DB cluster snapshot.
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
  , optionGroupName :: Core.Maybe Core.Text
    -- ^ The name of the option group to use for the restored DB cluster.
  , port :: Core.Maybe Core.Int
    -- ^ The port number on which the new DB cluster accepts connections.
--
-- Constraints: This value must be @1150-65535@ 
-- Default: The same port as the original DB cluster.
  , scalingConfiguration :: Core.Maybe Types.ScalingConfiguration
    -- ^ For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to be assigned to the restored DB cluster.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of VPC security groups that the new DB cluster will belong to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreDBClusterFromSnapshot' value with any optional fields omitted.
mkRestoreDBClusterFromSnapshot
    :: Core.Text -- ^ 'dBClusterIdentifier'
    -> Core.Text -- ^ 'snapshotIdentifier'
    -> Core.Text -- ^ 'engine'
    -> RestoreDBClusterFromSnapshot
mkRestoreDBClusterFromSnapshot dBClusterIdentifier
  snapshotIdentifier engine
  = RestoreDBClusterFromSnapshot'{dBClusterIdentifier,
                                  snapshotIdentifier, engine, availabilityZones = Core.Nothing,
                                  backtrackWindow = Core.Nothing, copyTagsToSnapshot = Core.Nothing,
                                  dBClusterParameterGroupName = Core.Nothing,
                                  dBSubnetGroupName = Core.Nothing, databaseName = Core.Nothing,
                                  deletionProtection = Core.Nothing, domain = Core.Nothing,
                                  domainIAMRoleName = Core.Nothing,
                                  enableCloudwatchLogsExports = Core.Nothing,
                                  enableIAMDatabaseAuthentication = Core.Nothing,
                                  engineMode = Core.Nothing, engineVersion = Core.Nothing,
                                  kmsKeyId = Core.Nothing, optionGroupName = Core.Nothing,
                                  port = Core.Nothing, scalingConfiguration = Core.Nothing,
                                  tags = Core.Nothing, vpcSecurityGroupIds = Core.Nothing}

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
rDBClusterIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Core.Text
rDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE rDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

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
rSnapshotIdentifier :: Lens.Lens' RestoreDBClusterFromSnapshot Core.Text
rSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE rSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

-- | The database engine to use for the new DB cluster.
--
-- Default: The same as source
-- Constraint: Must be compatible with the engine of the source
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEngine :: Lens.Lens' RestoreDBClusterFromSnapshot Core.Text
rEngine = Lens.field @"engine"
{-# INLINEABLE rEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Provides the list of Availability Zones (AZs) where instances in the restored DB cluster can be created.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAvailabilityZones :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe [Core.Text])
rAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE rAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

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
{-# INLINEABLE rBacktrackWindow #-}
{-# DEPRECATED backtrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead"  #-}

-- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCopyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Bool)
rCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# INLINEABLE rCopyTagsToSnapshot #-}
{-# DEPRECATED copyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead"  #-}

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
rDBClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Text)
rDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# INLINEABLE rDBClusterParameterGroupName #-}
{-# DEPRECATED dBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead"  #-}

-- | The name of the DB subnet group to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DB subnet group.
-- Example: @mySubnetgroup@ 
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDBSubnetGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Text)
rDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# INLINEABLE rDBSubnetGroupName #-}
{-# DEPRECATED dBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead"  #-}

-- | The database name for the restored DB cluster.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDatabaseName :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Text)
rDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE rDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. 
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeletionProtection :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Bool)
rDeletionProtection = Lens.field @"deletionProtection"
{-# INLINEABLE rDeletionProtection #-}
{-# DEPRECATED deletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead"  #-}

-- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ . 
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDomain :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Text)
rDomain = Lens.field @"domain"
{-# INLINEABLE rDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDomainIAMRoleName :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Text)
rDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# INLINEABLE rDomainIAMRoleName #-}
{-# DEPRECATED domainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead"  #-}

-- | The list of logs that the restored DB cluster is to export to Amazon CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe [Core.Text])
rEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# INLINEABLE rEnableCloudwatchLogsExports #-}
{-# DEPRECATED enableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead"  #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./ 
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Bool)
rEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# INLINEABLE rEnableIAMDatabaseAuthentication #-}
{-# DEPRECATED enableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead"  #-}

-- | The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
--
-- /Note:/ Consider using 'engineMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEngineMode :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Text)
rEngineMode = Lens.field @"engineMode"
{-# INLINEABLE rEngineMode #-}
{-# DEPRECATED engineMode "Use generic-lens or generic-optics with 'engineMode' instead"  #-}

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
rEngineVersion :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Text)
rEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE rEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

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
rKmsKeyId :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Text)
rKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE rKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The name of the option group to use for the restored DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOptionGroupName :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Text)
rOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE rOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: This value must be @1150-65535@ 
-- Default: The same port as the original DB cluster.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPort :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Core.Int)
rPort = Lens.field @"port"
{-# INLINEABLE rPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
--
-- /Note:/ Consider using 'scalingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rScalingConfiguration :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe Types.ScalingConfiguration)
rScalingConfiguration = Lens.field @"scalingConfiguration"
{-# INLINEABLE rScalingConfiguration #-}
{-# DEPRECATED scalingConfiguration "Use generic-lens or generic-optics with 'scalingConfiguration' instead"  #-}

-- | The tags to be assigned to the restored DB cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTags :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe [Types.Tag])
rTags = Lens.field @"tags"
{-# INLINEABLE rTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A list of VPC security groups that the new DB cluster will belong to.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rVpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromSnapshot (Core.Maybe [Core.Text])
rVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE rVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery RestoreDBClusterFromSnapshot where
        toQuery RestoreDBClusterFromSnapshot{..}
          = Core.toQueryPair "Action"
              ("RestoreDBClusterFromSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier
              Core.<> Core.toQueryPair "SnapshotIdentifier" snapshotIdentifier
              Core.<> Core.toQueryPair "Engine" engine
              Core.<>
              Core.toQueryPair "AvailabilityZones"
                (Core.maybe Core.mempty (Core.toQueryList "AvailabilityZone")
                   availabilityZones)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "BacktrackWindow")
                backtrackWindow
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CopyTagsToSnapshot")
                copyTagsToSnapshot
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DBClusterParameterGroupName")
                dBClusterParameterGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBSubnetGroupName")
                dBSubnetGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DatabaseName")
                databaseName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeletionProtection")
                deletionProtection
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Domain") domain
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DomainIAMRoleName")
                domainIAMRoleName
              Core.<>
              Core.toQueryPair "EnableCloudwatchLogsExports"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   enableCloudwatchLogsExports)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "EnableIAMDatabaseAuthentication")
                enableIAMDatabaseAuthentication
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EngineMode") engineMode
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EngineVersion")
                engineVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OptionGroupName")
                optionGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ScalingConfiguration")
                scalingConfiguration
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupIds)

instance Core.ToHeaders RestoreDBClusterFromSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RestoreDBClusterFromSnapshot where
        type Rs RestoreDBClusterFromSnapshot =
             RestoreDBClusterFromSnapshotResponse
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
          = Response.receiveXMLWrapper "RestoreDBClusterFromSnapshotResult"
              (\ s h x ->
                 RestoreDBClusterFromSnapshotResponse' Core.<$>
                   (x Core..@? "DBCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreDBClusterFromSnapshotResponse' smart constructor.
data RestoreDBClusterFromSnapshotResponse = RestoreDBClusterFromSnapshotResponse'
  { dBCluster :: Core.Maybe Types.DBCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreDBClusterFromSnapshotResponse' value with any optional fields omitted.
mkRestoreDBClusterFromSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreDBClusterFromSnapshotResponse
mkRestoreDBClusterFromSnapshotResponse responseStatus
  = RestoreDBClusterFromSnapshotResponse'{dBCluster = Core.Nothing,
                                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsDBCluster :: Lens.Lens' RestoreDBClusterFromSnapshotResponse (Core.Maybe Types.DBCluster)
rrsDBCluster = Lens.field @"dBCluster"
{-# INLINEABLE rrsDBCluster #-}
{-# DEPRECATED dBCluster "Use generic-lens or generic-optics with 'dBCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RestoreDBClusterFromSnapshotResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
