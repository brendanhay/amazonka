{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Aurora DB cluster.
--
-- You can use the @ReplicationSourceIdentifier@ parameter to create the DB cluster as a read replica of another DB cluster or Amazon RDS MySQL DB instance. For cross-region replication where the DB cluster identified by @ReplicationSourceIdentifier@ is encrypted, you must also specify the @PreSignedUrl@ parameter.
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.CreateDBCluster
  ( -- * Creating a request
    CreateDBCluster (..),
    mkCreateDBCluster,

    -- ** Request lenses
    cdbcDBClusterIdentifier,
    cdbcEngine,
    cdbcAvailabilityZones,
    cdbcBacktrackWindow,
    cdbcBackupRetentionPeriod,
    cdbcCharacterSetName,
    cdbcCopyTagsToSnapshot,
    cdbcDBClusterParameterGroupName,
    cdbcDBSubnetGroupName,
    cdbcDatabaseName,
    cdbcDeletionProtection,
    cdbcDomain,
    cdbcDomainIAMRoleName,
    cdbcEnableCloudwatchLogsExports,
    cdbcEnableGlobalWriteForwarding,
    cdbcEnableHttpEndpoint,
    cdbcEnableIAMDatabaseAuthentication,
    cdbcEngineMode,
    cdbcEngineVersion,
    cdbcGlobalClusterIdentifier,
    cdbcKmsKeyId,
    cdbcMasterUserPassword,
    cdbcMasterUsername,
    cdbcOptionGroupName,
    cdbcPort,
    cdbcPreSignedUrl,
    cdbcPreferredBackupWindow,
    cdbcPreferredMaintenanceWindow,
    cdbcReplicationSourceIdentifier,
    cdbcScalingConfiguration,
    cdbcStorageEncrypted,
    cdbcTags,
    cdbcVpcSecurityGroupIds,

    -- * Destructuring the response
    CreateDBClusterResponse (..),
    mkCreateDBClusterResponse,

    -- ** Response lenses
    cdbcrrsDBCluster,
    cdbcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateDBCluster' smart constructor.
data CreateDBCluster = CreateDBCluster'
  { -- | The DB cluster identifier. This parameter is stored as a lowercase string.
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
    -- | A list of Availability Zones (AZs) where instances in the DB cluster can be created. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones> in the /Amazon Aurora User Guide/ .
    availabilityZones :: Core.Maybe [Types.String],
    -- | The target backtrack window, in seconds. To disable backtracking, set this value to 0.
    --
    -- Default: 0
    -- Constraints:
    --
    --     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
    backtrackWindow :: Core.Maybe Core.Integer,
    -- | The number of days for which automated backups are retained.
    --
    -- Default: 1
    -- Constraints:
    --
    --     * Must be a value from 1 to 35
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | A value that indicates that the DB cluster should be associated with the specified CharacterSet.
    characterSetName :: Core.Maybe Types.String,
    -- | A value that indicates whether to copy all tags from the DB cluster to snapshots of the DB cluster. The default is not to copy them.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
    -- | The name of the DB cluster parameter group to associate with this DB cluster. If you do not specify a value, then the default DB cluster parameter group for the specified DB engine and version is used.
    --
    -- Constraints:
    --
    --     * If supplied, must match the name of an existing DB cluster parameter group.
    dBClusterParameterGroupName :: Core.Maybe Types.String,
    -- | A DB subnet group to associate with this DB cluster.
    --
    -- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
    -- Example: @mySubnetgroup@
    dBSubnetGroupName :: Core.Maybe Types.String,
    -- | The name for your database of up to 64 alphanumeric characters. If you do not provide a name, Amazon RDS doesn't create a database in the DB cluster you are creating.
    databaseName :: Core.Maybe Types.String,
    -- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The Active Directory directory ID to create the DB cluster in.
    --
    -- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
    domain :: Core.Maybe Types.String,
    -- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
    domainIAMRoleName :: Core.Maybe Types.String,
    -- | The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
    --
    -- __Aurora MySQL__
    -- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
    -- __Aurora PostgreSQL__
    -- Possible values are @postgresql@ and @upgrade@ .
    enableCloudwatchLogsExports :: Core.Maybe [Types.String],
    -- | A value that indicates whether to enable write operations to be forwarded from this cluster to the primary cluster in an Aurora global database. The resulting changes are replicated back to this cluster. This parameter only applies to DB clusters that are secondary clusters in an Aurora global database. By default, Aurora disallows write operations for secondary clusters.
    enableGlobalWriteForwarding :: Core.Maybe Core.Bool,
    -- | A value that indicates whether to enable the HTTP endpoint for an Aurora Serverless DB cluster. By default, the HTTP endpoint is disabled.
    --
    -- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
    enableHttpEndpoint :: Core.Maybe Core.Bool,
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
    -- | The DB engine mode of the DB cluster, either @provisioned@ @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
    --
    -- The @parallelquery@ engine mode isn't required for Aurora MySQL version 1.23 and higher 1.x versions, and version 2.09 and higher 2.x versions.
    -- The @global@ engine mode isn't required for Aurora MySQL version 1.22 and higher 1.x versions, and @global@ engine mode isn't required for any 2.x versions.
    -- The @multimaster@ engine mode only applies for DB clusters created with Aurora MySQL version 5.6.10a.
    -- For Aurora PostgreSQL, the @global@ engine mode isn't required, and both the @parallelquery@ and the @multimaster@ engine modes currently aren't supported.
    -- Limitations and requirements apply to some DB engine modes. For more information, see the following sections in the /Amazon Aurora User Guide/ :
    --
    --     * <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html#aurora-serverless.limitations Limitations of Aurora Serverless>
    --
    --
    --     * <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-mysql-parallel-query.html#aurora-mysql-parallel-query-limitations Limitations of Parallel Query>
    --
    --
    --     * <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-global-database.html#aurora-global-database.limitations Limitations of Aurora Global Databases>
    --
    --
    --     * <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-multi-master.html#aurora-multi-master-limitations Limitations of Multi-Master Clusters>
    engineMode :: Core.Maybe Types.String,
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
    -- | The global cluster ID of an Aurora cluster that becomes the primary cluster in the new global database cluster.
    globalClusterIdentifier :: Core.Maybe Types.String,
    -- | The AWS KMS key identifier for an encrypted DB cluster.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key.
    -- If an encryption key isn't specified in @KmsKeyId@ :
    --
    --     * If @ReplicationSourceIdentifier@ identifies an encrypted source, then Amazon RDS will use the encryption key used to encrypt the source. Otherwise, Amazon RDS will use your default encryption key.
    --
    --
    --     * If the @StorageEncrypted@ parameter is enabled and @ReplicationSourceIdentifier@ isn't specified, then Amazon RDS will use your default encryption key.
    --
    --
    -- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    -- If you create a read replica of an encrypted DB cluster in another AWS Region, you must set @KmsKeyId@ to a KMS key ID that is valid in the destination AWS Region. This key is used to encrypt the read replica in that AWS Region.
    kmsKeyId :: Core.Maybe Types.String,
    -- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Core.Maybe Types.String,
    -- | The name of the master user for the DB cluster.
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
    masterUsername :: Core.Maybe Types.String,
    -- | A value that indicates that the DB cluster should be associated with the specified option group.
    --
    -- Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
    optionGroupName :: Core.Maybe Types.String,
    -- | The port number on which the instances in the DB cluster accept connections.
    --
    -- Default: @3306@ if engine is set as aurora or @5432@ if set to aurora-postgresql.
    port :: Core.Maybe Core.Int,
    -- | A URL that contains a Signature Version 4 signed request for the @CreateDBCluster@ action to be called in the source AWS Region where the DB cluster is replicated from. You only need to specify @PreSignedUrl@ when you are performing cross-region replication from an encrypted DB cluster.
    --
    -- The pre-signed URL must be a valid request for the @CreateDBCluster@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster to be copied.
    -- The pre-signed URL request must contain the following parameter values:
    --
    --     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB cluster in the destination AWS Region. This should refer to the same KMS key for both the @CreateDBCluster@ action that is called in the destination AWS Region, and the action contained in the pre-signed URL.
    --
    --
    --     * @DestinationRegion@ - The name of the AWS Region that Aurora read replica will be created in.
    --
    --
    --     * @ReplicationSourceIdentifier@ - The DB cluster identifier for the encrypted DB cluster to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster from the us-west-2 AWS Region, then your @ReplicationSourceIdentifier@ would look like Example: @arn:aws:rds:us-west-2:123456789012:cluster:aurora-cluster1@ .
    --
    --
    -- To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
    preSignedUrl :: Core.Maybe Types.String,
    -- | The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./
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
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the source DB instance or DB cluster if this DB cluster is created as a read replica.
    replicationSourceIdentifier :: Core.Maybe Types.String,
    -- | For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
    scalingConfiguration :: Core.Maybe Types.ScalingConfiguration,
    -- | A value that indicates whether the DB cluster is encrypted.
    storageEncrypted :: Core.Maybe Core.Bool,
    -- | Tags to assign to the DB cluster.
    tags :: Core.Maybe [Types.Tag],
    -- | A list of EC2 VPC security groups to associate with this DB cluster.
    vpcSecurityGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBCluster' value with any optional fields omitted.
mkCreateDBCluster ::
  -- | 'dBClusterIdentifier'
  Types.String ->
  -- | 'engine'
  Types.String ->
  CreateDBCluster
mkCreateDBCluster dBClusterIdentifier engine =
  CreateDBCluster'
    { dBClusterIdentifier,
      engine,
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
      enableGlobalWriteForwarding = Core.Nothing,
      enableHttpEndpoint = Core.Nothing,
      enableIAMDatabaseAuthentication = Core.Nothing,
      engineMode = Core.Nothing,
      engineVersion = Core.Nothing,
      globalClusterIdentifier = Core.Nothing,
      kmsKeyId = Core.Nothing,
      masterUserPassword = Core.Nothing,
      masterUsername = Core.Nothing,
      optionGroupName = Core.Nothing,
      port = Core.Nothing,
      preSignedUrl = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      replicationSourceIdentifier = Core.Nothing,
      scalingConfiguration = Core.Nothing,
      storageEncrypted = Core.Nothing,
      tags = Core.Nothing,
      vpcSecurityGroupIds = Core.Nothing
    }

-- | The DB cluster identifier. This parameter is stored as a lowercase string.
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
cdbcDBClusterIdentifier :: Lens.Lens' CreateDBCluster Types.String
cdbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED cdbcDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcEngine :: Lens.Lens' CreateDBCluster Types.String
cdbcEngine = Lens.field @"engine"
{-# DEPRECATED cdbcEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | A list of Availability Zones (AZs) where instances in the DB cluster can be created. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcAvailabilityZones :: Lens.Lens' CreateDBCluster (Core.Maybe [Types.String])
cdbcAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED cdbcAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

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
cdbcBacktrackWindow :: Lens.Lens' CreateDBCluster (Core.Maybe Core.Integer)
cdbcBacktrackWindow = Lens.field @"backtrackWindow"
{-# DEPRECATED cdbcBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

-- | The number of days for which automated backups are retained.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 1 to 35
--
--
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcBackupRetentionPeriod :: Lens.Lens' CreateDBCluster (Core.Maybe Core.Int)
cdbcBackupRetentionPeriod = Lens.field @"backupRetentionPeriod"
{-# DEPRECATED cdbcBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | A value that indicates that the DB cluster should be associated with the specified CharacterSet.
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcCharacterSetName :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcCharacterSetName = Lens.field @"characterSetName"
{-# DEPRECATED cdbcCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

-- | A value that indicates whether to copy all tags from the DB cluster to snapshots of the DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcCopyTagsToSnapshot :: Lens.Lens' CreateDBCluster (Core.Maybe Core.Bool)
cdbcCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# DEPRECATED cdbcCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The name of the DB cluster parameter group to associate with this DB cluster. If you do not specify a value, then the default DB cluster parameter group for the specified DB engine and version is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DB cluster parameter group.
--
--
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcDBClusterParameterGroupName :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# DEPRECATED cdbcDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead." #-}

-- | A DB subnet group to associate with this DB cluster.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcDBSubnetGroupName :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED cdbcDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | The name for your database of up to 64 alphanumeric characters. If you do not provide a name, Amazon RDS doesn't create a database in the DB cluster you are creating.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcDatabaseName :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED cdbcDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcDeletionProtection :: Lens.Lens' CreateDBCluster (Core.Maybe Core.Bool)
cdbcDeletionProtection = Lens.field @"deletionProtection"
{-# DEPRECATED cdbcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The Active Directory directory ID to create the DB cluster in.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcDomain :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcDomain = Lens.field @"domain"
{-# DEPRECATED cdbcDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcDomainIAMRoleName :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# DEPRECATED cdbcDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
-- __Aurora MySQL__
-- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
-- __Aurora PostgreSQL__
-- Possible values are @postgresql@ and @upgrade@ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcEnableCloudwatchLogsExports :: Lens.Lens' CreateDBCluster (Core.Maybe [Types.String])
cdbcEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# DEPRECATED cdbcEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

-- | A value that indicates whether to enable write operations to be forwarded from this cluster to the primary cluster in an Aurora global database. The resulting changes are replicated back to this cluster. This parameter only applies to DB clusters that are secondary clusters in an Aurora global database. By default, Aurora disallows write operations for secondary clusters.
--
-- /Note:/ Consider using 'enableGlobalWriteForwarding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcEnableGlobalWriteForwarding :: Lens.Lens' CreateDBCluster (Core.Maybe Core.Bool)
cdbcEnableGlobalWriteForwarding = Lens.field @"enableGlobalWriteForwarding"
{-# DEPRECATED cdbcEnableGlobalWriteForwarding "Use generic-lens or generic-optics with 'enableGlobalWriteForwarding' instead." #-}

-- | A value that indicates whether to enable the HTTP endpoint for an Aurora Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableHttpEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcEnableHttpEndpoint :: Lens.Lens' CreateDBCluster (Core.Maybe Core.Bool)
cdbcEnableHttpEndpoint = Lens.field @"enableHttpEndpoint"
{-# DEPRECATED cdbcEnableHttpEndpoint "Use generic-lens or generic-optics with 'enableHttpEndpoint' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcEnableIAMDatabaseAuthentication :: Lens.Lens' CreateDBCluster (Core.Maybe Core.Bool)
cdbcEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# DEPRECATED cdbcEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | The DB engine mode of the DB cluster, either @provisioned@ @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
--
-- The @parallelquery@ engine mode isn't required for Aurora MySQL version 1.23 and higher 1.x versions, and version 2.09 and higher 2.x versions.
-- The @global@ engine mode isn't required for Aurora MySQL version 1.22 and higher 1.x versions, and @global@ engine mode isn't required for any 2.x versions.
-- The @multimaster@ engine mode only applies for DB clusters created with Aurora MySQL version 5.6.10a.
-- For Aurora PostgreSQL, the @global@ engine mode isn't required, and both the @parallelquery@ and the @multimaster@ engine modes currently aren't supported.
-- Limitations and requirements apply to some DB engine modes. For more information, see the following sections in the /Amazon Aurora User Guide/ :
--
--     * <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html#aurora-serverless.limitations Limitations of Aurora Serverless>
--
--
--     * <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-mysql-parallel-query.html#aurora-mysql-parallel-query-limitations Limitations of Parallel Query>
--
--
--     * <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-global-database.html#aurora-global-database.limitations Limitations of Aurora Global Databases>
--
--
--     * <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-multi-master.html#aurora-multi-master-limitations Limitations of Multi-Master Clusters>
--
--
--
-- /Note:/ Consider using 'engineMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcEngineMode :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcEngineMode = Lens.field @"engineMode"
{-# DEPRECATED cdbcEngineMode "Use generic-lens or generic-optics with 'engineMode' instead." #-}

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
cdbcEngineVersion :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED cdbcEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The global cluster ID of an Aurora cluster that becomes the primary cluster in the new global database cluster.
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcGlobalClusterIdentifier :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# DEPRECATED cdbcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

-- | The AWS KMS key identifier for an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key.
-- If an encryption key isn't specified in @KmsKeyId@ :
--
--     * If @ReplicationSourceIdentifier@ identifies an encrypted source, then Amazon RDS will use the encryption key used to encrypt the source. Otherwise, Amazon RDS will use your default encryption key.
--
--
--     * If the @StorageEncrypted@ parameter is enabled and @ReplicationSourceIdentifier@ isn't specified, then Amazon RDS will use your default encryption key.
--
--
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- If you create a read replica of an encrypted DB cluster in another AWS Region, you must set @KmsKeyId@ to a KMS key ID that is valid in the destination AWS Region. This key is used to encrypt the read replica in that AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcKmsKeyId :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED cdbcKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcMasterUserPassword :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcMasterUserPassword = Lens.field @"masterUserPassword"
{-# DEPRECATED cdbcMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The name of the master user for the DB cluster.
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
cdbcMasterUsername :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcMasterUsername = Lens.field @"masterUsername"
{-# DEPRECATED cdbcMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | A value that indicates that the DB cluster should be associated with the specified option group.
--
-- Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcOptionGroupName :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED cdbcOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The port number on which the instances in the DB cluster accept connections.
--
-- Default: @3306@ if engine is set as aurora or @5432@ if set to aurora-postgresql.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcPort :: Lens.Lens' CreateDBCluster (Core.Maybe Core.Int)
cdbcPort = Lens.field @"port"
{-# DEPRECATED cdbcPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A URL that contains a Signature Version 4 signed request for the @CreateDBCluster@ action to be called in the source AWS Region where the DB cluster is replicated from. You only need to specify @PreSignedUrl@ when you are performing cross-region replication from an encrypted DB cluster.
--
-- The pre-signed URL must be a valid request for the @CreateDBCluster@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster to be copied.
-- The pre-signed URL request must contain the following parameter values:
--
--     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB cluster in the destination AWS Region. This should refer to the same KMS key for both the @CreateDBCluster@ action that is called in the destination AWS Region, and the action contained in the pre-signed URL.
--
--
--     * @DestinationRegion@ - The name of the AWS Region that Aurora read replica will be created in.
--
--
--     * @ReplicationSourceIdentifier@ - The DB cluster identifier for the encrypted DB cluster to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster from the us-west-2 AWS Region, then your @ReplicationSourceIdentifier@ would look like Example: @arn:aws:rds:us-west-2:123456789012:cluster:aurora-cluster1@ .
--
--
-- To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- /Note:/ Consider using 'preSignedUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcPreSignedUrl :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcPreSignedUrl = Lens.field @"preSignedUrl"
{-# DEPRECATED cdbcPreSignedUrl "Use generic-lens or generic-optics with 'preSignedUrl' instead." #-}

-- | The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./
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
cdbcPreferredBackupWindow :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# DEPRECATED cdbcPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcPreferredMaintenanceWindow :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED cdbcPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The Amazon Resource Name (ARN) of the source DB instance or DB cluster if this DB cluster is created as a read replica.
--
-- /Note:/ Consider using 'replicationSourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcReplicationSourceIdentifier :: Lens.Lens' CreateDBCluster (Core.Maybe Types.String)
cdbcReplicationSourceIdentifier = Lens.field @"replicationSourceIdentifier"
{-# DEPRECATED cdbcReplicationSourceIdentifier "Use generic-lens or generic-optics with 'replicationSourceIdentifier' instead." #-}

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
--
-- /Note:/ Consider using 'scalingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcScalingConfiguration :: Lens.Lens' CreateDBCluster (Core.Maybe Types.ScalingConfiguration)
cdbcScalingConfiguration = Lens.field @"scalingConfiguration"
{-# DEPRECATED cdbcScalingConfiguration "Use generic-lens or generic-optics with 'scalingConfiguration' instead." #-}

-- | A value that indicates whether the DB cluster is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcStorageEncrypted :: Lens.Lens' CreateDBCluster (Core.Maybe Core.Bool)
cdbcStorageEncrypted = Lens.field @"storageEncrypted"
{-# DEPRECATED cdbcStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | Tags to assign to the DB cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcTags :: Lens.Lens' CreateDBCluster (Core.Maybe [Types.Tag])
cdbcTags = Lens.field @"tags"
{-# DEPRECATED cdbcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of EC2 VPC security groups to associate with this DB cluster.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcVpcSecurityGroupIds :: Lens.Lens' CreateDBCluster (Core.Maybe [Types.String])
cdbcVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED cdbcVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

instance Core.AWSRequest CreateDBCluster where
  type Rs CreateDBCluster = CreateDBClusterResponse
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
            ( Core.pure ("Action", "CreateDBCluster")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
                Core.<> (Core.toQueryValue "Engine" engine)
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
                Core.<> ( Core.toQueryValue "EnableGlobalWriteForwarding"
                            Core.<$> enableGlobalWriteForwarding
                        )
                Core.<> ( Core.toQueryValue "EnableHttpEndpoint"
                            Core.<$> enableHttpEndpoint
                        )
                Core.<> ( Core.toQueryValue "EnableIAMDatabaseAuthentication"
                            Core.<$> enableIAMDatabaseAuthentication
                        )
                Core.<> (Core.toQueryValue "EngineMode" Core.<$> engineMode)
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> ( Core.toQueryValue "GlobalClusterIdentifier"
                            Core.<$> globalClusterIdentifier
                        )
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> ( Core.toQueryValue "MasterUserPassword"
                            Core.<$> masterUserPassword
                        )
                Core.<> (Core.toQueryValue "MasterUsername" Core.<$> masterUsername)
                Core.<> (Core.toQueryValue "OptionGroupName" Core.<$> optionGroupName)
                Core.<> (Core.toQueryValue "Port" Core.<$> port)
                Core.<> (Core.toQueryValue "PreSignedUrl" Core.<$> preSignedUrl)
                Core.<> ( Core.toQueryValue "PreferredBackupWindow"
                            Core.<$> preferredBackupWindow
                        )
                Core.<> ( Core.toQueryValue "PreferredMaintenanceWindow"
                            Core.<$> preferredMaintenanceWindow
                        )
                Core.<> ( Core.toQueryValue "ReplicationSourceIdentifier"
                            Core.<$> replicationSourceIdentifier
                        )
                Core.<> ( Core.toQueryValue "ScalingConfiguration"
                            Core.<$> scalingConfiguration
                        )
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
      "CreateDBClusterResult"
      ( \s h x ->
          CreateDBClusterResponse'
            Core.<$> (x Core..@? "DBCluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDBClusterResponse' smart constructor.
data CreateDBClusterResponse = CreateDBClusterResponse'
  { dBCluster :: Core.Maybe Types.DBCluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateDBClusterResponse' value with any optional fields omitted.
mkCreateDBClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDBClusterResponse
mkCreateDBClusterResponse responseStatus =
  CreateDBClusterResponse'
    { dBCluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcrrsDBCluster :: Lens.Lens' CreateDBClusterResponse (Core.Maybe Types.DBCluster)
cdbcrrsDBCluster = Lens.field @"dBCluster"
{-# DEPRECATED cdbcrrsDBCluster "Use generic-lens or generic-optics with 'dBCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcrrsResponseStatus :: Lens.Lens' CreateDBClusterResponse Core.Int
cdbcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdbcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
