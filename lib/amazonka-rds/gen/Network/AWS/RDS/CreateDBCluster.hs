{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cdcEngineVersion,
    cdcEnableGlobalWriteForwarding,
    cdcDeletionProtection,
    cdcStorageEncrypted,
    cdcMasterUserPassword,
    cdcReplicationSourceIdentifier,
    cdcEnableHTTPEndpoint,
    cdcGlobalClusterIdentifier,
    cdcMasterUsername,
    cdcDBSubnetGroupName,
    cdcDomain,
    cdcBacktrackWindow,
    cdcPreSignedURL,
    cdcPreferredMaintenanceWindow,
    cdcAvailabilityZones,
    cdcCharacterSetName,
    cdcKMSKeyId,
    cdcPreferredBackupWindow,
    cdcBackupRetentionPeriod,
    cdcVPCSecurityGroupIds,
    cdcDatabaseName,
    cdcDBClusterParameterGroupName,
    cdcEngineMode,
    cdcScalingConfiguration,
    cdcOptionGroupName,
    cdcCopyTagsToSnapshot,
    cdcDomainIAMRoleName,
    cdcTags,
    cdcPort,
    cdcEnableIAMDatabaseAuthentication,
    cdcEnableCloudwatchLogsExports,
    cdcDBClusterIdentifier,
    cdcEngine,

    -- * Destructuring the response
    CreateDBClusterResponse (..),
    mkCreateDBClusterResponse,

    -- ** Response lenses
    cdcrsDBCluster,
    cdcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateDBCluster' smart constructor.
data CreateDBCluster = CreateDBCluster'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    enableGlobalWriteForwarding :: Lude.Maybe Lude.Bool,
    deletionProtection :: Lude.Maybe Lude.Bool,
    storageEncrypted :: Lude.Maybe Lude.Bool,
    masterUserPassword :: Lude.Maybe Lude.Text,
    replicationSourceIdentifier :: Lude.Maybe Lude.Text,
    enableHTTPEndpoint :: Lude.Maybe Lude.Bool,
    globalClusterIdentifier :: Lude.Maybe Lude.Text,
    masterUsername :: Lude.Maybe Lude.Text,
    dbSubnetGroupName :: Lude.Maybe Lude.Text,
    domain :: Lude.Maybe Lude.Text,
    backtrackWindow :: Lude.Maybe Lude.Integer,
    preSignedURL :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    availabilityZones :: Lude.Maybe [Lude.Text],
    characterSetName :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    databaseName :: Lude.Maybe Lude.Text,
    dbClusterParameterGroupName :: Lude.Maybe Lude.Text,
    engineMode :: Lude.Maybe Lude.Text,
    scalingConfiguration :: Lude.Maybe ScalingConfiguration,
    optionGroupName :: Lude.Maybe Lude.Text,
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    domainIAMRoleName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    port :: Lude.Maybe Lude.Int,
    enableIAMDatabaseAuthentication :: Lude.Maybe Lude.Bool,
    enableCloudwatchLogsExports :: Lude.Maybe [Lude.Text],
    dbClusterIdentifier :: Lude.Text,
    engine :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBCluster' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - A list of Availability Zones (AZs) where instances in the DB cluster can be created. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones> in the /Amazon Aurora User Guide/ .
-- * 'backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set this value to 0.
--
-- Default: 0
-- Constraints:
--
--     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
--
-- * 'backupRetentionPeriod' - The number of days for which automated backups are retained.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 1 to 35
--
--
-- * 'characterSetName' - A value that indicates that the DB cluster should be associated with the specified CharacterSet.
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB cluster to snapshots of the DB cluster. The default is not to copy them.
-- * 'databaseName' - The name for your database of up to 64 alphanumeric characters. If you do not provide a name, Amazon RDS doesn't create a database in the DB cluster you are creating.
-- * 'dbClusterIdentifier' - The DB cluster identifier. This parameter is stored as a lowercase string.
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
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB cluster. If you do not specify a value, then the default DB cluster parameter group for the specified DB engine and version is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DB cluster parameter group.
--
--
-- * 'dbSubnetGroupName' - A DB subnet group to associate with this DB cluster.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@
-- * 'deletionProtection' - A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
-- * 'domain' - The Active Directory directory ID to create the DB cluster in.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
-- * 'domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
-- * 'enableCloudwatchLogsExports' - The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
-- __Aurora MySQL__
-- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
-- __Aurora PostgreSQL__
-- Possible values are @postgresql@ and @upgrade@ .
-- * 'enableGlobalWriteForwarding' - A value that indicates whether to enable write operations to be forwarded from this cluster to the primary cluster in an Aurora global database. The resulting changes are replicated back to this cluster. This parameter only applies to DB clusters that are secondary clusters in an Aurora global database. By default, Aurora disallows write operations for secondary clusters.
-- * 'enableHTTPEndpoint' - A value that indicates whether to enable the HTTP endpoint for an Aurora Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
-- * 'enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
-- * 'engine' - The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
-- * 'engineMode' - The DB engine mode of the DB cluster, either @provisioned@ @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
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
-- * 'globalClusterIdentifier' - The global cluster ID of an Aurora cluster that becomes the primary cluster in the new global database cluster.
-- * 'kmsKeyId' - The AWS KMS key identifier for an encrypted DB cluster.
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
-- * 'masterUserPassword' - The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
-- * 'masterUsername' - The name of the master user for the DB cluster.
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
-- * 'optionGroupName' - A value that indicates that the DB cluster should be associated with the specified option group.
--
-- Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
-- * 'port' - The port number on which the instances in the DB cluster accept connections.
--
-- Default: @3306@ if engine is set as aurora or @5432@ if set to aurora-postgresql.
-- * 'preSignedURL' - A URL that contains a Signature Version 4 signed request for the @CreateDBCluster@ action to be called in the source AWS Region where the DB cluster is replicated from. You only need to specify @PreSignedUrl@ when you are performing cross-region replication from an encrypted DB cluster.
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
-- * 'preferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.
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
-- * 'preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
-- * 'replicationSourceIdentifier' - The Amazon Resource Name (ARN) of the source DB instance or DB cluster if this DB cluster is created as a read replica.
-- * 'scalingConfiguration' - For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
-- * 'storageEncrypted' - A value that indicates whether the DB cluster is encrypted.
-- * 'tags' - Tags to assign to the DB cluster.
-- * 'vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB cluster.
mkCreateDBCluster ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'engine'
  Lude.Text ->
  CreateDBCluster
mkCreateDBCluster pDBClusterIdentifier_ pEngine_ =
  CreateDBCluster'
    { engineVersion = Lude.Nothing,
      enableGlobalWriteForwarding = Lude.Nothing,
      deletionProtection = Lude.Nothing,
      storageEncrypted = Lude.Nothing,
      masterUserPassword = Lude.Nothing,
      replicationSourceIdentifier = Lude.Nothing,
      enableHTTPEndpoint = Lude.Nothing,
      globalClusterIdentifier = Lude.Nothing,
      masterUsername = Lude.Nothing,
      dbSubnetGroupName = Lude.Nothing,
      domain = Lude.Nothing,
      backtrackWindow = Lude.Nothing,
      preSignedURL = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      characterSetName = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      preferredBackupWindow = Lude.Nothing,
      backupRetentionPeriod = Lude.Nothing,
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
      enableCloudwatchLogsExports = Lude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_,
      engine = pEngine_
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
cdcEngineVersion :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcEngineVersion = Lens.lens (engineVersion :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CreateDBCluster)
{-# DEPRECATED cdcEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A value that indicates whether to enable write operations to be forwarded from this cluster to the primary cluster in an Aurora global database. The resulting changes are replicated back to this cluster. This parameter only applies to DB clusters that are secondary clusters in an Aurora global database. By default, Aurora disallows write operations for secondary clusters.
--
-- /Note:/ Consider using 'enableGlobalWriteForwarding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcEnableGlobalWriteForwarding :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Bool)
cdcEnableGlobalWriteForwarding = Lens.lens (enableGlobalWriteForwarding :: CreateDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {enableGlobalWriteForwarding = a} :: CreateDBCluster)
{-# DEPRECATED cdcEnableGlobalWriteForwarding "Use generic-lens or generic-optics with 'enableGlobalWriteForwarding' instead." #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDeletionProtection :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Bool)
cdcDeletionProtection = Lens.lens (deletionProtection :: CreateDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: CreateDBCluster)
{-# DEPRECATED cdcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | A value that indicates whether the DB cluster is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcStorageEncrypted :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Bool)
cdcStorageEncrypted = Lens.lens (storageEncrypted :: CreateDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {storageEncrypted = a} :: CreateDBCluster)
{-# DEPRECATED cdcStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcMasterUserPassword :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcMasterUserPassword = Lens.lens (masterUserPassword :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {masterUserPassword = a} :: CreateDBCluster)
{-# DEPRECATED cdcMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The Amazon Resource Name (ARN) of the source DB instance or DB cluster if this DB cluster is created as a read replica.
--
-- /Note:/ Consider using 'replicationSourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcReplicationSourceIdentifier :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcReplicationSourceIdentifier = Lens.lens (replicationSourceIdentifier :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {replicationSourceIdentifier = a} :: CreateDBCluster)
{-# DEPRECATED cdcReplicationSourceIdentifier "Use generic-lens or generic-optics with 'replicationSourceIdentifier' instead." #-}

-- | A value that indicates whether to enable the HTTP endpoint for an Aurora Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableHTTPEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcEnableHTTPEndpoint :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Bool)
cdcEnableHTTPEndpoint = Lens.lens (enableHTTPEndpoint :: CreateDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {enableHTTPEndpoint = a} :: CreateDBCluster)
{-# DEPRECATED cdcEnableHTTPEndpoint "Use generic-lens or generic-optics with 'enableHTTPEndpoint' instead." #-}

-- | The global cluster ID of an Aurora cluster that becomes the primary cluster in the new global database cluster.
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcGlobalClusterIdentifier :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcGlobalClusterIdentifier = Lens.lens (globalClusterIdentifier :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {globalClusterIdentifier = a} :: CreateDBCluster)
{-# DEPRECATED cdcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

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
cdcMasterUsername :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcMasterUsername = Lens.lens (masterUsername :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: CreateDBCluster)
{-# DEPRECATED cdcMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | A DB subnet group to associate with this DB cluster.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDBSubnetGroupName :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: CreateDBCluster)
{-# DEPRECATED cdcDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | The Active Directory directory ID to create the DB cluster in.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDomain :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcDomain = Lens.lens (domain :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: CreateDBCluster)
{-# DEPRECATED cdcDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

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
cdcBacktrackWindow :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Integer)
cdcBacktrackWindow = Lens.lens (backtrackWindow :: CreateDBCluster -> Lude.Maybe Lude.Integer) (\s a -> s {backtrackWindow = a} :: CreateDBCluster)
{-# DEPRECATED cdcBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

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
-- /Note:/ Consider using 'preSignedURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcPreSignedURL :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcPreSignedURL = Lens.lens (preSignedURL :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {preSignedURL = a} :: CreateDBCluster)
{-# DEPRECATED cdcPreSignedURL "Use generic-lens or generic-optics with 'preSignedURL' instead." #-}

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcPreferredMaintenanceWindow :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: CreateDBCluster)
{-# DEPRECATED cdcPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | A list of Availability Zones (AZs) where instances in the DB cluster can be created. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcAvailabilityZones :: Lens.Lens' CreateDBCluster (Lude.Maybe [Lude.Text])
cdcAvailabilityZones = Lens.lens (availabilityZones :: CreateDBCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: CreateDBCluster)
{-# DEPRECATED cdcAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A value that indicates that the DB cluster should be associated with the specified CharacterSet.
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcCharacterSetName :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcCharacterSetName = Lens.lens (characterSetName :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {characterSetName = a} :: CreateDBCluster)
{-# DEPRECATED cdcCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

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
cdcKMSKeyId :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcKMSKeyId = Lens.lens (kmsKeyId :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateDBCluster)
{-# DEPRECATED cdcKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

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
cdcPreferredBackupWindow :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcPreferredBackupWindow = Lens.lens (preferredBackupWindow :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: CreateDBCluster)
{-# DEPRECATED cdcPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

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
cdcBackupRetentionPeriod :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Int)
cdcBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: CreateDBCluster -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: CreateDBCluster)
{-# DEPRECATED cdcBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | A list of EC2 VPC security groups to associate with this DB cluster.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcVPCSecurityGroupIds :: Lens.Lens' CreateDBCluster (Lude.Maybe [Lude.Text])
cdcVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: CreateDBCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: CreateDBCluster)
{-# DEPRECATED cdcVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | The name for your database of up to 64 alphanumeric characters. If you do not provide a name, Amazon RDS doesn't create a database in the DB cluster you are creating.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDatabaseName :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcDatabaseName = Lens.lens (databaseName :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: CreateDBCluster)
{-# DEPRECATED cdcDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the DB cluster parameter group to associate with this DB cluster. If you do not specify a value, then the default DB cluster parameter group for the specified DB engine and version is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DB cluster parameter group.
--
--
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDBClusterParameterGroupName :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: CreateDBCluster)
{-# DEPRECATED cdcDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

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
cdcEngineMode :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcEngineMode = Lens.lens (engineMode :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {engineMode = a} :: CreateDBCluster)
{-# DEPRECATED cdcEngineMode "Use generic-lens or generic-optics with 'engineMode' instead." #-}

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
--
-- /Note:/ Consider using 'scalingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcScalingConfiguration :: Lens.Lens' CreateDBCluster (Lude.Maybe ScalingConfiguration)
cdcScalingConfiguration = Lens.lens (scalingConfiguration :: CreateDBCluster -> Lude.Maybe ScalingConfiguration) (\s a -> s {scalingConfiguration = a} :: CreateDBCluster)
{-# DEPRECATED cdcScalingConfiguration "Use generic-lens or generic-optics with 'scalingConfiguration' instead." #-}

-- | A value that indicates that the DB cluster should be associated with the specified option group.
--
-- Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcOptionGroupName :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcOptionGroupName = Lens.lens (optionGroupName :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: CreateDBCluster)
{-# DEPRECATED cdcOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the DB cluster to snapshots of the DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcCopyTagsToSnapshot :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Bool)
cdcCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: CreateDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: CreateDBCluster)
{-# DEPRECATED cdcCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDomainIAMRoleName :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Text)
cdcDomainIAMRoleName = Lens.lens (domainIAMRoleName :: CreateDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: CreateDBCluster)
{-# DEPRECATED cdcDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | Tags to assign to the DB cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcTags :: Lens.Lens' CreateDBCluster (Lude.Maybe [Tag])
cdcTags = Lens.lens (tags :: CreateDBCluster -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBCluster)
{-# DEPRECATED cdcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which the instances in the DB cluster accept connections.
--
-- Default: @3306@ if engine is set as aurora or @5432@ if set to aurora-postgresql.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcPort :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Int)
cdcPort = Lens.lens (port :: CreateDBCluster -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: CreateDBCluster)
{-# DEPRECATED cdcPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcEnableIAMDatabaseAuthentication :: Lens.Lens' CreateDBCluster (Lude.Maybe Lude.Bool)
cdcEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: CreateDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBCluster)
{-# DEPRECATED cdcEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
-- __Aurora MySQL__
-- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
-- __Aurora PostgreSQL__
-- Possible values are @postgresql@ and @upgrade@ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcEnableCloudwatchLogsExports :: Lens.Lens' CreateDBCluster (Lude.Maybe [Lude.Text])
cdcEnableCloudwatchLogsExports = Lens.lens (enableCloudwatchLogsExports :: CreateDBCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {enableCloudwatchLogsExports = a} :: CreateDBCluster)
{-# DEPRECATED cdcEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

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
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDBClusterIdentifier :: Lens.Lens' CreateDBCluster Lude.Text
cdcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: CreateDBCluster -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: CreateDBCluster)
{-# DEPRECATED cdcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcEngine :: Lens.Lens' CreateDBCluster Lude.Text
cdcEngine = Lens.lens (engine :: CreateDBCluster -> Lude.Text) (\s a -> s {engine = a} :: CreateDBCluster)
{-# DEPRECATED cdcEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

instance Lude.AWSRequest CreateDBCluster where
  type Rs CreateDBCluster = CreateDBClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBClusterResult"
      ( \s h x ->
          CreateDBClusterResponse'
            Lude.<$> (x Lude..@? "DBCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDBCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBCluster where
  toQuery CreateDBCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDBCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "EnableGlobalWriteForwarding" Lude.=: enableGlobalWriteForwarding,
        "DeletionProtection" Lude.=: deletionProtection,
        "StorageEncrypted" Lude.=: storageEncrypted,
        "MasterUserPassword" Lude.=: masterUserPassword,
        "ReplicationSourceIdentifier" Lude.=: replicationSourceIdentifier,
        "EnableHttpEndpoint" Lude.=: enableHTTPEndpoint,
        "GlobalClusterIdentifier" Lude.=: globalClusterIdentifier,
        "MasterUsername" Lude.=: masterUsername,
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "Domain" Lude.=: domain,
        "BacktrackWindow" Lude.=: backtrackWindow,
        "PreSignedUrl" Lude.=: preSignedURL,
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
            (Lude.toQueryList "member" Lude.<$> enableCloudwatchLogsExports),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "Engine" Lude.=: engine
      ]

-- | /See:/ 'mkCreateDBClusterResponse' smart constructor.
data CreateDBClusterResponse = CreateDBClusterResponse'
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

-- | Creates a value of 'CreateDBClusterResponse' with the minimum fields required to make a request.
--
-- * 'dbCluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateDBClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDBClusterResponse
mkCreateDBClusterResponse pResponseStatus_ =
  CreateDBClusterResponse'
    { dbCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsDBCluster :: Lens.Lens' CreateDBClusterResponse (Lude.Maybe DBCluster)
cdcrsDBCluster = Lens.lens (dbCluster :: CreateDBClusterResponse -> Lude.Maybe DBCluster) (\s a -> s {dbCluster = a} :: CreateDBClusterResponse)
{-# DEPRECATED cdcrsDBCluster "Use generic-lens or generic-optics with 'dbCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsResponseStatus :: Lens.Lens' CreateDBClusterResponse Lude.Int
cdcrsResponseStatus = Lens.lens (responseStatus :: CreateDBClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDBClusterResponse)
{-# DEPRECATED cdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
