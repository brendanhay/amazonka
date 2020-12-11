{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RestoreDBClusterToPointInTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a DB cluster to an arbitrary point in time. Users can restore to any point in time before @LatestRestorableTime@ for up to @BackupRetentionPeriod@ days. The target DB cluster is created from the source DB cluster with the same configuration as the original DB cluster, except that the new DB cluster is created with the default DB security group.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.RestoreDBClusterToPointInTime
  ( -- * Creating a request
    RestoreDBClusterToPointInTime (..),
    mkRestoreDBClusterToPointInTime,

    -- ** Request lenses
    rdctpitDeletionProtection,
    rdctpitUseLatestRestorableTime,
    rdctpitDBSubnetGroupName,
    rdctpitDomain,
    rdctpitBacktrackWindow,
    rdctpitKMSKeyId,
    rdctpitVPCSecurityGroupIds,
    rdctpitDBClusterParameterGroupName,
    rdctpitRestoreType,
    rdctpitOptionGroupName,
    rdctpitCopyTagsToSnapshot,
    rdctpitRestoreToTime,
    rdctpitDomainIAMRoleName,
    rdctpitTags,
    rdctpitPort,
    rdctpitEnableIAMDatabaseAuthentication,
    rdctpitEnableCloudwatchLogsExports,
    rdctpitDBClusterIdentifier,
    rdctpitSourceDBClusterIdentifier,

    -- * Destructuring the response
    RestoreDBClusterToPointInTimeResponse (..),
    mkRestoreDBClusterToPointInTimeResponse,

    -- ** Response lenses
    rdctpitrsDBCluster,
    rdctpitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRestoreDBClusterToPointInTime' smart constructor.
data RestoreDBClusterToPointInTime = RestoreDBClusterToPointInTime'
  { deletionProtection ::
      Lude.Maybe Lude.Bool,
    useLatestRestorableTime ::
      Lude.Maybe Lude.Bool,
    dbSubnetGroupName ::
      Lude.Maybe Lude.Text,
    domain :: Lude.Maybe Lude.Text,
    backtrackWindow ::
      Lude.Maybe Lude.Integer,
    kmsKeyId ::
      Lude.Maybe Lude.Text,
    vpcSecurityGroupIds ::
      Lude.Maybe [Lude.Text],
    dbClusterParameterGroupName ::
      Lude.Maybe Lude.Text,
    restoreType ::
      Lude.Maybe Lude.Text,
    optionGroupName ::
      Lude.Maybe Lude.Text,
    copyTagsToSnapshot ::
      Lude.Maybe Lude.Bool,
    restoreToTime ::
      Lude.Maybe Lude.ISO8601,
    domainIAMRoleName ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    port :: Lude.Maybe Lude.Int,
    enableIAMDatabaseAuthentication ::
      Lude.Maybe Lude.Bool,
    enableCloudwatchLogsExports ::
      Lude.Maybe [Lude.Text],
    dbClusterIdentifier ::
      Lude.Text,
    sourceDBClusterIdentifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBClusterToPointInTime' with the minimum fields required to make a request.
--
-- * 'backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set this value to 0.
--
-- Default: 0
-- Constraints:
--
--     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
--
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
-- * 'dbClusterIdentifier' - The name of the new DB cluster to be created.
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
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB cluster. If this argument is omitted, the default DB cluster parameter group for the specified engine is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DB cluster parameter group.
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
-- * 'dbSubnetGroupName' - The DB subnet group name to use for the new DB cluster.
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
-- * 'kmsKeyId' - The AWS KMS key identifier to use when restoring an encrypted DB cluster from an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key.
-- You can restore to a new DB cluster and encrypt the new DB cluster with a KMS key that is different than the KMS key used to encrypt the source DB cluster. The new DB cluster is encrypted with the KMS key identified by the @KmsKeyId@ parameter.
-- If you don't specify a value for the @KmsKeyId@ parameter, then the following occurs:
--
--     * If the DB cluster is encrypted, then the restored DB cluster is encrypted using the KMS key that was used to encrypt the source DB cluster.
--
--
--     * If the DB cluster isn't encrypted, then the restored DB cluster isn't encrypted.
--
--
-- If @DBClusterIdentifier@ refers to a DB cluster that isn't encrypted, then the restore request is rejected.
-- * 'optionGroupName' - The name of the option group for the new DB cluster.
-- * 'port' - The port number on which the new DB cluster accepts connections.
--
-- Constraints: A value from @1150-65535@ .
-- Default: The default port for the engine.
-- * 'restoreToTime' - The date and time to restore the DB cluster to.
--
-- Valid Values: Value must be a time in Universal Coordinated Time (UTC) format
-- Constraints:
--
--     * Must be before the latest restorable time for the DB instance
--
--
--     * Must be specified if @UseLatestRestorableTime@ parameter isn't provided
--
--
--     * Can't be specified if the @UseLatestRestorableTime@ parameter is enabled
--
--
--     * Can't be specified if the @RestoreType@ parameter is @copy-on-write@
--
--
-- Example: @2015-03-07T23:45:00Z@
-- * 'restoreType' - The type of restore to be performed. You can specify one of the following values:
--
--
--     * @full-copy@ - The new DB cluster is restored as a full copy of the source DB cluster.
--
--
--     * @copy-on-write@ - The new DB cluster is restored as a clone of the source DB cluster.
--
--
-- Constraints: You can't specify @copy-on-write@ if the engine version of the source DB cluster is earlier than 1.11.
-- If you don't specify a @RestoreType@ value, then the new DB cluster is restored as a full copy of the source DB cluster.
-- * 'sourceDBClusterIdentifier' - The identifier of the source DB cluster from which to restore.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
-- * 'tags' - Undocumented field.
-- * 'useLatestRestorableTime' - A value that indicates whether to restore the DB cluster to the latest restorable backup time. By default, the DB cluster isn't restored to the latest restorable backup time.
--
-- Constraints: Can't be specified if @RestoreToTime@ parameter is provided.
-- * 'vpcSecurityGroupIds' - A list of VPC security groups that the new DB cluster belongs to.
mkRestoreDBClusterToPointInTime ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'sourceDBClusterIdentifier'
  Lude.Text ->
  RestoreDBClusterToPointInTime
mkRestoreDBClusterToPointInTime
  pDBClusterIdentifier_
  pSourceDBClusterIdentifier_ =
    RestoreDBClusterToPointInTime'
      { deletionProtection = Lude.Nothing,
        useLatestRestorableTime = Lude.Nothing,
        dbSubnetGroupName = Lude.Nothing,
        domain = Lude.Nothing,
        backtrackWindow = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        vpcSecurityGroupIds = Lude.Nothing,
        dbClusterParameterGroupName = Lude.Nothing,
        restoreType = Lude.Nothing,
        optionGroupName = Lude.Nothing,
        copyTagsToSnapshot = Lude.Nothing,
        restoreToTime = Lude.Nothing,
        domainIAMRoleName = Lude.Nothing,
        tags = Lude.Nothing,
        port = Lude.Nothing,
        enableIAMDatabaseAuthentication = Lude.Nothing,
        enableCloudwatchLogsExports = Lude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        sourceDBClusterIdentifier = pSourceDBClusterIdentifier_
      }

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitDeletionProtection :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Bool)
rdctpitDeletionProtection = Lens.lens (deletionProtection :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | A value that indicates whether to restore the DB cluster to the latest restorable backup time. By default, the DB cluster isn't restored to the latest restorable backup time.
--
-- Constraints: Can't be specified if @RestoreToTime@ parameter is provided.
--
-- /Note:/ Consider using 'useLatestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitUseLatestRestorableTime :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Bool)
rdctpitUseLatestRestorableTime = Lens.lens (useLatestRestorableTime :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {useLatestRestorableTime = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitUseLatestRestorableTime "Use generic-lens or generic-optics with 'useLatestRestorableTime' instead." #-}

-- | The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitDBSubnetGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Text)
rdctpitDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitDomain :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Text)
rdctpitDomain = Lens.lens (domain :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

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
rdctpitBacktrackWindow :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Integer)
rdctpitBacktrackWindow = Lens.lens (backtrackWindow :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Integer) (\s a -> s {backtrackWindow = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

-- | The AWS KMS key identifier to use when restoring an encrypted DB cluster from an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key.
-- You can restore to a new DB cluster and encrypt the new DB cluster with a KMS key that is different than the KMS key used to encrypt the source DB cluster. The new DB cluster is encrypted with the KMS key identified by the @KmsKeyId@ parameter.
-- If you don't specify a value for the @KmsKeyId@ parameter, then the following occurs:
--
--     * If the DB cluster is encrypted, then the restored DB cluster is encrypted using the KMS key that was used to encrypt the source DB cluster.
--
--
--     * If the DB cluster isn't encrypted, then the restored DB cluster isn't encrypted.
--
--
-- If @DBClusterIdentifier@ refers to a DB cluster that isn't encrypted, then the restore request is rejected.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitKMSKeyId :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Text)
rdctpitKMSKeyId = Lens.lens (kmsKeyId :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A list of VPC security groups that the new DB cluster belongs to.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitVPCSecurityGroupIds :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe [Lude.Text])
rdctpitVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: RestoreDBClusterToPointInTime -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | The name of the DB cluster parameter group to associate with this DB cluster. If this argument is omitted, the default DB cluster parameter group for the specified engine is used.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DB cluster parameter group.
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
rdctpitDBClusterParameterGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Text)
rdctpitDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

-- | The type of restore to be performed. You can specify one of the following values:
--
--
--     * @full-copy@ - The new DB cluster is restored as a full copy of the source DB cluster.
--
--
--     * @copy-on-write@ - The new DB cluster is restored as a clone of the source DB cluster.
--
--
-- Constraints: You can't specify @copy-on-write@ if the engine version of the source DB cluster is earlier than 1.11.
-- If you don't specify a @RestoreType@ value, then the new DB cluster is restored as a full copy of the source DB cluster.
--
-- /Note:/ Consider using 'restoreType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitRestoreType :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Text)
rdctpitRestoreType = Lens.lens (restoreType :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {restoreType = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitRestoreType "Use generic-lens or generic-optics with 'restoreType' instead." #-}

-- | The name of the option group for the new DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitOptionGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Text)
rdctpitOptionGroupName = Lens.lens (optionGroupName :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitCopyTagsToSnapshot :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Bool)
rdctpitCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The date and time to restore the DB cluster to.
--
-- Valid Values: Value must be a time in Universal Coordinated Time (UTC) format
-- Constraints:
--
--     * Must be before the latest restorable time for the DB instance
--
--
--     * Must be specified if @UseLatestRestorableTime@ parameter isn't provided
--
--
--     * Can't be specified if the @UseLatestRestorableTime@ parameter is enabled
--
--
--     * Can't be specified if the @RestoreType@ parameter is @copy-on-write@
--
--
-- Example: @2015-03-07T23:45:00Z@
--
-- /Note:/ Consider using 'restoreToTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitRestoreToTime :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.ISO8601)
rdctpitRestoreToTime = Lens.lens (restoreToTime :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.ISO8601) (\s a -> s {restoreToTime = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitRestoreToTime "Use generic-lens or generic-optics with 'restoreToTime' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitDomainIAMRoleName :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Text)
rdctpitDomainIAMRoleName = Lens.lens (domainIAMRoleName :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitTags :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe [Tag])
rdctpitTags = Lens.lens (tags :: RestoreDBClusterToPointInTime -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: A value from @1150-65535@ .
-- Default: The default port for the engine.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitPort :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Int)
rdctpitPort = Lens.lens (port :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe Lude.Bool)
rdctpitEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: RestoreDBClusterToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | The list of logs that the restored DB cluster is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterToPointInTime (Lude.Maybe [Lude.Text])
rdctpitEnableCloudwatchLogsExports = Lens.lens (enableCloudwatchLogsExports :: RestoreDBClusterToPointInTime -> Lude.Maybe [Lude.Text]) (\s a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

-- | The name of the new DB cluster to be created.
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
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitDBClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Lude.Text
rdctpitDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: RestoreDBClusterToPointInTime -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The identifier of the source DB cluster from which to restore.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
--
-- /Note:/ Consider using 'sourceDBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitSourceDBClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Lude.Text
rdctpitSourceDBClusterIdentifier = Lens.lens (sourceDBClusterIdentifier :: RestoreDBClusterToPointInTime -> Lude.Text) (\s a -> s {sourceDBClusterIdentifier = a} :: RestoreDBClusterToPointInTime)
{-# DEPRECATED rdctpitSourceDBClusterIdentifier "Use generic-lens or generic-optics with 'sourceDBClusterIdentifier' instead." #-}

instance Lude.AWSRequest RestoreDBClusterToPointInTime where
  type
    Rs RestoreDBClusterToPointInTime =
      RestoreDBClusterToPointInTimeResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RestoreDBClusterToPointInTimeResult"
      ( \s h x ->
          RestoreDBClusterToPointInTimeResponse'
            Lude.<$> (x Lude..@? "DBCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreDBClusterToPointInTime where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestoreDBClusterToPointInTime where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreDBClusterToPointInTime where
  toQuery RestoreDBClusterToPointInTime' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RestoreDBClusterToPointInTime" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DeletionProtection" Lude.=: deletionProtection,
        "UseLatestRestorableTime" Lude.=: useLatestRestorableTime,
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "Domain" Lude.=: domain,
        "BacktrackWindow" Lude.=: backtrackWindow,
        "KmsKeyId" Lude.=: kmsKeyId,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "DBClusterParameterGroupName" Lude.=: dbClusterParameterGroupName,
        "RestoreType" Lude.=: restoreType,
        "OptionGroupName" Lude.=: optionGroupName,
        "CopyTagsToSnapshot" Lude.=: copyTagsToSnapshot,
        "RestoreToTime" Lude.=: restoreToTime,
        "DomainIAMRoleName" Lude.=: domainIAMRoleName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "EnableIAMDatabaseAuthentication"
          Lude.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> enableCloudwatchLogsExports),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "SourceDBClusterIdentifier" Lude.=: sourceDBClusterIdentifier
      ]

-- | /See:/ 'mkRestoreDBClusterToPointInTimeResponse' smart constructor.
data RestoreDBClusterToPointInTimeResponse = RestoreDBClusterToPointInTimeResponse'
  { dbCluster ::
      Lude.Maybe
        DBCluster,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreDBClusterToPointInTimeResponse' with the minimum fields required to make a request.
--
-- * 'dbCluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkRestoreDBClusterToPointInTimeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreDBClusterToPointInTimeResponse
mkRestoreDBClusterToPointInTimeResponse pResponseStatus_ =
  RestoreDBClusterToPointInTimeResponse'
    { dbCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitrsDBCluster :: Lens.Lens' RestoreDBClusterToPointInTimeResponse (Lude.Maybe DBCluster)
rdctpitrsDBCluster = Lens.lens (dbCluster :: RestoreDBClusterToPointInTimeResponse -> Lude.Maybe DBCluster) (\s a -> s {dbCluster = a} :: RestoreDBClusterToPointInTimeResponse)
{-# DEPRECATED rdctpitrsDBCluster "Use generic-lens or generic-optics with 'dbCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdctpitrsResponseStatus :: Lens.Lens' RestoreDBClusterToPointInTimeResponse Lude.Int
rdctpitrsResponseStatus = Lens.lens (responseStatus :: RestoreDBClusterToPointInTimeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreDBClusterToPointInTimeResponse)
{-# DEPRECATED rdctpitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
