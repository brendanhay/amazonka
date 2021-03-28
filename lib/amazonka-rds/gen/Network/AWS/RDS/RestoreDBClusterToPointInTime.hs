{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RestoreDBClusterToPointInTime (..)
    , mkRestoreDBClusterToPointInTime
    -- ** Request lenses
    , rdbctpitDBClusterIdentifier
    , rdbctpitSourceDBClusterIdentifier
    , rdbctpitBacktrackWindow
    , rdbctpitCopyTagsToSnapshot
    , rdbctpitDBClusterParameterGroupName
    , rdbctpitDBSubnetGroupName
    , rdbctpitDeletionProtection
    , rdbctpitDomain
    , rdbctpitDomainIAMRoleName
    , rdbctpitEnableCloudwatchLogsExports
    , rdbctpitEnableIAMDatabaseAuthentication
    , rdbctpitKmsKeyId
    , rdbctpitOptionGroupName
    , rdbctpitPort
    , rdbctpitRestoreToTime
    , rdbctpitRestoreType
    , rdbctpitTags
    , rdbctpitUseLatestRestorableTime
    , rdbctpitVpcSecurityGroupIds

    -- * Destructuring the response
    , RestoreDBClusterToPointInTimeResponse (..)
    , mkRestoreDBClusterToPointInTimeResponse
    -- ** Response lenses
    , rdbctpitrrsDBCluster
    , rdbctpitrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRestoreDBClusterToPointInTime' smart constructor.
data RestoreDBClusterToPointInTime = RestoreDBClusterToPointInTime'
  { dBClusterIdentifier :: Core.Text
    -- ^ The name of the new DB cluster to be created.
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
  , sourceDBClusterIdentifier :: Core.Text
    -- ^ The identifier of the source DB cluster from which to restore.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
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
  , dBSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@ 
  , deletionProtection :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. 
  , domain :: Core.Maybe Core.Text
    -- ^ Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation. 
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ . 
  , domainIAMRoleName :: Core.Maybe Core.Text
    -- ^ Specify the name of the IAM role to be used when making API calls to the Directory Service.
  , enableCloudwatchLogsExports :: Core.Maybe [Core.Text]
    -- ^ The list of logs that the restored DB cluster is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
  , enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./ 
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS KMS key identifier to use when restoring an encrypted DB cluster from an encrypted DB cluster.
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
  , optionGroupName :: Core.Maybe Core.Text
    -- ^ The name of the option group for the new DB cluster.
  , port :: Core.Maybe Core.Int
    -- ^ The port number on which the new DB cluster accepts connections.
--
-- Constraints: A value from @1150-65535@ . 
-- Default: The default port for the engine.
  , restoreToTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time to restore the DB cluster to.
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
  , restoreType :: Core.Maybe Core.Text
    -- ^ The type of restore to be performed. You can specify one of the following values:
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
  , tags :: Core.Maybe [Types.Tag]
  , useLatestRestorableTime :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to restore the DB cluster to the latest restorable backup time. By default, the DB cluster isn't restored to the latest restorable backup time. 
--
-- Constraints: Can't be specified if @RestoreToTime@ parameter is provided.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of VPC security groups that the new DB cluster belongs to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreDBClusterToPointInTime' value with any optional fields omitted.
mkRestoreDBClusterToPointInTime
    :: Core.Text -- ^ 'dBClusterIdentifier'
    -> Core.Text -- ^ 'sourceDBClusterIdentifier'
    -> RestoreDBClusterToPointInTime
mkRestoreDBClusterToPointInTime dBClusterIdentifier
  sourceDBClusterIdentifier
  = RestoreDBClusterToPointInTime'{dBClusterIdentifier,
                                   sourceDBClusterIdentifier, backtrackWindow = Core.Nothing,
                                   copyTagsToSnapshot = Core.Nothing,
                                   dBClusterParameterGroupName = Core.Nothing,
                                   dBSubnetGroupName = Core.Nothing,
                                   deletionProtection = Core.Nothing, domain = Core.Nothing,
                                   domainIAMRoleName = Core.Nothing,
                                   enableCloudwatchLogsExports = Core.Nothing,
                                   enableIAMDatabaseAuthentication = Core.Nothing,
                                   kmsKeyId = Core.Nothing, optionGroupName = Core.Nothing,
                                   port = Core.Nothing, restoreToTime = Core.Nothing,
                                   restoreType = Core.Nothing, tags = Core.Nothing,
                                   useLatestRestorableTime = Core.Nothing,
                                   vpcSecurityGroupIds = Core.Nothing}

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
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitDBClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Core.Text
rdbctpitDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE rdbctpitDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | The identifier of the source DB cluster from which to restore.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
--
-- /Note:/ Consider using 'sourceDBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitSourceDBClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Core.Text
rdbctpitSourceDBClusterIdentifier = Lens.field @"sourceDBClusterIdentifier"
{-# INLINEABLE rdbctpitSourceDBClusterIdentifier #-}
{-# DEPRECATED sourceDBClusterIdentifier "Use generic-lens or generic-optics with 'sourceDBClusterIdentifier' instead"  #-}

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
rdbctpitBacktrackWindow :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Integer)
rdbctpitBacktrackWindow = Lens.field @"backtrackWindow"
{-# INLINEABLE rdbctpitBacktrackWindow #-}
{-# DEPRECATED backtrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead"  #-}

-- | A value that indicates whether to copy all tags from the restored DB cluster to snapshots of the restored DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitCopyTagsToSnapshot :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Bool)
rdbctpitCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# INLINEABLE rdbctpitCopyTagsToSnapshot #-}
{-# DEPRECATED copyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead"  #-}

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
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitDBClusterParameterGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
rdbctpitDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# INLINEABLE rdbctpitDBClusterParameterGroupName #-}
{-# DEPRECATED dBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead"  #-}

-- | The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing DBSubnetGroup.
-- Example: @mySubnetgroup@ 
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitDBSubnetGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
rdbctpitDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# INLINEABLE rdbctpitDBSubnetGroupName #-}
{-# DEPRECATED dBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead"  #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. 
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitDeletionProtection :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Bool)
rdbctpitDeletionProtection = Lens.field @"deletionProtection"
{-# INLINEABLE rdbctpitDeletionProtection #-}
{-# DEPRECATED deletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead"  #-}

-- | Specify the Active Directory directory ID to restore the DB cluster in. The domain must be created prior to this operation. 
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ . 
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitDomain :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
rdbctpitDomain = Lens.field @"domain"
{-# INLINEABLE rdbctpitDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitDomainIAMRoleName :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
rdbctpitDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# INLINEABLE rdbctpitDomainIAMRoleName #-}
{-# DEPRECATED domainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead"  #-}

-- | The list of logs that the restored DB cluster is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitEnableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe [Core.Text])
rdbctpitEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# INLINEABLE rdbctpitEnableCloudwatchLogsExports #-}
{-# DEPRECATED enableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead"  #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./ 
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitEnableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Bool)
rdbctpitEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# INLINEABLE rdbctpitEnableIAMDatabaseAuthentication #-}
{-# DEPRECATED enableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead"  #-}

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
rdbctpitKmsKeyId :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
rdbctpitKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE rdbctpitKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The name of the option group for the new DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitOptionGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
rdbctpitOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE rdbctpitOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: A value from @1150-65535@ . 
-- Default: The default port for the engine.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitPort :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Int)
rdbctpitPort = Lens.field @"port"
{-# INLINEABLE rdbctpitPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

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
rdbctpitRestoreToTime :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.UTCTime)
rdbctpitRestoreToTime = Lens.field @"restoreToTime"
{-# INLINEABLE rdbctpitRestoreToTime #-}
{-# DEPRECATED restoreToTime "Use generic-lens or generic-optics with 'restoreToTime' instead"  #-}

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
rdbctpitRestoreType :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Text)
rdbctpitRestoreType = Lens.field @"restoreType"
{-# INLINEABLE rdbctpitRestoreType #-}
{-# DEPRECATED restoreType "Use generic-lens or generic-optics with 'restoreType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitTags :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe [Types.Tag])
rdbctpitTags = Lens.field @"tags"
{-# INLINEABLE rdbctpitTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A value that indicates whether to restore the DB cluster to the latest restorable backup time. By default, the DB cluster isn't restored to the latest restorable backup time. 
--
-- Constraints: Can't be specified if @RestoreToTime@ parameter is provided.
--
-- /Note:/ Consider using 'useLatestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitUseLatestRestorableTime :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe Core.Bool)
rdbctpitUseLatestRestorableTime = Lens.field @"useLatestRestorableTime"
{-# INLINEABLE rdbctpitUseLatestRestorableTime #-}
{-# DEPRECATED useLatestRestorableTime "Use generic-lens or generic-optics with 'useLatestRestorableTime' instead"  #-}

-- | A list of VPC security groups that the new DB cluster belongs to.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitVpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterToPointInTime (Core.Maybe [Core.Text])
rdbctpitVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE rdbctpitVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery RestoreDBClusterToPointInTime where
        toQuery RestoreDBClusterToPointInTime{..}
          = Core.toQueryPair "Action"
              ("RestoreDBClusterToPointInTime" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier
              Core.<>
              Core.toQueryPair "SourceDBClusterIdentifier"
                sourceDBClusterIdentifier
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
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OptionGroupName")
                optionGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RestoreToTime")
                restoreToTime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RestoreType") restoreType
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UseLatestRestorableTime")
                useLatestRestorableTime
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupIds)

instance Core.ToHeaders RestoreDBClusterToPointInTime where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RestoreDBClusterToPointInTime where
        type Rs RestoreDBClusterToPointInTime =
             RestoreDBClusterToPointInTimeResponse
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
          = Response.receiveXMLWrapper "RestoreDBClusterToPointInTimeResult"
              (\ s h x ->
                 RestoreDBClusterToPointInTimeResponse' Core.<$>
                   (x Core..@? "DBCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreDBClusterToPointInTimeResponse' smart constructor.
data RestoreDBClusterToPointInTimeResponse = RestoreDBClusterToPointInTimeResponse'
  { dBCluster :: Core.Maybe Types.DBCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreDBClusterToPointInTimeResponse' value with any optional fields omitted.
mkRestoreDBClusterToPointInTimeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreDBClusterToPointInTimeResponse
mkRestoreDBClusterToPointInTimeResponse responseStatus
  = RestoreDBClusterToPointInTimeResponse'{dBCluster = Core.Nothing,
                                           responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitrrsDBCluster :: Lens.Lens' RestoreDBClusterToPointInTimeResponse (Core.Maybe Types.DBCluster)
rdbctpitrrsDBCluster = Lens.field @"dBCluster"
{-# INLINEABLE rdbctpitrrsDBCluster #-}
{-# DEPRECATED dBCluster "Use generic-lens or generic-optics with 'dBCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbctpitrrsResponseStatus :: Lens.Lens' RestoreDBClusterToPointInTimeResponse Core.Int
rdbctpitrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdbctpitrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
