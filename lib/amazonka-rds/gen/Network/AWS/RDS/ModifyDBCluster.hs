{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for an Amazon Aurora DB cluster. You can change one or more database configuration parameters by specifying these parameters and the new values in the request. For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
module Network.AWS.RDS.ModifyDBCluster
    (
    -- * Creating a request
      ModifyDBCluster (..)
    , mkModifyDBCluster
    -- ** Request lenses
    , mdbcDBClusterIdentifier
    , mdbcAllowMajorVersionUpgrade
    , mdbcApplyImmediately
    , mdbcBacktrackWindow
    , mdbcBackupRetentionPeriod
    , mdbcCloudwatchLogsExportConfiguration
    , mdbcCopyTagsToSnapshot
    , mdbcDBClusterParameterGroupName
    , mdbcDBInstanceParameterGroupName
    , mdbcDeletionProtection
    , mdbcDomain
    , mdbcDomainIAMRoleName
    , mdbcEnableGlobalWriteForwarding
    , mdbcEnableHttpEndpoint
    , mdbcEnableIAMDatabaseAuthentication
    , mdbcEngineVersion
    , mdbcMasterUserPassword
    , mdbcNewDBClusterIdentifier
    , mdbcOptionGroupName
    , mdbcPort
    , mdbcPreferredBackupWindow
    , mdbcPreferredMaintenanceWindow
    , mdbcScalingConfiguration
    , mdbcVpcSecurityGroupIds

    -- * Destructuring the response
    , ModifyDBClusterResponse (..)
    , mkModifyDBClusterResponse
    -- ** Response lenses
    , mdbcrrsDBCluster
    , mdbcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyDBCluster' smart constructor.
data ModifyDBCluster = ModifyDBCluster'
  { dBClusterIdentifier :: Core.Text
    -- ^ The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints: This identifier must match the identifier of an existing DB cluster.
  , allowMajorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a value for the @EngineVersion@ parameter that is a different major version than the DB cluster's current version.
  , applyImmediately :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB cluster. If this parameter is disabled, changes to the DB cluster are applied during the next maintenance window.
--
-- The @ApplyImmediately@ parameter only affects the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is disabled, then changes to the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values are applied during the next maintenance window. All other changes are applied immediately, regardless of the value of the @ApplyImmediately@ parameter.
-- By default, this parameter is disabled.
  , backtrackWindow :: Core.Maybe Core.Integer
    -- ^ The target backtrack window, in seconds. To disable backtracking, set this value to 0.
--
-- Default: 0
-- Constraints:
--
--     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
--
  , backupRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days for which automated backups are retained. You must specify a minimum value of 1.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 1 to 35
--
--
  , cloudwatchLogsExportConfiguration :: Core.Maybe Types.CloudwatchLogsExportConfiguration
    -- ^ The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB cluster.
  , copyTagsToSnapshot :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to copy all tags from the DB cluster to snapshots of the DB cluster. The default is not to copy them.
  , dBClusterParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB cluster parameter group to use for the DB cluster.
  , dBInstanceParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB parameter group to apply to all instances of the DB cluster. 
--
-- Default: The existing name setting
-- Constraints:
--
--     * The DB parameter group must be in the same DB parameter group family as this DB cluster.
--
--
--     * The @DBInstanceParameterGroupName@ parameter is only valid in combination with the @AllowMajorVersionUpgrade@ parameter.
--
--
  , deletionProtection :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. 
  , domain :: Core.Maybe Core.Text
    -- ^ The Active Directory directory ID to move the DB cluster to. Specify @none@ to remove the cluster from its current domain. The domain must be created prior to this operation. 
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ . 
  , domainIAMRoleName :: Core.Maybe Core.Text
    -- ^ Specify the name of the IAM role to be used when making API calls to the Directory Service.
  , enableGlobalWriteForwarding :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to enable write operations to be forwarded from this cluster to the primary cluster in an Aurora global database. The resulting changes are replicated back to this cluster. This parameter only applies to DB clusters that are secondary clusters in an Aurora global database. By default, Aurora disallows write operations for secondary clusters.
  , enableHttpEndpoint :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to enable the HTTP endpoint for an Aurora Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
  , enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./ 
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The version number of the database engine to which you want to upgrade. Changing this parameter results in an outage. The change is applied during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available engine versions for @aurora@ (for MySQL 5.6-compatible Aurora), use the following command:
-- @aws rds describe-db-engine-versions --engine aurora --query "DBEngineVersions[].EngineVersion"@ 
-- To list all of the available engine versions for @aurora-mysql@ (for MySQL 5.7-compatible Aurora), use the following command:
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query "DBEngineVersions[].EngineVersion"@ 
-- To list all of the available engine versions for @aurora-postgresql@ , use the following command:
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query "DBEngineVersions[].EngineVersion"@ 
  , masterUserPassword :: Core.Maybe Core.Text
    -- ^ The new password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
  , newDBClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The new DB cluster identifier for the DB cluster when renaming a DB cluster. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens
--
--
--     * The first character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-cluster2@ 
  , optionGroupName :: Core.Maybe Core.Text
    -- ^ A value that indicates that the DB cluster should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case, and the change is applied during the next maintenance window unless the @ApplyImmediately@ is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted. 
--
-- Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
  , port :: Core.Maybe Core.Int
    -- ^ The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@ 
-- Default: The same port as the original DB cluster.
  , preferredBackupWindow :: Core.Maybe Core.Text
    -- ^ The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. 
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
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@ 
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./ 
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
  , scalingConfiguration :: Core.Maybe Types.ScalingConfiguration
    -- ^ The scaling properties of the DB cluster. You can only modify scaling properties for DB clusters in @serverless@ DB engine mode.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of VPC security groups that the DB cluster will belong to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBCluster' value with any optional fields omitted.
mkModifyDBCluster
    :: Core.Text -- ^ 'dBClusterIdentifier'
    -> ModifyDBCluster
mkModifyDBCluster dBClusterIdentifier
  = ModifyDBCluster'{dBClusterIdentifier,
                     allowMajorVersionUpgrade = Core.Nothing,
                     applyImmediately = Core.Nothing, backtrackWindow = Core.Nothing,
                     backupRetentionPeriod = Core.Nothing,
                     cloudwatchLogsExportConfiguration = Core.Nothing,
                     copyTagsToSnapshot = Core.Nothing,
                     dBClusterParameterGroupName = Core.Nothing,
                     dBInstanceParameterGroupName = Core.Nothing,
                     deletionProtection = Core.Nothing, domain = Core.Nothing,
                     domainIAMRoleName = Core.Nothing,
                     enableGlobalWriteForwarding = Core.Nothing,
                     enableHttpEndpoint = Core.Nothing,
                     enableIAMDatabaseAuthentication = Core.Nothing,
                     engineVersion = Core.Nothing, masterUserPassword = Core.Nothing,
                     newDBClusterIdentifier = Core.Nothing,
                     optionGroupName = Core.Nothing, port = Core.Nothing,
                     preferredBackupWindow = Core.Nothing,
                     preferredMaintenanceWindow = Core.Nothing,
                     scalingConfiguration = Core.Nothing,
                     vpcSecurityGroupIds = Core.Nothing}

-- | The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints: This identifier must match the identifier of an existing DB cluster.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcDBClusterIdentifier :: Lens.Lens' ModifyDBCluster Core.Text
mdbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE mdbcDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a value for the @EngineVersion@ parameter that is a different major version than the DB cluster's current version.
--
-- /Note:/ Consider using 'allowMajorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcAllowMajorVersionUpgrade :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
mdbcAllowMajorVersionUpgrade = Lens.field @"allowMajorVersionUpgrade"
{-# INLINEABLE mdbcAllowMajorVersionUpgrade #-}
{-# DEPRECATED allowMajorVersionUpgrade "Use generic-lens or generic-optics with 'allowMajorVersionUpgrade' instead"  #-}

-- | A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB cluster. If this parameter is disabled, changes to the DB cluster are applied during the next maintenance window.
--
-- The @ApplyImmediately@ parameter only affects the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is disabled, then changes to the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values are applied during the next maintenance window. All other changes are applied immediately, regardless of the value of the @ApplyImmediately@ parameter.
-- By default, this parameter is disabled.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcApplyImmediately :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
mdbcApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE mdbcApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

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
mdbcBacktrackWindow :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Integer)
mdbcBacktrackWindow = Lens.field @"backtrackWindow"
{-# INLINEABLE mdbcBacktrackWindow #-}
{-# DEPRECATED backtrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead"  #-}

-- | The number of days for which automated backups are retained. You must specify a minimum value of 1.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 1 to 35
--
--
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcBackupRetentionPeriod :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Int)
mdbcBackupRetentionPeriod = Lens.field @"backupRetentionPeriod"
{-# INLINEABLE mdbcBackupRetentionPeriod #-}
{-# DEPRECATED backupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead"  #-}

-- | The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB cluster.
--
-- /Note:/ Consider using 'cloudwatchLogsExportConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcCloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBCluster (Core.Maybe Types.CloudwatchLogsExportConfiguration)
mdbcCloudwatchLogsExportConfiguration = Lens.field @"cloudwatchLogsExportConfiguration"
{-# INLINEABLE mdbcCloudwatchLogsExportConfiguration #-}
{-# DEPRECATED cloudwatchLogsExportConfiguration "Use generic-lens or generic-optics with 'cloudwatchLogsExportConfiguration' instead"  #-}

-- | A value that indicates whether to copy all tags from the DB cluster to snapshots of the DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcCopyTagsToSnapshot :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
mdbcCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# INLINEABLE mdbcCopyTagsToSnapshot #-}
{-# DEPRECATED copyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead"  #-}

-- | The name of the DB cluster parameter group to use for the DB cluster.
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcDBClusterParameterGroupName :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
mdbcDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# INLINEABLE mdbcDBClusterParameterGroupName #-}
{-# DEPRECATED dBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead"  #-}

-- | The name of the DB parameter group to apply to all instances of the DB cluster. 
--
-- Default: The existing name setting
-- Constraints:
--
--     * The DB parameter group must be in the same DB parameter group family as this DB cluster.
--
--
--     * The @DBInstanceParameterGroupName@ parameter is only valid in combination with the @AllowMajorVersionUpgrade@ parameter.
--
--
--
-- /Note:/ Consider using 'dBInstanceParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcDBInstanceParameterGroupName :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
mdbcDBInstanceParameterGroupName = Lens.field @"dBInstanceParameterGroupName"
{-# INLINEABLE mdbcDBInstanceParameterGroupName #-}
{-# DEPRECATED dBInstanceParameterGroupName "Use generic-lens or generic-optics with 'dBInstanceParameterGroupName' instead"  #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. 
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcDeletionProtection :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
mdbcDeletionProtection = Lens.field @"deletionProtection"
{-# INLINEABLE mdbcDeletionProtection #-}
{-# DEPRECATED deletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead"  #-}

-- | The Active Directory directory ID to move the DB cluster to. Specify @none@ to remove the cluster from its current domain. The domain must be created prior to this operation. 
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ . 
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcDomain :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
mdbcDomain = Lens.field @"domain"
{-# INLINEABLE mdbcDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcDomainIAMRoleName :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
mdbcDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# INLINEABLE mdbcDomainIAMRoleName #-}
{-# DEPRECATED domainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead"  #-}

-- | A value that indicates whether to enable write operations to be forwarded from this cluster to the primary cluster in an Aurora global database. The resulting changes are replicated back to this cluster. This parameter only applies to DB clusters that are secondary clusters in an Aurora global database. By default, Aurora disallows write operations for secondary clusters.
--
-- /Note:/ Consider using 'enableGlobalWriteForwarding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcEnableGlobalWriteForwarding :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
mdbcEnableGlobalWriteForwarding = Lens.field @"enableGlobalWriteForwarding"
{-# INLINEABLE mdbcEnableGlobalWriteForwarding #-}
{-# DEPRECATED enableGlobalWriteForwarding "Use generic-lens or generic-optics with 'enableGlobalWriteForwarding' instead"  #-}

-- | A value that indicates whether to enable the HTTP endpoint for an Aurora Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableHttpEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcEnableHttpEndpoint :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
mdbcEnableHttpEndpoint = Lens.field @"enableHttpEndpoint"
{-# INLINEABLE mdbcEnableHttpEndpoint #-}
{-# DEPRECATED enableHttpEndpoint "Use generic-lens or generic-optics with 'enableHttpEndpoint' instead"  #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./ 
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcEnableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
mdbcEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# INLINEABLE mdbcEnableIAMDatabaseAuthentication #-}
{-# DEPRECATED enableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead"  #-}

-- | The version number of the database engine to which you want to upgrade. Changing this parameter results in an outage. The change is applied during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available engine versions for @aurora@ (for MySQL 5.6-compatible Aurora), use the following command:
-- @aws rds describe-db-engine-versions --engine aurora --query "DBEngineVersions[].EngineVersion"@ 
-- To list all of the available engine versions for @aurora-mysql@ (for MySQL 5.7-compatible Aurora), use the following command:
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query "DBEngineVersions[].EngineVersion"@ 
-- To list all of the available engine versions for @aurora-postgresql@ , use the following command:
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query "DBEngineVersions[].EngineVersion"@ 
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcEngineVersion :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
mdbcEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE mdbcEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The new password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcMasterUserPassword :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
mdbcMasterUserPassword = Lens.field @"masterUserPassword"
{-# INLINEABLE mdbcMasterUserPassword #-}
{-# DEPRECATED masterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead"  #-}

-- | The new DB cluster identifier for the DB cluster when renaming a DB cluster. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens
--
--
--     * The first character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-cluster2@ 
--
-- /Note:/ Consider using 'newDBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcNewDBClusterIdentifier :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
mdbcNewDBClusterIdentifier = Lens.field @"newDBClusterIdentifier"
{-# INLINEABLE mdbcNewDBClusterIdentifier #-}
{-# DEPRECATED newDBClusterIdentifier "Use generic-lens or generic-optics with 'newDBClusterIdentifier' instead"  #-}

-- | A value that indicates that the DB cluster should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case, and the change is applied during the next maintenance window unless the @ApplyImmediately@ is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted. 
--
-- Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcOptionGroupName :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
mdbcOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE mdbcOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@ 
-- Default: The same port as the original DB cluster.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcPort :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Int)
mdbcPort = Lens.field @"port"
{-# INLINEABLE mdbcPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. 
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
mdbcPreferredBackupWindow :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
mdbcPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# INLINEABLE mdbcPreferredBackupWindow #-}
{-# DEPRECATED preferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead"  #-}

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@ 
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./ 
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcPreferredMaintenanceWindow :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
mdbcPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE mdbcPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | The scaling properties of the DB cluster. You can only modify scaling properties for DB clusters in @serverless@ DB engine mode.
--
-- /Note:/ Consider using 'scalingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcScalingConfiguration :: Lens.Lens' ModifyDBCluster (Core.Maybe Types.ScalingConfiguration)
mdbcScalingConfiguration = Lens.field @"scalingConfiguration"
{-# INLINEABLE mdbcScalingConfiguration #-}
{-# DEPRECATED scalingConfiguration "Use generic-lens or generic-optics with 'scalingConfiguration' instead"  #-}

-- | A list of VPC security groups that the DB cluster will belong to.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcVpcSecurityGroupIds :: Lens.Lens' ModifyDBCluster (Core.Maybe [Core.Text])
mdbcVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE mdbcVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery ModifyDBCluster where
        toQuery ModifyDBCluster{..}
          = Core.toQueryPair "Action" ("ModifyDBCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AllowMajorVersionUpgrade")
                allowMajorVersionUpgrade
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ApplyImmediately")
                applyImmediately
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "BacktrackWindow")
                backtrackWindow
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "BackupRetentionPeriod")
                backupRetentionPeriod
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "CloudwatchLogsExportConfiguration")
                cloudwatchLogsExportConfiguration
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CopyTagsToSnapshot")
                copyTagsToSnapshot
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DBClusterParameterGroupName")
                dBClusterParameterGroupName
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DBInstanceParameterGroupName")
                dBInstanceParameterGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeletionProtection")
                deletionProtection
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Domain") domain
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DomainIAMRoleName")
                domainIAMRoleName
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "EnableGlobalWriteForwarding")
                enableGlobalWriteForwarding
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnableHttpEndpoint")
                enableHttpEndpoint
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "EnableIAMDatabaseAuthentication")
                enableIAMDatabaseAuthentication
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EngineVersion")
                engineVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MasterUserPassword")
                masterUserPassword
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NewDBClusterIdentifier")
                newDBClusterIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OptionGroupName")
                optionGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PreferredBackupWindow")
                preferredBackupWindow
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PreferredMaintenanceWindow")
                preferredMaintenanceWindow
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ScalingConfiguration")
                scalingConfiguration
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupIds)

instance Core.ToHeaders ModifyDBCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyDBCluster where
        type Rs ModifyDBCluster = ModifyDBClusterResponse
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
          = Response.receiveXMLWrapper "ModifyDBClusterResult"
              (\ s h x ->
                 ModifyDBClusterResponse' Core.<$>
                   (x Core..@? "DBCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyDBClusterResponse' smart constructor.
data ModifyDBClusterResponse = ModifyDBClusterResponse'
  { dBCluster :: Core.Maybe Types.DBCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyDBClusterResponse' value with any optional fields omitted.
mkModifyDBClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyDBClusterResponse
mkModifyDBClusterResponse responseStatus
  = ModifyDBClusterResponse'{dBCluster = Core.Nothing,
                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcrrsDBCluster :: Lens.Lens' ModifyDBClusterResponse (Core.Maybe Types.DBCluster)
mdbcrrsDBCluster = Lens.field @"dBCluster"
{-# INLINEABLE mdbcrrsDBCluster #-}
{-# DEPRECATED dBCluster "Use generic-lens or generic-optics with 'dBCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcrrsResponseStatus :: Lens.Lens' ModifyDBClusterResponse Core.Int
mdbcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mdbcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
