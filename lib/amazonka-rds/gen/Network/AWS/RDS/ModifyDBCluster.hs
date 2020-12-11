{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyDBCluster (..),
    mkModifyDBCluster,

    -- ** Request lenses
    mdcEngineVersion,
    mdcEnableGlobalWriteForwarding,
    mdcDeletionProtection,
    mdcMasterUserPassword,
    mdcEnableHTTPEndpoint,
    mdcAllowMajorVersionUpgrade,
    mdcDomain,
    mdcBacktrackWindow,
    mdcCloudwatchLogsExportConfiguration,
    mdcPreferredMaintenanceWindow,
    mdcPreferredBackupWindow,
    mdcBackupRetentionPeriod,
    mdcVPCSecurityGroupIds,
    mdcDBClusterParameterGroupName,
    mdcScalingConfiguration,
    mdcApplyImmediately,
    mdcOptionGroupName,
    mdcCopyTagsToSnapshot,
    mdcNewDBClusterIdentifier,
    mdcDBInstanceParameterGroupName,
    mdcDomainIAMRoleName,
    mdcPort,
    mdcEnableIAMDatabaseAuthentication,
    mdcDBClusterIdentifier,

    -- * Destructuring the response
    ModifyDBClusterResponse (..),
    mkModifyDBClusterResponse,

    -- ** Response lenses
    mdcrsDBCluster,
    mdcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyDBCluster' smart constructor.
data ModifyDBCluster = ModifyDBCluster'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    enableGlobalWriteForwarding :: Lude.Maybe Lude.Bool,
    deletionProtection :: Lude.Maybe Lude.Bool,
    masterUserPassword :: Lude.Maybe Lude.Text,
    enableHTTPEndpoint :: Lude.Maybe Lude.Bool,
    allowMajorVersionUpgrade :: Lude.Maybe Lude.Bool,
    domain :: Lude.Maybe Lude.Text,
    backtrackWindow :: Lude.Maybe Lude.Integer,
    cloudwatchLogsExportConfiguration ::
      Lude.Maybe CloudwatchLogsExportConfiguration,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    dbClusterParameterGroupName :: Lude.Maybe Lude.Text,
    scalingConfiguration :: Lude.Maybe ScalingConfiguration,
    applyImmediately :: Lude.Maybe Lude.Bool,
    optionGroupName :: Lude.Maybe Lude.Text,
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    newDBClusterIdentifier :: Lude.Maybe Lude.Text,
    dbInstanceParameterGroupName :: Lude.Maybe Lude.Text,
    domainIAMRoleName :: Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Int,
    enableIAMDatabaseAuthentication :: Lude.Maybe Lude.Bool,
    dbClusterIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBCluster' with the minimum fields required to make a request.
--
-- * 'allowMajorVersionUpgrade' - A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a value for the @EngineVersion@ parameter that is a different major version than the DB cluster's current version.
-- * 'applyImmediately' - A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB cluster. If this parameter is disabled, changes to the DB cluster are applied during the next maintenance window.
--
-- The @ApplyImmediately@ parameter only affects the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is disabled, then changes to the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values are applied during the next maintenance window. All other changes are applied immediately, regardless of the value of the @ApplyImmediately@ parameter.
-- By default, this parameter is disabled.
-- * 'backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set this value to 0.
--
-- Default: 0
-- Constraints:
--
--     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
--
-- * 'backupRetentionPeriod' - The number of days for which automated backups are retained. You must specify a minimum value of 1.
--
-- Default: 1
-- Constraints:
--
--     * Must be a value from 1 to 35
--
--
-- * 'cloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB cluster.
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB cluster to snapshots of the DB cluster. The default is not to copy them.
-- * 'dbClusterIdentifier' - The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints: This identifier must match the identifier of an existing DB cluster.
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group to use for the DB cluster.
-- * 'dbInstanceParameterGroupName' - The name of the DB parameter group to apply to all instances of the DB cluster.
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
-- * 'deletionProtection' - A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
-- * 'domain' - The Active Directory directory ID to move the DB cluster to. Specify @none@ to remove the cluster from its current domain. The domain must be created prior to this operation.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
-- * 'domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
-- * 'enableGlobalWriteForwarding' - A value that indicates whether to enable write operations to be forwarded from this cluster to the primary cluster in an Aurora global database. The resulting changes are replicated back to this cluster. This parameter only applies to DB clusters that are secondary clusters in an Aurora global database. By default, Aurora disallows write operations for secondary clusters.
-- * 'enableHTTPEndpoint' - A value that indicates whether to enable the HTTP endpoint for an Aurora Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
-- * 'enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
-- * 'engineVersion' - The version number of the database engine to which you want to upgrade. Changing this parameter results in an outage. The change is applied during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available engine versions for @aurora@ (for MySQL 5.6-compatible Aurora), use the following command:
-- @aws rds describe-db-engine-versions --engine aurora --query "DBEngineVersions[].EngineVersion"@
-- To list all of the available engine versions for @aurora-mysql@ (for MySQL 5.7-compatible Aurora), use the following command:
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query "DBEngineVersions[].EngineVersion"@
-- To list all of the available engine versions for @aurora-postgresql@ , use the following command:
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query "DBEngineVersions[].EngineVersion"@
-- * 'masterUserPassword' - The new password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
-- * 'newDBClusterIdentifier' - The new DB cluster identifier for the DB cluster when renaming a DB cluster. This value is stored as a lowercase string.
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
-- * 'optionGroupName' - A value that indicates that the DB cluster should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case, and the change is applied during the next maintenance window unless the @ApplyImmediately@ is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted.
--
-- Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
-- * 'port' - The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
-- Default: The same port as the original DB cluster.
-- * 'preferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter.
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
-- * 'scalingConfiguration' - The scaling properties of the DB cluster. You can only modify scaling properties for DB clusters in @serverless@ DB engine mode.
-- * 'vpcSecurityGroupIds' - A list of VPC security groups that the DB cluster will belong to.
mkModifyDBCluster ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  ModifyDBCluster
mkModifyDBCluster pDBClusterIdentifier_ =
  ModifyDBCluster'
    { engineVersion = Lude.Nothing,
      enableGlobalWriteForwarding = Lude.Nothing,
      deletionProtection = Lude.Nothing,
      masterUserPassword = Lude.Nothing,
      enableHTTPEndpoint = Lude.Nothing,
      allowMajorVersionUpgrade = Lude.Nothing,
      domain = Lude.Nothing,
      backtrackWindow = Lude.Nothing,
      cloudwatchLogsExportConfiguration = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      preferredBackupWindow = Lude.Nothing,
      backupRetentionPeriod = Lude.Nothing,
      vpcSecurityGroupIds = Lude.Nothing,
      dbClusterParameterGroupName = Lude.Nothing,
      scalingConfiguration = Lude.Nothing,
      applyImmediately = Lude.Nothing,
      optionGroupName = Lude.Nothing,
      copyTagsToSnapshot = Lude.Nothing,
      newDBClusterIdentifier = Lude.Nothing,
      dbInstanceParameterGroupName = Lude.Nothing,
      domainIAMRoleName = Lude.Nothing,
      port = Lude.Nothing,
      enableIAMDatabaseAuthentication = Lude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

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
mdcEngineVersion :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Text)
mdcEngineVersion = Lens.lens (engineVersion :: ModifyDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: ModifyDBCluster)
{-# DEPRECATED mdcEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A value that indicates whether to enable write operations to be forwarded from this cluster to the primary cluster in an Aurora global database. The resulting changes are replicated back to this cluster. This parameter only applies to DB clusters that are secondary clusters in an Aurora global database. By default, Aurora disallows write operations for secondary clusters.
--
-- /Note:/ Consider using 'enableGlobalWriteForwarding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcEnableGlobalWriteForwarding :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Bool)
mdcEnableGlobalWriteForwarding = Lens.lens (enableGlobalWriteForwarding :: ModifyDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {enableGlobalWriteForwarding = a} :: ModifyDBCluster)
{-# DEPRECATED mdcEnableGlobalWriteForwarding "Use generic-lens or generic-optics with 'enableGlobalWriteForwarding' instead." #-}

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcDeletionProtection :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Bool)
mdcDeletionProtection = Lens.lens (deletionProtection :: ModifyDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: ModifyDBCluster)
{-# DEPRECATED mdcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The new password for the master database user. This password can contain any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcMasterUserPassword :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Text)
mdcMasterUserPassword = Lens.lens (masterUserPassword :: ModifyDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {masterUserPassword = a} :: ModifyDBCluster)
{-# DEPRECATED mdcMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | A value that indicates whether to enable the HTTP endpoint for an Aurora Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'enableHTTPEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcEnableHTTPEndpoint :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Bool)
mdcEnableHTTPEndpoint = Lens.lens (enableHTTPEndpoint :: ModifyDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {enableHTTPEndpoint = a} :: ModifyDBCluster)
{-# DEPRECATED mdcEnableHTTPEndpoint "Use generic-lens or generic-optics with 'enableHTTPEndpoint' instead." #-}

-- | A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a value for the @EngineVersion@ parameter that is a different major version than the DB cluster's current version.
--
-- /Note:/ Consider using 'allowMajorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcAllowMajorVersionUpgrade :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Bool)
mdcAllowMajorVersionUpgrade = Lens.lens (allowMajorVersionUpgrade :: ModifyDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {allowMajorVersionUpgrade = a} :: ModifyDBCluster)
{-# DEPRECATED mdcAllowMajorVersionUpgrade "Use generic-lens or generic-optics with 'allowMajorVersionUpgrade' instead." #-}

-- | The Active Directory directory ID to move the DB cluster to. Specify @none@ to remove the cluster from its current domain. The domain must be created prior to this operation.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcDomain :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Text)
mdcDomain = Lens.lens (domain :: ModifyDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: ModifyDBCluster)
{-# DEPRECATED mdcDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

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
mdcBacktrackWindow :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Integer)
mdcBacktrackWindow = Lens.lens (backtrackWindow :: ModifyDBCluster -> Lude.Maybe Lude.Integer) (\s a -> s {backtrackWindow = a} :: ModifyDBCluster)
{-# DEPRECATED mdcBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

-- | The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB cluster.
--
-- /Note:/ Consider using 'cloudwatchLogsExportConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcCloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBCluster (Lude.Maybe CloudwatchLogsExportConfiguration)
mdcCloudwatchLogsExportConfiguration = Lens.lens (cloudwatchLogsExportConfiguration :: ModifyDBCluster -> Lude.Maybe CloudwatchLogsExportConfiguration) (\s a -> s {cloudwatchLogsExportConfiguration = a} :: ModifyDBCluster)
{-# DEPRECATED mdcCloudwatchLogsExportConfiguration "Use generic-lens or generic-optics with 'cloudwatchLogsExportConfiguration' instead." #-}

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcPreferredMaintenanceWindow :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Text)
mdcPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: ModifyDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: ModifyDBCluster)
{-# DEPRECATED mdcPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

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
mdcPreferredBackupWindow :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Text)
mdcPreferredBackupWindow = Lens.lens (preferredBackupWindow :: ModifyDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: ModifyDBCluster)
{-# DEPRECATED mdcPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

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
mdcBackupRetentionPeriod :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Int)
mdcBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: ModifyDBCluster -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: ModifyDBCluster)
{-# DEPRECATED mdcBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | A list of VPC security groups that the DB cluster will belong to.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcVPCSecurityGroupIds :: Lens.Lens' ModifyDBCluster (Lude.Maybe [Lude.Text])
mdcVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: ModifyDBCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: ModifyDBCluster)
{-# DEPRECATED mdcVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | The name of the DB cluster parameter group to use for the DB cluster.
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcDBClusterParameterGroupName :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Text)
mdcDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: ModifyDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: ModifyDBCluster)
{-# DEPRECATED mdcDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

-- | The scaling properties of the DB cluster. You can only modify scaling properties for DB clusters in @serverless@ DB engine mode.
--
-- /Note:/ Consider using 'scalingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcScalingConfiguration :: Lens.Lens' ModifyDBCluster (Lude.Maybe ScalingConfiguration)
mdcScalingConfiguration = Lens.lens (scalingConfiguration :: ModifyDBCluster -> Lude.Maybe ScalingConfiguration) (\s a -> s {scalingConfiguration = a} :: ModifyDBCluster)
{-# DEPRECATED mdcScalingConfiguration "Use generic-lens or generic-optics with 'scalingConfiguration' instead." #-}

-- | A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB cluster. If this parameter is disabled, changes to the DB cluster are applied during the next maintenance window.
--
-- The @ApplyImmediately@ parameter only affects the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is disabled, then changes to the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values are applied during the next maintenance window. All other changes are applied immediately, regardless of the value of the @ApplyImmediately@ parameter.
-- By default, this parameter is disabled.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcApplyImmediately :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Bool)
mdcApplyImmediately = Lens.lens (applyImmediately :: ModifyDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {applyImmediately = a} :: ModifyDBCluster)
{-# DEPRECATED mdcApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | A value that indicates that the DB cluster should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case, and the change is applied during the next maintenance window unless the @ApplyImmediately@ is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted.
--
-- Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcOptionGroupName :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Text)
mdcOptionGroupName = Lens.lens (optionGroupName :: ModifyDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: ModifyDBCluster)
{-# DEPRECATED mdcOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the DB cluster to snapshots of the DB cluster. The default is not to copy them.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcCopyTagsToSnapshot :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Bool)
mdcCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: ModifyDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: ModifyDBCluster)
{-# DEPRECATED mdcCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

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
mdcNewDBClusterIdentifier :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Text)
mdcNewDBClusterIdentifier = Lens.lens (newDBClusterIdentifier :: ModifyDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {newDBClusterIdentifier = a} :: ModifyDBCluster)
{-# DEPRECATED mdcNewDBClusterIdentifier "Use generic-lens or generic-optics with 'newDBClusterIdentifier' instead." #-}

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
-- /Note:/ Consider using 'dbInstanceParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcDBInstanceParameterGroupName :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Text)
mdcDBInstanceParameterGroupName = Lens.lens (dbInstanceParameterGroupName :: ModifyDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceParameterGroupName = a} :: ModifyDBCluster)
{-# DEPRECATED mdcDBInstanceParameterGroupName "Use generic-lens or generic-optics with 'dbInstanceParameterGroupName' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcDomainIAMRoleName :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Text)
mdcDomainIAMRoleName = Lens.lens (domainIAMRoleName :: ModifyDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: ModifyDBCluster)
{-# DEPRECATED mdcDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
-- Default: The same port as the original DB cluster.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcPort :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Int)
mdcPort = Lens.lens (port :: ModifyDBCluster -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: ModifyDBCluster)
{-# DEPRECATED mdcPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcEnableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBCluster (Lude.Maybe Lude.Bool)
mdcEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: ModifyDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: ModifyDBCluster)
{-# DEPRECATED mdcEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints: This identifier must match the identifier of an existing DB cluster.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcDBClusterIdentifier :: Lens.Lens' ModifyDBCluster Lude.Text
mdcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: ModifyDBCluster -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: ModifyDBCluster)
{-# DEPRECATED mdcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

instance Lude.AWSRequest ModifyDBCluster where
  type Rs ModifyDBCluster = ModifyDBClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBClusterResult"
      ( \s h x ->
          ModifyDBClusterResponse'
            Lude.<$> (x Lude..@? "DBCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyDBCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBCluster where
  toQuery ModifyDBCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyDBCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "EnableGlobalWriteForwarding" Lude.=: enableGlobalWriteForwarding,
        "DeletionProtection" Lude.=: deletionProtection,
        "MasterUserPassword" Lude.=: masterUserPassword,
        "EnableHttpEndpoint" Lude.=: enableHTTPEndpoint,
        "AllowMajorVersionUpgrade" Lude.=: allowMajorVersionUpgrade,
        "Domain" Lude.=: domain,
        "BacktrackWindow" Lude.=: backtrackWindow,
        "CloudwatchLogsExportConfiguration"
          Lude.=: cloudwatchLogsExportConfiguration,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "PreferredBackupWindow" Lude.=: preferredBackupWindow,
        "BackupRetentionPeriod" Lude.=: backupRetentionPeriod,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "DBClusterParameterGroupName" Lude.=: dbClusterParameterGroupName,
        "ScalingConfiguration" Lude.=: scalingConfiguration,
        "ApplyImmediately" Lude.=: applyImmediately,
        "OptionGroupName" Lude.=: optionGroupName,
        "CopyTagsToSnapshot" Lude.=: copyTagsToSnapshot,
        "NewDBClusterIdentifier" Lude.=: newDBClusterIdentifier,
        "DBInstanceParameterGroupName"
          Lude.=: dbInstanceParameterGroupName,
        "DomainIAMRoleName" Lude.=: domainIAMRoleName,
        "Port" Lude.=: port,
        "EnableIAMDatabaseAuthentication"
          Lude.=: enableIAMDatabaseAuthentication,
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier
      ]

-- | /See:/ 'mkModifyDBClusterResponse' smart constructor.
data ModifyDBClusterResponse = ModifyDBClusterResponse'
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

-- | Creates a value of 'ModifyDBClusterResponse' with the minimum fields required to make a request.
--
-- * 'dbCluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkModifyDBClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyDBClusterResponse
mkModifyDBClusterResponse pResponseStatus_ =
  ModifyDBClusterResponse'
    { dbCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcrsDBCluster :: Lens.Lens' ModifyDBClusterResponse (Lude.Maybe DBCluster)
mdcrsDBCluster = Lens.lens (dbCluster :: ModifyDBClusterResponse -> Lude.Maybe DBCluster) (\s a -> s {dbCluster = a} :: ModifyDBClusterResponse)
{-# DEPRECATED mdcrsDBCluster "Use generic-lens or generic-optics with 'dbCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcrsResponseStatus :: Lens.Lens' ModifyDBClusterResponse Lude.Int
mdcrsResponseStatus = Lens.lens (responseStatus :: ModifyDBClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyDBClusterResponse)
{-# DEPRECATED mdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
