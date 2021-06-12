{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for an Amazon Aurora DB cluster. You can change one or
-- more database configuration parameters by specifying these parameters
-- and the new values in the request. For more information on Amazon
-- Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.ModifyDBCluster
  ( -- * Creating a Request
    ModifyDBCluster (..),
    newModifyDBCluster,

    -- * Request Lenses
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_enableIAMDatabaseAuthentication,
    modifyDBCluster_dbInstanceParameterGroupName,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_domain,
    modifyDBCluster_allowMajorVersionUpgrade,
    modifyDBCluster_scalingConfiguration,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_enableHttpEndpoint,
    modifyDBCluster_engineVersion,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_enableGlobalWriteForwarding,
    modifyDBCluster_port,
    modifyDBCluster_domainIAMRoleName,
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_backtrackWindow,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_dbClusterIdentifier,

    -- * Destructuring the Response
    ModifyDBClusterResponse (..),
    newModifyDBClusterResponse,

    -- * Response Lenses
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyDBCluster' smart constructor.
data ModifyDBCluster = ModifyDBCluster'
  { -- | The number of days for which automated backups are retained. You must
    -- specify a minimum value of 1.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled, using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each AWS Region. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
    -- in the /Amazon Aurora User Guide./
    --
    -- Constraints:
    --
    -- -   Must be in the format @hh24:mi-hh24:mi@.
    --
    -- -   Must be in Universal Coordinated Time (UTC).
    --
    -- -   Must not conflict with the preferred maintenance window.
    --
    -- -   Must be at least 30 minutes.
    preferredBackupWindow :: Core.Maybe Core.Text,
    -- | A value that indicates whether to enable mapping of AWS Identity and
    -- Access Management (IAM) accounts to database accounts. By default,
    -- mapping is disabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
    -- | The name of the DB parameter group to apply to all instances of the DB
    -- cluster.
    --
    -- When you apply a parameter group using the
    -- @DBInstanceParameterGroupName@ parameter, the DB cluster isn\'t rebooted
    -- automatically. Also, parameter changes aren\'t applied during the next
    -- maintenance window but instead are applied immediately.
    --
    -- Default: The existing name setting
    --
    -- Constraints:
    --
    -- -   The DB parameter group must be in the same DB parameter group family
    --     as this DB cluster.
    --
    -- -   The @DBInstanceParameterGroupName@ parameter is only valid in
    --     combination with the @AllowMajorVersionUpgrade@ parameter.
    dbInstanceParameterGroupName :: Core.Maybe Core.Text,
    -- | A value that indicates that the DB cluster should be associated with the
    -- specified option group. Changing this parameter doesn\'t result in an
    -- outage except in the following case, and the change is applied during
    -- the next maintenance window unless the @ApplyImmediately@ is enabled for
    -- this request. If the parameter change results in an option group that
    -- enables OEM, this change can cause a brief (sub-second) period during
    -- which new connections are rejected but existing connections are not
    -- interrupted.
    --
    -- Permanent options can\'t be removed from an option group. The option
    -- group can\'t be removed from a DB cluster once it is associated with a
    -- DB cluster.
    optionGroupName :: Core.Maybe Core.Text,
    -- | The Active Directory directory ID to move the DB cluster to. Specify
    -- @none@ to remove the cluster from its current domain. The domain must be
    -- created prior to this operation.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon Aurora User Guide/.
    domain :: Core.Maybe Core.Text,
    -- | A value that indicates whether major version upgrades are allowed.
    --
    -- Constraints: You must allow major version upgrades when specifying a
    -- value for the @EngineVersion@ parameter that is a different major
    -- version than the DB cluster\'s current version.
    allowMajorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The scaling properties of the DB cluster. You can only modify scaling
    -- properties for DB clusters in @serverless@ DB engine mode.
    scalingConfiguration :: Core.Maybe ScalingConfiguration,
    -- | The new password for the master database user. This password can contain
    -- any printable ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Core.Maybe Core.Text,
    -- | A list of VPC security groups that the DB cluster will belong to.
    vpcSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | A value that indicates whether to enable the HTTP endpoint for an Aurora
    -- Serverless DB cluster. By default, the HTTP endpoint is disabled.
    --
    -- When enabled, the HTTP endpoint provides a connectionless web service
    -- API for running SQL queries on the Aurora Serverless DB cluster. You can
    -- also query your database from inside the RDS console with the query
    -- editor.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless>
    -- in the /Amazon Aurora User Guide/.
    enableHttpEndpoint :: Core.Maybe Core.Bool,
    -- | The version number of the database engine to which you want to upgrade.
    -- Changing this parameter results in an outage. The change is applied
    -- during the next maintenance window unless @ApplyImmediately@ is enabled.
    --
    -- To list all of the available engine versions for @aurora@ (for MySQL
    -- 5.6-compatible Aurora), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for @aurora-mysql@ (for
    -- MySQL 5.7-compatible Aurora), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for @aurora-postgresql@,
    -- use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
    engineVersion :: Core.Maybe Core.Text,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each AWS Region, occurring on a random day of the
    -- week. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
    -- in the /Amazon Aurora User Guide./
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | A value that indicates whether to enable this DB cluster to forward
    -- write operations to the primary cluster of an Aurora global database
    -- (GlobalCluster). By default, write operations are not allowed on Aurora
    -- DB clusters that are secondary clusters in an Aurora global database.
    --
    -- You can set this value only on Aurora DB clusters that are members of an
    -- Aurora global database. With this parameter enabled, a secondary cluster
    -- can forward writes to the current primary cluster and the resulting
    -- changes are replicated back to this cluster. For the primary DB cluster
    -- of an Aurora global database, this value is used immediately if the
    -- primary is demoted by the FailoverGlobalCluster API operation, but it
    -- does nothing until then.
    enableGlobalWriteForwarding :: Core.Maybe Core.Bool,
    -- | The port number on which the DB cluster accepts connections.
    --
    -- Constraints: Value must be @1150-65535@
    --
    -- Default: The same port as the original DB cluster.
    port :: Core.Maybe Core.Int,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Core.Maybe Core.Text,
    -- | The new DB cluster identifier for the DB cluster when renaming a DB
    -- cluster. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    -- -   The first character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-cluster2@
    newDBClusterIdentifier' :: Core.Maybe Core.Text,
    -- | The configuration setting for the log types to be enabled for export to
    -- CloudWatch Logs for a specific DB cluster.
    cloudwatchLogsExportConfiguration :: Core.Maybe CloudwatchLogsExportConfiguration,
    -- | A value that indicates whether to copy all tags from the DB cluster to
    -- snapshots of the DB cluster. The default is not to copy them.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
    -- | The target backtrack window, in seconds. To disable backtracking, set
    -- this value to 0.
    --
    -- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
    --
    -- Default: 0
    --
    -- Constraints:
    --
    -- -   If specified, this value must be set to a number from 0 to 259,200
    --     (72 hours).
    backtrackWindow :: Core.Maybe Core.Integer,
    -- | A value that indicates whether the modifications in this request and any
    -- pending modifications are asynchronously applied as soon as possible,
    -- regardless of the @PreferredMaintenanceWindow@ setting for the DB
    -- cluster. If this parameter is disabled, changes to the DB cluster are
    -- applied during the next maintenance window.
    --
    -- The @ApplyImmediately@ parameter only affects the
    -- @EnableIAMDatabaseAuthentication@, @MasterUserPassword@, and
    -- @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is
    -- disabled, then changes to the @EnableIAMDatabaseAuthentication@,
    -- @MasterUserPassword@, and @NewDBClusterIdentifier@ values are applied
    -- during the next maintenance window. All other changes are applied
    -- immediately, regardless of the value of the @ApplyImmediately@
    -- parameter.
    --
    -- By default, this parameter is disabled.
    applyImmediately :: Core.Maybe Core.Bool,
    -- | The name of the DB cluster parameter group to use for the DB cluster.
    dbClusterParameterGroupName :: Core.Maybe Core.Text,
    -- | The DB cluster identifier for the cluster being modified. This parameter
    -- isn\'t case-sensitive.
    --
    -- Constraints: This identifier must match the identifier of an existing DB
    -- cluster.
    dbClusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPeriod', 'modifyDBCluster_backupRetentionPeriod' - The number of days for which automated backups are retained. You must
-- specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- 'deletionProtection', 'modifyDBCluster_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
--
-- 'preferredBackupWindow', 'modifyDBCluster_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
--
-- 'enableIAMDatabaseAuthentication', 'modifyDBCluster_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
--
-- 'dbInstanceParameterGroupName', 'modifyDBCluster_dbInstanceParameterGroupName' - The name of the DB parameter group to apply to all instances of the DB
-- cluster.
--
-- When you apply a parameter group using the
-- @DBInstanceParameterGroupName@ parameter, the DB cluster isn\'t rebooted
-- automatically. Also, parameter changes aren\'t applied during the next
-- maintenance window but instead are applied immediately.
--
-- Default: The existing name setting
--
-- Constraints:
--
-- -   The DB parameter group must be in the same DB parameter group family
--     as this DB cluster.
--
-- -   The @DBInstanceParameterGroupName@ parameter is only valid in
--     combination with the @AllowMajorVersionUpgrade@ parameter.
--
-- 'optionGroupName', 'modifyDBCluster_optionGroupName' - A value that indicates that the DB cluster should be associated with the
-- specified option group. Changing this parameter doesn\'t result in an
-- outage except in the following case, and the change is applied during
-- the next maintenance window unless the @ApplyImmediately@ is enabled for
-- this request. If the parameter change results in an option group that
-- enables OEM, this change can cause a brief (sub-second) period during
-- which new connections are rejected but existing connections are not
-- interrupted.
--
-- Permanent options can\'t be removed from an option group. The option
-- group can\'t be removed from a DB cluster once it is associated with a
-- DB cluster.
--
-- 'domain', 'modifyDBCluster_domain' - The Active Directory directory ID to move the DB cluster to. Specify
-- @none@ to remove the cluster from its current domain. The domain must be
-- created prior to this operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- 'allowMajorVersionUpgrade', 'modifyDBCluster_allowMajorVersionUpgrade' - A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a
-- value for the @EngineVersion@ parameter that is a different major
-- version than the DB cluster\'s current version.
--
-- 'scalingConfiguration', 'modifyDBCluster_scalingConfiguration' - The scaling properties of the DB cluster. You can only modify scaling
-- properties for DB clusters in @serverless@ DB engine mode.
--
-- 'masterUserPassword', 'modifyDBCluster_masterUserPassword' - The new password for the master database user. This password can contain
-- any printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- 'vpcSecurityGroupIds', 'modifyDBCluster_vpcSecurityGroupIds' - A list of VPC security groups that the DB cluster will belong to.
--
-- 'enableHttpEndpoint', 'modifyDBCluster_enableHttpEndpoint' - A value that indicates whether to enable the HTTP endpoint for an Aurora
-- Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service
-- API for running SQL queries on the Aurora Serverless DB cluster. You can
-- also query your database from inside the RDS console with the query
-- editor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
--
-- 'engineVersion', 'modifyDBCluster_engineVersion' - The version number of the database engine to which you want to upgrade.
-- Changing this parameter results in an outage. The change is applied
-- during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available engine versions for @aurora@ (for MySQL
-- 5.6-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL 5.7-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-postgresql@,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- 'preferredMaintenanceWindow', 'modifyDBCluster_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region, occurring on a random day of the
-- week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- 'enableGlobalWriteForwarding', 'modifyDBCluster_enableGlobalWriteForwarding' - A value that indicates whether to enable this DB cluster to forward
-- write operations to the primary cluster of an Aurora global database
-- (GlobalCluster). By default, write operations are not allowed on Aurora
-- DB clusters that are secondary clusters in an Aurora global database.
--
-- You can set this value only on Aurora DB clusters that are members of an
-- Aurora global database. With this parameter enabled, a secondary cluster
-- can forward writes to the current primary cluster and the resulting
-- changes are replicated back to this cluster. For the primary DB cluster
-- of an Aurora global database, this value is used immediately if the
-- primary is demoted by the FailoverGlobalCluster API operation, but it
-- does nothing until then.
--
-- 'port', 'modifyDBCluster_port' - The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
--
-- 'domainIAMRoleName', 'modifyDBCluster_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'newDBClusterIdentifier'', 'modifyDBCluster_newDBClusterIdentifier' - The new DB cluster identifier for the DB cluster when renaming a DB
-- cluster. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   The first character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
--
-- 'cloudwatchLogsExportConfiguration', 'modifyDBCluster_cloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB cluster.
--
-- 'copyTagsToSnapshot', 'modifyDBCluster_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB cluster to
-- snapshots of the DB cluster. The default is not to copy them.
--
-- 'backtrackWindow', 'modifyDBCluster_backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
--
-- 'applyImmediately', 'modifyDBCluster_applyImmediately' - A value that indicates whether the modifications in this request and any
-- pending modifications are asynchronously applied as soon as possible,
-- regardless of the @PreferredMaintenanceWindow@ setting for the DB
-- cluster. If this parameter is disabled, changes to the DB cluster are
-- applied during the next maintenance window.
--
-- The @ApplyImmediately@ parameter only affects the
-- @EnableIAMDatabaseAuthentication@, @MasterUserPassword@, and
-- @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is
-- disabled, then changes to the @EnableIAMDatabaseAuthentication@,
-- @MasterUserPassword@, and @NewDBClusterIdentifier@ values are applied
-- during the next maintenance window. All other changes are applied
-- immediately, regardless of the value of the @ApplyImmediately@
-- parameter.
--
-- By default, this parameter is disabled.
--
-- 'dbClusterParameterGroupName', 'modifyDBCluster_dbClusterParameterGroupName' - The name of the DB cluster parameter group to use for the DB cluster.
--
-- 'dbClusterIdentifier', 'modifyDBCluster_dbClusterIdentifier' - The DB cluster identifier for the cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints: This identifier must match the identifier of an existing DB
-- cluster.
newModifyDBCluster ::
  -- | 'dbClusterIdentifier'
  Core.Text ->
  ModifyDBCluster
newModifyDBCluster pDBClusterIdentifier_ =
  ModifyDBCluster'
    { backupRetentionPeriod =
        Core.Nothing,
      deletionProtection = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      enableIAMDatabaseAuthentication = Core.Nothing,
      dbInstanceParameterGroupName = Core.Nothing,
      optionGroupName = Core.Nothing,
      domain = Core.Nothing,
      allowMajorVersionUpgrade = Core.Nothing,
      scalingConfiguration = Core.Nothing,
      masterUserPassword = Core.Nothing,
      vpcSecurityGroupIds = Core.Nothing,
      enableHttpEndpoint = Core.Nothing,
      engineVersion = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      enableGlobalWriteForwarding = Core.Nothing,
      port = Core.Nothing,
      domainIAMRoleName = Core.Nothing,
      newDBClusterIdentifier' = Core.Nothing,
      cloudwatchLogsExportConfiguration = Core.Nothing,
      copyTagsToSnapshot = Core.Nothing,
      backtrackWindow = Core.Nothing,
      applyImmediately = Core.Nothing,
      dbClusterParameterGroupName = Core.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

-- | The number of days for which automated backups are retained. You must
-- specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
modifyDBCluster_backupRetentionPeriod :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Int)
modifyDBCluster_backupRetentionPeriod = Lens.lens (\ModifyDBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@ModifyDBCluster' {} a -> s {backupRetentionPeriod = a} :: ModifyDBCluster)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
modifyDBCluster_deletionProtection :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
modifyDBCluster_deletionProtection = Lens.lens (\ModifyDBCluster' {deletionProtection} -> deletionProtection) (\s@ModifyDBCluster' {} a -> s {deletionProtection = a} :: ModifyDBCluster)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
modifyDBCluster_preferredBackupWindow :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
modifyDBCluster_preferredBackupWindow = Lens.lens (\ModifyDBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@ModifyDBCluster' {} a -> s {preferredBackupWindow = a} :: ModifyDBCluster)

-- | A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
modifyDBCluster_enableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
modifyDBCluster_enableIAMDatabaseAuthentication = Lens.lens (\ModifyDBCluster' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@ModifyDBCluster' {} a -> s {enableIAMDatabaseAuthentication = a} :: ModifyDBCluster)

-- | The name of the DB parameter group to apply to all instances of the DB
-- cluster.
--
-- When you apply a parameter group using the
-- @DBInstanceParameterGroupName@ parameter, the DB cluster isn\'t rebooted
-- automatically. Also, parameter changes aren\'t applied during the next
-- maintenance window but instead are applied immediately.
--
-- Default: The existing name setting
--
-- Constraints:
--
-- -   The DB parameter group must be in the same DB parameter group family
--     as this DB cluster.
--
-- -   The @DBInstanceParameterGroupName@ parameter is only valid in
--     combination with the @AllowMajorVersionUpgrade@ parameter.
modifyDBCluster_dbInstanceParameterGroupName :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
modifyDBCluster_dbInstanceParameterGroupName = Lens.lens (\ModifyDBCluster' {dbInstanceParameterGroupName} -> dbInstanceParameterGroupName) (\s@ModifyDBCluster' {} a -> s {dbInstanceParameterGroupName = a} :: ModifyDBCluster)

-- | A value that indicates that the DB cluster should be associated with the
-- specified option group. Changing this parameter doesn\'t result in an
-- outage except in the following case, and the change is applied during
-- the next maintenance window unless the @ApplyImmediately@ is enabled for
-- this request. If the parameter change results in an option group that
-- enables OEM, this change can cause a brief (sub-second) period during
-- which new connections are rejected but existing connections are not
-- interrupted.
--
-- Permanent options can\'t be removed from an option group. The option
-- group can\'t be removed from a DB cluster once it is associated with a
-- DB cluster.
modifyDBCluster_optionGroupName :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
modifyDBCluster_optionGroupName = Lens.lens (\ModifyDBCluster' {optionGroupName} -> optionGroupName) (\s@ModifyDBCluster' {} a -> s {optionGroupName = a} :: ModifyDBCluster)

-- | The Active Directory directory ID to move the DB cluster to. Specify
-- @none@ to remove the cluster from its current domain. The domain must be
-- created prior to this operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
modifyDBCluster_domain :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
modifyDBCluster_domain = Lens.lens (\ModifyDBCluster' {domain} -> domain) (\s@ModifyDBCluster' {} a -> s {domain = a} :: ModifyDBCluster)

-- | A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a
-- value for the @EngineVersion@ parameter that is a different major
-- version than the DB cluster\'s current version.
modifyDBCluster_allowMajorVersionUpgrade :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
modifyDBCluster_allowMajorVersionUpgrade = Lens.lens (\ModifyDBCluster' {allowMajorVersionUpgrade} -> allowMajorVersionUpgrade) (\s@ModifyDBCluster' {} a -> s {allowMajorVersionUpgrade = a} :: ModifyDBCluster)

-- | The scaling properties of the DB cluster. You can only modify scaling
-- properties for DB clusters in @serverless@ DB engine mode.
modifyDBCluster_scalingConfiguration :: Lens.Lens' ModifyDBCluster (Core.Maybe ScalingConfiguration)
modifyDBCluster_scalingConfiguration = Lens.lens (\ModifyDBCluster' {scalingConfiguration} -> scalingConfiguration) (\s@ModifyDBCluster' {} a -> s {scalingConfiguration = a} :: ModifyDBCluster)

-- | The new password for the master database user. This password can contain
-- any printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
modifyDBCluster_masterUserPassword :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
modifyDBCluster_masterUserPassword = Lens.lens (\ModifyDBCluster' {masterUserPassword} -> masterUserPassword) (\s@ModifyDBCluster' {} a -> s {masterUserPassword = a} :: ModifyDBCluster)

-- | A list of VPC security groups that the DB cluster will belong to.
modifyDBCluster_vpcSecurityGroupIds :: Lens.Lens' ModifyDBCluster (Core.Maybe [Core.Text])
modifyDBCluster_vpcSecurityGroupIds = Lens.lens (\ModifyDBCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@ModifyDBCluster' {} a -> s {vpcSecurityGroupIds = a} :: ModifyDBCluster) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates whether to enable the HTTP endpoint for an Aurora
-- Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service
-- API for running SQL queries on the Aurora Serverless DB cluster. You can
-- also query your database from inside the RDS console with the query
-- editor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
modifyDBCluster_enableHttpEndpoint :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
modifyDBCluster_enableHttpEndpoint = Lens.lens (\ModifyDBCluster' {enableHttpEndpoint} -> enableHttpEndpoint) (\s@ModifyDBCluster' {} a -> s {enableHttpEndpoint = a} :: ModifyDBCluster)

-- | The version number of the database engine to which you want to upgrade.
-- Changing this parameter results in an outage. The change is applied
-- during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available engine versions for @aurora@ (for MySQL
-- 5.6-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL 5.7-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-postgresql@,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
modifyDBCluster_engineVersion :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
modifyDBCluster_engineVersion = Lens.lens (\ModifyDBCluster' {engineVersion} -> engineVersion) (\s@ModifyDBCluster' {} a -> s {engineVersion = a} :: ModifyDBCluster)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region, occurring on a random day of the
-- week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
modifyDBCluster_preferredMaintenanceWindow :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
modifyDBCluster_preferredMaintenanceWindow = Lens.lens (\ModifyDBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyDBCluster' {} a -> s {preferredMaintenanceWindow = a} :: ModifyDBCluster)

-- | A value that indicates whether to enable this DB cluster to forward
-- write operations to the primary cluster of an Aurora global database
-- (GlobalCluster). By default, write operations are not allowed on Aurora
-- DB clusters that are secondary clusters in an Aurora global database.
--
-- You can set this value only on Aurora DB clusters that are members of an
-- Aurora global database. With this parameter enabled, a secondary cluster
-- can forward writes to the current primary cluster and the resulting
-- changes are replicated back to this cluster. For the primary DB cluster
-- of an Aurora global database, this value is used immediately if the
-- primary is demoted by the FailoverGlobalCluster API operation, but it
-- does nothing until then.
modifyDBCluster_enableGlobalWriteForwarding :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
modifyDBCluster_enableGlobalWriteForwarding = Lens.lens (\ModifyDBCluster' {enableGlobalWriteForwarding} -> enableGlobalWriteForwarding) (\s@ModifyDBCluster' {} a -> s {enableGlobalWriteForwarding = a} :: ModifyDBCluster)

-- | The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
modifyDBCluster_port :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Int)
modifyDBCluster_port = Lens.lens (\ModifyDBCluster' {port} -> port) (\s@ModifyDBCluster' {} a -> s {port = a} :: ModifyDBCluster)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
modifyDBCluster_domainIAMRoleName :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
modifyDBCluster_domainIAMRoleName = Lens.lens (\ModifyDBCluster' {domainIAMRoleName} -> domainIAMRoleName) (\s@ModifyDBCluster' {} a -> s {domainIAMRoleName = a} :: ModifyDBCluster)

-- | The new DB cluster identifier for the DB cluster when renaming a DB
-- cluster. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   The first character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
modifyDBCluster_newDBClusterIdentifier :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
modifyDBCluster_newDBClusterIdentifier = Lens.lens (\ModifyDBCluster' {newDBClusterIdentifier'} -> newDBClusterIdentifier') (\s@ModifyDBCluster' {} a -> s {newDBClusterIdentifier' = a} :: ModifyDBCluster)

-- | The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB cluster.
modifyDBCluster_cloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBCluster (Core.Maybe CloudwatchLogsExportConfiguration)
modifyDBCluster_cloudwatchLogsExportConfiguration = Lens.lens (\ModifyDBCluster' {cloudwatchLogsExportConfiguration} -> cloudwatchLogsExportConfiguration) (\s@ModifyDBCluster' {} a -> s {cloudwatchLogsExportConfiguration = a} :: ModifyDBCluster)

-- | A value that indicates whether to copy all tags from the DB cluster to
-- snapshots of the DB cluster. The default is not to copy them.
modifyDBCluster_copyTagsToSnapshot :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
modifyDBCluster_copyTagsToSnapshot = Lens.lens (\ModifyDBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@ModifyDBCluster' {} a -> s {copyTagsToSnapshot = a} :: ModifyDBCluster)

-- | The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
modifyDBCluster_backtrackWindow :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Integer)
modifyDBCluster_backtrackWindow = Lens.lens (\ModifyDBCluster' {backtrackWindow} -> backtrackWindow) (\s@ModifyDBCluster' {} a -> s {backtrackWindow = a} :: ModifyDBCluster)

-- | A value that indicates whether the modifications in this request and any
-- pending modifications are asynchronously applied as soon as possible,
-- regardless of the @PreferredMaintenanceWindow@ setting for the DB
-- cluster. If this parameter is disabled, changes to the DB cluster are
-- applied during the next maintenance window.
--
-- The @ApplyImmediately@ parameter only affects the
-- @EnableIAMDatabaseAuthentication@, @MasterUserPassword@, and
-- @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is
-- disabled, then changes to the @EnableIAMDatabaseAuthentication@,
-- @MasterUserPassword@, and @NewDBClusterIdentifier@ values are applied
-- during the next maintenance window. All other changes are applied
-- immediately, regardless of the value of the @ApplyImmediately@
-- parameter.
--
-- By default, this parameter is disabled.
modifyDBCluster_applyImmediately :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Bool)
modifyDBCluster_applyImmediately = Lens.lens (\ModifyDBCluster' {applyImmediately} -> applyImmediately) (\s@ModifyDBCluster' {} a -> s {applyImmediately = a} :: ModifyDBCluster)

-- | The name of the DB cluster parameter group to use for the DB cluster.
modifyDBCluster_dbClusterParameterGroupName :: Lens.Lens' ModifyDBCluster (Core.Maybe Core.Text)
modifyDBCluster_dbClusterParameterGroupName = Lens.lens (\ModifyDBCluster' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@ModifyDBCluster' {} a -> s {dbClusterParameterGroupName = a} :: ModifyDBCluster)

-- | The DB cluster identifier for the cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints: This identifier must match the identifier of an existing DB
-- cluster.
modifyDBCluster_dbClusterIdentifier :: Lens.Lens' ModifyDBCluster Core.Text
modifyDBCluster_dbClusterIdentifier = Lens.lens (\ModifyDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@ModifyDBCluster' {} a -> s {dbClusterIdentifier = a} :: ModifyDBCluster)

instance Core.AWSRequest ModifyDBCluster where
  type
    AWSResponse ModifyDBCluster =
      ModifyDBClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterResult"
      ( \s h x ->
          ModifyDBClusterResponse'
            Core.<$> (x Core..@? "DBCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyDBCluster

instance Core.NFData ModifyDBCluster

instance Core.ToHeaders ModifyDBCluster where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyDBCluster where
  toPath = Core.const "/"

instance Core.ToQuery ModifyDBCluster where
  toQuery ModifyDBCluster' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyDBCluster" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "DeletionProtection" Core.=: deletionProtection,
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "DBInstanceParameterGroupName"
          Core.=: dbInstanceParameterGroupName,
        "OptionGroupName" Core.=: optionGroupName,
        "Domain" Core.=: domain,
        "AllowMajorVersionUpgrade"
          Core.=: allowMajorVersionUpgrade,
        "ScalingConfiguration" Core.=: scalingConfiguration,
        "MasterUserPassword" Core.=: masterUserPassword,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Core.<$> vpcSecurityGroupIds
            ),
        "EnableHttpEndpoint" Core.=: enableHttpEndpoint,
        "EngineVersion" Core.=: engineVersion,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "EnableGlobalWriteForwarding"
          Core.=: enableGlobalWriteForwarding,
        "Port" Core.=: port,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "NewDBClusterIdentifier"
          Core.=: newDBClusterIdentifier',
        "CloudwatchLogsExportConfiguration"
          Core.=: cloudwatchLogsExportConfiguration,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "BacktrackWindow" Core.=: backtrackWindow,
        "ApplyImmediately" Core.=: applyImmediately,
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier
      ]

-- | /See:/ 'newModifyDBClusterResponse' smart constructor.
data ModifyDBClusterResponse = ModifyDBClusterResponse'
  { dbCluster :: Core.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'modifyDBClusterResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'modifyDBClusterResponse_httpStatus' - The response's http status code.
newModifyDBClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyDBClusterResponse
newModifyDBClusterResponse pHttpStatus_ =
  ModifyDBClusterResponse'
    { dbCluster = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBClusterResponse_dbCluster :: Lens.Lens' ModifyDBClusterResponse (Core.Maybe DBCluster)
modifyDBClusterResponse_dbCluster = Lens.lens (\ModifyDBClusterResponse' {dbCluster} -> dbCluster) (\s@ModifyDBClusterResponse' {} a -> s {dbCluster = a} :: ModifyDBClusterResponse)

-- | The response's http status code.
modifyDBClusterResponse_httpStatus :: Lens.Lens' ModifyDBClusterResponse Core.Int
modifyDBClusterResponse_httpStatus = Lens.lens (\ModifyDBClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyDBClusterResponse' {} a -> s {httpStatus = a} :: ModifyDBClusterResponse)

instance Core.NFData ModifyDBClusterResponse
