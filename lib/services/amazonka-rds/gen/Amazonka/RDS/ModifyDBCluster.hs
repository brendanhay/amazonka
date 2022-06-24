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
-- Module      : Amazonka.RDS.ModifyDBCluster
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
module Amazonka.RDS.ModifyDBCluster
  ( -- * Creating a Request
    ModifyDBCluster (..),
    newModifyDBCluster,

    -- * Request Lenses
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_port,
    modifyDBCluster_enableGlobalWriteForwarding,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_domainIAMRoleName,
    modifyDBCluster_dbInstanceParameterGroupName,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_allowMajorVersionUpgrade,
    modifyDBCluster_domain,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_enableIAMDatabaseAuthentication,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_enableHttpEndpoint,
    modifyDBCluster_backtrackWindow,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_scalingConfiguration,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_engineVersion,
    modifyDBCluster_dbClusterIdentifier,

    -- * Destructuring the Response
    ModifyDBClusterResponse (..),
    newModifyDBClusterResponse,

    -- * Response Lenses
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newModifyDBCluster' smart constructor.
data ModifyDBCluster = ModifyDBCluster'
  { -- | The new DB cluster identifier for the DB cluster when renaming a DB
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
    newDBClusterIdentifier' :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the DB cluster accepts connections.
    --
    -- Constraints: Value must be @1150-65535@
    --
    -- Default: The same port as the original DB cluster.
    port :: Prelude.Maybe Prelude.Int,
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
    enableGlobalWriteForwarding :: Prelude.Maybe Prelude.Bool,
    -- | A list of VPC security groups that the DB cluster will belong to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled, using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region. To view the time
    -- blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
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
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which automated backups are retained. You must
    -- specify a minimum value of 1.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to copy all tags from the DB cluster to
    -- snapshots of the DB cluster. The default is not to copy them.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
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
    dbInstanceParameterGroupName :: Prelude.Maybe Prelude.Text,
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
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether major version upgrades are allowed.
    --
    -- Constraints: You must allow major version upgrades when specifying a
    -- value for the @EngineVersion@ parameter that is a different major
    -- version than the DB cluster\'s current version.
    allowMajorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Active Directory directory ID to move the DB cluster to. Specify
    -- @none@ to remove the cluster from its current domain. The domain must be
    -- created prior to this operation.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon Aurora User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
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
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping is disabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The new password for the master database user. This password can contain
    -- any printable ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
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
    enableHttpEndpoint :: Prelude.Maybe Prelude.Bool,
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
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
    -- | The configuration setting for the log types to be enabled for export to
    -- CloudWatch Logs for a specific DB cluster.
    cloudwatchLogsExportConfiguration :: Prelude.Maybe CloudwatchLogsExportConfiguration,
    -- | The scaling properties of the DB cluster. You can only modify scaling
    -- properties for DB clusters in @serverless@ DB engine mode.
    scalingConfiguration :: Prelude.Maybe ScalingConfiguration,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region, occurring on a random
    -- day of the week. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
    -- in the /Amazon Aurora User Guide./
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster parameter group to use for the DB cluster.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
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
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The DB cluster identifier for the cluster being modified. This parameter
    -- isn\'t case-sensitive.
    --
    -- Constraints: This identifier must match the identifier of an existing DB
    -- cluster.
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'port', 'modifyDBCluster_port' - The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
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
-- 'vpcSecurityGroupIds', 'modifyDBCluster_vpcSecurityGroupIds' - A list of VPC security groups that the DB cluster will belong to.
--
-- 'preferredBackupWindow', 'modifyDBCluster_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To view the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
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
-- 'backupRetentionPeriod', 'modifyDBCluster_backupRetentionPeriod' - The number of days for which automated backups are retained. You must
-- specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- 'copyTagsToSnapshot', 'modifyDBCluster_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB cluster to
-- snapshots of the DB cluster. The default is not to copy them.
--
-- 'domainIAMRoleName', 'modifyDBCluster_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
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
-- 'allowMajorVersionUpgrade', 'modifyDBCluster_allowMajorVersionUpgrade' - A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a
-- value for the @EngineVersion@ parameter that is a different major
-- version than the DB cluster\'s current version.
--
-- 'domain', 'modifyDBCluster_domain' - The Active Directory directory ID to move the DB cluster to. Specify
-- @none@ to remove the cluster from its current domain. The domain must be
-- created prior to this operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
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
-- 'enableIAMDatabaseAuthentication', 'modifyDBCluster_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
--
-- 'masterUserPassword', 'modifyDBCluster_masterUserPassword' - The new password for the master database user. This password can contain
-- any printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
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
-- 'cloudwatchLogsExportConfiguration', 'modifyDBCluster_cloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB cluster.
--
-- 'scalingConfiguration', 'modifyDBCluster_scalingConfiguration' - The scaling properties of the DB cluster. You can only modify scaling
-- properties for DB clusters in @serverless@ DB engine mode.
--
-- 'deletionProtection', 'modifyDBCluster_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
--
-- 'preferredMaintenanceWindow', 'modifyDBCluster_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- 'dbClusterParameterGroupName', 'modifyDBCluster_dbClusterParameterGroupName' - The name of the DB cluster parameter group to use for the DB cluster.
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
-- 'dbClusterIdentifier', 'modifyDBCluster_dbClusterIdentifier' - The DB cluster identifier for the cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints: This identifier must match the identifier of an existing DB
-- cluster.
newModifyDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  ModifyDBCluster
newModifyDBCluster pDBClusterIdentifier_ =
  ModifyDBCluster'
    { newDBClusterIdentifier' =
        Prelude.Nothing,
      port = Prelude.Nothing,
      enableGlobalWriteForwarding = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      domainIAMRoleName = Prelude.Nothing,
      dbInstanceParameterGroupName = Prelude.Nothing,
      applyImmediately = Prelude.Nothing,
      allowMajorVersionUpgrade = Prelude.Nothing,
      domain = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      enableIAMDatabaseAuthentication = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      enableHttpEndpoint = Prelude.Nothing,
      backtrackWindow = Prelude.Nothing,
      cloudwatchLogsExportConfiguration = Prelude.Nothing,
      scalingConfiguration = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      dbClusterParameterGroupName = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

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
modifyDBCluster_newDBClusterIdentifier :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_newDBClusterIdentifier = Lens.lens (\ModifyDBCluster' {newDBClusterIdentifier'} -> newDBClusterIdentifier') (\s@ModifyDBCluster' {} a -> s {newDBClusterIdentifier' = a} :: ModifyDBCluster)

-- | The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
modifyDBCluster_port :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Int)
modifyDBCluster_port = Lens.lens (\ModifyDBCluster' {port} -> port) (\s@ModifyDBCluster' {} a -> s {port = a} :: ModifyDBCluster)

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
modifyDBCluster_enableGlobalWriteForwarding :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_enableGlobalWriteForwarding = Lens.lens (\ModifyDBCluster' {enableGlobalWriteForwarding} -> enableGlobalWriteForwarding) (\s@ModifyDBCluster' {} a -> s {enableGlobalWriteForwarding = a} :: ModifyDBCluster)

-- | A list of VPC security groups that the DB cluster will belong to.
modifyDBCluster_vpcSecurityGroupIds :: Lens.Lens' ModifyDBCluster (Prelude.Maybe [Prelude.Text])
modifyDBCluster_vpcSecurityGroupIds = Lens.lens (\ModifyDBCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@ModifyDBCluster' {} a -> s {vpcSecurityGroupIds = a} :: ModifyDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To view the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
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
modifyDBCluster_preferredBackupWindow :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_preferredBackupWindow = Lens.lens (\ModifyDBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@ModifyDBCluster' {} a -> s {preferredBackupWindow = a} :: ModifyDBCluster)

-- | The number of days for which automated backups are retained. You must
-- specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
modifyDBCluster_backupRetentionPeriod :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Int)
modifyDBCluster_backupRetentionPeriod = Lens.lens (\ModifyDBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@ModifyDBCluster' {} a -> s {backupRetentionPeriod = a} :: ModifyDBCluster)

-- | A value that indicates whether to copy all tags from the DB cluster to
-- snapshots of the DB cluster. The default is not to copy them.
modifyDBCluster_copyTagsToSnapshot :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_copyTagsToSnapshot = Lens.lens (\ModifyDBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@ModifyDBCluster' {} a -> s {copyTagsToSnapshot = a} :: ModifyDBCluster)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
modifyDBCluster_domainIAMRoleName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_domainIAMRoleName = Lens.lens (\ModifyDBCluster' {domainIAMRoleName} -> domainIAMRoleName) (\s@ModifyDBCluster' {} a -> s {domainIAMRoleName = a} :: ModifyDBCluster)

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
modifyDBCluster_dbInstanceParameterGroupName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_dbInstanceParameterGroupName = Lens.lens (\ModifyDBCluster' {dbInstanceParameterGroupName} -> dbInstanceParameterGroupName) (\s@ModifyDBCluster' {} a -> s {dbInstanceParameterGroupName = a} :: ModifyDBCluster)

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
modifyDBCluster_applyImmediately :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_applyImmediately = Lens.lens (\ModifyDBCluster' {applyImmediately} -> applyImmediately) (\s@ModifyDBCluster' {} a -> s {applyImmediately = a} :: ModifyDBCluster)

-- | A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a
-- value for the @EngineVersion@ parameter that is a different major
-- version than the DB cluster\'s current version.
modifyDBCluster_allowMajorVersionUpgrade :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_allowMajorVersionUpgrade = Lens.lens (\ModifyDBCluster' {allowMajorVersionUpgrade} -> allowMajorVersionUpgrade) (\s@ModifyDBCluster' {} a -> s {allowMajorVersionUpgrade = a} :: ModifyDBCluster)

-- | The Active Directory directory ID to move the DB cluster to. Specify
-- @none@ to remove the cluster from its current domain. The domain must be
-- created prior to this operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
modifyDBCluster_domain :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_domain = Lens.lens (\ModifyDBCluster' {domain} -> domain) (\s@ModifyDBCluster' {} a -> s {domain = a} :: ModifyDBCluster)

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
modifyDBCluster_optionGroupName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_optionGroupName = Lens.lens (\ModifyDBCluster' {optionGroupName} -> optionGroupName) (\s@ModifyDBCluster' {} a -> s {optionGroupName = a} :: ModifyDBCluster)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
modifyDBCluster_enableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_enableIAMDatabaseAuthentication = Lens.lens (\ModifyDBCluster' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@ModifyDBCluster' {} a -> s {enableIAMDatabaseAuthentication = a} :: ModifyDBCluster)

-- | The new password for the master database user. This password can contain
-- any printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
modifyDBCluster_masterUserPassword :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_masterUserPassword = Lens.lens (\ModifyDBCluster' {masterUserPassword} -> masterUserPassword) (\s@ModifyDBCluster' {} a -> s {masterUserPassword = a} :: ModifyDBCluster)

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
modifyDBCluster_enableHttpEndpoint :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_enableHttpEndpoint = Lens.lens (\ModifyDBCluster' {enableHttpEndpoint} -> enableHttpEndpoint) (\s@ModifyDBCluster' {} a -> s {enableHttpEndpoint = a} :: ModifyDBCluster)

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
modifyDBCluster_backtrackWindow :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Integer)
modifyDBCluster_backtrackWindow = Lens.lens (\ModifyDBCluster' {backtrackWindow} -> backtrackWindow) (\s@ModifyDBCluster' {} a -> s {backtrackWindow = a} :: ModifyDBCluster)

-- | The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB cluster.
modifyDBCluster_cloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBCluster (Prelude.Maybe CloudwatchLogsExportConfiguration)
modifyDBCluster_cloudwatchLogsExportConfiguration = Lens.lens (\ModifyDBCluster' {cloudwatchLogsExportConfiguration} -> cloudwatchLogsExportConfiguration) (\s@ModifyDBCluster' {} a -> s {cloudwatchLogsExportConfiguration = a} :: ModifyDBCluster)

-- | The scaling properties of the DB cluster. You can only modify scaling
-- properties for DB clusters in @serverless@ DB engine mode.
modifyDBCluster_scalingConfiguration :: Lens.Lens' ModifyDBCluster (Prelude.Maybe ScalingConfiguration)
modifyDBCluster_scalingConfiguration = Lens.lens (\ModifyDBCluster' {scalingConfiguration} -> scalingConfiguration) (\s@ModifyDBCluster' {} a -> s {scalingConfiguration = a} :: ModifyDBCluster)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
modifyDBCluster_deletionProtection :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_deletionProtection = Lens.lens (\ModifyDBCluster' {deletionProtection} -> deletionProtection) (\s@ModifyDBCluster' {} a -> s {deletionProtection = a} :: ModifyDBCluster)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
modifyDBCluster_preferredMaintenanceWindow :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_preferredMaintenanceWindow = Lens.lens (\ModifyDBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyDBCluster' {} a -> s {preferredMaintenanceWindow = a} :: ModifyDBCluster)

-- | The name of the DB cluster parameter group to use for the DB cluster.
modifyDBCluster_dbClusterParameterGroupName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_dbClusterParameterGroupName = Lens.lens (\ModifyDBCluster' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@ModifyDBCluster' {} a -> s {dbClusterParameterGroupName = a} :: ModifyDBCluster)

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
modifyDBCluster_engineVersion :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_engineVersion = Lens.lens (\ModifyDBCluster' {engineVersion} -> engineVersion) (\s@ModifyDBCluster' {} a -> s {engineVersion = a} :: ModifyDBCluster)

-- | The DB cluster identifier for the cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints: This identifier must match the identifier of an existing DB
-- cluster.
modifyDBCluster_dbClusterIdentifier :: Lens.Lens' ModifyDBCluster Prelude.Text
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
            Prelude.<$> (x Core..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBCluster where
  hashWithSalt _salt ModifyDBCluster' {..} =
    _salt
      `Prelude.hashWithSalt` newDBClusterIdentifier'
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` enableGlobalWriteForwarding
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` dbInstanceParameterGroupName
      `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` allowMajorVersionUpgrade
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` enableHttpEndpoint
      `Prelude.hashWithSalt` backtrackWindow
      `Prelude.hashWithSalt` cloudwatchLogsExportConfiguration
      `Prelude.hashWithSalt` scalingConfiguration
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData ModifyDBCluster where
  rnf ModifyDBCluster' {..} =
    Prelude.rnf newDBClusterIdentifier'
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf enableGlobalWriteForwarding
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf dbInstanceParameterGroupName
      `Prelude.seq` Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf allowMajorVersionUpgrade
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf
        enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf enableHttpEndpoint
      `Prelude.seq` Prelude.rnf backtrackWindow
      `Prelude.seq` Prelude.rnf
        cloudwatchLogsExportConfiguration
      `Prelude.seq` Prelude.rnf scalingConfiguration
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        dbClusterIdentifier

instance Core.ToHeaders ModifyDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyDBCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyDBCluster where
  toQuery ModifyDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyDBCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "NewDBClusterIdentifier"
          Core.=: newDBClusterIdentifier',
        "Port" Core.=: port,
        "EnableGlobalWriteForwarding"
          Core.=: enableGlobalWriteForwarding,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "DBInstanceParameterGroupName"
          Core.=: dbInstanceParameterGroupName,
        "ApplyImmediately" Core.=: applyImmediately,
        "AllowMajorVersionUpgrade"
          Core.=: allowMajorVersionUpgrade,
        "Domain" Core.=: domain,
        "OptionGroupName" Core.=: optionGroupName,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "MasterUserPassword" Core.=: masterUserPassword,
        "EnableHttpEndpoint" Core.=: enableHttpEndpoint,
        "BacktrackWindow" Core.=: backtrackWindow,
        "CloudwatchLogsExportConfiguration"
          Core.=: cloudwatchLogsExportConfiguration,
        "ScalingConfiguration" Core.=: scalingConfiguration,
        "DeletionProtection" Core.=: deletionProtection,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "EngineVersion" Core.=: engineVersion,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier
      ]

-- | /See:/ 'newModifyDBClusterResponse' smart constructor.
data ModifyDBClusterResponse = ModifyDBClusterResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyDBClusterResponse
newModifyDBClusterResponse pHttpStatus_ =
  ModifyDBClusterResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBClusterResponse_dbCluster :: Lens.Lens' ModifyDBClusterResponse (Prelude.Maybe DBCluster)
modifyDBClusterResponse_dbCluster = Lens.lens (\ModifyDBClusterResponse' {dbCluster} -> dbCluster) (\s@ModifyDBClusterResponse' {} a -> s {dbCluster = a} :: ModifyDBClusterResponse)

-- | The response's http status code.
modifyDBClusterResponse_httpStatus :: Lens.Lens' ModifyDBClusterResponse Prelude.Int
modifyDBClusterResponse_httpStatus = Lens.lens (\ModifyDBClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyDBClusterResponse' {} a -> s {httpStatus = a} :: ModifyDBClusterResponse)

instance Prelude.NFData ModifyDBClusterResponse where
  rnf ModifyDBClusterResponse' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
