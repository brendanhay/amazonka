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
-- Module      : Amazonka.Neptune.ModifyDBCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for a DB cluster. You can change one or more database
-- configuration parameters by specifying these parameters and the new
-- values in the request.
module Amazonka.Neptune.ModifyDBCluster
  ( -- * Creating a Request
    ModifyDBCluster (..),
    newModifyDBCluster,

    -- * Request Lenses
    modifyDBCluster_allowMajorVersionUpgrade,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_dbInstanceParameterGroupName,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_enableIAMDatabaseAuthentication,
    modifyDBCluster_engineVersion,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_port,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_serverlessV2ScalingConfiguration,
    modifyDBCluster_vpcSecurityGroupIds,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyDBCluster' smart constructor.
data ModifyDBCluster = ModifyDBCluster'
  { -- | A value that indicates whether upgrades between different major versions
    -- are allowed.
    --
    -- Constraints: You must set the allow-major-version-upgrade flag when
    -- providing an @EngineVersion@ parameter that uses a different major
    -- version than the DB cluster\'s current version.
    allowMajorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | A value that specifies whether the modifications in this request and any
    -- pending modifications are asynchronously applied as soon as possible,
    -- regardless of the @PreferredMaintenanceWindow@ setting for the DB
    -- cluster. If this parameter is set to @false@, changes to the DB cluster
    -- are applied during the next maintenance window.
    --
    -- The @ApplyImmediately@ parameter only affects @NewDBClusterIdentifier@
    -- values. If you set the @ApplyImmediately@ parameter value to false, then
    -- changes to @NewDBClusterIdentifier@ values are applied during the next
    -- maintenance window. All other changes are applied immediately,
    -- regardless of the value of the @ApplyImmediately@ parameter.
    --
    -- Default: @false@
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | The number of days for which automated backups are retained. You must
    -- specify a minimum value of 1.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The configuration setting for the log types to be enabled for export to
    -- CloudWatch Logs for a specific DB cluster.
    cloudwatchLogsExportConfiguration :: Prelude.Maybe CloudwatchLogsExportConfiguration,
    -- | /If set to @true@, tags are copied to any snapshot of the DB cluster
    -- that is created./
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The name of the DB cluster parameter group to use for the DB cluster.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group to apply to all instances of the DB
    -- cluster.
    --
    -- When you apply a parameter group using @DBInstanceParameterGroupName@,
    -- parameter changes aren\'t applied during the next maintenance window but
    -- instead are applied immediately.
    --
    -- Default: The existing name setting
    --
    -- Constraints:
    --
    -- -   The DB parameter group must be in the same DB parameter group family
    --     as the target DB cluster version.
    --
    -- -   The @DBInstanceParameterGroupName@ parameter is only valid in
    --     combination with the @AllowMajorVersionUpgrade@ parameter.
    dbInstanceParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | True to enable mapping of Amazon Identity and Access Management (IAM)
    -- accounts to database accounts, and otherwise false.
    --
    -- Default: @false@
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the database engine to which you want to upgrade.
    -- Changing this parameter results in an outage. The change is applied
    -- during the next maintenance window unless the @ApplyImmediately@
    -- parameter is set to true.
    --
    -- For a list of valid engine versions, see
    -- <https://docs.aws.amazon.com/neptune/latest/userguide/engine-releases.html Engine Releases for Amazon Neptune>,
    -- or call
    -- <https://docs.aws.amazon.com/neptune/latest/userguide/api-other-apis.html#DescribeDBEngineVersions DescribeDBEngineVersions>.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The new DB cluster identifier for the DB cluster when renaming a DB
    -- cluster. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    -- -   The first character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-cluster2@
    newDBClusterIdentifier' :: Prelude.Maybe Prelude.Text,
    -- | /Not supported by Neptune./
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the DB cluster accepts connections.
    --
    -- Constraints: Value must be @1150-65535@
    --
    -- Default: The same port as the original DB cluster.
    port :: Prelude.Maybe Prelude.Int,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled, using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Region.
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
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Region, occurring on a random day of the
    -- week.
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    serverlessV2ScalingConfiguration :: Prelude.Maybe ServerlessV2ScalingConfiguration,
    -- | A list of VPC security groups that the DB cluster will belong to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The DB cluster identifier for the cluster being modified. This parameter
    -- is not case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBCluster.
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
-- 'allowMajorVersionUpgrade', 'modifyDBCluster_allowMajorVersionUpgrade' - A value that indicates whether upgrades between different major versions
-- are allowed.
--
-- Constraints: You must set the allow-major-version-upgrade flag when
-- providing an @EngineVersion@ parameter that uses a different major
-- version than the DB cluster\'s current version.
--
-- 'applyImmediately', 'modifyDBCluster_applyImmediately' - A value that specifies whether the modifications in this request and any
-- pending modifications are asynchronously applied as soon as possible,
-- regardless of the @PreferredMaintenanceWindow@ setting for the DB
-- cluster. If this parameter is set to @false@, changes to the DB cluster
-- are applied during the next maintenance window.
--
-- The @ApplyImmediately@ parameter only affects @NewDBClusterIdentifier@
-- values. If you set the @ApplyImmediately@ parameter value to false, then
-- changes to @NewDBClusterIdentifier@ values are applied during the next
-- maintenance window. All other changes are applied immediately,
-- regardless of the value of the @ApplyImmediately@ parameter.
--
-- Default: @false@
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
-- 'cloudwatchLogsExportConfiguration', 'modifyDBCluster_cloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB cluster.
--
-- 'copyTagsToSnapshot', 'modifyDBCluster_copyTagsToSnapshot' - /If set to @true@, tags are copied to any snapshot of the DB cluster
-- that is created./
--
-- 'dbClusterParameterGroupName', 'modifyDBCluster_dbClusterParameterGroupName' - The name of the DB cluster parameter group to use for the DB cluster.
--
-- 'dbInstanceParameterGroupName', 'modifyDBCluster_dbInstanceParameterGroupName' - The name of the DB parameter group to apply to all instances of the DB
-- cluster.
--
-- When you apply a parameter group using @DBInstanceParameterGroupName@,
-- parameter changes aren\'t applied during the next maintenance window but
-- instead are applied immediately.
--
-- Default: The existing name setting
--
-- Constraints:
--
-- -   The DB parameter group must be in the same DB parameter group family
--     as the target DB cluster version.
--
-- -   The @DBInstanceParameterGroupName@ parameter is only valid in
--     combination with the @AllowMajorVersionUpgrade@ parameter.
--
-- 'deletionProtection', 'modifyDBCluster_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
--
-- 'enableIAMDatabaseAuthentication', 'modifyDBCluster_enableIAMDatabaseAuthentication' - True to enable mapping of Amazon Identity and Access Management (IAM)
-- accounts to database accounts, and otherwise false.
--
-- Default: @false@
--
-- 'engineVersion', 'modifyDBCluster_engineVersion' - The version number of the database engine to which you want to upgrade.
-- Changing this parameter results in an outage. The change is applied
-- during the next maintenance window unless the @ApplyImmediately@
-- parameter is set to true.
--
-- For a list of valid engine versions, see
-- <https://docs.aws.amazon.com/neptune/latest/userguide/engine-releases.html Engine Releases for Amazon Neptune>,
-- or call
-- <https://docs.aws.amazon.com/neptune/latest/userguide/api-other-apis.html#DescribeDBEngineVersions DescribeDBEngineVersions>.
--
-- 'masterUserPassword', 'modifyDBCluster_masterUserPassword' - Not supported by Neptune.
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
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
--
-- 'optionGroupName', 'modifyDBCluster_optionGroupName' - /Not supported by Neptune./
--
-- 'port', 'modifyDBCluster_port' - The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
--
-- 'preferredBackupWindow', 'modifyDBCluster_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Region.
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
-- 'preferredMaintenanceWindow', 'modifyDBCluster_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Region, occurring on a random day of the
-- week.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- 'serverlessV2ScalingConfiguration', 'modifyDBCluster_serverlessV2ScalingConfiguration' - Undocumented member.
--
-- 'vpcSecurityGroupIds', 'modifyDBCluster_vpcSecurityGroupIds' - A list of VPC security groups that the DB cluster will belong to.
--
-- 'dbClusterIdentifier', 'modifyDBCluster_dbClusterIdentifier' - The DB cluster identifier for the cluster being modified. This parameter
-- is not case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
newModifyDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  ModifyDBCluster
newModifyDBCluster pDBClusterIdentifier_ =
  ModifyDBCluster'
    { allowMajorVersionUpgrade =
        Prelude.Nothing,
      applyImmediately = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      cloudwatchLogsExportConfiguration = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      dbClusterParameterGroupName = Prelude.Nothing,
      dbInstanceParameterGroupName = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      enableIAMDatabaseAuthentication = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      newDBClusterIdentifier' = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      port = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      serverlessV2ScalingConfiguration = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

-- | A value that indicates whether upgrades between different major versions
-- are allowed.
--
-- Constraints: You must set the allow-major-version-upgrade flag when
-- providing an @EngineVersion@ parameter that uses a different major
-- version than the DB cluster\'s current version.
modifyDBCluster_allowMajorVersionUpgrade :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_allowMajorVersionUpgrade = Lens.lens (\ModifyDBCluster' {allowMajorVersionUpgrade} -> allowMajorVersionUpgrade) (\s@ModifyDBCluster' {} a -> s {allowMajorVersionUpgrade = a} :: ModifyDBCluster)

-- | A value that specifies whether the modifications in this request and any
-- pending modifications are asynchronously applied as soon as possible,
-- regardless of the @PreferredMaintenanceWindow@ setting for the DB
-- cluster. If this parameter is set to @false@, changes to the DB cluster
-- are applied during the next maintenance window.
--
-- The @ApplyImmediately@ parameter only affects @NewDBClusterIdentifier@
-- values. If you set the @ApplyImmediately@ parameter value to false, then
-- changes to @NewDBClusterIdentifier@ values are applied during the next
-- maintenance window. All other changes are applied immediately,
-- regardless of the value of the @ApplyImmediately@ parameter.
--
-- Default: @false@
modifyDBCluster_applyImmediately :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_applyImmediately = Lens.lens (\ModifyDBCluster' {applyImmediately} -> applyImmediately) (\s@ModifyDBCluster' {} a -> s {applyImmediately = a} :: ModifyDBCluster)

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

-- | The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB cluster.
modifyDBCluster_cloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBCluster (Prelude.Maybe CloudwatchLogsExportConfiguration)
modifyDBCluster_cloudwatchLogsExportConfiguration = Lens.lens (\ModifyDBCluster' {cloudwatchLogsExportConfiguration} -> cloudwatchLogsExportConfiguration) (\s@ModifyDBCluster' {} a -> s {cloudwatchLogsExportConfiguration = a} :: ModifyDBCluster)

-- | /If set to @true@, tags are copied to any snapshot of the DB cluster
-- that is created./
modifyDBCluster_copyTagsToSnapshot :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_copyTagsToSnapshot = Lens.lens (\ModifyDBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@ModifyDBCluster' {} a -> s {copyTagsToSnapshot = a} :: ModifyDBCluster)

-- | The name of the DB cluster parameter group to use for the DB cluster.
modifyDBCluster_dbClusterParameterGroupName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_dbClusterParameterGroupName = Lens.lens (\ModifyDBCluster' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@ModifyDBCluster' {} a -> s {dbClusterParameterGroupName = a} :: ModifyDBCluster)

-- | The name of the DB parameter group to apply to all instances of the DB
-- cluster.
--
-- When you apply a parameter group using @DBInstanceParameterGroupName@,
-- parameter changes aren\'t applied during the next maintenance window but
-- instead are applied immediately.
--
-- Default: The existing name setting
--
-- Constraints:
--
-- -   The DB parameter group must be in the same DB parameter group family
--     as the target DB cluster version.
--
-- -   The @DBInstanceParameterGroupName@ parameter is only valid in
--     combination with the @AllowMajorVersionUpgrade@ parameter.
modifyDBCluster_dbInstanceParameterGroupName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_dbInstanceParameterGroupName = Lens.lens (\ModifyDBCluster' {dbInstanceParameterGroupName} -> dbInstanceParameterGroupName) (\s@ModifyDBCluster' {} a -> s {dbInstanceParameterGroupName = a} :: ModifyDBCluster)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
modifyDBCluster_deletionProtection :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_deletionProtection = Lens.lens (\ModifyDBCluster' {deletionProtection} -> deletionProtection) (\s@ModifyDBCluster' {} a -> s {deletionProtection = a} :: ModifyDBCluster)

-- | True to enable mapping of Amazon Identity and Access Management (IAM)
-- accounts to database accounts, and otherwise false.
--
-- Default: @false@
modifyDBCluster_enableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_enableIAMDatabaseAuthentication = Lens.lens (\ModifyDBCluster' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@ModifyDBCluster' {} a -> s {enableIAMDatabaseAuthentication = a} :: ModifyDBCluster)

-- | The version number of the database engine to which you want to upgrade.
-- Changing this parameter results in an outage. The change is applied
-- during the next maintenance window unless the @ApplyImmediately@
-- parameter is set to true.
--
-- For a list of valid engine versions, see
-- <https://docs.aws.amazon.com/neptune/latest/userguide/engine-releases.html Engine Releases for Amazon Neptune>,
-- or call
-- <https://docs.aws.amazon.com/neptune/latest/userguide/api-other-apis.html#DescribeDBEngineVersions DescribeDBEngineVersions>.
modifyDBCluster_engineVersion :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_engineVersion = Lens.lens (\ModifyDBCluster' {engineVersion} -> engineVersion) (\s@ModifyDBCluster' {} a -> s {engineVersion = a} :: ModifyDBCluster)

-- | Not supported by Neptune.
modifyDBCluster_masterUserPassword :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_masterUserPassword = Lens.lens (\ModifyDBCluster' {masterUserPassword} -> masterUserPassword) (\s@ModifyDBCluster' {} a -> s {masterUserPassword = a} :: ModifyDBCluster)

-- | The new DB cluster identifier for the DB cluster when renaming a DB
-- cluster. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   The first character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
modifyDBCluster_newDBClusterIdentifier :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_newDBClusterIdentifier = Lens.lens (\ModifyDBCluster' {newDBClusterIdentifier'} -> newDBClusterIdentifier') (\s@ModifyDBCluster' {} a -> s {newDBClusterIdentifier' = a} :: ModifyDBCluster)

-- | /Not supported by Neptune./
modifyDBCluster_optionGroupName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_optionGroupName = Lens.lens (\ModifyDBCluster' {optionGroupName} -> optionGroupName) (\s@ModifyDBCluster' {} a -> s {optionGroupName = a} :: ModifyDBCluster)

-- | The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
modifyDBCluster_port :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Int)
modifyDBCluster_port = Lens.lens (\ModifyDBCluster' {port} -> port) (\s@ModifyDBCluster' {} a -> s {port = a} :: ModifyDBCluster)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Region.
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

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Region, occurring on a random day of the
-- week.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
modifyDBCluster_preferredMaintenanceWindow :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_preferredMaintenanceWindow = Lens.lens (\ModifyDBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyDBCluster' {} a -> s {preferredMaintenanceWindow = a} :: ModifyDBCluster)

-- | Undocumented member.
modifyDBCluster_serverlessV2ScalingConfiguration :: Lens.Lens' ModifyDBCluster (Prelude.Maybe ServerlessV2ScalingConfiguration)
modifyDBCluster_serverlessV2ScalingConfiguration = Lens.lens (\ModifyDBCluster' {serverlessV2ScalingConfiguration} -> serverlessV2ScalingConfiguration) (\s@ModifyDBCluster' {} a -> s {serverlessV2ScalingConfiguration = a} :: ModifyDBCluster)

-- | A list of VPC security groups that the DB cluster will belong to.
modifyDBCluster_vpcSecurityGroupIds :: Lens.Lens' ModifyDBCluster (Prelude.Maybe [Prelude.Text])
modifyDBCluster_vpcSecurityGroupIds = Lens.lens (\ModifyDBCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@ModifyDBCluster' {} a -> s {vpcSecurityGroupIds = a} :: ModifyDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The DB cluster identifier for the cluster being modified. This parameter
-- is not case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
modifyDBCluster_dbClusterIdentifier :: Lens.Lens' ModifyDBCluster Prelude.Text
modifyDBCluster_dbClusterIdentifier = Lens.lens (\ModifyDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@ModifyDBCluster' {} a -> s {dbClusterIdentifier = a} :: ModifyDBCluster)

instance Core.AWSRequest ModifyDBCluster where
  type
    AWSResponse ModifyDBCluster =
      ModifyDBClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterResult"
      ( \s h x ->
          ModifyDBClusterResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBCluster where
  hashWithSalt _salt ModifyDBCluster' {..} =
    _salt
      `Prelude.hashWithSalt` allowMajorVersionUpgrade
      `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` cloudwatchLogsExportConfiguration
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbInstanceParameterGroupName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` newDBClusterIdentifier'
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` serverlessV2ScalingConfiguration
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData ModifyDBCluster where
  rnf ModifyDBCluster' {..} =
    Prelude.rnf allowMajorVersionUpgrade
      `Prelude.seq` Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf cloudwatchLogsExportConfiguration
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf dbInstanceParameterGroupName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf newDBClusterIdentifier'
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        serverlessV2ScalingConfiguration
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf dbClusterIdentifier

instance Data.ToHeaders ModifyDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyDBCluster where
  toQuery ModifyDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyDBCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AllowMajorVersionUpgrade"
          Data.=: allowMajorVersionUpgrade,
        "ApplyImmediately" Data.=: applyImmediately,
        "BackupRetentionPeriod"
          Data.=: backupRetentionPeriod,
        "CloudwatchLogsExportConfiguration"
          Data.=: cloudwatchLogsExportConfiguration,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName,
        "DBInstanceParameterGroupName"
          Data.=: dbInstanceParameterGroupName,
        "DeletionProtection" Data.=: deletionProtection,
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "EngineVersion" Data.=: engineVersion,
        "MasterUserPassword" Data.=: masterUserPassword,
        "NewDBClusterIdentifier"
          Data.=: newDBClusterIdentifier',
        "OptionGroupName" Data.=: optionGroupName,
        "Port" Data.=: port,
        "PreferredBackupWindow"
          Data.=: preferredBackupWindow,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "ServerlessV2ScalingConfiguration"
          Data.=: serverlessV2ScalingConfiguration,
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
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
