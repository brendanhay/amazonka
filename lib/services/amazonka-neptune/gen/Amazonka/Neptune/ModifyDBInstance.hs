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
-- Module      : Amazonka.Neptune.ModifyDBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies settings for a DB instance. You can change one or more database
-- configuration parameters by specifying these parameters and the new
-- values in the request. To learn what modifications you can make to your
-- DB instance, call DescribeValidDBInstanceModifications before you call
-- ModifyDBInstance.
module Amazonka.Neptune.ModifyDBInstance
  ( -- * Creating a Request
    ModifyDBInstance (..),
    newModifyDBInstance,

    -- * Request Lenses
    modifyDBInstance_allocatedStorage,
    modifyDBInstance_allowMajorVersionUpgrade,
    modifyDBInstance_applyImmediately,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_backupRetentionPeriod,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_cloudwatchLogsExportConfiguration,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_dbParameterGroupName,
    modifyDBInstance_dbPortNumber,
    modifyDBInstance_dbSecurityGroups,
    modifyDBInstance_dbSubnetGroupName,
    modifyDBInstance_deletionProtection,
    modifyDBInstance_domain,
    modifyDBInstance_domainIAMRoleName,
    modifyDBInstance_enableIAMDatabaseAuthentication,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_engineVersion,
    modifyDBInstance_iops,
    modifyDBInstance_licenseModel,
    modifyDBInstance_masterUserPassword,
    modifyDBInstance_monitoringInterval,
    modifyDBInstance_monitoringRoleArn,
    modifyDBInstance_multiAZ,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_optionGroupName,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_preferredBackupWindow,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_promotionTier,
    modifyDBInstance_publiclyAccessible,
    modifyDBInstance_storageType,
    modifyDBInstance_tdeCredentialArn,
    modifyDBInstance_tdeCredentialPassword,
    modifyDBInstance_vpcSecurityGroupIds,
    modifyDBInstance_dbInstanceIdentifier,

    -- * Destructuring the Response
    ModifyDBInstanceResponse (..),
    newModifyDBInstanceResponse,

    -- * Response Lenses
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyDBInstance' smart constructor.
data ModifyDBInstance = ModifyDBInstance'
  { -- | Not supported by Neptune.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Indicates that major version upgrades are allowed. Changing this
    -- parameter doesn\'t result in an outage and the change is asynchronously
    -- applied as soon as possible.
    allowMajorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the modifications in this request and any pending
    -- modifications are asynchronously applied as soon as possible, regardless
    -- of the @PreferredMaintenanceWindow@ setting for the DB instance.
    --
    -- If this parameter is set to @false@, changes to the DB instance are
    -- applied during the next maintenance window. Some parameter changes can
    -- cause an outage and are applied on the next call to RebootDBInstance, or
    -- the next failure reboot.
    --
    -- Default: @false@
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that minor version upgrades are applied automatically to the
    -- DB instance during the maintenance window. Changing this parameter
    -- doesn\'t result in an outage except in the following case and the change
    -- is asynchronously applied as soon as possible. An outage will result if
    -- this parameter is set to @true@ during the maintenance window, and a
    -- newer minor version is available, and Neptune has enabled auto patching
    -- for that engine version.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Not applicable. The retention period for automated backups is managed by
    -- the DB cluster. For more information, see ModifyDBCluster.
    --
    -- Default: Uses existing setting
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Indicates the certificate that needs to be associated with the instance.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The configuration setting for the log types to be enabled for export to
    -- CloudWatch Logs for a specific DB instance or DB cluster.
    cloudwatchLogsExportConfiguration :: Prelude.Maybe CloudwatchLogsExportConfiguration,
    -- | True to copy all tags from the DB instance to snapshots of the DB
    -- instance, and otherwise false. The default is false.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The new compute and memory capacity of the DB instance, for example,
    -- @db.m4.large@. Not all DB instance classes are available in all Amazon
    -- Regions.
    --
    -- If you modify the DB instance class, an outage occurs during the change.
    -- The change is applied during the next maintenance window, unless
    -- @ApplyImmediately@ is specified as @true@ for this request.
    --
    -- Default: Uses existing setting
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group to apply to the DB instance. Changing
    -- this setting doesn\'t result in an outage. The parameter group name
    -- itself is changed immediately, but the actual parameter changes are not
    -- applied until you reboot the instance without failover. The db instance
    -- will NOT be rebooted automatically and the parameter changes will NOT be
    -- applied during the next maintenance window.
    --
    -- Default: Uses existing setting
    --
    -- Constraints: The DB parameter group must be in the same DB parameter
    -- group family as this DB instance.
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the database accepts connections.
    --
    -- The value of the @DBPortNumber@ parameter must not match any of the port
    -- values specified for options in the option group for the DB instance.
    --
    -- Your database will restart when you change the @DBPortNumber@ value
    -- regardless of the value of the @ApplyImmediately@ parameter.
    --
    -- Default: @8182@
    dbPortNumber :: Prelude.Maybe Prelude.Int,
    -- | A list of DB security groups to authorize on this DB instance. Changing
    -- this setting doesn\'t result in an outage and the change is
    -- asynchronously applied as soon as possible.
    --
    -- Constraints:
    --
    -- -   If supplied, must match existing DBSecurityGroups.
    dbSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The new DB subnet group for the DB instance. You can use this parameter
    -- to move your DB instance to a different VPC.
    --
    -- Changing the subnet group causes an outage during the change. The change
    -- is applied during the next maintenance window, unless you specify @true@
    -- for the @ApplyImmediately@ parameter.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mySubnetGroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. See
    -- <https://docs.aws.amazon.com/neptune/latest/userguide/manage-console-instances-delete.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Not supported.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Not supported
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | True to enable mapping of Amazon Identity and Access Management (IAM)
    -- accounts to database accounts, and otherwise false.
    --
    -- You can enable IAM database authentication for the following database
    -- engines
    --
    -- Not applicable. Mapping Amazon IAM accounts to database accounts is
    -- managed by the DB cluster. For more information, see ModifyDBCluster.
    --
    -- Default: @false@
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | /(Not supported by Neptune)/
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the database engine to upgrade to. Currently,
    -- setting this parameter has no effect. To upgrade your database engine to
    -- the most recent release, use the ApplyPendingMaintenanceAction API.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The new Provisioned IOPS (I\/O operations per second) value for the
    -- instance.
    --
    -- Changing this setting doesn\'t result in an outage and the change is
    -- applied during the next maintenance window unless the @ApplyImmediately@
    -- parameter is set to @true@ for this request.
    --
    -- Default: Uses existing setting
    iops :: Prelude.Maybe Prelude.Int,
    -- | Not supported by Neptune.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance. To disable collecting
    -- Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set
    -- @MonitoringInterval@ to a value other than 0.
    --
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The ARN for the IAM role that permits Neptune to send enhanced
    -- monitoring metrics to Amazon CloudWatch Logs. For example,
    -- @arn:aws:iam:123456789012:role\/emaccess@.
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must
    -- supply a @MonitoringRoleArn@ value.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies if the DB instance is a Multi-AZ deployment. Changing this
    -- parameter doesn\'t result in an outage and the change is applied during
    -- the next maintenance window unless the @ApplyImmediately@ parameter is
    -- set to @true@ for this request.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The new DB instance identifier for the DB instance when renaming a DB
    -- instance. When you change the DB instance identifier, an instance reboot
    -- will occur immediately if you set @Apply Immediately@ to true, or will
    -- occur during the next maintenance window if @Apply Immediately@ to
    -- false. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @mydbinstance@
    newDBInstanceIdentifier' :: Prelude.Maybe Prelude.Text,
    -- | /(Not supported by Neptune)/
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | /(Not supported by Neptune)/
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled.
    --
    -- Not applicable. The daily time range for creating automated backups is
    -- managed by the DB cluster. For more information, see ModifyDBCluster.
    --
    -- Constraints:
    --
    -- -   Must be in the format hh24:mi-hh24:mi
    --
    -- -   Must be in Universal Time Coordinated (UTC)
    --
    -- -   Must not conflict with the preferred maintenance window
    --
    -- -   Must be at least 30 minutes
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range (in UTC) during which system maintenance can
    -- occur, which might result in an outage. Changing this parameter doesn\'t
    -- result in an outage, except in the following situation, and the change
    -- is asynchronously applied as soon as possible. If there are pending
    -- actions that cause a reboot, and the maintenance window is changed to
    -- include the current time, then changing this parameter will cause a
    -- reboot of the DB instance. If moving this window to the current time,
    -- there must be at least 30 minutes between the current time and end of
    -- the window to ensure pending changes are applied.
    --
    -- Default: Uses existing setting
    --
    -- Format: ddd:hh24:mi-ddd:hh24:mi
    --
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    --
    -- Constraints: Must be at least 30 minutes
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the order in which a Read Replica is promoted to
    -- the primary instance after a failure of the existing primary instance.
    --
    -- Default: 1
    --
    -- Valid Values: 0 - 15
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | This flag should no longer be used.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Not supported.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The password for the given ARN from the key store in order to access the
    -- device.
    tdeCredentialPassword :: Prelude.Maybe Prelude.Text,
    -- | A list of EC2 VPC security groups to authorize on this DB instance. This
    -- change is asynchronously applied as soon as possible.
    --
    -- Not applicable. The associated list of EC2 VPC security groups is
    -- managed by the DB cluster. For more information, see ModifyDBCluster.
    --
    -- Constraints:
    --
    -- -   If supplied, must match existing VpcSecurityGroupIds.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The DB instance identifier. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'modifyDBInstance_allocatedStorage' - Not supported by Neptune.
--
-- 'allowMajorVersionUpgrade', 'modifyDBInstance_allowMajorVersionUpgrade' - Indicates that major version upgrades are allowed. Changing this
-- parameter doesn\'t result in an outage and the change is asynchronously
-- applied as soon as possible.
--
-- 'applyImmediately', 'modifyDBInstance_applyImmediately' - Specifies whether the modifications in this request and any pending
-- modifications are asynchronously applied as soon as possible, regardless
-- of the @PreferredMaintenanceWindow@ setting for the DB instance.
--
-- If this parameter is set to @false@, changes to the DB instance are
-- applied during the next maintenance window. Some parameter changes can
-- cause an outage and are applied on the next call to RebootDBInstance, or
-- the next failure reboot.
--
-- Default: @false@
--
-- 'autoMinorVersionUpgrade', 'modifyDBInstance_autoMinorVersionUpgrade' - Indicates that minor version upgrades are applied automatically to the
-- DB instance during the maintenance window. Changing this parameter
-- doesn\'t result in an outage except in the following case and the change
-- is asynchronously applied as soon as possible. An outage will result if
-- this parameter is set to @true@ during the maintenance window, and a
-- newer minor version is available, and Neptune has enabled auto patching
-- for that engine version.
--
-- 'backupRetentionPeriod', 'modifyDBInstance_backupRetentionPeriod' - Not applicable. The retention period for automated backups is managed by
-- the DB cluster. For more information, see ModifyDBCluster.
--
-- Default: Uses existing setting
--
-- 'cACertificateIdentifier', 'modifyDBInstance_cACertificateIdentifier' - Indicates the certificate that needs to be associated with the instance.
--
-- 'cloudwatchLogsExportConfiguration', 'modifyDBInstance_cloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB instance or DB cluster.
--
-- 'copyTagsToSnapshot', 'modifyDBInstance_copyTagsToSnapshot' - True to copy all tags from the DB instance to snapshots of the DB
-- instance, and otherwise false. The default is false.
--
-- 'dbInstanceClass', 'modifyDBInstance_dbInstanceClass' - The new compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all Amazon
-- Regions.
--
-- If you modify the DB instance class, an outage occurs during the change.
-- The change is applied during the next maintenance window, unless
-- @ApplyImmediately@ is specified as @true@ for this request.
--
-- Default: Uses existing setting
--
-- 'dbParameterGroupName', 'modifyDBInstance_dbParameterGroupName' - The name of the DB parameter group to apply to the DB instance. Changing
-- this setting doesn\'t result in an outage. The parameter group name
-- itself is changed immediately, but the actual parameter changes are not
-- applied until you reboot the instance without failover. The db instance
-- will NOT be rebooted automatically and the parameter changes will NOT be
-- applied during the next maintenance window.
--
-- Default: Uses existing setting
--
-- Constraints: The DB parameter group must be in the same DB parameter
-- group family as this DB instance.
--
-- 'dbPortNumber', 'modifyDBInstance_dbPortNumber' - The port number on which the database accepts connections.
--
-- The value of the @DBPortNumber@ parameter must not match any of the port
-- values specified for options in the option group for the DB instance.
--
-- Your database will restart when you change the @DBPortNumber@ value
-- regardless of the value of the @ApplyImmediately@ parameter.
--
-- Default: @8182@
--
-- 'dbSecurityGroups', 'modifyDBInstance_dbSecurityGroups' - A list of DB security groups to authorize on this DB instance. Changing
-- this setting doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints:
--
-- -   If supplied, must match existing DBSecurityGroups.
--
-- 'dbSubnetGroupName', 'modifyDBInstance_dbSubnetGroupName' - The new DB subnet group for the DB instance. You can use this parameter
-- to move your DB instance to a different VPC.
--
-- Changing the subnet group causes an outage during the change. The change
-- is applied during the next maintenance window, unless you specify @true@
-- for the @ApplyImmediately@ parameter.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetGroup@
--
-- 'deletionProtection', 'modifyDBInstance_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. See
-- <https://docs.aws.amazon.com/neptune/latest/userguide/manage-console-instances-delete.html Deleting a DB Instance>.
--
-- 'domain', 'modifyDBInstance_domain' - Not supported.
--
-- 'domainIAMRoleName', 'modifyDBInstance_domainIAMRoleName' - Not supported
--
-- 'enableIAMDatabaseAuthentication', 'modifyDBInstance_enableIAMDatabaseAuthentication' - True to enable mapping of Amazon Identity and Access Management (IAM)
-- accounts to database accounts, and otherwise false.
--
-- You can enable IAM database authentication for the following database
-- engines
--
-- Not applicable. Mapping Amazon IAM accounts to database accounts is
-- managed by the DB cluster. For more information, see ModifyDBCluster.
--
-- Default: @false@
--
-- 'enablePerformanceInsights', 'modifyDBInstance_enablePerformanceInsights' - /(Not supported by Neptune)/
--
-- 'engineVersion', 'modifyDBInstance_engineVersion' - The version number of the database engine to upgrade to. Currently,
-- setting this parameter has no effect. To upgrade your database engine to
-- the most recent release, use the ApplyPendingMaintenanceAction API.
--
-- 'iops', 'modifyDBInstance_iops' - The new Provisioned IOPS (I\/O operations per second) value for the
-- instance.
--
-- Changing this setting doesn\'t result in an outage and the change is
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is set to @true@ for this request.
--
-- Default: Uses existing setting
--
-- 'licenseModel', 'modifyDBInstance_licenseModel' - Not supported by Neptune.
--
-- 'masterUserPassword', 'modifyDBInstance_masterUserPassword' - Not supported by Neptune.
--
-- 'monitoringInterval', 'modifyDBInstance_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- 'monitoringRoleArn', 'modifyDBInstance_monitoringRoleArn' - The ARN for the IAM role that permits Neptune to send enhanced
-- monitoring metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@.
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
--
-- 'multiAZ', 'modifyDBInstance_multiAZ' - Specifies if the DB instance is a Multi-AZ deployment. Changing this
-- parameter doesn\'t result in an outage and the change is applied during
-- the next maintenance window unless the @ApplyImmediately@ parameter is
-- set to @true@ for this request.
--
-- 'newDBInstanceIdentifier'', 'modifyDBInstance_newDBInstanceIdentifier' - The new DB instance identifier for the DB instance when renaming a DB
-- instance. When you change the DB instance identifier, an instance reboot
-- will occur immediately if you set @Apply Immediately@ to true, or will
-- occur during the next maintenance window if @Apply Immediately@ to
-- false. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
--
-- 'optionGroupName', 'modifyDBInstance_optionGroupName' - /(Not supported by Neptune)/
--
-- 'performanceInsightsKMSKeyId', 'modifyDBInstance_performanceInsightsKMSKeyId' - /(Not supported by Neptune)/
--
-- 'preferredBackupWindow', 'modifyDBInstance_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled.
--
-- Not applicable. The daily time range for creating automated backups is
-- managed by the DB cluster. For more information, see ModifyDBCluster.
--
-- Constraints:
--
-- -   Must be in the format hh24:mi-hh24:mi
--
-- -   Must be in Universal Time Coordinated (UTC)
--
-- -   Must not conflict with the preferred maintenance window
--
-- -   Must be at least 30 minutes
--
-- 'preferredMaintenanceWindow', 'modifyDBInstance_preferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can
-- occur, which might result in an outage. Changing this parameter doesn\'t
-- result in an outage, except in the following situation, and the change
-- is asynchronously applied as soon as possible. If there are pending
-- actions that cause a reboot, and the maintenance window is changed to
-- include the current time, then changing this parameter will cause a
-- reboot of the DB instance. If moving this window to the current time,
-- there must be at least 30 minutes between the current time and end of
-- the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
--
-- Format: ddd:hh24:mi-ddd:hh24:mi
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Must be at least 30 minutes
--
-- 'promotionTier', 'modifyDBInstance_promotionTier' - A value that specifies the order in which a Read Replica is promoted to
-- the primary instance after a failure of the existing primary instance.
--
-- Default: 1
--
-- Valid Values: 0 - 15
--
-- 'publiclyAccessible', 'modifyDBInstance_publiclyAccessible' - This flag should no longer be used.
--
-- 'storageType', 'modifyDBInstance_storageType' - Not supported.
--
-- 'tdeCredentialArn', 'modifyDBInstance_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'tdeCredentialPassword', 'modifyDBInstance_tdeCredentialPassword' - The password for the given ARN from the key store in order to access the
-- device.
--
-- 'vpcSecurityGroupIds', 'modifyDBInstance_vpcSecurityGroupIds' - A list of EC2 VPC security groups to authorize on this DB instance. This
-- change is asynchronously applied as soon as possible.
--
-- Not applicable. The associated list of EC2 VPC security groups is
-- managed by the DB cluster. For more information, see ModifyDBCluster.
--
-- Constraints:
--
-- -   If supplied, must match existing VpcSecurityGroupIds.
--
-- 'dbInstanceIdentifier', 'modifyDBInstance_dbInstanceIdentifier' - The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
newModifyDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  ModifyDBInstance
newModifyDBInstance pDBInstanceIdentifier_ =
  ModifyDBInstance'
    { allocatedStorage =
        Prelude.Nothing,
      allowMajorVersionUpgrade = Prelude.Nothing,
      applyImmediately = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      cACertificateIdentifier = Prelude.Nothing,
      cloudwatchLogsExportConfiguration = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      dbParameterGroupName = Prelude.Nothing,
      dbPortNumber = Prelude.Nothing,
      dbSecurityGroups = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      domain = Prelude.Nothing,
      domainIAMRoleName = Prelude.Nothing,
      enableIAMDatabaseAuthentication = Prelude.Nothing,
      enablePerformanceInsights = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      iops = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      monitoringInterval = Prelude.Nothing,
      monitoringRoleArn = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      newDBInstanceIdentifier' = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      performanceInsightsKMSKeyId = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      promotionTier = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      storageType = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      tdeCredentialPassword = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | Not supported by Neptune.
modifyDBInstance_allocatedStorage :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_allocatedStorage = Lens.lens (\ModifyDBInstance' {allocatedStorage} -> allocatedStorage) (\s@ModifyDBInstance' {} a -> s {allocatedStorage = a} :: ModifyDBInstance)

-- | Indicates that major version upgrades are allowed. Changing this
-- parameter doesn\'t result in an outage and the change is asynchronously
-- applied as soon as possible.
modifyDBInstance_allowMajorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_allowMajorVersionUpgrade = Lens.lens (\ModifyDBInstance' {allowMajorVersionUpgrade} -> allowMajorVersionUpgrade) (\s@ModifyDBInstance' {} a -> s {allowMajorVersionUpgrade = a} :: ModifyDBInstance)

-- | Specifies whether the modifications in this request and any pending
-- modifications are asynchronously applied as soon as possible, regardless
-- of the @PreferredMaintenanceWindow@ setting for the DB instance.
--
-- If this parameter is set to @false@, changes to the DB instance are
-- applied during the next maintenance window. Some parameter changes can
-- cause an outage and are applied on the next call to RebootDBInstance, or
-- the next failure reboot.
--
-- Default: @false@
modifyDBInstance_applyImmediately :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_applyImmediately = Lens.lens (\ModifyDBInstance' {applyImmediately} -> applyImmediately) (\s@ModifyDBInstance' {} a -> s {applyImmediately = a} :: ModifyDBInstance)

-- | Indicates that minor version upgrades are applied automatically to the
-- DB instance during the maintenance window. Changing this parameter
-- doesn\'t result in an outage except in the following case and the change
-- is asynchronously applied as soon as possible. An outage will result if
-- this parameter is set to @true@ during the maintenance window, and a
-- newer minor version is available, and Neptune has enabled auto patching
-- for that engine version.
modifyDBInstance_autoMinorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_autoMinorVersionUpgrade = Lens.lens (\ModifyDBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ModifyDBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: ModifyDBInstance)

-- | Not applicable. The retention period for automated backups is managed by
-- the DB cluster. For more information, see ModifyDBCluster.
--
-- Default: Uses existing setting
modifyDBInstance_backupRetentionPeriod :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_backupRetentionPeriod = Lens.lens (\ModifyDBInstance' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@ModifyDBInstance' {} a -> s {backupRetentionPeriod = a} :: ModifyDBInstance)

-- | Indicates the certificate that needs to be associated with the instance.
modifyDBInstance_cACertificateIdentifier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_cACertificateIdentifier = Lens.lens (\ModifyDBInstance' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@ModifyDBInstance' {} a -> s {cACertificateIdentifier = a} :: ModifyDBInstance)

-- | The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB instance or DB cluster.
modifyDBInstance_cloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBInstance (Prelude.Maybe CloudwatchLogsExportConfiguration)
modifyDBInstance_cloudwatchLogsExportConfiguration = Lens.lens (\ModifyDBInstance' {cloudwatchLogsExportConfiguration} -> cloudwatchLogsExportConfiguration) (\s@ModifyDBInstance' {} a -> s {cloudwatchLogsExportConfiguration = a} :: ModifyDBInstance)

-- | True to copy all tags from the DB instance to snapshots of the DB
-- instance, and otherwise false. The default is false.
modifyDBInstance_copyTagsToSnapshot :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_copyTagsToSnapshot = Lens.lens (\ModifyDBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@ModifyDBInstance' {} a -> s {copyTagsToSnapshot = a} :: ModifyDBInstance)

-- | The new compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all Amazon
-- Regions.
--
-- If you modify the DB instance class, an outage occurs during the change.
-- The change is applied during the next maintenance window, unless
-- @ApplyImmediately@ is specified as @true@ for this request.
--
-- Default: Uses existing setting
modifyDBInstance_dbInstanceClass :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_dbInstanceClass = Lens.lens (\ModifyDBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@ModifyDBInstance' {} a -> s {dbInstanceClass = a} :: ModifyDBInstance)

-- | The name of the DB parameter group to apply to the DB instance. Changing
-- this setting doesn\'t result in an outage. The parameter group name
-- itself is changed immediately, but the actual parameter changes are not
-- applied until you reboot the instance without failover. The db instance
-- will NOT be rebooted automatically and the parameter changes will NOT be
-- applied during the next maintenance window.
--
-- Default: Uses existing setting
--
-- Constraints: The DB parameter group must be in the same DB parameter
-- group family as this DB instance.
modifyDBInstance_dbParameterGroupName :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_dbParameterGroupName = Lens.lens (\ModifyDBInstance' {dbParameterGroupName} -> dbParameterGroupName) (\s@ModifyDBInstance' {} a -> s {dbParameterGroupName = a} :: ModifyDBInstance)

-- | The port number on which the database accepts connections.
--
-- The value of the @DBPortNumber@ parameter must not match any of the port
-- values specified for options in the option group for the DB instance.
--
-- Your database will restart when you change the @DBPortNumber@ value
-- regardless of the value of the @ApplyImmediately@ parameter.
--
-- Default: @8182@
modifyDBInstance_dbPortNumber :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_dbPortNumber = Lens.lens (\ModifyDBInstance' {dbPortNumber} -> dbPortNumber) (\s@ModifyDBInstance' {} a -> s {dbPortNumber = a} :: ModifyDBInstance)

-- | A list of DB security groups to authorize on this DB instance. Changing
-- this setting doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints:
--
-- -   If supplied, must match existing DBSecurityGroups.
modifyDBInstance_dbSecurityGroups :: Lens.Lens' ModifyDBInstance (Prelude.Maybe [Prelude.Text])
modifyDBInstance_dbSecurityGroups = Lens.lens (\ModifyDBInstance' {dbSecurityGroups} -> dbSecurityGroups) (\s@ModifyDBInstance' {} a -> s {dbSecurityGroups = a} :: ModifyDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The new DB subnet group for the DB instance. You can use this parameter
-- to move your DB instance to a different VPC.
--
-- Changing the subnet group causes an outage during the change. The change
-- is applied during the next maintenance window, unless you specify @true@
-- for the @ApplyImmediately@ parameter.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetGroup@
modifyDBInstance_dbSubnetGroupName :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_dbSubnetGroupName = Lens.lens (\ModifyDBInstance' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@ModifyDBInstance' {} a -> s {dbSubnetGroupName = a} :: ModifyDBInstance)

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. See
-- <https://docs.aws.amazon.com/neptune/latest/userguide/manage-console-instances-delete.html Deleting a DB Instance>.
modifyDBInstance_deletionProtection :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_deletionProtection = Lens.lens (\ModifyDBInstance' {deletionProtection} -> deletionProtection) (\s@ModifyDBInstance' {} a -> s {deletionProtection = a} :: ModifyDBInstance)

-- | Not supported.
modifyDBInstance_domain :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_domain = Lens.lens (\ModifyDBInstance' {domain} -> domain) (\s@ModifyDBInstance' {} a -> s {domain = a} :: ModifyDBInstance)

-- | Not supported
modifyDBInstance_domainIAMRoleName :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_domainIAMRoleName = Lens.lens (\ModifyDBInstance' {domainIAMRoleName} -> domainIAMRoleName) (\s@ModifyDBInstance' {} a -> s {domainIAMRoleName = a} :: ModifyDBInstance)

-- | True to enable mapping of Amazon Identity and Access Management (IAM)
-- accounts to database accounts, and otherwise false.
--
-- You can enable IAM database authentication for the following database
-- engines
--
-- Not applicable. Mapping Amazon IAM accounts to database accounts is
-- managed by the DB cluster. For more information, see ModifyDBCluster.
--
-- Default: @false@
modifyDBInstance_enableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_enableIAMDatabaseAuthentication = Lens.lens (\ModifyDBInstance' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@ModifyDBInstance' {} a -> s {enableIAMDatabaseAuthentication = a} :: ModifyDBInstance)

-- | /(Not supported by Neptune)/
modifyDBInstance_enablePerformanceInsights :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_enablePerformanceInsights = Lens.lens (\ModifyDBInstance' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@ModifyDBInstance' {} a -> s {enablePerformanceInsights = a} :: ModifyDBInstance)

-- | The version number of the database engine to upgrade to. Currently,
-- setting this parameter has no effect. To upgrade your database engine to
-- the most recent release, use the ApplyPendingMaintenanceAction API.
modifyDBInstance_engineVersion :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_engineVersion = Lens.lens (\ModifyDBInstance' {engineVersion} -> engineVersion) (\s@ModifyDBInstance' {} a -> s {engineVersion = a} :: ModifyDBInstance)

-- | The new Provisioned IOPS (I\/O operations per second) value for the
-- instance.
--
-- Changing this setting doesn\'t result in an outage and the change is
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is set to @true@ for this request.
--
-- Default: Uses existing setting
modifyDBInstance_iops :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_iops = Lens.lens (\ModifyDBInstance' {iops} -> iops) (\s@ModifyDBInstance' {} a -> s {iops = a} :: ModifyDBInstance)

-- | Not supported by Neptune.
modifyDBInstance_licenseModel :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_licenseModel = Lens.lens (\ModifyDBInstance' {licenseModel} -> licenseModel) (\s@ModifyDBInstance' {} a -> s {licenseModel = a} :: ModifyDBInstance)

-- | Not supported by Neptune.
modifyDBInstance_masterUserPassword :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_masterUserPassword = Lens.lens (\ModifyDBInstance' {masterUserPassword} -> masterUserPassword) (\s@ModifyDBInstance' {} a -> s {masterUserPassword = a} :: ModifyDBInstance)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
modifyDBInstance_monitoringInterval :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_monitoringInterval = Lens.lens (\ModifyDBInstance' {monitoringInterval} -> monitoringInterval) (\s@ModifyDBInstance' {} a -> s {monitoringInterval = a} :: ModifyDBInstance)

-- | The ARN for the IAM role that permits Neptune to send enhanced
-- monitoring metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@.
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
modifyDBInstance_monitoringRoleArn :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_monitoringRoleArn = Lens.lens (\ModifyDBInstance' {monitoringRoleArn} -> monitoringRoleArn) (\s@ModifyDBInstance' {} a -> s {monitoringRoleArn = a} :: ModifyDBInstance)

-- | Specifies if the DB instance is a Multi-AZ deployment. Changing this
-- parameter doesn\'t result in an outage and the change is applied during
-- the next maintenance window unless the @ApplyImmediately@ parameter is
-- set to @true@ for this request.
modifyDBInstance_multiAZ :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_multiAZ = Lens.lens (\ModifyDBInstance' {multiAZ} -> multiAZ) (\s@ModifyDBInstance' {} a -> s {multiAZ = a} :: ModifyDBInstance)

-- | The new DB instance identifier for the DB instance when renaming a DB
-- instance. When you change the DB instance identifier, an instance reboot
-- will occur immediately if you set @Apply Immediately@ to true, or will
-- occur during the next maintenance window if @Apply Immediately@ to
-- false. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
modifyDBInstance_newDBInstanceIdentifier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_newDBInstanceIdentifier = Lens.lens (\ModifyDBInstance' {newDBInstanceIdentifier'} -> newDBInstanceIdentifier') (\s@ModifyDBInstance' {} a -> s {newDBInstanceIdentifier' = a} :: ModifyDBInstance)

-- | /(Not supported by Neptune)/
modifyDBInstance_optionGroupName :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_optionGroupName = Lens.lens (\ModifyDBInstance' {optionGroupName} -> optionGroupName) (\s@ModifyDBInstance' {} a -> s {optionGroupName = a} :: ModifyDBInstance)

-- | /(Not supported by Neptune)/
modifyDBInstance_performanceInsightsKMSKeyId :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_performanceInsightsKMSKeyId = Lens.lens (\ModifyDBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@ModifyDBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: ModifyDBInstance)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled.
--
-- Not applicable. The daily time range for creating automated backups is
-- managed by the DB cluster. For more information, see ModifyDBCluster.
--
-- Constraints:
--
-- -   Must be in the format hh24:mi-hh24:mi
--
-- -   Must be in Universal Time Coordinated (UTC)
--
-- -   Must not conflict with the preferred maintenance window
--
-- -   Must be at least 30 minutes
modifyDBInstance_preferredBackupWindow :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_preferredBackupWindow = Lens.lens (\ModifyDBInstance' {preferredBackupWindow} -> preferredBackupWindow) (\s@ModifyDBInstance' {} a -> s {preferredBackupWindow = a} :: ModifyDBInstance)

-- | The weekly time range (in UTC) during which system maintenance can
-- occur, which might result in an outage. Changing this parameter doesn\'t
-- result in an outage, except in the following situation, and the change
-- is asynchronously applied as soon as possible. If there are pending
-- actions that cause a reboot, and the maintenance window is changed to
-- include the current time, then changing this parameter will cause a
-- reboot of the DB instance. If moving this window to the current time,
-- there must be at least 30 minutes between the current time and end of
-- the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
--
-- Format: ddd:hh24:mi-ddd:hh24:mi
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Must be at least 30 minutes
modifyDBInstance_preferredMaintenanceWindow :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_preferredMaintenanceWindow = Lens.lens (\ModifyDBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyDBInstance' {} a -> s {preferredMaintenanceWindow = a} :: ModifyDBInstance)

-- | A value that specifies the order in which a Read Replica is promoted to
-- the primary instance after a failure of the existing primary instance.
--
-- Default: 1
--
-- Valid Values: 0 - 15
modifyDBInstance_promotionTier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_promotionTier = Lens.lens (\ModifyDBInstance' {promotionTier} -> promotionTier) (\s@ModifyDBInstance' {} a -> s {promotionTier = a} :: ModifyDBInstance)

-- | This flag should no longer be used.
modifyDBInstance_publiclyAccessible :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_publiclyAccessible = Lens.lens (\ModifyDBInstance' {publiclyAccessible} -> publiclyAccessible) (\s@ModifyDBInstance' {} a -> s {publiclyAccessible = a} :: ModifyDBInstance)

-- | Not supported.
modifyDBInstance_storageType :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_storageType = Lens.lens (\ModifyDBInstance' {storageType} -> storageType) (\s@ModifyDBInstance' {} a -> s {storageType = a} :: ModifyDBInstance)

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
modifyDBInstance_tdeCredentialArn :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_tdeCredentialArn = Lens.lens (\ModifyDBInstance' {tdeCredentialArn} -> tdeCredentialArn) (\s@ModifyDBInstance' {} a -> s {tdeCredentialArn = a} :: ModifyDBInstance)

-- | The password for the given ARN from the key store in order to access the
-- device.
modifyDBInstance_tdeCredentialPassword :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_tdeCredentialPassword = Lens.lens (\ModifyDBInstance' {tdeCredentialPassword} -> tdeCredentialPassword) (\s@ModifyDBInstance' {} a -> s {tdeCredentialPassword = a} :: ModifyDBInstance)

-- | A list of EC2 VPC security groups to authorize on this DB instance. This
-- change is asynchronously applied as soon as possible.
--
-- Not applicable. The associated list of EC2 VPC security groups is
-- managed by the DB cluster. For more information, see ModifyDBCluster.
--
-- Constraints:
--
-- -   If supplied, must match existing VpcSecurityGroupIds.
modifyDBInstance_vpcSecurityGroupIds :: Lens.Lens' ModifyDBInstance (Prelude.Maybe [Prelude.Text])
modifyDBInstance_vpcSecurityGroupIds = Lens.lens (\ModifyDBInstance' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@ModifyDBInstance' {} a -> s {vpcSecurityGroupIds = a} :: ModifyDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
modifyDBInstance_dbInstanceIdentifier :: Lens.Lens' ModifyDBInstance Prelude.Text
modifyDBInstance_dbInstanceIdentifier = Lens.lens (\ModifyDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@ModifyDBInstance' {} a -> s {dbInstanceIdentifier = a} :: ModifyDBInstance)

instance Core.AWSRequest ModifyDBInstance where
  type
    AWSResponse ModifyDBInstance =
      ModifyDBInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBInstanceResult"
      ( \s h x ->
          ModifyDBInstanceResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBInstance where
  hashWithSalt _salt ModifyDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` allowMajorVersionUpgrade
      `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` cACertificateIdentifier
      `Prelude.hashWithSalt` cloudwatchLogsExportConfiguration
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` dbPortNumber
      `Prelude.hashWithSalt` dbSecurityGroups
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` enablePerformanceInsights
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` newDBInstanceIdentifier'
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` promotionTier
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tdeCredentialArn
      `Prelude.hashWithSalt` tdeCredentialPassword
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbInstanceIdentifier

instance Prelude.NFData ModifyDBInstance where
  rnf ModifyDBInstance' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf allowMajorVersionUpgrade
      `Prelude.seq` Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf cACertificateIdentifier
      `Prelude.seq` Prelude.rnf cloudwatchLogsExportConfiguration
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf dbPortNumber
      `Prelude.seq` Prelude.rnf dbSecurityGroups
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf
        enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf
        enablePerformanceInsights
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf
        masterUserPassword
      `Prelude.seq` Prelude.rnf
        monitoringInterval
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf
        newDBInstanceIdentifier'
      `Prelude.seq` Prelude.rnf
        optionGroupName
      `Prelude.seq` Prelude.rnf
        performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        promotionTier
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tdeCredentialArn
      `Prelude.seq` Prelude.rnf
        tdeCredentialPassword
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        dbInstanceIdentifier

instance Data.ToHeaders ModifyDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyDBInstance where
  toQuery ModifyDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AllocatedStorage" Data.=: allocatedStorage,
        "AllowMajorVersionUpgrade"
          Data.=: allowMajorVersionUpgrade,
        "ApplyImmediately" Data.=: applyImmediately,
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "BackupRetentionPeriod"
          Data.=: backupRetentionPeriod,
        "CACertificateIdentifier"
          Data.=: cACertificateIdentifier,
        "CloudwatchLogsExportConfiguration"
          Data.=: cloudwatchLogsExportConfiguration,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "DBInstanceClass" Data.=: dbInstanceClass,
        "DBParameterGroupName" Data.=: dbParameterGroupName,
        "DBPortNumber" Data.=: dbPortNumber,
        "DBSecurityGroups"
          Data.=: Data.toQuery
            ( Data.toQueryList "DBSecurityGroupName"
                Prelude.<$> dbSecurityGroups
            ),
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DeletionProtection" Data.=: deletionProtection,
        "Domain" Data.=: domain,
        "DomainIAMRoleName" Data.=: domainIAMRoleName,
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "EnablePerformanceInsights"
          Data.=: enablePerformanceInsights,
        "EngineVersion" Data.=: engineVersion,
        "Iops" Data.=: iops,
        "LicenseModel" Data.=: licenseModel,
        "MasterUserPassword" Data.=: masterUserPassword,
        "MonitoringInterval" Data.=: monitoringInterval,
        "MonitoringRoleArn" Data.=: monitoringRoleArn,
        "MultiAZ" Data.=: multiAZ,
        "NewDBInstanceIdentifier"
          Data.=: newDBInstanceIdentifier',
        "OptionGroupName" Data.=: optionGroupName,
        "PerformanceInsightsKMSKeyId"
          Data.=: performanceInsightsKMSKeyId,
        "PreferredBackupWindow"
          Data.=: preferredBackupWindow,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "PromotionTier" Data.=: promotionTier,
        "PubliclyAccessible" Data.=: publiclyAccessible,
        "StorageType" Data.=: storageType,
        "TdeCredentialArn" Data.=: tdeCredentialArn,
        "TdeCredentialPassword"
          Data.=: tdeCredentialPassword,
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newModifyDBInstanceResponse' smart constructor.
data ModifyDBInstanceResponse = ModifyDBInstanceResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'modifyDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'modifyDBInstanceResponse_httpStatus' - The response's http status code.
newModifyDBInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyDBInstanceResponse
newModifyDBInstanceResponse pHttpStatus_ =
  ModifyDBInstanceResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBInstanceResponse_dbInstance :: Lens.Lens' ModifyDBInstanceResponse (Prelude.Maybe DBInstance)
modifyDBInstanceResponse_dbInstance = Lens.lens (\ModifyDBInstanceResponse' {dbInstance} -> dbInstance) (\s@ModifyDBInstanceResponse' {} a -> s {dbInstance = a} :: ModifyDBInstanceResponse)

-- | The response's http status code.
modifyDBInstanceResponse_httpStatus :: Lens.Lens' ModifyDBInstanceResponse Prelude.Int
modifyDBInstanceResponse_httpStatus = Lens.lens (\ModifyDBInstanceResponse' {httpStatus} -> httpStatus) (\s@ModifyDBInstanceResponse' {} a -> s {httpStatus = a} :: ModifyDBInstanceResponse)

instance Prelude.NFData ModifyDBInstanceResponse where
  rnf ModifyDBInstanceResponse' {..} =
    Prelude.rnf dbInstance `Prelude.seq`
      Prelude.rnf httpStatus
