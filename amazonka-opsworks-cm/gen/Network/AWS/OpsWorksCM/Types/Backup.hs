{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.Backup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.Backup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types.BackupStatus
import Network.AWS.OpsWorksCM.Types.BackupType

-- | Describes a single backup.
--
-- /See:/ 'newBackup' smart constructor.
data Backup = Backup'
  { -- | The security group IDs that are obtained from the server when the backup
    -- is created.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The EC2 instance profile ARN that is obtained from the server when the
    -- backup is created. Because this value is stored, you are not required to
    -- provide the InstanceProfileArn again if you restore a backup.
    instanceProfileArn :: Core.Maybe Core.Text,
    -- | The preferred backup period that is obtained from the server when the
    -- backup is created.
    preferredBackupWindow :: Core.Maybe Core.Text,
    -- | The status of a backup while in progress.
    status :: Core.Maybe BackupStatus,
    -- | The service role ARN that is obtained from the server when the backup is
    -- created.
    serviceRoleArn :: Core.Maybe Core.Text,
    -- | The IAM user ARN of the requester for manual backups. This field is
    -- empty for automated backups.
    userArn :: Core.Maybe Core.Text,
    -- | The instance type that is obtained from the server when the backup is
    -- created.
    instanceType :: Core.Maybe Core.Text,
    -- | The backup type. Valid values are @automated@ or @manual@.
    backupType :: Core.Maybe BackupType,
    -- | The generated ID of the backup. Example:
    -- @myServerName-yyyyMMddHHmmssSSS@
    backupId :: Core.Maybe Core.Text,
    -- | This field is deprecated and is no longer used.
    s3DataUrl :: Core.Maybe Core.Text,
    -- | The ARN of the backup.
    backupArn :: Core.Maybe Core.Text,
    -- | The subnet IDs that are obtained from the server when the backup is
    -- created.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The key pair that is obtained from the server when the backup is
    -- created.
    keyPair :: Core.Maybe Core.Text,
    -- | This field is deprecated and is no longer used.
    s3DataSize :: Core.Maybe Core.Int,
    -- | The time stamp when the backup was created in the database. Example:
    -- @2016-07-29T13:38:47.520Z@
    createdAt :: Core.Maybe Core.POSIX,
    -- | The name of the server from which the backup was made.
    serverName :: Core.Maybe Core.Text,
    -- | The Amazon S3 URL of the backup\'s log file.
    s3LogUrl :: Core.Maybe Core.Text,
    -- | The engine version that is obtained from the server when the backup is
    -- created.
    engineVersion :: Core.Maybe Core.Text,
    -- | The preferred maintenance period that is obtained from the server when
    -- the backup is created.
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | The version of AWS OpsWorks CM-specific tools that is obtained from the
    -- server when the backup is created.
    toolsVersion :: Core.Maybe Core.Text,
    -- | The engine model that is obtained from the server when the backup is
    -- created.
    engineModel :: Core.Maybe Core.Text,
    -- | The engine type that is obtained from the server when the backup is
    -- created.
    engine :: Core.Maybe Core.Text,
    -- | A user-provided description for a manual backup. This field is empty for
    -- automated backups.
    description :: Core.Maybe Core.Text,
    -- | An informational message about backup status.
    statusDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Backup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'backup_securityGroupIds' - The security group IDs that are obtained from the server when the backup
-- is created.
--
-- 'instanceProfileArn', 'backup_instanceProfileArn' - The EC2 instance profile ARN that is obtained from the server when the
-- backup is created. Because this value is stored, you are not required to
-- provide the InstanceProfileArn again if you restore a backup.
--
-- 'preferredBackupWindow', 'backup_preferredBackupWindow' - The preferred backup period that is obtained from the server when the
-- backup is created.
--
-- 'status', 'backup_status' - The status of a backup while in progress.
--
-- 'serviceRoleArn', 'backup_serviceRoleArn' - The service role ARN that is obtained from the server when the backup is
-- created.
--
-- 'userArn', 'backup_userArn' - The IAM user ARN of the requester for manual backups. This field is
-- empty for automated backups.
--
-- 'instanceType', 'backup_instanceType' - The instance type that is obtained from the server when the backup is
-- created.
--
-- 'backupType', 'backup_backupType' - The backup type. Valid values are @automated@ or @manual@.
--
-- 'backupId', 'backup_backupId' - The generated ID of the backup. Example:
-- @myServerName-yyyyMMddHHmmssSSS@
--
-- 's3DataUrl', 'backup_s3DataUrl' - This field is deprecated and is no longer used.
--
-- 'backupArn', 'backup_backupArn' - The ARN of the backup.
--
-- 'subnetIds', 'backup_subnetIds' - The subnet IDs that are obtained from the server when the backup is
-- created.
--
-- 'keyPair', 'backup_keyPair' - The key pair that is obtained from the server when the backup is
-- created.
--
-- 's3DataSize', 'backup_s3DataSize' - This field is deprecated and is no longer used.
--
-- 'createdAt', 'backup_createdAt' - The time stamp when the backup was created in the database. Example:
-- @2016-07-29T13:38:47.520Z@
--
-- 'serverName', 'backup_serverName' - The name of the server from which the backup was made.
--
-- 's3LogUrl', 'backup_s3LogUrl' - The Amazon S3 URL of the backup\'s log file.
--
-- 'engineVersion', 'backup_engineVersion' - The engine version that is obtained from the server when the backup is
-- created.
--
-- 'preferredMaintenanceWindow', 'backup_preferredMaintenanceWindow' - The preferred maintenance period that is obtained from the server when
-- the backup is created.
--
-- 'toolsVersion', 'backup_toolsVersion' - The version of AWS OpsWorks CM-specific tools that is obtained from the
-- server when the backup is created.
--
-- 'engineModel', 'backup_engineModel' - The engine model that is obtained from the server when the backup is
-- created.
--
-- 'engine', 'backup_engine' - The engine type that is obtained from the server when the backup is
-- created.
--
-- 'description', 'backup_description' - A user-provided description for a manual backup. This field is empty for
-- automated backups.
--
-- 'statusDescription', 'backup_statusDescription' - An informational message about backup status.
newBackup ::
  Backup
newBackup =
  Backup'
    { securityGroupIds = Core.Nothing,
      instanceProfileArn = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      status = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      userArn = Core.Nothing,
      instanceType = Core.Nothing,
      backupType = Core.Nothing,
      backupId = Core.Nothing,
      s3DataUrl = Core.Nothing,
      backupArn = Core.Nothing,
      subnetIds = Core.Nothing,
      keyPair = Core.Nothing,
      s3DataSize = Core.Nothing,
      createdAt = Core.Nothing,
      serverName = Core.Nothing,
      s3LogUrl = Core.Nothing,
      engineVersion = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      toolsVersion = Core.Nothing,
      engineModel = Core.Nothing,
      engine = Core.Nothing,
      description = Core.Nothing,
      statusDescription = Core.Nothing
    }

-- | The security group IDs that are obtained from the server when the backup
-- is created.
backup_securityGroupIds :: Lens.Lens' Backup (Core.Maybe [Core.Text])
backup_securityGroupIds = Lens.lens (\Backup' {securityGroupIds} -> securityGroupIds) (\s@Backup' {} a -> s {securityGroupIds = a} :: Backup) Core.. Lens.mapping Lens._Coerce

-- | The EC2 instance profile ARN that is obtained from the server when the
-- backup is created. Because this value is stored, you are not required to
-- provide the InstanceProfileArn again if you restore a backup.
backup_instanceProfileArn :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_instanceProfileArn = Lens.lens (\Backup' {instanceProfileArn} -> instanceProfileArn) (\s@Backup' {} a -> s {instanceProfileArn = a} :: Backup)

-- | The preferred backup period that is obtained from the server when the
-- backup is created.
backup_preferredBackupWindow :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_preferredBackupWindow = Lens.lens (\Backup' {preferredBackupWindow} -> preferredBackupWindow) (\s@Backup' {} a -> s {preferredBackupWindow = a} :: Backup)

-- | The status of a backup while in progress.
backup_status :: Lens.Lens' Backup (Core.Maybe BackupStatus)
backup_status = Lens.lens (\Backup' {status} -> status) (\s@Backup' {} a -> s {status = a} :: Backup)

-- | The service role ARN that is obtained from the server when the backup is
-- created.
backup_serviceRoleArn :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_serviceRoleArn = Lens.lens (\Backup' {serviceRoleArn} -> serviceRoleArn) (\s@Backup' {} a -> s {serviceRoleArn = a} :: Backup)

-- | The IAM user ARN of the requester for manual backups. This field is
-- empty for automated backups.
backup_userArn :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_userArn = Lens.lens (\Backup' {userArn} -> userArn) (\s@Backup' {} a -> s {userArn = a} :: Backup)

-- | The instance type that is obtained from the server when the backup is
-- created.
backup_instanceType :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_instanceType = Lens.lens (\Backup' {instanceType} -> instanceType) (\s@Backup' {} a -> s {instanceType = a} :: Backup)

-- | The backup type. Valid values are @automated@ or @manual@.
backup_backupType :: Lens.Lens' Backup (Core.Maybe BackupType)
backup_backupType = Lens.lens (\Backup' {backupType} -> backupType) (\s@Backup' {} a -> s {backupType = a} :: Backup)

-- | The generated ID of the backup. Example:
-- @myServerName-yyyyMMddHHmmssSSS@
backup_backupId :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_backupId = Lens.lens (\Backup' {backupId} -> backupId) (\s@Backup' {} a -> s {backupId = a} :: Backup)

-- | This field is deprecated and is no longer used.
backup_s3DataUrl :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_s3DataUrl = Lens.lens (\Backup' {s3DataUrl} -> s3DataUrl) (\s@Backup' {} a -> s {s3DataUrl = a} :: Backup)

-- | The ARN of the backup.
backup_backupArn :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_backupArn = Lens.lens (\Backup' {backupArn} -> backupArn) (\s@Backup' {} a -> s {backupArn = a} :: Backup)

-- | The subnet IDs that are obtained from the server when the backup is
-- created.
backup_subnetIds :: Lens.Lens' Backup (Core.Maybe [Core.Text])
backup_subnetIds = Lens.lens (\Backup' {subnetIds} -> subnetIds) (\s@Backup' {} a -> s {subnetIds = a} :: Backup) Core.. Lens.mapping Lens._Coerce

-- | The key pair that is obtained from the server when the backup is
-- created.
backup_keyPair :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_keyPair = Lens.lens (\Backup' {keyPair} -> keyPair) (\s@Backup' {} a -> s {keyPair = a} :: Backup)

-- | This field is deprecated and is no longer used.
backup_s3DataSize :: Lens.Lens' Backup (Core.Maybe Core.Int)
backup_s3DataSize = Lens.lens (\Backup' {s3DataSize} -> s3DataSize) (\s@Backup' {} a -> s {s3DataSize = a} :: Backup)

-- | The time stamp when the backup was created in the database. Example:
-- @2016-07-29T13:38:47.520Z@
backup_createdAt :: Lens.Lens' Backup (Core.Maybe Core.UTCTime)
backup_createdAt = Lens.lens (\Backup' {createdAt} -> createdAt) (\s@Backup' {} a -> s {createdAt = a} :: Backup) Core.. Lens.mapping Core._Time

-- | The name of the server from which the backup was made.
backup_serverName :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_serverName = Lens.lens (\Backup' {serverName} -> serverName) (\s@Backup' {} a -> s {serverName = a} :: Backup)

-- | The Amazon S3 URL of the backup\'s log file.
backup_s3LogUrl :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_s3LogUrl = Lens.lens (\Backup' {s3LogUrl} -> s3LogUrl) (\s@Backup' {} a -> s {s3LogUrl = a} :: Backup)

-- | The engine version that is obtained from the server when the backup is
-- created.
backup_engineVersion :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_engineVersion = Lens.lens (\Backup' {engineVersion} -> engineVersion) (\s@Backup' {} a -> s {engineVersion = a} :: Backup)

-- | The preferred maintenance period that is obtained from the server when
-- the backup is created.
backup_preferredMaintenanceWindow :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_preferredMaintenanceWindow = Lens.lens (\Backup' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@Backup' {} a -> s {preferredMaintenanceWindow = a} :: Backup)

-- | The version of AWS OpsWorks CM-specific tools that is obtained from the
-- server when the backup is created.
backup_toolsVersion :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_toolsVersion = Lens.lens (\Backup' {toolsVersion} -> toolsVersion) (\s@Backup' {} a -> s {toolsVersion = a} :: Backup)

-- | The engine model that is obtained from the server when the backup is
-- created.
backup_engineModel :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_engineModel = Lens.lens (\Backup' {engineModel} -> engineModel) (\s@Backup' {} a -> s {engineModel = a} :: Backup)

-- | The engine type that is obtained from the server when the backup is
-- created.
backup_engine :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_engine = Lens.lens (\Backup' {engine} -> engine) (\s@Backup' {} a -> s {engine = a} :: Backup)

-- | A user-provided description for a manual backup. This field is empty for
-- automated backups.
backup_description :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_description = Lens.lens (\Backup' {description} -> description) (\s@Backup' {} a -> s {description = a} :: Backup)

-- | An informational message about backup status.
backup_statusDescription :: Lens.Lens' Backup (Core.Maybe Core.Text)
backup_statusDescription = Lens.lens (\Backup' {statusDescription} -> statusDescription) (\s@Backup' {} a -> s {statusDescription = a} :: Backup)

instance Core.FromJSON Backup where
  parseJSON =
    Core.withObject
      "Backup"
      ( \x ->
          Backup'
            Core.<$> (x Core..:? "SecurityGroupIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "InstanceProfileArn")
            Core.<*> (x Core..:? "PreferredBackupWindow")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "ServiceRoleArn")
            Core.<*> (x Core..:? "UserArn")
            Core.<*> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "BackupType")
            Core.<*> (x Core..:? "BackupId")
            Core.<*> (x Core..:? "S3DataUrl")
            Core.<*> (x Core..:? "BackupArn")
            Core.<*> (x Core..:? "SubnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "KeyPair")
            Core.<*> (x Core..:? "S3DataSize")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "ServerName")
            Core.<*> (x Core..:? "S3LogUrl")
            Core.<*> (x Core..:? "EngineVersion")
            Core.<*> (x Core..:? "PreferredMaintenanceWindow")
            Core.<*> (x Core..:? "ToolsVersion")
            Core.<*> (x Core..:? "EngineModel")
            Core.<*> (x Core..:? "Engine")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "StatusDescription")
      )

instance Core.Hashable Backup

instance Core.NFData Backup
