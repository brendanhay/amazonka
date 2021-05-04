{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types.BackupStatus
import Network.AWS.OpsWorksCM.Types.BackupType
import qualified Network.AWS.Prelude as Prelude

-- | Describes a single backup.
--
-- /See:/ 'newBackup' smart constructor.
data Backup = Backup'
  { -- | The security group IDs that are obtained from the server when the backup
    -- is created.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The EC2 instance profile ARN that is obtained from the server when the
    -- backup is created. Because this value is stored, you are not required to
    -- provide the InstanceProfileArn again if you restore a backup.
    instanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The preferred backup period that is obtained from the server when the
    -- backup is created.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The status of a backup while in progress.
    status :: Prelude.Maybe BackupStatus,
    -- | The service role ARN that is obtained from the server when the backup is
    -- created.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The IAM user ARN of the requester for manual backups. This field is
    -- empty for automated backups.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The instance type that is obtained from the server when the backup is
    -- created.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The backup type. Valid values are @automated@ or @manual@.
    backupType :: Prelude.Maybe BackupType,
    -- | The generated ID of the backup. Example:
    -- @myServerName-yyyyMMddHHmmssSSS@
    backupId :: Prelude.Maybe Prelude.Text,
    -- | This field is deprecated and is no longer used.
    s3DataUrl :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the backup.
    backupArn :: Prelude.Maybe Prelude.Text,
    -- | The subnet IDs that are obtained from the server when the backup is
    -- created.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The key pair that is obtained from the server when the backup is
    -- created.
    keyPair :: Prelude.Maybe Prelude.Text,
    -- | This field is deprecated and is no longer used.
    s3DataSize :: Prelude.Maybe Prelude.Int,
    -- | The time stamp when the backup was created in the database. Example:
    -- @2016-07-29T13:38:47.520Z@
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the server from which the backup was made.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URL of the backup\'s log file.
    s3LogUrl :: Prelude.Maybe Prelude.Text,
    -- | The engine version that is obtained from the server when the backup is
    -- created.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The preferred maintenance period that is obtained from the server when
    -- the backup is created.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The version of AWS OpsWorks CM-specific tools that is obtained from the
    -- server when the backup is created.
    toolsVersion :: Prelude.Maybe Prelude.Text,
    -- | The engine model that is obtained from the server when the backup is
    -- created.
    engineModel :: Prelude.Maybe Prelude.Text,
    -- | The engine type that is obtained from the server when the backup is
    -- created.
    engine :: Prelude.Maybe Prelude.Text,
    -- | A user-provided description for a manual backup. This field is empty for
    -- automated backups.
    description :: Prelude.Maybe Prelude.Text,
    -- | An informational message about backup status.
    statusDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { securityGroupIds = Prelude.Nothing,
      instanceProfileArn = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      status = Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      userArn = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      backupType = Prelude.Nothing,
      backupId = Prelude.Nothing,
      s3DataUrl = Prelude.Nothing,
      backupArn = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      keyPair = Prelude.Nothing,
      s3DataSize = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      serverName = Prelude.Nothing,
      s3LogUrl = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      toolsVersion = Prelude.Nothing,
      engineModel = Prelude.Nothing,
      engine = Prelude.Nothing,
      description = Prelude.Nothing,
      statusDescription = Prelude.Nothing
    }

-- | The security group IDs that are obtained from the server when the backup
-- is created.
backup_securityGroupIds :: Lens.Lens' Backup (Prelude.Maybe [Prelude.Text])
backup_securityGroupIds = Lens.lens (\Backup' {securityGroupIds} -> securityGroupIds) (\s@Backup' {} a -> s {securityGroupIds = a} :: Backup) Prelude.. Lens.mapping Prelude._Coerce

-- | The EC2 instance profile ARN that is obtained from the server when the
-- backup is created. Because this value is stored, you are not required to
-- provide the InstanceProfileArn again if you restore a backup.
backup_instanceProfileArn :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_instanceProfileArn = Lens.lens (\Backup' {instanceProfileArn} -> instanceProfileArn) (\s@Backup' {} a -> s {instanceProfileArn = a} :: Backup)

-- | The preferred backup period that is obtained from the server when the
-- backup is created.
backup_preferredBackupWindow :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_preferredBackupWindow = Lens.lens (\Backup' {preferredBackupWindow} -> preferredBackupWindow) (\s@Backup' {} a -> s {preferredBackupWindow = a} :: Backup)

-- | The status of a backup while in progress.
backup_status :: Lens.Lens' Backup (Prelude.Maybe BackupStatus)
backup_status = Lens.lens (\Backup' {status} -> status) (\s@Backup' {} a -> s {status = a} :: Backup)

-- | The service role ARN that is obtained from the server when the backup is
-- created.
backup_serviceRoleArn :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_serviceRoleArn = Lens.lens (\Backup' {serviceRoleArn} -> serviceRoleArn) (\s@Backup' {} a -> s {serviceRoleArn = a} :: Backup)

-- | The IAM user ARN of the requester for manual backups. This field is
-- empty for automated backups.
backup_userArn :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_userArn = Lens.lens (\Backup' {userArn} -> userArn) (\s@Backup' {} a -> s {userArn = a} :: Backup)

-- | The instance type that is obtained from the server when the backup is
-- created.
backup_instanceType :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_instanceType = Lens.lens (\Backup' {instanceType} -> instanceType) (\s@Backup' {} a -> s {instanceType = a} :: Backup)

-- | The backup type. Valid values are @automated@ or @manual@.
backup_backupType :: Lens.Lens' Backup (Prelude.Maybe BackupType)
backup_backupType = Lens.lens (\Backup' {backupType} -> backupType) (\s@Backup' {} a -> s {backupType = a} :: Backup)

-- | The generated ID of the backup. Example:
-- @myServerName-yyyyMMddHHmmssSSS@
backup_backupId :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_backupId = Lens.lens (\Backup' {backupId} -> backupId) (\s@Backup' {} a -> s {backupId = a} :: Backup)

-- | This field is deprecated and is no longer used.
backup_s3DataUrl :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_s3DataUrl = Lens.lens (\Backup' {s3DataUrl} -> s3DataUrl) (\s@Backup' {} a -> s {s3DataUrl = a} :: Backup)

-- | The ARN of the backup.
backup_backupArn :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_backupArn = Lens.lens (\Backup' {backupArn} -> backupArn) (\s@Backup' {} a -> s {backupArn = a} :: Backup)

-- | The subnet IDs that are obtained from the server when the backup is
-- created.
backup_subnetIds :: Lens.Lens' Backup (Prelude.Maybe [Prelude.Text])
backup_subnetIds = Lens.lens (\Backup' {subnetIds} -> subnetIds) (\s@Backup' {} a -> s {subnetIds = a} :: Backup) Prelude.. Lens.mapping Prelude._Coerce

-- | The key pair that is obtained from the server when the backup is
-- created.
backup_keyPair :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_keyPair = Lens.lens (\Backup' {keyPair} -> keyPair) (\s@Backup' {} a -> s {keyPair = a} :: Backup)

-- | This field is deprecated and is no longer used.
backup_s3DataSize :: Lens.Lens' Backup (Prelude.Maybe Prelude.Int)
backup_s3DataSize = Lens.lens (\Backup' {s3DataSize} -> s3DataSize) (\s@Backup' {} a -> s {s3DataSize = a} :: Backup)

-- | The time stamp when the backup was created in the database. Example:
-- @2016-07-29T13:38:47.520Z@
backup_createdAt :: Lens.Lens' Backup (Prelude.Maybe Prelude.UTCTime)
backup_createdAt = Lens.lens (\Backup' {createdAt} -> createdAt) (\s@Backup' {} a -> s {createdAt = a} :: Backup) Prelude.. Lens.mapping Prelude._Time

-- | The name of the server from which the backup was made.
backup_serverName :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_serverName = Lens.lens (\Backup' {serverName} -> serverName) (\s@Backup' {} a -> s {serverName = a} :: Backup)

-- | The Amazon S3 URL of the backup\'s log file.
backup_s3LogUrl :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_s3LogUrl = Lens.lens (\Backup' {s3LogUrl} -> s3LogUrl) (\s@Backup' {} a -> s {s3LogUrl = a} :: Backup)

-- | The engine version that is obtained from the server when the backup is
-- created.
backup_engineVersion :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_engineVersion = Lens.lens (\Backup' {engineVersion} -> engineVersion) (\s@Backup' {} a -> s {engineVersion = a} :: Backup)

-- | The preferred maintenance period that is obtained from the server when
-- the backup is created.
backup_preferredMaintenanceWindow :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_preferredMaintenanceWindow = Lens.lens (\Backup' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@Backup' {} a -> s {preferredMaintenanceWindow = a} :: Backup)

-- | The version of AWS OpsWorks CM-specific tools that is obtained from the
-- server when the backup is created.
backup_toolsVersion :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_toolsVersion = Lens.lens (\Backup' {toolsVersion} -> toolsVersion) (\s@Backup' {} a -> s {toolsVersion = a} :: Backup)

-- | The engine model that is obtained from the server when the backup is
-- created.
backup_engineModel :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_engineModel = Lens.lens (\Backup' {engineModel} -> engineModel) (\s@Backup' {} a -> s {engineModel = a} :: Backup)

-- | The engine type that is obtained from the server when the backup is
-- created.
backup_engine :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_engine = Lens.lens (\Backup' {engine} -> engine) (\s@Backup' {} a -> s {engine = a} :: Backup)

-- | A user-provided description for a manual backup. This field is empty for
-- automated backups.
backup_description :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_description = Lens.lens (\Backup' {description} -> description) (\s@Backup' {} a -> s {description = a} :: Backup)

-- | An informational message about backup status.
backup_statusDescription :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_statusDescription = Lens.lens (\Backup' {statusDescription} -> statusDescription) (\s@Backup' {} a -> s {statusDescription = a} :: Backup)

instance Prelude.FromJSON Backup where
  parseJSON =
    Prelude.withObject
      "Backup"
      ( \x ->
          Backup'
            Prelude.<$> ( x Prelude..:? "SecurityGroupIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "InstanceProfileArn")
            Prelude.<*> (x Prelude..:? "PreferredBackupWindow")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "ServiceRoleArn")
            Prelude.<*> (x Prelude..:? "UserArn")
            Prelude.<*> (x Prelude..:? "InstanceType")
            Prelude.<*> (x Prelude..:? "BackupType")
            Prelude.<*> (x Prelude..:? "BackupId")
            Prelude.<*> (x Prelude..:? "S3DataUrl")
            Prelude.<*> (x Prelude..:? "BackupArn")
            Prelude.<*> ( x Prelude..:? "SubnetIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "KeyPair")
            Prelude.<*> (x Prelude..:? "S3DataSize")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "ServerName")
            Prelude.<*> (x Prelude..:? "S3LogUrl")
            Prelude.<*> (x Prelude..:? "EngineVersion")
            Prelude.<*> (x Prelude..:? "PreferredMaintenanceWindow")
            Prelude.<*> (x Prelude..:? "ToolsVersion")
            Prelude.<*> (x Prelude..:? "EngineModel")
            Prelude.<*> (x Prelude..:? "Engine")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "StatusDescription")
      )

instance Prelude.Hashable Backup

instance Prelude.NFData Backup
