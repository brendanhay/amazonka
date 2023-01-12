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
-- Module      : Amazonka.FSx.Types.UpdateFileSystemWindowsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.UpdateFileSystemWindowsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.SelfManagedActiveDirectoryConfigurationUpdates
import Amazonka.FSx.Types.WindowsAuditLogCreateConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Updates the configuration for an existing Amazon FSx for Windows File
-- Server file system. Amazon FSx only overwrites existing properties with
-- non-null values provided in the request.
--
-- /See:/ 'newUpdateFileSystemWindowsConfiguration' smart constructor.
data UpdateFileSystemWindowsConfiguration = UpdateFileSystemWindowsConfiguration'
  { -- | The configuration that Amazon FSx for Windows File Server uses to audit
    -- and log user accesses of files, folders, and file shares on the Amazon
    -- FSx for Windows File Server file system..
    auditLogConfiguration :: Prelude.Maybe WindowsAuditLogCreateConfiguration,
    -- | The number of days to retain automatic daily backups. Setting this to
    -- zero (0) disables automatic daily backups. You can retain automatic
    -- daily backups for a maximum of 90 days. For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/using-backups.html#automatic-backups Working with Automatic Daily Backups>.
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | The preferred time to start the daily automatic backup, in the UTC time
    -- zone, for example, @02:00@
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | The configuration Amazon FSx uses to join the Windows File Server
    -- instance to the self-managed Microsoft AD directory. You cannot make a
    -- self-managed Microsoft AD update request if there is an existing
    -- self-managed Microsoft AD update request in progress.
    selfManagedActiveDirectoryConfiguration :: Prelude.Maybe SelfManagedActiveDirectoryConfigurationUpdates,
    -- | Sets the target value for a file system\'s throughput capacity, in
    -- MB\/s, that you are updating the file system to. Valid values are 8, 16,
    -- 32, 64, 128, 256, 512, 1024, 2048. You cannot make a throughput capacity
    -- update request if there is an existing throughput capacity update
    -- request in progress. For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-throughput-capacity.html Managing Throughput Capacity>.
    throughputCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The preferred start time to perform weekly maintenance, formatted
    -- d:HH:MM in the UTC time zone. Where d is the weekday number, from 1
    -- through 7, with 1 = Monday and 7 = Sunday.
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileSystemWindowsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogConfiguration', 'updateFileSystemWindowsConfiguration_auditLogConfiguration' - The configuration that Amazon FSx for Windows File Server uses to audit
-- and log user accesses of files, folders, and file shares on the Amazon
-- FSx for Windows File Server file system..
--
-- 'automaticBackupRetentionDays', 'updateFileSystemWindowsConfiguration_automaticBackupRetentionDays' - The number of days to retain automatic daily backups. Setting this to
-- zero (0) disables automatic daily backups. You can retain automatic
-- daily backups for a maximum of 90 days. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/using-backups.html#automatic-backups Working with Automatic Daily Backups>.
--
-- 'dailyAutomaticBackupStartTime', 'updateFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime' - The preferred time to start the daily automatic backup, in the UTC time
-- zone, for example, @02:00@
--
-- 'selfManagedActiveDirectoryConfiguration', 'updateFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration' - The configuration Amazon FSx uses to join the Windows File Server
-- instance to the self-managed Microsoft AD directory. You cannot make a
-- self-managed Microsoft AD update request if there is an existing
-- self-managed Microsoft AD update request in progress.
--
-- 'throughputCapacity', 'updateFileSystemWindowsConfiguration_throughputCapacity' - Sets the target value for a file system\'s throughput capacity, in
-- MB\/s, that you are updating the file system to. Valid values are 8, 16,
-- 32, 64, 128, 256, 512, 1024, 2048. You cannot make a throughput capacity
-- update request if there is an existing throughput capacity update
-- request in progress. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-throughput-capacity.html Managing Throughput Capacity>.
--
-- 'weeklyMaintenanceStartTime', 'updateFileSystemWindowsConfiguration_weeklyMaintenanceStartTime' - The preferred start time to perform weekly maintenance, formatted
-- d:HH:MM in the UTC time zone. Where d is the weekday number, from 1
-- through 7, with 1 = Monday and 7 = Sunday.
newUpdateFileSystemWindowsConfiguration ::
  UpdateFileSystemWindowsConfiguration
newUpdateFileSystemWindowsConfiguration =
  UpdateFileSystemWindowsConfiguration'
    { auditLogConfiguration =
        Prelude.Nothing,
      automaticBackupRetentionDays =
        Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing,
      selfManagedActiveDirectoryConfiguration =
        Prelude.Nothing,
      throughputCapacity = Prelude.Nothing,
      weeklyMaintenanceStartTime =
        Prelude.Nothing
    }

-- | The configuration that Amazon FSx for Windows File Server uses to audit
-- and log user accesses of files, folders, and file shares on the Amazon
-- FSx for Windows File Server file system..
updateFileSystemWindowsConfiguration_auditLogConfiguration :: Lens.Lens' UpdateFileSystemWindowsConfiguration (Prelude.Maybe WindowsAuditLogCreateConfiguration)
updateFileSystemWindowsConfiguration_auditLogConfiguration = Lens.lens (\UpdateFileSystemWindowsConfiguration' {auditLogConfiguration} -> auditLogConfiguration) (\s@UpdateFileSystemWindowsConfiguration' {} a -> s {auditLogConfiguration = a} :: UpdateFileSystemWindowsConfiguration)

-- | The number of days to retain automatic daily backups. Setting this to
-- zero (0) disables automatic daily backups. You can retain automatic
-- daily backups for a maximum of 90 days. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/using-backups.html#automatic-backups Working with Automatic Daily Backups>.
updateFileSystemWindowsConfiguration_automaticBackupRetentionDays :: Lens.Lens' UpdateFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Natural)
updateFileSystemWindowsConfiguration_automaticBackupRetentionDays = Lens.lens (\UpdateFileSystemWindowsConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@UpdateFileSystemWindowsConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: UpdateFileSystemWindowsConfiguration)

-- | The preferred time to start the daily automatic backup, in the UTC time
-- zone, for example, @02:00@
updateFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' UpdateFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Text)
updateFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\UpdateFileSystemWindowsConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@UpdateFileSystemWindowsConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: UpdateFileSystemWindowsConfiguration)

-- | The configuration Amazon FSx uses to join the Windows File Server
-- instance to the self-managed Microsoft AD directory. You cannot make a
-- self-managed Microsoft AD update request if there is an existing
-- self-managed Microsoft AD update request in progress.
updateFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration :: Lens.Lens' UpdateFileSystemWindowsConfiguration (Prelude.Maybe SelfManagedActiveDirectoryConfigurationUpdates)
updateFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration = Lens.lens (\UpdateFileSystemWindowsConfiguration' {selfManagedActiveDirectoryConfiguration} -> selfManagedActiveDirectoryConfiguration) (\s@UpdateFileSystemWindowsConfiguration' {} a -> s {selfManagedActiveDirectoryConfiguration = a} :: UpdateFileSystemWindowsConfiguration)

-- | Sets the target value for a file system\'s throughput capacity, in
-- MB\/s, that you are updating the file system to. Valid values are 8, 16,
-- 32, 64, 128, 256, 512, 1024, 2048. You cannot make a throughput capacity
-- update request if there is an existing throughput capacity update
-- request in progress. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-throughput-capacity.html Managing Throughput Capacity>.
updateFileSystemWindowsConfiguration_throughputCapacity :: Lens.Lens' UpdateFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Natural)
updateFileSystemWindowsConfiguration_throughputCapacity = Lens.lens (\UpdateFileSystemWindowsConfiguration' {throughputCapacity} -> throughputCapacity) (\s@UpdateFileSystemWindowsConfiguration' {} a -> s {throughputCapacity = a} :: UpdateFileSystemWindowsConfiguration)

-- | The preferred start time to perform weekly maintenance, formatted
-- d:HH:MM in the UTC time zone. Where d is the weekday number, from 1
-- through 7, with 1 = Monday and 7 = Sunday.
updateFileSystemWindowsConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' UpdateFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Text)
updateFileSystemWindowsConfiguration_weeklyMaintenanceStartTime = Lens.lens (\UpdateFileSystemWindowsConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@UpdateFileSystemWindowsConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: UpdateFileSystemWindowsConfiguration)

instance
  Prelude.Hashable
    UpdateFileSystemWindowsConfiguration
  where
  hashWithSalt
    _salt
    UpdateFileSystemWindowsConfiguration' {..} =
      _salt `Prelude.hashWithSalt` auditLogConfiguration
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` selfManagedActiveDirectoryConfiguration
        `Prelude.hashWithSalt` throughputCapacity
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime

instance
  Prelude.NFData
    UpdateFileSystemWindowsConfiguration
  where
  rnf UpdateFileSystemWindowsConfiguration' {..} =
    Prelude.rnf auditLogConfiguration
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf selfManagedActiveDirectoryConfiguration
      `Prelude.seq` Prelude.rnf throughputCapacity
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime

instance
  Data.ToJSON
    UpdateFileSystemWindowsConfiguration
  where
  toJSON UpdateFileSystemWindowsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuditLogConfiguration" Data..=)
              Prelude.<$> auditLogConfiguration,
            ("AutomaticBackupRetentionDays" Data..=)
              Prelude.<$> automaticBackupRetentionDays,
            ("DailyAutomaticBackupStartTime" Data..=)
              Prelude.<$> dailyAutomaticBackupStartTime,
            ("SelfManagedActiveDirectoryConfiguration" Data..=)
              Prelude.<$> selfManagedActiveDirectoryConfiguration,
            ("ThroughputCapacity" Data..=)
              Prelude.<$> throughputCapacity,
            ("WeeklyMaintenanceStartTime" Data..=)
              Prelude.<$> weeklyMaintenanceStartTime
          ]
      )
