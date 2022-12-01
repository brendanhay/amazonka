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
-- Module      : Amazonka.FSx.Types.UpdateFileSystemOpenZFSConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.UpdateFileSystemOpenZFSConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.DiskIopsConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration updates for an Amazon FSx for OpenZFS file system.
--
-- /See:/ 'newUpdateFileSystemOpenZFSConfiguration' smart constructor.
data UpdateFileSystemOpenZFSConfiguration = UpdateFileSystemOpenZFSConfiguration'
  { -- | A Boolean value indicating whether tags for the file system should be
    -- copied to backups. This value defaults to @false@. If it\'s set to
    -- @true@, all tags for the file system are copied to all automatic and
    -- user-initiated backups where the user doesn\'t specify tags. If this
    -- value is @true@ and you specify one or more tags, only the specified
    -- tags are copied to backups. If you specify one or more tags when
    -- creating a user-initiated backup, no tags are copied from the file
    -- system, regardless of this value.
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    -- | The throughput of an Amazon FSx file system, measured in megabytes per
    -- second (MBps). Valid values are 64, 128, 256, 512, 1024, 2048, 3072, or
    -- 4096 MB\/s.
    throughputCapacity :: Prelude.Maybe Prelude.Natural,
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    diskIopsConfiguration :: Prelude.Maybe DiskIopsConfiguration,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether tags for the volume should be copied
    -- to snapshots. This value defaults to @false@. If it\'s set to @true@,
    -- all tags for the volume are copied to snapshots where the user doesn\'t
    -- specify tags. If this value is @true@ and you specify one or more tags,
    -- only the specified tags are copied to snapshots. If you specify one or
    -- more tags when creating the snapshot, no tags are copied from the
    -- volume, regardless of this value.
    copyTagsToVolumes :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileSystemOpenZFSConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyTagsToBackups', 'updateFileSystemOpenZFSConfiguration_copyTagsToBackups' - A Boolean value indicating whether tags for the file system should be
-- copied to backups. This value defaults to @false@. If it\'s set to
-- @true@, all tags for the file system are copied to all automatic and
-- user-initiated backups where the user doesn\'t specify tags. If this
-- value is @true@ and you specify one or more tags, only the specified
-- tags are copied to backups. If you specify one or more tags when
-- creating a user-initiated backup, no tags are copied from the file
-- system, regardless of this value.
--
-- 'weeklyMaintenanceStartTime', 'updateFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
--
-- 'throughputCapacity', 'updateFileSystemOpenZFSConfiguration_throughputCapacity' - The throughput of an Amazon FSx file system, measured in megabytes per
-- second (MBps). Valid values are 64, 128, 256, 512, 1024, 2048, 3072, or
-- 4096 MB\/s.
--
-- 'automaticBackupRetentionDays', 'updateFileSystemOpenZFSConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'diskIopsConfiguration', 'updateFileSystemOpenZFSConfiguration_diskIopsConfiguration' - Undocumented member.
--
-- 'dailyAutomaticBackupStartTime', 'updateFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'copyTagsToVolumes', 'updateFileSystemOpenZFSConfiguration_copyTagsToVolumes' - A Boolean value indicating whether tags for the volume should be copied
-- to snapshots. This value defaults to @false@. If it\'s set to @true@,
-- all tags for the volume are copied to snapshots where the user doesn\'t
-- specify tags. If this value is @true@ and you specify one or more tags,
-- only the specified tags are copied to snapshots. If you specify one or
-- more tags when creating the snapshot, no tags are copied from the
-- volume, regardless of this value.
newUpdateFileSystemOpenZFSConfiguration ::
  UpdateFileSystemOpenZFSConfiguration
newUpdateFileSystemOpenZFSConfiguration =
  UpdateFileSystemOpenZFSConfiguration'
    { copyTagsToBackups =
        Prelude.Nothing,
      weeklyMaintenanceStartTime =
        Prelude.Nothing,
      throughputCapacity = Prelude.Nothing,
      automaticBackupRetentionDays =
        Prelude.Nothing,
      diskIopsConfiguration =
        Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing,
      copyTagsToVolumes = Prelude.Nothing
    }

-- | A Boolean value indicating whether tags for the file system should be
-- copied to backups. This value defaults to @false@. If it\'s set to
-- @true@, all tags for the file system are copied to all automatic and
-- user-initiated backups where the user doesn\'t specify tags. If this
-- value is @true@ and you specify one or more tags, only the specified
-- tags are copied to backups. If you specify one or more tags when
-- creating a user-initiated backup, no tags are copied from the file
-- system, regardless of this value.
updateFileSystemOpenZFSConfiguration_copyTagsToBackups :: Lens.Lens' UpdateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Bool)
updateFileSystemOpenZFSConfiguration_copyTagsToBackups = Lens.lens (\UpdateFileSystemOpenZFSConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@UpdateFileSystemOpenZFSConfiguration' {} a -> s {copyTagsToBackups = a} :: UpdateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
updateFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' UpdateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Text)
updateFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime = Lens.lens (\UpdateFileSystemOpenZFSConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@UpdateFileSystemOpenZFSConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: UpdateFileSystemOpenZFSConfiguration)

-- | The throughput of an Amazon FSx file system, measured in megabytes per
-- second (MBps). Valid values are 64, 128, 256, 512, 1024, 2048, 3072, or
-- 4096 MB\/s.
updateFileSystemOpenZFSConfiguration_throughputCapacity :: Lens.Lens' UpdateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Natural)
updateFileSystemOpenZFSConfiguration_throughputCapacity = Lens.lens (\UpdateFileSystemOpenZFSConfiguration' {throughputCapacity} -> throughputCapacity) (\s@UpdateFileSystemOpenZFSConfiguration' {} a -> s {throughputCapacity = a} :: UpdateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
updateFileSystemOpenZFSConfiguration_automaticBackupRetentionDays :: Lens.Lens' UpdateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Natural)
updateFileSystemOpenZFSConfiguration_automaticBackupRetentionDays = Lens.lens (\UpdateFileSystemOpenZFSConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@UpdateFileSystemOpenZFSConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: UpdateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
updateFileSystemOpenZFSConfiguration_diskIopsConfiguration :: Lens.Lens' UpdateFileSystemOpenZFSConfiguration (Prelude.Maybe DiskIopsConfiguration)
updateFileSystemOpenZFSConfiguration_diskIopsConfiguration = Lens.lens (\UpdateFileSystemOpenZFSConfiguration' {diskIopsConfiguration} -> diskIopsConfiguration) (\s@UpdateFileSystemOpenZFSConfiguration' {} a -> s {diskIopsConfiguration = a} :: UpdateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
updateFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' UpdateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Text)
updateFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\UpdateFileSystemOpenZFSConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@UpdateFileSystemOpenZFSConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: UpdateFileSystemOpenZFSConfiguration)

-- | A Boolean value indicating whether tags for the volume should be copied
-- to snapshots. This value defaults to @false@. If it\'s set to @true@,
-- all tags for the volume are copied to snapshots where the user doesn\'t
-- specify tags. If this value is @true@ and you specify one or more tags,
-- only the specified tags are copied to snapshots. If you specify one or
-- more tags when creating the snapshot, no tags are copied from the
-- volume, regardless of this value.
updateFileSystemOpenZFSConfiguration_copyTagsToVolumes :: Lens.Lens' UpdateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Bool)
updateFileSystemOpenZFSConfiguration_copyTagsToVolumes = Lens.lens (\UpdateFileSystemOpenZFSConfiguration' {copyTagsToVolumes} -> copyTagsToVolumes) (\s@UpdateFileSystemOpenZFSConfiguration' {} a -> s {copyTagsToVolumes = a} :: UpdateFileSystemOpenZFSConfiguration)

instance
  Prelude.Hashable
    UpdateFileSystemOpenZFSConfiguration
  where
  hashWithSalt
    _salt
    UpdateFileSystemOpenZFSConfiguration' {..} =
      _salt `Prelude.hashWithSalt` copyTagsToBackups
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime
        `Prelude.hashWithSalt` throughputCapacity
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` diskIopsConfiguration
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` copyTagsToVolumes

instance
  Prelude.NFData
    UpdateFileSystemOpenZFSConfiguration
  where
  rnf UpdateFileSystemOpenZFSConfiguration' {..} =
    Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf throughputCapacity
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf diskIopsConfiguration
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf copyTagsToVolumes

instance
  Core.ToJSON
    UpdateFileSystemOpenZFSConfiguration
  where
  toJSON UpdateFileSystemOpenZFSConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CopyTagsToBackups" Core..=)
              Prelude.<$> copyTagsToBackups,
            ("WeeklyMaintenanceStartTime" Core..=)
              Prelude.<$> weeklyMaintenanceStartTime,
            ("ThroughputCapacity" Core..=)
              Prelude.<$> throughputCapacity,
            ("AutomaticBackupRetentionDays" Core..=)
              Prelude.<$> automaticBackupRetentionDays,
            ("DiskIopsConfiguration" Core..=)
              Prelude.<$> diskIopsConfiguration,
            ("DailyAutomaticBackupStartTime" Core..=)
              Prelude.<$> dailyAutomaticBackupStartTime,
            ("CopyTagsToVolumes" Core..=)
              Prelude.<$> copyTagsToVolumes
          ]
      )
