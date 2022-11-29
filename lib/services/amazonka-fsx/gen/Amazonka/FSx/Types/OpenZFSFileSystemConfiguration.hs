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
-- Module      : Amazonka.FSx.Types.OpenZFSFileSystemConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OpenZFSFileSystemConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.DiskIopsConfiguration
import Amazonka.FSx.Types.OpenZFSDeploymentType
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the Amazon FSx for OpenZFS file system.
--
-- /See:/ 'newOpenZFSFileSystemConfiguration' smart constructor.
data OpenZFSFileSystemConfiguration = OpenZFSFileSystemConfiguration'
  { -- | A Boolean value indicating whether tags on the file system should be
    -- copied to backups. If it\'s set to @true@, all tags on the file system
    -- are copied to all automatic backups and any user-initiated backups where
    -- the user doesn\'t specify any tags. If this value is @true@ and you
    -- specify one or more tags, only the specified tags are copied to backups.
    -- If you specify one or more tags when creating a user-initiated backup,
    -- no tags are copied from the file system, regardless of this value.
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    -- | The throughput of an Amazon FSx file system, measured in megabytes per
    -- second (MBps). Valid values are 64, 128, 256, 512, 1024, 2048, 3072, or
    -- 4096 MB\/s.
    throughputCapacity :: Prelude.Maybe Prelude.Natural,
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the file-system deployment type. Amazon FSx for OpenZFS
    -- supports @SINGLE_AZ_1@. @SINGLE_AZ_1@ is a file system configured for a
    -- single Availability Zone (AZ) of redundancy.
    deploymentType :: Prelude.Maybe OpenZFSDeploymentType,
    -- | The ID of the root volume of the OpenZFS file system.
    rootVolumeId :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'OpenZFSFileSystemConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyTagsToBackups', 'openZFSFileSystemConfiguration_copyTagsToBackups' - A Boolean value indicating whether tags on the file system should be
-- copied to backups. If it\'s set to @true@, all tags on the file system
-- are copied to all automatic backups and any user-initiated backups where
-- the user doesn\'t specify any tags. If this value is @true@ and you
-- specify one or more tags, only the specified tags are copied to backups.
-- If you specify one or more tags when creating a user-initiated backup,
-- no tags are copied from the file system, regardless of this value.
--
-- 'weeklyMaintenanceStartTime', 'openZFSFileSystemConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
--
-- 'throughputCapacity', 'openZFSFileSystemConfiguration_throughputCapacity' - The throughput of an Amazon FSx file system, measured in megabytes per
-- second (MBps). Valid values are 64, 128, 256, 512, 1024, 2048, 3072, or
-- 4096 MB\/s.
--
-- 'automaticBackupRetentionDays', 'openZFSFileSystemConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'deploymentType', 'openZFSFileSystemConfiguration_deploymentType' - Specifies the file-system deployment type. Amazon FSx for OpenZFS
-- supports @SINGLE_AZ_1@. @SINGLE_AZ_1@ is a file system configured for a
-- single Availability Zone (AZ) of redundancy.
--
-- 'rootVolumeId', 'openZFSFileSystemConfiguration_rootVolumeId' - The ID of the root volume of the OpenZFS file system.
--
-- 'diskIopsConfiguration', 'openZFSFileSystemConfiguration_diskIopsConfiguration' - Undocumented member.
--
-- 'dailyAutomaticBackupStartTime', 'openZFSFileSystemConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'copyTagsToVolumes', 'openZFSFileSystemConfiguration_copyTagsToVolumes' - A Boolean value indicating whether tags for the volume should be copied
-- to snapshots. This value defaults to @false@. If it\'s set to @true@,
-- all tags for the volume are copied to snapshots where the user doesn\'t
-- specify tags. If this value is @true@ and you specify one or more tags,
-- only the specified tags are copied to snapshots. If you specify one or
-- more tags when creating the snapshot, no tags are copied from the
-- volume, regardless of this value.
newOpenZFSFileSystemConfiguration ::
  OpenZFSFileSystemConfiguration
newOpenZFSFileSystemConfiguration =
  OpenZFSFileSystemConfiguration'
    { copyTagsToBackups =
        Prelude.Nothing,
      weeklyMaintenanceStartTime =
        Prelude.Nothing,
      throughputCapacity = Prelude.Nothing,
      automaticBackupRetentionDays =
        Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      rootVolumeId = Prelude.Nothing,
      diskIopsConfiguration = Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing,
      copyTagsToVolumes = Prelude.Nothing
    }

-- | A Boolean value indicating whether tags on the file system should be
-- copied to backups. If it\'s set to @true@, all tags on the file system
-- are copied to all automatic backups and any user-initiated backups where
-- the user doesn\'t specify any tags. If this value is @true@ and you
-- specify one or more tags, only the specified tags are copied to backups.
-- If you specify one or more tags when creating a user-initiated backup,
-- no tags are copied from the file system, regardless of this value.
openZFSFileSystemConfiguration_copyTagsToBackups :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Bool)
openZFSFileSystemConfiguration_copyTagsToBackups = Lens.lens (\OpenZFSFileSystemConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@OpenZFSFileSystemConfiguration' {} a -> s {copyTagsToBackups = a} :: OpenZFSFileSystemConfiguration)

-- | Undocumented member.
openZFSFileSystemConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Text)
openZFSFileSystemConfiguration_weeklyMaintenanceStartTime = Lens.lens (\OpenZFSFileSystemConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@OpenZFSFileSystemConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: OpenZFSFileSystemConfiguration)

-- | The throughput of an Amazon FSx file system, measured in megabytes per
-- second (MBps). Valid values are 64, 128, 256, 512, 1024, 2048, 3072, or
-- 4096 MB\/s.
openZFSFileSystemConfiguration_throughputCapacity :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
openZFSFileSystemConfiguration_throughputCapacity = Lens.lens (\OpenZFSFileSystemConfiguration' {throughputCapacity} -> throughputCapacity) (\s@OpenZFSFileSystemConfiguration' {} a -> s {throughputCapacity = a} :: OpenZFSFileSystemConfiguration)

-- | Undocumented member.
openZFSFileSystemConfiguration_automaticBackupRetentionDays :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
openZFSFileSystemConfiguration_automaticBackupRetentionDays = Lens.lens (\OpenZFSFileSystemConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@OpenZFSFileSystemConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: OpenZFSFileSystemConfiguration)

-- | Specifies the file-system deployment type. Amazon FSx for OpenZFS
-- supports @SINGLE_AZ_1@. @SINGLE_AZ_1@ is a file system configured for a
-- single Availability Zone (AZ) of redundancy.
openZFSFileSystemConfiguration_deploymentType :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe OpenZFSDeploymentType)
openZFSFileSystemConfiguration_deploymentType = Lens.lens (\OpenZFSFileSystemConfiguration' {deploymentType} -> deploymentType) (\s@OpenZFSFileSystemConfiguration' {} a -> s {deploymentType = a} :: OpenZFSFileSystemConfiguration)

-- | The ID of the root volume of the OpenZFS file system.
openZFSFileSystemConfiguration_rootVolumeId :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Text)
openZFSFileSystemConfiguration_rootVolumeId = Lens.lens (\OpenZFSFileSystemConfiguration' {rootVolumeId} -> rootVolumeId) (\s@OpenZFSFileSystemConfiguration' {} a -> s {rootVolumeId = a} :: OpenZFSFileSystemConfiguration)

-- | Undocumented member.
openZFSFileSystemConfiguration_diskIopsConfiguration :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe DiskIopsConfiguration)
openZFSFileSystemConfiguration_diskIopsConfiguration = Lens.lens (\OpenZFSFileSystemConfiguration' {diskIopsConfiguration} -> diskIopsConfiguration) (\s@OpenZFSFileSystemConfiguration' {} a -> s {diskIopsConfiguration = a} :: OpenZFSFileSystemConfiguration)

-- | Undocumented member.
openZFSFileSystemConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Text)
openZFSFileSystemConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\OpenZFSFileSystemConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@OpenZFSFileSystemConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: OpenZFSFileSystemConfiguration)

-- | A Boolean value indicating whether tags for the volume should be copied
-- to snapshots. This value defaults to @false@. If it\'s set to @true@,
-- all tags for the volume are copied to snapshots where the user doesn\'t
-- specify tags. If this value is @true@ and you specify one or more tags,
-- only the specified tags are copied to snapshots. If you specify one or
-- more tags when creating the snapshot, no tags are copied from the
-- volume, regardless of this value.
openZFSFileSystemConfiguration_copyTagsToVolumes :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Bool)
openZFSFileSystemConfiguration_copyTagsToVolumes = Lens.lens (\OpenZFSFileSystemConfiguration' {copyTagsToVolumes} -> copyTagsToVolumes) (\s@OpenZFSFileSystemConfiguration' {} a -> s {copyTagsToVolumes = a} :: OpenZFSFileSystemConfiguration)

instance Core.FromJSON OpenZFSFileSystemConfiguration where
  parseJSON =
    Core.withObject
      "OpenZFSFileSystemConfiguration"
      ( \x ->
          OpenZFSFileSystemConfiguration'
            Prelude.<$> (x Core..:? "CopyTagsToBackups")
            Prelude.<*> (x Core..:? "WeeklyMaintenanceStartTime")
            Prelude.<*> (x Core..:? "ThroughputCapacity")
            Prelude.<*> (x Core..:? "AutomaticBackupRetentionDays")
            Prelude.<*> (x Core..:? "DeploymentType")
            Prelude.<*> (x Core..:? "RootVolumeId")
            Prelude.<*> (x Core..:? "DiskIopsConfiguration")
            Prelude.<*> (x Core..:? "DailyAutomaticBackupStartTime")
            Prelude.<*> (x Core..:? "CopyTagsToVolumes")
      )

instance
  Prelude.Hashable
    OpenZFSFileSystemConfiguration
  where
  hashWithSalt
    _salt
    OpenZFSFileSystemConfiguration' {..} =
      _salt `Prelude.hashWithSalt` copyTagsToBackups
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime
        `Prelude.hashWithSalt` throughputCapacity
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` deploymentType
        `Prelude.hashWithSalt` rootVolumeId
        `Prelude.hashWithSalt` diskIopsConfiguration
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` copyTagsToVolumes

instance
  Prelude.NFData
    OpenZFSFileSystemConfiguration
  where
  rnf OpenZFSFileSystemConfiguration' {..} =
    Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf throughputCapacity
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf rootVolumeId
      `Prelude.seq` Prelude.rnf diskIopsConfiguration
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf copyTagsToVolumes
