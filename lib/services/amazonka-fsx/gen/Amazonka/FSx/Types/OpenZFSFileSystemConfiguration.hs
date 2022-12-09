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
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.DiskIopsConfiguration
import Amazonka.FSx.Types.OpenZFSDeploymentType
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the Amazon FSx for OpenZFS file system.
--
-- /See:/ 'newOpenZFSFileSystemConfiguration' smart constructor.
data OpenZFSFileSystemConfiguration = OpenZFSFileSystemConfiguration'
  { automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | A Boolean value indicating whether tags on the file system should be
    -- copied to backups. If it\'s set to @true@, all tags on the file system
    -- are copied to all automatic backups and any user-initiated backups where
    -- the user doesn\'t specify any tags. If this value is @true@ and you
    -- specify one or more tags, only the specified tags are copied to backups.
    -- If you specify one or more tags when creating a user-initiated backup,
    -- no tags are copied from the file system, regardless of this value.
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value indicating whether tags for the volume should be copied
    -- to snapshots. This value defaults to @false@. If it\'s set to @true@,
    -- all tags for the volume are copied to snapshots where the user doesn\'t
    -- specify tags. If this value is @true@ and you specify one or more tags,
    -- only the specified tags are copied to snapshots. If you specify one or
    -- more tags when creating the snapshot, no tags are copied from the
    -- volume, regardless of this value.
    copyTagsToVolumes :: Prelude.Maybe Prelude.Bool,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies the file-system deployment type. Amazon FSx for OpenZFS
    -- supports  @SINGLE_AZ_1@ and @SINGLE_AZ_2@.
    deploymentType :: Prelude.Maybe OpenZFSDeploymentType,
    diskIopsConfiguration :: Prelude.Maybe DiskIopsConfiguration,
    -- | The ID of the root volume of the OpenZFS file system.
    rootVolumeId :: Prelude.Maybe Prelude.Text,
    -- | The throughput of an Amazon FSx file system, measured in megabytes per
    -- second (MBps).
    throughputCapacity :: Prelude.Maybe Prelude.Natural,
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text
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
-- 'automaticBackupRetentionDays', 'openZFSFileSystemConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'copyTagsToBackups', 'openZFSFileSystemConfiguration_copyTagsToBackups' - A Boolean value indicating whether tags on the file system should be
-- copied to backups. If it\'s set to @true@, all tags on the file system
-- are copied to all automatic backups and any user-initiated backups where
-- the user doesn\'t specify any tags. If this value is @true@ and you
-- specify one or more tags, only the specified tags are copied to backups.
-- If you specify one or more tags when creating a user-initiated backup,
-- no tags are copied from the file system, regardless of this value.
--
-- 'copyTagsToVolumes', 'openZFSFileSystemConfiguration_copyTagsToVolumes' - A Boolean value indicating whether tags for the volume should be copied
-- to snapshots. This value defaults to @false@. If it\'s set to @true@,
-- all tags for the volume are copied to snapshots where the user doesn\'t
-- specify tags. If this value is @true@ and you specify one or more tags,
-- only the specified tags are copied to snapshots. If you specify one or
-- more tags when creating the snapshot, no tags are copied from the
-- volume, regardless of this value.
--
-- 'dailyAutomaticBackupStartTime', 'openZFSFileSystemConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'deploymentType', 'openZFSFileSystemConfiguration_deploymentType' - Specifies the file-system deployment type. Amazon FSx for OpenZFS
-- supports  @SINGLE_AZ_1@ and @SINGLE_AZ_2@.
--
-- 'diskIopsConfiguration', 'openZFSFileSystemConfiguration_diskIopsConfiguration' - Undocumented member.
--
-- 'rootVolumeId', 'openZFSFileSystemConfiguration_rootVolumeId' - The ID of the root volume of the OpenZFS file system.
--
-- 'throughputCapacity', 'openZFSFileSystemConfiguration_throughputCapacity' - The throughput of an Amazon FSx file system, measured in megabytes per
-- second (MBps).
--
-- 'weeklyMaintenanceStartTime', 'openZFSFileSystemConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
newOpenZFSFileSystemConfiguration ::
  OpenZFSFileSystemConfiguration
newOpenZFSFileSystemConfiguration =
  OpenZFSFileSystemConfiguration'
    { automaticBackupRetentionDays =
        Prelude.Nothing,
      copyTagsToBackups = Prelude.Nothing,
      copyTagsToVolumes = Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      diskIopsConfiguration = Prelude.Nothing,
      rootVolumeId = Prelude.Nothing,
      throughputCapacity = Prelude.Nothing,
      weeklyMaintenanceStartTime =
        Prelude.Nothing
    }

-- | Undocumented member.
openZFSFileSystemConfiguration_automaticBackupRetentionDays :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
openZFSFileSystemConfiguration_automaticBackupRetentionDays = Lens.lens (\OpenZFSFileSystemConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@OpenZFSFileSystemConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: OpenZFSFileSystemConfiguration)

-- | A Boolean value indicating whether tags on the file system should be
-- copied to backups. If it\'s set to @true@, all tags on the file system
-- are copied to all automatic backups and any user-initiated backups where
-- the user doesn\'t specify any tags. If this value is @true@ and you
-- specify one or more tags, only the specified tags are copied to backups.
-- If you specify one or more tags when creating a user-initiated backup,
-- no tags are copied from the file system, regardless of this value.
openZFSFileSystemConfiguration_copyTagsToBackups :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Bool)
openZFSFileSystemConfiguration_copyTagsToBackups = Lens.lens (\OpenZFSFileSystemConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@OpenZFSFileSystemConfiguration' {} a -> s {copyTagsToBackups = a} :: OpenZFSFileSystemConfiguration)

-- | A Boolean value indicating whether tags for the volume should be copied
-- to snapshots. This value defaults to @false@. If it\'s set to @true@,
-- all tags for the volume are copied to snapshots where the user doesn\'t
-- specify tags. If this value is @true@ and you specify one or more tags,
-- only the specified tags are copied to snapshots. If you specify one or
-- more tags when creating the snapshot, no tags are copied from the
-- volume, regardless of this value.
openZFSFileSystemConfiguration_copyTagsToVolumes :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Bool)
openZFSFileSystemConfiguration_copyTagsToVolumes = Lens.lens (\OpenZFSFileSystemConfiguration' {copyTagsToVolumes} -> copyTagsToVolumes) (\s@OpenZFSFileSystemConfiguration' {} a -> s {copyTagsToVolumes = a} :: OpenZFSFileSystemConfiguration)

-- | Undocumented member.
openZFSFileSystemConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Text)
openZFSFileSystemConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\OpenZFSFileSystemConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@OpenZFSFileSystemConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: OpenZFSFileSystemConfiguration)

-- | Specifies the file-system deployment type. Amazon FSx for OpenZFS
-- supports  @SINGLE_AZ_1@ and @SINGLE_AZ_2@.
openZFSFileSystemConfiguration_deploymentType :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe OpenZFSDeploymentType)
openZFSFileSystemConfiguration_deploymentType = Lens.lens (\OpenZFSFileSystemConfiguration' {deploymentType} -> deploymentType) (\s@OpenZFSFileSystemConfiguration' {} a -> s {deploymentType = a} :: OpenZFSFileSystemConfiguration)

-- | Undocumented member.
openZFSFileSystemConfiguration_diskIopsConfiguration :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe DiskIopsConfiguration)
openZFSFileSystemConfiguration_diskIopsConfiguration = Lens.lens (\OpenZFSFileSystemConfiguration' {diskIopsConfiguration} -> diskIopsConfiguration) (\s@OpenZFSFileSystemConfiguration' {} a -> s {diskIopsConfiguration = a} :: OpenZFSFileSystemConfiguration)

-- | The ID of the root volume of the OpenZFS file system.
openZFSFileSystemConfiguration_rootVolumeId :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Text)
openZFSFileSystemConfiguration_rootVolumeId = Lens.lens (\OpenZFSFileSystemConfiguration' {rootVolumeId} -> rootVolumeId) (\s@OpenZFSFileSystemConfiguration' {} a -> s {rootVolumeId = a} :: OpenZFSFileSystemConfiguration)

-- | The throughput of an Amazon FSx file system, measured in megabytes per
-- second (MBps).
openZFSFileSystemConfiguration_throughputCapacity :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
openZFSFileSystemConfiguration_throughputCapacity = Lens.lens (\OpenZFSFileSystemConfiguration' {throughputCapacity} -> throughputCapacity) (\s@OpenZFSFileSystemConfiguration' {} a -> s {throughputCapacity = a} :: OpenZFSFileSystemConfiguration)

-- | Undocumented member.
openZFSFileSystemConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' OpenZFSFileSystemConfiguration (Prelude.Maybe Prelude.Text)
openZFSFileSystemConfiguration_weeklyMaintenanceStartTime = Lens.lens (\OpenZFSFileSystemConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@OpenZFSFileSystemConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: OpenZFSFileSystemConfiguration)

instance Data.FromJSON OpenZFSFileSystemConfiguration where
  parseJSON =
    Data.withObject
      "OpenZFSFileSystemConfiguration"
      ( \x ->
          OpenZFSFileSystemConfiguration'
            Prelude.<$> (x Data..:? "AutomaticBackupRetentionDays")
            Prelude.<*> (x Data..:? "CopyTagsToBackups")
            Prelude.<*> (x Data..:? "CopyTagsToVolumes")
            Prelude.<*> (x Data..:? "DailyAutomaticBackupStartTime")
            Prelude.<*> (x Data..:? "DeploymentType")
            Prelude.<*> (x Data..:? "DiskIopsConfiguration")
            Prelude.<*> (x Data..:? "RootVolumeId")
            Prelude.<*> (x Data..:? "ThroughputCapacity")
            Prelude.<*> (x Data..:? "WeeklyMaintenanceStartTime")
      )

instance
  Prelude.Hashable
    OpenZFSFileSystemConfiguration
  where
  hashWithSalt
    _salt
    OpenZFSFileSystemConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` copyTagsToBackups
        `Prelude.hashWithSalt` copyTagsToVolumes
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` deploymentType
        `Prelude.hashWithSalt` diskIopsConfiguration
        `Prelude.hashWithSalt` rootVolumeId
        `Prelude.hashWithSalt` throughputCapacity
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime

instance
  Prelude.NFData
    OpenZFSFileSystemConfiguration
  where
  rnf OpenZFSFileSystemConfiguration' {..} =
    Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf copyTagsToVolumes
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf diskIopsConfiguration
      `Prelude.seq` Prelude.rnf rootVolumeId
      `Prelude.seq` Prelude.rnf throughputCapacity
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
