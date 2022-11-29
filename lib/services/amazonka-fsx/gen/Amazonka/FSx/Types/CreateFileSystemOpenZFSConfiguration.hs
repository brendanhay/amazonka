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
-- Module      : Amazonka.FSx.Types.CreateFileSystemOpenZFSConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateFileSystemOpenZFSConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.DiskIopsConfiguration
import Amazonka.FSx.Types.OpenZFSCreateRootVolumeConfiguration
import Amazonka.FSx.Types.OpenZFSDeploymentType
import qualified Amazonka.Prelude as Prelude

-- | The Amazon FSx for OpenZFS configuration properties for the file system
-- that you are creating.
--
-- /See:/ 'newCreateFileSystemOpenZFSConfiguration' smart constructor.
data CreateFileSystemOpenZFSConfiguration = CreateFileSystemOpenZFSConfiguration'
  { -- | A Boolean value indicating whether tags for the file system should be
    -- copied to backups. This value defaults to @false@. If it\'s set to
    -- @true@, all tags for the file system are copied to all automatic and
    -- user-initiated backups where the user doesn\'t specify tags. If this
    -- value is @true@, and you specify one or more tags, only the specified
    -- tags are copied to backups. If you specify one or more tags when
    -- creating a user-initiated backup, no tags are copied from the file
    -- system, regardless of this value.
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    diskIopsConfiguration :: Prelude.Maybe DiskIopsConfiguration,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether tags for the file system should be
    -- copied to volumes. This value defaults to @false@. If it\'s set to
    -- @true@, all tags for the file system are copied to volumes where the
    -- user doesn\'t specify tags. If this value is @true@, and you specify one
    -- or more tags, only the specified tags are copied to volumes. If you
    -- specify one or more tags when creating the volume, no tags are copied
    -- from the file system, regardless of this value.
    copyTagsToVolumes :: Prelude.Maybe Prelude.Bool,
    -- | The configuration Amazon FSx uses when creating the root value of the
    -- Amazon FSx for OpenZFS file system. All volumes are children of the root
    -- volume.
    rootVolumeConfiguration :: Prelude.Maybe OpenZFSCreateRootVolumeConfiguration,
    -- | Specifies the file system deployment type. Amazon FSx for OpenZFS
    -- supports @SINGLE_AZ_1@. @SINGLE_AZ_1@ deployment type is configured for
    -- redundancy within a single Availability Zone.
    deploymentType :: OpenZFSDeploymentType,
    -- | Specifies the throughput of an Amazon FSx for OpenZFS file system,
    -- measured in megabytes per second (MB\/s). Valid values are 64, 128, 256,
    -- 512, 1024, 2048, 3072, or 4096 MB\/s. You pay for additional throughput
    -- capacity that you provision.
    throughputCapacity :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileSystemOpenZFSConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyTagsToBackups', 'createFileSystemOpenZFSConfiguration_copyTagsToBackups' - A Boolean value indicating whether tags for the file system should be
-- copied to backups. This value defaults to @false@. If it\'s set to
-- @true@, all tags for the file system are copied to all automatic and
-- user-initiated backups where the user doesn\'t specify tags. If this
-- value is @true@, and you specify one or more tags, only the specified
-- tags are copied to backups. If you specify one or more tags when
-- creating a user-initiated backup, no tags are copied from the file
-- system, regardless of this value.
--
-- 'weeklyMaintenanceStartTime', 'createFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
--
-- 'automaticBackupRetentionDays', 'createFileSystemOpenZFSConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'diskIopsConfiguration', 'createFileSystemOpenZFSConfiguration_diskIopsConfiguration' - Undocumented member.
--
-- 'dailyAutomaticBackupStartTime', 'createFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'copyTagsToVolumes', 'createFileSystemOpenZFSConfiguration_copyTagsToVolumes' - A Boolean value indicating whether tags for the file system should be
-- copied to volumes. This value defaults to @false@. If it\'s set to
-- @true@, all tags for the file system are copied to volumes where the
-- user doesn\'t specify tags. If this value is @true@, and you specify one
-- or more tags, only the specified tags are copied to volumes. If you
-- specify one or more tags when creating the volume, no tags are copied
-- from the file system, regardless of this value.
--
-- 'rootVolumeConfiguration', 'createFileSystemOpenZFSConfiguration_rootVolumeConfiguration' - The configuration Amazon FSx uses when creating the root value of the
-- Amazon FSx for OpenZFS file system. All volumes are children of the root
-- volume.
--
-- 'deploymentType', 'createFileSystemOpenZFSConfiguration_deploymentType' - Specifies the file system deployment type. Amazon FSx for OpenZFS
-- supports @SINGLE_AZ_1@. @SINGLE_AZ_1@ deployment type is configured for
-- redundancy within a single Availability Zone.
--
-- 'throughputCapacity', 'createFileSystemOpenZFSConfiguration_throughputCapacity' - Specifies the throughput of an Amazon FSx for OpenZFS file system,
-- measured in megabytes per second (MB\/s). Valid values are 64, 128, 256,
-- 512, 1024, 2048, 3072, or 4096 MB\/s. You pay for additional throughput
-- capacity that you provision.
newCreateFileSystemOpenZFSConfiguration ::
  -- | 'deploymentType'
  OpenZFSDeploymentType ->
  -- | 'throughputCapacity'
  Prelude.Natural ->
  CreateFileSystemOpenZFSConfiguration
newCreateFileSystemOpenZFSConfiguration
  pDeploymentType_
  pThroughputCapacity_ =
    CreateFileSystemOpenZFSConfiguration'
      { copyTagsToBackups =
          Prelude.Nothing,
        weeklyMaintenanceStartTime =
          Prelude.Nothing,
        automaticBackupRetentionDays =
          Prelude.Nothing,
        diskIopsConfiguration =
          Prelude.Nothing,
        dailyAutomaticBackupStartTime =
          Prelude.Nothing,
        copyTagsToVolumes = Prelude.Nothing,
        rootVolumeConfiguration =
          Prelude.Nothing,
        deploymentType = pDeploymentType_,
        throughputCapacity =
          pThroughputCapacity_
      }

-- | A Boolean value indicating whether tags for the file system should be
-- copied to backups. This value defaults to @false@. If it\'s set to
-- @true@, all tags for the file system are copied to all automatic and
-- user-initiated backups where the user doesn\'t specify tags. If this
-- value is @true@, and you specify one or more tags, only the specified
-- tags are copied to backups. If you specify one or more tags when
-- creating a user-initiated backup, no tags are copied from the file
-- system, regardless of this value.
createFileSystemOpenZFSConfiguration_copyTagsToBackups :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Bool)
createFileSystemOpenZFSConfiguration_copyTagsToBackups = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {copyTagsToBackups = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
createFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
createFileSystemOpenZFSConfiguration_automaticBackupRetentionDays :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Natural)
createFileSystemOpenZFSConfiguration_automaticBackupRetentionDays = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
createFileSystemOpenZFSConfiguration_diskIopsConfiguration :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe DiskIopsConfiguration)
createFileSystemOpenZFSConfiguration_diskIopsConfiguration = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {diskIopsConfiguration} -> diskIopsConfiguration) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {diskIopsConfiguration = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
createFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: CreateFileSystemOpenZFSConfiguration)

-- | A Boolean value indicating whether tags for the file system should be
-- copied to volumes. This value defaults to @false@. If it\'s set to
-- @true@, all tags for the file system are copied to volumes where the
-- user doesn\'t specify tags. If this value is @true@, and you specify one
-- or more tags, only the specified tags are copied to volumes. If you
-- specify one or more tags when creating the volume, no tags are copied
-- from the file system, regardless of this value.
createFileSystemOpenZFSConfiguration_copyTagsToVolumes :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Bool)
createFileSystemOpenZFSConfiguration_copyTagsToVolumes = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {copyTagsToVolumes} -> copyTagsToVolumes) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {copyTagsToVolumes = a} :: CreateFileSystemOpenZFSConfiguration)

-- | The configuration Amazon FSx uses when creating the root value of the
-- Amazon FSx for OpenZFS file system. All volumes are children of the root
-- volume.
createFileSystemOpenZFSConfiguration_rootVolumeConfiguration :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe OpenZFSCreateRootVolumeConfiguration)
createFileSystemOpenZFSConfiguration_rootVolumeConfiguration = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {rootVolumeConfiguration} -> rootVolumeConfiguration) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {rootVolumeConfiguration = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Specifies the file system deployment type. Amazon FSx for OpenZFS
-- supports @SINGLE_AZ_1@. @SINGLE_AZ_1@ deployment type is configured for
-- redundancy within a single Availability Zone.
createFileSystemOpenZFSConfiguration_deploymentType :: Lens.Lens' CreateFileSystemOpenZFSConfiguration OpenZFSDeploymentType
createFileSystemOpenZFSConfiguration_deploymentType = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {deploymentType} -> deploymentType) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {deploymentType = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Specifies the throughput of an Amazon FSx for OpenZFS file system,
-- measured in megabytes per second (MB\/s). Valid values are 64, 128, 256,
-- 512, 1024, 2048, 3072, or 4096 MB\/s. You pay for additional throughput
-- capacity that you provision.
createFileSystemOpenZFSConfiguration_throughputCapacity :: Lens.Lens' CreateFileSystemOpenZFSConfiguration Prelude.Natural
createFileSystemOpenZFSConfiguration_throughputCapacity = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {throughputCapacity} -> throughputCapacity) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {throughputCapacity = a} :: CreateFileSystemOpenZFSConfiguration)

instance
  Prelude.Hashable
    CreateFileSystemOpenZFSConfiguration
  where
  hashWithSalt
    _salt
    CreateFileSystemOpenZFSConfiguration' {..} =
      _salt `Prelude.hashWithSalt` copyTagsToBackups
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` diskIopsConfiguration
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` copyTagsToVolumes
        `Prelude.hashWithSalt` rootVolumeConfiguration
        `Prelude.hashWithSalt` deploymentType
        `Prelude.hashWithSalt` throughputCapacity

instance
  Prelude.NFData
    CreateFileSystemOpenZFSConfiguration
  where
  rnf CreateFileSystemOpenZFSConfiguration' {..} =
    Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf diskIopsConfiguration
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf copyTagsToVolumes
      `Prelude.seq` Prelude.rnf rootVolumeConfiguration
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf throughputCapacity

instance
  Core.ToJSON
    CreateFileSystemOpenZFSConfiguration
  where
  toJSON CreateFileSystemOpenZFSConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CopyTagsToBackups" Core..=)
              Prelude.<$> copyTagsToBackups,
            ("WeeklyMaintenanceStartTime" Core..=)
              Prelude.<$> weeklyMaintenanceStartTime,
            ("AutomaticBackupRetentionDays" Core..=)
              Prelude.<$> automaticBackupRetentionDays,
            ("DiskIopsConfiguration" Core..=)
              Prelude.<$> diskIopsConfiguration,
            ("DailyAutomaticBackupStartTime" Core..=)
              Prelude.<$> dailyAutomaticBackupStartTime,
            ("CopyTagsToVolumes" Core..=)
              Prelude.<$> copyTagsToVolumes,
            ("RootVolumeConfiguration" Core..=)
              Prelude.<$> rootVolumeConfiguration,
            Prelude.Just
              ("DeploymentType" Core..= deploymentType),
            Prelude.Just
              ("ThroughputCapacity" Core..= throughputCapacity)
          ]
      )
