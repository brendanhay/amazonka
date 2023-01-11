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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateFileSystemOpenZFSConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.DiskIopsConfiguration
import Amazonka.FSx.Types.OpenZFSCreateRootVolumeConfiguration
import Amazonka.FSx.Types.OpenZFSDeploymentType
import qualified Amazonka.Prelude as Prelude

-- | The Amazon FSx for OpenZFS configuration properties for the file system
-- that you are creating.
--
-- /See:/ 'newCreateFileSystemOpenZFSConfiguration' smart constructor.
data CreateFileSystemOpenZFSConfiguration = CreateFileSystemOpenZFSConfiguration'
  { automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | A Boolean value indicating whether tags for the file system should be
    -- copied to backups. This value defaults to @false@. If it\'s set to
    -- @true@, all tags for the file system are copied to all automatic and
    -- user-initiated backups where the user doesn\'t specify tags. If this
    -- value is @true@, and you specify one or more tags, only the specified
    -- tags are copied to backups. If you specify one or more tags when
    -- creating a user-initiated backup, no tags are copied from the file
    -- system, regardless of this value.
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value indicating whether tags for the file system should be
    -- copied to volumes. This value defaults to @false@. If it\'s set to
    -- @true@, all tags for the file system are copied to volumes where the
    -- user doesn\'t specify tags. If this value is @true@, and you specify one
    -- or more tags, only the specified tags are copied to volumes. If you
    -- specify one or more tags when creating the volume, no tags are copied
    -- from the file system, regardless of this value.
    copyTagsToVolumes :: Prelude.Maybe Prelude.Bool,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    diskIopsConfiguration :: Prelude.Maybe DiskIopsConfiguration,
    -- | The configuration Amazon FSx uses when creating the root value of the
    -- Amazon FSx for OpenZFS file system. All volumes are children of the root
    -- volume.
    rootVolumeConfiguration :: Prelude.Maybe OpenZFSCreateRootVolumeConfiguration,
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies the file system deployment type. Single AZ deployment types
    -- are configured for redundancy within a single Availability Zone in an
    -- Amazon Web Services Region . Valid values are the following:
    --
    -- -   @SINGLE_AZ_1@- (Default) Creates file systems with throughput
    --     capacities of 64 - 4,096 MB\/s. @Single_AZ_1@ is available in all
    --     Amazon Web Services Regions where Amazon FSx for OpenZFS is
    --     available, except US West (Oregon).
    --
    -- -   @SINGLE_AZ_2@- Creates file systems with throughput capacities of
    --     160 - 10,240 MB\/s using an NVMe L2ARC cache. @Single_AZ_2@ is
    --     available only in the US East (N. Virginia), US East (Ohio), US West
    --     (Oregon), and Europe (Ireland) Amazon Web Services Regions.
    --
    -- For more information, see:
    -- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/availability-durability.html#available-aws-regions Deployment type availability>
    -- and
    -- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#zfs-fs-performance File system performance>
    -- in the /Amazon FSx for OpenZFS User Guide/.
    deploymentType :: OpenZFSDeploymentType,
    -- | Specifies the throughput of an Amazon FSx for OpenZFS file system,
    -- measured in megabytes per second (MB\/s). Valid values depend on the
    -- DeploymentType you choose, as follows:
    --
    -- -   For @SINGLE_AZ_1@, valid values are 64, 128, 256, 512, 1024, 2048,
    --     3072, or 4096 MB\/s.
    --
    -- -   For @SINGLE_AZ_2@, valid values are 160, 320, 640, 1280, 2560, 3840,
    --     5120, 7680, or 10240 MB\/s.
    --
    -- You pay for additional throughput capacity that you provision.
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
-- 'automaticBackupRetentionDays', 'createFileSystemOpenZFSConfiguration_automaticBackupRetentionDays' - Undocumented member.
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
-- 'copyTagsToVolumes', 'createFileSystemOpenZFSConfiguration_copyTagsToVolumes' - A Boolean value indicating whether tags for the file system should be
-- copied to volumes. This value defaults to @false@. If it\'s set to
-- @true@, all tags for the file system are copied to volumes where the
-- user doesn\'t specify tags. If this value is @true@, and you specify one
-- or more tags, only the specified tags are copied to volumes. If you
-- specify one or more tags when creating the volume, no tags are copied
-- from the file system, regardless of this value.
--
-- 'dailyAutomaticBackupStartTime', 'createFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'diskIopsConfiguration', 'createFileSystemOpenZFSConfiguration_diskIopsConfiguration' - Undocumented member.
--
-- 'rootVolumeConfiguration', 'createFileSystemOpenZFSConfiguration_rootVolumeConfiguration' - The configuration Amazon FSx uses when creating the root value of the
-- Amazon FSx for OpenZFS file system. All volumes are children of the root
-- volume.
--
-- 'weeklyMaintenanceStartTime', 'createFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
--
-- 'deploymentType', 'createFileSystemOpenZFSConfiguration_deploymentType' - Specifies the file system deployment type. Single AZ deployment types
-- are configured for redundancy within a single Availability Zone in an
-- Amazon Web Services Region . Valid values are the following:
--
-- -   @SINGLE_AZ_1@- (Default) Creates file systems with throughput
--     capacities of 64 - 4,096 MB\/s. @Single_AZ_1@ is available in all
--     Amazon Web Services Regions where Amazon FSx for OpenZFS is
--     available, except US West (Oregon).
--
-- -   @SINGLE_AZ_2@- Creates file systems with throughput capacities of
--     160 - 10,240 MB\/s using an NVMe L2ARC cache. @Single_AZ_2@ is
--     available only in the US East (N. Virginia), US East (Ohio), US West
--     (Oregon), and Europe (Ireland) Amazon Web Services Regions.
--
-- For more information, see:
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/availability-durability.html#available-aws-regions Deployment type availability>
-- and
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#zfs-fs-performance File system performance>
-- in the /Amazon FSx for OpenZFS User Guide/.
--
-- 'throughputCapacity', 'createFileSystemOpenZFSConfiguration_throughputCapacity' - Specifies the throughput of an Amazon FSx for OpenZFS file system,
-- measured in megabytes per second (MB\/s). Valid values depend on the
-- DeploymentType you choose, as follows:
--
-- -   For @SINGLE_AZ_1@, valid values are 64, 128, 256, 512, 1024, 2048,
--     3072, or 4096 MB\/s.
--
-- -   For @SINGLE_AZ_2@, valid values are 160, 320, 640, 1280, 2560, 3840,
--     5120, 7680, or 10240 MB\/s.
--
-- You pay for additional throughput capacity that you provision.
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
      { automaticBackupRetentionDays =
          Prelude.Nothing,
        copyTagsToBackups = Prelude.Nothing,
        copyTagsToVolumes = Prelude.Nothing,
        dailyAutomaticBackupStartTime =
          Prelude.Nothing,
        diskIopsConfiguration =
          Prelude.Nothing,
        rootVolumeConfiguration =
          Prelude.Nothing,
        weeklyMaintenanceStartTime =
          Prelude.Nothing,
        deploymentType = pDeploymentType_,
        throughputCapacity =
          pThroughputCapacity_
      }

-- | Undocumented member.
createFileSystemOpenZFSConfiguration_automaticBackupRetentionDays :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Natural)
createFileSystemOpenZFSConfiguration_automaticBackupRetentionDays = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: CreateFileSystemOpenZFSConfiguration)

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

-- | A Boolean value indicating whether tags for the file system should be
-- copied to volumes. This value defaults to @false@. If it\'s set to
-- @true@, all tags for the file system are copied to volumes where the
-- user doesn\'t specify tags. If this value is @true@, and you specify one
-- or more tags, only the specified tags are copied to volumes. If you
-- specify one or more tags when creating the volume, no tags are copied
-- from the file system, regardless of this value.
createFileSystemOpenZFSConfiguration_copyTagsToVolumes :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Bool)
createFileSystemOpenZFSConfiguration_copyTagsToVolumes = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {copyTagsToVolumes} -> copyTagsToVolumes) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {copyTagsToVolumes = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
createFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
createFileSystemOpenZFSConfiguration_diskIopsConfiguration :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe DiskIopsConfiguration)
createFileSystemOpenZFSConfiguration_diskIopsConfiguration = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {diskIopsConfiguration} -> diskIopsConfiguration) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {diskIopsConfiguration = a} :: CreateFileSystemOpenZFSConfiguration)

-- | The configuration Amazon FSx uses when creating the root value of the
-- Amazon FSx for OpenZFS file system. All volumes are children of the root
-- volume.
createFileSystemOpenZFSConfiguration_rootVolumeConfiguration :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe OpenZFSCreateRootVolumeConfiguration)
createFileSystemOpenZFSConfiguration_rootVolumeConfiguration = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {rootVolumeConfiguration} -> rootVolumeConfiguration) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {rootVolumeConfiguration = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Undocumented member.
createFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' CreateFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Specifies the file system deployment type. Single AZ deployment types
-- are configured for redundancy within a single Availability Zone in an
-- Amazon Web Services Region . Valid values are the following:
--
-- -   @SINGLE_AZ_1@- (Default) Creates file systems with throughput
--     capacities of 64 - 4,096 MB\/s. @Single_AZ_1@ is available in all
--     Amazon Web Services Regions where Amazon FSx for OpenZFS is
--     available, except US West (Oregon).
--
-- -   @SINGLE_AZ_2@- Creates file systems with throughput capacities of
--     160 - 10,240 MB\/s using an NVMe L2ARC cache. @Single_AZ_2@ is
--     available only in the US East (N. Virginia), US East (Ohio), US West
--     (Oregon), and Europe (Ireland) Amazon Web Services Regions.
--
-- For more information, see:
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/availability-durability.html#available-aws-regions Deployment type availability>
-- and
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#zfs-fs-performance File system performance>
-- in the /Amazon FSx for OpenZFS User Guide/.
createFileSystemOpenZFSConfiguration_deploymentType :: Lens.Lens' CreateFileSystemOpenZFSConfiguration OpenZFSDeploymentType
createFileSystemOpenZFSConfiguration_deploymentType = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {deploymentType} -> deploymentType) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {deploymentType = a} :: CreateFileSystemOpenZFSConfiguration)

-- | Specifies the throughput of an Amazon FSx for OpenZFS file system,
-- measured in megabytes per second (MB\/s). Valid values depend on the
-- DeploymentType you choose, as follows:
--
-- -   For @SINGLE_AZ_1@, valid values are 64, 128, 256, 512, 1024, 2048,
--     3072, or 4096 MB\/s.
--
-- -   For @SINGLE_AZ_2@, valid values are 160, 320, 640, 1280, 2560, 3840,
--     5120, 7680, or 10240 MB\/s.
--
-- You pay for additional throughput capacity that you provision.
createFileSystemOpenZFSConfiguration_throughputCapacity :: Lens.Lens' CreateFileSystemOpenZFSConfiguration Prelude.Natural
createFileSystemOpenZFSConfiguration_throughputCapacity = Lens.lens (\CreateFileSystemOpenZFSConfiguration' {throughputCapacity} -> throughputCapacity) (\s@CreateFileSystemOpenZFSConfiguration' {} a -> s {throughputCapacity = a} :: CreateFileSystemOpenZFSConfiguration)

instance
  Prelude.Hashable
    CreateFileSystemOpenZFSConfiguration
  where
  hashWithSalt
    _salt
    CreateFileSystemOpenZFSConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` copyTagsToBackups
        `Prelude.hashWithSalt` copyTagsToVolumes
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` diskIopsConfiguration
        `Prelude.hashWithSalt` rootVolumeConfiguration
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime
        `Prelude.hashWithSalt` deploymentType
        `Prelude.hashWithSalt` throughputCapacity

instance
  Prelude.NFData
    CreateFileSystemOpenZFSConfiguration
  where
  rnf CreateFileSystemOpenZFSConfiguration' {..} =
    Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf copyTagsToVolumes
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf diskIopsConfiguration
      `Prelude.seq` Prelude.rnf rootVolumeConfiguration
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf throughputCapacity

instance
  Data.ToJSON
    CreateFileSystemOpenZFSConfiguration
  where
  toJSON CreateFileSystemOpenZFSConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutomaticBackupRetentionDays" Data..=)
              Prelude.<$> automaticBackupRetentionDays,
            ("CopyTagsToBackups" Data..=)
              Prelude.<$> copyTagsToBackups,
            ("CopyTagsToVolumes" Data..=)
              Prelude.<$> copyTagsToVolumes,
            ("DailyAutomaticBackupStartTime" Data..=)
              Prelude.<$> dailyAutomaticBackupStartTime,
            ("DiskIopsConfiguration" Data..=)
              Prelude.<$> diskIopsConfiguration,
            ("RootVolumeConfiguration" Data..=)
              Prelude.<$> rootVolumeConfiguration,
            ("WeeklyMaintenanceStartTime" Data..=)
              Prelude.<$> weeklyMaintenanceStartTime,
            Prelude.Just
              ("DeploymentType" Data..= deploymentType),
            Prelude.Just
              ("ThroughputCapacity" Data..= throughputCapacity)
          ]
      )
