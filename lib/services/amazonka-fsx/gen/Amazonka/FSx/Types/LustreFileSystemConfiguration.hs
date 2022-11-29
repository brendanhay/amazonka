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
-- Module      : Amazonka.FSx.Types.LustreFileSystemConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.LustreFileSystemConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.DataCompressionType
import Amazonka.FSx.Types.DataRepositoryConfiguration
import Amazonka.FSx.Types.DriveCacheType
import Amazonka.FSx.Types.LustreDeploymentType
import Amazonka.FSx.Types.LustreLogConfiguration
import Amazonka.FSx.Types.LustreRootSquashConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the Amazon FSx for Lustre file system.
--
-- /See:/ 'newLustreFileSystemConfiguration' smart constructor.
data LustreFileSystemConfiguration = LustreFileSystemConfiguration'
  { -- | A boolean flag indicating whether tags on the file system are copied to
    -- backups. If it\'s set to true, all tags on the file system are copied to
    -- all automatic backups and any user-initiated backups where the user
    -- doesn\'t specify any tags. If this value is true, and you specify one or
    -- more tags, only the specified tags are copied to backups. If you specify
    -- one or more tags when creating a user-initiated backup, no tags are
    -- copied from the file system, regardless of this value. (Default = false)
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    -- | The type of drive cache used by @PERSISTENT_1@ file systems that are
    -- provisioned with HDD storage devices. This parameter is required when
    -- @StorageType@ is HDD. When set to @READ@ the file system has an SSD
    -- storage cache that is sized to 20% of the file system\'s storage
    -- capacity. This improves the performance for frequently accessed files by
    -- caching up to 20% of the total storage capacity.
    --
    -- This parameter is required when @StorageType@ is set to HDD.
    driveCacheType :: Prelude.Maybe DriveCacheType,
    -- | The preferred start time to perform weekly maintenance, formatted
    -- d:HH:MM in the UTC time zone. Here, @d@ is the weekday number, from 1
    -- through 7, beginning with Monday and ending with Sunday.
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    -- | The Lustre logging configuration. Lustre logging writes the enabled log
    -- events for your file system to Amazon CloudWatch Logs.
    logConfiguration :: Prelude.Maybe LustreLogConfiguration,
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | The deployment type of the FSx for Lustre file system. /Scratch
    -- deployment type/ is designed for temporary storage and shorter-term
    -- processing of data.
    --
    -- @SCRATCH_1@ and @SCRATCH_2@ deployment types are best suited for when
    -- you need temporary storage and shorter-term processing of data. The
    -- @SCRATCH_2@ deployment type provides in-transit encryption of data and
    -- higher burst throughput capacity than @SCRATCH_1@.
    --
    -- The @PERSISTENT_1@ and @PERSISTENT_2@ deployment type is used for
    -- longer-term storage and workloads and encryption of data in transit.
    -- @PERSISTENT_2@ is built on Lustre v2.12 and offers higher
    -- @PerUnitStorageThroughput@ (up to 1000 MB\/s\/TiB) along with a lower
    -- minimum storage capacity requirement (600 GiB). To learn more about FSx
    -- for Lustre deployment types, see
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/lustre-deployment-types.html FSx for Lustre deployment options>.
    --
    -- The default is @SCRATCH_1@.
    deploymentType :: Prelude.Maybe LustreDeploymentType,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | Per unit storage throughput represents the megabytes per second of read
    -- or write throughput per 1 tebibyte of storage provisioned. File system
    -- throughput capacity is equal to Storage capacity (TiB) *
    -- PerUnitStorageThroughput (MB\/s\/TiB). This option is only valid for
    -- @PERSISTENT_1@ and @PERSISTENT_2@ deployment types.
    --
    -- Valid values:
    --
    -- -   For @PERSISTENT_1@ SSD storage: 50, 100, 200.
    --
    -- -   For @PERSISTENT_1@ HDD storage: 12, 40.
    --
    -- -   For @PERSISTENT_2@ SSD storage: 125, 250, 500, 1000.
    perUnitStorageThroughput :: Prelude.Maybe Prelude.Natural,
    -- | The data compression configuration for the file system.
    -- @DataCompressionType@ can have the following values:
    --
    -- -   @NONE@ - Data compression is turned off for the file system.
    --
    -- -   @LZ4@ - Data compression is turned on with the LZ4 algorithm.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/data-compression.html Lustre data compression>.
    dataCompressionType :: Prelude.Maybe DataCompressionType,
    -- | You use the @MountName@ value when mounting the file system.
    --
    -- For the @SCRATCH_1@ deployment type, this value is always \"@fsx@\". For
    -- @SCRATCH_2@, @PERSISTENT_1@, and @PERSISTENT_2@ deployment types, this
    -- value is a string that is unique within an Amazon Web Services Region.
    mountName :: Prelude.Maybe Prelude.Text,
    -- | The Lustre root squash configuration for an Amazon FSx for Lustre file
    -- system. When enabled, root squash restricts root-level access from
    -- clients that try to access your file system as a root user.
    rootSquashConfiguration :: Prelude.Maybe LustreRootSquashConfiguration,
    dataRepositoryConfiguration :: Prelude.Maybe DataRepositoryConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LustreFileSystemConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyTagsToBackups', 'lustreFileSystemConfiguration_copyTagsToBackups' - A boolean flag indicating whether tags on the file system are copied to
-- backups. If it\'s set to true, all tags on the file system are copied to
-- all automatic backups and any user-initiated backups where the user
-- doesn\'t specify any tags. If this value is true, and you specify one or
-- more tags, only the specified tags are copied to backups. If you specify
-- one or more tags when creating a user-initiated backup, no tags are
-- copied from the file system, regardless of this value. (Default = false)
--
-- 'driveCacheType', 'lustreFileSystemConfiguration_driveCacheType' - The type of drive cache used by @PERSISTENT_1@ file systems that are
-- provisioned with HDD storage devices. This parameter is required when
-- @StorageType@ is HDD. When set to @READ@ the file system has an SSD
-- storage cache that is sized to 20% of the file system\'s storage
-- capacity. This improves the performance for frequently accessed files by
-- caching up to 20% of the total storage capacity.
--
-- This parameter is required when @StorageType@ is set to HDD.
--
-- 'weeklyMaintenanceStartTime', 'lustreFileSystemConfiguration_weeklyMaintenanceStartTime' - The preferred start time to perform weekly maintenance, formatted
-- d:HH:MM in the UTC time zone. Here, @d@ is the weekday number, from 1
-- through 7, beginning with Monday and ending with Sunday.
--
-- 'logConfiguration', 'lustreFileSystemConfiguration_logConfiguration' - The Lustre logging configuration. Lustre logging writes the enabled log
-- events for your file system to Amazon CloudWatch Logs.
--
-- 'automaticBackupRetentionDays', 'lustreFileSystemConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'deploymentType', 'lustreFileSystemConfiguration_deploymentType' - The deployment type of the FSx for Lustre file system. /Scratch
-- deployment type/ is designed for temporary storage and shorter-term
-- processing of data.
--
-- @SCRATCH_1@ and @SCRATCH_2@ deployment types are best suited for when
-- you need temporary storage and shorter-term processing of data. The
-- @SCRATCH_2@ deployment type provides in-transit encryption of data and
-- higher burst throughput capacity than @SCRATCH_1@.
--
-- The @PERSISTENT_1@ and @PERSISTENT_2@ deployment type is used for
-- longer-term storage and workloads and encryption of data in transit.
-- @PERSISTENT_2@ is built on Lustre v2.12 and offers higher
-- @PerUnitStorageThroughput@ (up to 1000 MB\/s\/TiB) along with a lower
-- minimum storage capacity requirement (600 GiB). To learn more about FSx
-- for Lustre deployment types, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/lustre-deployment-types.html FSx for Lustre deployment options>.
--
-- The default is @SCRATCH_1@.
--
-- 'dailyAutomaticBackupStartTime', 'lustreFileSystemConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'perUnitStorageThroughput', 'lustreFileSystemConfiguration_perUnitStorageThroughput' - Per unit storage throughput represents the megabytes per second of read
-- or write throughput per 1 tebibyte of storage provisioned. File system
-- throughput capacity is equal to Storage capacity (TiB) *
-- PerUnitStorageThroughput (MB\/s\/TiB). This option is only valid for
-- @PERSISTENT_1@ and @PERSISTENT_2@ deployment types.
--
-- Valid values:
--
-- -   For @PERSISTENT_1@ SSD storage: 50, 100, 200.
--
-- -   For @PERSISTENT_1@ HDD storage: 12, 40.
--
-- -   For @PERSISTENT_2@ SSD storage: 125, 250, 500, 1000.
--
-- 'dataCompressionType', 'lustreFileSystemConfiguration_dataCompressionType' - The data compression configuration for the file system.
-- @DataCompressionType@ can have the following values:
--
-- -   @NONE@ - Data compression is turned off for the file system.
--
-- -   @LZ4@ - Data compression is turned on with the LZ4 algorithm.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/data-compression.html Lustre data compression>.
--
-- 'mountName', 'lustreFileSystemConfiguration_mountName' - You use the @MountName@ value when mounting the file system.
--
-- For the @SCRATCH_1@ deployment type, this value is always \"@fsx@\". For
-- @SCRATCH_2@, @PERSISTENT_1@, and @PERSISTENT_2@ deployment types, this
-- value is a string that is unique within an Amazon Web Services Region.
--
-- 'rootSquashConfiguration', 'lustreFileSystemConfiguration_rootSquashConfiguration' - The Lustre root squash configuration for an Amazon FSx for Lustre file
-- system. When enabled, root squash restricts root-level access from
-- clients that try to access your file system as a root user.
--
-- 'dataRepositoryConfiguration', 'lustreFileSystemConfiguration_dataRepositoryConfiguration' - Undocumented member.
newLustreFileSystemConfiguration ::
  LustreFileSystemConfiguration
newLustreFileSystemConfiguration =
  LustreFileSystemConfiguration'
    { copyTagsToBackups =
        Prelude.Nothing,
      driveCacheType = Prelude.Nothing,
      weeklyMaintenanceStartTime = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      automaticBackupRetentionDays =
        Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing,
      perUnitStorageThroughput = Prelude.Nothing,
      dataCompressionType = Prelude.Nothing,
      mountName = Prelude.Nothing,
      rootSquashConfiguration = Prelude.Nothing,
      dataRepositoryConfiguration =
        Prelude.Nothing
    }

-- | A boolean flag indicating whether tags on the file system are copied to
-- backups. If it\'s set to true, all tags on the file system are copied to
-- all automatic backups and any user-initiated backups where the user
-- doesn\'t specify any tags. If this value is true, and you specify one or
-- more tags, only the specified tags are copied to backups. If you specify
-- one or more tags when creating a user-initiated backup, no tags are
-- copied from the file system, regardless of this value. (Default = false)
lustreFileSystemConfiguration_copyTagsToBackups :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Bool)
lustreFileSystemConfiguration_copyTagsToBackups = Lens.lens (\LustreFileSystemConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@LustreFileSystemConfiguration' {} a -> s {copyTagsToBackups = a} :: LustreFileSystemConfiguration)

-- | The type of drive cache used by @PERSISTENT_1@ file systems that are
-- provisioned with HDD storage devices. This parameter is required when
-- @StorageType@ is HDD. When set to @READ@ the file system has an SSD
-- storage cache that is sized to 20% of the file system\'s storage
-- capacity. This improves the performance for frequently accessed files by
-- caching up to 20% of the total storage capacity.
--
-- This parameter is required when @StorageType@ is set to HDD.
lustreFileSystemConfiguration_driveCacheType :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe DriveCacheType)
lustreFileSystemConfiguration_driveCacheType = Lens.lens (\LustreFileSystemConfiguration' {driveCacheType} -> driveCacheType) (\s@LustreFileSystemConfiguration' {} a -> s {driveCacheType = a} :: LustreFileSystemConfiguration)

-- | The preferred start time to perform weekly maintenance, formatted
-- d:HH:MM in the UTC time zone. Here, @d@ is the weekday number, from 1
-- through 7, beginning with Monday and ending with Sunday.
lustreFileSystemConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Text)
lustreFileSystemConfiguration_weeklyMaintenanceStartTime = Lens.lens (\LustreFileSystemConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@LustreFileSystemConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: LustreFileSystemConfiguration)

-- | The Lustre logging configuration. Lustre logging writes the enabled log
-- events for your file system to Amazon CloudWatch Logs.
lustreFileSystemConfiguration_logConfiguration :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe LustreLogConfiguration)
lustreFileSystemConfiguration_logConfiguration = Lens.lens (\LustreFileSystemConfiguration' {logConfiguration} -> logConfiguration) (\s@LustreFileSystemConfiguration' {} a -> s {logConfiguration = a} :: LustreFileSystemConfiguration)

-- | Undocumented member.
lustreFileSystemConfiguration_automaticBackupRetentionDays :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
lustreFileSystemConfiguration_automaticBackupRetentionDays = Lens.lens (\LustreFileSystemConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@LustreFileSystemConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: LustreFileSystemConfiguration)

-- | The deployment type of the FSx for Lustre file system. /Scratch
-- deployment type/ is designed for temporary storage and shorter-term
-- processing of data.
--
-- @SCRATCH_1@ and @SCRATCH_2@ deployment types are best suited for when
-- you need temporary storage and shorter-term processing of data. The
-- @SCRATCH_2@ deployment type provides in-transit encryption of data and
-- higher burst throughput capacity than @SCRATCH_1@.
--
-- The @PERSISTENT_1@ and @PERSISTENT_2@ deployment type is used for
-- longer-term storage and workloads and encryption of data in transit.
-- @PERSISTENT_2@ is built on Lustre v2.12 and offers higher
-- @PerUnitStorageThroughput@ (up to 1000 MB\/s\/TiB) along with a lower
-- minimum storage capacity requirement (600 GiB). To learn more about FSx
-- for Lustre deployment types, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/lustre-deployment-types.html FSx for Lustre deployment options>.
--
-- The default is @SCRATCH_1@.
lustreFileSystemConfiguration_deploymentType :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe LustreDeploymentType)
lustreFileSystemConfiguration_deploymentType = Lens.lens (\LustreFileSystemConfiguration' {deploymentType} -> deploymentType) (\s@LustreFileSystemConfiguration' {} a -> s {deploymentType = a} :: LustreFileSystemConfiguration)

-- | Undocumented member.
lustreFileSystemConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Text)
lustreFileSystemConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\LustreFileSystemConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@LustreFileSystemConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: LustreFileSystemConfiguration)

-- | Per unit storage throughput represents the megabytes per second of read
-- or write throughput per 1 tebibyte of storage provisioned. File system
-- throughput capacity is equal to Storage capacity (TiB) *
-- PerUnitStorageThroughput (MB\/s\/TiB). This option is only valid for
-- @PERSISTENT_1@ and @PERSISTENT_2@ deployment types.
--
-- Valid values:
--
-- -   For @PERSISTENT_1@ SSD storage: 50, 100, 200.
--
-- -   For @PERSISTENT_1@ HDD storage: 12, 40.
--
-- -   For @PERSISTENT_2@ SSD storage: 125, 250, 500, 1000.
lustreFileSystemConfiguration_perUnitStorageThroughput :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
lustreFileSystemConfiguration_perUnitStorageThroughput = Lens.lens (\LustreFileSystemConfiguration' {perUnitStorageThroughput} -> perUnitStorageThroughput) (\s@LustreFileSystemConfiguration' {} a -> s {perUnitStorageThroughput = a} :: LustreFileSystemConfiguration)

-- | The data compression configuration for the file system.
-- @DataCompressionType@ can have the following values:
--
-- -   @NONE@ - Data compression is turned off for the file system.
--
-- -   @LZ4@ - Data compression is turned on with the LZ4 algorithm.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/data-compression.html Lustre data compression>.
lustreFileSystemConfiguration_dataCompressionType :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe DataCompressionType)
lustreFileSystemConfiguration_dataCompressionType = Lens.lens (\LustreFileSystemConfiguration' {dataCompressionType} -> dataCompressionType) (\s@LustreFileSystemConfiguration' {} a -> s {dataCompressionType = a} :: LustreFileSystemConfiguration)

-- | You use the @MountName@ value when mounting the file system.
--
-- For the @SCRATCH_1@ deployment type, this value is always \"@fsx@\". For
-- @SCRATCH_2@, @PERSISTENT_1@, and @PERSISTENT_2@ deployment types, this
-- value is a string that is unique within an Amazon Web Services Region.
lustreFileSystemConfiguration_mountName :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Text)
lustreFileSystemConfiguration_mountName = Lens.lens (\LustreFileSystemConfiguration' {mountName} -> mountName) (\s@LustreFileSystemConfiguration' {} a -> s {mountName = a} :: LustreFileSystemConfiguration)

-- | The Lustre root squash configuration for an Amazon FSx for Lustre file
-- system. When enabled, root squash restricts root-level access from
-- clients that try to access your file system as a root user.
lustreFileSystemConfiguration_rootSquashConfiguration :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe LustreRootSquashConfiguration)
lustreFileSystemConfiguration_rootSquashConfiguration = Lens.lens (\LustreFileSystemConfiguration' {rootSquashConfiguration} -> rootSquashConfiguration) (\s@LustreFileSystemConfiguration' {} a -> s {rootSquashConfiguration = a} :: LustreFileSystemConfiguration)

-- | Undocumented member.
lustreFileSystemConfiguration_dataRepositoryConfiguration :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe DataRepositoryConfiguration)
lustreFileSystemConfiguration_dataRepositoryConfiguration = Lens.lens (\LustreFileSystemConfiguration' {dataRepositoryConfiguration} -> dataRepositoryConfiguration) (\s@LustreFileSystemConfiguration' {} a -> s {dataRepositoryConfiguration = a} :: LustreFileSystemConfiguration)

instance Core.FromJSON LustreFileSystemConfiguration where
  parseJSON =
    Core.withObject
      "LustreFileSystemConfiguration"
      ( \x ->
          LustreFileSystemConfiguration'
            Prelude.<$> (x Core..:? "CopyTagsToBackups")
            Prelude.<*> (x Core..:? "DriveCacheType")
            Prelude.<*> (x Core..:? "WeeklyMaintenanceStartTime")
            Prelude.<*> (x Core..:? "LogConfiguration")
            Prelude.<*> (x Core..:? "AutomaticBackupRetentionDays")
            Prelude.<*> (x Core..:? "DeploymentType")
            Prelude.<*> (x Core..:? "DailyAutomaticBackupStartTime")
            Prelude.<*> (x Core..:? "PerUnitStorageThroughput")
            Prelude.<*> (x Core..:? "DataCompressionType")
            Prelude.<*> (x Core..:? "MountName")
            Prelude.<*> (x Core..:? "RootSquashConfiguration")
            Prelude.<*> (x Core..:? "DataRepositoryConfiguration")
      )

instance
  Prelude.Hashable
    LustreFileSystemConfiguration
  where
  hashWithSalt _salt LustreFileSystemConfiguration' {..} =
    _salt `Prelude.hashWithSalt` copyTagsToBackups
      `Prelude.hashWithSalt` driveCacheType
      `Prelude.hashWithSalt` weeklyMaintenanceStartTime
      `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` automaticBackupRetentionDays
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
      `Prelude.hashWithSalt` perUnitStorageThroughput
      `Prelude.hashWithSalt` dataCompressionType
      `Prelude.hashWithSalt` mountName
      `Prelude.hashWithSalt` rootSquashConfiguration
      `Prelude.hashWithSalt` dataRepositoryConfiguration

instance Prelude.NFData LustreFileSystemConfiguration where
  rnf LustreFileSystemConfiguration' {..} =
    Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf driveCacheType
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf perUnitStorageThroughput
      `Prelude.seq` Prelude.rnf dataCompressionType
      `Prelude.seq` Prelude.rnf mountName
      `Prelude.seq` Prelude.rnf rootSquashConfiguration
      `Prelude.seq` Prelude.rnf dataRepositoryConfiguration
