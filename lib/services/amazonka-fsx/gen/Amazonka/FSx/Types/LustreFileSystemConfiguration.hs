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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.LustreFileSystemConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | A boolean flag indicating whether tags on the file system are copied to
    -- backups. If it\'s set to true, all tags on the file system are copied to
    -- all automatic backups and any user-initiated backups where the user
    -- doesn\'t specify any tags. If this value is true, and you specify one or
    -- more tags, only the specified tags are copied to backups. If you specify
    -- one or more tags when creating a user-initiated backup, no tags are
    -- copied from the file system, regardless of this value. (Default = false)
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
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
    dataRepositoryConfiguration :: Prelude.Maybe DataRepositoryConfiguration,
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
    -- | The type of drive cache used by @PERSISTENT_1@ file systems that are
    -- provisioned with HDD storage devices. This parameter is required when
    -- @StorageType@ is HDD. When set to @READ@ the file system has an SSD
    -- storage cache that is sized to 20% of the file system\'s storage
    -- capacity. This improves the performance for frequently accessed files by
    -- caching up to 20% of the total storage capacity.
    --
    -- This parameter is required when @StorageType@ is set to HDD.
    driveCacheType :: Prelude.Maybe DriveCacheType,
    -- | The Lustre logging configuration. Lustre logging writes the enabled log
    -- events for your file system to Amazon CloudWatch Logs.
    logConfiguration :: Prelude.Maybe LustreLogConfiguration,
    -- | You use the @MountName@ value when mounting the file system.
    --
    -- For the @SCRATCH_1@ deployment type, this value is always \"@fsx@\". For
    -- @SCRATCH_2@, @PERSISTENT_1@, and @PERSISTENT_2@ deployment types, this
    -- value is a string that is unique within an Amazon Web Services Region.
    mountName :: Prelude.Maybe Prelude.Text,
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
    -- | The Lustre root squash configuration for an Amazon FSx for Lustre file
    -- system. When enabled, root squash restricts root-level access from
    -- clients that try to access your file system as a root user.
    rootSquashConfiguration :: Prelude.Maybe LustreRootSquashConfiguration,
    -- | The preferred start time to perform weekly maintenance, formatted
    -- d:HH:MM in the UTC time zone. Here, @d@ is the weekday number, from 1
    -- through 7, beginning with Monday and ending with Sunday.
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text
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
-- 'automaticBackupRetentionDays', 'lustreFileSystemConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'copyTagsToBackups', 'lustreFileSystemConfiguration_copyTagsToBackups' - A boolean flag indicating whether tags on the file system are copied to
-- backups. If it\'s set to true, all tags on the file system are copied to
-- all automatic backups and any user-initiated backups where the user
-- doesn\'t specify any tags. If this value is true, and you specify one or
-- more tags, only the specified tags are copied to backups. If you specify
-- one or more tags when creating a user-initiated backup, no tags are
-- copied from the file system, regardless of this value. (Default = false)
--
-- 'dailyAutomaticBackupStartTime', 'lustreFileSystemConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
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
-- 'dataRepositoryConfiguration', 'lustreFileSystemConfiguration_dataRepositoryConfiguration' - Undocumented member.
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
-- 'driveCacheType', 'lustreFileSystemConfiguration_driveCacheType' - The type of drive cache used by @PERSISTENT_1@ file systems that are
-- provisioned with HDD storage devices. This parameter is required when
-- @StorageType@ is HDD. When set to @READ@ the file system has an SSD
-- storage cache that is sized to 20% of the file system\'s storage
-- capacity. This improves the performance for frequently accessed files by
-- caching up to 20% of the total storage capacity.
--
-- This parameter is required when @StorageType@ is set to HDD.
--
-- 'logConfiguration', 'lustreFileSystemConfiguration_logConfiguration' - The Lustre logging configuration. Lustre logging writes the enabled log
-- events for your file system to Amazon CloudWatch Logs.
--
-- 'mountName', 'lustreFileSystemConfiguration_mountName' - You use the @MountName@ value when mounting the file system.
--
-- For the @SCRATCH_1@ deployment type, this value is always \"@fsx@\". For
-- @SCRATCH_2@, @PERSISTENT_1@, and @PERSISTENT_2@ deployment types, this
-- value is a string that is unique within an Amazon Web Services Region.
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
-- 'rootSquashConfiguration', 'lustreFileSystemConfiguration_rootSquashConfiguration' - The Lustre root squash configuration for an Amazon FSx for Lustre file
-- system. When enabled, root squash restricts root-level access from
-- clients that try to access your file system as a root user.
--
-- 'weeklyMaintenanceStartTime', 'lustreFileSystemConfiguration_weeklyMaintenanceStartTime' - The preferred start time to perform weekly maintenance, formatted
-- d:HH:MM in the UTC time zone. Here, @d@ is the weekday number, from 1
-- through 7, beginning with Monday and ending with Sunday.
newLustreFileSystemConfiguration ::
  LustreFileSystemConfiguration
newLustreFileSystemConfiguration =
  LustreFileSystemConfiguration'
    { automaticBackupRetentionDays =
        Prelude.Nothing,
      copyTagsToBackups = Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing,
      dataCompressionType = Prelude.Nothing,
      dataRepositoryConfiguration =
        Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      driveCacheType = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      mountName = Prelude.Nothing,
      perUnitStorageThroughput = Prelude.Nothing,
      rootSquashConfiguration = Prelude.Nothing,
      weeklyMaintenanceStartTime = Prelude.Nothing
    }

-- | Undocumented member.
lustreFileSystemConfiguration_automaticBackupRetentionDays :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
lustreFileSystemConfiguration_automaticBackupRetentionDays = Lens.lens (\LustreFileSystemConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@LustreFileSystemConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: LustreFileSystemConfiguration)

-- | A boolean flag indicating whether tags on the file system are copied to
-- backups. If it\'s set to true, all tags on the file system are copied to
-- all automatic backups and any user-initiated backups where the user
-- doesn\'t specify any tags. If this value is true, and you specify one or
-- more tags, only the specified tags are copied to backups. If you specify
-- one or more tags when creating a user-initiated backup, no tags are
-- copied from the file system, regardless of this value. (Default = false)
lustreFileSystemConfiguration_copyTagsToBackups :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Bool)
lustreFileSystemConfiguration_copyTagsToBackups = Lens.lens (\LustreFileSystemConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@LustreFileSystemConfiguration' {} a -> s {copyTagsToBackups = a} :: LustreFileSystemConfiguration)

-- | Undocumented member.
lustreFileSystemConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Text)
lustreFileSystemConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\LustreFileSystemConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@LustreFileSystemConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: LustreFileSystemConfiguration)

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

-- | Undocumented member.
lustreFileSystemConfiguration_dataRepositoryConfiguration :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe DataRepositoryConfiguration)
lustreFileSystemConfiguration_dataRepositoryConfiguration = Lens.lens (\LustreFileSystemConfiguration' {dataRepositoryConfiguration} -> dataRepositoryConfiguration) (\s@LustreFileSystemConfiguration' {} a -> s {dataRepositoryConfiguration = a} :: LustreFileSystemConfiguration)

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

-- | The Lustre logging configuration. Lustre logging writes the enabled log
-- events for your file system to Amazon CloudWatch Logs.
lustreFileSystemConfiguration_logConfiguration :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe LustreLogConfiguration)
lustreFileSystemConfiguration_logConfiguration = Lens.lens (\LustreFileSystemConfiguration' {logConfiguration} -> logConfiguration) (\s@LustreFileSystemConfiguration' {} a -> s {logConfiguration = a} :: LustreFileSystemConfiguration)

-- | You use the @MountName@ value when mounting the file system.
--
-- For the @SCRATCH_1@ deployment type, this value is always \"@fsx@\". For
-- @SCRATCH_2@, @PERSISTENT_1@, and @PERSISTENT_2@ deployment types, this
-- value is a string that is unique within an Amazon Web Services Region.
lustreFileSystemConfiguration_mountName :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Text)
lustreFileSystemConfiguration_mountName = Lens.lens (\LustreFileSystemConfiguration' {mountName} -> mountName) (\s@LustreFileSystemConfiguration' {} a -> s {mountName = a} :: LustreFileSystemConfiguration)

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

-- | The Lustre root squash configuration for an Amazon FSx for Lustre file
-- system. When enabled, root squash restricts root-level access from
-- clients that try to access your file system as a root user.
lustreFileSystemConfiguration_rootSquashConfiguration :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe LustreRootSquashConfiguration)
lustreFileSystemConfiguration_rootSquashConfiguration = Lens.lens (\LustreFileSystemConfiguration' {rootSquashConfiguration} -> rootSquashConfiguration) (\s@LustreFileSystemConfiguration' {} a -> s {rootSquashConfiguration = a} :: LustreFileSystemConfiguration)

-- | The preferred start time to perform weekly maintenance, formatted
-- d:HH:MM in the UTC time zone. Here, @d@ is the weekday number, from 1
-- through 7, beginning with Monday and ending with Sunday.
lustreFileSystemConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' LustreFileSystemConfiguration (Prelude.Maybe Prelude.Text)
lustreFileSystemConfiguration_weeklyMaintenanceStartTime = Lens.lens (\LustreFileSystemConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@LustreFileSystemConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: LustreFileSystemConfiguration)

instance Data.FromJSON LustreFileSystemConfiguration where
  parseJSON =
    Data.withObject
      "LustreFileSystemConfiguration"
      ( \x ->
          LustreFileSystemConfiguration'
            Prelude.<$> (x Data..:? "AutomaticBackupRetentionDays")
            Prelude.<*> (x Data..:? "CopyTagsToBackups")
            Prelude.<*> (x Data..:? "DailyAutomaticBackupStartTime")
            Prelude.<*> (x Data..:? "DataCompressionType")
            Prelude.<*> (x Data..:? "DataRepositoryConfiguration")
            Prelude.<*> (x Data..:? "DeploymentType")
            Prelude.<*> (x Data..:? "DriveCacheType")
            Prelude.<*> (x Data..:? "LogConfiguration")
            Prelude.<*> (x Data..:? "MountName")
            Prelude.<*> (x Data..:? "PerUnitStorageThroughput")
            Prelude.<*> (x Data..:? "RootSquashConfiguration")
            Prelude.<*> (x Data..:? "WeeklyMaintenanceStartTime")
      )

instance
  Prelude.Hashable
    LustreFileSystemConfiguration
  where
  hashWithSalt _salt LustreFileSystemConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` automaticBackupRetentionDays
      `Prelude.hashWithSalt` copyTagsToBackups
      `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
      `Prelude.hashWithSalt` dataCompressionType
      `Prelude.hashWithSalt` dataRepositoryConfiguration
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` driveCacheType
      `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` mountName
      `Prelude.hashWithSalt` perUnitStorageThroughput
      `Prelude.hashWithSalt` rootSquashConfiguration
      `Prelude.hashWithSalt` weeklyMaintenanceStartTime

instance Prelude.NFData LustreFileSystemConfiguration where
  rnf LustreFileSystemConfiguration' {..} =
    Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf dataCompressionType
      `Prelude.seq` Prelude.rnf dataRepositoryConfiguration
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf driveCacheType
      `Prelude.seq` Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf mountName
      `Prelude.seq` Prelude.rnf perUnitStorageThroughput
      `Prelude.seq` Prelude.rnf rootSquashConfiguration
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
