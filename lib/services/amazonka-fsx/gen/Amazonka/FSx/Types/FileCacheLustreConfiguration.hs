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
-- Module      : Amazonka.FSx.Types.FileCacheLustreConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileCacheLustreConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.FileCacheLustreDeploymentType
import Amazonka.FSx.Types.FileCacheLustreMetadataConfiguration
import Amazonka.FSx.Types.LustreLogConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the Amazon File Cache resource.
--
-- /See:/ 'newFileCacheLustreConfiguration' smart constructor.
data FileCacheLustreConfiguration = FileCacheLustreConfiguration'
  { -- | The configuration for a Lustre MDT (Metadata Target) storage volume.
    metadataConfiguration :: Prelude.Maybe FileCacheLustreMetadataConfiguration,
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    -- | The configuration for Lustre logging used to write the enabled logging
    -- events for your Amazon File Cache resource to Amazon CloudWatch Logs.
    logConfiguration :: Prelude.Maybe LustreLogConfiguration,
    -- | The deployment type of the Amazon File Cache resource, which must be
    -- @CACHE_1@.
    deploymentType :: Prelude.Maybe FileCacheLustreDeploymentType,
    -- | Per unit storage throughput represents the megabytes per second of read
    -- or write throughput per 1 tebibyte of storage provisioned. Cache
    -- throughput capacity is equal to Storage capacity (TiB) *
    -- PerUnitStorageThroughput (MB\/s\/TiB). The only supported value is
    -- @1000@.
    perUnitStorageThroughput :: Prelude.Maybe Prelude.Natural,
    -- | You use the @MountName@ value when mounting the cache. If you pass a
    -- cache ID to the @DescribeFileCaches@ operation, it returns the the
    -- @MountName@ value as part of the cache\'s description.
    mountName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileCacheLustreConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataConfiguration', 'fileCacheLustreConfiguration_metadataConfiguration' - The configuration for a Lustre MDT (Metadata Target) storage volume.
--
-- 'weeklyMaintenanceStartTime', 'fileCacheLustreConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
--
-- 'logConfiguration', 'fileCacheLustreConfiguration_logConfiguration' - The configuration for Lustre logging used to write the enabled logging
-- events for your Amazon File Cache resource to Amazon CloudWatch Logs.
--
-- 'deploymentType', 'fileCacheLustreConfiguration_deploymentType' - The deployment type of the Amazon File Cache resource, which must be
-- @CACHE_1@.
--
-- 'perUnitStorageThroughput', 'fileCacheLustreConfiguration_perUnitStorageThroughput' - Per unit storage throughput represents the megabytes per second of read
-- or write throughput per 1 tebibyte of storage provisioned. Cache
-- throughput capacity is equal to Storage capacity (TiB) *
-- PerUnitStorageThroughput (MB\/s\/TiB). The only supported value is
-- @1000@.
--
-- 'mountName', 'fileCacheLustreConfiguration_mountName' - You use the @MountName@ value when mounting the cache. If you pass a
-- cache ID to the @DescribeFileCaches@ operation, it returns the the
-- @MountName@ value as part of the cache\'s description.
newFileCacheLustreConfiguration ::
  FileCacheLustreConfiguration
newFileCacheLustreConfiguration =
  FileCacheLustreConfiguration'
    { metadataConfiguration =
        Prelude.Nothing,
      weeklyMaintenanceStartTime = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      perUnitStorageThroughput = Prelude.Nothing,
      mountName = Prelude.Nothing
    }

-- | The configuration for a Lustre MDT (Metadata Target) storage volume.
fileCacheLustreConfiguration_metadataConfiguration :: Lens.Lens' FileCacheLustreConfiguration (Prelude.Maybe FileCacheLustreMetadataConfiguration)
fileCacheLustreConfiguration_metadataConfiguration = Lens.lens (\FileCacheLustreConfiguration' {metadataConfiguration} -> metadataConfiguration) (\s@FileCacheLustreConfiguration' {} a -> s {metadataConfiguration = a} :: FileCacheLustreConfiguration)

-- | Undocumented member.
fileCacheLustreConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' FileCacheLustreConfiguration (Prelude.Maybe Prelude.Text)
fileCacheLustreConfiguration_weeklyMaintenanceStartTime = Lens.lens (\FileCacheLustreConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@FileCacheLustreConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: FileCacheLustreConfiguration)

-- | The configuration for Lustre logging used to write the enabled logging
-- events for your Amazon File Cache resource to Amazon CloudWatch Logs.
fileCacheLustreConfiguration_logConfiguration :: Lens.Lens' FileCacheLustreConfiguration (Prelude.Maybe LustreLogConfiguration)
fileCacheLustreConfiguration_logConfiguration = Lens.lens (\FileCacheLustreConfiguration' {logConfiguration} -> logConfiguration) (\s@FileCacheLustreConfiguration' {} a -> s {logConfiguration = a} :: FileCacheLustreConfiguration)

-- | The deployment type of the Amazon File Cache resource, which must be
-- @CACHE_1@.
fileCacheLustreConfiguration_deploymentType :: Lens.Lens' FileCacheLustreConfiguration (Prelude.Maybe FileCacheLustreDeploymentType)
fileCacheLustreConfiguration_deploymentType = Lens.lens (\FileCacheLustreConfiguration' {deploymentType} -> deploymentType) (\s@FileCacheLustreConfiguration' {} a -> s {deploymentType = a} :: FileCacheLustreConfiguration)

-- | Per unit storage throughput represents the megabytes per second of read
-- or write throughput per 1 tebibyte of storage provisioned. Cache
-- throughput capacity is equal to Storage capacity (TiB) *
-- PerUnitStorageThroughput (MB\/s\/TiB). The only supported value is
-- @1000@.
fileCacheLustreConfiguration_perUnitStorageThroughput :: Lens.Lens' FileCacheLustreConfiguration (Prelude.Maybe Prelude.Natural)
fileCacheLustreConfiguration_perUnitStorageThroughput = Lens.lens (\FileCacheLustreConfiguration' {perUnitStorageThroughput} -> perUnitStorageThroughput) (\s@FileCacheLustreConfiguration' {} a -> s {perUnitStorageThroughput = a} :: FileCacheLustreConfiguration)

-- | You use the @MountName@ value when mounting the cache. If you pass a
-- cache ID to the @DescribeFileCaches@ operation, it returns the the
-- @MountName@ value as part of the cache\'s description.
fileCacheLustreConfiguration_mountName :: Lens.Lens' FileCacheLustreConfiguration (Prelude.Maybe Prelude.Text)
fileCacheLustreConfiguration_mountName = Lens.lens (\FileCacheLustreConfiguration' {mountName} -> mountName) (\s@FileCacheLustreConfiguration' {} a -> s {mountName = a} :: FileCacheLustreConfiguration)

instance Core.FromJSON FileCacheLustreConfiguration where
  parseJSON =
    Core.withObject
      "FileCacheLustreConfiguration"
      ( \x ->
          FileCacheLustreConfiguration'
            Prelude.<$> (x Core..:? "MetadataConfiguration")
            Prelude.<*> (x Core..:? "WeeklyMaintenanceStartTime")
            Prelude.<*> (x Core..:? "LogConfiguration")
            Prelude.<*> (x Core..:? "DeploymentType")
            Prelude.<*> (x Core..:? "PerUnitStorageThroughput")
            Prelude.<*> (x Core..:? "MountName")
      )

instance
  Prelude.Hashable
    FileCacheLustreConfiguration
  where
  hashWithSalt _salt FileCacheLustreConfiguration' {..} =
    _salt `Prelude.hashWithSalt` metadataConfiguration
      `Prelude.hashWithSalt` weeklyMaintenanceStartTime
      `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` perUnitStorageThroughput
      `Prelude.hashWithSalt` mountName

instance Prelude.NFData FileCacheLustreConfiguration where
  rnf FileCacheLustreConfiguration' {..} =
    Prelude.rnf metadataConfiguration
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf perUnitStorageThroughput
      `Prelude.seq` Prelude.rnf mountName
