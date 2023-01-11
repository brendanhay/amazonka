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
-- Module      : Amazonka.FSx.Types.CreateFileCacheLustreConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateFileCacheLustreConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.FileCacheLustreDeploymentType
import Amazonka.FSx.Types.FileCacheLustreMetadataConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The Amazon File Cache configuration for the cache that you are creating.
--
-- /See:/ 'newCreateFileCacheLustreConfiguration' smart constructor.
data CreateFileCacheLustreConfiguration = CreateFileCacheLustreConfiguration'
  { weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    -- | Provisions the amount of read and write throughput for each 1 tebibyte
    -- (TiB) of cache storage capacity, in MB\/s\/TiB. The only supported value
    -- is @1000@.
    perUnitStorageThroughput :: Prelude.Natural,
    -- | Specifies the cache deployment type, which must be @CACHE_1@.
    deploymentType :: FileCacheLustreDeploymentType,
    -- | The configuration for a Lustre MDT (Metadata Target) storage volume.
    metadataConfiguration :: FileCacheLustreMetadataConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileCacheLustreConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weeklyMaintenanceStartTime', 'createFileCacheLustreConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
--
-- 'perUnitStorageThroughput', 'createFileCacheLustreConfiguration_perUnitStorageThroughput' - Provisions the amount of read and write throughput for each 1 tebibyte
-- (TiB) of cache storage capacity, in MB\/s\/TiB. The only supported value
-- is @1000@.
--
-- 'deploymentType', 'createFileCacheLustreConfiguration_deploymentType' - Specifies the cache deployment type, which must be @CACHE_1@.
--
-- 'metadataConfiguration', 'createFileCacheLustreConfiguration_metadataConfiguration' - The configuration for a Lustre MDT (Metadata Target) storage volume.
newCreateFileCacheLustreConfiguration ::
  -- | 'perUnitStorageThroughput'
  Prelude.Natural ->
  -- | 'deploymentType'
  FileCacheLustreDeploymentType ->
  -- | 'metadataConfiguration'
  FileCacheLustreMetadataConfiguration ->
  CreateFileCacheLustreConfiguration
newCreateFileCacheLustreConfiguration
  pPerUnitStorageThroughput_
  pDeploymentType_
  pMetadataConfiguration_ =
    CreateFileCacheLustreConfiguration'
      { weeklyMaintenanceStartTime =
          Prelude.Nothing,
        perUnitStorageThroughput =
          pPerUnitStorageThroughput_,
        deploymentType = pDeploymentType_,
        metadataConfiguration =
          pMetadataConfiguration_
      }

-- | Undocumented member.
createFileCacheLustreConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' CreateFileCacheLustreConfiguration (Prelude.Maybe Prelude.Text)
createFileCacheLustreConfiguration_weeklyMaintenanceStartTime = Lens.lens (\CreateFileCacheLustreConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@CreateFileCacheLustreConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: CreateFileCacheLustreConfiguration)

-- | Provisions the amount of read and write throughput for each 1 tebibyte
-- (TiB) of cache storage capacity, in MB\/s\/TiB. The only supported value
-- is @1000@.
createFileCacheLustreConfiguration_perUnitStorageThroughput :: Lens.Lens' CreateFileCacheLustreConfiguration Prelude.Natural
createFileCacheLustreConfiguration_perUnitStorageThroughput = Lens.lens (\CreateFileCacheLustreConfiguration' {perUnitStorageThroughput} -> perUnitStorageThroughput) (\s@CreateFileCacheLustreConfiguration' {} a -> s {perUnitStorageThroughput = a} :: CreateFileCacheLustreConfiguration)

-- | Specifies the cache deployment type, which must be @CACHE_1@.
createFileCacheLustreConfiguration_deploymentType :: Lens.Lens' CreateFileCacheLustreConfiguration FileCacheLustreDeploymentType
createFileCacheLustreConfiguration_deploymentType = Lens.lens (\CreateFileCacheLustreConfiguration' {deploymentType} -> deploymentType) (\s@CreateFileCacheLustreConfiguration' {} a -> s {deploymentType = a} :: CreateFileCacheLustreConfiguration)

-- | The configuration for a Lustre MDT (Metadata Target) storage volume.
createFileCacheLustreConfiguration_metadataConfiguration :: Lens.Lens' CreateFileCacheLustreConfiguration FileCacheLustreMetadataConfiguration
createFileCacheLustreConfiguration_metadataConfiguration = Lens.lens (\CreateFileCacheLustreConfiguration' {metadataConfiguration} -> metadataConfiguration) (\s@CreateFileCacheLustreConfiguration' {} a -> s {metadataConfiguration = a} :: CreateFileCacheLustreConfiguration)

instance
  Prelude.Hashable
    CreateFileCacheLustreConfiguration
  where
  hashWithSalt
    _salt
    CreateFileCacheLustreConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime
        `Prelude.hashWithSalt` perUnitStorageThroughput
        `Prelude.hashWithSalt` deploymentType
        `Prelude.hashWithSalt` metadataConfiguration

instance
  Prelude.NFData
    CreateFileCacheLustreConfiguration
  where
  rnf CreateFileCacheLustreConfiguration' {..} =
    Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf perUnitStorageThroughput
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf metadataConfiguration

instance
  Data.ToJSON
    CreateFileCacheLustreConfiguration
  where
  toJSON CreateFileCacheLustreConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WeeklyMaintenanceStartTime" Data..=)
              Prelude.<$> weeklyMaintenanceStartTime,
            Prelude.Just
              ( "PerUnitStorageThroughput"
                  Data..= perUnitStorageThroughput
              ),
            Prelude.Just
              ("DeploymentType" Data..= deploymentType),
            Prelude.Just
              ( "MetadataConfiguration"
                  Data..= metadataConfiguration
              )
          ]
      )
