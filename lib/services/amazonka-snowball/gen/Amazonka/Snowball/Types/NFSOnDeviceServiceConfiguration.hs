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
-- Module      : Amazonka.Snowball.Types.NFSOnDeviceServiceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.NFSOnDeviceServiceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.StorageUnit

-- | An object that represents the metadata and configuration settings for
-- the NFS (Network File System) service on an Amazon Web Services Snow
-- Family device.
--
-- /See:/ 'newNFSOnDeviceServiceConfiguration' smart constructor.
data NFSOnDeviceServiceConfiguration = NFSOnDeviceServiceConfiguration'
  { -- | The maximum NFS storage for one Snow Family device.
    storageLimit :: Prelude.Maybe Prelude.Natural,
    -- | The scale unit of the NFS storage on the device.
    --
    -- Valid values: TB.
    storageUnit :: Prelude.Maybe StorageUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NFSOnDeviceServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageLimit', 'nFSOnDeviceServiceConfiguration_storageLimit' - The maximum NFS storage for one Snow Family device.
--
-- 'storageUnit', 'nFSOnDeviceServiceConfiguration_storageUnit' - The scale unit of the NFS storage on the device.
--
-- Valid values: TB.
newNFSOnDeviceServiceConfiguration ::
  NFSOnDeviceServiceConfiguration
newNFSOnDeviceServiceConfiguration =
  NFSOnDeviceServiceConfiguration'
    { storageLimit =
        Prelude.Nothing,
      storageUnit = Prelude.Nothing
    }

-- | The maximum NFS storage for one Snow Family device.
nFSOnDeviceServiceConfiguration_storageLimit :: Lens.Lens' NFSOnDeviceServiceConfiguration (Prelude.Maybe Prelude.Natural)
nFSOnDeviceServiceConfiguration_storageLimit = Lens.lens (\NFSOnDeviceServiceConfiguration' {storageLimit} -> storageLimit) (\s@NFSOnDeviceServiceConfiguration' {} a -> s {storageLimit = a} :: NFSOnDeviceServiceConfiguration)

-- | The scale unit of the NFS storage on the device.
--
-- Valid values: TB.
nFSOnDeviceServiceConfiguration_storageUnit :: Lens.Lens' NFSOnDeviceServiceConfiguration (Prelude.Maybe StorageUnit)
nFSOnDeviceServiceConfiguration_storageUnit = Lens.lens (\NFSOnDeviceServiceConfiguration' {storageUnit} -> storageUnit) (\s@NFSOnDeviceServiceConfiguration' {} a -> s {storageUnit = a} :: NFSOnDeviceServiceConfiguration)

instance
  Data.FromJSON
    NFSOnDeviceServiceConfiguration
  where
  parseJSON =
    Data.withObject
      "NFSOnDeviceServiceConfiguration"
      ( \x ->
          NFSOnDeviceServiceConfiguration'
            Prelude.<$> (x Data..:? "StorageLimit")
            Prelude.<*> (x Data..:? "StorageUnit")
      )

instance
  Prelude.Hashable
    NFSOnDeviceServiceConfiguration
  where
  hashWithSalt
    _salt
    NFSOnDeviceServiceConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` storageLimit
        `Prelude.hashWithSalt` storageUnit

instance
  Prelude.NFData
    NFSOnDeviceServiceConfiguration
  where
  rnf NFSOnDeviceServiceConfiguration' {..} =
    Prelude.rnf storageLimit `Prelude.seq`
      Prelude.rnf storageUnit

instance Data.ToJSON NFSOnDeviceServiceConfiguration where
  toJSON NFSOnDeviceServiceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StorageLimit" Data..=) Prelude.<$> storageLimit,
            ("StorageUnit" Data..=) Prelude.<$> storageUnit
          ]
      )
