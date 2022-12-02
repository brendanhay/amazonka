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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The scale unit of the NFS storage on the device.
    --
    -- Valid values: TB.
    storageUnit :: Prelude.Maybe StorageUnit,
    -- | The maximum NFS storage for one Snow Family device.
    storageLimit :: Prelude.Maybe Prelude.Natural
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
-- 'storageUnit', 'nFSOnDeviceServiceConfiguration_storageUnit' - The scale unit of the NFS storage on the device.
--
-- Valid values: TB.
--
-- 'storageLimit', 'nFSOnDeviceServiceConfiguration_storageLimit' - The maximum NFS storage for one Snow Family device.
newNFSOnDeviceServiceConfiguration ::
  NFSOnDeviceServiceConfiguration
newNFSOnDeviceServiceConfiguration =
  NFSOnDeviceServiceConfiguration'
    { storageUnit =
        Prelude.Nothing,
      storageLimit = Prelude.Nothing
    }

-- | The scale unit of the NFS storage on the device.
--
-- Valid values: TB.
nFSOnDeviceServiceConfiguration_storageUnit :: Lens.Lens' NFSOnDeviceServiceConfiguration (Prelude.Maybe StorageUnit)
nFSOnDeviceServiceConfiguration_storageUnit = Lens.lens (\NFSOnDeviceServiceConfiguration' {storageUnit} -> storageUnit) (\s@NFSOnDeviceServiceConfiguration' {} a -> s {storageUnit = a} :: NFSOnDeviceServiceConfiguration)

-- | The maximum NFS storage for one Snow Family device.
nFSOnDeviceServiceConfiguration_storageLimit :: Lens.Lens' NFSOnDeviceServiceConfiguration (Prelude.Maybe Prelude.Natural)
nFSOnDeviceServiceConfiguration_storageLimit = Lens.lens (\NFSOnDeviceServiceConfiguration' {storageLimit} -> storageLimit) (\s@NFSOnDeviceServiceConfiguration' {} a -> s {storageLimit = a} :: NFSOnDeviceServiceConfiguration)

instance
  Data.FromJSON
    NFSOnDeviceServiceConfiguration
  where
  parseJSON =
    Data.withObject
      "NFSOnDeviceServiceConfiguration"
      ( \x ->
          NFSOnDeviceServiceConfiguration'
            Prelude.<$> (x Data..:? "StorageUnit")
            Prelude.<*> (x Data..:? "StorageLimit")
      )

instance
  Prelude.Hashable
    NFSOnDeviceServiceConfiguration
  where
  hashWithSalt
    _salt
    NFSOnDeviceServiceConfiguration' {..} =
      _salt `Prelude.hashWithSalt` storageUnit
        `Prelude.hashWithSalt` storageLimit

instance
  Prelude.NFData
    NFSOnDeviceServiceConfiguration
  where
  rnf NFSOnDeviceServiceConfiguration' {..} =
    Prelude.rnf storageUnit
      `Prelude.seq` Prelude.rnf storageLimit

instance Data.ToJSON NFSOnDeviceServiceConfiguration where
  toJSON NFSOnDeviceServiceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StorageUnit" Data..=) Prelude.<$> storageUnit,
            ("StorageLimit" Data..=) Prelude.<$> storageLimit
          ]
      )
