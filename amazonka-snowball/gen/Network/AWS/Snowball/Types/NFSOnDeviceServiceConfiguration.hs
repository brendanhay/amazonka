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
-- Module      : Network.AWS.Snowball.Types.NFSOnDeviceServiceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.NFSOnDeviceServiceConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Snowball.Types.StorageUnit

-- | An object that represents metadata and configuration settings for NFS
-- service on an AWS Snow Family device.
--
-- /See:/ 'newNFSOnDeviceServiceConfiguration' smart constructor.
data NFSOnDeviceServiceConfiguration = NFSOnDeviceServiceConfiguration'
  { -- | The maximum NFS storage for one Snowball Family device.
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
-- 'storageLimit', 'nFSOnDeviceServiceConfiguration_storageLimit' - The maximum NFS storage for one Snowball Family device.
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

-- | The maximum NFS storage for one Snowball Family device.
nFSOnDeviceServiceConfiguration_storageLimit :: Lens.Lens' NFSOnDeviceServiceConfiguration (Prelude.Maybe Prelude.Natural)
nFSOnDeviceServiceConfiguration_storageLimit = Lens.lens (\NFSOnDeviceServiceConfiguration' {storageLimit} -> storageLimit) (\s@NFSOnDeviceServiceConfiguration' {} a -> s {storageLimit = a} :: NFSOnDeviceServiceConfiguration)

-- | The scale unit of the NFS storage on the device.
--
-- Valid values: TB.
nFSOnDeviceServiceConfiguration_storageUnit :: Lens.Lens' NFSOnDeviceServiceConfiguration (Prelude.Maybe StorageUnit)
nFSOnDeviceServiceConfiguration_storageUnit = Lens.lens (\NFSOnDeviceServiceConfiguration' {storageUnit} -> storageUnit) (\s@NFSOnDeviceServiceConfiguration' {} a -> s {storageUnit = a} :: NFSOnDeviceServiceConfiguration)

instance
  Core.FromJSON
    NFSOnDeviceServiceConfiguration
  where
  parseJSON =
    Core.withObject
      "NFSOnDeviceServiceConfiguration"
      ( \x ->
          NFSOnDeviceServiceConfiguration'
            Prelude.<$> (x Core..:? "StorageLimit")
            Prelude.<*> (x Core..:? "StorageUnit")
      )

instance
  Prelude.Hashable
    NFSOnDeviceServiceConfiguration

instance
  Prelude.NFData
    NFSOnDeviceServiceConfiguration

instance Core.ToJSON NFSOnDeviceServiceConfiguration where
  toJSON NFSOnDeviceServiceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StorageLimit" Core..=) Prelude.<$> storageLimit,
            ("StorageUnit" Core..=) Prelude.<$> storageUnit
          ]
      )
