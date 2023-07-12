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
-- Module      : Amazonka.Snowball.Types.TGWOnDeviceServiceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.TGWOnDeviceServiceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.StorageUnit

-- | An object that represents the metadata and configuration settings for
-- the Storage Gateway service Tape Gateway type on an Amazon Web Services
-- Snow Family device.
--
-- /See:/ 'newTGWOnDeviceServiceConfiguration' smart constructor.
data TGWOnDeviceServiceConfiguration = TGWOnDeviceServiceConfiguration'
  { -- | The maximum number of virtual tapes to store on one Snow Family device.
    -- Due to physical resource limitations, this value must be set to 80 for
    -- Snowball Edge.
    storageLimit :: Prelude.Maybe Prelude.Natural,
    -- | The scale unit of the virtual tapes on the device.
    storageUnit :: Prelude.Maybe StorageUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TGWOnDeviceServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageLimit', 'tGWOnDeviceServiceConfiguration_storageLimit' - The maximum number of virtual tapes to store on one Snow Family device.
-- Due to physical resource limitations, this value must be set to 80 for
-- Snowball Edge.
--
-- 'storageUnit', 'tGWOnDeviceServiceConfiguration_storageUnit' - The scale unit of the virtual tapes on the device.
newTGWOnDeviceServiceConfiguration ::
  TGWOnDeviceServiceConfiguration
newTGWOnDeviceServiceConfiguration =
  TGWOnDeviceServiceConfiguration'
    { storageLimit =
        Prelude.Nothing,
      storageUnit = Prelude.Nothing
    }

-- | The maximum number of virtual tapes to store on one Snow Family device.
-- Due to physical resource limitations, this value must be set to 80 for
-- Snowball Edge.
tGWOnDeviceServiceConfiguration_storageLimit :: Lens.Lens' TGWOnDeviceServiceConfiguration (Prelude.Maybe Prelude.Natural)
tGWOnDeviceServiceConfiguration_storageLimit = Lens.lens (\TGWOnDeviceServiceConfiguration' {storageLimit} -> storageLimit) (\s@TGWOnDeviceServiceConfiguration' {} a -> s {storageLimit = a} :: TGWOnDeviceServiceConfiguration)

-- | The scale unit of the virtual tapes on the device.
tGWOnDeviceServiceConfiguration_storageUnit :: Lens.Lens' TGWOnDeviceServiceConfiguration (Prelude.Maybe StorageUnit)
tGWOnDeviceServiceConfiguration_storageUnit = Lens.lens (\TGWOnDeviceServiceConfiguration' {storageUnit} -> storageUnit) (\s@TGWOnDeviceServiceConfiguration' {} a -> s {storageUnit = a} :: TGWOnDeviceServiceConfiguration)

instance
  Data.FromJSON
    TGWOnDeviceServiceConfiguration
  where
  parseJSON =
    Data.withObject
      "TGWOnDeviceServiceConfiguration"
      ( \x ->
          TGWOnDeviceServiceConfiguration'
            Prelude.<$> (x Data..:? "StorageLimit")
            Prelude.<*> (x Data..:? "StorageUnit")
      )

instance
  Prelude.Hashable
    TGWOnDeviceServiceConfiguration
  where
  hashWithSalt
    _salt
    TGWOnDeviceServiceConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` storageLimit
        `Prelude.hashWithSalt` storageUnit

instance
  Prelude.NFData
    TGWOnDeviceServiceConfiguration
  where
  rnf TGWOnDeviceServiceConfiguration' {..} =
    Prelude.rnf storageLimit
      `Prelude.seq` Prelude.rnf storageUnit

instance Data.ToJSON TGWOnDeviceServiceConfiguration where
  toJSON TGWOnDeviceServiceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StorageLimit" Data..=) Prelude.<$> storageLimit,
            ("StorageUnit" Data..=) Prelude.<$> storageUnit
          ]
      )
