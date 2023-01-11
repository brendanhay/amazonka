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
-- Module      : Amazonka.TimeStreamWrite.Types.MagneticStoreWriteProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.MagneticStoreWriteProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.MagneticStoreRejectedDataLocation

-- | The set of properties on a table for configuring magnetic store writes.
--
-- /See:/ 'newMagneticStoreWriteProperties' smart constructor.
data MagneticStoreWriteProperties = MagneticStoreWriteProperties'
  { -- | The location to write error reports for records rejected asynchronously
    -- during magnetic store writes.
    magneticStoreRejectedDataLocation :: Prelude.Maybe MagneticStoreRejectedDataLocation,
    -- | A flag to enable magnetic store writes.
    enableMagneticStoreWrites :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MagneticStoreWriteProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'magneticStoreRejectedDataLocation', 'magneticStoreWriteProperties_magneticStoreRejectedDataLocation' - The location to write error reports for records rejected asynchronously
-- during magnetic store writes.
--
-- 'enableMagneticStoreWrites', 'magneticStoreWriteProperties_enableMagneticStoreWrites' - A flag to enable magnetic store writes.
newMagneticStoreWriteProperties ::
  -- | 'enableMagneticStoreWrites'
  Prelude.Bool ->
  MagneticStoreWriteProperties
newMagneticStoreWriteProperties
  pEnableMagneticStoreWrites_ =
    MagneticStoreWriteProperties'
      { magneticStoreRejectedDataLocation =
          Prelude.Nothing,
        enableMagneticStoreWrites =
          pEnableMagneticStoreWrites_
      }

-- | The location to write error reports for records rejected asynchronously
-- during magnetic store writes.
magneticStoreWriteProperties_magneticStoreRejectedDataLocation :: Lens.Lens' MagneticStoreWriteProperties (Prelude.Maybe MagneticStoreRejectedDataLocation)
magneticStoreWriteProperties_magneticStoreRejectedDataLocation = Lens.lens (\MagneticStoreWriteProperties' {magneticStoreRejectedDataLocation} -> magneticStoreRejectedDataLocation) (\s@MagneticStoreWriteProperties' {} a -> s {magneticStoreRejectedDataLocation = a} :: MagneticStoreWriteProperties)

-- | A flag to enable magnetic store writes.
magneticStoreWriteProperties_enableMagneticStoreWrites :: Lens.Lens' MagneticStoreWriteProperties Prelude.Bool
magneticStoreWriteProperties_enableMagneticStoreWrites = Lens.lens (\MagneticStoreWriteProperties' {enableMagneticStoreWrites} -> enableMagneticStoreWrites) (\s@MagneticStoreWriteProperties' {} a -> s {enableMagneticStoreWrites = a} :: MagneticStoreWriteProperties)

instance Data.FromJSON MagneticStoreWriteProperties where
  parseJSON =
    Data.withObject
      "MagneticStoreWriteProperties"
      ( \x ->
          MagneticStoreWriteProperties'
            Prelude.<$> (x Data..:? "MagneticStoreRejectedDataLocation")
            Prelude.<*> (x Data..: "EnableMagneticStoreWrites")
      )

instance
  Prelude.Hashable
    MagneticStoreWriteProperties
  where
  hashWithSalt _salt MagneticStoreWriteProperties' {..} =
    _salt
      `Prelude.hashWithSalt` magneticStoreRejectedDataLocation
      `Prelude.hashWithSalt` enableMagneticStoreWrites

instance Prelude.NFData MagneticStoreWriteProperties where
  rnf MagneticStoreWriteProperties' {..} =
    Prelude.rnf magneticStoreRejectedDataLocation
      `Prelude.seq` Prelude.rnf enableMagneticStoreWrites

instance Data.ToJSON MagneticStoreWriteProperties where
  toJSON MagneticStoreWriteProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MagneticStoreRejectedDataLocation" Data..=)
              Prelude.<$> magneticStoreRejectedDataLocation,
            Prelude.Just
              ( "EnableMagneticStoreWrites"
                  Data..= enableMagneticStoreWrites
              )
          ]
      )
