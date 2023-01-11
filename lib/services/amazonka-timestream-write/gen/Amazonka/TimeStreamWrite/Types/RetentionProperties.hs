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
-- Module      : Amazonka.TimeStreamWrite.Types.RetentionProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.RetentionProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Retention properties contain the duration for which your time series
-- data must be stored in the magnetic store and the memory store.
--
-- /See:/ 'newRetentionProperties' smart constructor.
data RetentionProperties = RetentionProperties'
  { -- | The duration for which data must be stored in the memory store.
    memoryStoreRetentionPeriodInHours :: Prelude.Natural,
    -- | The duration for which data must be stored in the magnetic store.
    magneticStoreRetentionPeriodInDays :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetentionProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memoryStoreRetentionPeriodInHours', 'retentionProperties_memoryStoreRetentionPeriodInHours' - The duration for which data must be stored in the memory store.
--
-- 'magneticStoreRetentionPeriodInDays', 'retentionProperties_magneticStoreRetentionPeriodInDays' - The duration for which data must be stored in the magnetic store.
newRetentionProperties ::
  -- | 'memoryStoreRetentionPeriodInHours'
  Prelude.Natural ->
  -- | 'magneticStoreRetentionPeriodInDays'
  Prelude.Natural ->
  RetentionProperties
newRetentionProperties
  pMemoryStoreRetentionPeriodInHours_
  pMagneticStoreRetentionPeriodInDays_ =
    RetentionProperties'
      { memoryStoreRetentionPeriodInHours =
          pMemoryStoreRetentionPeriodInHours_,
        magneticStoreRetentionPeriodInDays =
          pMagneticStoreRetentionPeriodInDays_
      }

-- | The duration for which data must be stored in the memory store.
retentionProperties_memoryStoreRetentionPeriodInHours :: Lens.Lens' RetentionProperties Prelude.Natural
retentionProperties_memoryStoreRetentionPeriodInHours = Lens.lens (\RetentionProperties' {memoryStoreRetentionPeriodInHours} -> memoryStoreRetentionPeriodInHours) (\s@RetentionProperties' {} a -> s {memoryStoreRetentionPeriodInHours = a} :: RetentionProperties)

-- | The duration for which data must be stored in the magnetic store.
retentionProperties_magneticStoreRetentionPeriodInDays :: Lens.Lens' RetentionProperties Prelude.Natural
retentionProperties_magneticStoreRetentionPeriodInDays = Lens.lens (\RetentionProperties' {magneticStoreRetentionPeriodInDays} -> magneticStoreRetentionPeriodInDays) (\s@RetentionProperties' {} a -> s {magneticStoreRetentionPeriodInDays = a} :: RetentionProperties)

instance Data.FromJSON RetentionProperties where
  parseJSON =
    Data.withObject
      "RetentionProperties"
      ( \x ->
          RetentionProperties'
            Prelude.<$> (x Data..: "MemoryStoreRetentionPeriodInHours")
            Prelude.<*> (x Data..: "MagneticStoreRetentionPeriodInDays")
      )

instance Prelude.Hashable RetentionProperties where
  hashWithSalt _salt RetentionProperties' {..} =
    _salt
      `Prelude.hashWithSalt` memoryStoreRetentionPeriodInHours
      `Prelude.hashWithSalt` magneticStoreRetentionPeriodInDays

instance Prelude.NFData RetentionProperties where
  rnf RetentionProperties' {..} =
    Prelude.rnf memoryStoreRetentionPeriodInHours
      `Prelude.seq` Prelude.rnf magneticStoreRetentionPeriodInDays

instance Data.ToJSON RetentionProperties where
  toJSON RetentionProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MemoryStoreRetentionPeriodInHours"
                  Data..= memoryStoreRetentionPeriodInHours
              ),
            Prelude.Just
              ( "MagneticStoreRetentionPeriodInDays"
                  Data..= magneticStoreRetentionPeriodInDays
              )
          ]
      )
