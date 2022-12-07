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
-- Module      : Amazonka.LookoutEquipment.Types.InvalidSensorData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InvalidSensorData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises aggregated information on sensors having
-- insufficient data.
--
-- /See:/ 'newInvalidSensorData' smart constructor.
data InvalidSensorData = InvalidSensorData'
  { -- | Indicates the number of sensors that have at least some invalid values.
    affectedSensorCount :: Prelude.Int,
    -- | Indicates the total number of invalid values across all the sensors.
    totalNumberOfInvalidValues :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvalidSensorData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affectedSensorCount', 'invalidSensorData_affectedSensorCount' - Indicates the number of sensors that have at least some invalid values.
--
-- 'totalNumberOfInvalidValues', 'invalidSensorData_totalNumberOfInvalidValues' - Indicates the total number of invalid values across all the sensors.
newInvalidSensorData ::
  -- | 'affectedSensorCount'
  Prelude.Int ->
  -- | 'totalNumberOfInvalidValues'
  Prelude.Int ->
  InvalidSensorData
newInvalidSensorData
  pAffectedSensorCount_
  pTotalNumberOfInvalidValues_ =
    InvalidSensorData'
      { affectedSensorCount =
          pAffectedSensorCount_,
        totalNumberOfInvalidValues =
          pTotalNumberOfInvalidValues_
      }

-- | Indicates the number of sensors that have at least some invalid values.
invalidSensorData_affectedSensorCount :: Lens.Lens' InvalidSensorData Prelude.Int
invalidSensorData_affectedSensorCount = Lens.lens (\InvalidSensorData' {affectedSensorCount} -> affectedSensorCount) (\s@InvalidSensorData' {} a -> s {affectedSensorCount = a} :: InvalidSensorData)

-- | Indicates the total number of invalid values across all the sensors.
invalidSensorData_totalNumberOfInvalidValues :: Lens.Lens' InvalidSensorData Prelude.Int
invalidSensorData_totalNumberOfInvalidValues = Lens.lens (\InvalidSensorData' {totalNumberOfInvalidValues} -> totalNumberOfInvalidValues) (\s@InvalidSensorData' {} a -> s {totalNumberOfInvalidValues = a} :: InvalidSensorData)

instance Data.FromJSON InvalidSensorData where
  parseJSON =
    Data.withObject
      "InvalidSensorData"
      ( \x ->
          InvalidSensorData'
            Prelude.<$> (x Data..: "AffectedSensorCount")
            Prelude.<*> (x Data..: "TotalNumberOfInvalidValues")
      )

instance Prelude.Hashable InvalidSensorData where
  hashWithSalt _salt InvalidSensorData' {..} =
    _salt `Prelude.hashWithSalt` affectedSensorCount
      `Prelude.hashWithSalt` totalNumberOfInvalidValues

instance Prelude.NFData InvalidSensorData where
  rnf InvalidSensorData' {..} =
    Prelude.rnf affectedSensorCount
      `Prelude.seq` Prelude.rnf totalNumberOfInvalidValues
