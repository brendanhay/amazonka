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
-- Module      : Amazonka.LookoutEquipment.Types.MissingSensorData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.MissingSensorData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises aggregated information on sensors having missing
-- data.
--
-- /See:/ 'newMissingSensorData' smart constructor.
data MissingSensorData = MissingSensorData'
  { -- | Indicates the number of sensors that have atleast some data missing.
    affectedSensorCount :: Prelude.Int,
    -- | Indicates the total number of missing values across all the sensors.
    totalNumberOfMissingValues :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MissingSensorData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affectedSensorCount', 'missingSensorData_affectedSensorCount' - Indicates the number of sensors that have atleast some data missing.
--
-- 'totalNumberOfMissingValues', 'missingSensorData_totalNumberOfMissingValues' - Indicates the total number of missing values across all the sensors.
newMissingSensorData ::
  -- | 'affectedSensorCount'
  Prelude.Int ->
  -- | 'totalNumberOfMissingValues'
  Prelude.Int ->
  MissingSensorData
newMissingSensorData
  pAffectedSensorCount_
  pTotalNumberOfMissingValues_ =
    MissingSensorData'
      { affectedSensorCount =
          pAffectedSensorCount_,
        totalNumberOfMissingValues =
          pTotalNumberOfMissingValues_
      }

-- | Indicates the number of sensors that have atleast some data missing.
missingSensorData_affectedSensorCount :: Lens.Lens' MissingSensorData Prelude.Int
missingSensorData_affectedSensorCount = Lens.lens (\MissingSensorData' {affectedSensorCount} -> affectedSensorCount) (\s@MissingSensorData' {} a -> s {affectedSensorCount = a} :: MissingSensorData)

-- | Indicates the total number of missing values across all the sensors.
missingSensorData_totalNumberOfMissingValues :: Lens.Lens' MissingSensorData Prelude.Int
missingSensorData_totalNumberOfMissingValues = Lens.lens (\MissingSensorData' {totalNumberOfMissingValues} -> totalNumberOfMissingValues) (\s@MissingSensorData' {} a -> s {totalNumberOfMissingValues = a} :: MissingSensorData)

instance Data.FromJSON MissingSensorData where
  parseJSON =
    Data.withObject
      "MissingSensorData"
      ( \x ->
          MissingSensorData'
            Prelude.<$> (x Data..: "AffectedSensorCount")
            Prelude.<*> (x Data..: "TotalNumberOfMissingValues")
      )

instance Prelude.Hashable MissingSensorData where
  hashWithSalt _salt MissingSensorData' {..} =
    _salt `Prelude.hashWithSalt` affectedSensorCount
      `Prelude.hashWithSalt` totalNumberOfMissingValues

instance Prelude.NFData MissingSensorData where
  rnf MissingSensorData' {..} =
    Prelude.rnf affectedSensorCount
      `Prelude.seq` Prelude.rnf totalNumberOfMissingValues
