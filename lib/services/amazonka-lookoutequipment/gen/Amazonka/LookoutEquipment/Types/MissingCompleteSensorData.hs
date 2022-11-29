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
-- Module      : Amazonka.LookoutEquipment.Types.MissingCompleteSensorData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.MissingCompleteSensorData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises information on sensors that have sensor data
-- completely missing.
--
-- /See:/ 'newMissingCompleteSensorData' smart constructor.
data MissingCompleteSensorData = MissingCompleteSensorData'
  { -- | Indicates the number of sensors that have data missing completely.
    affectedSensorCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MissingCompleteSensorData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affectedSensorCount', 'missingCompleteSensorData_affectedSensorCount' - Indicates the number of sensors that have data missing completely.
newMissingCompleteSensorData ::
  -- | 'affectedSensorCount'
  Prelude.Int ->
  MissingCompleteSensorData
newMissingCompleteSensorData pAffectedSensorCount_ =
  MissingCompleteSensorData'
    { affectedSensorCount =
        pAffectedSensorCount_
    }

-- | Indicates the number of sensors that have data missing completely.
missingCompleteSensorData_affectedSensorCount :: Lens.Lens' MissingCompleteSensorData Prelude.Int
missingCompleteSensorData_affectedSensorCount = Lens.lens (\MissingCompleteSensorData' {affectedSensorCount} -> affectedSensorCount) (\s@MissingCompleteSensorData' {} a -> s {affectedSensorCount = a} :: MissingCompleteSensorData)

instance Core.FromJSON MissingCompleteSensorData where
  parseJSON =
    Core.withObject
      "MissingCompleteSensorData"
      ( \x ->
          MissingCompleteSensorData'
            Prelude.<$> (x Core..: "AffectedSensorCount")
      )

instance Prelude.Hashable MissingCompleteSensorData where
  hashWithSalt _salt MissingCompleteSensorData' {..} =
    _salt `Prelude.hashWithSalt` affectedSensorCount

instance Prelude.NFData MissingCompleteSensorData where
  rnf MissingCompleteSensorData' {..} =
    Prelude.rnf affectedSensorCount
