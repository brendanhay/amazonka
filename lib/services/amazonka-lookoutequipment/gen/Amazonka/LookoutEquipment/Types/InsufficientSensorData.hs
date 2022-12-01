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
-- Module      : Amazonka.LookoutEquipment.Types.InsufficientSensorData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InsufficientSensorData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutEquipment.Types.MissingCompleteSensorData
import Amazonka.LookoutEquipment.Types.SensorsWithShortDateRange
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises aggregated information on sensors having
-- insufficient data.
--
-- /See:/ 'newInsufficientSensorData' smart constructor.
data InsufficientSensorData = InsufficientSensorData'
  { -- | Parameter that describes the total number of sensors that have data
    -- completely missing for it.
    missingCompleteSensorData :: MissingCompleteSensorData,
    -- | Parameter that describes the total number of sensors that have a short
    -- date range of less than 90 days of data overall.
    sensorsWithShortDateRange :: SensorsWithShortDateRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsufficientSensorData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'missingCompleteSensorData', 'insufficientSensorData_missingCompleteSensorData' - Parameter that describes the total number of sensors that have data
-- completely missing for it.
--
-- 'sensorsWithShortDateRange', 'insufficientSensorData_sensorsWithShortDateRange' - Parameter that describes the total number of sensors that have a short
-- date range of less than 90 days of data overall.
newInsufficientSensorData ::
  -- | 'missingCompleteSensorData'
  MissingCompleteSensorData ->
  -- | 'sensorsWithShortDateRange'
  SensorsWithShortDateRange ->
  InsufficientSensorData
newInsufficientSensorData
  pMissingCompleteSensorData_
  pSensorsWithShortDateRange_ =
    InsufficientSensorData'
      { missingCompleteSensorData =
          pMissingCompleteSensorData_,
        sensorsWithShortDateRange =
          pSensorsWithShortDateRange_
      }

-- | Parameter that describes the total number of sensors that have data
-- completely missing for it.
insufficientSensorData_missingCompleteSensorData :: Lens.Lens' InsufficientSensorData MissingCompleteSensorData
insufficientSensorData_missingCompleteSensorData = Lens.lens (\InsufficientSensorData' {missingCompleteSensorData} -> missingCompleteSensorData) (\s@InsufficientSensorData' {} a -> s {missingCompleteSensorData = a} :: InsufficientSensorData)

-- | Parameter that describes the total number of sensors that have a short
-- date range of less than 90 days of data overall.
insufficientSensorData_sensorsWithShortDateRange :: Lens.Lens' InsufficientSensorData SensorsWithShortDateRange
insufficientSensorData_sensorsWithShortDateRange = Lens.lens (\InsufficientSensorData' {sensorsWithShortDateRange} -> sensorsWithShortDateRange) (\s@InsufficientSensorData' {} a -> s {sensorsWithShortDateRange = a} :: InsufficientSensorData)

instance Core.FromJSON InsufficientSensorData where
  parseJSON =
    Core.withObject
      "InsufficientSensorData"
      ( \x ->
          InsufficientSensorData'
            Prelude.<$> (x Core..: "MissingCompleteSensorData")
            Prelude.<*> (x Core..: "SensorsWithShortDateRange")
      )

instance Prelude.Hashable InsufficientSensorData where
  hashWithSalt _salt InsufficientSensorData' {..} =
    _salt
      `Prelude.hashWithSalt` missingCompleteSensorData
      `Prelude.hashWithSalt` sensorsWithShortDateRange

instance Prelude.NFData InsufficientSensorData where
  rnf InsufficientSensorData' {..} =
    Prelude.rnf missingCompleteSensorData
      `Prelude.seq` Prelude.rnf sensorsWithShortDateRange
