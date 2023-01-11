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
-- Module      : Amazonka.LookoutEquipment.Types.DataQualitySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.DataQualitySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.DuplicateTimestamps
import Amazonka.LookoutEquipment.Types.InsufficientSensorData
import Amazonka.LookoutEquipment.Types.InvalidSensorData
import Amazonka.LookoutEquipment.Types.MissingSensorData
import Amazonka.LookoutEquipment.Types.UnsupportedTimestamps
import qualified Amazonka.Prelude as Prelude

-- | DataQualitySummary gives aggregated statistics over all the sensors
-- about a completed ingestion job. It primarily gives more information
-- about statistics over different incorrect data like
-- MissingCompleteSensorData, MissingSensorData, UnsupportedDateFormats,
-- InsufficientSensorData, DuplicateTimeStamps.
--
-- /See:/ 'newDataQualitySummary' smart constructor.
data DataQualitySummary = DataQualitySummary'
  { -- | Parameter that gives information about insufficient data for sensors in
    -- the dataset. This includes information about those sensors that have
    -- complete data missing and those with a short date range.
    insufficientSensorData :: InsufficientSensorData,
    -- | Parameter that gives information about data that is missing over all the
    -- sensors in the input data.
    missingSensorData :: MissingSensorData,
    -- | Parameter that gives information about data that is invalid over all the
    -- sensors in the input data.
    invalidSensorData :: InvalidSensorData,
    -- | Parameter that gives information about unsupported timestamps in the
    -- input data.
    unsupportedTimestamps :: UnsupportedTimestamps,
    -- | Parameter that gives information about duplicate timestamps in the input
    -- data.
    duplicateTimestamps :: DuplicateTimestamps
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualitySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insufficientSensorData', 'dataQualitySummary_insufficientSensorData' - Parameter that gives information about insufficient data for sensors in
-- the dataset. This includes information about those sensors that have
-- complete data missing and those with a short date range.
--
-- 'missingSensorData', 'dataQualitySummary_missingSensorData' - Parameter that gives information about data that is missing over all the
-- sensors in the input data.
--
-- 'invalidSensorData', 'dataQualitySummary_invalidSensorData' - Parameter that gives information about data that is invalid over all the
-- sensors in the input data.
--
-- 'unsupportedTimestamps', 'dataQualitySummary_unsupportedTimestamps' - Parameter that gives information about unsupported timestamps in the
-- input data.
--
-- 'duplicateTimestamps', 'dataQualitySummary_duplicateTimestamps' - Parameter that gives information about duplicate timestamps in the input
-- data.
newDataQualitySummary ::
  -- | 'insufficientSensorData'
  InsufficientSensorData ->
  -- | 'missingSensorData'
  MissingSensorData ->
  -- | 'invalidSensorData'
  InvalidSensorData ->
  -- | 'unsupportedTimestamps'
  UnsupportedTimestamps ->
  -- | 'duplicateTimestamps'
  DuplicateTimestamps ->
  DataQualitySummary
newDataQualitySummary
  pInsufficientSensorData_
  pMissingSensorData_
  pInvalidSensorData_
  pUnsupportedTimestamps_
  pDuplicateTimestamps_ =
    DataQualitySummary'
      { insufficientSensorData =
          pInsufficientSensorData_,
        missingSensorData = pMissingSensorData_,
        invalidSensorData = pInvalidSensorData_,
        unsupportedTimestamps = pUnsupportedTimestamps_,
        duplicateTimestamps = pDuplicateTimestamps_
      }

-- | Parameter that gives information about insufficient data for sensors in
-- the dataset. This includes information about those sensors that have
-- complete data missing and those with a short date range.
dataQualitySummary_insufficientSensorData :: Lens.Lens' DataQualitySummary InsufficientSensorData
dataQualitySummary_insufficientSensorData = Lens.lens (\DataQualitySummary' {insufficientSensorData} -> insufficientSensorData) (\s@DataQualitySummary' {} a -> s {insufficientSensorData = a} :: DataQualitySummary)

-- | Parameter that gives information about data that is missing over all the
-- sensors in the input data.
dataQualitySummary_missingSensorData :: Lens.Lens' DataQualitySummary MissingSensorData
dataQualitySummary_missingSensorData = Lens.lens (\DataQualitySummary' {missingSensorData} -> missingSensorData) (\s@DataQualitySummary' {} a -> s {missingSensorData = a} :: DataQualitySummary)

-- | Parameter that gives information about data that is invalid over all the
-- sensors in the input data.
dataQualitySummary_invalidSensorData :: Lens.Lens' DataQualitySummary InvalidSensorData
dataQualitySummary_invalidSensorData = Lens.lens (\DataQualitySummary' {invalidSensorData} -> invalidSensorData) (\s@DataQualitySummary' {} a -> s {invalidSensorData = a} :: DataQualitySummary)

-- | Parameter that gives information about unsupported timestamps in the
-- input data.
dataQualitySummary_unsupportedTimestamps :: Lens.Lens' DataQualitySummary UnsupportedTimestamps
dataQualitySummary_unsupportedTimestamps = Lens.lens (\DataQualitySummary' {unsupportedTimestamps} -> unsupportedTimestamps) (\s@DataQualitySummary' {} a -> s {unsupportedTimestamps = a} :: DataQualitySummary)

-- | Parameter that gives information about duplicate timestamps in the input
-- data.
dataQualitySummary_duplicateTimestamps :: Lens.Lens' DataQualitySummary DuplicateTimestamps
dataQualitySummary_duplicateTimestamps = Lens.lens (\DataQualitySummary' {duplicateTimestamps} -> duplicateTimestamps) (\s@DataQualitySummary' {} a -> s {duplicateTimestamps = a} :: DataQualitySummary)

instance Data.FromJSON DataQualitySummary where
  parseJSON =
    Data.withObject
      "DataQualitySummary"
      ( \x ->
          DataQualitySummary'
            Prelude.<$> (x Data..: "InsufficientSensorData")
            Prelude.<*> (x Data..: "MissingSensorData")
            Prelude.<*> (x Data..: "InvalidSensorData")
            Prelude.<*> (x Data..: "UnsupportedTimestamps")
            Prelude.<*> (x Data..: "DuplicateTimestamps")
      )

instance Prelude.Hashable DataQualitySummary where
  hashWithSalt _salt DataQualitySummary' {..} =
    _salt `Prelude.hashWithSalt` insufficientSensorData
      `Prelude.hashWithSalt` missingSensorData
      `Prelude.hashWithSalt` invalidSensorData
      `Prelude.hashWithSalt` unsupportedTimestamps
      `Prelude.hashWithSalt` duplicateTimestamps

instance Prelude.NFData DataQualitySummary where
  rnf DataQualitySummary' {..} =
    Prelude.rnf insufficientSensorData
      `Prelude.seq` Prelude.rnf missingSensorData
      `Prelude.seq` Prelude.rnf invalidSensorData
      `Prelude.seq` Prelude.rnf unsupportedTimestamps
      `Prelude.seq` Prelude.rnf duplicateTimestamps
