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
-- Module      : Amazonka.LookoutEquipment.Types.SensorStatisticsSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.SensorStatisticsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.CategoricalValues
import Amazonka.LookoutEquipment.Types.CountPercent
import Amazonka.LookoutEquipment.Types.LargeTimestampGaps
import Amazonka.LookoutEquipment.Types.MonotonicValues
import Amazonka.LookoutEquipment.Types.MultipleOperatingModes
import qualified Amazonka.Prelude as Prelude

-- | Summary of ingestion statistics like whether data exists, number of
-- missing values, number of invalid values and so on related to the
-- particular sensor.
--
-- /See:/ 'newSensorStatisticsSummary' smart constructor.
data SensorStatisticsSummary = SensorStatisticsSummary'
  { -- | Parameter that describes potential risk about whether data associated
    -- with the sensor is categorical.
    categoricalValues :: Prelude.Maybe CategoricalValues,
    -- | Name of the component to which the particular sensor belongs for which
    -- the statistics belong to.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the time reference to indicate the end of valid data
    -- associated with the sensor that the statistics belong to.
    dataEndTime :: Prelude.Maybe Data.POSIX,
    -- | Parameter that indicates whether data exists for the sensor that the
    -- statistics belong to.
    dataExists :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the time reference to indicate the beginning of valid data
    -- associated with the sensor that the statistics belong to.
    dataStartTime :: Prelude.Maybe Data.POSIX,
    -- | Parameter that describes the total number of duplicate timestamp records
    -- associated with the sensor that the statistics belong to.
    duplicateTimestamps :: Prelude.Maybe CountPercent,
    -- | Parameter that describes the total number of invalid date entries
    -- associated with the sensor that the statistics belong to.
    invalidDateEntries :: Prelude.Maybe CountPercent,
    -- | Parameter that describes the total number of, and percentage of, values
    -- that are invalid for the sensor that the statistics belong to.
    invalidValues :: Prelude.Maybe CountPercent,
    -- | Parameter that describes potential risk about whether data associated
    -- with the sensor contains one or more large gaps between consecutive
    -- timestamps.
    largeTimestampGaps :: Prelude.Maybe LargeTimestampGaps,
    -- | Parameter that describes the total number of, and percentage of, values
    -- that are missing for the sensor that the statistics belong to.
    missingValues :: Prelude.Maybe CountPercent,
    -- | Parameter that describes potential risk about whether data associated
    -- with the sensor is mostly monotonic.
    monotonicValues :: Prelude.Maybe MonotonicValues,
    -- | Parameter that describes potential risk about whether data associated
    -- with the sensor has more than one operating mode.
    multipleOperatingModes :: Prelude.Maybe MultipleOperatingModes,
    -- | Name of the sensor that the statistics belong to.
    sensorName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SensorStatisticsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoricalValues', 'sensorStatisticsSummary_categoricalValues' - Parameter that describes potential risk about whether data associated
-- with the sensor is categorical.
--
-- 'componentName', 'sensorStatisticsSummary_componentName' - Name of the component to which the particular sensor belongs for which
-- the statistics belong to.
--
-- 'dataEndTime', 'sensorStatisticsSummary_dataEndTime' - Indicates the time reference to indicate the end of valid data
-- associated with the sensor that the statistics belong to.
--
-- 'dataExists', 'sensorStatisticsSummary_dataExists' - Parameter that indicates whether data exists for the sensor that the
-- statistics belong to.
--
-- 'dataStartTime', 'sensorStatisticsSummary_dataStartTime' - Indicates the time reference to indicate the beginning of valid data
-- associated with the sensor that the statistics belong to.
--
-- 'duplicateTimestamps', 'sensorStatisticsSummary_duplicateTimestamps' - Parameter that describes the total number of duplicate timestamp records
-- associated with the sensor that the statistics belong to.
--
-- 'invalidDateEntries', 'sensorStatisticsSummary_invalidDateEntries' - Parameter that describes the total number of invalid date entries
-- associated with the sensor that the statistics belong to.
--
-- 'invalidValues', 'sensorStatisticsSummary_invalidValues' - Parameter that describes the total number of, and percentage of, values
-- that are invalid for the sensor that the statistics belong to.
--
-- 'largeTimestampGaps', 'sensorStatisticsSummary_largeTimestampGaps' - Parameter that describes potential risk about whether data associated
-- with the sensor contains one or more large gaps between consecutive
-- timestamps.
--
-- 'missingValues', 'sensorStatisticsSummary_missingValues' - Parameter that describes the total number of, and percentage of, values
-- that are missing for the sensor that the statistics belong to.
--
-- 'monotonicValues', 'sensorStatisticsSummary_monotonicValues' - Parameter that describes potential risk about whether data associated
-- with the sensor is mostly monotonic.
--
-- 'multipleOperatingModes', 'sensorStatisticsSummary_multipleOperatingModes' - Parameter that describes potential risk about whether data associated
-- with the sensor has more than one operating mode.
--
-- 'sensorName', 'sensorStatisticsSummary_sensorName' - Name of the sensor that the statistics belong to.
newSensorStatisticsSummary ::
  SensorStatisticsSummary
newSensorStatisticsSummary =
  SensorStatisticsSummary'
    { categoricalValues =
        Prelude.Nothing,
      componentName = Prelude.Nothing,
      dataEndTime = Prelude.Nothing,
      dataExists = Prelude.Nothing,
      dataStartTime = Prelude.Nothing,
      duplicateTimestamps = Prelude.Nothing,
      invalidDateEntries = Prelude.Nothing,
      invalidValues = Prelude.Nothing,
      largeTimestampGaps = Prelude.Nothing,
      missingValues = Prelude.Nothing,
      monotonicValues = Prelude.Nothing,
      multipleOperatingModes = Prelude.Nothing,
      sensorName = Prelude.Nothing
    }

-- | Parameter that describes potential risk about whether data associated
-- with the sensor is categorical.
sensorStatisticsSummary_categoricalValues :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe CategoricalValues)
sensorStatisticsSummary_categoricalValues = Lens.lens (\SensorStatisticsSummary' {categoricalValues} -> categoricalValues) (\s@SensorStatisticsSummary' {} a -> s {categoricalValues = a} :: SensorStatisticsSummary)

-- | Name of the component to which the particular sensor belongs for which
-- the statistics belong to.
sensorStatisticsSummary_componentName :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe Prelude.Text)
sensorStatisticsSummary_componentName = Lens.lens (\SensorStatisticsSummary' {componentName} -> componentName) (\s@SensorStatisticsSummary' {} a -> s {componentName = a} :: SensorStatisticsSummary)

-- | Indicates the time reference to indicate the end of valid data
-- associated with the sensor that the statistics belong to.
sensorStatisticsSummary_dataEndTime :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe Prelude.UTCTime)
sensorStatisticsSummary_dataEndTime = Lens.lens (\SensorStatisticsSummary' {dataEndTime} -> dataEndTime) (\s@SensorStatisticsSummary' {} a -> s {dataEndTime = a} :: SensorStatisticsSummary) Prelude.. Lens.mapping Data._Time

-- | Parameter that indicates whether data exists for the sensor that the
-- statistics belong to.
sensorStatisticsSummary_dataExists :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe Prelude.Bool)
sensorStatisticsSummary_dataExists = Lens.lens (\SensorStatisticsSummary' {dataExists} -> dataExists) (\s@SensorStatisticsSummary' {} a -> s {dataExists = a} :: SensorStatisticsSummary)

-- | Indicates the time reference to indicate the beginning of valid data
-- associated with the sensor that the statistics belong to.
sensorStatisticsSummary_dataStartTime :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe Prelude.UTCTime)
sensorStatisticsSummary_dataStartTime = Lens.lens (\SensorStatisticsSummary' {dataStartTime} -> dataStartTime) (\s@SensorStatisticsSummary' {} a -> s {dataStartTime = a} :: SensorStatisticsSummary) Prelude.. Lens.mapping Data._Time

-- | Parameter that describes the total number of duplicate timestamp records
-- associated with the sensor that the statistics belong to.
sensorStatisticsSummary_duplicateTimestamps :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe CountPercent)
sensorStatisticsSummary_duplicateTimestamps = Lens.lens (\SensorStatisticsSummary' {duplicateTimestamps} -> duplicateTimestamps) (\s@SensorStatisticsSummary' {} a -> s {duplicateTimestamps = a} :: SensorStatisticsSummary)

-- | Parameter that describes the total number of invalid date entries
-- associated with the sensor that the statistics belong to.
sensorStatisticsSummary_invalidDateEntries :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe CountPercent)
sensorStatisticsSummary_invalidDateEntries = Lens.lens (\SensorStatisticsSummary' {invalidDateEntries} -> invalidDateEntries) (\s@SensorStatisticsSummary' {} a -> s {invalidDateEntries = a} :: SensorStatisticsSummary)

-- | Parameter that describes the total number of, and percentage of, values
-- that are invalid for the sensor that the statistics belong to.
sensorStatisticsSummary_invalidValues :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe CountPercent)
sensorStatisticsSummary_invalidValues = Lens.lens (\SensorStatisticsSummary' {invalidValues} -> invalidValues) (\s@SensorStatisticsSummary' {} a -> s {invalidValues = a} :: SensorStatisticsSummary)

-- | Parameter that describes potential risk about whether data associated
-- with the sensor contains one or more large gaps between consecutive
-- timestamps.
sensorStatisticsSummary_largeTimestampGaps :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe LargeTimestampGaps)
sensorStatisticsSummary_largeTimestampGaps = Lens.lens (\SensorStatisticsSummary' {largeTimestampGaps} -> largeTimestampGaps) (\s@SensorStatisticsSummary' {} a -> s {largeTimestampGaps = a} :: SensorStatisticsSummary)

-- | Parameter that describes the total number of, and percentage of, values
-- that are missing for the sensor that the statistics belong to.
sensorStatisticsSummary_missingValues :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe CountPercent)
sensorStatisticsSummary_missingValues = Lens.lens (\SensorStatisticsSummary' {missingValues} -> missingValues) (\s@SensorStatisticsSummary' {} a -> s {missingValues = a} :: SensorStatisticsSummary)

-- | Parameter that describes potential risk about whether data associated
-- with the sensor is mostly monotonic.
sensorStatisticsSummary_monotonicValues :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe MonotonicValues)
sensorStatisticsSummary_monotonicValues = Lens.lens (\SensorStatisticsSummary' {monotonicValues} -> monotonicValues) (\s@SensorStatisticsSummary' {} a -> s {monotonicValues = a} :: SensorStatisticsSummary)

-- | Parameter that describes potential risk about whether data associated
-- with the sensor has more than one operating mode.
sensorStatisticsSummary_multipleOperatingModes :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe MultipleOperatingModes)
sensorStatisticsSummary_multipleOperatingModes = Lens.lens (\SensorStatisticsSummary' {multipleOperatingModes} -> multipleOperatingModes) (\s@SensorStatisticsSummary' {} a -> s {multipleOperatingModes = a} :: SensorStatisticsSummary)

-- | Name of the sensor that the statistics belong to.
sensorStatisticsSummary_sensorName :: Lens.Lens' SensorStatisticsSummary (Prelude.Maybe Prelude.Text)
sensorStatisticsSummary_sensorName = Lens.lens (\SensorStatisticsSummary' {sensorName} -> sensorName) (\s@SensorStatisticsSummary' {} a -> s {sensorName = a} :: SensorStatisticsSummary)

instance Data.FromJSON SensorStatisticsSummary where
  parseJSON =
    Data.withObject
      "SensorStatisticsSummary"
      ( \x ->
          SensorStatisticsSummary'
            Prelude.<$> (x Data..:? "CategoricalValues")
            Prelude.<*> (x Data..:? "ComponentName")
            Prelude.<*> (x Data..:? "DataEndTime")
            Prelude.<*> (x Data..:? "DataExists")
            Prelude.<*> (x Data..:? "DataStartTime")
            Prelude.<*> (x Data..:? "DuplicateTimestamps")
            Prelude.<*> (x Data..:? "InvalidDateEntries")
            Prelude.<*> (x Data..:? "InvalidValues")
            Prelude.<*> (x Data..:? "LargeTimestampGaps")
            Prelude.<*> (x Data..:? "MissingValues")
            Prelude.<*> (x Data..:? "MonotonicValues")
            Prelude.<*> (x Data..:? "MultipleOperatingModes")
            Prelude.<*> (x Data..:? "SensorName")
      )

instance Prelude.Hashable SensorStatisticsSummary where
  hashWithSalt _salt SensorStatisticsSummary' {..} =
    _salt
      `Prelude.hashWithSalt` categoricalValues
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` dataEndTime
      `Prelude.hashWithSalt` dataExists
      `Prelude.hashWithSalt` dataStartTime
      `Prelude.hashWithSalt` duplicateTimestamps
      `Prelude.hashWithSalt` invalidDateEntries
      `Prelude.hashWithSalt` invalidValues
      `Prelude.hashWithSalt` largeTimestampGaps
      `Prelude.hashWithSalt` missingValues
      `Prelude.hashWithSalt` monotonicValues
      `Prelude.hashWithSalt` multipleOperatingModes
      `Prelude.hashWithSalt` sensorName

instance Prelude.NFData SensorStatisticsSummary where
  rnf SensorStatisticsSummary' {..} =
    Prelude.rnf categoricalValues
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf dataEndTime
      `Prelude.seq` Prelude.rnf dataExists
      `Prelude.seq` Prelude.rnf dataStartTime
      `Prelude.seq` Prelude.rnf duplicateTimestamps
      `Prelude.seq` Prelude.rnf invalidDateEntries
      `Prelude.seq` Prelude.rnf invalidValues
      `Prelude.seq` Prelude.rnf largeTimestampGaps
      `Prelude.seq` Prelude.rnf missingValues
      `Prelude.seq` Prelude.rnf monotonicValues
      `Prelude.seq` Prelude.rnf multipleOperatingModes
      `Prelude.seq` Prelude.rnf sensorName
